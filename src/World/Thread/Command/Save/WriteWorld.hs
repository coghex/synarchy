{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The save path: capture every live world page into an immutable,
--   validated 'SessionSnapshot' (#758), fully ENCODE it while the
--   coordinated #757 barrier is still held, then release the capture
--   lock so every state owner resumes, and finally write the
--   already-encoded bytes to disk — the transaction itself stays open
--   ('SaveEncoding') and only reaches its terminal outcome once that
--   write actually resolves, so a disk failure after release still
--   surfaces as a real failure rather than a save the barrier already
--   called a success. Split out of "World.Thread.Command.Save"
--   (issue #561).
module World.Thread.Command.Save.WriteWorld
    ( handleWorldSaveCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Exception (SomeException, evaluate, try)
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logError, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Save.Serialize (encodeSaveData, writeSaveFiles)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import Unit.Types (UnitManager(..), unitsOnPage)
import Building.Types (BuildingManager(bmNextId))
import Unit.Sim.Types (UnitThreadState(..))
import World.Thread.Helpers (unWorldPageId)
import Engine.PlayerEvent.Emit (emitEvent)
import Engine.Save.Barrier
    (releaseCaptureLock, finishSave, failSave, readSaveStatus, ssRequestId)
import World.Edit.Types (WorldEdit(..), WorldEdits, appendEdit)
import World.Generate.Coordinates (chunkToGlobal)

-- | Save: capture the live WorldState into a validated snapshot,
--   release the barrier, then write to disk.
handleWorldSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → Text
                       → Text → HM.HashMap Text Text → IO ()
handleWorldSaveCommand env logger pageId saveName timestampTxt luaBlobs = do
    mgr ← readIORef (worldManagerRef env)
    -- The page whose live camera IS the global Camera2D, and whose clock
    -- scripts/pause.lua retimes via its single prevTimeScale on resume, is the
    -- actually-VISIBLE world (head of wmVisible, if registered). NOT the raw
    -- wmWorlds head (resolveActiveWorld's fallback) — a hidden page can sit
    -- there. 'Nothing' when nothing is visible.
    let visibleId = case wmVisible mgr of
            (vid:_) | isJust (lookup vid (wmWorlds mgr)) → Just vid
            _                                            → Nothing
        -- The save's PRIMARY page (restores as main_world, drives the listing
        -- metadata) is the REQUESTED 'pageId' — engine.saveWorld is explicitly
        -- page-targeted and a debug/headless caller may save a non-visible page.
        -- This is independent of 'visibleId', which only governs camera/clock
        -- attribution below (those belong to whatever is actually on screen).
        primaryId = pageId
    case lookup primaryId (wmWorlds mgr) of
        Nothing →
            do
                let err = "World not found for save: " <> unWorldPageId primaryId
                logWarn logger CatWorld err
                failTransaction env err
        Just primaryWs → do
            -- Auto-pause BEFORE reading state so the snapshot
            -- captures pause = True (DF convention — saved worlds
            -- load paused so the player can plan the next move).
            writeIORef (enginePausedRef env) True
            -- Globals: read once, shared across every page (we're on the
            -- world thread, so no races with worldLoop writes).
            cam        ← readIORef (cameraRef env)
            gameTime   ← readIORef (gameTimeRef env)
            -- v54 (structure persistence): the texture palette is global.
            texPalette ← readIORef (texPaletteRef env)
            -- v56 (item-instance identity, #67): persist the allocator so
            -- new items created after a reload keep unique ids.
            nextItemId ← readIORef (nextItemInstanceIdRef env)
            -- The entity managers are global across worlds (#76/#78); read
            -- them once and slice per page below. bmNextId/umNextId are
            -- likewise global counters, captured once at the top level
            -- (#758) rather than only living duplicated inside every
            -- page's BuildingSnapshot/UnitSnapshot.
            bm         ← readIORef (buildingManagerRef env)
            um         ← readIORef (unitManagerRef env)
            uts        ← readIORef (utsRef env)
            mParams ← readIORef (wsGenParamsRef primaryWs)
            case mParams of
                Nothing →
                    do
                        let err = "Cannot save: visible world has no gen params"
                        logWarn logger CatWorld err
                        failTransaction env err
                Just _ → do
                    -- Every page must be snapshotable.  Omitting an
                    -- in-progress page makes a superficially successful save
                    -- corrupt the whole session, so fail the transaction.
                    maybePages ← forM (wmWorlds mgr) $ \(pid, ws) → do
                        mPageParams ← readIORef (wsGenParamsRef ws)
                        case mPageParams of
                            Nothing → pure $ Left ("page is not snapshotable: " <> unWorldPageId pid)
                            Just params → do
                                WorldTime h m    ← readIORef (wsTimeRef ws)
                                WorldDate y mo d ← readIORef (wsDateRef ws)
                                -- Freeze ONLY the VISIBLE page's clock here, to
                                -- match scripts/pause.lua: its prevTimeScale /
                                -- resume dance retimes just world.getActiveWorldId(),
                                -- so zeroing any other page's wsTimeScaleRef
                                -- would leave it stuck at 0 once shown (nothing
                                -- restores it). Drift while paused is already
                                -- prevented for every page by tickWorldTime,
                                -- which gates advancement on enginePausedRef and
                                -- only ticks wmVisible worlds (#42) — so a
                                -- hidden page can't advance regardless. Time
                                -- scale itself is never captured in the
                                -- snapshot at all (#758: load policy, not
                                -- gameplay state — see World.Save.Snapshot).
                                when (Just pid ≡ visibleId) $
                                    writeIORef (wsTimeScaleRef ws) 0
                                mapMode   ← readIORef (wsMapModeRef ws)
                                edits     ← readIORef (wsEditsRef ws)
                                tiles     ← readIORef (wsTilesRef ws)
                                mineDesigs ← readIORef (wsMineDesignationsRef ws)
                                constructDesigs ← readIORef
                                    (wsConstructDesignationsRef ws)
                                groundItems ← readIORef (wsGroundItemsRef ws)
                                spoilPiles ← readIORef (wsSpoilRef ws)
                                floraHarvests ← readIORef (wsFloraHarvestsRef ws)
                                chopDesigs ← readIORef (wsChopDesignationsRef ws)
                                tillDesigs ← readIORef (wsTillDesignationsRef ws)
                                cropPlots ← readIORef (wsCropPlotsRef ws)
                                plantDesigs ← readIORef (wsPlantDesignationsRef ws)
                                craftBills ← readIORef (wsCraftBillsRef ws)
                                powerNodes ← readIORef (wsPowerNodesRef ws)
                                identity  ← readIORef (wsIdentityRef ws)
                                WorldCamera wcx wcy ← readIORef (wsCameraRef ws)
                                let buildings = toBuildingSnapshot pid bm
                                    units     = toUnitSnapshot pid um
                                    -- Keep only this page's units' sim states.
                                    savedUids = HM.keysSet
                                        (unitsOnPage pid (umInstances um))
                                    simStates = HM.filterWithKey
                                        (\uid _ → uid `HS.member` savedUids)
                                        (utsSimStates uts)
                                    persistedEdits = appendFluidSnapshot edits tiles
                                pure $ Right PageSnapshot
                                    { pgsPageId     = pid
                                    , pgsGenParams  = params
                                    , pgsCameraX    = wcx
                                    , pgsCameraY    = wcy
                                    , pgsTimeHour   = h
                                    , pgsTimeMinute = m
                                    , pgsDateYear   = y
                                    , pgsDateMonth  = mo
                                    , pgsDateDay    = d
                                    , pgsMapMode    = mapMode
                                    , pgsEdits      = persistedEdits
                                    , pgsMineDesignations = mineDesigs
                                    , pgsConstructDesignations = constructDesigs
                                    , pgsGroundItems = groundItems
                                    , pgsSpoilPiles  = spoilPiles
                                    , pgsBuildings   = buildings
                                    , pgsUnits       = units
                                    , pgsUnitSimStates = simStates
                                    , pgsFloraHarvests = floraHarvests
                                    , pgsChopDesignations = chopDesigs
                                    , pgsCraftBills  = craftBills
                                    , pgsPowerNodes  = powerNodes
                                    , pgsTillDesignations = tillDesigs
                                    , pgsCropPlots   = cropPlots
                                    , pgsPlantDesignations = plantDesigs
                                    , pgsIdentity    = identity
                                    }
                    case sequence maybePages of
                      Left err → do
                        logWarn logger CatWorld err
                        failTransaction env err
                      Right pages → do
                        let liveCamera = LiveCameraSnapshot
                                { lcsOwnerPage = visibleId
                                , lcsX      = fst (camPosition cam)
                                , lcsY      = snd (camPosition cam)
                                , lcsZoom   = camZoom cam
                                , lcsFacing = camFacing cam
                                }
                            globals = SessionGlobals
                                { sgGameTime       = gameTime
                                , sgTexPalette     = texPalette
                                , sgNextItemId     = nextItemId
                                , sgNextBuildingId = bmNextId bm
                                , sgNextUnitId     = umNextId um
                                , sgLuaModules     = luaBlobs
                                , sgActivePage     = primaryId
                                -- Record visibility so the loaded game comes up
                                -- showing what the player last saw (#216).
                                , sgVisiblePages   = wmVisible mgr
                                , sgLiveCamera     = liveCamera
                                }
                        case captureSessionSnapshot globals pages of
                          Left errs → do
                            let msg = "session snapshot failed validation: "
                                    <> T.intercalate "; " (map (T.pack ∘ show) errs)
                            logWarn logger CatWorld msg
                            failTransaction env msg
                          Right snap → do
                            -- UTC ISO 8601 microsecond precision, captured and
                            -- monotonically clamped at the API request time
                            -- (see saveWorldFn) — NOT here, so two saves
                            -- queued back-to-back don't get the same
                            -- wall-second timestamp from world-thread
                            -- processing latency. Lexicographic sort by this
                            -- fixed-width string is chronologically correct,
                            -- so the Lua-side `a.timestamp > b.timestamp` in
                            -- main_menu works without further wrapping.
                            let req = SaveRequestMeta
                                    { srmSlotName  = saveName
                                    , srmTimestamp = timestampTxt
                                    }
                                sd  = snapshotToSaveData req snap
                            -- Force the FULL encoding now, while the capture
                            -- lock is STILL held (#758 requirement 7): cereal
                            -- cannot produce a ByteString without visiting
                            -- every field, so this either succeeds completely
                            -- right here or throws right here — never
                            -- partway through the disk write below, after
                            -- other owners have already resumed. Anything
                            -- 'World.Save.Snapshot'/'.Adapter' left as an
                            -- unevaluated thunk (their record fields are only
                            -- forced to WHNF, not deeply) gets touched here.
                            -- A thrown exception is a capture failure, not a
                            -- disk failure: fail the transaction directly
                            -- and skip the release entirely — failSave's own
                            -- phase transition already unblocks
                            -- 'captureLocked' for every other owner.
                            encodedOrErr ← try (evaluate (encodeSaveData sd))
                            case encodedOrErr of
                              Left (e ∷ SomeException) → do
                                let msg = "session snapshot failed to encode: "
                                        <> T.pack (show e)
                                logWarn logger CatWorld msg
                                failTransaction env msg
                              Right encoded → do
                                -- Every state owner may resume as soon as the
                                -- snapshot is fully captured, validated, AND
                                -- encoded (#758 requirement 10) — but the
                                -- save TRANSACTION stays open (non-terminal
                                -- 'SaveEncoding') until the disk write below
                                -- actually resolves, so a write failure still
                                -- surfaces as a real 'SaveFailed' outcome
                                -- instead of the barrier having already
                                -- declared success. 'encoded'/'sd' are
                                -- already-computed immutable values — never
                                -- live state again — so a mutation the
                                -- instant after release can never change
                                -- what gets written.
                                releaseCaptureLock' env
                                result ← writeSaveFiles saveName encoded sd
                                case result of
                                  Right () →
                                    do
                                        completeTransaction env
                                        logInfo logger CatWorld $
                                            "World saved successfully: " <> saveName
                                        emitEvent env "save_load" "World.Save" $
                                            "Game saved: " <> saveName
                                  Left err →
                                    do
                                        failTransaction env err
                                        logError logger CatWorld $
                                            "Failed to save world: " <> err
                                        emitEvent env "save_load" "World.Save" $
                                            "Save failed: " <> err

-- | #758: release the barrier so state owners resume WITHOUT declaring
--   the transaction terminally complete yet — see 'releaseCaptureLock'.
releaseCaptureLock' ∷ EngineEnv → IO ()
releaseCaptureLock' env = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → releaseCaptureLock (saveBarrierRef env) (ssRequestId s)

completeTransaction ∷ EngineEnv → IO ()
completeTransaction env = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → finishSave (saveBarrierRef env) (ssRequestId s)

failTransaction ∷ EngineEnv → Text → IO ()
failTransaction env err = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → failSave (saveBarrierRef env) (ssRequestId s) err

-- | The simulation owns the live fluid map and publishes it back to the
-- world thread.  Preserve the settled state of every loaded chunk as trailing
-- replay edits so loading a paused save does not discard a pre-boundary
-- World → Sim → World writeback.
appendFluidSnapshot ∷ WorldEdits → WorldTileData → WorldEdits
appendFluidSnapshot edits tiles =
    HM.foldl' appendChunk (dropReplacedSnapshots edits) (wtdChunks tiles)
  where
    -- A snapshot is a replacement for the currently loaded chunk's old
    -- snapshot, not another historical edit.  Preserve snapshots for chunks
    -- that are not loaded this save: they still carry their last settled
    -- simulation state and will be replayed if the chunk is loaded later.
    dropReplacedSnapshots = HM.mapMaybeWithKey $ \coord chunkEdits →
        let kept = if HM.member coord (wtdChunks tiles)
                   then filter (not . isFluidSnapshot) chunkEdits
                   else chunkEdits
        in if null kept then Nothing else Just kept
    appendChunk acc lc = V.ifoldl' (appendCell (lcCoord lc)) acc (lcFluidMap lc)
    appendCell coord acc idx mCell =
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            (gx, gy) = chunkToGlobal coord lx ly
            edit = case mCell of
                Just cell → WeSetFluidSnapshot gx gy (fcType cell) (fcSurface cell)
                Nothing   → WeClearFluidSnapshot gx gy
        in appendEdit coord edit acc

isFluidSnapshot ∷ WorldEdit → Bool
isFluidSnapshot (WeSetFluidSnapshot _ _ _ _) = True
isFluidSnapshot (WeClearFluidSnapshot _ _)   = True
isFluidSnapshot _                            = False
