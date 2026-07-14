{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The save path: snapshot every live world page into 'SaveData' and
--   write it to disk. Split out of "World.Thread.Command.Save"
--   (issue #561).
module World.Thread.Command.Save.WriteWorld
    ( handleWorldSaveCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logError, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Save.Serialize (saveWorld)
import Unit.Types (UnitManager(..), unitsOnPage)
import Unit.Sim.Types (UnitThreadState(..))
import World.Thread.Helpers (unWorldPageId)
import Engine.PlayerEvent.Emit (emitEvent)
import Engine.Save.Barrier (finishSave, failSave, readSaveStatus, ssRequestId)

-- | Save: snapshot the live WorldState and write to disk ──logInfo logger CatWorld $ "Saving world: " <> unWorldPageId pageId
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
        -- Resume speed is no longer a reason to override the request: a load's
        -- resume restores the active page's OWN speed via pause.onSaveLoaded.
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
            paused     ← readIORef (enginePausedRef env)
            -- v54 (structure persistence): the texture palette is global.
            texPalette ← readIORef (texPaletteRef env)
            -- v56 (item-instance identity, #67): persist the allocator so
            -- new items created after a reload keep unique ids.
            nextItemId ← readIORef (nextItemInstanceIdRef env)
            -- The entity managers are global across worlds (#76/#78); read
            -- them once and slice per page below.
            bm         ← readIORef (buildingManagerRef env)
            um         ← readIORef (unitManagerRef env)
            uts        ← readIORef (utsRef env)
            -- The primary page's gen params drive the listing metadata,
            -- and its player-facing identity (#707) rides along so save
            -- listings can show the world's name without decoding the
            -- full page list.
            mActiveParams ← readIORef (wsGenParamsRef primaryWs)
            primaryIdentity ← readIORef (wsIdentityRef primaryWs)
            case mActiveParams of
                Nothing →
                    do
                        let err = "Cannot save: visible world has no gen params"
                        logWarn logger CatWorld err
                        failTransaction env err
                Just activeParams → do
                    -- #216: snapshot EVERY live page in wmWorlds, not just
                    -- the active one. A page with no gen params (e.g. an
                    -- arena still mid-init) is not a real, persistable world
                    -- — skip it rather than abort the whole save.
                    pageSaves ← forM (wmWorlds mgr) $ \(pid, ws) → do
                        mParams ← readIORef (wsGenParamsRef ws)
                        case mParams of
                            Nothing → pure Nothing
                            Just params → do
                                WorldTime h m    ← readIORef (wsTimeRef ws)
                                WorldDate y mo d ← readIORef (wsDateRef ws)
                                _tScale   ← readIORef (wsTimeScaleRef ws)
                                -- Freeze ONLY the VISIBLE page's clock here, to
                                -- match scripts/pause.lua: its prevTimeScale /
                                -- resume dance retimes just world.getActiveWorldId(),
                                -- so zeroing any other page's wsTimeScaleRef
                                -- would leave it stuck at 0 once shown (nothing
                                -- restores it). Drift while paused is already
                                -- prevented for every page by tickWorldTime,
                                -- which gates advancement on enginePausedRef and
                                -- only ticks wmVisible worlds (#42) — so a
                                -- hidden page can't advance regardless. 'tScale'
                                -- (the player's chosen speed) is captured for
                                -- wpsTimeScale first, so this loses nothing.
                                when (Just pid ≡ visibleId) $
                                    writeIORef (wsTimeScaleRef ws) 0
                                mapMode   ← readIORef (wsMapModeRef ws)
                                toolMode  ← readIORef (wsToolModeRef ws)
                                edits     ← readIORef (wsEditsRef ws)
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
                                -- Camera: the VISIBLE page uses the live global
                                -- Camera2D (authoritative position/zoom/facing
                                -- the player sees). Other pages carry only a
                                -- WorldCamera (x, y) in their own state — no
                                -- per-page zoom/facing exists — so they save
                                -- their stored position with the global
                                -- zoom/facing as the best available value.
                                let isVisible = Just pid ≡ visibleId
                                    (cx, cy) = if isVisible
                                               then camPosition cam
                                               else (wcx, wcy)
                                    buildings = toBuildingSnapshot pid bm
                                    units     = toUnitSnapshot pid um
                                    -- Keep only this page's units' sim states.
                                    savedUids = HM.keysSet
                                        (unitsOnPage pid (umInstances um))
                                    simStates = HM.filterWithKey
                                        (\uid _ → uid `HS.member` savedUids)
                                        (utsSimStates uts)
                                pure $ Just WorldPageSave
                                    { wpsPageId     = pid
                                    , wpsGenParams  = params
                                    , wpsCameraX    = cx
                                    , wpsCameraY    = cy
                                    , wpsCameraZoom = camZoom cam
                                    , wpsCameraFacing = camFacing cam
                                    , wpsTimeHour   = h
                                    , wpsTimeMinute = m
                                    , wpsDateYear   = y
                                    , wpsDateMonth  = mo
                                    , wpsDateDay    = d
                                    -- Positional compatibility field: retained
                                    -- on disk, but never carries the player's
                                    -- pre-save speed.  A loaded session always
                                    -- resumes at the normal default scale.
                                    , wpsTimeScale  = 1
                                    , wpsMapMode    = mapMode
                                    , wpsToolMode   = toolMode
                                    , wpsEdits      = edits
                                    , wpsMineDesignations = mineDesigs
                                    , wpsConstructDesignations = constructDesigs
                                    , wpsGroundItems = groundItems
                                    , wpsSpoilPiles  = spoilPiles
                                    , wpsBuildings   = buildings
                                    , wpsUnits       = units
                                    , wpsUnitSimStates = simStates
                                    , wpsFloraHarvests = floraHarvests
                                    , wpsChopDesignations = chopDesigs
                                    , wpsCraftBills  = craftBills
                                    , wpsPowerNodes  = powerNodes
                                    , wpsTillDesignations = tillDesigs
                                    , wpsCropPlots   = cropPlots
                                    , wpsPlantDesignations = plantDesigs
                                    , wpsIdentity    = identity
                                    }
                    -- UTC ISO 8601 microsecond precision, captured and
                    -- monotonically clamped at the API request time (see
                    -- saveWorldFn) — NOT here, so two saves queued
                    -- back-to-back don't get the same wall-second
                    -- timestamp from world-thread processing latency.
                    -- Lexicographic sort by this fixed-width string is
                    -- chronologically correct, so the
                    -- Lua-side `a.timestamp > b.timestamp` in
                    -- main_menu works without further wrapping.
                    let meta = SaveMetadata
                            { smName       = saveName
                            , smSeed       = wgpSeed activeParams
                            , smWorldSize  = wgpWorldSize activeParams
                            , smPlateCount = wgpPlateCount activeParams
                            , smTimestamp  = timestampTxt
                            , smWorldName  = wiName ⊚ primaryIdentity
                            , smWorldGloss = wiGloss =≪ primaryIdentity
                            }
                        sd = SaveData
                            { sdMetadata   = meta
                            , sdGameTime     = gameTime
                            , sdEnginePaused = paused
                            , sdLuaModules   = luaBlobs
                            , sdTexPalette   = texPalette
                            , sdNextItemInstanceId = nextItemId
                            , sdActivePage   = primaryId
                            -- Record visibility so the loaded game comes up
                            -- showing what the player last saw (#216).
                            , sdVisiblePages = wmVisible mgr
                            , sdWorlds       = catMaybes pageSaves
                            }
                    result ← saveWorld saveName sd
                    case result of
                        Right () → do
                            completeTransaction env
                            logInfo logger CatWorld $
                                "World saved successfully: " <> saveName
                            emitEvent env "save_load" "World.Save" $
                                "Game saved: " <> saveName
                        Left err → do
                            failTransaction env err
                            logError logger CatWorld $
                                "Failed to save world: " <> err
                            emitEvent env "save_load" "World.Save" $
                                "Save failed: " <> err

completeTransaction ∷ EngineEnv → IO ()
completeTransaction env = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → finishSave (saveBarrierRef env) (ssRequestId s)

failTransaction ∷ EngineEnv → Text → IO ()
failTransaction env err = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → failSave (saveBarrierRef env) (ssRequestId s) err
