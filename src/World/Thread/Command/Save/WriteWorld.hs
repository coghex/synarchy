{-# LANGUAGE Strict #-}

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
    , restoreAfterAutosave
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Exception (SomeException, evaluate, finally, try)
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.WorldSim
    (toWorldSimCapability, restoreIfPlayerIdle)
import Engine.Core.Log (logInfo, logError, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Pause
    (reassertSavePause, releasePauseHeld, setPauseResumeScale)
import World.Save.Serialize (encodeSessionSnapshot, writeSaveFiles)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Integrity
    (sessionIntegrityErrors, sessionIntegrityWarnings
    , capIntegrityErrors, renderIntegrityReport
    , IntegrityReport(..), buildKnownEntities, LuaRefEdge
    , luaReferenceErrors, integrityErrorCap)
import World.Save.Payload (LuaComponentSpec(..))
import Unit.Types (UnitManager(..), unitsOnPage)
import Building.Types (BuildingManager(bmNextId))
import Unit.Sim.Types (UnitThreadState(..))
import Engine.PlayerEvent.Emit (emitEvent)
import Engine.Save.Barrier
    (releaseCaptureLock, finishSave, failSave, readSaveStatus, ssRequestId)
import World.Edit.Types (WorldEdit(..), WorldEdits, appendEdit)
import World.Generate.Coordinates (chunkToGlobal)

-- | Save: capture the live WorldState into a validated snapshot,
--   release the barrier, then write to disk. @luaComponents@ is every
--   currently-registered Lua save component (bare name, version,
--   required, already-encoded payload — issue #761), gathered on the
--   Lua thread before this command was queued; it rides straight into
--   'encodeSessionSnapshot' below rather than through 'SessionGlobals'
--   at all — Lua-owned state is no longer part of 'SessionSnapshot'.
handleWorldSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → Text
                       → Text → [LuaComponentSpec]
                       → [LuaRefEdge]
                       → Maybe AutosaveRequest
                       → IO ()
handleWorldSaveCommand env logger pageId saveName timestampTxt luaComponents
                        luaRefs mAutosave = do
    -- Only the WORDING of a failure report depends on this; every other
    -- autosave-specific behaviour reads 'mAutosave' itself.
    let isAutosave = isJust mAutosave
    mgr ← readIORef (worldManagerRef env)
    -- The page whose live camera IS the global Camera2D, and whose clock
    -- a pause epoch takes hold of ('World.Pause'), is the actually-VISIBLE
    -- world (head of wmVisible, if registered). NOT the raw wmWorlds head
    -- (resolveActiveWorld's fallback) — a hidden page can sit there.
    -- 'Nothing' when nothing is visible. Same rule as
    -- 'World.State.Types.visiblePageState', which is what the pause path
    -- resolves through — spelled out here only because this side needs
    -- the page's ID rather than its state. Keeping the two in step is
    -- what makes the page this save restores after an autosave the page
    -- whose clock the save's own pause zeroed.
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
                failTransaction env isAutosave err
        Just primaryWs → do
            -- Auto-pause BEFORE reading state so the snapshot
            -- captures pause = True (DF convention — saved worlds
            -- load paused so the player can plan the next move).
            --
            -- Almost always a RE-assertion: the Lua acceptance
            -- ('Engine.Scripting.Lua.API.Save.acceptSaveRequest')
            -- already opened this save's pause epoch, so this finds the
            -- session paused and deliberately changes nothing — in
            -- particular it does not re-capture the clock it already
            -- zeroed, which would replace the player's speed with 0
            -- (#1599). For a 'WorldSave' that reached this thread
            -- another way it is the epoch's real start.
            --
            -- 'reassertSavePause' rather than 'imposePause' because this
            -- pause is the SAVE's own (#1730): counting it as an
            -- independent source would make every autosave observe a
            -- pause it imposed itself and decline to restore.
            reassertSavePause (toWorldSimCapability env)
            -- Globals: read once, shared across every page (we're on the
            -- world thread, so no races with worldTick writes).
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
                        failTransaction env isAutosave err
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
                                -- No clock write here: 'imposePause' above
                                -- owns the (flag, visible page's clock) pair
                                -- for the whole epoch and has already zeroed
                                -- exactly this page — it resolves the
                                -- visible page by the same rule
                                -- 'visibleId' above does. A second
                                -- zeroing here was redundant on
                                -- the paired path and actively wrong on its
                                -- own: it discarded the speed it overwrote,
                                -- which is the #1599 defect. Drift while
                                -- paused is prevented for every page by
                                -- tickWorldTime, which gates advancement on
                                -- enginePausedRef and only ticks wmVisible
                                -- worlds (#42). Time scale itself is never
                                -- captured in the snapshot at all (#758: load
                                -- policy, not gameplay state — see
                                -- World.Save.Snapshot).
                                mapMode   ← readIORef (wsMapModeRef ws)
                                edits     ← readIORef (wsEditsRef ws)
                                tiles     ← readIORef (wsTilesRef ws)
                                mineDesigs ← readIORef (wsMineDesignationsRef ws)
                                constructDesigs ← readIORef
                                    (wsConstructDesignationsRef ws)
                                constructNext ← readIORef
                                    (wsConstructAttemptRef ws)
                                groundItems ← readIORef (wsGroundItemsRef ws)
                                spoilPiles ← readIORef (wsSpoilRef ws)
                                floraHarvests ← readIORef (wsFloraHarvestsRef ws)
                                chopDesigs ← readIORef (wsChopDesignationsRef ws)
                                tillDesigs ← readIORef (wsTillDesignationsRef ws)
                                cropPlots ← readIORef (wsCropPlotsRef ws)
                                plantDesigs ← readIORef (wsPlantDesignationsRef ws)
                                craftBills ← readIORef (wsCraftBillsRef ws)
                                transferOrders ← readIORef
                                    (wsTransferOrdersRef ws)
                                powerNodes ← readIORef (wsPowerNodesRef ws)
                                containerKnowledge ← readIORef
                                    (wsContainerKnowledgeRef ws)
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
                                    , pgsConstructNextAttempt = constructNext
                                    , pgsGroundItems = groundItems
                                    , pgsSpoilPiles  = spoilPiles
                                    , pgsBuildings   = buildings
                                    , pgsUnits       = units
                                    , pgsUnitSimStates = simStates
                                    , pgsFloraHarvests = floraHarvests
                                    , pgsChopDesignations = chopDesigs
                                    , pgsCraftBills  = craftBills
                                    , pgsTransferOrders = transferOrders
                                    , pgsPowerNodes  = powerNodes
                                    , pgsContainerKnowledge = containerKnowledge
                                    , pgsTillDesignations = tillDesigs
                                    , pgsCropPlots   = cropPlots
                                    , pgsPlantDesignations = plantDesigs
                                    , pgsIdentity    = identity
                                    }
                    case sequence maybePages of
                      Left err → do
                        logWarn logger CatWorld err
                        failTransaction env isAutosave err
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
                                , sgActivePage     = primaryId
                                -- Record visibility so the loaded game comes up
                                -- showing what the player last saw (#216).
                                , sgVisiblePages   = wmVisible mgr
                                , sgLiveCamera     = liveCamera
                                }
                        case captureSessionSnapshot globals pages of
                          Left errs → do
                            -- Issue #764: sort + cap this
                            -- list too, at the SAME 'integrityErrorCap' the
                            -- rest of the integrity graph uses, rather than
                            -- rendering a raw, uncapped, insertion-order
                            -- list — the same "never an arbitrary first
                            -- hash-map entry, always bounded" contract
                            -- 'capComponentErrors'/'capIntegrityErrors'
                            -- already enforce at the other boundaries.
                            let rendered = L.sort (map (T.pack ∘ show) errs)
                                total    = length rendered
                                capped   = take integrityErrorCap rendered
                                omitted  = max 0 (total - length capped)
                                trailer  =
                                    [ tshow omitted
                                        <> " additional snapshot finding(s) \
                                           \omitted (see \
                                           \World.Save.Integrity.integrityErrorCap)"
                                    | omitted > 0 ]
                                msg = "session snapshot failed validation: "
                                    <> T.intercalate "; " (capped ⧺ trailer)
                            logWarn logger CatWorld msg
                            failTransaction env isAutosave msg
                          Right snap → case capIntegrityErrors
                                              (sessionIntegrityErrors snap) of
                            report | not (null (irErrors report)) → do
                              let msg = "session snapshot failed integrity \
                                        \validation: " <> T.intercalate "; "
                                        (renderIntegrityReport report)
                              logWarn logger CatWorld msg
                              failTransaction env isAutosave msg
                            _ → do
                                -- Issue #764 (save-overhaul C3): cross-validate
                                -- every Lua-declared reference (gathered on the
                                -- SAME live snapshot saveModules.snapshotAll()
                                -- just captured) against the same known-entity
                                -- graph the load boundary uses — save and load
                                -- share one complete integrity picture, not two
                                -- independently-decided ones. Never load/save-
                                -- blocking (the #761-established tolerated-
                                -- dangling-reference contract) — logged as
                                -- diagnostics only (requirement 16).
                                let knownLua = buildKnownEntities snap
                                    -- componentVersions (issue #764):
                                    -- luaComponents already
                                    -- carries each component's just-
                                    -- snapshotted schema version, so this
                                    -- diagnostic reports the version the
                                    -- edge was actually collected against
                                    -- rather than a hardcoded placeholder.
                                    componentVersions = HM.fromList
                                        [ (lcsId c, lcsVersion c)
                                        | c ← luaComponents ]
                                    luaReport = capIntegrityErrors
                                        (luaReferenceErrors
                                            componentVersions knownLua luaRefs)
                                forM_ (renderIntegrityReport luaReport) $ \m →
                                    logWarn logger CatWorld $
                                        "saveWorld '" <> saveName
                                        <> "': integrity diagnostic: " <> m
                                -- Issue #1246: the NON-BLOCKING half of the
                                -- Haskell-side graph, on the same terms. A
                                -- transfer order whose carrier, endpoint or
                                -- item is gone is tolerated gameplay, so it is
                                -- reported HERE — after the hard check above
                                -- has already passed — and the save proceeds
                                -- with the order intact. Routing it through
                                -- 'sessionIntegrityErrors' instead would abort
                                -- the transaction, which is exactly the
                                -- distinction 'sessionIntegrityWarnings' exists
                                -- to keep.
                                let warnReport = capIntegrityErrors
                                        (sessionIntegrityWarnings snap)
                                forM_ (renderIntegrityReport warnReport) $ \m →
                                    logWarn logger CatWorld $
                                        "saveWorld '" <> saveName
                                        <> "': integrity diagnostic: " <> m
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
                                        , srmAutosave  = isJust mAutosave
                                        }
                                    meta = snapshotSaveMetadata req snap
                                -- Force the FULL encoding now, while the capture
                                -- lock is STILL held (#758 requirement 7): every
                                -- component's cereal encode must visit every
                                -- field, so this either succeeds completely right
                                -- here or throws right here — never partway
                                -- through the disk write below, after other owners
                                -- have already resumed. Anything
                                -- 'World.Save.Snapshot' left as an unevaluated
                                -- thunk (its record fields are only forced to
                                -- WHNF, not deeply) gets touched here.
                                -- A thrown exception is a capture failure, not a
                                -- disk failure: fail the transaction directly
                                -- and skip the release entirely — failSave's own
                                -- phase transition already unblocks
                                -- 'captureLocked' for every other owner.
                                encodedOrErr ← try (evaluate
                                    (encodeSessionSnapshot meta snap luaComponents))
                                case encodedOrErr of
                                  Left (e ∷ SomeException) → do
                                    let msg = "session snapshot failed to encode: "
                                            <> tshow e
                                    logWarn logger CatWorld msg
                                    failTransaction env isAutosave msg
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
                                    let luaKnownNames =
                                            HS.fromList (map lcsId luaComponents)
                                        luaRequiredNames = HS.fromList
                                            [ lcsId c
                                            | c ← luaComponents, lcsRequired c ]
                                    result ← writeSaveFiles saveName meta encoded
                                                luaKnownNames luaRequiredNames
                                    case result of
                                      Right warnings →
                                        -- #913: the transaction stays
                                        -- NON-TERMINAL until every piece of
                                        -- session state this save is going to
                                        -- touch has been touched. Completing it
                                        -- first would drop the save/load
                                        -- mutual exclusion while the autosave
                                        -- restore below still has pause and
                                        -- time scale left to write: a
                                        -- Lua-thread engine.loadSave accepted
                                        -- in that window pauses synchronously
                                        -- at acceptance, and this restore would
                                        -- then unpause or retime a session
                                        -- mid-load. 'finally' rather than a
                                        -- plain reorder so an exception in the
                                        -- restore or the event emission can
                                        -- never leave the barrier wedged open
                                        -- (the disk write already succeeded, so
                                        -- success is still the honest outcome).
                                        (do
                                            forM_ warnings $ \w →
                                                logWarn logger CatWorld $
                                                    "World saved with a cleanup \
                                                    \warning: " <> w
                                            logInfo logger CatWorld $
                                                "World saved successfully: " <> saveName
                                            -- The restore runs BEFORE the
                                            -- success event, which a
                                            -- pause-configured notification
                                            -- category may itself pause on
                                            -- (Engine.PlayerEvent.Emit imposes
                                            -- a pause of its own). That
                                            -- ordering is what makes the
                                            -- event's own result authoritative
                                            -- rather than something this
                                            -- restore could undo — and since
                                            -- #1599 that event opens a fresh
                                            -- pause epoch over the speed this
                                            -- restore just handed back, so a
                                            -- later resume returns to it
                                            -- rather than to 1x.
                                            void $ restoreAfterAutosave
                                                env logger mAutosave
                                            emitEvent env "save_load" "World.Save" $
                                                "Game saved: " <> saveName)
                                          `finally` completeTransaction env
                                      Left err →
                                        do
                                            failTransaction env isAutosave err
                                            logError logger CatWorld $
                                                "Failed to save world: " <> err

-- | #913: hand an AUTOSAVE's pre-request pause state and visible-world
--   time scale back to the player, once the transaction has actually
--   succeeded. Returns whether anything was restored.
--
--   Restores onto 'World.Save.Types.arPausedPage', the page the request
--   recorded at acceptance, rather than whatever is visible by now.
--
--   FOUR ways this deliberately does nothing:
--
--     * a MANUAL save ('Nothing') — the save path's pause is the
--       long-standing DF-style contract and is not this issue's to
--       change;
--     * the player touched pause or the time scale during the request
--       window ('arIntentGen' no longer matches
--       'playerIntentGenRef') — the player wins outright, including the
--       double-toggle case where the final BOOLEAN is unchanged, which
--       is exactly why this compares a generation rather than values;
--     * the autosave began from an already-PAUSED world — there is
--       nothing to resume, and resuming would be the autosave changing
--       gameplay;
--     * #1730: an ENGINE pause source independent of this save asserted
--       a pause during the window ('arEnginePauseGen' no longer matches
--       'Engine.Core.State.enginePauseGenRef'). A @pause: true@
--       notification landing here is a complete no-op on the flag — the
--       epoch is already open and records no owner — so without that
--       counter this restore closed the epoch out from under it and left
--       the popup announcing the pause standing over a running game. The
--       save's OWN re-assertion above does not move the counter, so a
--       plain autosave still restores.
--
--   A FAILED transaction never reaches here at all, so the existing
--   one-way "a failed save leaves you paused" safety ratchet is
--   preserved untouched.
restoreAfterAutosave
    ∷ EngineEnv → LoggerState → Maybe AutosaveRequest → IO Bool
restoreAfterAutosave _ _ Nothing = pure False
restoreAfterAutosave env logger (Just ar)
    | arPrePaused ar = pure False
    | otherwise = do
        -- The comparison and the writes are ONE critical section, under
        -- the same lock every player pause/time-scale transition takes.
        -- Read-then-write would leave a window in which the Lua thread
        -- applies a pause, sees a bump land, and has it overwritten here
        -- anyway by a value that was already stale when it was read.
        -- 'restoreIfPlayerIdle' holds that lock across the action, which
        -- is also the epoch mutex — hence the @…Held@ variants below.
        outcome ← restoreIfPlayerIdle (toWorldSimCapability env)
                                      (arIntentGen ar) $ do
          -- #1730: the SECOND reason to decline, read inside the same
          -- critical section as the generation comparison above. An
          -- engine pause and this restore both take the epoch mutex, so
          -- the two orderings are the only two there are: a pause that
          -- wins the lock is seen here and declines the restore, and one
          -- that loses it opens a fresh epoch over the speed this
          -- restore just handed back.
          engineGen ← readIORef (enginePauseGenRef env)
          if engineGen ≢ arEnginePauseGen ar then pure False else do
            -- The pre-request scale is handed to the pause epoch this
            -- save opened rather than written straight onto the page, so
            -- 'releasePauseHeld' installs it in the same scale-then-flag
            -- order every other resume uses — there is never even a
            -- momentary "unpaused at scale 0" reading of the pair, and
            -- the epoch is closed rather than left behind for the next
            -- resume to re-apply.
            --
            -- The target is 'arPausedPage' — the page this save's own
            -- pause zeroed — and deliberately NOT the currently visible
            -- one: the player can bring a different page to the front
            -- while an autosave runs, and writing the speed there would
            -- retime a page the save never paused (#1599 requirement 8).
            -- A page that is gone by now simply gets nothing, which is
            -- also what its vanished epoch does.
            forM_ (arPausedPage ar) $ \vid → do
                mgr ← readIORef (worldManagerRef env)
                forM_ (lookup vid (wmWorlds mgr)) $ \ws →
                    setPauseResumeScale ws (arPreTimeScale ar)
            releasePauseHeld (toWorldSimCapability env)
            pure True
        -- Each decline names its OWN reason: attributing an engine
        -- pause to the player would be a false statement in the log the
        -- player reads (#1730).
        case outcome of
            Nothing → logInfo logger CatWorld
                "Autosave finished, but the player changed pause or time \
                \scale while it ran -- leaving their choice in place"
            Just False → logInfo logger CatWorld
                "Autosave finished, but the game was paused again while \
                \it ran -- leaving that pause in place"
            Just True → pure ()
        pure (outcome ≡ Just True)

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

-- | Fail the transaction AND tell the player, in one place.
--
--   Every branch below this point is a TERMINAL failure of an ALREADY
--   ACCEPTED save: acceptance has paused the game, and (#913) an
--   autosave leaves it paused as the deliberate safety ratchet. A
--   failure that only logged would strand the player paused with no
--   explanation and no save — for an autosave, one they never asked for
--   at that moment and would have no reason to be looking for. Reporting
--   lives HERE rather than at each call site so a future failure branch
--   physically cannot forget it (only the storage-write branch used to
--   report, and every earlier one — page not found, missing gen params,
--   an unsnapshotable page, snapshot/integrity validation, encode — did
--   not).
--
--   @isAutosave@ only picks the wording: an unexplained \"Save failed\"
--   for a save the player never initiated reads as a bug report about
--   something they did.
failTransaction ∷ EngineEnv → Bool → Text → IO ()
failTransaction env isAutosave err = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → failSave (saveBarrierRef env) (ssRequestId s) err
    emitEvent env "save_load" "World.Save" $
        (if isAutosave then "Autosave failed: " else "Save failed: ") <> err

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
