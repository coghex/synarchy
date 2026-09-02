{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Save
    ( saveListFn
    , saveWorldFn
    , saveStatusFn
    , saveConfigFn
    , defaultSaveConfigFn
    , setSaveConfigFn
    , prepareAutosaveCycleFn
    , finalizeAutosaveRotationFn
    , loadSaveFn
    , guardAcceptedLoad
    , renderEscapedLoadException
    , acceptSaveRequest
    , loadStatusFn
    , applyLuaLoad
    , abortLuaLoad
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Control.Monad.Catch as Catch
import Control.Exception (SomeException, SomeAsyncException
                         , fromException, displayException)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Engine.Scripting.Lua.API.Save.Bridge
    ( describeLuaComponents, collectLuaComponents, prepareLuaLoad
    , applyLuaLoad, abortLuaLoad )
import Engine.Scripting.Lua.API.Save.Config
    ( pushSaveConfig, optBooleanField, optIntegerField )
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)
import Engine.PlayerEvent.Emit (emitEvent)
import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import World.Material (mergeMaterialRegistry)
import World.Save.Serialize
    (listSaves, loadWorld, sanitizeSaveName, SaveListing(..))
import World.Save.Autosave
    (prepareAutosaveCycle, finalizeAutosaveRotation, publicSaveListings)
import Engine.Core.Capability.WorldSim
    (toWorldSimCapability, withPlayerIntentHeld)
import Engine.Save.Config
    ( SaveConfig(..), loadSaveConfig, writeSaveConfig, clampSaveConfig
    , defaultSaveConfig, saveConfigDefaultPath, saveConfigLocalPath )
import World.Save.Types (SaveMetadata(..), SaveData(..), WorldPageSave(..)
                        , AutosaveRequest(..)
                        , missingDefReferences, renderMissingDefRef
                        , missingItemDefReferences, renderMissingItemDefRef
                        , missingSignificantItemReferences
                        , renderMissingSignificantItemRef
                        , missingRecipeReferences, renderMissingRecipeRef
                        , missingBillOutputItemReferences
                        , renderMissingBillOutputItemRef
                        , missingConstructDefReferences
                        , renderMissingConstructDefRef
                        , missingMaterialReferences
                        , renderMissingMaterialRef
                        , missingFloraReferences
                        , renderMissingFloraRef
                        , missingLocationDefReferences
                        , resolveLegacyLocations
                        , renderMissingLocationRef
                        , missingInfectionReferences
                        , renderMissingInfectionRef)
import Location.Instance (locationGeometryErrorText)
import Location.Types (LocationRegistry(..), LocationDef(..))
import Building.Types (BuildingManager(..))
import Unit.Types (UnitManager(..))
import Item.Types (ItemManager(..))
import Craft.Types (RecipeManager(..))
import World.Save.Integrity
    ( luaReferenceErrors
    , capIntegrityErrors, renderIntegrityReport )
import World.Types
    ( WorldCommand(..), WorldManager(wmWorlds)
    , WorldState(wsGenParamsRef, wsTimeScaleRef), visiblePage )
import World.Page.Types (WorldPageId(..))
import World.Pause (imposePause, imposePauseHeld)
import Data.IORef (readIORef, atomicModifyIORef')
import qualified Data.Set as Set
import Engine.Save.Barrier
import Engine.Load.Status
    ( LoadPhase(..), LoadStatus(..), ReconciliationFailure(..)
    , LoadStatusRef
    , beginLoad, advanceLoad, failLoad, readLoadStatus, loadInProgress )

-- | engine.listSaves() → returns a Lua table of {name, seed, worldSize, timestamp}
--   sorted newest-first by timestamp. `name` is the save-slot identity
--   (the directory under saves/). A save whose active page carries a
--   player-facing identity (#707) additionally exposes `worldName` and,
--   when one was stored, `worldGloss`; unnamed saves omit both fields.
--   `recovered` is `true` (otherwise omitted) when the listed metadata
--   came from the slot's PREVIOUS generation because its authoritative
--   generation had recoverable storage corruption (issue #762 requirement
--   8). Since #1107 the save browser renders it as a "[Recovered]" row
--   tag, beside the "[Autosave]" one — both are durable classifications
--   of the slot, never inferred from its name.
--
--   This is the PUBLIC listing boundary, and #1413 makes it the one
--   place the autosave rotation's two internal staging slots are hidden
--   (see 'World.Save.Autosave.publicSaveListings'). Applying it here
--   rather than inside 'listSaves' is deliberate twice over: rotation
--   reads that same enumeration and must keep seeing both slots, and a
--   future listing surface inherits the rule without a private filter of
--   its own — neither @scripts\/main_menu.lua@ nor
--   @scripts\/save_browser.lua@ carries one.
saveListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveListFn env = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    -- issue #761: the current Lua registry's ids widen the envelope's
    -- known-component set so a save carrying a required "lua.<module>"
    -- component never becomes unlistable merely because Haskell's own
    -- static component set doesn't recognise it. listSaves is a
    -- read-only, best-effort listing operation (unlike save/load), so a
    -- malformed live descriptor here degrades to an empty (more
    -- restrictive, never less) known-name set rather than failing the
    -- whole listing outright.
    descriptorsOrErr ← describeLuaComponents logger
    descriptors ← case descriptorsOrErr of
        Right ds → return ds
        Left err → do
            Lua.liftIO $ logWarn logger CatLua $ "listSaves: " <> err
            return []
    let luaKnownNames = HS.fromList [ name | (name, _, _) ← descriptors ]
    listed ← Lua.liftIO $ listSaves logger luaKnownNames
    -- Filter BEFORE indexing: the survivors are numbered 1..n with no
    -- hole, which is what `#`, `[1]` and `ipairs` on the Lua side
    -- already rely on.
    let saves = publicSaveListings listed
    Lua.newtable
    forM_ (zip [1..] saves) $ \(i, listing) → do
        let name = slName listing
            meta = slMetadata listing
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (-2) "name"
        Lua.pushinteger (fromIntegral $ smSeed meta)
        Lua.setfield (-2) "seed"
        Lua.pushinteger (fromIntegral $ smWorldSize meta)
        Lua.setfield (-2) "worldSize"
        Lua.pushstring (TE.encodeUtf8 $ smTimestamp meta)
        Lua.setfield (-2) "timestamp"
        forM_ (smWorldName meta) $ \wn → do
            Lua.pushstring (TE.encodeUtf8 wn)
            Lua.setfield (-2) "worldName"
        forM_ (smWorldGloss meta) $ \wg → do
            Lua.pushstring (TE.encodeUtf8 wg)
            Lua.setfield (-2) "worldGloss"
        when (slRecovered listing) $ do
            Lua.pushboolean True
            Lua.setfield (-2) "recovered"
        -- #913: the durable autosave/manual classification. Always
        -- present (never omitted-when-false like `recovered` above) so a
        -- consumer can distinguish "this build classifies saves" from
        -- "this row happens to be manual" without a version check --
        -- the save browser's label and the rotation's manual-collision
        -- refusal both read it, and a silently missing key would read as
        -- manual in exactly the case that must never be guessed.
        Lua.pushboolean (smAutosave meta)
        Lua.setfield (-2) "autosave"
        Lua.rawseti (-2) i
    return 1

-- | engine.getSaveStatus() exposes the authoritative transaction state to
-- headless diagnostics without coupling probes to log timing.
saveStatusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveStatusFn env = do
    status ← Lua.liftIO $ readSaveStatus (saveBarrierRef env)
    case status of
        Nothing → Lua.pushnil
        Just s → do
            Lua.newtable
            Lua.pushinteger (fromIntegral $ ssRequestId s)
            Lua.setfield (-2) "id"
            Lua.pushstring . TE.encodeUtf8 . T.pack . show $ ssPhase s
            Lua.setfield (-2) "phase"
            Lua.pushinteger (fromIntegral $ Set.size $ ssAcknowledged s)
            Lua.setfield (-2) "acknowledgedOwners"
            Lua.pushinteger (fromIntegral $ Set.size $ ssOwners s)
            Lua.setfield (-2) "ownerCount"
            forM_ (ssOutcome s) $ \outcome → do
                Lua.pushstring . TE.encodeUtf8 . T.pack . show $ outcome
                Lua.setfield (-2) "outcome"
    pure 1

-- | engine.getLoadStatus() — issue #763 requirement 16: exposes the
--   whole-session LOAD transaction's phase/outcome so a headless caller
--   can wait for a SPECIFIC load (by @id@) to finish rather than polling
--   stale state left behind by a previous one. Mirrors 'saveStatusFn'
--   exactly; see "Engine.Load.Status" for the phase vocabulary.
loadStatusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadStatusFn env = do
    status ← Lua.liftIO $ readLoadStatus (loadStatusRef env)
    case status of
        Nothing → Lua.pushnil
        Just s → do
            Lua.newtable
            Lua.pushinteger (fromIntegral $ lsRequestId s)
            Lua.setfield (-2) "id"
            Lua.pushstring (TE.encodeUtf8 (lsSaveName s))
            Lua.setfield (-2) "saveName"
            Lua.pushstring . TE.encodeUtf8 . T.pack . show $ lsPhase s
            Lua.setfield (-2) "phase"
            forM_ (lsOutcome s) $ \outcome → do
                Lua.pushstring . TE.encodeUtf8 . T.pack . show $ outcome
                Lua.setfield (-2) "outcome"
            -- 'phase' above is 'LoadFailed' itself once
            -- the transaction is terminal-and-aborted, which on its own
            -- says nothing about how far the attempt actually got.
            -- 'failedAtPhase' is the phase 'lsPhase' held immediately
            -- BEFORE 'failLoad' overwrote it — present only on a failed
            -- load.
            forM_ (lsFailedAtPhase s) $ \phase → do
                Lua.pushstring . TE.encodeUtf8 . T.pack . show $ phase
                Lua.setfield (-2) "failedAtPhase"
            -- Issue #1204: a post-publication reconciliation failure
            -- reports through its OWN terminal phase
            -- ('LoadReconciliationFailed') and outcome
            -- ('LoadReconciliationIncomplete'), never through
            -- 'failedAtPhase' above — that field's presence is what
            -- promises the old session survived unchanged, which a
            -- failure past the session swap cannot promise. This array
            -- (absent unless a callback actually raised) carries the
            -- unambiguous module-to-error association the flattened
            -- outcome string can only summarize: one @{module, error}@
            -- entry per failing Lua module, in broadcast order.
            unless (null (lsReconciliationFailures s)) $ do
                Lua.newtable
                forM_ (zip [1 ∷ Int ..] (lsReconciliationFailures s)) $ \(i, f) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (rfModule f))
                    Lua.setfield (-2) "module"
                    Lua.pushstring (TE.encodeUtf8 (rfError f))
                    Lua.setfield (-2) "error"
                    Lua.rawseti (-2) (fromIntegral i)
                Lua.setfield (-2) "reconciliationFailures"
    pure 1

-- | The full save/load-transaction owner set, minus
--   'SaveInput' when the input thread was never started —
--   'App.Headless' boots without one (no GLFW window to poll), so
--   requiring it unconditionally would make 'waitForOwners' time out
--   on every headless save/load waiting for an owner that can never
--   acknowledge. Shared by 'saveWorldFn' here and
--   'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged' (duplicated
--   rather than factored into "Engine.Save.Barrier" itself, which
--   'Engine.Core.State' already depends on for 'SaveBarrier' — the
--   reverse import would cycle).
saveOwnerSet ∷ EngineEnv → IO (Set.Set SaveOwner)
saveOwnerSet env = do
    inputActive ← readIORef (inputThreadActiveRef env)
    let base = Set.fromList
            [SaveLua, SaveWorld, SaveUnit, SaveBuilding, SaveCombat, SaveSimulation]
    pure $ if inputActive then Set.insert SaveInput base else base

-- | engine.saveWorld(pageId, saveName). Validates the request
--   synchronously (name, world-exists, gen-params present), then
--   collects every registered Lua module's state via
--   `scripts.lib.save_modules.snapshotAll()` and enqueues a
--   `WorldSave` command carrying the per-module envelope components to
--   the world thread.
--
--   Returns false on any validation failure (with a logged reason);
--   true once the command is queued. A REQUIRED Lua component's
--   snapshot/encode failure aborts the WHOLE save transaction
--   (issue #761 requirement 6) — the barrier is failed and the command
--   is never queued, rather than silently proceeding with partial Lua
--   state the way the pre-#761 blob map did. Disk-write failures are
--   inherently async and surface via the engine→Lua `onWorldGenLog`
--   broadcast (see `Save.hs:128-135`).
saveWorldFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveWorldFn env = do
    pageIdArg ← Lua.tostring 1
    nameArg   ← Lua.tostring 2
    -- #913: engine.saveWorld(pageId, name [, {autosave = true}]). An
    -- options TABLE rather than a bare flag so a later save-request
    -- option needs no third positional argument; a missing/non-table
    -- third argument is an ordinary MANUAL save, which is what every
    -- existing call site already passes.
    wantAutosave ← do
        isTable ← Lua.istable 3
        if not isTable then pure False else do
            _ ← Lua.getfield 3 "autosave"
            b ← Lua.toboolean (-1)
            Lua.pop 1
            pure b
    case (pageIdArg, nameArg) of
        (Just pageIdBS, Just nameBS) → do
            let saveName = TE.decodeUtf8Lenient nameBS
                pageId   = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            logger ← Lua.liftIO $ readIORef (loggerRef env)
            loading ← Lua.liftIO $ loadInProgress (loadStatusRef env)
            if loading
              then do
                Lua.liftIO $ logWarn logger CatLua $
                    "saveWorld rejected: a load transaction is already active"
                Lua.pushboolean False
              else case sanitizeSaveName saveName of
                Left err → do
                    Lua.liftIO $ do
                        logWarn logger CatLua $
                            "saveWorld rejected: " <> err
                        emitEvent env "save_load" "World.Save" $
                            "Save failed: " <> err
                    Lua.pushboolean False
                Right name → do
                    mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
                    case lookup pageId (wmWorlds mgr) of
                        Nothing → do
                            Lua.liftIO $ do
                                logWarn logger CatLua $
                                    "saveWorld: world not found: "
                                      <> unWorldPageId pageId
                                emitEvent env "save_load" "World.Save" $
                                    "Save failed: world '"
                                      <> unWorldPageId pageId
                                      <> "' not found"
                            Lua.pushboolean False
                        Just worldState → do
                            mParams ← Lua.liftIO $ readIORef
                                        (wsGenParamsRef worldState)
                            case mParams of
                                Nothing → do
                                    Lua.liftIO $ do
                                        logWarn logger CatLua $
                                            "saveWorld: world has no gen \
                                            \params: "
                                              <> unWorldPageId pageId
                                        emitEvent env "save_load"
                                            "World.Save" $
                                            "Save failed: world has no \
                                            \gen params"
                                    Lua.pushboolean False
                                Just _ → do
                                    -- A save is one ordered transaction.  The
                                    -- Lua thread is the caller and therefore
                                    -- cannot be acknowledged by another loop;
                                    -- it acknowledges only after the worker
                                    -- owners reached their tick boundary.
                                    owners ← Lua.liftIO $ saveOwnerSet env
                                    started ← Lua.liftIO $ beginSave
                                        (saveBarrierRef env) owners
                                    case started of
                                      Left err → do
                                        Lua.liftIO $ do
                                            logWarn logger CatLua err
                                            emitEvent env "save_load" "World.Save" $
                                                "Save failed: " <> err
                                        Lua.pushboolean False
                                      Right requestId → do
                                        -- #913: capture what the player was
                                        -- looking at, then impose the save's
                                        -- own pause, as ONE step -- see
                                        -- 'acceptSaveRequest'.
                                        autosaveReq ← Lua.liftIO $
                                            acceptSaveRequest env mgr wantAutosave
                                        Lua.liftIO $
                                            acknowledgeSave (saveBarrierRef env)
                                                requestId SaveLua
                                        ready ← Lua.liftIO $ waitForOwners
                                            5000000 (saveBarrierRef env) requestId
                                        case ready of
                                          Left err → do
                                            Lua.liftIO $ do
                                                failSave (saveBarrierRef env) requestId err
                                                logWarn logger CatLua err
                                                emitEvent env "save_load" "World.Save" $
                                                    "Save failed: " <> err
                                            Lua.pushboolean False
                                          Right () → do
                                            Lua.liftIO $ reachSnapshot
                                                (saveBarrierRef env) requestId
                                            componentsOrErr ← collectLuaComponents logger
                                            case componentsOrErr of
                                              Left err → do
                                                Lua.liftIO $ do
                                                    failSave (saveBarrierRef env) requestId err
                                                    logWarn logger CatLua err
                                                    emitEvent env "save_load" "World.Save" $
                                                        "Save failed: " <> err
                                                Lua.pushboolean False
                                              Right (components, luaRefs) → do
                                    -- Capture the timestamp at API
                                    -- (request) time so two saves
                                    -- queued back-to-back get distinct
                                    -- ISO timestamps even when the world
                                    -- thread later processes them in the
                                    -- same wall second. Wall-clock alone
                                    -- only shrinks the collision window
                                    -- (two saves in the same microsecond
                                    -- still tie), so we clamp each
                                    -- timestamp to strictly exceed the
                                    -- previous one by ≥1 µs via
                                    -- lastSaveTimeRef. Formatted at
                                    -- microsecond precision (%6Q → fixed
                                    -- 6-digit fraction): a ≥1 µs gap
                                    -- always bumps the µs-floor, so the
                                    -- fixed-width strings are strictly
                                    -- increasing and the lexicographic
                                    -- save-list sort is exact (#98).
                                                nowText ← Lua.liftIO $ do
                                                    now ← getCurrentTime
                                                    -- 1 µs, matching the %6Q format
                                                    -- resolution.
                                                    let epsilon = 1e-6
                                                    ts ← atomicModifyIORef'
                                                        (lastSaveTimeRef env) $ \prev →
                                                            let next = max now
                                                                  (addUTCTime epsilon prev)
                                                            in (next, next)
                                                    return $ T.pack $ formatTime
                                                        defaultTimeLocale "%FT%T%6QZ" ts
                                                Lua.liftIO $ Q.writeQueue
                                                    (worldQueue env)
                                                    (WorldSave pageId name nowText
                                                        components luaRefs
                                                        autosaveReq)
                                                Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

-- | Apply the save path's acceptance state, and (for an autosave)
--   snapshot what it is replacing — as ONE step, under the player-intent
--   lock (#913).
--
--   Two things are settled here:
--
--     * The acceptance PAUSE is a pair, not a flag, and since #1599
--       'World.Pause.imposePause' is what maintains that pair: it sets
--       'enginePausedRef' AND, on the transition into a pause, captures
--       the VISIBLE page's chosen speed before zeroing its clock,
--       because a paused world whose time-of-day keeps advancing is the
--       "half-paused world" "World.Pause" exists to make unrepresentable
--       — and because a clock zeroed with the speed captured NOWHERE is
--       how a manual save at 10x used to drop the player back to 1x on
--       resume. Every
--       failure path BETWEEN acceptance and the world thread's own
--       re-assertion (an owner-acknowledgement timeout, a required Lua
--       component failing to snapshot) is terminal and never reaches
--       it, which is why the pair has to be complete HERE — otherwise an
--       accepted autosave is left paused with a live clock, in violation
--       of its own "a failed autosave stays paused and zero-scaled"
--       ratchet.
--     * For an autosave, the pre-request pause\/scale\/generation triple
--       it restores on success. It has to be read HERE rather than on the
--       Lua scheduler's side: between a scheduler-side read and this
--       acceptance point the values could still move, and the generation
--       would then be snapshotted against the wrong instant.
--
--   Holding the intent lock across both makes the snapshot and the
--   overwrite indivisible: a player pause landing between them can no
--   longer be captured as "the pre-save state" and handed straight back
--   on success.
--
--   The visible page is resolved by 'World.State.Types.visiblePage', the
--   same rule "World.Pause" uses — the head of @wmVisible@ that is still
--   a live page — so the scale captured here, the page zeroed here, and
--   the page an eventual restore writes back to are always the same one.
--   With no visible page there is no clock at all; that case is
--   unreachable through the scheduler (which only fires in a gameplay
--   view) and is defensive only.
acceptSaveRequest
    ∷ EngineEnv → WorldManager → Bool → IO (Maybe AutosaveRequest)
acceptSaveRequest env mgr wantAutosave =
    withPlayerIntentHeld (toWorldSimCapability env) $ \gen → do
        prePaused ← readIORef (enginePausedRef env)
        -- Read BEFORE 'imposePauseHeld' zeroes it, and record WHICH page
        -- it came from: that page, not whatever is visible when the
        -- transaction finishes, is the one an autosave restore may write.
        let mVisible = visiblePage mgr
        scale ← case mVisible of
            Just (_, ws) → readIORef (wsTimeScaleRef ws)
            Nothing      → pure 0
        -- The pause is authoritative and remains set even if the barrier
        -- times out or serialization fails. 'imposePauseHeld' because
        -- this whole function already holds the epoch mutex.
        imposePauseHeld (toWorldSimCapability env)
        -- #1730: the count of pause assertions made by sources
        -- INDEPENDENT of this save. Read after 'imposePauseHeld' only
        -- for readability -- that call deliberately does not move it,
        -- for the same reason 'arIntentGen' is not bumped here: a save
        -- may not count its own pause as somebody else's.
        enginePauseGen ← readIORef (enginePauseGenRef env)
        pure $ if not wantAutosave then Nothing else Just AutosaveRequest
            { arPrePaused      = prePaused
            , arPreTimeScale   = scale
            , arPausedPage     = fst <$> mVisible
            , arIntentGen      = gen
            , arEnginePauseGen = enginePauseGen
            }

-- | engine.getSaveConfig() → {enabled=, intervalMinutes=, rotationDepth=}
--   The EFFECTIVE autosave configuration: the tracked template overlaid
--   key by key with the player's local overrides (see
--   "Engine.Save.Config"). Read from disk on each call rather than
--   cached in 'EngineEnv': the only consumers are the Lua scheduler
--   (which caches it itself and refreshes on an explicit settings
--   change) and the settings screen, so a live ref would add a piece of
--   engine state with no engine-side reader.
saveConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveConfigFn env = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    cfg ← Lua.liftIO $
        loadSaveConfig logger saveConfigDefaultPath saveConfigLocalPath
    pushSaveConfig cfg
    pure 1

-- | engine.getDefaultSaveConfig() → the same shape, but from the tracked
--   template ALONE — what the settings screen's Defaults button resets
--   to. Deliberately ignores @config\/save.local.yaml@; "defaults" that
--   folded in the player's own overrides would reset to nothing.
defaultSaveConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
defaultSaveConfigFn env = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    cfg ← Lua.liftIO $
        loadSaveConfig logger saveConfigDefaultPath "config/does-not-exist.yaml"
    pushSaveConfig cfg
    pure 1

-- | engine.setSaveConfig({enabled=, intervalMinutes=, rotationDepth=})
--   → bool. Persists to @config\/save.local.yaml@.
--
--   The table is a PATCH: any key it omits keeps its current effective
--   value, so a caller changing one setting can not accidentally rewrite
--   the other two from stale UI state. Values are clamped into range on
--   the way out ('clampSaveConfig'), so what lands on disk always decodes
--   back to exactly what was written — and only the keys that actually
--   differ from the tracked template are recorded there at all (see
--   'Engine.Save.Config.writeSaveConfig').
setSaveConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setSaveConfigFn env = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    isTable ← Lua.istable 1
    if not isTable
      then do
        Lua.liftIO $ logWarn logger CatLua
            "setSaveConfig: expected a table of settings"
        Lua.pushboolean False
        pure 1
      else do
        current ← Lua.liftIO $
            loadSaveConfig logger saveConfigDefaultPath saveConfigLocalPath
        enabled  ← optBooleanField 1 "enabled"
        interval ← optIntegerField 1 "intervalMinutes"
        depth    ← optIntegerField 1 "rotationDepth"
        let updated = clampSaveConfig current
                { scEnabled         = fromMaybe (scEnabled current) enabled
                , scIntervalMinutes =
                    fromMaybe (scIntervalMinutes current) interval
                , scRotationDepth   =
                    fromMaybe (scRotationDepth current) depth
                }
        result ← Lua.liftIO $ writeSaveConfig logger saveConfigDefaultPath
                                             saveConfigLocalPath updated
        case result of
            Right () → Lua.pushboolean True
            Left err → do
                Lua.liftIO $ logWarn logger CatLua $ "setSaveConfig: " <> err
                Lua.pushboolean False
        pure 1

-- | engine.prepareAutosaveCycle(depth) → true | false, reason
--
--   Verify that a new autosave cycle may proceed over the reserved
--   @autosave-\<n\>@ family (see "World.Save.Autosave" for the ownership
--   rules), and rotate in any generation a previous cycle published but
--   never finished rotating. Separate from @engine.saveWorld@ on
--   purpose: a refusal — a manual save squatting on one of the family's
--   names — must be reportable as an autosave FAILURE with nothing
--   touched and no save transaction ever begun.
prepareAutosaveCycleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
prepareAutosaveCycleFn env = autosaveSlotVerb env prepareAutosaveCycle

-- | engine.finalizeAutosaveRotation(depth) → true | false, reason
--
--   Rotate the generation just published to
--   'World.Save.Autosave.autosaveIncomingSlotName' into @autosave-1@,
--   ageing the rest of the family down. Called by the scheduler ONLY
--   after that publish reached a successful terminal outcome, so a
--   failed autosave can never have discarded or renumbered anything.
finalizeAutosaveRotationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
finalizeAutosaveRotationFn env = autosaveSlotVerb env finalizeAutosaveRotation

-- | Shared shape of the two slot verbs above: read the depth argument,
--   supply the live Lua component registry's ids (so a save carrying a
--   required @lua.*@ component this build knows about still LISTS, and
--   therefore still classifies), and report @true@ or @false, reason@.
autosaveSlotVerb
    ∷ EngineEnv
    → (LoggerState → HS.HashSet Text → Int → IO (Either Text ()))
    → Lua.LuaE Lua.Exception Lua.NumResults
autosaveSlotVerb env action = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    depthArg ← Lua.tointeger 1
    descriptorsOrErr ← describeLuaComponents logger
    let luaKnownNames = case descriptorsOrErr of
            Right ds → HS.fromList [ name | (name, _, _) ← ds ]
            Left _   → HS.empty
        depth = maybe (scRotationDepth defaultSaveConfig) fromIntegral depthArg
    result ← Lua.liftIO $ action logger luaKnownNames depth
    case result of
        Right () → do
            Lua.pushboolean True
            pure 1
        Left err → do
            Lua.liftIO $ logWarn logger CatWorld err
            Lua.pushboolean False
            Lua.pushstring (TE.encodeUtf8 err)
            pure 2

-- | engine.loadSave(saveName) — issue #763 (save-overhaul C2): request a
--   whole-session LOAD transaction. Everything this function does runs
--   synchronously on the Lua thread and touches no live gameplay state
--   beyond 'enginePausedRef' (requirement 3: pause synchronously at
--   acceptance, before any decode work): mutual-exclusion against a
--   concurrent save/load, request acceptance ('Engine.Load.Status.beginLoad'),
--   storage-source selection + envelope validation + Haskell component
--   decode/migration + snapshot assembly (all performed by 'loadWorld',
--   issues #759-#762), gameplay content-reference validation (missing
--   defs reject the load outright), and Lua-component prepare/validate
--   ('saveModules.prepareLoad', issue #761 requirement 11 — no live Lua
--   mutation yet). Once every one of those succeeds, the expensive
--   per-page reconstruction (chunk gen, zoom cache, ...) is handed to the
--   world thread as a 'WorldLoadTransaction' — this call returns before
--   that finishes; poll 'engine.getLoadStatus()' for completion
--   (requirement 16). A failure at any step here rejects the load with
--   nothing touched beyond the pause (requirement 15: the old session
--   stays complete and usable).
loadSaveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadSaveFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Nothing → Lua.pushboolean False >> return 1
        Just nameBS → do
            let saveName = TE.decodeUtf8Lenient nameBS
            logger ← Lua.liftIO $ readIORef (loggerRef env)
            -- Requirement 1: a load and a save must never overlap, for
            -- the load's WHOLE duration (not just its brief publish
            -- window) — see "Engine.Load.Status"'s haddock for why this
            -- is a plain reject rather than the barrier itself.
            saving ← Lua.liftIO $ saveInProgress (saveBarrierRef env)
            loading ← Lua.liftIO $ loadInProgress (loadStatusRef env)
            if saving ∨ loading
              then do
                Lua.liftIO $ logWarn logger CatWorld $
                    "loadSave rejected for '" <> saveName <> "': a "
                    <> (if saving then "save" else "load")
                    <> " transaction is already active"
                Lua.pushboolean False
                return 1
              else do
                begun ← Lua.liftIO $ beginLoad (loadStatusRef env) saveName
                case begun of
                  Left err → do
                    Lua.liftIO $ logWarn logger CatWorld $
                        "loadSave rejected for '" <> saveName <> "': " <> err
                    Lua.pushboolean False
                  Right requestId → do
                    -- #2162: from here until the transaction is handed
                    -- to the world thread, the request is ACCEPTED and
                    -- only this thread can end it — so a Haskell
                    -- exception escaping any step must terminalize it
                    -- ('guardAcceptedLoad') instead of leaving it parked
                    -- non-terminal forever. The single Lua result is
                    -- pushed OUTSIDE the guarded interval: a successful
                    -- hand-off is the last effect inside it, so nothing
                    -- after the world thread owns the transaction can
                    -- retroactively fail it.
                    accepted ← guardAcceptedLoad (loadStatusRef env) logger
                                                 requestId saveName $ do
                        -- Pause synchronously at acceptance (requirement 3),
                        -- before the potentially slower decode/validate work
                        -- below. A failed load leaves this pause in place —
                        -- deliberately not restored on any failure path.
                        Lua.liftIO $ do
                            imposePause (toWorldSimCapability env)
                            advanceLoad (loadStatusRef env) requestId LoadPaused
                        descriptorsOrErr ← describeLuaComponents logger
                        case descriptorsOrErr of
                            Left err → do
                                Lua.liftIO $ do
                                    logWarn logger CatWorld $
                                        "loadSave rejected for '" <> saveName
                                        <> "': " <> err
                                    failLoad (loadStatusRef env) requestId err
                                pure False
                            Right descriptors →
                                continueLoad env logger requestId saveName
                                             descriptors
                    Lua.pushboolean accepted
                return 1

-- | #2162: the exception boundary of an ACCEPTED load. Runs the
--   Lua-thread half of the transaction — everything between
--   'beginLoad' returning a request id and 'Q.writeQueue' handing the
--   'WorldLoadTransaction' to the world thread — and guarantees the
--   transaction reaches a terminal state if a Haskell exception escapes
--   it.
--
--   Before this existed, one bare IO site inside that interval was
--   enough to strand the whole engine: a legacy flat-file save whose
--   'BS.readFile' threw (permission error, file replaced by a special
--   file, vanished between the existence check and the read) escaped
--   'loadWorld' as an 'IOException', so 'failLoad' never ran.
--   'Engine.Scripting.Lua.API.Internal.registerLuaFunction' turned it
--   into a Lua error — the Lua thread survived — but the status stayed
--   at 'LoadPaused' with no outcome, 'loadInProgress' kept answering
--   'True', and every later @engine.saveWorld@ / @engine.loadSave@ was
--   rejected as "a load transaction is already active" until the
--   process restarted.
--
--   The taxonomy is exactly 'registerLuaFunction''s, so the two guards
--   compose rather than disagree:
--
--     * A 'SomeAsyncException' ('ThreadKilled' included) propagates
--       UNCHANGED and is not converted into a load failure — shutdown's
--       @killThread@ keeps working, and an interrupted transaction is
--       not reported as a decode failure it never had.
--     * A 'Lua.Exception' keeps its HsLua propagation semantics
--       (re-thrown for hslua's own richer conversion) — but the
--       transaction is terminalized FIRST, so an accepted request can
--       never be left non-terminal by it.
--     * Every other synchronous exception terminalizes the request and
--       yields the same single @false@ result every other rejected load
--       returns; @engine.loadSave@ never raises a Lua error for it.
--
--   Terminalizing means 'failLoad' for THIS request, recorded before the
--   fallible logging that follows it, against whatever phase the
--   interval had reached (the same "retain real progress" rule the
--   'Left' path follows), with a diagnostic naming the save and carrying
--   the exception's own 'displayException' text
--   ('renderEscapedLoadException'). 'failLoad' is a no-op once the
--   request already has an outcome, so a failure that was already
--   recorded through the ordinary 'Left' path is never overwritten.
--
--   The pause imposed at acceptance is deliberately left in place, as on
--   every other failure path (@docs/persistence_contract.md@: a failed
--   load leaves the old session unchanged and paused).
--
--   Exported so the headless suite can drive the boundary with injected
--   exceptions (an 'ErrorCall', a 'ThreadKilled', a 'Lua.Exception')
--   without needing a production site that raises each one.
guardAcceptedLoad
    ∷ LoadStatusRef → LoggerState → Int → Text
    → Lua.LuaE Lua.Exception Bool → Lua.LuaE Lua.Exception Bool
guardAcceptedLoad statusRef logger requestId saveName action =
    action `Catch.catch` handler
  where
    handler ∷ SomeException → Lua.LuaE Lua.Exception Bool
    handler e
        | Just (ae ∷ SomeAsyncException) ← fromException e = Catch.throwM ae
        | Just (le ∷ Lua.Exception) ← fromException e = do
            terminalize e
            Catch.throwM le
        | otherwise = do
            terminalize e
            pure False
    terminalize e = Lua.liftIO $ do
        let diag = renderEscapedLoadException saveName e
        failLoad statusRef requestId diag
        logWarn logger CatWorld ("loadSave failed: " <> diag)

-- | The 'LoadAborted' diagnostic 'guardAcceptedLoad' records: names the
--   save whose load the exception ended and carries the exception's own
--   rendering — for an 'IOException' that is the path, the failing
--   operation and the OS error text.
renderEscapedLoadException ∷ Text → SomeException → Text
renderEscapedLoadException saveName e =
    "unhandled exception while loading '" <> saveName <> "': "
        <> T.pack (displayException e)

-- | Continue 'loadSaveFn' once the current Lua registry's component
--   descriptors are known (issue #761): split out so a
--   malformed descriptor list can reject the load in 'loadSaveFn' BEFORE
--   this ever runs, rather than proceeding with an incomplete
--   known/required id set. Returns the single boolean @engine.loadSave@
--   answers with — 'True' only once the transaction has been handed to
--   the world thread — rather than pushing it itself, so the push
--   happens outside 'guardAcceptedLoad''s protected interval (#2162).
continueLoad
    ∷ EngineEnv → LoggerState → Int → Text → [(Text, Word32, Bool)]
    → Lua.LuaE Lua.Exception Bool
continueLoad env logger requestId saveName descriptors = do
    let luaKnownNames    = HS.fromList [ n | (n, _, _)   ← descriptors ]
        luaRequiredNames = HS.fromList [ n | (n, _, req) ← descriptors, req ]
    result ← Lua.liftIO $
        loadWorld logger saveName luaKnownNames luaRequiredNames
    case result of
        -- Retain whichever phase 'loadWorld' actually
        -- reached before failing, rather than jumping straight from
        -- 'LoadPaused' to 'LoadFailed' regardless of real progress.
        Left (phase, err) → do
            Lua.liftIO $ do
                logWarn logger CatWorld $
                    "loadSave failed for '" <> saveName <> "': " <> err
                advanceLoad (loadStatusRef env) requestId phase
                failLoad (loadStatusRef env) requestId err
            pure False
        Right (saveData, luaComponents, isMigratedLegacyBaseline) → do
            -- 'loadWorld' already selected the storage generation,
            -- validated the envelope, decoded + migrated every Haskell
            -- component, and assembled + cross-validated the complete
            -- session snapshot (issues #759-#762) — those phases are
            -- already behind us by the time it returns.
            Lua.liftIO $ mapM_ (advanceLoad (loadStatusRef env) requestId)
                [ LoadSourceSelected, LoadEnvelopeValidated
                , LoadComponentsDecoded, LoadComponentsMigrated
                , LoadSnapshotAssembled ]
            -- #760 req. 9 (extended to every gameplay content
            -- reference, not just building/unit defs; issue #763
            -- extends it again to material ids, flora ids,
            -- location-overlay ids, and wound-infection ids — the
            -- approved issue's own acceptance criteria names "material"
            -- explicitly alongside unit/item/building/recipe, and flora
            -- species / placed locations / wound infections all drive gameplay
            -- the same way): validate every saved content-definition
            -- reference against the currently-registered defs BEFORE
            -- publishing ANY live state. A missing gameplay DEFINITION
            -- rejects the COMPLETE load with a clear error naming
            -- what's missing (requirement 9: never silently prune
            -- affected entities). (Missing visual ASSETS stay a soft
            -- fallback, not gated here — only definitions. Equipment
            -- slot-id keys remain a documented, pre-existing,
            -- out-of-scope gap per docs/persistence_state_inventory.md
            -- §9.)
            bm ← Lua.liftIO $ readIORef (buildingManagerRef env)
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            im ← Lua.liftIO $ readIORef (itemManagerRef env)
            rm ← Lua.liftIO $ readIORef (recipeManagerRef env)
            -- The material registry is otherwise only
            -- populated by World.Thread.Command.Init's "Step 0.5" (part
            -- of world.init) — a headless boot that goes straight to
            -- engine.loadSave with no prior world.init in the SAME
            -- process would see an entirely empty registry here (every
            -- id but air reporting as "unknown"). Built OFF TO THE
            -- SIDE, never written to the live materialRegistryRef here
            -- (this runs before the load is even known
            -- to succeed — writing it live now would discard any
            -- runtime/custom material registrations the OLD, still-
            -- live session had if THIS load later gets rejected by one
            -- of the OTHER missing-def checks below, or by staging
            -- itself). Threaded through WorldLoadTransaction instead so
            -- staging validates and builds against this exact registry,
            -- and "World.Load.Publish" is the sole point it ever
            -- reaches the live ref, same as every other piece of
            -- session state.
            --
            -- A from-disk-only rebuild silently
            -- dropped whatever the LIVE session had ALREADY registered
            -- at runtime — world.init's own base pass (irrelevant here,
            -- since it registers the exact same data/materials set this
            -- rebuild does) but ALSO any engine.loadMaterialYaml custom
            -- registration, which lives ONLY in materialRegistryRef,
            -- never on disk under data/materials. A save referencing a
            -- valid custom material was rejected as "unknown", and even
            -- a successful base-only load discarded the live
            -- registrations on publish. Merge the current live registry
            -- ON TOP of the freshly-rebuilt base one — live/custom
            -- registrations win on any id collision (the same
            -- "newest registration wins" rule loadMaterialYamlFn's own
            -- live-registry fold already follows) — so validation sees
            -- both, and a successful publish preserves the live
            -- registrations exactly as they were.
            baseMatReg ← Lua.liftIO $ loadPopulatedMaterialRegistry logger "data/materials"
            liveMatReg ← Lua.liftIO $ readIORef (materialRegistryRef env)
            let matReg = mergeMaterialRegistry baseMatReg liveMatReg
            floraCat ← Lua.liftIO $ readIORef (floraCatalogRef env)
            locReg ← Lua.liftIO $ readIORef (locationDefsRef env)
            infMgr ← Lua.liftIO $ readIORef (infectionManagerRef env)
            let buildingDefs = HM.keysSet (bmDefs bm)
                locationDefIds = HS.fromList (map ldId (lrDefs locReg))
                pages = [ (wpsPageId w, w) | w ← sdWorlds saveData ]
                missing = missingDefReferences
                    buildingDefs (HM.keysSet (umDefs um))
                    [ (wpsPageId w, wpsBuildings w, wpsUnits w)
                    | w ← sdWorlds saveData ]
                missingItems =
                    missingItemDefReferences (HM.keysSet (imDefs im)) pages
                -- #917: an UNSPAWNED significant obligation names the
                -- item the next chunk load will try to spawn. If that
                -- def is gone the spawn fails on every attempt and the
                -- location can never clear, so the save is refused here
                -- rather than published into that state — the load-path
                -- counterpart of the authoring-time rejection in
                -- 'Engine.Asset.YamlLocations.significantItemErrors'.
                missingSignificant =
                    missingSignificantItemReferences
                        (HM.keysSet (imDefs im)) pages
                missingRecipes =
                    missingRecipeReferences (HM.keysSet (rmDefs rm)) pages
                missingBillOutputItems =
                    missingBillOutputItemReferences
                        (HM.keysSet (imDefs im)) pages
                missingConstruct =
                    missingConstructDefReferences buildingDefs pages
                missingMaterials =
                    missingMaterialReferences matReg pages
                missingFlora =
                    missingFloraReferences floraCat pages
                missingLocations =
                    missingLocationDefReferences locationDefIds pages
                missingInfections =
                    missingInfectionReferences infMgr pages
                allMissing = length missing + length missingItems
                    + length missingSignificant
                    + length missingRecipes
                    + length missingBillOutputItems
                    + length missingConstruct
                    + length missingMaterials
                    + length missingFlora
                    + length missingLocations
                    + length missingInfections
                allMessages =
                    map renderMissingDefRef missing
                    ⧺ map renderMissingItemDefRef missingItems
                    ⧺ map renderMissingSignificantItemRef missingSignificant
                    ⧺ map renderMissingRecipeRef missingRecipes
                    ⧺ map renderMissingBillOutputItemRef
                          missingBillOutputItems
                    ⧺ map renderMissingConstructDefRef missingConstruct
                    ⧺ map renderMissingMaterialRef missingMaterials
                    ⧺ map renderMissingFloraRef missingFlora
                    ⧺ map renderMissingLocationRef missingLocations
                    ⧺ map renderMissingInfectionRef missingInfections
            -- Advance to the content-validation
            -- checkpoint BEFORE running the gate below, not only once it
            -- succeeds — a failure inside it (any of the missing-*
            -- checks folded into allMissing) previously left lsPhase at
            -- whatever the PRIOR checkpoint was (LoadSnapshotAssembled),
            -- so engine.getLoadStatus().failedAtPhase misreported a
            -- content-validation failure as having happened one phase
            -- earlier than it actually did. Both branches below now see
            -- the phase already advanced.
            Lua.liftIO $ advanceLoad (loadStatusRef env) requestId LoadContentValidated
            if allMissing > 0
              then do
                let msg = tshow allMissing
                        <> " saved entit" <> (if allMissing ≡ 1
                                                then "y references a"
                                                else "ies reference")
                        <> " gameplay definition no longer registered — "
                        <> "aborting the entire load (nothing changed): "
                        <> T.intercalate "; " allMessages
                Lua.liftIO $ do
                    logWarn logger CatWorld $
                        "loadSave rejected for '" <> saveName <> "': " <> msg
                    failLoad (loadStatusRef env) requestId msg
                pure False
              else case traverse (resolveLegacyLocations locReg)
                                 (sdWorlds saveData) of
                -- #1796: the legacy reconstruction below builds
                -- instance geometry through the checked construction,
                -- so an out-of-envelope saved overlay coordinate is a
                -- LOAD REJECTION here rather than a wrapped box that
                -- reaches staging. Same terms as the def-reference gate
                -- above: nothing has been staged or published yet, so
                -- the live session is untouched.
                Left err → do
                    let msg = "a saved page's location geometry is not \
                              \representable — aborting the entire load \
                              \(nothing changed): "
                              <> locationGeometryErrorText err
                    Lua.liftIO $ do
                        logWarn logger CatWorld $
                            "loadSave rejected for '" <> saveName <> "': " <> msg
                        failLoad (loadStatusRef env) requestId msg
                    pure False
                Right resolvedWorlds → do
                    -- #911: a pre-instance-identity save carries its
                    -- locations as per-chunk discovered / contents-spawned
                    -- sets with no instance table. Turning those into
                    -- instances needs each definition's bounds / label
                    -- (since #1230 there is no margin to resolve), which no
                    -- component decoder can reach — so the
                    -- pure decode left them PENDING and they are resolved
                    -- HERE, against the same registry the check above just
                    -- validated every location id against, before anything
                    -- is staged or published. Total and idempotent: a save
                    -- already carrying instances (or a page with no
                    -- locations at all) passes through untouched, so stored
                    -- instance state is never overwritten from a definition
                    -- edited since placement.
                    let resolvedSaveData = saveData
                            { sdWorlds = resolvedWorlds }
                    -- issue #761 requirement 11: decode + migrate +
                    -- component-locally-validate EVERY registered Lua
                    -- component before touching any live Lua state. Any
                    -- failure aborts the whole load (nothing has changed
                    -- yet), exactly like the def-reference check above.
                    -- issue #766 requirement 5: a recognized pre-#760
                    -- compatibility migration predates every Lua-owned
                    -- persistent component (luaComponents is always empty
                    -- here), so isMigratedLegacyBaseline tells
                    -- save_modules.prepareLoad to supply each currently-
                    -- required module's own empty-state default instead of
                    -- hard-failing on "missing".
                    -- issue #900: the restored session's entity context,
                    -- computed ONCE here and used for two things — handed to
                    -- prepareLoad so each component's apply() can resolve its
                    -- rows' ownership (it is stashed for the later applyAll),
                    -- and reused below for the reference-edge diagnostics
                    -- that were previously its only consumer.
                    let known = knownEntitiesFromSaveData resolvedSaveData
                    prepared ← prepareLuaLoad logger requestId luaComponents
                                              isMigratedLegacyBaseline known
                    case prepared of
                      Left err → do
                        Lua.liftIO $ do
                            logWarn logger CatWorld $
                                "loadSave rejected for '" <> saveName
                                <> "': " <> err
                            failLoad (loadStatusRef env) requestId err
                        pure False
                      Right luaRefs → do
                        Lua.liftIO $ do
                            -- Issue #764 (save-overhaul C3): cross-validate
                            -- every Lua-declared reference against this
                            -- load's real entity sets. Never load-blocking
                            -- (the #761-established tolerated-dangling-
                            -- reference contract — see
                            -- "World.Save.Integrity"'s haddock) — logged as
                            -- diagnostics only (requirement 16).
                            let -- componentVersions (issue #764):
                                -- 'descriptors' is this SAME load's
                                -- current Lua registry ({id,version,required}),
                                -- already threaded into 'continueLoad' above --
                                -- reused here rather than re-deriving it, so
                                -- each diagnostic's version matches the reader
                                -- that actually decoded its edge.
                                componentVersions = HM.fromList
                                    [ (n, v) | (n, v, _) ← descriptors ]
                                report = capIntegrityErrors
                                    (luaReferenceErrors componentVersions known luaRefs)
                            forM_ (renderIntegrityReport report) $ \msg →
                                logWarn logger CatWorld $
                                    "loadSave '" <> saveName
                                    <> "': integrity diagnostic: " <> msg
                            -- Hand off the expensive per-page reconstruction
                            -- to the world thread (World.Load.Stage) — it
                            -- touches no live ref (requirement 6), so
                            -- nothing here needs to wait for it.
                            Q.writeQueue (worldQueue env)
                                (WorldLoadTransaction requestId resolvedSaveData matReg)
                        pure True
