{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Save
    ( saveListFn
    , saveWorldFn
    , saveStatusFn
    , loadSaveFn
    , loadStatusFn
    , applyLuaLoad
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Engine.PlayerEvent.Emit (emitEvent)
import World.Save.Serialize
    (listSaves, loadWorld, sanitizeSaveName, SaveListing(..))
import World.Save.Types (SaveMetadata(..), SaveData(..), WorldPageSave(..)
                        , missingDefReferences, renderMissingDefRef
                        , missingItemDefReferences, renderMissingItemDefRef
                        , missingRecipeReferences, renderMissingRecipeRef
                        , missingBillOutputItemReferences
                        , renderMissingBillOutputItemRef
                        , missingConstructDefReferences
                        , renderMissingConstructDefRef)
import Building.Types (BuildingManager(..))
import Unit.Types (UnitManager(..))
import Item.Types (ItemManager(..))
import Craft.Types (RecipeManager(..))
import World.Types
    (WorldCommand(..), WorldManager(wmWorlds), WorldState(wsGenParamsRef))
import World.Page.Types (WorldPageId(..))
import World.Thread.Helpers (unWorldPageId)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Set as Set
import Engine.Save.Barrier
import Engine.Load.Status
    ( LoadPhase(..), LoadStatus(..), LoadOutcome(..)
    , beginLoad, advanceLoad, failLoad, readLoadStatus, loadInProgress )

-- | engine.listSaves() → returns a Lua table of {name, seed, worldSize, timestamp}
--   sorted newest-first by timestamp. `name` is the save-slot identity
--   (the directory under saves/). A save whose active page carries a
--   player-facing identity (#707) additionally exposes `worldName` and,
--   when one was stored, `worldGloss`; unnamed saves omit both fields.
--   `recovered` is `true` (otherwise omitted) when the listed metadata
--   came from the slot's PREVIOUS generation because its authoritative
--   generation had recoverable storage corruption (issue #762 requirement
--   8) — a machine-readable status; no save/load UI change consumes it.
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
    saves ← Lua.liftIO $ listSaves logger luaKnownNames
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
    pure 1

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
                                    started ← Lua.liftIO $ beginSave
                                        (saveBarrierRef env)
                                        (Set.fromList [SaveLua, SaveWorld,
                                          SaveUnit, SaveBuilding, SaveCombat,
                                          SaveSimulation])
                                    case started of
                                      Left err → do
                                        Lua.liftIO $ do
                                            logWarn logger CatLua err
                                            emitEvent env "save_load" "World.Save" $
                                                "Save failed: " <> err
                                        Lua.pushboolean False
                                      Right requestId → do
                                        Lua.liftIO $ do
                                            -- The pause is authoritative and
                                            -- remains set even if the barrier
                                            -- times out or serialization fails.
                                            writeIORef (enginePausedRef env) True
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
                                              Right components → do
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
                                                    (WorldSave pageId name nowText components)
                                                Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

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
                    -- Pause synchronously at acceptance (requirement 3),
                    -- before the potentially slower decode/validate work
                    -- below. A failed load leaves this pause in place —
                    -- deliberately not restored on any failure path.
                    Lua.liftIO $ do
                        writeIORef (enginePausedRef env) True
                        advanceLoad (loadStatusRef env) requestId LoadPaused
                    descriptorsOrErr ← describeLuaComponents logger
                    case descriptorsOrErr of
                        Left err → do
                            Lua.liftIO $ do
                                logWarn logger CatWorld $
                                    "loadSave rejected for '" <> saveName
                                    <> "': " <> err
                                failLoad (loadStatusRef env) requestId err
                            Lua.pushboolean False
                        Right descriptors →
                            continueLoad env logger requestId saveName descriptors
                return 1

-- | Continue 'loadSaveFn' once the current Lua registry's component
--   descriptors are known (issue #761 round-4 review): split out so a
--   malformed descriptor list can reject the load in 'loadSaveFn' BEFORE
--   this ever runs, rather than proceeding with an incomplete
--   known/required id set.
continueLoad
    ∷ EngineEnv → LoggerState → Int → Text → [(Text, Word32, Bool)]
    → Lua.LuaE Lua.Exception ()
continueLoad env logger requestId saveName descriptors = do
    let luaKnownNames    = HS.fromList [ n | (n, _, _)   ← descriptors ]
        luaRequiredNames = HS.fromList [ n | (n, _, req) ← descriptors, req ]
    result ← Lua.liftIO $
        loadWorld logger saveName luaKnownNames luaRequiredNames
    case result of
        -- Round 2 review: retain whichever phase 'loadWorld' actually
        -- reached before failing, rather than jumping straight from
        -- 'LoadPaused' to 'LoadFailed' regardless of real progress.
        Left (phase, err) → do
            Lua.liftIO $ do
                logWarn logger CatWorld $
                    "loadSave failed for '" <> saveName <> "': " <> err
                advanceLoad (loadStatusRef env) requestId phase
                failLoad (loadStatusRef env) requestId err
            Lua.pushboolean False
        Right (saveData, luaComponents) → do
            -- 'loadWorld' already selected the storage generation,
            -- validated the envelope, decoded + migrated every Haskell
            -- component, and assembled + cross-validated the complete
            -- session snapshot (issues #759-#762) — those phases are
            -- already behind us by the time it returns.
            Lua.liftIO $ mapM_ (advanceLoad (loadStatusRef env) requestId)
                [ LoadSourceSelected, LoadEnvelopeValidated
                , LoadComponentsDecoded, LoadComponentsMigrated
                , LoadSnapshotAssembled ]
            -- #760 req. 9 (round 8 extends this to every gameplay
            -- content reference, not just building/unit defs):
            -- validate every saved content-definition reference against
            -- the currently-registered defs BEFORE publishing ANY live
            -- state. A missing gameplay DEFINITION rejects the COMPLETE
            -- load with a clear error naming what's missing (requirement
            -- 9: never silently prune affected entities). (Missing
            -- visual ASSETS stay a soft fallback, not gated here — only
            -- definitions. FloraId/location-overlay ids, MaterialId
            -- references, and equipment slot-id keys all remain a
            -- documented, pre-existing, out-of-scope gap per
            -- docs/persistence_state_inventory.md §9.)
            bm ← Lua.liftIO $ readIORef (buildingManagerRef env)
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            im ← Lua.liftIO $ readIORef (itemManagerRef env)
            rm ← Lua.liftIO $ readIORef (recipeManagerRef env)
            let buildingDefs = HM.keysSet (bmDefs bm)
                pages = [ (wpsPageId w, w) | w ← sdWorlds saveData ]
                missing = missingDefReferences
                    buildingDefs (HM.keysSet (umDefs um))
                    [ (wpsPageId w, wpsBuildings w, wpsUnits w)
                    | w ← sdWorlds saveData ]
                missingItems =
                    missingItemDefReferences (HM.keysSet (imDefs im)) pages
                missingRecipes =
                    missingRecipeReferences (HM.keysSet (rmDefs rm)) pages
                missingBillOutputItems =
                    missingBillOutputItemReferences
                        (HM.keysSet (imDefs im)) pages
                missingConstruct =
                    missingConstructDefReferences buildingDefs pages
                allMissing = length missing + length missingItems
                    + length missingRecipes
                    + length missingBillOutputItems
                    + length missingConstruct
                allMessages =
                    map renderMissingDefRef missing
                    ⧺ map renderMissingItemDefRef missingItems
                    ⧺ map renderMissingRecipeRef missingRecipes
                    ⧺ map renderMissingBillOutputItemRef
                          missingBillOutputItems
                    ⧺ map renderMissingConstructDefRef missingConstruct
            if allMissing > 0
              then do
                let msg = T.pack (show allMissing)
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
                Lua.pushboolean False
              else do
                -- issue #761 requirement 11: decode + migrate +
                -- component-locally-validate EVERY registered Lua
                -- component before touching any live Lua state. Any
                -- failure aborts the whole load (nothing has changed
                -- yet), exactly like the def-reference check above.
                prepared ← prepareLuaLoad logger luaComponents
                case prepared of
                  Left err → do
                    Lua.liftIO $ do
                        logWarn logger CatWorld $
                            "loadSave rejected for '" <> saveName
                            <> "': " <> err
                        failLoad (loadStatusRef env) requestId err
                    Lua.pushboolean False
                  Right () → do
                    Lua.liftIO $ do
                        advanceLoad (loadStatusRef env) requestId LoadContentValidated
                        -- Hand off the expensive per-page reconstruction
                        -- to the world thread (World.Load.Stage) — it
                        -- touches no live ref (requirement 6), so
                        -- nothing here needs to wait for it.
                        Q.writeQueue (worldQueue env)
                            (WorldLoadTransaction requestId saveData)
                    Lua.pushboolean True

-- | Pop the Lua error message at the top of the stack and log it
--   via the engine logger. Used by every save_modules.* bridge call
--   below to surface pcall failures that would otherwise be silent.
luaLogPcallError ∷ LoggerState → Text → Lua.LuaE Lua.Exception ()
luaLogPcallError logger ctx = do
    err ← Lua.tostring (-1)
    Lua.pop 1
    Lua.liftIO $ logWarn logger CatLua $
        ctx <> ": " <> maybe "<no message>" TE.decodeUtf8Lenient err

-- | require("scripts.lib.save_modules") and call one of its functions,
--   pushing the arguments @pushArgs@ leaves on the stack (must push
--   exactly @nargs@ values, in order) and requesting exactly ONE Lua
--   return value. On success ('True'), that one value is left on the
--   stack top for the caller to read and pop; on any failure (require
--   failing, the function missing/not-a-function, or the call itself
--   crashing), nothing is left on the stack and 'False' is returned,
--   having already logged the reason via the engine logger.
callSaveModules1
    ∷ LoggerState → Text → Lua.NumArgs → Lua.LuaE Lua.Exception ()
    → Lua.LuaE Lua.Exception Bool
callSaveModules1 logger fnName nargs pushArgs = do
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    requireStatus ← Lua.pcall 1 1 Nothing
    case requireStatus of
        Lua.OK → do
            _ ← Lua.getfield (-1) (Lua.Name (TE.encodeUtf8 fnName))
            isFun ← Lua.isfunction (-1)
            if not isFun
                then do
                    Lua.pop 2  -- non-function value + module table
                    Lua.liftIO $ logWarn logger CatLua $
                        "save_modules." <> fnName <> " is not a function"
                    return False
                else do
                    pushArgs
                    callStatus ← Lua.pcall nargs 1 Nothing
                    case callStatus of
                        Lua.OK → do
                            Lua.remove (-2)  -- drop the module table, keep the 1 result
                            return True
                        _ → do
                            luaLogPcallError logger
                                ("save_modules." <> fnName <> " crashed")
                            Lua.pop 1  -- module table
                            return False
        _ → do
            luaLogPcallError logger
                "require scripts.lib.save_modules failed"
            return False

-- | Same as 'callSaveModules1', but for a call with no arguments and no
--   return value the caller cares about (@applyAll@).
callSaveModules0 ∷ LoggerState → Text → Lua.LuaE Lua.Exception Bool
callSaveModules0 logger fnName = do
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    requireStatus ← Lua.pcall 1 1 Nothing
    case requireStatus of
        Lua.OK → do
            _ ← Lua.getfield (-1) (Lua.Name (TE.encodeUtf8 fnName))
            isFun ← Lua.isfunction (-1)
            if not isFun
                then do
                    Lua.pop 2
                    Lua.liftIO $ logWarn logger CatLua $
                        "save_modules." <> fnName <> " is not a function"
                    return False
                else do
                    callStatus ← Lua.pcall 0 0 Nothing
                    case callStatus of
                        Lua.OK → do
                            Lua.pop 1  -- module table
                            return True
                        _ → do
                            luaLogPcallError logger
                                ("save_modules." <> fnName <> " crashed")
                            Lua.pop 1
                            return False
        _ → do
            luaLogPcallError logger
                "require scripts.lib.save_modules failed"
            return False

-- | Read every element of the Lua array at the top of the stack (NOT
--   popped) via @readElem@, which must read whatever is at the NEW
--   stack top (the current element, already pushed) and leave the
--   stack depth unchanged relative to its own entry. FAILS CLOSED
--   (issue #761 round-4 review): a malformed element ('Nothing') aborts
--   the whole read with 'Left' rather than being silently skipped —
--   the two callers this backs (component descriptors, snapshot
--   payloads) can each carry a REQUIRED component's own record, and a
--   value HsLua can't convert (e.g. a Lua @version@ outside Word32's
--   range) must not be able to make that component quietly vanish from
--   a save/load instead of failing it outright.
readLuaArrayAt
    ∷ Lua.LuaE Lua.Exception (Maybe a)
    → Lua.LuaE Lua.Exception (Either Text [a])
readLuaArrayAt readElem = do
    n ← Lua.rawlen (-1)
    go 1 (fromIntegral n) []
  where
    go i n acc
        | i > (n ∷ Int) = return (Right (reverse acc))
        | otherwise = do
            _ ← Lua.rawgeti (-1) (fromIntegral i)
            mv ← readElem
            Lua.pop 1
            case mv of
                Nothing → return (Left ("malformed array element at index "
                    <> T.pack (show i)))
                Just v  → go (i + 1) n (v : acc)

-- | Read {id=string, version=number, required=boolean} from the table
--   at the top of the stack.
readComponentDescriptorField
    ∷ Lua.LuaE Lua.Exception (Maybe (Text, Word32, Bool))
readComponentDescriptorField = do
    _ ← Lua.getfield (-1) "id"
    midB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "version"
    mver ← Lua.tointeger (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "required"
    req ← Lua.toboolean (-1)
    Lua.pop 1
    case (midB, mver) of
        (Just idb, Just ver) →
            return (Just (TE.decodeUtf8Lenient idb, fromIntegral ver, req))
        _ → return Nothing

-- | Call @saveModules.describeAll()@ (issue #761): every currently-
--   registered persistent Lua component's (name, version, required),
--   used to build the envelope's dynamic known/required id sets before
--   both encode and decode. Returns 'Left' on any error (require
--   failing, describeAll missing/crashing, or a malformed descriptor —
--   round-4 review "fail closed" — the caller decides whether that's
--   fatal for its own operation).
describeLuaComponents
    ∷ LoggerState
    → Lua.LuaE Lua.Exception (Either Text [(Text, Word32, Bool)])
describeLuaComponents logger = do
    ok ← callSaveModules1 logger "describeAll" 0 (return ())
    if not ok
      then return (Left "save_modules.describeAll() could not be called \
                         \(see engine log)")
      else do
        result ← readLuaArrayAt readComponentDescriptorField
        Lua.pop 1  -- describeAll() result array
        return $ case result of
            Right xs → Right xs
            Left err → Left ("save_modules.describeAll() returned a "
                <> "malformed component descriptor: " <> err)

-- | Read {id=string, version=number, required=boolean, payload=string}
--   from the table at the top of the stack.
readSnapshotComponentField
    ∷ Lua.LuaE Lua.Exception (Maybe (Text, Word32, Bool, BS.ByteString))
readSnapshotComponentField = do
    _ ← Lua.getfield (-1) "id"
    midB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "version"
    mver ← Lua.tointeger (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "required"
    req ← Lua.toboolean (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "payload"
    mpayload ← Lua.tostring (-1)
    Lua.pop 1
    case (midB, mver, mpayload) of
        (Just idb, Just ver, Just payload) →
            return (Just (TE.decodeUtf8Lenient idb, fromIntegral ver, req, payload))
        _ → return Nothing

-- | Call @saveModules.snapshotAll()@ (issue #761): a REQUIRED
--   component's snapshot/encode failure aborts the WHOLE save —
--   reported as 'Left' so the caller fails the save transaction before
--   anything is queued to the world thread, rather than silently
--   continuing with partial Lua state the way the pre-#761 blob map
--   used to (the engine save no longer "still proceeds, just without
--   Lua blobs").
collectLuaComponents
    ∷ LoggerState
    → Lua.LuaE Lua.Exception (Either Text [(Text, Word32, Bool, BS.ByteString)])
collectLuaComponents logger = do
    ok ← callSaveModules1 logger "snapshotAll" 0 (return ())
    if not ok
      then return (Left "save_modules.snapshotAll() could not be called \
                         \(see engine log)")
      else do
        _ ← Lua.getfield (-1) "ok"
        isOk ← Lua.toboolean (-1)
        Lua.pop 1
        result ←
            if isOk
              then do
                _ ← Lua.getfield (-1) "components"
                arrResult ← readLuaArrayAt readSnapshotComponentField
                Lua.pop 1  -- components array
                return $ case arrResult of
                    Right xs → Right xs
                    -- round-4 review "fail closed": a component record
                    -- HsLua can't fully read (e.g. an out-of-range
                    -- version) must abort the save, not vanish from
                    -- the list — dropping it here is indistinguishable
                    -- from that REQUIRED component never having
                    -- existed at all.
                    Left err → Left ("save_modules.snapshotAll() returned "
                        <> "a malformed component record: " <> err)
              else do
                _ ← Lua.getfield (-1) "error"
                merr ← Lua.tostring (-1)
                Lua.pop 1
                return (Left (maybe "unknown save_modules.snapshotAll() \
                                    \error" TE.decodeUtf8Lenient merr))
        Lua.pop 1  -- snapshotAll() result table
        return result

-- | Push a Lua array of {id=, version=, payload=} tables from a list of
--   (name, version, payload) — the shape @saveModules.prepareLoad@
--   expects.
pushComponentsArray
    ∷ [(Text, Word32, BS.ByteString)] → Lua.LuaE Lua.Exception ()
pushComponentsArray xs = do
    Lua.newtable
    forM_ (zip [1..] xs) $ \(i, (name, ver, payload)) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (-2) "id"
        Lua.pushinteger (fromIntegral ver)
        Lua.setfield (-2) "version"
        Lua.pushstring payload
        Lua.setfield (-2) "payload"
        Lua.rawseti (-2) i

readErrorStringField ∷ Lua.LuaE Lua.Exception (Maybe Text)
readErrorStringField = do
    ty ← Lua.ltype (-1)
    if ty ≡ Lua.TypeString
        then (TE.decodeUtf8Lenient ⊚) ⊚ Lua.tostring (-1)
        else return Nothing

-- | Call @saveModules.prepareLoad(components)@ (issue #761): decode +
--   migrate + component-locally-validate EVERY registered Lua component
--   with NO live mutation (requirement 11). Any failure — a require/call
--   failure, or a reported validation error — aborts the load; nothing
--   has touched live Lua state yet either way.
prepareLuaLoad
    ∷ LoggerState → [(Text, Word32, BS.ByteString)]
    → Lua.LuaE Lua.Exception (Either Text ())
prepareLuaLoad logger components = do
    ok ← callSaveModules1 logger "prepareLoad" 1
            (pushComponentsArray components)
    if not ok
      then return (Left "save_modules.prepareLoad() could not be called \
                         \(see engine log)")
      else do
        _ ← Lua.getfield (-1) "ok"
        isOk ← Lua.toboolean (-1)
        Lua.pop 1
        result ←
            if isOk
              then return (Right ())
              else do
                _ ← Lua.getfield (-1) "errors"
                -- Purely a diagnostic message list here — the load is
                -- already known to be failing (isOk == False)
                -- regardless of whether every entry parses, so a
                -- malformed entry degrades to an empty list (falling
                -- into the "no error detail" message below) rather
                -- than needing its own fail-closed handling.
                arrResult ← readLuaArrayAt readErrorStringField
                Lua.pop 1
                let errs = either (const []) id arrResult
                return (Left (if null errs
                                then "save_modules.prepareLoad() failed \
                                     \(no error detail)"
                                else T.intercalate "; " errs))
        Lua.pop 1  -- prepareLoad() result table
        return result

-- | Call @saveModules.applyAll()@ (issue #761): apply the load prepared
--   by the most recent successful 'prepareLuaLoad', then run every
--   registered reset hook. Only reachable after 'prepareLuaLoad'
--   returned 'Right', so a failure here is a genuine apply()/reset-hook
--   bug rather than a data problem — but it must still be REPORTED
--   (never warning-only, requirement 6): the caller
--   ('Engine.Scripting.Lua.API.Save.loadValidatedSave') aborts the
--   whole load rather than queuing the Haskell-side restore on top of a
--   Lua state that only partially applied.
applyLuaLoad ∷ LoggerState → Lua.LuaE Lua.Exception (Either Text ())
applyLuaLoad logger = do
    ok ← callSaveModules0 logger "applyAll"
    return $ if ok then Right ()
             else Left "save_modules.applyAll() failed (see engine log)"
