{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Save
    ( saveListFn
    , saveWorldFn
    , loadSaveFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Structure.Types (emptyChunkStructures)
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Engine.PlayerEvent.Emit (emitEvent)
import World.Save.Serialize (listSaves, saveWorld, loadWorld
                            , sanitizeSaveName)
import World.Save.Types (SaveMetadata(..), SaveData(..))
import World.Types (WorldCommand(..), WorldManager(..), WorldState(..)
                   , LoadPhase(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Helpers (unWorldPageId)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')

-- | engine.listSaves() → returns a Lua table of {name, seed, worldSize, timestamp}
--   sorted newest-first by timestamp.
saveListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveListFn env = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    saves ← Lua.liftIO $ listSaves logger
    Lua.newtable
    forM_ (zip [1..] saves) $ \(i, (name, meta)) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (-2) "name"
        Lua.pushinteger (fromIntegral $ smSeed meta)
        Lua.setfield (-2) "seed"
        Lua.pushinteger (fromIntegral $ smWorldSize meta)
        Lua.setfield (-2) "worldSize"
        Lua.pushstring (TE.encodeUtf8 $ smTimestamp meta)
        Lua.setfield (-2) "timestamp"
        Lua.rawseti (-2) i
    return 1

-- | engine.saveWorld(pageId, saveName). Validates the request
--   synchronously (name, world-exists, gen-params present), then
--   collects each registered Lua module's state via
--   `scripts.lib.save_modules.serializeAll()` and enqueues a
--   `WorldSave` command carrying the blobs to the world thread.
--
--   Returns false on any validation failure (with a logged reason);
--   true once the command is queued. Disk-write failures are
--   inherently async and surface via the engine→Lua `onWorldGenLog`
--   broadcast (see `Save.hs:128-135`).
saveWorldFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveWorldFn env = do
    pageIdArg ← Lua.tostring 1
    nameArg   ← Lua.tostring 2
    case (pageIdArg, nameArg) of
        (Just pageIdBS, Just nameBS) → do
            let saveName = TE.decodeUtf8 nameBS
                pageId   = WorldPageId (TE.decodeUtf8 pageIdBS)
            logger ← Lua.liftIO $ readIORef (loggerRef env)
            case sanitizeSaveName saveName of
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
                                    -- Pause the engine BEFORE collecting
                                    -- Lua state. Other threads (world,
                                    -- unit) early-return on this flag, so
                                    -- their state stops drifting before
                                    -- we snapshot Lua-side. Without this,
                                    -- the Lua snapshot references engine
                                    -- ids whose fields can change before
                                    -- the world thread runs WorldSave.
                                    -- handleWorldSaveCommand also writes
                                    -- this flag (defense for non-Lua
                                    -- callers); the double-write is
                                    -- harmless and intentional.
                                    Lua.liftIO $ writeIORef
                                        (enginePausedRef env) True
                                    blobs ← collectLuaBlobs logger
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
                                        (WorldSave pageId name
                                            nowText blobs)
                                    Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

-- | engine.loadSave(saveName) → loads the file, restores Lua module
--   state via `saveModules.deserializeAll`, then queues the engine-side
--   restore (chunks/buildings/units/sim). Lua state restores BEFORE
--   the engine queue runs so any AI/spawn-sequencer references to
--   restored units are valid by the time the world thread writes
--   unitManagerRef.
loadSaveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadSaveFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Just nameBS → do
            let saveName = TE.decodeUtf8 nameBS
            result ← Lua.liftIO $ loadWorld saveName
            case result of
                Right saveData → do
                    logger ← Lua.liftIO $ readIORef (loggerRef env)
                    restoreLuaBlobs logger (sdLuaModules saveData)
                    let pageId = WorldPageId "main_world"
                    -- Synchronously flip the current head world's
                    -- phaseRef so a follow-up world.waitForInit
                    -- doesn't read stale LoadDone from the previous
                    -- gen and return immediately. The real handler
                    -- creates its own WorldState and prepends it to
                    -- wmWorlds; once that lands, waitForInit polls
                    -- the new head and follows it through to
                    -- LoadDone. This write is shadowed by the new
                    -- WorldState as soon as the handler runs.
                    Lua.liftIO $ markHeadWorldLoading env
                    -- Drop the structure read-your-writes staging cache: the
                    -- loaded world rebuilds lcStructures from its own edits, so
                    -- any staged placement from the prior world is now stale.
                    Lua.liftIO $ writeIORef (structureStageRef env)
                                            emptyChunkStructures
                    Lua.liftIO $ Q.writeQueue (worldQueue env)
                        (WorldLoadSave pageId saveData)
                    Lua.pushboolean True
                Left err → do
                    Lua.liftIO $ do
                        logger ← readIORef (loggerRef env)
                        logWarn logger CatWorld $
                            "loadSave failed for '" <> saveName <> "': " <> err
                    Lua.pushboolean False
            return 1
        Nothing → do
            Lua.pushboolean False
            return 1

-- | Set the head world's loading phase to "in progress" so
--   'world.waitForInit' will block correctly even though the actual
--   load handler hasn't run yet. No-op when wmWorlds is empty
--   (waitForInit already handles that case by polling).
markHeadWorldLoading ∷ EngineEnv → IO ()
markHeadWorldLoading env = do
    mgr ← readIORef (worldManagerRef env)
    case wmWorlds mgr of
        ((_, ws):_) → writeIORef (wsLoadPhaseRef ws) (LoadPhase1 1 1)
        []          → return ()

-- | Pop the Lua error message at the top of the stack and log it
--   via the engine logger. Used by collectLuaBlobs / restoreLuaBlobs
--   to surface pcall failures that would otherwise be silent.
luaLogPcallError ∷ LoggerState → Text → Lua.LuaE Lua.Exception ()
luaLogPcallError logger ctx = do
    err ← Lua.tostring (-1)
    Lua.pop 1
    Lua.liftIO $ logWarn logger CatLua $
        ctx <> ": " <> maybe "<no message>" TE.decodeUtf8 err

-- | Invoke `require("scripts.lib.save_modules").serializeAll()` and
--   read the resulting `{ name → blob }` table into a HashMap.
--   Returns empty on any error (require failing, serializeAll missing
--   or not a function, serializeAll crashing) and logs via the engine
--   logger — the engine save still proceeds, just without Lua blobs.
collectLuaBlobs ∷ LoggerState
                → Lua.LuaE Lua.Exception (HM.HashMap Text Text)
collectLuaBlobs logger = do
    -- require("scripts.lib.save_modules")
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    requireStatus ← Lua.pcall 1 1 Nothing
    case requireStatus of
        Lua.OK → do
            -- module table on top; look up serializeAll
            _ ← Lua.getfield (-1) "serializeAll"
            isFun ← Lua.isfunction (-1)
            if not isFun
                then do
                    Lua.pop 2  -- non-function value + module table
                    Lua.liftIO $ logWarn logger CatLua
                        "collectLuaBlobs: save_modules.serializeAll \
                        \is not a function"
                    return HM.empty
                else do
                    serStatus ← Lua.pcall 0 1 Nothing
                    case serStatus of
                        Lua.OK → do
                            -- blobs table on top
                            blobs ← readStringTable
                            Lua.pop 2  -- blobs table + module table
                            return blobs
                        _ → do
                            luaLogPcallError logger
                                "collectLuaBlobs: serializeAll crashed"
                            Lua.pop 1  -- module table
                            return HM.empty
        _ → do
            luaLogPcallError logger
                "collectLuaBlobs: require scripts.lib.save_modules failed"
            return HM.empty

-- | Push the blobs back to Lua and call
--   `require("scripts.lib.save_modules").deserializeAll(blobs)`.
--   Logs and continues on any error (require failing, deserializeAll
--   missing or not a function, deserializeAll crashing) so the engine
--   load can still proceed — Lua module state will simply be missing.
restoreLuaBlobs ∷ LoggerState → HM.HashMap Text Text
                → Lua.LuaE Lua.Exception ()
restoreLuaBlobs logger blobs = do
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    requireStatus ← Lua.pcall 1 1 Nothing
    case requireStatus of
        Lua.OK → do
            _ ← Lua.getfield (-1) "deserializeAll"
            isFun ← Lua.isfunction (-1)
            if not isFun
                then do
                    Lua.pop 2  -- non-function + module table
                    Lua.liftIO $ logWarn logger CatLua
                        "restoreLuaBlobs: save_modules.deserializeAll \
                        \is not a function"
                else do
                    pushStringTable blobs
                    desStatus ← Lua.pcall 1 0 Nothing
                    case desStatus of
                        Lua.OK → Lua.pop 1  -- module table
                        _      → do
                            luaLogPcallError logger
                                "restoreLuaBlobs: deserializeAll crashed"
                            Lua.pop 1  -- module table
        _ → luaLogPcallError logger
              "restoreLuaBlobs: require scripts.lib.save_modules failed"

-- | Iterate the Lua table at top of stack as { string → string }.
--   Non-string keys or values are skipped silently — the registry on
--   the Lua side enforces well-formed shape via the saveModules lib.
readStringTable ∷ Lua.LuaE Lua.Exception (HM.HashMap Text Text)
readStringTable = do
    Lua.pushnil  -- first key
    loop HM.empty
  where
    loop acc = do
        more ← Lua.next (-2)
        if not more
            then return acc
            else do
                -- Check the key's type instead of converting it:
                -- lua_tolstring on a numeric key mutates it in place,
                -- and next() then errors with "invalid key to 'next'".
                keyTy ← Lua.ltype (-2)
                mk ← if keyTy ≡ Lua.TypeString
                         then Lua.tostring (-2)
                         else return Nothing
                mv ← Lua.tostring (-1)
                Lua.pop 1  -- pop value, keep key for the next next()
                case (mk, mv) of
                    (Just kb, Just vb) →
                        loop (HM.insert (TE.decodeUtf8 kb)
                                        (TE.decodeUtf8 vb) acc)
                    _ → loop acc

-- | Push a HashMap onto the stack as a Lua table with string keys.
pushStringTable ∷ HM.HashMap Text Text → Lua.LuaE Lua.Exception ()
pushStringTable m = do
    Lua.newtable
    forM_ (HM.toList m) $ \(k, v) → do
        Lua.pushstring (TE.encodeUtf8 k)
        Lua.pushstring (TE.encodeUtf8 v)
        Lua.rawset (-3)
