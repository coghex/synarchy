{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Save
    ( saveListFn
    , saveWorldFn
    , loadSaveFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logWarn)
import World.Save.Serialize (listSaves, saveWorld, loadWorld)
import World.Save.Types (SaveMetadata(..), SaveData(..))
import World.Types (WorldCommand(..))
import World.Page.Types (WorldPageId(..))
import Data.IORef (readIORef)

-- | engine.listSaves() → returns a Lua table of {name, seed, worldSize, timestamp}
saveListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveListFn _env = do
    saves ← Lua.liftIO listSaves
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

-- | engine.saveWorld(pageId, saveName). Calls Lua-side
--   `scripts.lib.save_modules.serializeAll()` to collect each
--   registered module's state into a blob table, then enqueues
--   a `WorldSave` command carrying the blobs to the world thread.
saveWorldFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveWorldFn env = do
    pageIdArg ← Lua.tostring 1
    nameArg   ← Lua.tostring 2
    case (pageIdArg, nameArg) of
        (Just pageIdBS, Just nameBS) → do
            blobs ← collectLuaBlobs
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                    saveName = TE.decodeUtf8 nameBS
                Q.writeQueue (worldQueue env) (WorldSave pageId saveName blobs)
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
                    restoreLuaBlobs (sdLuaModules saveData)
                    let pageId = WorldPageId "main_world"
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

-- | Invoke `require("scripts.lib.save_modules").serializeAll()` and
--   read the resulting `{ name → blob }` table into a HashMap.
--   Returns empty on any error (already-failed save_modules registry,
--   missing lib file, etc.) — the engine save still proceeds, just
--   without Lua blobs.
collectLuaBlobs ∷ Lua.LuaE Lua.Exception (HM.HashMap Text Text)
collectLuaBlobs = do
    -- require("scripts.lib.save_modules")
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    _ ← Lua.pcall 1 1 Nothing
    -- module table on top; call serializeAll
    _ ← Lua.getfield (-1) "serializeAll"
    _ ← Lua.pcall 0 1 Nothing
    -- blobs table on top
    blobs ← readStringTable
    Lua.pop 2  -- blobs table + module table
    return blobs

-- | Push the blobs back to Lua and call
--   `require("scripts.lib.save_modules").deserializeAll(blobs)`.
restoreLuaBlobs ∷ HM.HashMap Text Text → Lua.LuaE Lua.Exception ()
restoreLuaBlobs blobs = do
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    _ ← Lua.pcall 1 1 Nothing
    _ ← Lua.getfield (-1) "deserializeAll"
    pushStringTable blobs
    _ ← Lua.pcall 1 0 Nothing
    Lua.pop 1  -- module table

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
                mk ← Lua.tostring (-2)
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
