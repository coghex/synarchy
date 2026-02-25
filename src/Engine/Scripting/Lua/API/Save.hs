{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Save
    ( saveListFn
    , saveWorldFn
    , loadSaveFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import World.Save.Serialize (listSaves, saveWorld, loadWorld)
import World.Save.Types (SaveMetadata(..))
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

-- | engine.saveWorld(pageId, saveName)
saveWorldFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveWorldFn env = do
    pageIdArg ← Lua.tostring 1
    nameArg   ← Lua.tostring 2
    case (pageIdArg, nameArg) of
        (Just pageIdBS, Just nameBS) → Lua.liftIO $ do
            let _pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                saveName = TE.decodeUtf8 nameBS
            -- Snapshot current world state and serialize
            -- (You'll read from the WorldManager here)
            -- For now, queue a save command
            Q.writeQueue (worldQueue env) (WorldSave _pageId saveName)
        _ → pure ()
    Lua.pushboolean True
    return 1

-- | engine.loadSave(saveName) → loads and queues WorldLoadSave
loadSaveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadSaveFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Just nameBS → do
            let saveName = TE.decodeUtf8 nameBS
            result ← Lua.liftIO $ loadWorld saveName
            case result of
                Right saveData → do
                    let pageId = WorldPageId "main_world"
                    Lua.liftIO $ Q.writeQueue (worldQueue env)
                        (WorldLoadSave pageId saveData)
                    Lua.pushboolean True
                Left err → do
                    Lua.pushboolean False
            return 1
        Nothing → do
            Lua.pushboolean False
            return 1
