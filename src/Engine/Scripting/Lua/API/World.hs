{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World
    ( worldInitFn
    , worldShowFn
    , worldHideFn
    , worldSetTextureFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldCommand(..), WorldPageId(..), WorldTextureType(..))
import Data.IORef (atomicModifyIORef')

-- | world.init(pageId)
-- Initialize a world page with given ID
worldInitFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
worldInitFn env = do
    pageIdArg <- Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS -> Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldInit pageId)
        Nothing -> pure ()
    
    return 0

-- | world.show(pageId)
-- Make a world page visible for rendering
worldShowFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
worldShowFn env = do
    pageIdArg <- Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS -> Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldShow pageId)
        Nothing -> pure ()
    
    return 0

-- | world.hide(pageId)
-- Hide a world page from rendering
worldHideFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
worldHideFn env = do
    pageIdArg <- Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS -> Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldHide pageId)
        Nothing -> pure ()
    
    return 0

-- | world.setTexture(pageId, textureType, textureHandle)
-- Set a texture for a specific tile type in a world
worldSetTextureFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
worldSetTextureFn env = do
    pageIdArg <- Lua.tostring 1
    textureTypeArg <- Lua.tostring 2
    textureHandleArg <- Lua.tointeger 3
    
    case (pageIdArg, textureTypeArg, textureHandleArg) of
        (Just pageIdBS, Just typeBS, Just handle) -> Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texType = parseTextureType (TE.decodeUtf8 typeBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) (WorldSetTexture pageId texType texHandle)
        _ -> pure ()
    
    return 0

parseTextureType :: Text -> WorldTextureType
parseTextureType "grass" = GrassTexture
parseTextureType _ = GrassTexture  -- Default
