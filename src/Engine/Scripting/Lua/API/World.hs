{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World
    ( worldInitFn
    , worldShowFn
    , worldHideFn
    , worldSetTextureFn
    , worldSetCameraFn
    , worldSetSunAngleFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldCommand(..), WorldPageId(..), WorldTextureType(..))
import Data.IORef (atomicModifyIORef')

-- | world.init(pageId, seed, worldSizeInChunks)
worldInitFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
worldInitFn env = do
    pageIdArg <- Lua.tostring 1
    seedArg   <- Lua.tointeger 2
    sizeArg   <- Lua.tointeger 3
    
    case pageIdArg of
        Just pageIdBS -> Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                seed   = maybe 42 fromIntegral seedArg
                size   = maybe 64 fromIntegral sizeArg
            Q.writeQueue (worldQueue env) (WorldInit pageId seed size)
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

-- | world.setCamera(pageId, x, y)
-- Sync camera position to world state for hit-testing / tile picking
worldSetCameraFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCameraFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just (Lua.Number x), Just (Lua.Number y)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetCamera pageId (realToFrac x) (realToFrac y))
        _ → pure ()

    return 0

worldSetSunAngleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSunAngleFn env = do
    angleArg ← Lua.tonumber 1

    case angleArg of
        Just (Lua.Number angle) → Lua.liftIO $ do
            atomicModifyIORef' (sunAngleRef env) $ \_ -> (realToFrac angle, ())
        _ → pure ()

    return 0

parseTextureType :: Text -> WorldTextureType
parseTextureType "grass"         = GrassTexture
parseTextureType "grass_facemap" = GrassFaceMap
parseTextureType _               = GrassTexture  -- Default
