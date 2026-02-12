module Engine.Scripting.Lua.API.Graphics
  ( loadTextureFn
  , spawnSpriteFn
  , setPosFn
  , setColorFn
  , setSizeFn
  , setVisibleFn
  , destroyFn
  , getUIScaleFn
  ) where

import UPrelude
import Math (colorToVec4)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Asset.Manager (updateTextureState, generateTextureHandle)
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Scene.Base (ObjectId(..), LayerId(..))
import Engine.Graphics.Config (VideoConfig(..), WindowMode(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Core.State (EngineEnv(..), EngineState(..), glfwWindow)
import Engine.Core.Log (LogCategory(..), logWarn, logDebug)
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel(..), defaultLoc)

-- | engine.getUIScale()
getUIScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getUIScaleFn env = do
    vconfig ← Lua.liftIO $ readIORef (videoConfigRef env)
    Lua.pushnumber (Lua.Number (realToFrac (vcUIScale vconfig)))
    return 1

loadTextureFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
loadTextureFn backendState = do
  path ← Lua.tostring 1
  case path of
    Just pathBS → do
      handle ← Lua.liftIO $ do
        let pathStr = TE.decodeUtf8 pathBS
            (lteq, _) = lbsMsgQueues backendState
        pool ← readIORef (lbsAssetPool backendState)
        handle ← generateTextureHandle pool
        updateTextureState handle
          (AssetLoading (T.unpack pathStr) [] 0.0) pool
        writeIORef (lbsAssetPool backendState) pool
        Q.writeQueue lteq (LuaLoadTextureRequest handle (T.unpack pathStr))
        return handle
      let (TextureHandle n) = handle
      Lua.pushnumber (Lua.Number (fromIntegral n))
    Nothing → Lua.pushnil
  return 1

spawnSpriteFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
spawnSpriteFn env backendState = do
  x ← Lua.tonumber 1
  y ← Lua.tonumber 2
  width ← Lua.tonumber 3
  height ← Lua.tonumber 4
  texHandleNum ← Lua.tointeger 5
  layer ← Lua.tointeger 6
  
  case (x, y, width, height, texHandleNum) of
    (Just xVal, Just yVal, Just wVal, Just hVal, Just texNum) → do
      let layerId = LayerId $ fromIntegral $ fromMaybe 0 layer
      objId ← Lua.liftIO $ do
        logger ← readIORef $ loggerRef env
        objId ← atomicModifyIORef' (lbsNextObjectId backendState) 
          (\n → (n + 1, ObjectId n))
        
        logDebug logger CatLua $ "Lua spawning sprite with ID " 
                       <> T.pack (show objId)
        
        let (lteq, _) = lbsMsgQueues backendState
            texHandle = TextureHandle (fromIntegral texNum)
            msg = LuaSpawnSpriteRequest
              { lssObjectId      = objId
              , lssX             = realToFrac xVal
              , lssY             = realToFrac yVal
              , lssWidth         = realToFrac wVal
              , lssHeight        = realToFrac hVal
              , lssTextureHandle = texHandle
              , lssLayer         = layerId
              }
        Q.writeQueue lteq msg
        return objId
      
      let (ObjectId n) = objId
      Lua.pushinteger (Lua.Integer $ fromIntegral n)
      
    _ → do
      Lua.liftIO $ do
        logger ← readIORef $ loggerRef env
        logWarn logger CatLua
          "spawnSprite requires 5 arguments: x, y, width, height, textureHandle"
      Lua.pushnil
  return 1

setPosFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setPosFn env backendState = do
  objIdNum ← Lua.tointeger 1
  x ← Lua.tonumber 2
  y ← Lua.tonumber 3
  case (objIdNum, x, y) of
    (Just idVal, Just xVal, Just yVal) → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaSetPosRequest (ObjectId (fromIntegral idVal))
              (realToFrac xVal) (realToFrac yVal)
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setPos requires 3 arguments: objectId, x, y"
      return 0

setColorFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setColorFn env backendState = do
  objIdNum ← Lua.tointeger 1
  color ← Lua.tostring 2
  case (objIdNum, color) of
    (Just idVal, Just c) → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            cStr = T.unpack $ TE.decodeUtf8 c
            msg = LuaSetColorRequest (ObjectId (fromIntegral idVal)) (colorToVec4 cStr)
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setColor requires 2 arguments: objectId, color"
      return 0

setSizeFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setSizeFn env backendState = do
  objIdNum ← Lua.tointeger 1
  width ← Lua.tonumber 2
  height ← Lua.tonumber 3
  case (objIdNum, width, height) of
    (Just idVal, Just wVal, Just hVal) → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaSetSizeRequest (ObjectId (fromIntegral idVal))
              (realToFrac wVal) (realToFrac hVal)
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setSize requires 3 arguments: objectId, width, height"
      return 0

setVisibleFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setVisibleFn env backendState = do
  objIdNum ← Lua.tointeger 1
  visible ← Lua.toboolean 2
  case objIdNum of
    Just idVal → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaSetVisibleRequest (ObjectId (fromIntegral idVal)) visible
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setVisible requires 2 arguments: objectId, visible"
      return 0

destroyFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
destroyFn env backendState = do
  objIdNum ← Lua.tointeger 1
  case objIdNum of
    Just idVal → do
      Lua.liftIO $ do
        logger ← readIORef $ loggerRef env
        let objId = ObjectId (fromIntegral idVal)
        logDebug logger CatLua $ "Lua destroying object with ID " 
                       <> T.pack (show objId)
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaDestroyRequest (ObjectId (fromIntegral idVal))
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "destroy requires 1 argument: objectId"
      return 0
