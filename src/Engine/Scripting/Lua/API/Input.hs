module Engine.Scripting.Lua.API.Input
  ( isKeyDownFn
  , isActionDownFn
  , getMousePositionFn
  , isMouseButtonDownFn
  , getWindowSizeFn
  , getFramebufferSizeFn
  , getWorldCoordFn
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Input.Types (InputState(..), inpMousePos, inpMouseBtns, 
                           inpWindowSize, inpFramebufferSize)
import Engine.Input.Bindings (checkKeyDown, isActionDown)
import Engine.Graphics.Camera (Camera2D(..), camZoom, camPosition)
import Engine.Core.State (EngineEnv(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.IORef (readIORef)

isKeyDownFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
isKeyDownFn backendState = do
  keyStr ← Lua.tostring 1
  case keyStr of
    Just keyBS → do
      isDown ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        let keyName = TE.decodeUtf8 keyBS
        return $ checkKeyDown keyName inputState
      Lua.pushboolean isDown
    Nothing → Lua.pushboolean False
  return 1

isActionDownFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
isActionDownFn env backendState = do
  actionStr ← Lua.tostring 1
  case actionStr of
    Just actionBS → do
      isDown ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        bindings ← readIORef (keyBindingsRef env)
        let actionName = TE.decodeUtf8 actionBS
        return $ isActionDown actionName bindings inputState
      Lua.pushboolean isDown
    Nothing → Lua.pushboolean False
  return 1

getMousePositionFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
getMousePositionFn backendState = do
  (x, y) ← Lua.liftIO $ do
    inputState ← readIORef (lbsInputState backendState)
    return $ inpMousePos inputState
  Lua.pushnumber (Lua.Number x)
  Lua.pushnumber (Lua.Number y)
  return 2

isMouseButtonDownFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
isMouseButtonDownFn backendState = do
  buttonNum ← Lua.tointeger 1
  case buttonNum of
    Just btn → do
      isDown ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        let button = case btn of
              1 → GLFW.MouseButton'1
              2 → GLFW.MouseButton'2
              3 → GLFW.MouseButton'3
              _ → GLFW.MouseButton'1
        return $ Map.findWithDefault False button (inpMouseBtns inputState)
      Lua.pushboolean isDown
    Nothing → Lua.pushboolean False
  return 1

getWindowSizeFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
getWindowSizeFn backendState = do
  (w, h) ← Lua.liftIO $ do
    inputState ← readIORef (lbsInputState backendState)
    let (winW, winH) = inpWindowSize inputState
    return (fromIntegral winW, fromIntegral winH)
  Lua.pushnumber (Lua.Number w)
  Lua.pushnumber (Lua.Number h)
  return 2

getFramebufferSizeFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
getFramebufferSizeFn backendState = do
  (w, h) ← Lua.liftIO $ do
    inputState ← readIORef (lbsInputState backendState)
    let (winW, winH) = inpFramebufferSize inputState
    return (fromIntegral winW, fromIntegral winH)
  Lua.pushnumber (Lua.Number w)
  Lua.pushnumber (Lua.Number h)
  return 2

getWorldCoordFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
getWorldCoordFn env backendState = do
  sx ← Lua.tonumber 1
  sy ← Lua.tonumber 2
  case (sx, sy) of
    (Just (Lua.Number screenX), Just (Lua.Number screenY)) → do
      (worldX, worldY) ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        camera ← readIORef (cameraRef env)
        let (winW, winH) = inpWindowSize inputState
            (fbW, fbH) = inpFramebufferSize inputState
            aspect = fromIntegral fbW / fromIntegral fbH
            zoom = realToFrac (camZoom camera)
            viewWidth = zoom * aspect
            viewHeight = zoom
            normX = screenX / fromIntegral winW
            normY = screenY / fromIntegral winH
            viewX = (normX * 2.0 - 1.0) * viewWidth
            viewY = (normY * 2.0 - 1.0) * viewHeight
            (camX, camY) = camPosition camera
            worldX' = viewX + realToFrac camX
            worldY' = viewY + realToFrac camY
        return (worldX', worldY')
      Lua.pushnumber (Lua.Number worldX)
      Lua.pushnumber (Lua.Number worldY)
    _ → do
      Lua.pushnil
      Lua.pushnil
  return 2
