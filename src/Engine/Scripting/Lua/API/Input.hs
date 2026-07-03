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
import Engine.Input.Types (InputState(..), inpMousePos, inpMouseBtns)
import Engine.Input.Bindings (checkKeyDown, isActionDown)
import Engine.Graphics.Camera (Camera2D(..), camZoom, camPosition)
import Engine.Graphics.Viewport (viewportDegenerate)
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
        let keyName = TE.decodeUtf8Lenient keyBS
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
        let actionName = TE.decodeUtf8Lenient actionBS
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
        let mButton = case btn of
              1 → Just GLFW.MouseButton'1
              2 → Just GLFW.MouseButton'2
              3 → Just GLFW.MouseButton'3
              _ → Nothing
        return $ case mButton of
          Just button → Map.findWithDefault False button (inpMouseBtns inputState)
          Nothing     → False
      Lua.pushboolean isDown
    Nothing → Lua.pushboolean False
  return 1

getWindowSizeFn ∷ EngineEnv → LuaBackendState
  → Lua.LuaE Lua.Exception Lua.NumResults
getWindowSizeFn env _backendState = do
  (w, h) ← Lua.liftIO $ readIORef (windowSizeRef env)
  Lua.pushnumber (Lua.Number (fromIntegral w))
  Lua.pushnumber (Lua.Number (fromIntegral h))
  return 2

getFramebufferSizeFn ∷ EngineEnv → LuaBackendState
  → Lua.LuaE Lua.Exception Lua.NumResults
getFramebufferSizeFn env _backendState = do
  (w, h) ← Lua.liftIO $ readIORef (framebufferSizeRef env)
  Lua.pushnumber (Lua.Number (fromIntegral w))
  Lua.pushnumber (Lua.Number (fromIntegral h))
  return 2

getWorldCoordFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
getWorldCoordFn env _backendState = do
  sx ← Lua.tonumber 1
  sy ← Lua.tonumber 2
  case (sx, sy) of
    (Just (Lua.Number screenX), Just (Lua.Number screenY)) → do
      mCoord ← Lua.liftIO $ do
        camera ← readIORef (cameraRef env)
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH) ← readIORef (framebufferSizeRef env)
        -- Minimized window: a zero-size window/framebuffer makes the
        -- divisions below hand NaN (zero height) or a centerline-collapsed
        -- coord (zero width) to Lua camera math. Report "no coordinate".
        if viewportDegenerate winW winH fbW fbH
          then return Nothing
          else do
            let aspect = fromIntegral fbW / fromIntegral fbH
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
            return $ Just (worldX', worldY')
      case mCoord of
        Just (worldX, worldY) → do
          Lua.pushnumber (Lua.Number worldX)
          Lua.pushnumber (Lua.Number worldY)
        Nothing → do
          Lua.pushnil
          Lua.pushnil
    _ → do
      Lua.pushnil
      Lua.pushnil
  return 2
