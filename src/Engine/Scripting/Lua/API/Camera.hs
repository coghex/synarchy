{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Camera
    ( cameraMoveFn
    , cameraGetPositionFn
    , cameraSetPositionFn
    , cameraGetZoomFn
    , cameraSetZoomFn
    , cameraGetZSliceFn
    , cameraSetZSliceFn
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..))
import qualified HsLua as Lua

-- | camera.move(dx, dy)
-- Move camera by delta in world-space units.
-- Used for smooth per-frame panning from Lua.
cameraMoveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraMoveFn env = do
    dxArg ← Lua.tonumber 1
    dyArg ← Lua.tonumber 2
    case (dxArg, dyArg) of
        (Just (Lua.Number dx), Just (Lua.Number dy)) → Lua.liftIO $ do
            atomicModifyIORef' (cameraRef env) $ \cam →
                let (cx, cy) = camPosition cam
                in (cam { camPosition = ( cx + realToFrac dx
                                        , cy + realToFrac dy ) }, ())
        _ → pure ()
    return 0

-- | camera.getPosition() → x, y
-- Returns the current camera position in world-space.
cameraGetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetPositionFn env = do
    (x, y) ← Lua.liftIO $ do
        cam ← readIORef (cameraRef env)
        return (camPosition cam)
    Lua.pushnumber (Lua.Number (realToFrac x))
    Lua.pushnumber (Lua.Number (realToFrac y))
    return 2

-- | camera.setPosition(x, y)
-- Set camera position directly in world-space.
cameraSetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetPositionFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → Lua.liftIO $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camPosition = (realToFrac x, realToFrac y) }, ())
        _ → pure ()
    return 0

-- | camera.getZoom() → zoom
-- Returns the current camera zoom level.
cameraGetZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZoomFn env = do
    z ← Lua.liftIO $ do
        cam ← readIORef (cameraRef env)
        return (camZoom cam)
    Lua.pushnumber (Lua.Number (realToFrac z))
    return 1

-- | camera.setZoom(z)
-- Set camera zoom level. Clamped to a minimum of 0.1.
cameraSetZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZoomFn env = do
    zArg ← Lua.tonumber 1
    case zArg of
        Just (Lua.Number z) → Lua.liftIO $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZoom = max 0.1 (realToFrac z) }, ())
        _ → pure ()
    return 0

-- | camera.getZSlice() -> int
cameraGetZSliceFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZSliceFn env = do
    cam <- Lua.liftIO $ readIORef (cameraRef env)
    Lua.pushinteger (fromIntegral $ camZSlice cam)
    return 1

-- | camera.setZSlice(z)
cameraSetZSliceFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZSliceFn env = do
    zArg <- Lua.tointeger 1
    case zArg of
        Just z -> Lua.liftIO $ 
            atomicModifyIORef' (cameraRef env) $ \cam ->
                (cam { camZSlice = fromIntegral z }, ())
        Nothing -> pure ()
    return 0
