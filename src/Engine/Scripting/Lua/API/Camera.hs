{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Camera
    ( cameraMoveFn
    , cameraGetPositionFn
    , cameraSetPositionFn
    , cameraGetZoomFn
    , cameraSetZoomFn
    , cameraGetZoomVelocityFn
    , cameraSetZoomVelocityFn
    , cameraGetZSliceFn
    , cameraSetZSliceFn
    , cameraGotoTileFn
    , cameraRotateCWFn
    , cameraRotateCCWFn
    , cameraGetFacingFn
    , cameraGetZTrackingFn
    , cameraSetZTrackingFn
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), rotateCW, rotateCCW)
import World.Grid (gridToWorld)
import World.Types
import World.Material (MaterialId(..))
import World.Plate (generatePlates, elevationAtGlobal)
import World.Render (surfaceHeadroom)
import World.Generate (globalToChunk, applyTimeline)
import World.ZoomMap (buildZoomCache)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified HsLua as Lua

-- | camera.move(dx, dy)
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
cameraGetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetPositionFn env = do
    (x, y) ← Lua.liftIO $ do
        cam ← readIORef (cameraRef env)
        return (camPosition cam)
    Lua.pushnumber (Lua.Number (realToFrac x))
    Lua.pushnumber (Lua.Number (realToFrac y))
    return 2

-- | camera.setPosition(x, y)
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
cameraGetZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZoomFn env = do
    z ← Lua.liftIO $ do
        cam ← readIORef (cameraRef env)
        return (camZoom cam)
    Lua.pushnumber (Lua.Number (realToFrac z))
    return 1

-- | camera.setZoom(z)
cameraSetZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZoomFn env = do
    zArg ← Lua.tonumber 1
    case zArg of
        Just (Lua.Number z) → Lua.liftIO $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZoom = max 0.1 (realToFrac z) }, ())
        _ → pure ()
    return 0

-- | camera.getZoomVelocity() -> number
cameraGetZoomVelocityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZoomVelocityFn env = do
    cam ← Lua.liftIO $ readIORef (cameraRef env)
    Lua.pushnumber (Lua.Number (realToFrac (camZoomVelocity cam)))
    return 1

-- | camera.setZoomVelocity(v)
cameraSetZoomVelocityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZoomVelocityFn env = do
    vArg ← Lua.tonumber 1
    case vArg of
        Just (Lua.Number v) → Lua.liftIO $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZoomVelocity = realToFrac v }, ())
        _ → pure ()
    return 0

-- | camera.getZSlice() -> int
cameraGetZSliceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZSliceFn env = do
    cam ← Lua.liftIO $ readIORef (cameraRef env)
    Lua.pushinteger (fromIntegral $ camZSlice cam)
    return 1

-- | camera.setZSlice(z)
cameraSetZSliceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZSliceFn env = do
    zArg ← Lua.tointeger 1
    case zArg of
        Just z → Lua.liftIO $ 
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZSlice = fromIntegral z }, ())
        Nothing → pure ()
    return 0

-- | camera.gotoTile(gx, gy)
--   Teleport camera to a global tile coordinate.
--   Sets position, zoom to tile level, and computes the correct
--   z-slice from the world gen params (works even if the chunk
--   isn't loaded yet).
cameraGotoTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGotoTileFn env = do
    gxArg ← Lua.tointeger 1
    gyArg ← Lua.tointeger 2
    case (gxArg, gyArg) of
        (Just gxRaw, Just gyRaw) → Lua.liftIO $ do
            let gx = fromIntegral gxRaw ∷ Int
                gy = fromIntegral gyRaw ∷ Int
            cam ← readIORef (cameraRef env)
            let facing = camFacing cam
                (wx, wy) = gridToWorld facing gx gy

            -- Set position and zoom
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camPosition = (wx, wy)
                     , camZoom     = 0.5
                     , camVelocity = (0, 0)
                     , camDragging = False
                     }, ())

            -- Compute surface elevation from world gen params.
            -- This is a pure computation — no loaded chunks needed.
            manager ← readIORef (worldManagerRef env)
            forM_ (wmVisible manager) $ \pageId →
                case lookup pageId (wmWorlds manager) of
                    Just worldState → do
                        mParams ← readIORef (wsGenParamsRef worldState)
                        case mParams of
                            Just params → do
                                let seed      = wgpSeed params
                                    worldSize = wgpWorldSize params
                                    timeline  = wgpGeoTimeline params
                                    plates    = generatePlates seed worldSize (wgpPlateCount params)
                                    (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx gy
                                    (finalElev, _) = applyTimeline timeline worldSize gx gy (baseElev, baseMat)
                                    targetZ = finalElev + surfaceHeadroom
                                atomicModifyIORef' (cameraRef env) $ \cam →
                                    (cam { camZSlice = targetZ, camZTracking = True }, ())
                            Nothing → return ()
                    Nothing → return ()

        _ → pure ()
    return 0


-- | camera.rotateCW()
cameraRotateCWFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraRotateCWFn env = do
    Lua.liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camFacing = rotateCW (camFacing cam) }, ())
    -- Invalidate caches here too (same as step 5)
    Lua.liftIO $ invalidateWorldCaches env
    return 0

-- | camera.rotateCCW()
cameraRotateCCWFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraRotateCCWFn env = do
    Lua.liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camFacing = rotateCCW (camFacing cam) }, ())
    Lua.liftIO $ invalidateWorldCaches env
    return 0

-- | camera.getFacing() → int (0=South, 1=West, 2=North, 3=East)
cameraGetFacingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetFacingFn env = do
    cam ← Lua.liftIO $ readIORef (cameraRef env)
    Lua.pushinteger $ case camFacing cam of
        FaceSouth → 0
        FaceWest  → 1
        FaceNorth → 2
        FaceEast  → 3
    return 1

-- Helper to invalidate all baked caches
invalidateWorldCaches ∷ EngineEnv → IO ()
invalidateWorldCaches env = do
    camera <- readIORef (cameraRef env)
    let facing = camFacing camera
    manager ← readIORef (worldManagerRef env)
    forM_ (wmWorlds manager) $ \(_, ws) → do
        writeIORef (wsQuadCacheRef ws)     Nothing
        writeIORef (wsZoomQuadCacheRef ws) Nothing
        writeIORef (wsBgQuadCacheRef ws)   Nothing
        writeIORef (wsBakedZoomRef ws)     (V.empty, defaultWorldTextures)
        writeIORef (wsBakedBgRef ws)       (V.empty, defaultWorldTextures)
        mParams ← readIORef (wsGenParamsRef ws)
        case mParams of
            Just params → writeIORef (wsZoomCacheRef ws) (buildZoomCache facing params)
            Nothing → return ()

-- | camera.getZTracking() -> bool
cameraGetZTrackingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZTrackingFn env = do
    cam ← Lua.liftIO $ readIORef (cameraRef env)
    Lua.pushboolean (camZTracking cam)
    return 1

-- | camera.setZTracking(bool)
cameraSetZTrackingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZTrackingFn env = do
    bArg ← Lua.toboolean 1
    Lua.liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camZTracking = bArg }, ())
    return 0
