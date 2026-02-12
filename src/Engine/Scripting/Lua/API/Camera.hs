{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Camera
    ( cameraMoveFn
    , cameraGetPositionFn
    , cameraSetPositionFn
    , cameraGetZoomFn
    , cameraSetZoomFn
    , cameraGetZSliceFn
    , cameraSetZSliceFn
    , cameraGotoTileFn
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..))
import World.Grid (gridToWorld)
import World.Types
import World.Material (MaterialId(..))
import World.Plate (generatePlates, elevationAtGlobal)
import World.Generate (globalToChunk)
import World.Geology (applyGeoEvent, applyErosion, GeoModification(..))
import qualified Data.HashMap.Strict as HM
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

-- | camera.gotoTile(gx, gy)
--   Teleport camera to a global tile coordinate.
--   Sets position, zoom to tile level, and computes the correct
--   z-slice from the world gen params (works even if the chunk
--   isn't loaded yet).
cameraGotoTileFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
cameraGotoTileFn env = do
    gxArg <- Lua.tointeger 1
    gyArg <- Lua.tointeger 2
    case (gxArg, gyArg) of
        (Just gxRaw, Just gyRaw) -> Lua.liftIO $ do
            let gx = fromIntegral gxRaw :: Int
                gy = fromIntegral gyRaw :: Int
                (wx, wy) = gridToWorld gx gy

            -- Set position and zoom
            atomicModifyIORef' (cameraRef env) $ \cam ->
                (cam { camPosition = (wx, wy)
                     , camZoom     = 0.5
                     , camVelocity = (0, 0)
                     , camDragging = False
                     }, ())

            -- Compute surface elevation from world gen params.
            -- This is a pure computation — no loaded chunks needed.
            manager <- readIORef (worldManagerRef env)
            forM_ (wmVisible manager) $ \pageId ->
                case lookup pageId (wmWorlds manager) of
                    Just worldState -> do
                        mParams <- readIORef (wsGenParamsRef worldState)
                        case mParams of
                            Just params -> do
                                let seed      = wgpSeed params
                                    worldSize = wgpWorldSize params
                                    timeline  = wgpGeoTimeline params
                                    plates    = generatePlates seed worldSize (wgpPlateCount params)
                                    (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx gy
                                    (finalElev, _) = applyTimeline timeline worldSize gx gy (baseElev, baseMat)
                                    targetZ = finalElev + 3
                                atomicModifyIORef' (cameraRef env) $ \cam ->
                                    (cam { camZSlice = targetZ }, ())
                            Nothing -> return ()
                    Nothing -> return ()

        _ -> pure ()
    return 0

-- | Walk the geological timeline, applying each period's events
--   and erosion to get the final elevation and material.
applyTimeline :: GeoTimeline -> Int -> Int -> Int -> (Int, MaterialId) -> (Int, MaterialId)
applyTimeline timeline worldSize gx gy (baseElev, baseMat) =
    foldl' applyPeriod (baseElev, baseMat) (gtPeriods timeline)
  where
    applyPeriod (elev, mat) period =
        let (elev', mat') = foldl' applyOneEvent (elev, mat) (gpEvents period)
            erosionMod = applyErosion (gpErosion period) worldSize gx gy elev'
            elev'' = elev' + gmElevDelta erosionMod
            mat'' = case gmMaterialOverride erosionMod of
                Just m  -> MaterialId m
                Nothing -> mat'
        in (elev'', mat'')

    applyOneEvent (elev, mat) event =
        let mod' = applyGeoEvent event worldSize gx gy elev
            elev' = elev + gmElevDelta mod'
            mat'  = case gmMaterialOverride mod' of
                Just m  -> MaterialId m
                Nothing -> mat
        in (elev', mat')
