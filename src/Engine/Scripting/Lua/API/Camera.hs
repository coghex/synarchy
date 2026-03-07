{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Camera
    ( cameraMoveFn
    , cameraGetPositionFn
    , cameraSetPositionFn
    , cameraGetZoomFn
    , cameraSetZoomFn
    , cameraGetZoomFadeStartFn
    , cameraGetZoomFadeEndFn
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
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, LogCategory(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), rotateCW, rotateCCW)
import World.Grid
import World.Types
import World.Material (MaterialId(..), MaterialProps(..), getMaterialProps)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Render (surfaceHeadroom)
import World.Generate (globalToChunk, applyTimeline, viewDepth)
import World.Generate.Coordinates (globalToChunk)
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

-- | camera.getZoomFadeStart() -> number
cameraGetZoomFadeStartFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZoomFadeStartFn = do
    Lua.pushnumber (Lua.Number (realToFrac zoomFadeStart))
    return 1

-- | camera.getZoomFadeEnd() -> number
cameraGetZoomFadeEndFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZoomFadeEndFn = do
    Lua.pushnumber (Lua.Number (realToFrac zoomFadeEnd))
    return 1

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
            registry ← readIORef (materialRegistryRef env)
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
                                    hardness = mpHardness $ getMaterialProps registry baseMat
                                    (finalElev, _) = applyTimeline timeline worldSize gx gy hardness (baseElev, baseMat)
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
    Lua.liftIO $ rotateCamera env rotateCW
    return 0

-- | camera.rotateCCW()
cameraRotateCCWFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraRotateCCWFn env = do
    Lua.liftIO $ rotateCamera env rotateCCW
    return 0

-- | Shared rotation logic: find the tile at visual screen center,
--   rotate, and re-center the camera on that same tile.
rotateCamera ∷ EngineEnv → (CameraFacing → CameraFacing) → IO ()
rotateCamera env rotateFn = do
    cam ← readIORef (cameraRef env)
    let oldFacing = camFacing cam
        (cx, cy)  = camPosition cam
        zSlice    = camZSlice cam
        newFacing = rotateFn oldFacing

    -- Find the tile at the visual center of the screen.
    -- Walk down from zSlice like the cursor hit-test does.
    mHit ← findVisualCenterTile env oldFacing cx cy zSlice

    case mHit of
        Just (gx, gy, surfZ) → do
            -- Where that tile renders in the old facing (at its actual elevation)
            let zOffset = fromIntegral (zSlice - surfZ) * tileSideHeight
            -- Where that same tile will be in the new facing
                (nx, ny) = gridToWorld newFacing gx gy
            -- Apply same height offset so camera stays at same visual height
                newCy = ny + zOffset
            atomicModifyIORef' (cameraRef env) $ \cam' →
                (cam' { camFacing   = newFacing
                      , camPosition = (nx, newCy)
                      , camVelocity = (0, 0)
                      }, ())
        Nothing → do
            -- Fallback: no terrain found, just do grid-based rotation
            let (gx, gy) = worldToGrid oldFacing cx cy
                (nx, ny) = gridToWorld newFacing gx gy
            atomicModifyIORef' (cameraRef env) $ \cam' →
                (cam' { camFacing   = newFacing
                      , camPosition = (nx, ny)
                      , camVelocity = (0, 0)
                      }, ())

    invalidateWorldCaches env

-- | Find the topmost solid tile visible at screen center.
--   Same approach as the cursor hit-test in Quads.hs:
--   walk downward from zSlice, adjusting worldY for each z level,
--   until we find a solid tile.
findVisualCenterTile ∷ EngineEnv → CameraFacing → Float → Float → Int
                     → IO (Maybe (Int, Int, Int))
findVisualCenterTile env facing cx cy zSlice = do
    wm ← readIORef (worldManagerRef env)
    case wmVisible wm of
        [] → return Nothing
        (pageId:_) → case lookup pageId (wmWorlds wm) of
            Nothing → return Nothing
            Just ws → do
                td ← readIORef (wsTilesRef ws)
                let zMin = zSlice - viewDepth
                    tryZ z
                        | z < zMin = Nothing
                        | otherwise =
                            let relZ = z - zSlice
                                adjustedY = cy + fromIntegral relZ * tileSideHeight
                                (gx, gy) = worldToGrid facing cx adjustedY
                                (chunkCoord, (lx, ly)) = globalToChunk gx gy
                            in case lookupChunk chunkCoord td of
                                Nothing → tryZ (z - 1)
                                Just lc →
                                    let col = lcTiles lc V.! columnIndex lx ly
                                        i = z - ctStartZ col
                                        colLen = VU.length (ctMats col)
                                    in if i < 0 ∨ i >= colLen
                                       then tryZ (z - 1)
                                       else if ctMats col VU.! i ≠ 0
                                            then Just (gx, gy, z)
                                            else tryZ (z - 1)
                return (tryZ zSlice)

-- | Look up the surface elevation at the tile under the visual
--   center of the screen (accounting for zSlice height offset).
lookupVisualCenterZ ∷ EngineEnv → CameraFacing → Float → Float → Int → IO (Maybe Int)
lookupVisualCenterZ env facing cx cy zSlice = do
    wm ← readIORef (worldManagerRef env)
    go (wmVisible wm) (wmWorlds wm)
  where
    -- The visual center is shifted in Y by the height offset.
    -- We approximate by sampling at the camera's grid position
    -- (accurate for flat terrain, close enough for hilly terrain).
    (gx, gy) = worldToGrid facing cx cy
    (chunkCoord, (lx, ly)) = globalToChunk gx gy
    go [] _ = return Nothing
    go (pageId:rest) worlds =
        case lookup pageId worlds of
            Nothing → go rest worlds
            Just ws → do
                td ← readIORef (wsTilesRef ws)
                case lookupChunk chunkCoord td of
                    Just lc → return $ Just ((lcSurfaceMap lc) VU.! columnIndex lx ly)
                    Nothing → go rest worlds

-- | Rotate camera screen position one step clockwise.
--   Derivation: CW facing rotates the screen axes (a,b) → (b,-a).
--   Since sx = (a-b)*halfW and sy = (a+b)*halfDH:
--     new_sx =  sy * (halfW / halfDH)
--     new_sy = -sx * (halfDH / halfW)
rotateCamCW ∷ (Float, Float) → (Float, Float)
rotateCamCW (sx, sy) =
    let r = tileHalfWidth / tileHalfDiamondHeight
    in ( sy * r, -sx / r)

-- | Rotate camera screen position one step counter-clockwise.
rotateCamCCW ∷ (Float, Float) → (Float, Float)
rotateCamCCW (sx, sy) =
    let r = tileHalfWidth / tileHalfDiamondHeight
    in (-sy * r,  sx / r)

-- | Decompose screen-space (sx, sy) into the internal isometric
--   axes (a, b) at float precision.  This is the continuous inverse
--   of  sx = (a-b)*halfW,  sy = (a+b)*halfDH.
screenToAB ∷ Float → Float → (Float, Float)
screenToAB sx sy =
    let hw  = tileHalfWidth
        hdh = tileHalfDiamondHeight
        a   = sx / (2.0 * hw) + sy / (2.0 * hdh)
        b   = sy / (2.0 * hdh) - sx / (2.0 * hw)
    in (a, b)

-- | Recompose isometric axes (a, b) into screen-space (sx, sy).
abToScreen ∷ Float → Float → (Float, Float)
abToScreen a b =
    let hw  = tileHalfWidth
        hdh = tileHalfDiamondHeight
    in ((a - b) * hw, (a + b) * hdh)

-- | One CW facing step rotates (a, b) → (b, -a).
rotateScreenPosCW ∷ (Float, Float) → (Float, Float)
rotateScreenPosCW (sx, sy) =
    let (a, b) = screenToAB sx sy
        -- CW: new_a = b, new_b = -a  (matches applyFacing South→West)
    in abToScreen b (-a)

-- | One CCW facing step rotates (a, b) → (-b, a).
rotateScreenPosCCW ∷ (Float, Float) → (Float, Float)
rotateScreenPosCCW (sx, sy) =
    let (a, b) = screenToAB sx sy
        -- CCW: new_a = -b, new_b = a  (matches applyFacing South→East)
    in abToScreen (-b) a

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
    manager ← readIORef (worldManagerRef env)
    forM_ (wmWorlds manager) $ \(_, ws) → do
        writeIORef (wsQuadCacheRef ws)     Nothing
        writeIORef (wsZoomQuadCacheRef ws) Nothing
        writeIORef (wsBgQuadCacheRef ws)   Nothing
        writeIORef (wsBakedZoomRef ws)     (V.empty, defaultWorldTextures, FaceSouth)
        writeIORef (wsBakedBgRef ws)       (V.empty, defaultWorldTextures, FaceSouth)

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
