{-# LANGUAGE Strict #-}
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
    , cameraApplyScrollZoomFn
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
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv
  , resolveActiveWorld )
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), rotateCW, rotateCCW)
import Engine.Loop.Camera (applyLimits, scrollZoomImpulse)
import World.Grid
import World.Types
import World.Plate (generatePlates, elevationAtGlobal)
import World.Render (surfaceHeadroom)
import World.Generate (globalToChunk, applyTimelineFast, viewDepth)
import qualified Data.Vector as V
import qualified HsLua as Lua

-- | camera.move(dx, dy)
cameraMoveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraMoveFn env = do
    dxArg ← Lua.tonumber 1
    dyArg ← Lua.tonumber 2
    case (dxArg, dyArg) of
        (Just (Lua.Number dx), Just (Lua.Number dy)) → Lua.liftIO $ do
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                let (cx, cy) = camPosition cam
                in (cam { camPosition = ( cx + realToFrac dx
                                        , cy + realToFrac dy ) }, ())
        _ → pure ()
    return 0

-- | camera.getPosition() → x, y
cameraGetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetPositionFn env = do
    (x, y) ← Lua.liftIO $ do
        cam ← readIORef (rvCameraRef (toRenderViewCapability env))
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
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                (cam { camPosition = (realToFrac x, realToFrac y) }, ())
        _ → pure ()
    return 0

-- | camera.getZoom() → zoom
cameraGetZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZoomFn env = do
    z ← Lua.liftIO $ do
        cam ← readIORef (rvCameraRef (toRenderViewCapability env))
        return (camZoom cam)
    Lua.pushnumber (Lua.Number (realToFrac z))
    return 1

-- | camera.setZoom(z)
cameraSetZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZoomFn env = do
    zArg ← Lua.tonumber 1
    case zArg of
        Just (Lua.Number z) → Lua.liftIO $
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
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
    cam ← Lua.liftIO $ readIORef (rvCameraRef (toRenderViewCapability env))
    Lua.pushnumber (Lua.Number (realToFrac (camZoomVelocity cam)))
    return 1

-- | camera.setZoomVelocity(v)
cameraSetZoomVelocityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZoomVelocityFn env = do
    vArg ← Lua.tonumber 1
    case vArg of
        Just (Lua.Number v) → Lua.liftIO $
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                (cam { camZoomVelocity = realToFrac v }, ())
        _ → pure ()
    return 0

-- | camera.applyScrollZoom(dy)
--   Applies one frame's (already-coalesced) scroll delta to zoom
--   velocity, calibrated by scroll amount rather than event count
--   (#596). The single call site both world_view.lua and
--   test_arena.lua's onScroll route through, so the two stay in sync.
cameraApplyScrollZoomFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraApplyScrollZoomFn env = do
    dyArg ← Lua.tonumber 1
    case dyArg of
        Just (Lua.Number dy) → Lua.liftIO $
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                let impulse = scrollZoomImpulse (camZoom cam) (realToFrac dy)
                in (cam { camZoomVelocity = camZoomVelocity cam + impulse }, ())
        _ → pure ()
    return 0

-- | camera.getZSlice() -> int
cameraGetZSliceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZSliceFn env = do
    cam ← Lua.liftIO $ readIORef (rvCameraRef (toRenderViewCapability env))
    Lua.pushinteger (fromIntegral $ camZSlice cam)
    return 1

-- | camera.setZSlice(z)
cameraSetZSliceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZSliceFn env = do
    zArg ← Lua.tointeger 1
    case zArg of
        Just z → Lua.liftIO $ 
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
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
            cam ← readIORef (rvCameraRef (toRenderViewCapability env))
            let facing = camFacing cam
                (wx0, wy0) = gridToWorld facing gx gy

            -- Compute surface elevation from world gen params.
            -- This is a pure computation — no loaded chunks needed.
            manager ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
            -- Track the ACTIVE world only (was: loop every visible world,
            -- last-wins — disagreed with the render-thread z-track and the
            -- rotation hit-test, #81).
            case resolveActiveWorld manager of
                Just (_, worldState) → do
                    mParams ← readIORef (wsGenParamsRef worldState)
                    case mParams of
                        Just params → do
                            let seed      = wgpSeed params
                                worldSize = wgpWorldSize params
                                timeline  = wgpGeoTimeline params
                                plates    = generatePlates seed worldSize (wgpPlateCount params)
                                -- Fence the teleport target on the SAME glacier
                                -- boundary the pan and drag paths use (#1953):
                                -- the outermost rim band frames half a screen of
                                -- the void past the world edge, so no camera path
                                -- is allowed onto it. Only the facing-dependent
                                -- v-axis is clamped; the cylindrical u-axis wraps
                                -- and has no edge. Identity for interior targets.
                                (wx, wy) = applyLimits worldSize facing wx0 wy0
                                -- Derive the z-slice from the CLAMPED tile, where
                                -- the camera actually lands, not the raw request:
                                -- a clamped teleport that ends up far from the
                                -- requested corner should track its real surface,
                                -- and this keeps elevation sampling off wildly
                                -- out-of-bounds coordinates.
                                (gxC, gyC) = worldToGrid facing wx wy
                                (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gxC gyC
                                (finalElev, _) = applyTimelineFast timeline plates worldSize gxC gyC registry (baseElev, baseMat)
                                targetZ = finalElev + surfaceHeadroom
                            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                                (cam { camPosition     = (wx, wy)
                                     -- Tile-level zoom (and the z-tracking that
                                     -- goes with it) on EVERY supported world
                                     -- size, the 8-chunk minimum included: a
                                     -- teleport exists to show the caller a tile.
                                     , camZoom         = 0.5
                                     , camVelocity     = (0, 0)
                                     -- Clear leftover scroll inertia, or the next
                                     -- updateCameraZoom would integrate it and
                                     -- drift the zoom the teleport just chose.
                                     , camZoomVelocity = 0
                                     , camDragging     = False
                                     , camZSlice       = targetZ
                                     , camZTracking    = True
                                     }, ())
                        -- No gen params (world size unknown): there is no world
                        -- extent to clamp against, so set the raw position.
                        Nothing →
                            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                                (cam { camPosition     = (wx0, wy0)
                                     , camZoom         = 0.5
                                     , camVelocity     = (0, 0)
                                     , camZoomVelocity = 0
                                     , camDragging     = False
                                     }, ())
                Nothing →
                    atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                        (cam { camPosition     = (wx0, wy0)
                             , camZoom         = 0.5
                             , camVelocity     = (0, 0)
                             , camZoomVelocity = 0
                             , camDragging     = False
                             }, ())

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
    cam ← readIORef (rvCameraRef (toRenderViewCapability env))
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
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam' →
                (cam' { camFacing   = newFacing
                      , camPosition = (nx, newCy)
                      , camVelocity = (0, 0)
                      }, ())
        Nothing → do
            -- Fallback: no terrain found, just do grid-based rotation
            let (gx, gy) = worldToGrid oldFacing cx cy
                (nx, ny) = gridToWorld newFacing gx gy
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam' →
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
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case resolveActiveWorld wm of
        Nothing → return Nothing
        Just (_, ws) → do
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
                                       else if ctMats col VU.! i ≢ 0
                                            then Just (gx, gy, z)
                                            else tryZ (z - 1)
                return (tryZ zSlice)

-- | camera.getFacing() → int (0=South, 1=West, 2=North, 3=East)
cameraGetFacingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetFacingFn env = do
    cam ← Lua.liftIO $ readIORef (rvCameraRef (toRenderViewCapability env))
    Lua.pushinteger $ case camFacing cam of
        FaceSouth → 0
        FaceWest  → 1
        FaceNorth → 2
        FaceEast  → 3
    return 1

invalidateWorldCaches ∷ EngineEnv → IO ()
invalidateWorldCaches env = do
    _camera ← readIORef (rvCameraRef (toRenderViewCapability env))
    manager ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    forM_ (wmWorlds manager) $ \(_, ws) → do
        bumpQuadCacheGen ws
        writeIORef (wsZoomQuadCacheRef ws) Nothing
        writeIORef (wsBgQuadCacheRef ws)   Nothing
        writeIORef (wsBakedZoomRef ws)     (V.empty, defaultWorldTextures, FaceSouth)
        writeIORef (wsBakedBgRef ws)       (V.empty, defaultWorldTextures, FaceSouth)

-- | camera.getZTracking() -> bool
cameraGetZTrackingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraGetZTrackingFn env = do
    cam ← Lua.liftIO $ readIORef (rvCameraRef (toRenderViewCapability env))
    Lua.pushboolean (camZTracking cam)
    return 1

-- | camera.setZTracking(bool)
cameraSetZTrackingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
cameraSetZTrackingFn env = do
    bArg ← Lua.toboolean 1
    Lua.liftIO $ atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
        (cam { camZTracking = bArg }, ())
    return 0
