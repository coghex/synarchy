{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Loop.Camera
    ( updateCameraPanning
    , updateCameraMouseDrag
    , updateCameraRotation
    ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.Monad (EngineM, liftIO)
import Engine.Core.State (EngineEnv(..), EngineState(..), TimingState(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), rotateCW, rotateCCW)
import Engine.Input.Types (InputState(..), KeyState(..))
import World.Grid (cameraPanSpeed, cameraPanAccel, cameraPanFriction,
                   tileHalfDiamondHeight, tileHalfWidth)
import World.Types (chunkSize, WorldState(..), WorldManager(..))
import Control.Monad.State.Class (gets)

cameraYLimit ∷ Float
cameraYLimit =
    let worldSizeChunks = 128
        halfTiles = (worldSizeChunks * chunkSize) `div` 2
        glacierBuffer = chunkSize * 2
        maxRow = halfTiles - glacierBuffer
    in fromIntegral maxRow * tileHalfDiamondHeight

-- | The full world width in screen-space X.
--   Wrapping grid-X by worldSize chunks (= worldSize * chunkSize tiles)
--   shifts screen-X by (worldSize * chunkSize * tileHalfWidth),
--   because screenX = (gx - gy) * tileHalfWidth and only gx changes.
cameraXWrap ∷ Float
cameraXWrap =
    let worldSizeChunks = 128
        worldTiles = worldSizeChunks * chunkSize
    in fromIntegral worldTiles * tileHalfWidth

wrapCameraAxis ∷ CameraFacing → Float → Float → (Float, Float)
wrapCameraAxis facing cx cy =
    let w = cameraXWrap
    in case facing of
        FaceSouth → (wrapCoord w cx, cy)
        FaceNorth → (wrapCoord w cx, cy)
        FaceWest  → (cx, wrapCoord w cy)
        FaceEast  → (cx, wrapCoord w cy)

wrapCoord ∷ Float → Float → Float
wrapCoord w x =
    let halfW = w / 2.0
        shifted = x + halfW
        wrapped = shifted - w * fromIntegral (floor (shifted / w) ∷ Int)
    in wrapped - halfW

updateCameraPanning ∷ EngineM ε σ ()
updateCameraPanning = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    dt ← gets (deltaTime . timingState)

    let held k = case Map.lookup k (inpKeyStates inpSt) of
                     Just ks → keyPressed ks
                     Nothing → False

        dtF = realToFrac dt ∷ Float

        inputX = (if held GLFW.Key'Right then  1 else 0)
               + (if held GLFW.Key'Left  then -1 else 0)
        inputY = (if held GLFW.Key'Down  then  1 else 0)
               + (if held GLFW.Key'Up    then -1 else 0)

    liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        let (vx, vy) = camVelocity cam
            zoom     = camZoom cam
            facing   = camFacing cam
            maxSpd   = cameraPanSpeed * zoom
            accel    = cameraPanAccel  * zoom
            friction = cameraPanFriction * zoom

            vx' = stepAxis inputX vx accel friction maxSpd dtF
            vy' = stepAxis inputY vy accel friction maxSpd dtF

            (cx, cy) = camPosition cam
            rawCx = cx + vx' * dtF
            rawCy = cy + vy' * dtF
            (cx', cy'') = wrapCameraAxis facing rawCx rawCy
            cy' = clampF (-cameraYLimit) cameraYLimit cy''
            rawY = cy + vy' * dtF

            vy'' = if cy' ≢ rawY then 0 else vy'

        in (cam { camPosition = (cx', cy')
                , camVelocity = (vx', vy'') }, ())

-- When facing South/North: X wraps, Y is clamped (glaciers at top/bottom)
-- When facing West/East:   Y wraps, X is clamped (glaciers at left/right)
applyLimits ∷ CameraFacing → Float → Float → (Float, Float)
applyLimits facing cx cy = case facing of
    FaceSouth → (cx, clampF (-cameraYLimit) cameraYLimit cy)
    FaceNorth → (cx, clampF (-cameraYLimit) cameraYLimit cy)
    FaceWest  → (clampF (-cameraYLimit) cameraYLimit cx, cy)
    FaceEast  → (clampF (-cameraYLimit) cameraYLimit cx, cy)

updateCameraMouseDrag ∷ EngineM ε σ ()
updateCameraMouseDrag = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    (winW, winH) ← liftIO $ readIORef (windowSizeRef env)

    let middleDown = case Map.lookup GLFW.MouseButton'3 (inpMouseBtns inpSt) of
                         Just True → True
                         _         → False
        mousePos = inpMousePos inpSt

    liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        case (middleDown, camDragging cam) of

            (True, False) →
                ( cam { camDragging   = True
                      , camDragOrigin = mousePos
                      , camVelocity   = (0, 0)
                      }
                , () )

            (True, True) →
                let (mx, my)   = mousePos
                    (ox, oy)   = camDragOrigin cam
                    (cx, cy)   = camPosition cam
                    zoom       = camZoom cam
                    facing     = camFacing cam
                    aspect     = fromIntegral winW / fromIntegral winH

                    pixToWorldX = 2.0 * realToFrac zoom * aspect / fromIntegral winW
                    pixToWorldY = 2.0 * realToFrac zoom          / fromIntegral winH

                    dx = -(mx - ox) * realToFrac pixToWorldX
                    dy = -(my - oy) * realToFrac pixToWorldY

                    newY = clampF (-cameraYLimit) cameraYLimit (cy + realToFrac dy)

                in ( cam { camPosition  = (wrapCameraAxis facing (cx + realToFrac dx) newY)
                         , camDragOrigin = mousePos
                         , camVelocity   = (0, 0)
                         }
                   , () )

            (False, True) →
                ( cam { camDragging = False }
                , () )

            (False, False) →
                (cam, ())

updateCameraRotation ∷ EngineM ε σ ()
updateCameraRotation = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)

    let justPressed k = case Map.lookup k (inpKeyStates inpSt) of
                            Just ks → keyPressed ks
                            Nothing → False

    when (justPressed GLFW.Key'Q) $ liftIO $
        atomicModifyIORef' (cameraRef env) $ \cam →
            (cam { camFacing = rotateCCW (camFacing cam) }, ())

    when (justPressed GLFW.Key'E) $ liftIO $
        atomicModifyIORef' (cameraRef env) $ \cam →
            (cam { camFacing = rotateCW (camFacing cam) }, ())
    -- invalidate baked caches
    when (justPressed GLFW.Key'Q ∨ justPressed GLFW.Key'E) $ liftIO $ do
        manager ← readIORef (worldManagerRef env)
        forM_ (wmWorlds manager) $ \(_, ws) → do
            writeIORef (wsQuadCacheRef ws)     Nothing
            writeIORef (wsZoomQuadCacheRef ws) Nothing
            writeIORef (wsBgQuadCacheRef ws)   Nothing
            writeIORef (wsBakedZoomRef ws)     V.empty
            writeIORef (wsBakedBgRef ws)       V.empty

stepAxis ∷ Float → Float → Float → Float → Float → Float → Float
stepAxis input vel accel friction maxSpd dt
    | input ≢ 0 =
        let dv     = accel * dt * input
            vel'   = vel + dv
        in clampAbs vel' maxSpd
    | otherwise =
        let reduction = friction * dt
        in applyFriction vel reduction

clampAbs ∷ Float → Float → Float
clampAbs v limit
    | v >  limit =  limit
    | v < -limit = -limit
    | otherwise  = v

applyFriction ∷ Float → Float → Float
applyFriction v reduction
    | v > 0     = max 0 (v - reduction)
    | v < 0     = min 0 (v + reduction)
    | otherwise = 0

clampF ∷ Float → Float → Float → Float
clampF lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x
