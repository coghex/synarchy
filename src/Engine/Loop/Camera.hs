{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Loop.Camera
    ( updateCameraPanning
    , updateCameraMouseDrag
    , updateCameraZoom
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
import World.Types (chunkSize, WorldState(..), WorldManager(..), WorldGenParams(..))
import Control.Monad.State.Class (gets)

-- | Compute the camera Y limit from the actual world size.
--   Glaciers sit at the top/bottom edges; we stop the camera
--   two chunks inward so you can't pan past the ice.
cameraYLimit ∷ Int → Float
cameraYLimit worldSizeChunks =
    let halfTiles = (worldSizeChunks * chunkSize) `div` 2
        glacierBuffer = chunkSize * 2
        maxRow = halfTiles - glacierBuffer
    in fromIntegral maxRow * tileHalfDiamondHeight

-- | The full world width in screen-space X.
--   Wrapping grid-X by worldSize chunks (= worldSize * chunkSize tiles)
--   shifts screen-X by (worldSize * chunkSize * tileHalfWidth),
--   because screenX = (gx - gy) * tileHalfWidth and only gx changes.
cameraXWrap ∷ Int → Float
cameraXWrap worldSizeChunks =
    let worldTiles = worldSizeChunks * chunkSize
    in fromIntegral worldTiles * tileHalfWidth

-- | Read the world size from the active world, defaulting to 128.
getWorldSize ∷ EngineEnv → IO Int
getWorldSize env = do
    manager ← readIORef (worldManagerRef env)
    case wmVisible manager of
        (pageId:_) → case lookup pageId (wmWorlds manager) of
            Just ws → do
                mParams ← readIORef (wsGenParamsRef ws)
                return $ case mParams of
                    Just p  → wgpWorldSize p
                    Nothing → 128
            Nothing → return 128
        [] → return 128

wrapCameraAxis ∷ Int → CameraFacing → Float → Float → (Float, Float)
wrapCameraAxis worldSize facing cx cy =
    let w = cameraXWrap worldSize
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

-- | When facing South/North: X wraps, Y is clamped (glaciers at top/bottom)
--   When facing West/East:   Y wraps, X is clamped (glaciers at left/right)
applyLimits ∷ Int → CameraFacing → Float → Float → (Float, Float)
applyLimits worldSize facing cx cy =
    let yLim = cameraYLimit worldSize
    in case facing of
        FaceSouth → (cx, clampF (-yLim) yLim cy)
        FaceNorth → (cx, clampF (-yLim) yLim cy)
        FaceWest  → (clampF (-yLim) yLim cx, cy)
        FaceEast  → (clampF (-yLim) yLim cx, cy)

updateCameraPanning ∷ EngineM ε σ ()
updateCameraPanning = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    dt ← gets (deltaTime . timingState)
    worldSize ← liftIO $ getWorldSize env

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
            (wrappedCx, wrappedCy) = wrapCameraAxis worldSize facing rawCx rawCy
            (cx', cy') = applyLimits worldSize facing wrappedCx wrappedCy

            -- Kill velocity on the clamped axis when hitting the wall
            vx'' = if cx' ≢ wrappedCx then 0 else vx'
            vy'' = if cy' ≢ wrappedCy then 0 else vy'

        in (cam { camPosition = (cx', cy')
                , camVelocity = (vx'', vy'') }, ())

updateCameraMouseDrag ∷ EngineM ε σ ()
updateCameraMouseDrag = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    (winW, winH) ← liftIO $ readIORef (windowSizeRef env)
    worldSize ← liftIO $ getWorldSize env

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

                    (wrappedX, wrappedY) = wrapCameraAxis worldSize facing (cx + realToFrac dx) (cy + realToFrac dy)
                    (finalX, finalY) = applyLimits worldSize facing wrappedX wrappedY
                in ( cam { camPosition = (finalX, finalY)
                         , camDragOrigin = mousePos
                         , camVelocity   = (0, 0)
                         }
                   , () )

            (False, True) →
                ( cam { camDragging = False }
                , () )

            (False, False) →
                (cam, ())

-- | Zoom constants
zoomFriction ∷ Float
zoomFriction = 20.0    -- how fast zoom velocity decays

zoomMinSpeed ∷ Float
zoomMinSpeed = 0.02   -- velocity below this snaps to zero

zoomMin ∷ Float
zoomMin = 0.1         -- closest zoom

updateCameraZoom ∷ EngineM ε σ ()
updateCameraZoom = do
    env ← ask
    dt ← gets (deltaTime . timingState)
    let dtF = realToFrac dt ∷ Float
    liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        let zv  = camZoomVelocity cam
            z   = camZoom cam
            -- Apply velocity
            z'  = max zoomMin (z + zv * dtF)
            -- Kill velocity when we hit the zoom floor
            hitMin = z' ≤ zoomMin ∧ zv < 0
            -- Apply friction to velocity
            zv' = if hitMin then 0 else applyFriction zv (zoomFriction * z * dtF)
            -- Snap to zero when slow enough
            zv'' = if abs zv' < zoomMinSpeed then 0 else zv'
        in (cam { camZoom = z', camZoomVelocity = zv'' }, ())

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
