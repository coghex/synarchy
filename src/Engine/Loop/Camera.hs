{-# LANGUAGE Strict #-}
module Engine.Loop.Camera
    ( updateCameraPanning
    , updateCameraMouseDrag
    , updateCameraZoom
    , stepCameraZoom
    , scrollZoomImpulse
    , zoomMin
    , applyLimits
    , cameraYLimitChunks
    , cameraGlacierBufferChunks
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Monad (EngineM, liftIO)
import Engine.Core.State (EngineEnv(..), EngineState(..), TimingState(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Viewport (windowDegenerate)
import Engine.Input.Types (InputState(..))
import Engine.Input.Bindings (isActionDown)
import World.Grid (cameraPanSpeed, cameraPanAccel, cameraPanFriction,
                   tileHalfDiamondHeight, tileHalfWidth)
import World.Types (chunkSize, WorldState(..), WorldManager(..), WorldGenParams(..))

-- | Compute the camera Y limit from the actual world size, fencing the
--   camera @bufferChunks@ chunks inside the glacier rim.
--
--   The effective buffer is bounded by the world's half-size, so the limit
--   can't go negative (which would invert the clampF range) on a world too
--   small to hold it. The smallest supported world (8 chunks → half-size 4)
--   still holds 'cameraGlacierBufferChunks' with room to spare, so the cap
--   is a guard against a pathological caller rather than a live case.
cameraYLimitChunks ∷ Int → Int → Float
cameraYLimitChunks bufferChunks worldSizeChunks =
    let halfSizeChunks = worldSizeChunks `div` 2
        halfTiles = halfSizeChunks * chunkSize
        effBuffer = min bufferChunks halfSizeChunks
        glacierBuffer = chunkSize * effBuffer
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

-- | The glacier buffer (in chunks) EVERY camera path is fenced by —
--   keyboard pan, middle-drag and @camera.goToTile@ alike (#1953).
--
--   It is a framing boundary, not a safety one: the outermost rim band
--   fills half the screen with the void beyond the world edge, so no
--   camera path is allowed onto it. Teleports once carried their own,
--   larger fence to keep the chunk loader away from the rim entirely,
--   guarding a defect in the shared chunk generator that has since been
--   repaired there (#298, PR #363 — the @minBound@ sentinel guard in
--   "World.Generate.Chunk"), which is why one buffer now serves all three.
cameraGlacierBufferChunks ∷ Int
cameraGlacierBufferChunks = 2

-- | When facing South/North: X wraps, Y is clamped (glaciers at top/bottom)
--   When facing West/East:   Y wraps, X is clamped (glaciers at left/right)
--
--   The wrapping axis is never clamped: it is the cylinder's seam and has
--   no edge to stop at.
applyLimits ∷ Int → CameraFacing → Float → Float → (Float, Float)
applyLimits = applyLimitsChunks cameraGlacierBufferChunks

-- | As @applyLimits@, but with a caller-chosen glacier buffer (in chunks),
--   mirroring the parameterization of 'cameraYLimitChunks'.
applyLimitsChunks ∷ Int → Int → CameraFacing → Float → Float → (Float, Float)
applyLimitsChunks bufferChunks worldSize facing cx cy =
    let yLim = cameraYLimitChunks bufferChunks worldSize
    in case facing of
        FaceSouth → (cx, clampF (-yLim) yLim cy)
        FaceNorth → (cx, clampF (-yLim) yLim cy)
        FaceWest  → (clampF (-yLim) yLim cx, cy)
        FaceEast  → (clampF (-yLim) yLim cx, cy)

updateCameraPanning ∷ EngineM σ ()
updateCameraPanning = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    bindings ← liftIO $ readIORef (keyBindingsRef env)
    dt ← gets (deltaTime . timingState)
    worldSize ← liftIO $ getWorldSize env

    -- Pan directions are bindable actions (default: arrows + WASD), read
    -- from the live keybinding table so rebinding changes camera control.
    let actionDown a = isActionDown a bindings inpSt

        dtF = realToFrac dt ∷ Float

        inputX = (if actionDown "moveRight" then  1 else 0)
               + (if actionDown "moveLeft"  then -1 else 0)
        inputY = (if actionDown "moveDown"  then  1 else 0)
               + (if actionDown "moveUp"    then -1 else 0)

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

updateCameraMouseDrag ∷ EngineM σ ()
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

            -- Zero-size window (a middle-drag surviving into minimize):
            -- the pixel→world divisions below would corrupt camera
            -- position/velocity with non-finite values. Hold position and
            -- re-anchor the drag origin so restore doesn't jump.
            (True, True) | windowDegenerate winW winH →
                ( cam { camDragOrigin = mousePos
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
zoomMin = 0.25         -- closest zoom

zoomMax ∷ Float
zoomMax = 100

-- | scroll-to-zoom calibration (#596): the velocity impulse contributed
--   by one frame's total scroll delta (every raw GLFW callback since the
--   last frame, summed). Scaled by the delta itself, not merely its
--   sign, so total impulse tracks total scroll amount rather than how
--   many callbacks it arrived as — a wheel notch that the OS splits into
--   several smaller deltas contributes the same total as one clean
--   delta of the same sum. dy > 0 zooms out, dy < 0 zooms in (camZoom is
--   viewport half-height, so smaller = closer).
zoomScrollScale ∷ Float
zoomScrollScale = 1.2

scrollZoomImpulse ∷ Float → Float → Float
scrollZoomImpulse zoom dy = zoomScrollScale * zoom * dy

-- | One frame's zoom integration: apply velocity, clamp to bounds, apply
--   friction (scaled by the pre-update zoom, so deceleration feels
--   consistent whether zoomed in or out), and snap to rest below
--   'zoomMinSpeed'. Pure so it can be exercised directly by tests,
--   independent of the IORef/EngineM plumbing (#596).
stepCameraZoom ∷ Float → Float → Float → (Float, Float)
stepCameraZoom dtF z zv =
    let -- Apply velocity
        z'  = min zoomMax (max zoomMin (z + zv * dtF))
        -- Kill velocity when we hit the zoom floor
        hitMin = z' ≤ zoomMin ∧ zv < 0
        hitMax = z' ≥ zoomMax ∧ zv > 0
        -- Apply friction to velocity
        zv' = if hitMin ∨ hitMax then 0 else applyFriction zv (zoomFriction * z * dtF)
        -- Snap to zero when slow enough
        zv'' = if abs zv' < zoomMinSpeed then 0 else zv'
    in (z', zv'')

updateCameraZoom ∷ EngineM σ ()
updateCameraZoom = do
    env ← ask
    dt ← gets (deltaTime . timingState)
    let dtF = realToFrac dt ∷ Float
    liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        let (z', zv'') = stepCameraZoom dtF (camZoom cam) (camZoomVelocity cam)
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
