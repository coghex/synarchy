{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Loop.Camera
    ( updateCameraPanning
    , updateCameraMouseDrag
    , updateCameraZoom
    , applyLimits
    , applyLimitsChunks
    , applyGotoLimits
    , cameraYLimit
    , cameraYLimitChunks
    , cameraGotoBufferChunks
    , gotoTileZoomSafe
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
import World.Generate.Constants (chunkLoadRadius)

-- | Compute the camera Y limit from the actual world size, fencing the
--   camera @bufferChunks@ chunks inside the glacier rim.
--
--   The effective buffer is bounded by the world's half-size, so the limit
--   can't go negative (which would invert the clampF range). On the smallest
--   supported worlds (8 chunks → half-size 4) the goto buffer (6) exceeds the
--   half-size and the camera pins to centre (limit 0): there the loader can't
--   reach the rim band the camera-centred initial view already loaded safely.
--   (Loading further toward the rim of such a tiny world heap-overflows
--   regardless of the camera — that is the pre-existing root cause, #298 — so
--   there is nothing closer to the rim a clamp could safely admit.)
cameraYLimitChunks ∷ Int → Int → Float
cameraYLimitChunks bufferChunks worldSizeChunks =
    let halfSizeChunks = worldSizeChunks `div` 2
        halfTiles = halfSizeChunks * chunkSize
        effBuffer = min bufferChunks halfSizeChunks
        glacierBuffer = chunkSize * effBuffer
        maxRow = halfTiles - glacierBuffer
    in fromIntegral maxRow * tileHalfDiamondHeight

-- | The camera Y limit used by the pan/drag paths: glaciers sit at the
--   top/bottom edges, so we stop the camera two chunks inward so you can't
--   pan past the ice.
cameraYLimit ∷ Int → Float
cameraYLimit = cameraYLimitChunks 2

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
applyLimits = applyLimitsChunks 2

-- | As 'applyLimits', but with a caller-chosen glacier buffer (in chunks).
--   Teleports (camera.gotoTile) use a larger buffer than the pan path —
--   see Engine.Scripting.Lua.API.Camera (#297).
applyLimitsChunks ∷ Int → Int → CameraFacing → Float → Float → (Float, Float)
applyLimitsChunks bufferChunks worldSize facing cx cy =
    let yLim = cameraYLimitChunks bufferChunks worldSize
    in case facing of
        FaceSouth → (cx, clampF (-yLim) yLim cy)
        FaceNorth → (cx, clampF (-yLim) yLim cy)
        FaceWest  → (clampF (-yLim) yLim cx, cy)
        FaceEast  → (clampF (-yLim) yLim cx, cy)

-- | Glacier buffer (in chunks) for teleports (camera.gotoTile), as opposed
--   to the 2-chunk buffer the pan/drag paths use.
--
--   gotoTile must fence the camera far enough in that the chunk loader's
--   pull radius around it stops short of the world's v-edge, where
--   generating a rim chunk heap-overflows the world thread (#297; root cause
--   in #298). The loader reaches 'chunkLoadRadius' chunks past the camera,
--   and the outermost rim band overflows on generation — empirically, loading
--   up to halfSize-4 chunks is safe while halfSize-2 overflows on a 128-world.
--   So we keep the loaded region (camera ± chunkLoadRadius) at least
--   'rimUnsafeChunks' inside the rim. The pan path can use a smaller buffer
--   because panning over the rim happens at world-map zoom, where the
--   per-chunk loader is gated off entirely.
--
--   On worlds too small to hold this buffer (the 8-chunk minimum),
--   'cameraYLimitChunks' caps the effective buffer at the world's half-size,
--   pinning the teleport to centre rather than inverting the clamp.
cameraGotoBufferChunks ∷ Int
cameraGotoBufferChunks = chunkLoadRadius + rimUnsafeChunks
  where rimUnsafeChunks = 4

-- | Clamp a teleport target to the gotoTile glacier fence ('cameraGotoBufferChunks').
applyGotoLimits ∷ Int → CameraFacing → Float → Float → (Float, Float)
applyGotoLimits = applyLimitsChunks cameraGotoBufferChunks

-- | Is it safe for a gotoTile teleport to drop to tile-level zoom on a world
--   of this size? Tile zoom enables the per-chunk loader
--   ('World.Thread.ChunkLoading.updateChunkLoading'), which pulls a
--   (2·chunkLoadRadius+1)² Chebyshev square around the camera chunk — its
--   v-corner reaches the clamped camera's v-chunk + 2·chunkLoadRadius. On
--   worlds too small for the goto fence (the 8-chunk minimum, half-size 4)
--   that corner lands on the rim band no matter where the camera is fenced
--   (even centred), so the loader would overflow the world thread (#298).
--   There is no safe zoomed-in region on such a world, so the teleport must
--   stay zoomed out (where the loader is gated off). The 2-chunk margin
--   matches the empirically-safe band (verified on 16- and 128-chunk worlds).
gotoTileZoomSafe ∷ Int → Bool
gotoTileZoomSafe worldSizeChunks =
    let halfSize  = worldSizeChunks `div` 2
        effBuffer = min cameraGotoBufferChunks halfSize
        cornerV   = (halfSize - effBuffer) + 2 * chunkLoadRadius
    in cornerV ≤ halfSize - 2

updateCameraPanning ∷ EngineM ε σ ()
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

updateCameraZoom ∷ EngineM ε σ ()
updateCameraZoom = do
    env ← ask
    dt ← gets (deltaTime . timingState)
    let dtF = realToFrac dt ∷ Float
    liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        let zv  = camZoomVelocity cam
            z   = camZoom cam
            -- Apply velocity
            z'  = min zoomMax (max zoomMin (z + zv * dtF))
            -- Kill velocity when we hit the zoom floor
            hitMin = z' ≤ zoomMin ∧ zv < 0
            hitMax = z' ≥ zoomMax ∧ zv > 0
            -- Apply friction to velocity
            zv' = if hitMin ∨ hitMax then 0 else applyFriction zv (zoomFriction * z * dtF)
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
