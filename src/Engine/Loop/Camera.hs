{-# LANGUAGE Strict #-}
module Engine.Loop.Camera
    ( updateCameraPanning
    , updateCameraMouseDrag
    ) where

import UPrelude
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Monad (EngineM, liftIO)
import Engine.Core.State (EngineEnv(..), EngineState(..), TimingState(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Input.Types (InputState(..), KeyState(..))
import World.Grid (cameraPanSpeed, cameraPanAccel, cameraPanFriction)
import Control.Monad.State.Class (gets)

-- | Poll arrow keys from InputState and apply smooth camera panning
--   with acceleration and friction.
--   Call this once per frame in the main loop, before drawFrame.
updateCameraPanning ∷ EngineM ε σ ()
updateCameraPanning = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    dt ← gets (deltaTime . timingState)

    let held k = case Map.lookup k (inpKeyStates inpSt) of
                     Just ks → keyPressed ks
                     Nothing → False

        dtF = realToFrac dt :: Float

        -- Input direction: -1, 0, or +1 per axis
        inputX = (if held GLFW.Key'Right then  1 else 0)
               + (if held GLFW.Key'Left  then -1 else 0)
        inputY = (if held GLFW.Key'Down  then  1 else 0)
               + (if held GLFW.Key'Up    then -1 else 0)

    liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
        let (vx, vy) = camVelocity cam
            maxSpd   = cameraPanSpeed
            accel    = cameraPanAccel
            friction = cameraPanFriction

            -- Accelerate or decelerate each axis independently
            vx' = stepAxis inputX vx accel friction maxSpd dtF
            vy' = stepAxis inputY vy accel friction maxSpd dtF

            -- Integrate position
            (cx, cy) = camPosition cam
            cx' = cx + vx' * dtF
            cy' = cy + vy' * dtF

        in (cam { camPosition = (cx', cy')
                , camVelocity = (vx', vy') }, ())

-- | Handle middle-mouse-button camera drag.
--   Converts window-space mouse delta to world-space camera delta
--   using the current zoom and window size.
--   Call once per frame, after updateCameraPanning.
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

            -- Just pressed: start drag, record origin, no movement yet
            (True, False) →
                ( cam { camDragging   = True
                      , camDragOrigin = mousePos
                      , camVelocity   = (0, 0)
                      }
                , () )

            -- Held: compute delta from last frame's position, apply to camera
            (True, True) →
                let (mx, my)   = mousePos
                    (ox, oy)   = camDragOrigin cam
                    (cx, cy)   = camPosition cam
                    zoom       = camZoom cam
                    aspect     = fromIntegral winW / fromIntegral winH

                    -- GLFW cursor positions are in window coordinates, not
                    -- framebuffer pixels. Use windowSize for the conversion.
                    -- Projection maps [-zoom*aspect, +zoom*aspect] → full window width
                    pixToWorldX = 2.0 * realToFrac zoom * aspect / fromIntegral winW
                    pixToWorldY = 2.0 * realToFrac zoom          / fromIntegral winH

                    dx = -(mx - ox) * realToFrac pixToWorldX
                    dy = -(my - oy) * realToFrac pixToWorldY

                in ( cam { camPosition  = (cx + realToFrac dx, cy + realToFrac dy)
                         , camDragOrigin = mousePos
                         , camVelocity   = (0, 0)
                         }
                   , () )

            -- Just released: stop drag
            (False, True) →
                ( cam { camDragging = False }
                , () )

            -- Not dragging, not pressed: nothing to do
            (False, False) →
                (cam, ())

-- | Step a single axis velocity:
--   If input is nonzero, accelerate towards maxSpd in that direction.
--   If input is zero, apply friction towards zero.
stepAxis ∷ Float → Float → Float → Float → Float → Float → Float
stepAxis input vel accel friction maxSpd dt
    | input /= 0 =
        -- Accelerate in input direction, clamp to max speed
        let target = input * maxSpd
            dv     = accel * dt * input
            vel'   = vel + dv
        in clampAbs vel' maxSpd
    | otherwise =
        -- No input: apply friction to slow down
        let reduction = friction * dt
        in applyFriction vel reduction

-- | Clamp a value to [-limit, +limit]
clampAbs ∷ Float → Float → Float
clampAbs v limit
    | v >  limit =  limit
    | v < -limit = -limit
    | otherwise  = v

-- | Reduce magnitude of v towards zero by the given amount.
--   Never overshoots past zero.
applyFriction ∷ Float → Float → Float
applyFriction v reduction
    | v > 0     = max 0 (v - reduction)
    | v < 0     = min 0 (v + reduction)
    | otherwise = 0
