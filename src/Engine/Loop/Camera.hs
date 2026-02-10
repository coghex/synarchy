{-# LANGUAGE Strict #-}
module Engine.Loop.Camera
    ( updateCameraPanning
    ) where

import UPrelude
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Monad (EngineM, liftIO)
import Engine.Core.State (EngineEnv(..), EngineState(..), TimingState(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Input.Types (InputState(..), KeyState(..))
import World.Grid (cameraPanSpeed)
import Control.Monad.State.Class (gets)

-- | Poll arrow keys from InputState and apply smooth camera panning.
--   Call this once per frame in the main loop, before drawFrame.
updateCameraPanning ∷ EngineM ε σ ()
updateCameraPanning = do
    env ← ask
    inpSt ← liftIO $ readIORef (inputStateRef env)
    dt ← gets (deltaTime . timingState)

    let held k = case Map.lookup k (inpKeyStates inpSt) of
                     Just ks → keyPressed ks
                     Nothing → False

        speed = realToFrac cameraPanSpeed * realToFrac dt

        -- Accumulate directional input
        dx = (if held GLFW.Key'Right then speed   else 0)
           + (if held GLFW.Key'Left  then -speed  else 0)

        -- Vulkan NDC: +Y is down on screen, but for world-space
        -- "up" (towards the back of the isometric grid) is negative Y.
        -- Up arrow should move the camera so tiles shift downward
        -- on screen → camera Y decreases.
        dy = (if held GLFW.Key'Down then speed    else 0)
           + (if held GLFW.Key'Up   then -speed   else 0)

    when (dx /= 0 || dy /= 0) $
        liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
            let (cx, cy) = camPosition cam
            in (cam { camPosition = (cx + dx, cy + dy) }, ())
