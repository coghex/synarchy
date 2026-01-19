{-# LANGUAGE Strict #-}
module Engine.Input.Event where

import UPrelude
import qualified Data.Map as Map
import Data.IORef (writeIORef, readIORef)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad (MonadIO(liftIO), EngineM')
import Engine.Input.Thread
import Engine.Core.Types
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var (atomically)
import Engine.Core.Error.Exception
import Engine.Graphics.Camera ( Camera2D(..) )
import Engine.Input.Types

-- | Process all pending input events
handleInputEvents ∷ EngineM' EngineEnv ()
handleInputEvents = do
    env ← ask
    -- read the shared input state
    sharedInput ← liftIO $ readIORef (inputStateRef env)
    -- update local copy in engine state
    modify $ \s → s { inputState = sharedInput }

-- | Process a single input event
processInputEvent ∷ InputEvent → EngineM' EngineEnv ()
processInputEvent event = do
    env ← ask
    state ← get
    let dt  = deltaTime $ timingState   state
        cam = camera2D  $ graphicsState state
    case event of
        InputKeyEvent key keyState mods → do
            -- Handle escape key
            when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) $ do
                logInfo "Escape pressed, shutting down..."
                liftIO $ writeIORef (lifecycleRef env) CleaningUp
            let newCam = updateCameraFromInput (inputState state) cam dt
            -- Update general input state
            modify $ \s → s { inputState = updateKeyState (inputState s) key keyState mods
                            , graphicsState = (graphicsState s) { camera2D = newCam } }
        InputMouseEvent btn pos state → do
            modify $ \s → s { inputState = updateMouseState (inputState s) btn pos state }
            
        InputWindowEvent winEv → do
            modify $ \s → s { inputState = updateWindowState (inputState s) winEv }
            
        InputScrollEvent x y → do
            modify $ \s → s { inputState = updateScrollState (inputState s) x y }

updateCameraFromInput ∷ InputState → Camera2D → Double → Camera2D
updateCameraFromInput input camera dt =
    let -- Base movement speed
        moveSpeed = 5.0 * realToFrac dt
        rot = camRotation camera
        (px, py) = camPosition camera
        
        -- Raw input
        movingRight = isKeyDown GLFW.Key'D (inpKeyStates input)
        movingLeft  = isKeyDown GLFW.Key'A (inpKeyStates input)
        movingUp    = isKeyDown GLFW.Key'W (inpKeyStates input)
        movingDown  = isKeyDown GLFW.Key'S (inpKeyStates input)
        
        -- Calculate movement vector
        dx = if movingRight then moveSpeed else if movingLeft then -moveSpeed else 0
        dy = if movingUp then moveSpeed else if movingDown then -moveSpeed else 0
        
        -- Transform movement by rotation
        -- Use negative rotation to move relative to camera view
        cosθ = cos (-rot)
        sinθ = sin (-rot)
        
        -- Calculate final movement
        finalDx = dx * cosθ - dy * sinθ
        finalDy = dx * sinθ + dy * cosθ
        
        -- Handle rotation
        rotSpeed = 2.0 * realToFrac dt
        rotDelta = if isKeyDown GLFW.Key'Q (inpKeyStates input)
                   then -rotSpeed
                   else if isKeyDown GLFW.Key'E (inpKeyStates input)
                   then rotSpeed
                   else 0
        
        -- Handle zoom
        zoomSpeed = 2.0 * realToFrac dt
        zoomDelta = if isKeyDown GLFW.Key'Equal (inpKeyStates input)
                    then -zoomSpeed
                    else if isKeyDown GLFW.Key'Minus (inpKeyStates input)
                    then zoomSpeed
                    else 0
        newZoom = max 0.1 $ min 10.0 $ camZoom camera + zoomDelta
        
    in camera
        { camPosition = (px + finalDx, py + finalDy)
        , camZoom = newZoom
        , camRotation = rot + rotDelta
        }
  where
    isKeyDown ∷ GLFW.Key → Map.Map GLFW.Key KeyState → Bool
    isKeyDown key keyStates = 
        case Map.lookup key keyStates of
            Nothing → False
            Just ks → keyPressed ks

