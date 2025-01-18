{-# LANGUAGE Strict #-}
module Engine.Input.Event where

import UPrelude
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, get)
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad (MonadIO(liftIO), EngineM')
import Engine.Input.Thread
import Engine.Core.Types
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Concurrent.Var (atomically)
import Engine.Graphics.Camera ( Camera2D(..) )
import Engine.Input.Types

-- | Process all pending input events
handleInputEvents ∷ EngineM' EngineEnv ()
handleInputEvents = do
    env ← ask
    mEvent ← liftIO $ Q.tryReadQueue (inputQueue env)
    case mEvent of
        Just event → do
            processInputEvent event
            handleInputEvents
        Nothing → return ()

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
                liftIO $ Q.writeQueue (logQueue env) "Escape pressed, shutting down..."
                modify $ \s → s { timingState = (timingState s) {
                                    engineRunning = False } }
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
    let -- Movement
        (px, py) = camPosition camera
        moveSpeed = 5.0 * realToFrac dt
        rot = camRotation camera
        
        -- Get raw movement input
        dx = if isKeyDown GLFW.Key'D (inpKeyStates input)
             then moveSpeed
             else if isKeyDown GLFW.Key'A (inpKeyStates input)
             then -moveSpeed
             else 0
        dy = if isKeyDown GLFW.Key'W (inpKeyStates input)
             then moveSpeed
             else if isKeyDown GLFW.Key'S (inpKeyStates input)
             then -moveSpeed
             else 0
        
        -- Transform movement by camera rotation
        finalDx = dx * cos rot - dy * sin rot
        finalDy = dx * sin rot + dy * cos rot
        
        -- Handle zoom
        currentZoom = camZoom camera
        zoomDelta = if isKeyDown GLFW.Key'Equal (inpKeyStates input)
                    then -moveSpeed
                    else if isKeyDown GLFW.Key'Minus (inpKeyStates input)
                    then moveSpeed
                    else 0
        newZoom = max 0.1 $ min 10.0 $ currentZoom + zoomDelta
        
        -- Handle rotation
        rotDelta = if isKeyDown GLFW.Key'Q (inpKeyStates input)
                   then -moveSpeed
                   else if isKeyDown GLFW.Key'E (inpKeyStates input)
                   then moveSpeed
                   else 0
    in camera
        { camPosition = (px - finalDx, py - finalDy)  -- Note the negation here
        , camZoom = newZoom
        , camRotation = rot + rotDelta
        }
  where
    isKeyDown ∷ GLFW.Key → Map.Map GLFW.Key KeyState → Bool
    isKeyDown key keyStates = 
        case Map.lookup key keyStates of
            Nothing → False
            Just ks → keyPressed ks

