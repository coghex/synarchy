{-# LANGUAGE Strict #-}
module Engine.Input.Event where

import UPrelude
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad (MonadIO(liftIO), EngineM')
import Engine.Input.Thread
import Engine.Core.Types
import Engine.Core.Queue as Q
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
    case event of
        InputKeyEvent key keyState mods → do
            -- Handle escape key
            when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) $ do
                liftIO $ Q.writeQueue (logQueue env) "Escape pressed, shutting down..."
                modify $ \s → s { engineRunning = False }
            -- Update general input state
            modify $ \s → s { inputState = updateKeyState (inputState s) key keyState mods }
            
        InputMouseEvent btn pos state → do
            modify $ \s → s { inputState = updateMouseState (inputState s) btn pos state }
            
        InputWindowEvent winEv → do
            modify $ \s → s { inputState = updateWindowState (inputState s) winEv }
            
        InputScrollEvent x y → do
            modify $ \s → s { inputState = updateScrollState (inputState s) x y }
