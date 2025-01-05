{-# LANGUAGE Strict #-}
module Engine.Input.Event 
    ( handleInputEvents
    , processInputEvent
    ) where

import UPrelude
import Control.Monad (when)
import Control.Monad.State (modify)
import Control.Monad.Reader (ask)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Types
import Engine.Core.Queue as Q
import Engine.Input.Types
import Engine.Input.Thread
import Engine.Core.Monad (EngineM', MonadIO(liftIO))

-- | Process all pending input events
handleInputEvents ∷ EngineM' EngineEnv ()
handleInputEvents = do
    env ← ask
    mEvent ← liftIO $ Q.tryReadQueue (inputQueue env)
    case mEvent of
        Just event → do
            processInputEvent event
            handleInputEvents  -- recursively process all pending events
        Nothing → return ()

-- | Process a single input event
processInputEvent ∷ InputEvent → EngineM' EngineEnv ()
processInputEvent event = case event of
    InputKeyEvent key keyState mods → do
        modify $ \s → s { inputState = updateKeyState (inputState s) key keyState mods }
        -- Optionally log or handle specific key combinations
        when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed) $
            modify $ \s → s { engineRunning = False }
            
    InputMouseEvent btn pos state → do
        modify $ \s → s { inputState = updateMouseState (inputState s) btn pos state }
        
    InputWindowEvent winEv → do
        modify $ \s → s { inputState = updateWindowState (inputState s) winEv }
        
    InputScrollEvent x y → do
        modify $ \s → s { inputState = updateScrollState (inputState s) x y }
