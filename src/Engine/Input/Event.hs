{-# LANGUAGE Strict #-}
module Engine.Input.Event where

import UPrelude
import qualified Data.Map as Map
import Data.IORef (writeIORef, readIORef)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad (MonadIO(liftIO), EngineM)
import Engine.Input.Thread
import Engine.Core.Types
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var (atomically)
import Engine.Core.Error.Exception
import Engine.Graphics.Camera ( Camera2D(..) )
import Engine.Input.Types

-- | Process all pending input events
handleInputEvents ∷ EngineM ε σ ()
handleInputEvents = do
    env ← ask
    -- read the shared input state
    sharedInput ← liftIO $ readIORef (inputStateRef env)
    -- update local copy in engine state
    modify $ \s → s { inputState = sharedInput }
