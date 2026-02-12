{-# LANGUAGE Strict #-}
module Engine.Input.Event where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef (writeIORef, readIORef)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad (MonadIO(liftIO), EngineM)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM, logDebugSM, logInfoSM)
import Engine.Input.Thread
import Engine.Core.Types
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var (atomically)
import Engine.Core.Error.Exception
import Engine.Graphics.Camera ( Camera2D(..) )
import Engine.Input.Types

----------- Event Processing ------------------------------

handleInputEvents ∷ EngineM ε σ ()
handleInputEvents = do
    env ← ask
    sharedInput ← liftIO $ readIORef (inputStateRef env)
    let pressedKeys = Map.filter keyPressed (inpKeyStates sharedInput)
        keyCount = Map.size pressedKeys
    when (keyCount > 0) $
        logDebugSM CatInput "Input state update"
            [("pressedKeys", T.pack $ show keyCount)
            ,("mousePos", let (x,y) = inpMousePos sharedInput 
                          in (T.pack (show x)) <> "," <> (T.pack (show y)))]
    modify $ \s → s { inputState = sharedInput }
