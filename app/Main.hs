module Main where

import UPrelude
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (when)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (modify')
import qualified Data.Text as T
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Event.Types

-- | Initial engine environment
initialEnv ∷ EngineEnv
initialEnv = EngineEnv
  { engineConfig = EngineConfig
      { windowWidth  = 800
      , windowHeight = 600
      , enableVSync  = True
      , enableDebug  = True
      }
  , vulkanInstance = undefined  -- We'll implement Vulkan setup later
  , vulkanDevice   = undefined
  }

-- | Initial engine state
initialState ∷ EngineState
initialState = EngineState
  { frameCount    = 0
  , engineRunning = True
  , currentTime   = 0
  , deltaTime     = 0
  }

-- | Test computation in our EngineM monad
testComputation ∷ EngineM ε σ T.Text
testComputation = do
  -- Test state manipulation
  modify' $ \s → s { frameCount = frameCount s + 1 }
  
  -- Test environment access
  env ← ask
  let config = engineConfig env
  
  -- Test error handling
  when (windowWidth config < 100) $
    throwEngineException $ EngineException
      { errorType = ExSystem
      , errorMsg  = "Window too small"
      }
      
  -- Return success message
  pure "Test computation completed successfully"

-- | Run the engine monad
runTest ∷ IO ()
runTest = do
  -- Create initial TVars for environment and state
  envVar ← newTVarIO initialEnv
  stateVar ← newTVarIO initialState
  
  -- Run the test computation
  result ← runEngine testComputation envVar stateVar $ \case
    Left err → do
      putStrLn $ "Error: " ⧺ show err
      pure $ Left err
    Right val → do
      putStrLn $ "Success: " ⧺ show val
      pure $ Right val
      
  -- Print final state
  finalState ← atomically $ readVar stateVar
  putStrLn $ "Final frame count: " ⧺ show (frameCount finalState)

main ∷ IO ()
main = do
  putStrLn "Starting engine monad test..."
  runTest
  putStrLn "Test complete"
