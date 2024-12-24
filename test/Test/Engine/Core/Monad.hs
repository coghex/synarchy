module Test.Engine.Core.Monad where

import UPrelude
import Test.Hspec
import Control.Monad.State.Class ( get, put )
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var

spec ∷ Spec
spec = describe "Engine Monad" $ do
  it "should maintain state through operations" $ do
    let initialState = EngineState
          { frameCount    = 0
          , engineRunning = True
          , currentTime   = 0.0
          , deltaTime     = 0.0
          }
        testAction ∷ EngineM' EngineEnv ()
        testAction = do
          st ← get
          put $ st { frameCount = frameCount st + 1 }
    
    envVar ← atomically $ newVar undefined
    stateVar ← atomically $ newVar initialState
    
    result ← runEngineM testAction envVar stateVar pure
    case result of
      Right _ → do
        finalState ← atomically $ readVar stateVar
        frameCount finalState `shouldBe` 1
      Left err → fail $ show err
