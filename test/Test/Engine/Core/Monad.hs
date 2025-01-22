{-# LANGUAGE ExplicitForAll #-}
module Test.Engine.Core.Monad (spec) where

import Test.Hspec
import UPrelude
import Engine.Core.Monad
import Engine.Core.Base
import Engine.Core.State
import Engine.Core.Types
import Engine.Core.Defaults
import Engine.Core.Error.Exception
import Engine.Core.Queue as Q
import Engine.Concurrent.Var
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (put)  -- Add this import
import Control.Monad.Error.Class (throwError, catchError)  -- Add this import
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Vector as V

spec ∷ Spec
spec = do
  describe "Engine.Core.Monad" $ do
    -- Test basic monad operations
    describe "Monad Laws" $ do
      it "respects left identity (return a >>= f = f a)" $ do
        logFunc <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue <- Q.newQueue
        inputQueue <- Q.newQueue
        logQueue <- Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env <- atomically $ newVar engineEnv
        state <- atomically $ newVar engineState
        
        let testValue = 42
            f x = pure (x * 2)
            cont = pure
        
        result1 <- runEngineM (return testValue >>= f) env state cont
        result2 <- runEngineM (f testValue) env state cont
        
        case (result1, result2) of
          (Right v1, Right v2) -> v1 `shouldBe` v2
          _ -> expectationFailure "Both results should be Right values"

      it "respects right identity (m >>= return = m)" $ do
        logFunc <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue <- Q.newQueue
        inputQueue <- Q.newQueue
        logQueue <- Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env <- atomically $ newVar engineEnv
        state <- atomically $ newVar engineState
        
        let testValue = pure 42 ∷ EngineM' () Int
            cont = pure
            
        result1 <- runEngineM (testValue >>= return) env state cont
        result2 <- runEngineM testValue env state cont
        
        case (result1, result2) of
          (Right v1, Right v2) -> v1 `shouldBe` v2
          _ -> expectationFailure "Both results should be Right values"

    -- Test MonadError instance
    describe "MonadError" $ do
      it "can throw and catch exceptions" $ do
        logFunc <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue <- Q.newQueue
        inputQueue <- Q.newQueue
        logQueue <- Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env <- atomically $ newVar engineEnv
        state <- atomically $ newVar engineState
        
        let cont = pure
            action = throwError (EngineException (ExSystem (TestError "test error")) "test message" mkErrorContext) `catchError` (\_ -> pure "caught")
            
        result <- runEngineM action env state cont
        result `shouldBe` Right "caught"

    -- Test MonadState instance
    describe "MonadState" $ do
      it "can get and modify state" $ do
        logFunc <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue <- Q.newQueue
        inputQueue <- Q.newQueue
        logQueue <- Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env <- atomically $ newVar engineEnv
        state <- atomically $ newVar engineState
        
        let cont = pure
            action = do
              oldState <- get
              let newState = oldState { timingState = (timingState oldState) 
                                        { frameCount = 1 } }
              put newState
              finalState <- get
              pure (frameCount $ timingState oldState, 
                    frameCount $ timingState finalState)
            
        result <- runEngineM action env state cont
        case result of
          Right (old, new) -> do
            (fromIntegral old) `shouldBe` (0 ∷ Int)
            (fromIntegral new) `shouldBe` (1 ∷ Int)
          Left err -> expectationFailure $ "Unexpected error: " ++ show err

    -- Test MonadReader instance
    describe "MonadReader" $ do
      it "can read environment" $ do
        logFunc <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue <- Q.newQueue
        inputQueue <- Q.newQueue
        logQueue <- Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env <- atomically $ newVar engineEnv
        state <- atomically $ newVar engineState
        
        let cont = pure
            action = do
              env <- ask
              let config = engineConfig env
              pure (windowWidth config, windowHeight config)
            
        result <- runEngineM action env state cont
        case result of
          Right (width, height) -> do
            width `shouldBe` 800
            height `shouldBe` 600
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
