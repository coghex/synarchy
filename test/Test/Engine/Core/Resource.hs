{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Core.Resource (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Resource
import Engine.Core.Monad
import Engine.Core.Error.Exception
import Engine.Core.Base
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Queue as Q
import Engine.Core.Var
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T

spec ∷ Spec
spec = do
  describe "Engine.Core.Resource" $ do
    describe "Basic Resource Management" $ do
      it "allocates and frees resources properly" $ do
        logFunc ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue ← Q.newQueue
        inputQueue ← Q.newQueue
        logQueue ← Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env ← atomically $ newVar engineEnv
        state ← atomically $ newVar engineState
        
        cleanupCalled ← atomically $ newVar False
        let cont = pure
            
            mockResource = do
              liftIO $ putStrLn "Resource allocated"
              pure "test resource"
              
            mockCleanup resource = do
              liftIO $ putStrLn $ "Cleaning up: " ⧺ resource
              liftIO $ atomically $ writeVar cleanupCalled True
        
        result ← runEngineM (allocResource mockCleanup mockResource) env state cont
        wasCleaned ← atomically $ readVar cleanupCalled
        
        case result of
          Right r → r `shouldBe` "test resource"
          Left err → expectationFailure $ "Unexpected error: " ⧺ show err
        wasCleaned `shouldBe` True

    describe "Nested Resource Management" $ do
      it "handles nested resources correctly" $ do
        logFunc ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue ← Q.newQueue
        inputQueue ← Q.newQueue
        logQueue ← Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logFunc      = logFunc
              }
            engineState = defaultEngineState logFunc
        
        env ← atomically $ newVar engineEnv
        state ← atomically $ newVar engineState
        
        cleanupOrder ← atomically $ newVar []
        let cont = pure
            
            recordCleanup name = do
              current ← atomically $ readVar cleanupOrder
              atomically $ writeVar cleanupOrder (name:current)
            
            innerResource = allocResource 
              (\_ → liftIO $ recordCleanup "inner")
              (pure "inner")
              
            outerResource = allocResource 
              (\_ → liftIO $ recordCleanup "outer")
              (innerResource >> pure "outer")
        
        result ← runEngineM outerResource env state cont
        order ← atomically $ readVar cleanupOrder
        
        order `shouldBe` ["inner", "outer"]  -- Resources should be cleaned up in reverse order

    describe "Resource Cleanup on Error" $ do
      it "ensures cleanup on error" $ do
        logFunc ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
        eventQueue ← Q.newQueue
        inputQueue ← Q.newQueue
        logQueue ← Q.newQueue
        
        let engineEnv = EngineEnv 
              { engineConfig = defaultEngineConfig
              , eventQueue   = eventQueue
              , inputQueue   = inputQueue
              , logQueue     = logQueue 
              }
            engineState = defaultEngineState logFunc
        
        env ← atomically $ newVar engineEnv
        state ← atomically $ newVar engineState
        
        cleanupCalled ← atomically $ newVar False
        let cont = pure
            
            -- First allocate a resource successfully
            failingResource = do
              _ ← allocResource 
                (\_ → liftIO $ atomically $ writeVar cleanupCalled True)
                (pure "resource")
              -- Then fail
              throwSystemError (GLFWError "test error") "Intentional failure"
        
        result ← runEngineM failingResource env state cont
        wasCleaned ← atomically $ readVar cleanupCalled
        
        liftIO $ putStrLn $ "Cleanup called: " ⧺ show wasCleaned
        
        wasCleaned `shouldBe` True
        case result of
          Left _ → pure ()  -- Expected failure
          Right _ → expectationFailure "Expected error but got success"
