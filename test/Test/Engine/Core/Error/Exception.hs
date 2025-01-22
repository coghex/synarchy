{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Core.Error.Exception (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Error.Exception
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Core.Base
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

spec ∷ Spec
spec = do
  describe "Engine.Core.Error.Exception" $ do
    describe "Exception Construction" $ do
      it "creates GraphicsError exceptions properly" $ do
        let err = VulkanDeviceLost
            msg = "Device lost during rendering"
            ex = EngineException (ExGraphics err) msg mkErrorContext
        errorType ex `shouldBe` ExGraphics VulkanDeviceLost
        errorMsg ex `shouldBe` msg

      it "creates ResourceError exceptions properly" $ do
        let err = ResourceNotFound "texture.png"
            msg = "Could not find texture file"
            ex = EngineException (ExResource err) msg mkErrorContext
        errorType ex `shouldBe` ExResource (ResourceNotFound "texture.png")
        errorMsg ex `shouldBe` msg

    describe "Exception Throwing and Catching" $ do
      it "can throw and catch exceptions" $ runEngineTest $ do
        result ← tryEngine $ throwGraphicsError VulkanDeviceLost "Test error"
        case result of
          Left ex → do
            liftIO $ errorType ex `shouldBe` ExGraphics VulkanDeviceLost
            liftIO $ errorMsg ex `shouldBe` "Test error"
          Right _ → liftIO $ expectationFailure "Expected exception but got success"

      it "can catch specific exceptions" $ runEngineTest $ do
        result ← throwGraphicsError VulkanDeviceLost "Test error"
          `catchEngine` \ex → case errorType ex of
            ExGraphics VulkanDeviceLost → pure "Caught device lost"
            _ → pure "Caught other error"
        liftIO $ result `shouldBe` "Caught device lost"

    describe "Error Context" $ do
      it "includes call stack in error context" $ do
        let ex = EngineException 
              (ExSystem $ TestError "test") 
              "Test error" 
              mkErrorContext
        let ctx = errorContext ex
        show (contextCallStack ctx) `shouldNotBe` ""

    describe "Different Error Types" $ do
      it "handles SystemError properly" $ runEngineTest $ do
        result ← tryEngine $ throwSystemError (GLFWError "Window creation failed") "Test error"
        case result of
          Left ex → liftIO $ errorType ex `shouldBe` ExSystem (GLFWError "Window creation failed")
          Right _ → liftIO $ expectationFailure "Expected exception but got success"

      it "handles InitError properly" $ runEngineTest $ do
        result ← tryEngine $ throwInitError WindowCreationFailed "Test error"
        case result of
          Left ex → liftIO $ errorType ex `shouldBe` ExInit WindowCreationFailed
          Right _ → liftIO $ expectationFailure "Expected exception but got success"

      it "handles StateError properly" $ runEngineTest $ do
        result ← tryEngine $ throwStateError (InvalidStateTransition "bad state") "Test error"
        case result of
          Left ex → liftIO $ errorType ex `shouldBe` ExState (InvalidStateTransition "bad state")
          Right _ → liftIO $ expectationFailure "Expected exception but got success"

    describe "Error Handling Utilities" $ do
      it "tryEngine returns Right for successful operations" $ runEngineTest $ do
        result ← tryEngine $ pure "success"
        case result of
          Right v → liftIO $ v `shouldBe` "success"
          Left _ → liftIO $ expectationFailure "Expected success but got error"

  where
    runEngineTest ∷ ∀ ε α. EngineM ε EngineState α → IO α
    runEngineTest action = do
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
      
      mvar ← atomically $ newVar Nothing
      let cont result = do
            case result of
              Right v → do
                atomically $ writeVar mvar (Just v)
                pure engineState
              Left err → error $ "Unexpected error: " ⧺ show err
      
      _ ← unEngineM action env state cont
      result ← atomically $ readVar mvar
      case result of
        Just v → pure v
        Nothing → error "No result produced"
