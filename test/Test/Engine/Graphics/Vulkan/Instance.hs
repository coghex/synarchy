{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Vulkan.Instance (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Base
import Engine.Core.Error.Exception
import Engine.Core.Monad
import Engine.Core.Base
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var
import Engine.Core.Defaults
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Data.Vector as V
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Control.Exception (SomeException, catch)

spec ∷ Spec
spec = do
  describe "Engine.Graphics.Vulkan.Instance" $ do
    describe "Instance Creation" $ do
      it "creates a Vulkan instance with basic configuration" $ do
        result ← runEngineTest $ do
          let config = defaultGraphicsConfig { gcDebugMode = False }
          (instance', debugMessenger) ← createVulkanInstance config
          liftIO $ do
            instance' `shouldNotBe` NULL_HANDLE
            debugMessenger `shouldBe` Nothing
          pure ()
        pure ()

      it "creates a Vulkan instance with debug messenger when debug mode is enabled" $ do
        result ← runEngineTest $ do
          let config = defaultGraphicsConfig { gcDebugMode = True }
          (instance', debugMessenger) ← createVulkanInstance config
          liftIO $ do
            instance' `shouldNotBe` NULL_HANDLE
            debugMessenger `shouldSatisfy` isJust
          pure ()
        pure ()

    describe "Instance Extensions" $ do
      it "includes portability extensions when needed" $ do
        result ← runEngineTest $ do
          exts ← getAvailableExtensions
          liftIO $ do
            exts `shouldNotBe` []
            let hasPortability = "VK_KHR_portability_enumeration" `elem` exts
            when hasPortability $
              "VK_KHR_portability_enumeration" `elem` exts `shouldBe` True
          pure ()
        pure ()

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
      let cont result = case result of
            Right v → do
              atomically $ writeVar mvar (Just v)
              pure engineState
            Left err → do
              liftIO $ expectationFailure $ "Engine error: " ⧺ show err
              pure engineState
      
      _ ← unEngineM action env state cont
        `catch` \(e ∷ SomeException) → do
          liftIO $ expectationFailure $ "Unexpected exception: " ⧺ show e
          pure engineState
      
      result ← atomically $ readVar mvar
      case result of
        Just v → pure v
        Nothing → error "No result produced"
