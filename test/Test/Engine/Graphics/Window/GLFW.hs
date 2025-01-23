{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Window.GLFW (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Graphics.Base
import Engine.Core.Error.Exception
import Engine.Core.Monad
import Engine.Core.Base
import Engine.Core.Defaults
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Exception (SomeException, catch)

spec ∷ EngineEnv → EngineState → Spec
spec env initialState = do
  describe "Window Management" $ do
    it "creates a window with basic configuration" $ do
      runEngineTest env initialState $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Test Window"
              , wcResizable = True
              }
        window ← createWindow config
        (width, height) ← getWindowSize (getGLFWWindow window)
        liftIO $ do
          width `shouldBe` 800
          height `shouldBe` 600

    it "can show and hide window" $ do
      runEngineTest env initialState $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Test Window"
              , wcResizable = True
              }
        window ← createWindow config
        showWindow (getGLFWWindow window)
        hideWindow (getGLFWWindow window)

  describe "Vulkan Integration" $ do
    it "returns required Vulkan instance extensions" $ do
      runEngineTest env initialState $ do
        exts ← getRequiredInstanceExtensions
        liftIO $ do
          exts `shouldNotBe` []
          exts `shouldSatisfy` (not . null)

    it "can get framebuffer size" $ do
      runEngineTest env initialState $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Test Window"
              , wcResizable = True
              }
        window ← createWindow config
        (width, height) ← getFramebufferSize (getGLFWWindow window)
        liftIO $ do
          width `shouldBeGreaterThan` 0
          height `shouldBeGreaterThan` 0

  where
    runEngineTest ∷ ∀ ε α. EngineEnv → EngineState → EngineM ε EngineState α → IO α
    runEngineTest env state action = do
      stateVar ← atomically $ newVar state
      envVar ← atomically $ newVar env
      mvar ← atomically $ newVar Nothing
      let cont result = case result of
            Right v → do
              atomically $ writeVar mvar (Just v)
              pure state
            Left err → do
              liftIO $ expectationFailure $ "Engine error: " ⧺ show err
              pure state
      
      _ ← unEngineM action envVar stateVar cont
        `catch` \(e ∷ SomeException) → do
          liftIO $ expectationFailure $ "Unexpected exception: " ⧺ show e
          pure state
      
      result ← atomically $ readVar mvar
      case result of
        Just v → pure v
        Nothing → error "No result produced"

    getGLFWWindow ∷ Window → GLFW.Window
    getGLFWWindow (Window win) = win

    shouldBeGreaterThan ∷ (Show a, Ord a) => a → a → Expectation
    shouldBeGreaterThan actual expected =
      actual `shouldSatisfy` (> expected)
