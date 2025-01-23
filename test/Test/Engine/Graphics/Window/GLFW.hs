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
import Engine.Core.Resource
import Engine.Core.State
import Engine.Core.Queue as Q
import Engine.Core.Var
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Engine.Graphics.Window.GLFW as GLFW
import Control.Exception (SomeException, catch)

-- | Main test specification for GLFW functionality
spec ∷ EngineState → EngineEnv → Spec
spec initialState env = do
  describe "GLFW Window Management" $ do
    it "initializes GLFW successfully" $ do
      runEngineTest initialState env $ do
        supported ← GLFW.vulkanSupported
        liftIO $ supported `shouldBe` True

    it "creates a window with correct dimensions" $ do
      runEngineTest initialState env $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Test Window"
              , wcResizable = True
              }
        window ← allocResource createWindow config
        (width, height) ← getWindowSize (GLFW.getGLFWWindow window)
        liftIO $ do
          width `shouldBe` 800
          height `shouldBe` 600

    it "handles window resizing correctly" $ do
      runEngineTest initialState env $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Resize Test Window"
              , wcResizable = True
              }
        window ← allocResource createWindow config
        let newWidth = 1024
            newHeight = 768
        GLFW.setWindowSize (GLFW.getGLFWWindow window) newWidth newHeight
        (width, height) ← getWindowSize (GLFW.getGLFWWindow window)
        liftIO $ do
          width `shouldBe` newWidth
          height `shouldBe` newHeight

    it "correctly reports framebuffer size" $ do
      runEngineTest initialState env $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Framebuffer Test Window"
              , wcResizable = True
              }
        window ← allocResource createWindow config
        (fbWidth, fbHeight) ← getFramebufferSize (GLFW.getGLFWWindow window)
        liftIO $ do
          fbWidth `shouldBeGreaterThan` 0
          fbHeight `shouldBeGreaterThan` 0

    it "can set window position" $ do
      runEngineTest initialState env $ do
        let config = WindowConfig
              { wcWidth = 800
              , wcHeight = 600
              , wcTitle = "Position Test Window"
              , wcResizable = True
              }
        window ← allocResource createWindow config
        let xPos = 100
            yPos = 100
        GLFW.setWindowPos (GLFW.getGLFWWindow window) xPos yPos
        (x, y) ← GLFW.getWindowPos (GLFW.getGLFWWindow window)
        liftIO $ do
          x `shouldBe` xPos
          y `shouldBe` yPos

  describe "Vulkan Integration" $ do
    it "reports required Vulkan extensions" $ do
      runEngineTest initialState env $ do
        exts ← getRequiredInstanceExtensions
        liftIO $ do
          exts `shouldNotBe` []
          exts `shouldSatisfy` (not . null)
          exts `shouldSatisfy` (any (T.isSuffixOf "surface"))

  where
    -- | Helper to run engine tests
    runEngineTest ∷ ∀ ε α. EngineState → EngineEnv → EngineM ε EngineState α → IO α
    runEngineTest state env action = do
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

    -- | Helper for size comparisons
    shouldBeGreaterThan ∷ (Show a, Ord a) => a → a → Expectation
    shouldBeGreaterThan actual expected =
      actual `shouldSatisfy` (> expected)
