{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Window.GLFW (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Graphics.Base
import Engine.Core.State
import Engine.Core.Monad
import Engine.Core.Base
import Engine.Core.Var
import qualified Graphics.UI.GLFW as GLFW
import Control.Exception (SomeException, catch, throwIO)

-- | Main test specification for GLFW functionality
spec ∷ EngineEnv → EngineState → Spec
spec env state = beforeAll initGLFW $ afterAll cleanupGLFW $ do
    describe "GLFW Window" $ do
        it "creates a window with specified dimensions" $ \_ → do
            let config = WindowConfig 
                    { wcWidth = 800
                    , wcHeight = 600
                    , wcTitle = "Test Window"
                    , wcResizable = True
                    }
            runEngineTest env state $ do
                win <- createWindow config
                (width, height) <- getWindowSize (getGLFWWindow win)
                liftIO $ do
                    width `shouldBe` 800
                    height `shouldBe` 600
                    GLFW.destroyWindow (getGLFWWindow win)

        it "supports Vulkan" $ \_ → do
            runEngineTest env state $ do
                supported <- vulkanSupported
                liftIO $ supported `shouldBe` True

        it "returns required Vulkan extensions" $ \_ → do
            runEngineTest env state $ do
                exts <- getRequiredInstanceExtensions
                liftIO $ length exts `shouldSatisfy` (> 0)

    where
        initGLFW :: IO Bool
        initGLFW = do
            GLFW.setErrorCallback (Just (\e d → 
                putStrLn $ "GLFW Error: " ⧺ show e ⧺ " " ⧺ show d))
            GLFW.init

        cleanupGLFW :: Bool → IO ()
        cleanupGLFW _ = GLFW.terminate

        runEngineTest ∷ ∀ ε α. EngineEnv → EngineState → EngineM ε EngineState α → IO α
        runEngineTest env state action = do
            stateVar ← atomically $ newVar state
            envVar ← atomically $ newVar env
            mvar ← atomically $ newVar Nothing
            
            let cont result = case result of
                    Right v → do
                        atomically $ writeVar mvar (Just v)
                        pure state
                    Left err → error $ "Engine error: " ⧺ show err
            
            _ ← unEngineM action envVar stateVar cont
                `catch` \(e ∷ SomeException) → do
                    liftIO $ putStrLn $ "Exception caught: " ⧺ show e
                    throwIO e
            
            result ← atomically $ readVar mvar
            case result of
                Just v → pure v
                Nothing → error "No result produced"
