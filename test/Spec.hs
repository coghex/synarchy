-- test/Spec.hs
module Main where

import UPrelude
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Test.UPrelude as UPrelude
import qualified Test.Engine.Core.Monad as CoreMonad
import qualified Test.Engine.Core.Resource as CoreResource
import qualified Test.Engine.Core.Queue as CoreQueue
import qualified Test.Engine.Graphics.Window.GLFW as TestGLFW
import qualified Test.Engine.Graphics.Vulkan.Instance as VulkanInstance
import qualified Test.Engine.Graphics.Vulkan.Surface as VulkanSurface
import Control.Concurrent (threadDelay)
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Base
import Engine.Core.Queue as Q
import Engine.Core.Error.Exception
import Engine.Input.Types
import qualified Control.Monad.Logger.CallStack as Logger

-- | Initialize a minimal EngineState for testing
initTestState ∷ IO (EngineEnv, EngineState)
initTestState = do
    -- Create queues
    eventQ ← Q.newQueue
    inputQ ← Q.newQueue
    logQ ← Q.newQueue
    
    -- Create logging function
    logFunc ← Logger.runStdoutLoggingT $ Logger.LoggingT pure

    let env = EngineEnv
            { engineConfig = defaultEngineConfig
            , eventQueue = eventQ
            , inputQueue = inputQ
            , logQueue = logQ }
    
    -- Return initial state
    pure (env, defaultEngineState logFunc)

main ∷ IO ()
main = do
    -- Initialize GLFW first
    putStrLn "[Debug] Initializing GLFW..."
    GLFW.setErrorCallback (Just (\e d → 
        putStrLn $ "[GLFW Error] " ⧺ show e ⧺ " " ⧺ show d))
    
    success ← GLFW.init
    unless success $ error "GLFW initialization failed"
    putStrLn "[Debug] GLFW initialized successfully"


    -- Initialize test state
    (env, state) ← initTestState
    putStrLn "[Debug] Test state initialized"

    -- Create window and update state
    putStrLn "[Debug] Creating GLFW window..."
    glfwWin <- GLFW.createRawWindow defaultWindowConfig
    initialState <- case glfwWin of
        Just (Window win) → do
            putStrLn "[Debug] GLFW window created successfully"
            let newState = state { graphicsState = (graphicsState state) {
                    glfwWindow = Just (Window win) } }
            pure newState
        _ → error "Failed to create GLFW window"

    hspec $ do
        -- Core tests (no graphics dependencies)
        describe "Core Tests" $ do
            describe "UPrelude" UPrelude.spec
            describe "Engine.Core.Monad" CoreMonad.spec
            describe "Engine.Core.Resource" CoreResource.spec
            describe "Engine.Core.Queue" CoreQueue.spec
        -- GLFW tests
        describe "GLFW Tests" $ TestGLFW.spec env initialState
        -- Vulkan tests
        describe "Engine.Graphics.Vulkan.Instance" $ VulkanInstance.spec env initialState
        describe "Engine.Graphics.Vulkan.Surface" $ VulkanSurface.spec env initialState

    -- Cleanup GLFW
    putStrLn "[Debug] Terminating GLFW..."
    GLFW.terminate
    putStrLn "[Debug] GLFW terminated"
