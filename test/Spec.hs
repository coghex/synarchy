-- test/Spec.hs
module Main where

import UPrelude
import Test.Hspec
import qualified Test.UPrelude as UPrelude
import qualified Test.Engine.Core.Queue as CoreQueue
import qualified Test.Engine.Core.Var as CoreVar
import qualified Test.Engine.Input.Thread as InputThread
import qualified Test.Engine.Graphics.Window.GLFW as TestGLFW
import qualified Test.Engine.Graphics.Vulkan.Instance as VulkanInstance
import qualified Test.Engine.Graphics.Vulkan.Surface as VulkanSurface
import qualified Test.Engine.Graphics.Vulkan.Device as VulkanDevice
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Var (atomically, readVar)
import Data.IORef (readIORef)
import Engine.Graphics.Config (defaultVideoConfig)

-- | Initialize an engine state for testing via the standard headless path.
initTestState ∷ IO (EngineEnv, EngineState)
initTestState = do
    EngineInitResult env ← initializeEngineHeadless
    st ← readIORef (engineStateRef env)
    pure (env, st)

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
    glfwWin <- GLFW.createRawWindow (defaultWindowConfig defaultVideoConfig)
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
            describe "Engine.Core.Queue" CoreQueue.spec
            describe "Engine.Core.Var" CoreVar.spec
            describe "Engine.Input.Thread" InputThread.spec
        -- GLFW tests
        describe "GLFW Tests" $ TestGLFW.spec env initialState
        -- Vulkan tests
        describe "Vulkan Tests" $ do
            describe "Engine.Graphics.Vulkan.Instance" $ VulkanInstance.spec env initialState
            describe "Engine.Graphics.Vulkan.Surface" $ VulkanSurface.spec env initialState
            describe "Engine.Graphics.Vulkan.Device" $ VulkanDevice.spec env initialState

    -- Cleanup GLFW
    putStrLn "[Debug] Terminating GLFW..."
    GLFW.terminate
    putStrLn "[Debug] GLFW terminated"
