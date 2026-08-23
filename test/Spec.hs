-- test/Spec.hs
--
-- BUILD-ONLY IN AUTOMATED GATES (#1153). Both CI
-- (`.github/workflows/ci.yml`) and `make ci` (`tools/ci-local.sh`)
-- COMPILE this suite and never run it: `main` below calls `GLFW.init`
-- and creates a real window, `error`ing on either failure, before
-- `hspec` is ever reached — so on a machine with no display it produces
-- no assertions at all, not a partial run. Running it by hand on a
-- graphics-capable desktop still works and is the only way it executes.
--
-- Every spec that needs no display now lives in
-- `synarchy-test-headless`, which every CI run does execute:
-- `Test.Headless.UPrelude`, `Test.Headless.Core.Queue` and
-- `Test.Headless.Input.State`. Do not add a GPU-free spec here — it
-- would be hostage to the display this suite requires.
module Main where

import UPrelude
import Test.Hspec
import qualified Test.Engine.Graphics.Window.GLFW as TestGLFW
import qualified Test.Engine.Graphics.Vulkan.Instance as VulkanInstance
import qualified Test.Engine.Graphics.Vulkan.Surface as VulkanSurface
import qualified Test.Engine.Graphics.Vulkan.Device as VulkanDevice
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
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

    -- The project-owned `createWindow` coverage (#1573) is driven from
    -- HERE rather than from inside an example. `createWindow` registers
    -- `GLFW.terminate` in its outermost `allocResource` scope, and
    -- `allocResource` runs cleanup when its continuation exits -- so an
    -- example-local invocation would terminate GLFW, destroying the
    -- window created above, under every later example including the
    -- Vulkan surface and device specs. Wrapping the whole `hspec` run
    -- keeps both windows and the GLFW initialization alive for every
    -- example, and still runs the destroy/terminate cleanup once, after
    -- the run exits. The observation it captures is passed to the GLFW
    -- spec as data.
    TestGLFW.withCreatedWindow env initialState $ \createObservation ->
      hspec $ do
        -- Every spec below needs the GLFW window created above: the
        -- GLFW specs query it, and the Vulkan surface/device specs pass
        -- it to `createWindowSurface`. The GPU-free specs moved to
        -- `synarchy-test-headless` in #1153.
        -- GLFW tests
        describe "GLFW Tests" $ TestGLFW.spec env initialState createObservation
        -- Vulkan tests
        describe "Vulkan Tests" $ do
            describe "Engine.Graphics.Vulkan.Instance" $ VulkanInstance.spec env initialState
            describe "Engine.Graphics.Vulkan.Surface" $ VulkanSurface.spec env initialState
            describe "Engine.Graphics.Vulkan.Device" $ VulkanDevice.spec env initialState

    -- Cleanup GLFW. `withCreatedWindow`'s resource cleanup has already
    -- run its own `GLFW.terminate` by this point; a second call is
    -- harmless (GLFW tolerates terminate on an uninitialized library)
    -- and keeps this teardown correct if that bracket is ever removed.
    putStrLn "[Debug] Terminating GLFW..."
    GLFW.terminate
    putStrLn "[Debug] GLFW terminated"
