-- | Issue #1693: a framebuffer resize must ask the renderer to rebuild
--   the swapchain, and must ask exactly once.
--
--   Before #1693 a resize asked for nothing at all: the GLFW callback
--   queued a @FramebufferResize@, the input thread wrote
--   @framebufferSizeRef@ and notified Lua, and the renderer went on
--   presenting through whatever extent the swapchain had been built
--   with — recreating only on an exceptional driver status, which the
--   Khronos guidance is explicit is not reported on every platform.
--
--   The seam under test is
--   "Engine.Graphics.Vulkan.ResizeRequest": no queue and no flag, just
--   the disagreement between @framebufferSizeRef@ (what the window IS)
--   and 'Engine.Core.State.swapchainFbSize' (what the live swapchain
--   was BUILT FOR). Everything below is GPU-free because that seam is
--   the whole decision — the only thing the render thread adds is
--   calling 'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor',
--   whose success is modelled here by
--   'recordSwapchainFramebufferSize', which is literally the call that
--   function makes, with the size the loop hands it.
--
--   Resizes are pushed through the REAL
--   'Engine.Input.Thread.Dispatch.processInputs' against the live
--   headless 'EngineEnv' (same technique as
--   'Test.Headless.Input.LayerA' — the harness starts no input thread,
--   so the queue is this spec's to drive), so the ref is written by
--   production code and not by the test.
module Test.Headless.Graphics.SwapchainResize (spec) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import Test.Hspec
import Engine.Core.Monad (EngineM', modifyGraphicsState, runEngineM)
import Engine.Core.State (EngineEnv(..), GraphicsState(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Vulkan.ResizeRequest
  ( FramebufferResizeAction(..), decideFramebufferResize
  , noteMinimizedFramebuffer, pendingFramebufferResize
  , recordSwapchainFramebufferSize )
import Engine.Input.Thread (processInputs)
import Engine.Input.Types (InputEvent(..), WindowEvent(..))

-- | Run one 'EngineM' action against the live headless env. None of
--   the seam's actions can fail, so a 'Left' is a genuine bug rather
--   than an expected outcome worth threading through every call site.
runE ∷ EngineEnv → EngineM' α → IO α
runE env action = do
    result ← runEngineM action env pure
    case result of
        Right value → pure value
        Left _      → error "swapchain resize seam threw an EngineException"

-- | Deliver one framebuffer resize exactly as the window system does:
--   queue the event the GLFW callback queues, then run the real input
--   drain. Nothing here writes @framebufferSizeRef@ directly.
pushResize ∷ EngineEnv → Int → Int → IO ()
pushResize env w h = do
    Q.writeQueue (inputQueue env) (InputWindowEvent (FramebufferResize w h))
    inpSt ← readIORef (inputStateRef env)
    _ ← processInputs env inpSt
    drainLuaQueue env

-- | Drop the @LuaFramebufferResize@ notifications the drain publishes;
--   this spec shares its engine with the rest of the suite.
drainLuaQueue ∷ EngineEnv → IO ()
drainLuaQueue env = do
    m ← Q.tryReadQueue (luaQueue env)
    case m of
        Just _  → drainLuaQueue env
        Nothing → pure ()

-- | What the render thread would ask for right now.
requested ∷ EngineEnv → IO FramebufferResizeAction
requested env = runE env (pendingFramebufferResize ∷ EngineM' FramebufferResizeAction)

-- | Model a SUCCESSFUL recreation for @fbSize@ — the exact call
--   'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor' makes with
--   the size it built from.
recreated ∷ EngineEnv → (Int, Int) → IO ()
recreated env fbSize =
    runE env (recordSwapchainFramebufferSize fbSize ∷ EngineM' ())

-- | Model the minimize branch of that same function.
minimized ∷ EngineEnv → IO ()
minimized env = runE env (noteMinimizedFramebuffer ∷ EngineM' ())

-- | Put both halves of the comparison in a known state: a live
--   swapchain built for @fbSize@ and a window currently that size, so
--   every example below starts with nothing pending.
settledAt ∷ EngineEnv → (Int, Int) → IO ()
settledAt env fbSize = do
    writeIORef (framebufferSizeRef env) fbSize
    recreated env fbSize
    drainLuaQueue env

spec ∷ SpecWith EngineEnv
spec = do
    it "a resize pushed through the input thread requests one recreation, and consuming it clears the request exactly once" $ \env → do
        settledAt env (1280, 720)

        pushResize env 1600 900
        requested env `shouldReturn` ResizeRecreate 1600 900
        -- Asking does not consume: an unacted request must still be
        -- there next frame.
        requested env `shouldReturn` ResizeRecreate 1600 900

        recreated env (1600, 900)
        requested env `shouldReturn` ResizeUpToDate

    it "a resize carrying the size already in effect requests nothing" $ \env → do
        settledAt env (1280, 720)

        pushResize env 1280 720
        requested env `shouldReturn` ResizeUpToDate

    it "several resizes arriving between two frames coalesce into one recreation" $ \env → do
        settledAt env (1280, 720)

        pushResize env 1300 740
        pushResize env 1400 800
        pushResize env 1600 900

        -- One request, naming only the LATEST size — the intermediate
        -- sizes were never separately requestable, so there is nothing
        -- left to recreate for once this one is served.
        requested env `shouldReturn` ResizeRecreate 1600 900
        recreated env (1600, 900)
        requested env `shouldReturn` ResizeUpToDate

    it "a minimize requests no recreation, and restoring to the SAME size recreates exactly once" $ \env → do
        settledAt env (1280, 720)

        pushResize env 0 0
        requested env `shouldReturn` ResizeMinimized
        minimized env
        -- Recorded, so the two values agree again: staying minimized
        -- is not a standing request, and 'noteMinimizedFramebuffer'
        -- fires exactly once per minimize rather than every frame the
        -- window spends in the dock.
        requested env `shouldReturn` ResizeUpToDate

        -- The window comes back at the dimensions it left at. Without
        -- the recorded minimize this would be indistinguishable from
        -- no change at all, and the swapchain destroyed/invalidated by
        -- the minimize would never be rebuilt.
        pushResize env 1280 720
        requested env `shouldReturn` ResizeRecreate 1280 720
        recreated env (1280, 720)
        requested env `shouldReturn` ResizeUpToDate

    it "keys on the RAW framebuffer size, so a surface that clamps the swapchain extent cannot loop" $ \env → do
        settledAt env (1280, 720)

        -- A request the surface will not honour verbatim:
        -- 'Engine.Graphics.Vulkan.Swapchain.chooseSwapExtent' returns
        -- the surface's own currentExtent when it is not the
        -- 0xFFFFFFFF sentinel, and otherwise clamps into
        -- min/maxImageExtent — so siSwapExtent here would come back as
        -- something like 1920x1080, not 4000x3000.
        pushResize env 4000 3000
        requested env `shouldReturn` ResizeRecreate 4000 3000

        -- recreateSwapchainFor records what it BUILT FROM, so the
        -- request is satisfied even though the resulting extent
        -- differs.
        recreated env (4000, 3000)
        requested env `shouldReturn` ResizeUpToDate

        -- Recording the clamped extent instead is the mistake this
        -- guards: it would leave the two values permanently unequal
        -- and re-request a recreation every single frame.
        recreated env (1920, 1080)
        requested env `shouldReturn` ResizeRecreate 4000 3000
        recreated env (4000, 3000)

    it "a resize arriving DURING a recreation survives that recreation" $ \env → do
        settledAt env (1280, 720)

        pushResize env 1600 900
        requested env `shouldReturn` ResizeRecreate 1600 900

        -- The render thread is now inside recreateSwapchain, built
        -- from its own 1600x900 sample, while the input thread
        -- delivers the next drag step.
        pushResize env 1700 950

        -- Consumption records the size the rebuild actually used, so
        -- the newer resize is still outstanding rather than silently
        -- dropped.
        recreated env (1600, 900)
        requested env `shouldReturn` ResizeRecreate 1700 950
        recreated env (1700, 950)
        requested env `shouldReturn` ResizeUpToDate

    it "requests nothing when no swapchain has been built (headless and offscreen)" $ \env → do
        writeIORef (framebufferSizeRef env) (1280, 720)
        runE env (modifyGraphicsState (\gs → gs { swapchainFbSize = Nothing })
                    ∷ EngineM' ())

        pushResize env 1600 900
        requested env `shouldReturn` ResizeUpToDate

        -- Leave the shared engine as this spec found it.
        settledAt env (1280, 720)

    it "decides purely from the two sizes" $ \_ → do
        decideFramebufferResize Nothing (1600, 900) `shouldBe` ResizeUpToDate
        decideFramebufferResize (Just (1600, 900)) (1600, 900)
            `shouldBe` ResizeUpToDate
        decideFramebufferResize (Just (1280, 720)) (1600, 900)
            `shouldBe` ResizeRecreate 1600 900
        decideFramebufferResize (Just (1280, 720)) (0, 0)
            `shouldBe` ResizeMinimized
        -- A single collapsed axis is still a zero-area framebuffer and
        -- still cannot be a swapchain extent.
        decideFramebufferResize (Just (1280, 720)) (1280, 0)
            `shouldBe` ResizeMinimized
        decideFramebufferResize (Just (1280, 720)) (0, 720)
            `shouldBe` ResizeMinimized
