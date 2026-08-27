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
--   the disagreement between the framebuffer state the window is in
--   (@framebufferSizeRef@ paired with @framebufferMinimizeGenRef@) and
--   'Engine.Core.State.swapchainFbState', the state the live swapchain
--   was BUILT FOR. Everything below is GPU-free because that seam is
--   the whole decision — the only thing the render thread adds is
--   calling 'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor',
--   whose success is modelled here by
--   'recordSwapchainFramebufferState', which is literally the call
--   that function makes with the state the loop hands it.
--
--   Resizes are pushed through the REAL
--   'Engine.Input.Thread.Dispatch.processInputs' against the live
--   headless 'EngineEnv' (same technique as
--   'Test.Headless.Input.LayerA' — the harness starts no input thread,
--   so the queue is this spec's to drive), so both refs are written by
--   production code and not by the test. That matters most for the
--   coalescing cases: what a single drain leaves behind is exactly
--   what the render thread would have found.
module Test.Headless.Graphics.SwapchainResize (spec) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import Test.Hspec
import Engine.Core.Monad (EngineM', modifyGraphicsState, runEngineM)
import Engine.Core.State (EngineEnv(..), GraphicsState(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Types (FramebufferState(..))
import Engine.Graphics.Vulkan.ResizeRequest
  ( FramebufferResizeAction(..), decideFramebufferResize
  , noteMinimizedFramebuffer, pendingFramebufferResize
  , recordSwapchainFramebufferState, sampleFramebufferState )
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

-- | Queue framebuffer resizes exactly as the GLFW callback does,
--   WITHOUT draining them. Several calls followed by one 'drainInput'
--   is how a burst that lands between two render ticks reaches the
--   render thread.
queueResizes ∷ EngineEnv → [(Int, Int)] → IO ()
queueResizes env =
    mapM_ (\(w, h) → Q.writeQueue (inputQueue env)
                       (InputWindowEvent (FramebufferResize w h)))

-- | One input-thread tick: the real drain, then the Lua notifications
--   it published dropped (this spec shares its engine with the suite).
drainInput ∷ EngineEnv → IO ()
drainInput env = do
    inpSt ← readIORef (inputStateRef env)
    _ ← processInputs env inpSt
    drainLuaQueue env

-- | Queue one resize and drain it — the common single-event case.
pushResize ∷ EngineEnv → Int → Int → IO ()
pushResize env w h = queueResizes env [(w, h)] *> drainInput env

drainLuaQueue ∷ EngineEnv → IO ()
drainLuaQueue env = do
    m ← Q.tryReadQueue (luaQueue env)
    case m of
        Just _  → drainLuaQueue env
        Nothing → pure ()

-- | What the render thread would ask for right now.
requested ∷ EngineEnv → IO FramebufferResizeAction
requested env =
    runE env (pendingFramebufferResize ∷ EngineM' FramebufferResizeAction)

-- | The framebuffer state the window is in, as the render thread reads
--   it — used to build the expected values, so the assertions name the
--   size that matters and never hard-code a generation the rest of the
--   suite may have advanced.
liveState ∷ EngineEnv → IO FramebufferState
liveState env = runE env (sampleFramebufferState ∷ EngineM' FramebufferState)

-- | The state a DIRECT recreation (VSync, MSAA, or one of the three
--   exceptional-status paths) builds for. 'recreateSwapchain' has no
--   requested state of its own and calls exactly this, which is the
--   whole point: the direct and pending paths read ONE source, so a
--   direct rebuild cannot disagree with a pending request.
directRecreationState ∷ EngineEnv → IO FramebufferState
directRecreationState = liveState

-- | Model a SUCCESSFUL recreation — the exact call
--   'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor' makes with
--   the state it built for.
recreated ∷ EngineEnv → FramebufferState → IO ()
recreated env fbState =
    runE env (recordSwapchainFramebufferState fbState ∷ EngineM' ())

-- | Model the minimize branch of that same function.
minimized ∷ EngineEnv → FramebufferState → IO ()
minimized env fbState =
    runE env (noteMinimizedFramebuffer fbState ∷ EngineM' ())

-- | Put both halves of the comparison in a known state: a live
--   swapchain built for the framebuffer state the window is currently
--   in, so every example below starts with nothing pending.
settledAt ∷ EngineEnv → (Int, Int) → IO FramebufferState
settledAt env size = do
    writeIORef (framebufferSizeRef env) size
    drainLuaQueue env
    st ← liveState env
    recreated env st
    pure st

spec ∷ SpecWith EngineEnv
spec = do
    it "a resize pushed through the input thread requests one recreation, and consuming it clears the request exactly once" $ \env → do
        settled ← settledAt env (1280, 720)

        pushResize env 1600 900
        let expected = settled { fbsSize = (1600, 900) }
        requested env `shouldReturn` ResizeRecreate expected
        -- Asking does not consume: an unacted request must still be
        -- there next frame.
        requested env `shouldReturn` ResizeRecreate expected

        recreated env expected
        requested env `shouldReturn` ResizeUpToDate

    it "a resize carrying the size already in effect requests nothing" $ \env → do
        _ ← settledAt env (1280, 720)

        pushResize env 1280 720
        requested env `shouldReturn` ResizeUpToDate

    it "several resizes arriving between two frames coalesce into one recreation" $ \env → do
        settled ← settledAt env (1280, 720)

        -- All three queued, then ONE drain: exactly the burst a render
        -- thread that was busy drawing would find waiting for it.
        queueResizes env [(1300, 740), (1400, 800), (1600, 900)]
        drainInput env

        -- One request, naming only the LATEST size — the intermediate
        -- sizes were never separately requestable, so there is nothing
        -- left to recreate for once this one is served.
        let expected = settled { fbsSize = (1600, 900) }
        requested env `shouldReturn` ResizeRecreate expected
        recreated env expected
        requested env `shouldReturn` ResizeUpToDate

    it "a minimize requests no recreation, and restoring to the SAME size recreates exactly once" $ \env → do
        settled ← settledAt env (1280, 720)

        pushResize env 0 0
        minimizeState ← liveState env
        fbsSize minimizeState `shouldBe` (0, 0)
        fbsMinimizeGen minimizeState `shouldBe` fbsMinimizeGen settled + 1
        requested env `shouldReturn` ResizeMinimized minimizeState
        minimized env minimizeState
        -- Recorded, so the two states agree again: staying minimized is
        -- not a standing request, and the minimize is consumed exactly
        -- once rather than every frame the window spends in the dock.
        requested env `shouldReturn` ResizeUpToDate

        -- The window comes back at the dimensions it left at. The SIZE
        -- alone is now indistinguishable from no change at all; the
        -- bumped minimize generation is what still says otherwise.
        pushResize env 1280 720
        let restored = minimizeState { fbsSize = (1280, 720) }
        requested env `shouldReturn` ResizeRecreate restored
        recreated env restored
        requested env `shouldReturn` ResizeUpToDate

    -- The failure the size-only design had: with both events drained
    -- before the render thread looks, the ref ends exactly where it
    -- started and a size comparison sees nothing to do — leaving the
    -- window presenting through a swapchain the minimize invalidated.
    it "a minimize and a same-size restore drained TOGETHER still recreate exactly once" $ \env → do
        settled ← settledAt env (1280, 720)

        queueResizes env [(0, 0), (1280, 720)]
        drainInput env

        -- The size is back to the settled one, and only the generation
        -- distinguishes this from a window that never moved.
        (readIORef (framebufferSizeRef env)) `shouldReturn` (1280, 720)
        let restored = settled { fbsMinimizeGen = fbsMinimizeGen settled + 1 }
        requested env `shouldReturn` ResizeRecreate restored

        recreated env restored
        requested env `shouldReturn` ResizeUpToDate

    it "a minimize burst costs one recreation on restore, not one per zero-area event" $ \env → do
        settled ← settledAt env (1280, 720)

        queueResizes env [(0, 0), (0, 0), (0, 0), (1280, 720)]
        drainInput env

        let restored = settled { fbsMinimizeGen = fbsMinimizeGen settled + 3 }
        requested env `shouldReturn` ResizeRecreate restored
        recreated env restored
        requested env `shouldReturn` ResizeUpToDate

    -- A VSync/MSAA change landing while a resize callback has fired
    -- but the ~60 Hz input worker has not drained it yet. Sampling
    -- GLFW directly in 'recreateSwapchain' would build and record the
    -- undrained size B, leave the same tick's pending check looking at
    -- the ref's stale A and rebuild for A, then rebuild a third time
    -- once the drain moved the ref to B. Reading one source costs two
    -- rebuilds — the pre-existing one the setting asked for, and the
    -- one the resize asked for.
    it "a direct VSync/MSAA recreation racing an undrained resize costs one further rebuild, not two" $ \env → do
        settled ← settledAt env (1280, 720)

        -- The callback has fired; the input thread has not run yet.
        queueResizes env [(1600, 900)]

        -- The settings message is processed on this tick. It builds
        -- for the state the pending check can also see, never a size
        -- only GLFW knows about yet.
        direct ← directRecreationState env
        direct `shouldBe` settled
        recreated env direct
        -- No redundant rebuild: the direct recreation satisfied the
        -- state the pending check compares against.
        requested env `shouldReturn` ResizeUpToDate

        -- The input thread drains, and the resize gets its own single
        -- recreation.
        drainInput env
        let expected = settled { fbsSize = (1600, 900) }
        requested env `shouldReturn` ResizeRecreate expected
        recreated env expected
        requested env `shouldReturn` ResizeUpToDate

    it "keys on the RAW framebuffer size, so a surface that clamps the swapchain extent cannot loop" $ \env → do
        settled ← settledAt env (1280, 720)

        -- A request the surface will not honour verbatim:
        -- 'Engine.Graphics.Vulkan.Swapchain.chooseSwapExtent' returns
        -- the surface's own currentExtent when it is not the
        -- 0xFFFFFFFF sentinel, and otherwise clamps into
        -- min/maxImageExtent — so siSwapExtent here would come back as
        -- something like 1920x1080, not 4000x3000.
        pushResize env 4000 3000
        let expected = settled { fbsSize = (4000, 3000) }
        requested env `shouldReturn` ResizeRecreate expected

        -- recreateSwapchainFor records what it BUILT FROM, so the
        -- request is satisfied even though the resulting extent
        -- differs.
        recreated env expected
        requested env `shouldReturn` ResizeUpToDate

        -- Recording the clamped extent instead is the mistake this
        -- guards: it would leave the two states permanently unequal
        -- and re-request a recreation every single frame.
        recreated env (expected { fbsSize = (1920, 1080) })
        requested env `shouldReturn` ResizeRecreate expected
        recreated env expected

    it "a resize arriving DURING a recreation survives that recreation" $ \env → do
        settled ← settledAt env (1280, 720)

        pushResize env 1600 900
        let building = settled { fbsSize = (1600, 900) }
        requested env `shouldReturn` ResizeRecreate building

        -- The render thread is now inside recreateSwapchainFor, built
        -- for its own 1600x900 state, while the input thread delivers
        -- the next drag step.
        pushResize env 1700 950

        -- Consumption records the state the rebuild actually used, so
        -- the newer resize is still outstanding rather than silently
        -- dropped.
        recreated env building
        let newer = settled { fbsSize = (1700, 950) }
        requested env `shouldReturn` ResizeRecreate newer
        recreated env newer
        requested env `shouldReturn` ResizeUpToDate

    it "requests nothing when no swapchain has been built (headless and offscreen)" $ \env → do
        writeIORef (framebufferSizeRef env) (1280, 720)
        runE env (modifyGraphicsState (\gs → gs { swapchainFbState = Nothing })
                    ∷ EngineM' ())

        pushResize env 1600 900
        requested env `shouldReturn` ResizeUpToDate

        -- Leave the shared engine as this spec found it.
        _ ← settledAt env (1280, 720)
        pure ()

    it "decides purely from the two framebuffer states" $ \_ → do
        let live = FramebufferState 7 (1280, 720)
        decideFramebufferResize Nothing (FramebufferState 7 (1600, 900))
            `shouldBe` ResizeUpToDate
        decideFramebufferResize (Just live) live `shouldBe` ResizeUpToDate

        let grown = FramebufferState 7 (1600, 900)
        decideFramebufferResize (Just live) grown `shouldBe` ResizeRecreate grown

        -- Same size, later generation: a minimize happened and this is
        -- the restore.
        let restored = FramebufferState 8 (1280, 720)
        decideFramebufferResize (Just live) restored
            `shouldBe` ResizeRecreate restored

        let gone = FramebufferState 8 (0, 0)
        decideFramebufferResize (Just live) gone `shouldBe` ResizeMinimized gone
        -- A single collapsed axis is still a zero-area framebuffer and
        -- still cannot be a swapchain extent.
        let flatW = FramebufferState 8 (1280, 0)
            flatH = FramebufferState 8 (0, 720)
        decideFramebufferResize (Just live) flatW `shouldBe` ResizeMinimized flatW
        decideFramebufferResize (Just live) flatH `shouldBe` ResizeMinimized flatH
