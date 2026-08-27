-- | The framebuffer-resize → swapchain-recreation request (#1693).
--
--   A framebuffer resize used to ask the renderer for nothing.
--   'Engine.Input.Callback.framebufferSizeCallback' queues a
--   @FramebufferResize@, and 'Engine.Input.Thread.Dispatch' writes
--   @framebufferSizeRef@ and notifies Lua — that was all. The renderer
--   recreated only on an exceptional driver status
--   (@ERROR_OUT_OF_DATE_KHR@ on acquire,
--   @ERROR_OUT_OF_DATE_KHR@\/@SUBOPTIMAL_KHR@ on present), and the
--   Khronos
--   <https://docs.vulkan.org/tutorial/latest/03_Drawing_a_triangle/04_Swap_chain_recreation.html swapchain-recreation guidance>
--   handles framebuffer-resize notification separately precisely
--   because out-of-date reporting is not guaranteed on every platform.
--   A WSI that keeps answering @SUCCESS@ across a resize left the
--   window rendering through the stale extent the swapchain was built
--   with.
--
--   == The mechanism
--
--   There is no queue and no pending flag. The request IS the
--   disagreement between the framebuffer state the window is in and
--   the framebuffer state the live swapchain was built for:
--
--   * 'sampleFramebufferState' reads what the window IS, from the two
--     shared refs the input thread writes: @framebufferSizeRef@ (also
--     written by 'Engine.Scripting.Lua.Message.Video.publishWindowGeometry'
--     for the scripted resolution\/window-mode paths) and
--     @framebufferMinimizeGenRef@. Both are reached through
--     'Engine.Core.Capability.RenderView', the worker-safe projection
--     that carries no @engineStateRef@.
--   * 'Engine.Core.State.swapchainFbState' is what the live swapchain
--     was BUILT FOR — main-render-thread-private
--     'Engine.Core.State.GraphicsState', per
--     @docs\/engineenv_capability_inventory.md@ §6.4(a) (render
--     mechanics belong to @EngineState@, not @EngineEnv@).
--
--   Deriving the request rather than recording one is what makes the
--   contract's harder halves fall out for free:
--
--   * __Coalescing.__ Any number of resize events between two frames
--     leave one size in the ref, so the render thread sees one
--     disagreement and recreates once.
--   * __Idempotence.__ An event carrying the size already in effect
--     leaves the two states equal, so it requests nothing.
--   * __No recreation loop.__ The record holds the RAW framebuffer
--     size a recreation was built from, never the resulting
--     @siSwapExtent@. 'Engine.Graphics.Vulkan.Swapchain.chooseSwapExtent'
--     honours the surface's @currentExtent@ and otherwise clamps into
--     @min\/maxImageExtent@, so extent and request legitimately differ
--     on a clamping surface — and comparing against the extent would
--     re-request forever.
--   * __A newer resize survives consumption.__ Nothing is cleared.
--     'recordSwapchainFramebufferState' stores the state the rebuild
--     actually used, so a resize that landed while that rebuild was
--     running is still a disagreement afterwards and gets its own
--     recreation.
--   * __A request always terminates.__ The consuming side hands
--     'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor' the very
--     state it saw outstanding, and that same value is what gets
--     recorded — so serving a request always satisfies exactly the
--     request that was served, with no second sample in between that
--     could disagree with it.
--   * __Every recreation path satisfies the request.__ Resize, VSync,
--     MSAA and all three exceptional-status paths reach
--     'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor', which
--     records what it built with — so one framebuffer change can never
--     cost two recreations.
--
--   == Why the size alone is not enough
--
--   A size is a LEVEL and a minimize is an EDGE. Minimizing and then
--   restoring to the same dimensions leaves @framebufferSizeRef@
--   exactly where it started, and both events can be drained by one
--   'Engine.Input.Thread.Dispatch.processInputs' call before the render
--   thread next looks — so a size-only comparison would see no change
--   at all and never rebuild the swapchain the minimize invalidated.
--   @framebufferMinimizeGenRef@ is the trace that survives that
--   coalescing, bumped by the one thread that sees every event.
--
--   The two refs are read separately and NOT atomically, which is
--   sound because the comparison is repeated every frame and the
--   generation is monotonic. A torn read can only pair an older
--   generation with a newer size (or vice versa); either way the
--   result differs from the record unless BOTH halves match, and a
--   generation bump that has not landed yet is simply seen on the next
--   tick. The failure mode is at worst one extra recreation or a
--   one-frame delay — never a missed one.
--
--   == Scope
--
--   Windowed only, which is both @App.Graphical@ and @App.Preview@
--   (both install the callbacks and run 'Engine.Loop.mainLoop').
--   @App.Offscreen@ and 'Engine.Loop.Headless' never seed the record,
--   so 'pendingFramebufferResize' answers 'ResizeUpToDate' for them by
--   construction and no non-windowed path can reach a swapchain
--   rebuild through here.
module Engine.Graphics.Vulkan.ResizeRequest
  ( FramebufferResizeAction(..)
  , decideFramebufferResize
  , sampleFramebufferState
  , pendingFramebufferResize
  , currentMinimizeGeneration
  , noteMinimizedFramebuffer
  , recordSwapchainFramebufferState
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..))
import Engine.Graphics.Types (FramebufferState(..))

-- | What the render thread must do about the framebuffer state the
--   window is currently in.
data FramebufferResizeAction
  = ResizeUpToDate
    -- ^ The live swapchain already corresponds to this framebuffer
    --   state — or there is no swapchain to recreate yet.
  | ResizeMinimized !FramebufferState
    -- ^ The framebuffer is zero-area. Recreate nothing (Vulkan cannot
    --   build a zero extent), but record this state, so that restoring
    --   — even to the exact size that was in effect before — is a
    --   disagreement again and recreates exactly once.
  | ResizeRecreate !FramebufferState
    -- ^ Recreate for this framebuffer state, and record it.
  deriving (Show, Eq)

-- | Whether a framebuffer size can back a swapchain at all. Either
--   axis at zero is a zero-area framebuffer and an invalid extent, not
--   just the @0x0@ that a minimize typically reports.
zeroArea ∷ (Int, Int) → Bool
zeroArea (w, h) = w ≡ 0 ∨ h ≡ 0

-- | The whole decision, as a pure function of the two states, so the
--   contract is provable without a GPU (see
--   @Test.Headless.Graphics.SwapchainResize@).
--
--   'Nothing' for the live record means no swapchain has been built:
--   headless and offscreen never seed it, and the windowed path seeds
--   it as part of building the first swapchain, so there is never a
--   window in which this could ask for a recreation of something that
--   does not exist.
decideFramebufferResize
  ∷ Maybe FramebufferState  -- ^ what the live swapchain was built for
  → FramebufferState        -- ^ what the window is in now
  → FramebufferResizeAction
decideFramebufferResize Nothing _ = ResizeUpToDate
decideFramebufferResize (Just live) current
  | live ≡ current            = ResizeUpToDate
  | zeroArea (fbsSize current) = ResizeMinimized current
  | otherwise                  = ResizeRecreate current

-- | Read the framebuffer state the window is currently in.
--
--   The generation is read FIRST so that the size sample is at least
--   as recent as it. That ordering is what keeps a torn read
--   conservative: the recorded generation can then lag reality but
--   never lead it, and a lagging generation costs an extra comparison
--   next tick rather than swallowing a minimize.
sampleFramebufferState ∷ EngineM σ FramebufferState
sampleFramebufferState = do
    env ← ask
    let rv = toRenderViewCapability env
    gen ← liftIO $ readIORef (rvFramebufferMinimizeGenRef rv)
    size ← liftIO $ readIORef (rvFramebufferSizeRef rv)
    pure (FramebufferState gen size)

-- | Just the minimize generation, for a caller that already has a
--   framebuffer size of its own to pair it with
--   ('Engine.Graphics.Vulkan.Recreate.recreateSwapchain'). Read it
--   BEFORE that size, for the reason 'sampleFramebufferState' gives.
currentMinimizeGeneration ∷ EngineM σ Word64
currentMinimizeGeneration = do
    env ← ask
    liftIO $ readIORef
      (rvFramebufferMinimizeGenRef (toRenderViewCapability env))

-- | 'decideFramebufferResize' against the live engine state. Read-only:
--   asking does not consume the request, so a caller that fails to act
--   on it simply sees it again next frame.
pendingFramebufferResize ∷ EngineM σ FramebufferResizeAction
pendingFramebufferResize = do
    current ← sampleFramebufferState
    live ← gets (swapchainFbState ∘ graphicsState)
    pure (decideFramebufferResize live current)

-- | Record a zero-area framebuffer state, so that a later restore is a
--   disagreement again — including a restore to the very dimensions
--   that were in effect before minimizing, which the size alone cannot
--   distinguish from no change at all.
noteMinimizedFramebuffer ∷ FramebufferState → EngineM σ ()
noteMinimizedFramebuffer = recordSwapchainFramebufferState

-- | Record the framebuffer state a swapchain was just built for.
--
--   The argument must be the EXACT state the caller decided on — the
--   size handed to
--   'Engine.Graphics.Vulkan.Swapchain.createVulkanSwapchain', not
--   @siSwapExtent@ (which the surface may clamp or override), and the
--   generation sampled with it rather than a fresh read (which may
--   already carry a minimize this swapchain does NOT account for, and
--   whose restore would then be silently dropped).
recordSwapchainFramebufferState ∷ FramebufferState → EngineM σ ()
recordSwapchainFramebufferState fbState =
    modifyGraphicsState $ \gs → gs { swapchainFbState = Just fbState }
