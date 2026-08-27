-- | The framebuffer-resize → swapchain-recreation request (#1693).
--
--   A framebuffer resize used to ask the renderer for nothing.
--   'Engine.Input.Callback.framebufferSizeCallback' queues a
--   @FramebufferResize@, and
--   'Engine.Input.Thread.Dispatch' writes @framebufferSizeRef@ and
--   notifies Lua — that was all. The renderer recreated only on an
--   exceptional driver status (@ERROR_OUT_OF_DATE_KHR@ on acquire,
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
--   There is no queue and no flag. The request IS the disagreement
--   between two values that already existed:
--
--   * @framebufferSizeRef@ — the size the window CURRENTLY has,
--     written by the input thread from GLFW's framebuffer callback and
--     by 'Engine.Scripting.Lua.Message.Video.publishWindowGeometry' for
--     the scripted resolution\/window-mode paths. Read here through
--     'Engine.Core.Capability.RenderView', the worker-safe projection
--     that carries no @engineStateRef@ — the narrower of the two views
--     of the same @IORef@, and the one @§3@'s main-render boundary
--     lets this module hold.
--   * 'Engine.Core.State.swapchainFbSize' — main-render-thread-private
--     'Engine.Core.State.GraphicsState', per
--     @docs\/engineenv_capability_inventory.md@ §6.4(a) (render
--     mechanics belong to @EngineState@, not @EngineEnv@). This is the
--     size the LIVE swapchain was built for.
--
--   Deriving the request rather than recording one is what makes the
--   contract's harder halves fall out for free:
--
--   * __Coalescing.__ Any number of resize events between two frames
--     leave one value in the ref, so the render thread sees one
--     disagreement and recreates once.
--   * __Idempotence.__ An event carrying the size already in effect
--     leaves the two equal, so it requests nothing.
--   * __No recreation loop.__ The record holds the RAW framebuffer
--     size a recreation was built from, never the resulting
--     @siSwapExtent@. 'Engine.Graphics.Vulkan.Swapchain.chooseSwapExtent'
--     honours the surface's @currentExtent@ and otherwise clamps into
--     @min\/maxImageExtent@, so extent and request legitimately differ
--     on a clamping surface — and comparing against the extent would
--     re-request forever.
--   * __A newer resize survives consumption.__ Nothing is cleared.
--     'recordSwapchainFramebufferSize' stores the size the rebuild
--     actually used, so a resize that landed in the ref while that
--     rebuild was running is still a disagreement afterwards and gets
--     its own recreation.
--   * __A request always terminates.__ The consuming side hands
--     'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor' the very
--     size it saw outstanding, and that same value is what gets
--     recorded — so serving a request always satisfies exactly the
--     request that was served, with no second GLFW sample in between
--     that could disagree with it.
--   * __Every recreation path satisfies the request.__ Resize, VSync,
--     MSAA and all three exceptional-status paths reach
--     'Engine.Graphics.Vulkan.Recreate.recreateSwapchainFor', which
--     records what it built with — so one framebuffer change can never
--     cost two recreations.
--
--   == Scope
--
--   Windowed only, which is both @App.Graphical@ and @App.Preview@
--   (both install the callbacks and run 'Engine.Loop.mainLoop').
--   @App.Offscreen@ and 'Engine.Loop.Headless' never seed the record,
--   so 'pendingFramebufferResize' answers 'ResizeUpToDate' for them by
--   construction and no non-windowed path can reach
--   @GLFW.getFramebufferSize@ through here.
module Engine.Graphics.Vulkan.ResizeRequest
  ( FramebufferResizeAction(..)
  , decideFramebufferResize
  , pendingFramebufferResize
  , noteMinimizedFramebuffer
  , recordSwapchainFramebufferSize
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..))

-- | What the render thread must do about the framebuffer size the
--   window currently has.
data FramebufferResizeAction
  = ResizeUpToDate
    -- ^ The live swapchain already corresponds to this framebuffer
    --   size — or there is no swapchain to recreate yet.
  | ResizeMinimized
    -- ^ The framebuffer is 0x0. Recreate nothing (Vulkan cannot build
    --   a zero-extent swapchain), but record the minimize so that
    --   restoring — even to the exact size that was in effect before —
    --   is a disagreement again and recreates exactly once.
  | ResizeRecreate !Int !Int
    -- ^ Recreate for this raw framebuffer size.
  deriving (Show, Eq)

-- | The whole decision, as a pure function of the two values, so the
--   contract is provable without a GPU (see
--   @Test.Headless.Graphics.SwapchainResize@).
--
--   'Nothing' for the live record means no swapchain has been built:
--   headless and offscreen never seed it, and the windowed path seeds
--   it as part of building the first swapchain, so there is never a
--   window in which this could ask for a recreation of something that
--   does not exist.
decideFramebufferResize
  ∷ Maybe (Int, Int)  -- ^ raw framebuffer size the live swapchain state corresponds to
  → (Int, Int)        -- ^ raw framebuffer size the window currently has
  → FramebufferResizeAction
decideFramebufferResize Nothing _ = ResizeUpToDate
decideFramebufferResize (Just current) requested@(w, h)
  | current ≡ requested = ResizeUpToDate
  | w ≡ 0 ∨ h ≡ 0       = ResizeMinimized
  | otherwise           = ResizeRecreate w h

-- | 'decideFramebufferResize' against the live engine state. Read-only:
--   asking does not consume the request, so a caller that fails to act
--   on it simply sees it again next frame.
pendingFramebufferResize ∷ EngineM σ FramebufferResizeAction
pendingFramebufferResize = do
    env ← ask
    requested ← liftIO $ readIORef
                  (rvFramebufferSizeRef (toRenderViewCapability env))
    live ← gets (swapchainFbSize ∘ graphicsState)
    pure (decideFramebufferResize live requested)

-- | Record that the window is minimized, so that a later restore is a
--   disagreement again — including a restore to the very dimensions
--   that were in effect before minimizing, which is otherwise
--   indistinguishable from no change at all.
noteMinimizedFramebuffer ∷ EngineM σ ()
noteMinimizedFramebuffer =
    modifyGraphicsState $ \gs → gs { swapchainFbSize = Just (0, 0) }

-- | Record the raw framebuffer size a swapchain was just built from.
--
--   The argument must be the EXACT sample handed to
--   'Engine.Graphics.Vulkan.Swapchain.createVulkanSwapchain' — not
--   @siSwapExtent@ (which the surface may clamp or override) and not a
--   fresh read of the ref or of GLFW (which may already carry a newer
--   resize that this swapchain was NOT built for, and whose recreation
--   would then be silently dropped).
recordSwapchainFramebufferSize ∷ (Int, Int) → EngineM σ ()
recordSwapchainFramebufferSize fbSize =
    modifyGraphicsState $ \gs → gs { swapchainFbSize = Just fbSize }
