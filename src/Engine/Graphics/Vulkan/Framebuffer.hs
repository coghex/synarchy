{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Framebuffer
  ( createVulkanFramebuffers
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.State
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..))
import Vulkan.Core10
import Vulkan.Zero

-- | Creates framebuffers for each swapchain image view.
-- When msaaImageView is provided (Just), creates MSAA framebuffers with two attachments.
-- When Nothing, creates single-attachment framebuffers (no MSAA).
createVulkanFramebuffers ∷ Device 
                        → RenderPass
                        → SwapchainInfo
                        → V.Vector ImageView     -- ^ Swapchain image views
                        → Maybe ImageView         -- ^ MSAA color image view (shared across all framebuffers)
                        → EngineM ε σ (V.Vector Framebuffer)
createVulkanFramebuffers device renderPass swapInfo imageViews mMsaaView = do
  framebuffers ← V.mapM (createOneFramebuffer device renderPass swapInfo mMsaaView) imageViews
  -- Build cleanup action
  let cleanupAction = V.forM_ framebuffers $ \fb →
          destroyFramebuffer device fb Nothing
  modify $ \s → s { graphicsState = (graphicsState s) {
      vulkanCleanup = (vulkanCleanup (graphicsState s)) {
          cleanupFramebuffers = cleanupAction
      }
  }}
  pure framebuffers
  where
    createOneFramebuffer dev rp si mMsaa swapchainImageView = do
      let Extent2D w h = siSwapExtent si
          attachments = case mMsaa of
              Just msaaView → V.fromList [msaaView, swapchainImageView]
              Nothing       → V.singleton swapchainImageView
          fbInfo = zero
            { renderPass  = rp
            , attachments = attachments
            , width       = w
            , height      = h
            , layers      = 1
            }
      createFramebuffer dev fbInfo Nothing

