{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Types.Cleanup
  ( SwapchainCleanup(..)
  ) where

import UPrelude

-- | Cleanup actions for swapchain-dependent resources
-- All fields are IO () actions that destroy the corresponding resource
data SwapchainCleanup = SwapchainCleanup
  { scSwapchain      ∷ IO ()
  , scImageViews     ∷ IO ()
  , scRenderPass     ∷ IO ()
  , scFramebuffers   ∷ IO ()
  , scBindless       ∷ IO ()
  , scBindlessUI     ∷ IO ()
  , scFont           ∷ IO ()
  , scFontUI         ∷ IO ()
  }
