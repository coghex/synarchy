{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Framebuffer
  ( createFramebuffers
  , destroyFramebuffers
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Image

-- | Create framebuffers for each image view in the swapchain
createFramebuffers ∷ Vk.Device → Vk.RenderPass → SwapchainInfo → V.Vector Vk.ImageView → EngineM ε σ (V.Vector Vk.Framebuffer)
createFramebuffers device renderPass swapchainInfo imageViews = do
  let Vk.Extent2D w h = siSwapExtent swapchainInfo
      createInfo imageView = zero
        { Vk.renderPass = renderPass
        , Vk.attachments = V.singleton imageView
        , Vk.width = w
        , Vk.height = h
        , Vk.layers = 1
        }
  
  allocResource (\fbs → V.mapM_ (flip (Vk.destroyFramebuffer device) Nothing) fbs) $
    V.mapM (\iv → Vk.createFramebuffer device (createInfo iv) Nothing) imageViews

-- | Destroy framebuffers
destroyFramebuffers ∷ Vk.Device → V.Vector Vk.Framebuffer → EngineM ε σ ()
destroyFramebuffers device framebuffers = 
  V.mapM_ (flip (Vk.destroyFramebuffer device) Nothing) framebuffers
