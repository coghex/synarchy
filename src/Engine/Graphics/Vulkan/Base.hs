module Engine.Graphics.Vulkan.Base where

import UPrelude
import Vulkan.Core10

-- | A loaded atlas's GPU image. It carries no sampler — every atlas
--   shares the bindless system's single 'btsTextureSampler'.
data TextureInfo = TextureInfo
  { tiImage    ∷ Image
  , tiView     ∷ ImageView
  , tiMemory   ∷ DeviceMemory
  , tiLayout   ∷ ImageLayout
  }

-- | Configuration for pipeline manager
data PipelineState = PipelineState
  { psPipeline       ∷ Pipeline
  , psPipelineLayout ∷ PipelineLayout
  , psRenderPass     ∷ RenderPass
  } deriving (Show)
