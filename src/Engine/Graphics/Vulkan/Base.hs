module Engine.Graphics.Vulkan.Base where

import UPrelude
import Vulkan.Core10

data TextureInfo = TextureInfo
  { tiImage    ∷ Image
  , tiView     ∷ ImageView
  , tiSampler  ∷ Sampler
  , tiMemory   ∷ DeviceMemory
  , tiLayout   ∷ ImageLayout
  }

-- | Configuration for pipeline manager
data PipelineState = PipelineState
  { psPipeline       ∷ Pipeline
  , psPipelineLayout ∷ PipelineLayout
  , psRenderPass     ∷ RenderPass
  } deriving (Show)
