module Engine.Graphics.Vulkan.Base where

import UPrelude
import Vulkan.Core10

-- | A loaded atlas's GPU image. It carries no sampler of its own: a
--   slot either follows the bindless system's shared
--   'btsTextureSampler' or, when registered pinned (world preview, zoom
--   atlas), the sampler recorded for it in @btsPinned@. Either way the
--   sampler comes from the shared refcounted cache, never from this
--   record.
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
