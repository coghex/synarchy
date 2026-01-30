module Engine.Graphics.Vulkan.Types.Font
  ( FontState(..)
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk

data FontState = FontState
  { fsPipeline             ∷ (Vk.Pipeline, Vk.PipelineLayout)
  , fsUIPipeline           ∷ (Vk.Pipeline, Vk.PipelineLayout)
  , fsQuadBuffer           ∷ (Vk.Buffer, Vk.DeviceMemory)
  , fsDescriptorPool       ∷ Vk.DescriptorPool
  , fsDescriptorLayout     ∷ Vk.DescriptorSetLayout
  , pendingInstanceBuffers ∷ V.Vector (Vk.Buffer, Vk.DeviceMemory)
  }
