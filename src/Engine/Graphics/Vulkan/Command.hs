-- src/Engine/Graphics/Vulkan/Command.hs
module Engine.Graphics.Vulkan.Command
  ( createVulkanCommandPool
  , createVulkanCommandCollection
  , allocateVulkanCommandBuffers
  , destroyVulkanCommandPool
  , VulkanCommandCollection(..)
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource (allocResource)
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Vulkan.Core10
import Vulkan.Zero

-- | Collection of command buffers and their pool
data VulkanCommandCollection = VulkanCommandCollection
  { vccCommandPool    ∷ CommandPool
  , vccCommandBuffers ∷ V.Vector CommandBuffer
  }

-- | Create a Vulkan command pool for allocating command buffers
createVulkanCommandPool ∷ Device → DevQueues → EngineM ε σ CommandPool
createVulkanCommandPool device queues = do
  let poolInfo = zero
        { flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        , queueFamilyIndex = graphicsFamIdx queues
        } ∷ CommandPoolCreateInfo
  
  allocResource (\pool → destroyCommandPool device pool Nothing) $
    createCommandPool device poolInfo Nothing

-- | Allocate Vulkan command buffers from the pool
allocateVulkanCommandBuffers ∷ Device 
                            → CommandPool 
                            → Int  -- ^ Number of buffers to allocate
                            → EngineM ε σ (V.Vector CommandBuffer)
allocateVulkanCommandBuffers device cmdPool numBuffers = do
  let allocInfo = zero
        { commandPool = cmdPool
        , level = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = fromIntegral numBuffers
        }
  
  liftIO $ allocateCommandBuffers device allocInfo

-- | Create a complete Vulkan command buffer collection
createVulkanCommandCollection ∷ Device 
                             → DevQueues 
                             → Int  -- ^ Number of buffers
                             → EngineM ε σ VulkanCommandCollection
createVulkanCommandCollection device queues numBuffers = do
  pool ← createVulkanCommandPool device queues
  buffers ← allocateVulkanCommandBuffers device pool numBuffers
  
  pure $ VulkanCommandCollection
    { vccCommandPool = pool
    , vccCommandBuffers = buffers
    }

-- | Cleanup Vulkan command pool resources
destroyVulkanCommandPool ∷ Device → CommandPool → EngineM ε σ ()
destroyVulkanCommandPool device pool =
  liftIO $ destroyCommandPool device pool Nothing
