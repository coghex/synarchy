{-# LANGUAGE BangPatterns #-}
module Engine.Graphics.Vulkan.Command
  ( -- * Command pool and buffer management
    createVulkanCommandPool
  , createVulkanCommandCollection
  , allocateVulkanCommandBuffer
  , allocateVulkanCommandBuffers
  , destroyVulkanCommandPool
  , beginVulkanCommandBuffer
  , endVulkanCommandBuffer
  , runCommandsOnce
  , submitToQueue
  , createFrameResources
  , VulkanCommandCollection(..)
    -- * Scene recording (re-exported from submodules)
  , recordSceneCommandBuffer
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Vector as V
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Monad
import Engine.Core.Resource (allocResource, locally)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Command.Record (recordSceneCommandBuffer)
import Vulkan.CStruct.Extends
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
                            → Int
                            → EngineM ε σ (V.Vector CommandBuffer)
allocateVulkanCommandBuffers device cmdPool numBuffers = do
  let allocInfo = zero
        { commandPool = cmdPool
        , level = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = fromIntegral numBuffers
        }
  
  allocResource (\cb0 → freeCommandBuffers device cmdPool cb0)
                $ allocateCommandBuffers device allocInfo

-- | Create a complete Vulkan command buffer collection
createVulkanCommandCollection ∷ Device 
                             → DevQueues 
                             → Int
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

-- | Begin recording a command buffer
beginVulkanCommandBuffer ∷ CommandBuffer → EngineM ε σ ()
beginVulkanCommandBuffer cmdBuf = do
  let beginInfo = zero 
        { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        , inheritanceInfo = Nothing
        } ∷ CommandBufferBeginInfo '[]
  
  liftIO $ beginCommandBuffer cmdBuf beginInfo

-- | End recording a command buffer
endVulkanCommandBuffer ∷ CommandBuffer → EngineM ε σ ()
endVulkanCommandBuffer cmdBuf = 
  liftIO $ endCommandBuffer cmdBuf

-- | Submit a command buffer to a queue and wait for it to complete
submitToQueue ∷ Device → Queue → CommandBuffer → EngineM ε σ ()
submitToQueue device queue cmdBuf = locally $ do
  fence ← allocResource (\f → destroyFence device f Nothing) $
    createFence device zero Nothing
  
  let submitInfo = zero
        { waitSemaphores = V.empty
        , commandBuffers = V.singleton (commandBufferHandle cmdBuf)
        , signalSemaphores = V.empty
        }
  
  queueSubmit queue (V.singleton (SomeStruct submitInfo)) fence
  waitForFences device (V.singleton fence) True maxBound
  pure ()

-- | Run commands once and wait for completion
runCommandsOnce ∷ Device → CommandPool → Queue
  → (CommandBuffer → EngineM ε σ α) → EngineM ε σ α
runCommandsOnce device commandPool cmdQueue action = do
  let allocInfo = zero 
        { level = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandPool = commandPool
        , commandBufferCount = 1 
        }
  buffer' ← allocResource (freeCommandBuffers device commandPool)
    $ allocateCommandBuffers device allocInfo
  let buffer = V.head buffer'
      beginInfo = zero 
        { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        , inheritanceInfo = Nothing 
        }
  
  beginCommandBuffer buffer beginInfo
  result ← action buffer
  endCommandBuffer buffer
  
  locally $ do
    fence ← allocResource (\f → destroyFence device f Nothing) $
      createFence device zero Nothing
    
    let submitInfo = zero
          { waitSemaphores = V.empty
          , waitDstStageMask = V.empty
          , commandBuffers = V.singleton (commandBufferHandle buffer)
          , signalSemaphores = V.empty 
          }
    
    queueSubmit cmdQueue (V.singleton (SomeStruct submitInfo)) fence
    waitForFences device (V.singleton fence) True maxBound
  
  pure result

-- | Create a set of frame resources
createFrameResources ∷ Device → DevQueues → EngineM ε σ FrameResources
createFrameResources device queues = do
    let poolInfo = (zero ∷ CommandPoolCreateInfo)
          { flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          , queueFamilyIndex = graphicsFamIdx queues
          }
    
    cmdPool ← allocResource (\pool → destroyCommandPool device pool Nothing) $
        createCommandPool device poolInfo Nothing
    
    let allocInfo = zero
          { commandPool = cmdPool
          , level = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 2
          }
    
    cmdBuffers ← allocResource (freeCommandBuffers device cmdPool) $
        allocateCommandBuffers device allocInfo
    
    imageAvailable ← allocResource (\s → destroySemaphore device s Nothing) $
        createSemaphore device zero Nothing
    
    renderFinished ← allocResource (\s → destroySemaphore device s Nothing) $
        createSemaphore device zero Nothing
    
    inFlight ← allocResource (\f → destroyFence device f Nothing) $
        createFence device (zero { flags = FENCE_CREATE_SIGNALED_BIT }) Nothing
    
    pure $ FrameResources
        { frCommandPool    = cmdPool
        , frCommandBuffer  = cmdBuffers
        , frImageAvailable = imageAvailable
        , frRenderFinished = renderFinished
        , frInFlight       = inFlight
        }

-- | Helper function to allocate a single command buffer
allocateVulkanCommandBuffer ∷ Device → CommandPool → EngineM ε σ CommandBuffer
allocateVulkanCommandBuffer device cmdPool = do
    let allocInfo = zero
          { commandPool = cmdPool
          , level = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    
    buffers ← allocResource (freeCommandBuffers device cmdPool) $
        allocateCommandBuffers device allocInfo
    
    case V.length buffers of
        0 → logAndThrowM CatGraphics (ExGraphics CommandBufferError)
                         "Failed to allocate command buffer"
        _ → pure $ V.head buffers
