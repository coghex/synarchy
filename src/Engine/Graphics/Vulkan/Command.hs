{-# LANGUAGE BangPatterns #-}
module Engine.Graphics.Vulkan.Command
  ( -- * Command buffer management
    runCommandsOnce
  , createFrameResources
    -- * Scene recording (re-exported from submodules)
  , recordSceneCommandBuffer
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource (allocResource, locally)
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Command.Record (recordSceneCommandBuffer)
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero

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
  
  void $ locally $ do
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
          , commandBufferCount = 1
          }
    
    cmdBuffers ← allocResource (freeCommandBuffers device cmdPool) $
        allocateCommandBuffers device allocInfo
    
    imageAvailable ← allocResource (\s → destroySemaphore device s Nothing) $
        createSemaphore device zero Nothing

    inFlight ← allocResource (\f → destroyFence device f Nothing) $
        createFence device (zero { flags = FENCE_CREATE_SIGNALED_BIT }) Nothing
    
    pure $ FrameResources
        { frCommandPool    = cmdPool
        , frCommandBuffer  = cmdBuffers
        , frImageAvailable = imageAvailable
        , frInFlight       = inFlight
        }

