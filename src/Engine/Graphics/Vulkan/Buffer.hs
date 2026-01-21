-- src/Engine/Graphics/Vulkan/Buffer.hs
module Engine.Graphics.Vulkan.Buffer
  ( -- Re-export from BufferUtils
    createVulkanBuffer
  , findMemoryType
    -- Buffer operations that need Command
  , copyBuffer
  , createStagingBuffer
  , createUniformBuffer
  , updateUniformBuffer
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Types
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBuffer, findMemoryType)
import Engine.Graphics.Vulkan.Command (runCommandsOnce)
import Vulkan.Core10
import Vulkan.Zero

-- | Copy data between buffers using a command buffer
copyBuffer ∷ Device → CommandPool → Queue 
          → Buffer → Buffer → DeviceSize 
          → EngineM ε σ ()
copyBuffer device cmdPool cmdQueue srcBuffer dstBuffer size =
  runCommandsOnce device cmdPool cmdQueue $ \cmdBuf → do
    let copyRegion = zero 
          { srcOffset = 0
          , dstOffset = 0
          , size = size
          }
    cmdCopyBuffer cmdBuf srcBuffer dstBuffer 
      $ V.singleton copyRegion

-- | Create a staging buffer and fill it with data
createStagingBuffer ∷ Device → PhysicalDevice → DeviceSize 
                   → [Word32] → EngineM ε σ (DeviceMemory, Buffer)
createStagingBuffer device pDevice bufferSize data' = do
  (mem, buf) ← createVulkanBuffer device pDevice bufferSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
  -- Map memory and copy data
  dataPtr ← mapMemory device mem 0 bufferSize zero
  liftIO $ pokeArray (castPtr dataPtr) data'
  unmapMemory device mem
  
  pure (mem, buf)

-- | Create a uniform buffer
createUniformBuffer ∷ Device → PhysicalDevice → DeviceSize 
                    → EngineM ε σ (Buffer, DeviceMemory)
createUniformBuffer device pDevice bufferSize = do
  (mem, buf) ← createVulkanBuffer device pDevice bufferSize
      BUFFER_USAGE_UNIFORM_BUFFER_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
  pure (buf, mem)

-- | Update uniform buffer data
updateUniformBuffer ∷ Storable α ⇒ Device → DeviceMemory → α → EngineM ε σ ()
updateUniformBuffer device memory uboData = do
  let size = fromIntegral $ sizeOf uboData
  dataPtr ← mapMemory device memory 0 size zero
  liftIO $ poke (castPtr dataPtr) uboData
  unmapMemory device memory
