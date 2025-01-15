-- src/Engine/Graphics/Vulkan/Buffer.hs
module Engine.Graphics.Vulkan.Buffer
  ( createVulkanBuffer
  , copyBuffer
  , findMemoryType
  , createStagingBuffer
  , createUniformBuffer
  , updateUniformBuffer
  ) where

import UPrelude
import Data.Bits (testBit, (.|.))
import Data.Word (Word32, Word64)
import qualified Data.Vector as V
import Foreign.Storable (Storable, sizeOf, poke)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Array (pokeArray)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Types
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Command
import Vulkan.Core10
import Vulkan.Zero

-- | Creates a Vulkan buffer with the specified properties and allocates memory for it
createVulkanBuffer ∷ Device → PhysicalDevice → DeviceSize
                   → BufferUsageFlags → MemoryPropertyFlags 
                   → EngineM ε σ (DeviceMemory, Buffer)
createVulkanBuffer device pDevice bufferSize usage memProperties = do
  let bufferInfo = zero 
        { size = bufferSize
        , usage = usage
        , sharingMode = SHARING_MODE_EXCLUSIVE
        , queueFamilyIndices = V.empty 
        }
  
  -- Create buffer
  (buffer, freeBufferLater) ← allocResource' 
    (\buf → destroyBuffer device buf Nothing)
    $ createBuffer device bufferInfo Nothing
  
  -- Get memory requirements
  MemoryRequirements { size=siz, memoryTypeBits=mtb }
    ← getBufferMemoryRequirements device buffer
  
  -- Find suitable memory type
  memTypeIndex ← findMemoryType pDevice mtb memProperties
  
  -- Allocate memory
  let allocInfo = zero 
        { allocationSize = siz
        , memoryTypeIndex = memTypeIndex
        }
  
  bufferMemory ← allocResource 
    (\mem → freeMemory device mem Nothing)
    $ allocateMemory device allocInfo Nothing
  
  -- Bind buffer memory
  freeBufferLater
  bindBufferMemory device buffer bufferMemory 0
  
  pure (bufferMemory, buffer)

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

-- | Find a memory type that satisfies both the type filter and properties
findMemoryType ∷ PhysicalDevice → Word32 → MemoryPropertyFlags 
               → EngineM ε σ Word32
findMemoryType pDevice typeFilter properties = do
  PhysicalDeviceMemoryProperties memTypeCount memTypes _ _ ← 
    getPhysicalDeviceMemoryProperties pDevice
  
  let findType i 
        | i ≡ memTypeCount = 
            throwSystemError (MemoryError "findMemoryType: ")
              "failed to find suitable memory type"
        | otherwise = 
            if testBit typeFilter (fromIntegral i)
               ∧ (propertyFlags (memTypes V.! (fromIntegral i))
                   ⌃ properties) ≡ properties
            then pure i 
            else findType (i + 1)
  
  findType 0

-- | Create a staging buffer and fill it with data
createStagingBuffer ∷ Device → PhysicalDevice → DeviceSize 
                   → [Word32] → EngineM ε σ (DeviceMemory, Buffer)
createStagingBuffer device pDevice bufferSize data' = do
  (memory, buffer) ← createVulkanBuffer device pDevice bufferSize
    BUFFER_USAGE_TRANSFER_SRC_BIT
    (MEMORY_PROPERTY_HOST_VISIBLE_BIT ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
  dataPtr ← mapMemory device memory 0 bufferSize zero
  liftIO $ pokeArray (castPtr dataPtr) data'
  unmapMemory device memory
  
  pure (memory, buffer)

createUniformBuffer ∷ Device → PhysicalDevice → Word64
                    → EngineM ε σ (Buffer, DeviceMemory)
createUniformBuffer device pDevice bufferSize = do
    (memory, buffer) ← createVulkanBuffer device pDevice bufferSize
        BUFFER_USAGE_UNIFORM_BUFFER_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    return (buffer, memory)

updateUniformBuffer ∷ Device → DeviceMemory → UniformBufferObject → EngineM' EngineEnv ()
updateUniformBuffer device memory uboData = do
    dataPtr ← mapMemory device memory 0 (fromIntegral $ sizeOf uboData) zero
    liftIO $ poke (castPtr dataPtr) uboData
    unmapMemory device memory
