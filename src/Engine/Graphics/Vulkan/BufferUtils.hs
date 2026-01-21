-- src/Engine/Graphics/Vulkan/BufferUtils.hs
-- Low-level buffer utilities with no dependencies on Command.hs
module Engine.Graphics.Vulkan.BufferUtils
  ( createVulkanBuffer
  , createVulkanBufferManual
  , findMemoryType
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception
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

-- | Creates a Vulkan buffer WITHOUT automatic cleanup registration
-- Caller is responsible for destroying buffer and freeing memory
createVulkanBufferManual ∷ Device → PhysicalDevice → DeviceSize
                         → BufferUsageFlags → MemoryPropertyFlags 
                         → EngineM ε σ (DeviceMemory, Buffer)
createVulkanBufferManual device pDevice bufferSize usage memProperties = do
  let bufferInfo = zero 
        { size = bufferSize
        , usage = usage
        , sharingMode = SHARING_MODE_EXCLUSIVE
        , queueFamilyIndices = V.empty 
        }
  
  -- Create buffer WITHOUT allocResource
  buffer ← createBuffer device bufferInfo Nothing
  
  -- Get memory requirements
  MemoryRequirements { size=siz, memoryTypeBits=mtb }
    ← getBufferMemoryRequirements device buffer
  
  -- Find suitable memory type
  memTypeIndex ← findMemoryType pDevice mtb memProperties
  
  -- Allocate memory WITHOUT allocResource
  let allocInfo = zero 
        { allocationSize = siz
        , memoryTypeIndex = memTypeIndex
        }
  
  bufferMemory ← allocateMemory device allocInfo Nothing
  
  -- Bind buffer memory
  bindBufferMemory device buffer bufferMemory 0
  
  pure (bufferMemory, buffer)
