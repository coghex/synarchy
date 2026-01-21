module Engine.Graphics.Vulkan.InstanceBuffer
  ( createInstanceBuffer
  ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Graphics.Font.Data
import Foreign.Storable (sizeOf, pokeElemOff)
import Foreign.Ptr (castPtr)
import Vulkan.Core10
import Vulkan.Zero

createInstanceBuffer ∷ Device → PhysicalDevice → V.Vector GlyphInstance 
                     → EngineM ε σ (Buffer, DeviceMemory)
createInstanceBuffer device pDevice instances = do
    let instanceCount = V.length instances
        bufferSize = fromIntegral $ instanceCount * sizeOf (undefined ∷ GlyphInstance)
    
    -- Create buffer
    let bufferInfo = zero 
          { size = bufferSize
          , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
          , sharingMode = SHARING_MODE_EXCLUSIVE
          , queueFamilyIndices = V.empty 
          }
    
    buffer ← createBuffer device bufferInfo Nothing
    
    -- Get memory requirements
    MemoryRequirements { size=memSize, memoryTypeBits=typeFilter } ← 
        getBufferMemoryRequirements device buffer
    
    -- Find memory type
    PhysicalDeviceMemoryProperties memTypeCount memTypes _ _ ← 
        getPhysicalDeviceMemoryProperties pDevice
    
    let findType i 
          | i == memTypeCount = 
              throwSystemError (MemoryError "createInstanceBuffer: ")
                "failed to find suitable memory type"
          | otherwise = 
              let memType = memTypes V.! (fromIntegral i)
                  properties = MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
              in if testBit typeFilter (fromIntegral i)
                     && (propertyFlags memType .&. properties) == properties
                 then pure i 
                 else findType (i + 1)
    
    memTypeIndex ← findType 0
    
    -- Allocate memory
    let allocInfo = zero 
          { allocationSize = memSize
          , memoryTypeIndex = memTypeIndex
          }
    
    memory ← allocateMemory device allocInfo Nothing
    
    -- Bind memory
    bindBufferMemory device buffer memory 0
    
    -- Map and upload
    dataPtr ← mapMemory device memory 0 bufferSize zero
    liftIO $ V.imapM_ (\i inst → pokeElemOff (castPtr dataPtr) i inst) instances
    unmapMemory device memory
    
    return (buffer, memory)
