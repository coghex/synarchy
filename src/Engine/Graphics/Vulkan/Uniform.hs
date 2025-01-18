{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Uniform
    ( createUniformBuffers
    , updateUniformBuffers
    , UniformBuffers(..)
    ) where

import UPrelude
import qualified Data.Vector as V
import Foreign.Storable (sizeOf, poke)
import Foreign.Ptr (castPtr)
import Linear (M44, V3(..), identity, (!*!))
import Engine.Core.Monad
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Buffer
import Engine.Scene.Types (Camera2D(..))
import Engine.Graphics.Camera (createViewMatrix, createProjectionMatrix)
import Vulkan.Core10
import Vulkan.Zero

data UniformBuffers = UniformBuffers
    { ubBuffers ∷ V.Vector Buffer
    , ubMemory  ∷ V.Vector DeviceMemory
    }

-- | Create uniform buffers for each frame in flight
createUniformBuffers ∷ Device → PhysicalDevice → Int → EngineM ε σ UniformBuffers
createUniformBuffers device pDevice numFrames = do
    let uboSize = fromIntegral $ sizeOf (undefined ∷ UniformBufferObject)
    buffers ← V.replicateM numFrames $ do
        (buffer, memory) ← createUniformBuffer device pDevice uboSize
        pure (buffer, memory)
    
    let (bufs, mems) = V.unzip buffers
    pure $ UniformBuffers
        { ubBuffers = bufs
        , ubMemory = mems
        }

-- | Update uniform buffer for current frame
updateUniformBuffers ∷ Device → UniformBuffers → Int → Camera2D → Float → Float
  → EngineM ε σ ()
updateUniformBuffers device uniforms frameIndex camera width height = do
    let memory = ubMemory uniforms V.! frameIndex
        uboSize = fromIntegral $ sizeOf (undefined ∷ UniformBufferObject)
        
        -- Create matrices
        modelMatrix = identity -- For 2D sprites, usually identity unless transformed
        viewMatrix = createViewMatrix camera
        projMatrix = createProjectionMatrix camera width height
        
        -- Create UBO data
        uboData = UBO
            { uboModel = modelMatrix
            , uboView  = viewMatrix
            , uboProj  = projMatrix
            }
    
    -- Map memory and update
    dataPtr ← mapMemory device memory 0 uboSize zero
    liftIO $ poke (castPtr dataPtr) uboData
    unmapMemory device memory
