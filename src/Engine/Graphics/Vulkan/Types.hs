-- Engine/Graphics/Vulkan/Types.hs
{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Types where
import UPrelude
import qualified Data.Vector as V
import Linear (V4(..), M44, identity)
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features

data VulkanExtensions = VulkanExtensions
  { veRequired ∷ V.Vector String  -- ^ Required extensions
  , veOptional ∷ V.Vector String  -- ^ Optional extensions
  }

data VulkanLayers = VulkanLayers
  { vlRequired ∷ V.Vector String  -- ^ Required layers
  , vlOptional ∷ V.Vector String  -- ^ Optional layers
  }

data SyncObjects = SyncObjects
  { imageAvailableSemaphores ∷ V.Vector Semaphore
  , renderFinishedSemaphores ∷ V.Vector Semaphore
  , inFlightFences           ∷ V.Vector Fence
  } deriving (Show)

data VulkanDescriptorInfo = VulkanDescriptorInfo
  { vdiLayout  ∷ DescriptorSetLayout
  , vdiPool    ∷ DescriptorPool
  , vdiSets    ∷ V.Vector DescriptorSet
  }

-- | Configuration for descriptor manager
data DescriptorManagerConfig = DescriptorManagerConfig
  { dmcMaxSets        ∷ Word32        -- ^ Maximum number of descriptor sets
  , dmcUniformCount   ∷ Word32        -- ^ Number of uniform buffer descriptors
  , dmcSamplerCount   ∷ Word32        -- ^ Number of combined image sampler descriptors
  } deriving (Show, Eq)

-- | Resources specific to a single frame in flight
data FrameResources = FrameResources
    { frCommandPool     ∷ CommandPool
    , frCommandBuffer   ∷ V.Vector CommandBuffer
    , frImageAvailable  ∷ Semaphore
    , frRenderFinished  ∷ Semaphore
    , frInFlight        ∷ Fence
    } deriving (Show)

maxTimeout ∷ Word64
maxTimeout = maxBound

-- | blocks on cleanup
data CleanupStatus = NotStarted | InProgress | Completed
  deriving (Show, Eq)

-- | Uniform buffer object matching shader layout
-- Layout (std140-ish, matching GLSL):
--   mat4 model       (offset   0, 64 bytes)
--   mat4 view        (offset  64, 64 bytes)
--   mat4 proj        (offset 128, 64 bytes)
--   mat4 uiView      (offset 192, 64 bytes)
--   mat4 uiProj      (offset 256, 64 bytes)
--   float brightness  (offset 320)
--   float screenW     (offset 324)
--   float screenH     (offset 328)
--   float pixelSnap   (offset 332)
--   float sunAngle    (offset 336)
--   float ambientLight(offset 340)
--   float cameraFacing (offset 344)
--   <padding>         (offset 348-351, 4 bytes)
-- Total: 5*64 + 32 = 352 bytes
data UniformBufferObject = UBO
    { uboModel        ∷ M44 Float  -- model matrix
    , uboView         ∷ M44 Float  -- view matrix
    , uboProj         ∷ M44 Float  -- projection matrix
    , uboUIView       ∷ M44 Float  -- UI view matrix
    , uboUIProj       ∷ M44 Float  -- UI projection matrix
    , uboBrightness   ∷ Float      -- brightness factor
    , uboScreenW      ∷ Float      -- screen width
    , uboScreenH      ∷ Float      -- screen height
    , uboPixelSnap    ∷ Float      -- pixel snapping factor
    , uboSunAngle     ∷ Float      -- day/night cycle angle (0..1)
    , uboAmbientLight ∷ Float      -- minimum ambient brightness
    , uboCameraFacing ∷ Float      -- 0.0 = south, 1.0 = west, 2.0 = north, 3.0 = east
    } deriving (Show)

instance Storable UniformBufferObject where
    -- 5 matrices (64 bytes each) + 6 floats (24 bytes) + 8 bytes padding = 352
    sizeOf _ = 5 * sizeOf (undefined ∷ M44 Float) + 32
    alignment _ = 16  -- Vulkan requires 16-byte alignment for uniform buffers
    peek ptr = UBO
        ⊚ peek (castPtr ptr)
        <*> peek (castPtr $ ptr `plusPtr` sizeOf (undefined ∷ M44 Float))
        <*> peek (castPtr $ ptr `plusPtr` (2 * sizeOf (undefined ∷ M44 Float)))
        <*> peek (castPtr $ ptr `plusPtr` (3 * sizeOf (undefined ∷ M44 Float)))
        <*> peek (castPtr $ ptr `plusPtr` (4 * sizeOf (undefined ∷ M44 Float)))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float)))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float) + 4))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float) + 8))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float) + 12))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float) + 16))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float) + 20))
        <*> peek (castPtr $ ptr `plusPtr` (5 * sizeOf (undefined ∷ M44 Float) + 24))
    poke ptr (UBO model view proj uiView uiProj brightness screenW screenH pixelSnap sunAngle ambientLight facing) = do
        poke (castPtr ptr) model
        poke (castPtr $ ptr `plusPtr` sizeOf model) view
        poke (castPtr $ ptr `plusPtr` (2 * sizeOf model)) proj
        poke (castPtr $ ptr `plusPtr` (3 * sizeOf model)) uiView
        poke (castPtr $ ptr `plusPtr` (4 * sizeOf model)) uiProj
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model)) brightness
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model + 4)) screenW
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model + 8)) screenH
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model + 12)) pixelSnap
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model + 16)) sunAngle
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model + 20)) ambientLight
        poke (castPtr $ ptr `plusPtr` (5 * sizeOf model + 24)) facing
