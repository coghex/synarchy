{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
module Engine.Graphics.Vulkan.Types where
import UPrelude
import qualified Data.Vector as V
import Vulkan.Core10
import Engine.Graphics.Vulkan.Uniform.Layout (declareUniformBufferObject)

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
    , frInFlight        ∷ Fence
    } deriving (Show)

maxTimeout ∷ Word64
maxTimeout = maxBound

-- | Cleanup progress tracked during shutdown
data CleanupStatus = NotStarted | InProgress | Completed
  deriving (Show, Eq)

-- | @data UniformBufferObject = UBO {…}@ plus its 'Storable' instance,
--   generated from the ONE layout definition in
--   "Engine.Graphics.Vulkan.Uniform.Layout" — which is also what the GLSL
--   @UniformBufferObject@ uniform block every shader declares is derived
--   from, so the two cannot drift (#1072).
--
--   Read that module for the member list, what each member is for, and the
--   std140 offsets; there is deliberately no copy of any of it here. The
--   field names and the positional @UBO@ constructor are unchanged, so
--   every existing call site still builds.
$(declareUniformBufferObject)
