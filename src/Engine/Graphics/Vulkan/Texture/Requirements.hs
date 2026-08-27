-- | The descriptor-indexing contract the bindless renderer runs under
--   (#1282, extended in #1689): exactly which Vulkan 1.2 features its
--   shaders and its descriptor-set layout require, and how many descriptors
--   that layout and its pool consume — written down once and independently
--   of any device.
--
--   This module exists for the same reason
--   "Engine.Graphics.Vulkan.Texture.Limits" does — several places have to
--   agree, and before #1282 they did not. The logical device enabled six
--   features from a hand-written literal
--   ("Engine.Graphics.Vulkan.Device"), the bindless layout set its binding
--   flags from a second literal
--   ("Engine.Graphics.Vulkan.Texture.Bindless"), and capability evaluation
--   ("Engine.Graphics.Vulkan.Capability") read neither. The layout therefore
--   asked for @DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT@ on a combined image
--   sampler while the device never enabled
--   @descriptorBindingSampledImageUpdateAfterBind@ — the one feature that
--   flag is valid under for that descriptor type. All three sites now read
--   the definitions here, so the same divergence cannot be written down
--   again.
--
--   Everything below is a pure value or a pure function of a reported
--   feature or properties struct: no 'Vulkan.Core10.PhysicalDevice', no
--   GPU, so a headless test can pin the whole contract.
module Engine.Graphics.Vulkan.Texture.Requirements
  ( -- * The required features
    BindlessFeature(..)
  , requiredBindlessFeatures
  , bindlessFeatureField
    -- * Reading and building feature structs
  , readBindlessFeature
  , enableBindlessFeature
  , requiredVulkan12Features
  , missingBindlessFeatures
    -- * The layout flags those features justify
  , bindlessTextureBindingRequirements
  , bindlessTextureBindingFlags
    -- * The descriptor-count rules that layout must satisfy
  , BindlessCapacity(..)
  , CapacityCheck(..)
  , capacityCheckHolds
  , bindlessCapacityRequirement
  , requiredBindlessCapacities
  , handleTableDescriptors
  , pipelineUniformBufferDescriptors
  , bindlessPipelineSetCount
  , bindlessColorAttachments
  , ordinaryCapacityField
  , updateAfterBindCapacityField
  , readOrdinaryLimit
  , readUpdateAfterBindLimit
  , bindlessCapacityApplies
  , bindlessCapacityCheck
  , reportedBindlessCapacities
  ) where

import UPrelude
import Engine.Graphics.Vulkan.Texture.Limits (maxBindlessTextures)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceLimits(..))
import Vulkan.Core12
  (PhysicalDeviceVulkan12Features(..), PhysicalDeviceVulkan12Properties(..))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits
  ( DescriptorBindingFlagBits(..)
  , DescriptorBindingFlags
  )
import Vulkan.Zero (zero)

-- | A Vulkan 1.2 descriptor-indexing feature this renderer genuinely uses.
--   The set is deliberately minimal: a feature belongs here only when a
--   shader construct or a layout flag the engine actually emits is invalid
--   without it. @runtimeDescriptorArray@,
--   @descriptorBindingUpdateUnusedWhilePending@ and
--   @descriptorBindingVariableDescriptorCount@ were requested before #1282
--   and are absent for that reason — the bindless arrays are fixed-size at
--   'Engine.Graphics.Vulkan.Texture.Limits.maxBindlessTextures' on every
--   accepted device, which 'requiredBindlessCapacities' below is what makes
--   true rather than assumed (#1689); the descriptors are never rewritten
--   while pending, and
--   @VARIABLE_DESCRIPTOR_COUNT@ is deliberately avoided for MoltenVK. The
--   aggregate @descriptorIndexing@ boolean is absent too: per
--   @VkPhysicalDeviceVulkan12Features@ it is roll-up metadata and does not
--   enable any of the fine-grained features below.
data BindlessFeature
  = -- | Required by @nonuniformEXT@ in both bindless fragment shaders
    --   ("Engine.Graphics.Vulkan.ShaderCode"), which index the combined
    --   image-sampler array with a per-fragment slot.
    FeatShaderSampledImageArrayNonUniformIndexing
  | -- | Required by @DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT@ on the texture
    --   binding: most of the fixed-size array is unwritten at any moment.
    FeatDescriptorBindingPartiallyBound
  | -- | Required by @DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT@ on a
    --   @DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ binding specifically —
    --   registering a texture rewrites one element of a set that is
    --   already bound.
    FeatDescriptorBindingSampledImageUpdateAfterBind
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every feature the bindless renderer requires. Derived from the type so
--   a constructor added above is required by construction rather than by
--   remembering to extend a list.
requiredBindlessFeatures ∷ [BindlessFeature]
requiredBindlessFeatures = [minBound .. maxBound]

-- | The @VkPhysicalDeviceVulkan12Features@ field name, for diagnostics that
--   have to name what a device is missing.
bindlessFeatureField ∷ BindlessFeature → Text
bindlessFeatureField = \case
  FeatShaderSampledImageArrayNonUniformIndexing →
    "shaderSampledImageArrayNonUniformIndexing"
  FeatDescriptorBindingPartiallyBound →
    "descriptorBindingPartiallyBound"
  FeatDescriptorBindingSampledImageUpdateAfterBind →
    "descriptorBindingSampledImageUpdateAfterBind"

-- | Whether a reported feature struct advertises one required feature.
readBindlessFeature ∷ PhysicalDeviceVulkan12Features → BindlessFeature → Bool
readBindlessFeature feats = \case
  FeatShaderSampledImageArrayNonUniformIndexing →
    shaderSampledImageArrayNonUniformIndexing feats
  FeatDescriptorBindingPartiallyBound →
    descriptorBindingPartiallyBound feats
  FeatDescriptorBindingSampledImageUpdateAfterBind →
    descriptorBindingSampledImageUpdateAfterBind feats

-- | Turn one required feature on in a feature struct.
enableBindlessFeature ∷ BindlessFeature
                      → PhysicalDeviceVulkan12Features
                      → PhysicalDeviceVulkan12Features
enableBindlessFeature f feats = case f of
  FeatShaderSampledImageArrayNonUniformIndexing →
    feats { shaderSampledImageArrayNonUniformIndexing = True }
  FeatDescriptorBindingPartiallyBound →
    feats { descriptorBindingPartiallyBound = True }
  FeatDescriptorBindingSampledImageUpdateAfterBind →
    feats { descriptorBindingSampledImageUpdateAfterBind = True }

-- | The feature struct to chain into @VkDeviceCreateInfo@: every required
--   feature enabled, everything else left at 'zero'. Enabling anything the
--   renderer does not use is a request the device may refuse with
--   @VK_ERROR_FEATURE_NOT_PRESENT@ for no benefit, which is exactly the
--   incidental failure #1282 removed.
requiredVulkan12Features ∷ PhysicalDeviceVulkan12Features
requiredVulkan12Features =
  foldr enableBindlessFeature zero requiredBindlessFeatures

-- | The required features a device does NOT advertise, in declaration
--   order. Empty means the device can run the bindless renderer as far as
--   features go (the version and limit gates are separate — see
--   "Engine.Graphics.Vulkan.Capability").
missingBindlessFeatures ∷ PhysicalDeviceVulkan12Features → [BindlessFeature]
missingBindlessFeatures feats =
  filter (not . readBindlessFeature feats) requiredBindlessFeatures

-- | Each descriptor-binding flag the bindless texture binding carries,
--   paired with the feature that flag is only valid under for a
--   @DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ descriptor. This is the join
--   that keeps layout flags and enabled features from drifting apart: a
--   flag added here without its feature in 'BindlessFeature' does not
--   compile, and one added to the layout without coming through here is
--   caught by the headless test that compares the two.
bindlessTextureBindingRequirements
  ∷ [(DescriptorBindingFlagBits, BindlessFeature)]
bindlessTextureBindingRequirements =
  [ ( DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
    , FeatDescriptorBindingPartiallyBound )
  , ( DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT
    , FeatDescriptorBindingSampledImageUpdateAfterBind )
  ]

-- | The @bindingFlags@ value for the bindless texture binding, folded from
--   'bindlessTextureBindingRequirements' so it cannot name a flag the
--   feature set does not cover. The handle→slot table (binding 1) is
--   deliberately absent: it is a plain storage buffer written once before
--   the set is ever bound, so it needs no flag and no extra feature.
bindlessTextureBindingFlags ∷ DescriptorBindingFlags
bindlessTextureBindingFlags =
  foldr ((⌄) . fst) zero bindlessTextureBindingRequirements
--   handle→slot table (#286, binding 1).
handleTableDescriptors ∷ Word32
handleTableDescriptors = 1

-- | Uniform-buffer descriptors set 0 contributes to the bindless pipeline
--   layout: one vertex-stage uniform buffer
--   ("Engine.Graphics.Vulkan.Descriptor"'s @createUniformDescriptorSetLayout@,
--   paired with the bindless texture layout in
--   "Engine.Graphics.Vulkan.Pipeline.Bindless").
pipelineUniformBufferDescriptors ∷ Word32
pipelineUniformBufferDescriptors = 1

-- | Descriptor sets in the bindless pipeline layout: set 0's uniform layout
--   and set 1's bindless texture layout.
bindlessPipelineSetCount ∷ Word32
bindlessPipelineSetCount = 2

-- | Framebuffer colour attachments each bindless pipeline writes — one, from
--   the single colour-blend attachment
--   'Engine.Graphics.Vulkan.Pipeline.Bindless.createBindlessPipelineWithShader'
--   configures (both the world and UI pipelines go through it).
--   @maxPerStageResources@'s text ends "For the fragment shader stage the
--   framebuffer color attachments also count against this limit", and the
--   update-after-bind form is that same limit re-scoped.
bindlessColorAttachments ∷ Word32
bindlessColorAttachments = 1

-- | One descriptor-count rule the bindless pipeline layout must satisfy: the
--   limit it is measured against, how many of this renderer's descriptors
--   count, and the EFFECTIVE capacity the device supplies for it.
data CapacityCheck = CapacityCheck
  { ccField    ∷ Text    -- ^ the limit(s) this rule is measured against
  , ccRequired ∷ Word32  -- ^ how many of ours the rule counts
  , ccReported ∷ Word32  -- ^ the effective capacity the device supplies
  } deriving (Show, Eq)

-- | Whether one rule is satisfied.
capacityCheckHolds ∷ CapacityCheck → Bool
capacityCheckHolds check = ccReported check ≥ ccRequired check

-- | A descriptor class the bindless pipeline layout puts descriptors into,
--   at one granularity. Most name a PAIR of limits — an ordinary
--   'PhysicalDeviceLimits' one and its update-after-bind counterpart on
--   'PhysicalDeviceVulkan12Properties' — whose EFFECTIVE capacity is the
--   greater of the two ('bindlessCapacityChecks').
data BindlessCapacity
  = -- | The texture array, per fragment stage.
    CapPerStageSampledImages
  | -- | The same descriptors as SAMPLERS: a combined image sampler is both.
    CapPerStageSamplers
  | -- | The handle→slot table, per fragment stage.
    CapPerStageStorageBuffers
  | -- | Set 0's uniform buffer, per vertex stage.
    CapPerStageUniformBuffers
  | -- | The busiest stage's aggregate: the fragment stage's array,
    --   handle→slot table and colour attachment. Plain @SAMPLER@ descriptors
    --   are excluded from this aggregate, so the array counts once.
    CapPerStageResources
  | -- | The texture array across the whole pipeline layout.
    CapSetSampledImages
  | -- | The same, as samplers.
    CapSetSamplers
  | -- | The handle→slot table across the whole pipeline layout.
    CapSetStorageBuffers
  | -- | Set 0's uniform buffer across the whole pipeline layout.
    CapSetUniformBuffers
  | -- | @maxBoundDescriptorSets@ (VUID 00286). No update-after-bind
    --   counterpart exists, so its own value is the capacity.
    CapBoundDescriptorSets
  | -- | @maxUpdateAfterBindDescriptorsInAllPools@ — update-after-bind only:
    --   it governs creating the pool at all, and exceeding it fails pool
    --   creation with @VK_ERROR_FRAGMENTATION@.
    CapDescriptorsInAllPools
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every descriptor class the renderer's layout is measured on. Derived
--   from the type so a constructor added above is required by construction.
requiredBindlessCapacities ∷ [BindlessCapacity]
requiredBindlessCapacities = [minBound .. maxBound]

-- | How many descriptors of one class the bindless pipeline layout puts
--   against its rule. Every texture figure is 'maxBindlessTextures' itself —
--   #975's single definition, the one both fragment shaders interpolate — so
--   the requirement and the shader array size cannot drift apart.
bindlessCapacityRequirement ∷ BindlessCapacity → Word32
bindlessCapacityRequirement = \case
  CapPerStageSampledImages  → maxBindlessTextures
  CapPerStageSamplers       → maxBindlessTextures
  CapPerStageStorageBuffers → handleTableDescriptors
  CapPerStageUniformBuffers → pipelineUniformBufferDescriptors
  CapPerStageResources      →
    maxBindlessTextures + handleTableDescriptors + bindlessColorAttachments
  CapSetSampledImages       → maxBindlessTextures
  CapSetSamplers            → maxBindlessTextures
  CapSetStorageBuffers      → handleTableDescriptors
  CapSetUniformBuffers      → pipelineUniformBufferDescriptors
  CapBoundDescriptorSets    → bindlessPipelineSetCount
  CapDescriptorsInAllPools  → maxBindlessTextures + handleTableDescriptors

-- | The @VkPhysicalDeviceLimits@ field carrying one class's ORDINARY limit,
--   and 'Nothing' where the class has no ordinary counterpart.
ordinaryCapacityField ∷ BindlessCapacity → Maybe Text
ordinaryCapacityField = \case
  CapPerStageSampledImages  → Just "maxPerStageDescriptorSampledImages"
  CapPerStageSamplers       → Just "maxPerStageDescriptorSamplers"
  CapPerStageStorageBuffers → Just "maxPerStageDescriptorStorageBuffers"
  CapPerStageUniformBuffers → Just "maxPerStageDescriptorUniformBuffers"
  CapPerStageResources      → Just "maxPerStageResources"
  CapSetSampledImages       → Just "maxDescriptorSetSampledImages"
  CapSetSamplers            → Just "maxDescriptorSetSamplers"
  CapSetStorageBuffers      → Just "maxDescriptorSetStorageBuffers"
  CapSetUniformBuffers      → Just "maxDescriptorSetUniformBuffers"
  CapBoundDescriptorSets    → Just "maxBoundDescriptorSets"
  CapDescriptorsInAllPools  → Nothing

-- | The @VkPhysicalDeviceVulkan12Properties@ field carrying one class's
--   UPDATE-AFTER-BIND limit, and 'Nothing' where there is none.
updateAfterBindCapacityField ∷ BindlessCapacity → Maybe Text
updateAfterBindCapacityField = \case
  CapPerStageSampledImages  →
    Just "maxPerStageDescriptorUpdateAfterBindSampledImages"
  CapPerStageSamplers       →
    Just "maxPerStageDescriptorUpdateAfterBindSamplers"
  CapPerStageStorageBuffers →
    Just "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
  CapPerStageUniformBuffers →
    Just "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
  CapPerStageResources      → Just "maxPerStageUpdateAfterBindResources"
  CapSetSampledImages       → Just "maxDescriptorSetUpdateAfterBindSampledImages"
  CapSetSamplers            → Just "maxDescriptorSetUpdateAfterBindSamplers"
  CapSetStorageBuffers      → Just "maxDescriptorSetUpdateAfterBindStorageBuffers"
  CapSetUniformBuffers      → Just "maxDescriptorSetUpdateAfterBindUniformBuffers"
  CapBoundDescriptorSets    → Nothing
  CapDescriptorsInAllPools  →
    Just "maxUpdateAfterBindDescriptorsInAllPools"

-- | What a device reports for one ordinary limit.
readOrdinaryLimit ∷ PhysicalDeviceLimits → BindlessCapacity → Word32
readOrdinaryLimit base = \case
  CapPerStageSampledImages  → maxPerStageDescriptorSampledImages base
  CapPerStageSamplers       → maxPerStageDescriptorSamplers base
  CapPerStageStorageBuffers → maxPerStageDescriptorStorageBuffers base
  CapPerStageUniformBuffers → maxPerStageDescriptorUniformBuffers base
  CapPerStageResources      → maxPerStageResources base
  CapSetSampledImages       → maxDescriptorSetSampledImages base
  CapSetSamplers            → maxDescriptorSetSamplers base
  CapSetStorageBuffers      → maxDescriptorSetStorageBuffers base
  CapSetUniformBuffers      → maxDescriptorSetUniformBuffers base
  CapBoundDescriptorSets    → maxBoundDescriptorSets base
  CapDescriptorsInAllPools  → 0

-- | What a device reports for one update-after-bind limit.
readUpdateAfterBindLimit
  ∷ PhysicalDeviceVulkan12Properties → BindlessCapacity → Word32
readUpdateAfterBindLimit props = \case
  CapPerStageSampledImages  →
    maxPerStageDescriptorUpdateAfterBindSampledImages props
  CapPerStageSamplers       → maxPerStageDescriptorUpdateAfterBindSamplers props
  CapPerStageStorageBuffers →
    maxPerStageDescriptorUpdateAfterBindStorageBuffers props
  CapPerStageUniformBuffers →
    maxPerStageDescriptorUpdateAfterBindUniformBuffers props
  CapPerStageResources      → maxPerStageUpdateAfterBindResources props
  CapSetSampledImages       → maxDescriptorSetUpdateAfterBindSampledImages props
  CapSetSamplers            → maxDescriptorSetUpdateAfterBindSamplers props
  CapSetStorageBuffers      → maxDescriptorSetUpdateAfterBindStorageBuffers props
  CapSetUniformBuffers      → maxDescriptorSetUpdateAfterBindUniformBuffers props
  CapBoundDescriptorSets    → 0
  CapDescriptorsInAllPools  → maxUpdateAfterBindDescriptorsInAllPools props

-- | Whether one class's UPDATE-AFTER-BIND limit participates on a device.
--
--   Each update-after-bind pipeline-layout statement is conditioned on the
--   device SUPPORTING that descriptor type's own
--   @descriptorBinding…UpdateAfterBind@ feature — support, not enablement,
--   which is what @vkGetPhysicalDeviceFeatures2@ reports. Where the
--   statement is inactive its limit contributes nothing to the effective
--   capacity, leaving the ordinary limit to supply it. The aggregate and the
--   pool ceiling are unconditional: the first governs any pipeline layout
--   holding an update-after-bind set, the second creating that pool at all.
bindlessCapacityApplies
  ∷ PhysicalDeviceVulkan12Features → BindlessCapacity → Bool
bindlessCapacityApplies feats = \case
  CapPerStageSampledImages  → sampledImages
  CapPerStageSamplers       → sampledImages
  CapSetSampledImages       → sampledImages
  CapSetSamplers            → sampledImages
  CapPerStageStorageBuffers → storageBuffers
  CapSetStorageBuffers      → storageBuffers
  CapPerStageUniformBuffers → uniformBuffers
  CapSetUniformBuffers      → uniformBuffers
  CapPerStageResources      → True
  CapDescriptorsInAllPools  → True
  CapBoundDescriptorSets    → True
  where
    sampledImages  = descriptorBindingSampledImageUpdateAfterBind feats
    storageBuffers = descriptorBindingStorageBufferUpdateAfterBind feats
    uniformBuffers = descriptorBindingUniformBufferUpdateAfterBind feats

-- | The rule the device must satisfy for one descriptor class.
--
--   The capacity is the EFFECTIVE limit: where a class has both an ordinary
--   limit and an update-after-bind counterpart, the greater of the two, so a
--   device whose ordinary limit already supplies the count is accepted
--   rather than refused on the update-after-bind figure alone. Where only
--   one of the pair exists — or where the update-after-bind statement is
--   inactive for want of its feature — that single value is the capacity.
bindlessCapacityCheck
  ∷ PhysicalDeviceVulkan12Features
  → PhysicalDeviceLimits
  → PhysicalDeviceVulkan12Properties
  → BindlessCapacity
  → CapacityCheck
bindlessCapacityCheck feats base props cap = CapacityCheck
  { ccField    = field
  , ccRequired = bindlessCapacityRequirement cap
  , ccReported = capacity
  }
  where
    ordinary       = readOrdinaryLimit base cap <$ ordinaryCapacityField cap
    updateAfterBind
      | bindlessCapacityApplies feats cap =
          readUpdateAfterBindLimit props cap <$ updateAfterBindCapacityField cap
      | otherwise = Nothing
    capacity = maximum (0 : catMaybes [ordinary, updateAfterBind])
    field = case (ordinaryCapacityField cap, updateAfterBind) of
      (Just a, Just _) → a <> " / "
        <> fromMaybe "" (updateAfterBindCapacityField cap)
        <> " (effective maximum)"
      (Just a, Nothing) → a
      (Nothing, _)      → fromMaybe "" (updateAfterBindCapacityField cap)

-- | Every rule the device must satisfy, in declaration order.
reportedBindlessCapacities
  ∷ PhysicalDeviceVulkan12Features
  → PhysicalDeviceLimits
  → PhysicalDeviceVulkan12Properties
  → [CapacityCheck]
reportedBindlessCapacities feats base props =
  map (bindlessCapacityCheck feats base props) requiredBindlessCapacities
