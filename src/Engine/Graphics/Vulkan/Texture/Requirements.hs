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
    -- * The descriptor capacities that layout consumes
  , BindlessCapacity(..)
  , requiredBindlessCapacities
  , handleTableDescriptors
  , pipelineUniformBufferDescriptors
  , bindlessPipelineSetCount
  , bindlessCapacityField
  , bindlessCapacityRequirement
  , bindlessCapacityApplies
  , readBindlessCapacity
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

-- | Descriptors the bindless set's storage-buffer binding consumes: the one
--   handle→slot table (#286, binding 1).
handleTableDescriptors ∷ Word32
handleTableDescriptors = 1

-- | Uniform-buffer descriptors the OTHER set in the bindless pipeline
--   layout consumes: set 0's single vertex-stage uniform buffer
--   ("Engine.Graphics.Vulkan.Descriptor"'s @createUniformDescriptorSetLayout@,
--   paired with the bindless texture layout in
--   "Engine.Graphics.Vulkan.Pipeline.Bindless"). Set 0 is an ordinary
--   layout, but the update-after-bind limits are scoped to the PIPELINE
--   LAYOUT: once any set in it carries
--   @UPDATE_AFTER_BIND_POOL_BIT@, every set's descriptors are measured
--   against the update-after-bind limits, so this one counts.
pipelineUniformBufferDescriptors ∷ Word32
pipelineUniformBufferDescriptors = 1

-- | Descriptor sets in the bindless pipeline layout: set 0's uniform layout
--   and set 1's bindless texture layout
--   ("Engine.Graphics.Vulkan.Pipeline.Bindless").
bindlessPipelineSetCount ∷ Word32
bindlessPipelineSetCount = 2

-- | One update-after-bind descriptor capacity the concrete bindless
--   descriptor set consumes, named as @VkPhysicalDeviceVulkan12Properties@
--   reports it.
--
--   The renderer's texture array is FIXED-SIZE: both bindless fragment
--   shaders declare @textures[maxBindlessTextures]@ and index it with
--   @nonuniformEXT@, so the array is statically used at its declared size,
--   and the Vulkan descriptor-set interface rule is that the binding must
--   hold at least that many descriptors. Without @runtimeDescriptorArray@
--   (deliberately not required — see 'BindlessFeature') there is no
--   exception to that rule, so a device is only usable when it can supply
--   the WHOLE binding. That makes each entry below a hard acceptance gate
--   rather than a figure to clamp the allocation down to (#1689).
--
--   The gate cannot be the sampled-image limit alone. The set holds two
--   bindings — a combined-image-sampler array and the handle→slot storage
--   buffer — and a @DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ descriptor
--   counts against the SAMPLER limits as well as the sampled-image ones.
--
--   The scope is the whole PIPELINE LAYOUT, not this set. Every
--   update-after-bind Valid Usage statement on @VkPipelineLayoutCreateInfo@
--   (03022-03027, 03036-03043) counts descriptors \"across all elements of
--   @pSetLayouts@\" — sets created WITHOUT
--   @UPDATE_AFTER_BIND_POOL_BIT@ included — so set 0's vertex-stage uniform
--   buffer counts too ('pipelineUniformBufferDescriptors'). There is no
--   maximum taken with the ordinary limit: the ordinary statements
--   (03016-03018, 03028-03029) are a SEPARATE, simultaneous constraint that
--   counts only the non-update-after-bind set layouts. Those cannot fail
--   here — set 0 contributes one uniform buffer against Vulkan's guaranteed
--   minima (@maxPerStageDescriptorUniformBuffers@ ≥ 12,
--   @maxDescriptorSetUniformBuffers@ ≥ 72), and the bindless set is
--   excluded from them entirely — which is why only the
--   update-after-bind side is gated on below.
--
--   The ordinary statements ARE gated on below, as the @Cap…Base…@ and
--   'CapBoundDescriptorSets' entries — but only where they count something.
--   Set 0's single vertex-stage uniform buffer is the layout's ONLY
--   non-update-after-bind descriptor, so the ordinary sampler
--   (03016/03028), sampled-image and storage-buffer (03018) statements
--   count zero here and have no entry; @maxPerStageResources@ is
--   non-update-after-bind-only in the same way (its own limit text says so),
--   leaving that one uniform buffer plus the fragment stage's colour
--   attachments against a guaranteed minimum of 128.
--
--   Each update-after-bind statement is nevertheless CONDITIONAL on the
--   device SUPPORTING the matching @descriptorBinding…UpdateAfterBind@
--   feature
--   ('bindlessCapacityApplies'). This renderer requires only the
--   sampled-image one, so the uniform- and storage-buffer capacities bind
--   only on a device that advertises their features; requiring them
--   unconditionally would refuse devices whose pipeline layout Vulkan
--   accepts.
--
--   @maxDescriptorSet…@ is a pipeline-layout total across all stages;
--   @maxPerStage…@ and @maxPerStageUpdateAfterBindResources@ are per SHADER
--   STAGE, so the vertex-stage uniform buffer and the fragment-stage
--   bindings never share a per-stage budget.
data BindlessCapacity
  = -- | @maxPerStageDescriptorUpdateAfterBindSampledImages@ — the texture
    --   array, in the fragment stage that declares it.
    CapPerStageSampledImages
  | -- | @maxPerStageDescriptorUpdateAfterBindSamplers@ — the same
    --   descriptors again: a combined image sampler is a sampler too.
    CapPerStageSamplers
  | -- | @maxPerStageDescriptorUpdateAfterBindStorageBuffers@ — the
    --   handle→slot table.
    CapPerStageStorageBuffers
  | -- | @maxPerStageDescriptorUpdateAfterBindUniformBuffers@ — set 0's
    --   uniform buffer, in the vertex stage that declares it.
    CapPerStageUniformBuffers
  | -- | @maxPerStageUpdateAfterBindResources@ — the fragment stage's two
    --   bindings together, the busiest stage in the layout. This is the
    --   aggregate @VkGraphicsPipelineCreateInfo@'s @maxPerStageResources@
    --   rule uses once a set in the layout is update-after-bind. Samplers
    --   are excluded from it (as they are from @maxPerStageResources@), so
    --   the array counts once; the uniform buffer is a different stage and
    --   does not add to it.
    CapPerStageResources
  | -- | @maxDescriptorSetUpdateAfterBindSampledImages@ — the texture array
    --   within its own descriptor set.
    CapSetSampledImages
  | -- | @maxDescriptorSetUpdateAfterBindSamplers@ — the same, as samplers.
    CapSetSamplers
  | -- | @maxDescriptorSetUpdateAfterBindStorageBuffers@ — the handle→slot
    --   table within the pipeline layout.
    CapSetStorageBuffers
  | -- | @maxDescriptorSetUpdateAfterBindUniformBuffers@ — set 0's uniform
    --   buffer within that same pipeline-layout total.
    CapSetUniformBuffers
  | -- | @maxUpdateAfterBindDescriptorsInAllPools@ — the descriptor pool
    --   this set is allocated from is the renderer's only
    --   @UPDATE_AFTER_BIND@ pool, so both bindings' descriptors sit inside
    --   this one ceiling. Exceeding it fails pool creation with
    --   @VK_ERROR_FRAGMENTATION@; it is a property of creating such a pool
    --   at all, so no feature conditions it.
    CapDescriptorsInAllPools
  | -- | @maxBoundDescriptorSets@ — the pipeline layout binds exactly
    --   'bindlessPipelineSetCount' sets
    --   (VUID-VkPipelineLayoutCreateInfo-setLayoutCount-00286).
    CapBoundDescriptorSets
  | -- | @maxPerStageDescriptorUniformBuffers@ — the ORDINARY per-stage
    --   statement (03017), which counts only set layouts created WITHOUT
    --   @UPDATE_AFTER_BIND_POOL_BIT@: here, set 0's vertex-stage uniform
    --   buffer alone.
    CapBasePerStageUniformBuffers
  | -- | @maxDescriptorSetUniformBuffers@ — the same descriptor under the
    --   ordinary all-stages statement (03029).
    CapBaseSetUniformBuffers
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every capacity a device must supply before the bindless renderer will
--   accept it. Derived from the type for the same reason
--   'requiredBindlessFeatures' is: a constructor added above is required by
--   construction.
requiredBindlessCapacities ∷ [BindlessCapacity]
requiredBindlessCapacities = [minBound .. maxBound]

-- | The @VkPhysicalDeviceVulkan12Properties@ field name, for diagnostics
--   that have to name which limit a device came up short on.
bindlessCapacityField ∷ BindlessCapacity → Text
bindlessCapacityField = \case
  CapPerStageSampledImages →
    "maxPerStageDescriptorUpdateAfterBindSampledImages"
  CapPerStageSamplers →
    "maxPerStageDescriptorUpdateAfterBindSamplers"
  CapPerStageStorageBuffers →
    "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
  CapPerStageUniformBuffers →
    "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
  CapPerStageResources →
    "maxPerStageUpdateAfterBindResources"
  CapSetSampledImages →
    "maxDescriptorSetUpdateAfterBindSampledImages"
  CapSetSamplers →
    "maxDescriptorSetUpdateAfterBindSamplers"
  CapSetStorageBuffers →
    "maxDescriptorSetUpdateAfterBindStorageBuffers"
  CapSetUniformBuffers →
    "maxDescriptorSetUpdateAfterBindUniformBuffers"
  CapDescriptorsInAllPools →
    "maxUpdateAfterBindDescriptorsInAllPools"
  CapBoundDescriptorSets →
    "maxBoundDescriptorSets"
  CapBasePerStageUniformBuffers →
    "maxPerStageDescriptorUniformBuffers"
  CapBaseSetUniformBuffers →
    "maxDescriptorSetUniformBuffers"

-- | How many descriptors the bindless set needs from one capacity. Every
--   texture figure is 'maxBindlessTextures' itself — #975's single
--   definition, the same one both fragment shaders interpolate — so the
--   requirement and the shader array size cannot drift apart.
bindlessCapacityRequirement ∷ BindlessCapacity → Word32
bindlessCapacityRequirement = \case
  CapPerStageSampledImages  → maxBindlessTextures
  CapPerStageSamplers       → maxBindlessTextures
  CapPerStageStorageBuffers → handleTableDescriptors
  CapPerStageUniformBuffers → pipelineUniformBufferDescriptors
  CapPerStageResources      → maxBindlessTextures + handleTableDescriptors
  CapSetSampledImages       → maxBindlessTextures
  CapSetSamplers            → maxBindlessTextures
  CapSetStorageBuffers      → handleTableDescriptors
  CapSetUniformBuffers      → pipelineUniformBufferDescriptors
  CapDescriptorsInAllPools  → maxBindlessTextures + handleTableDescriptors
  CapBoundDescriptorSets        → bindlessPipelineSetCount
  CapBasePerStageUniformBuffers → pipelineUniformBufferDescriptors
  CapBaseSetUniformBuffers      → pipelineUniformBufferDescriptors

-- | Whether one capacity's Valid Usage statement applies to a device at
--   all. Each update-after-bind pipeline-layout limit binds only when the
--   device SUPPORTS the @descriptorBinding…UpdateAfterBind@ feature for that
--   descriptor type — support, not enablement, which is what
--   @vkGetPhysicalDeviceFeatures2@ reports. A capacity that does not apply
--   must not gate acceptance: its limit constrains nothing the renderer
--   builds, so refusing on it would reject a device whose pipeline layout
--   Vulkan accepts.
--
--   'CapPerStageResources' and 'CapDescriptorsInAllPools' are
--   unconditional: the first is the aggregate that governs any pipeline
--   layout containing an update-after-bind set, and the second governs
--   creating the update-after-bind pool itself. Both always describe what
--   this renderer builds.
bindlessCapacityApplies
  ∷ PhysicalDeviceVulkan12Features → BindlessCapacity → Bool
bindlessCapacityApplies feats = \case
  CapPerStageSampledImages  → sampledImages
  CapPerStageSamplers       → sampledImages
  CapPerStageStorageBuffers → storageBuffers
  CapPerStageUniformBuffers → uniformBuffers
  CapPerStageResources      → True
  CapSetSampledImages       → sampledImages
  CapSetSamplers            → sampledImages
  CapSetStorageBuffers      → storageBuffers
  CapSetUniformBuffers      → uniformBuffers
  CapDescriptorsInAllPools  → True
  -- The ordinary statements are unconditional: they hold on every device,
  -- for the descriptors in the layout's non-update-after-bind set.
  CapBoundDescriptorSets        → True
  CapBasePerStageUniformBuffers → True
  CapBaseSetUniformBuffers      → True
  where
    -- The one this renderer REQUIRES, so these four always apply on a
    -- device it accepts at all (VUIDs 03022, 03025, 03036, 03041).
    sampledImages  = descriptorBindingSampledImageUpdateAfterBind feats
    -- Not required (#1282): the handle→slot buffer carries no
    -- update-after-bind binding flag (VUIDs 03024, 03039).
    storageBuffers = descriptorBindingStorageBufferUpdateAfterBind feats
    -- Not required: set 0 is an ordinary layout (VUIDs 03023, 03037).
    uniformBuffers = descriptorBindingUniformBufferUpdateAfterBind feats

-- | What a device's reported limits advertise for one capacity. Takes both
--   structs because the two families are genuinely different limits, not
--   two spellings of one: 'PhysicalDeviceLimits' carries the ordinary
--   statements' limits and 'PhysicalDeviceVulkan12Properties' the
--   update-after-bind ones. They are never combined — see 'BindlessCapacity'
--   for which descriptors each counts.
readBindlessCapacity ∷ PhysicalDeviceLimits
                     → PhysicalDeviceVulkan12Properties
                     → BindlessCapacity → Word32
readBindlessCapacity base props = \case
  CapPerStageSampledImages →
    maxPerStageDescriptorUpdateAfterBindSampledImages props
  CapPerStageSamplers →
    maxPerStageDescriptorUpdateAfterBindSamplers props
  CapPerStageStorageBuffers →
    maxPerStageDescriptorUpdateAfterBindStorageBuffers props
  CapPerStageUniformBuffers →
    maxPerStageDescriptorUpdateAfterBindUniformBuffers props
  CapPerStageResources →
    maxPerStageUpdateAfterBindResources props
  CapSetSampledImages →
    maxDescriptorSetUpdateAfterBindSampledImages props
  CapSetSamplers →
    maxDescriptorSetUpdateAfterBindSamplers props
  CapSetStorageBuffers →
    maxDescriptorSetUpdateAfterBindStorageBuffers props
  CapSetUniformBuffers →
    maxDescriptorSetUpdateAfterBindUniformBuffers props
  CapDescriptorsInAllPools →
    maxUpdateAfterBindDescriptorsInAllPools props
  CapBoundDescriptorSets →
    maxBoundDescriptorSets base
  CapBasePerStageUniformBuffers →
    maxPerStageDescriptorUniformBuffers base
  CapBaseSetUniformBuffers →
    maxDescriptorSetUniformBuffers base

-- | Every capacity that APPLIES to this device, paired with what it
--   reports, in declaration order. A capacity whose Valid Usage statement
--   the device's features do not activate is absent rather than zero — it
--   constrains nothing, so it must not be able to produce a shortfall.
reportedBindlessCapacities
  ∷ PhysicalDeviceVulkan12Features
  → PhysicalDeviceLimits
  → PhysicalDeviceVulkan12Properties
  → [(BindlessCapacity, Word32)]
reportedBindlessCapacities feats base props =
  [ (cap, readBindlessCapacity base props cap)
  | cap ← requiredBindlessCapacities
  , bindlessCapacityApplies feats cap ]
