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
  , CapacityScope(..)
  , CapacityCheck(..)
  , capacityCheckHolds
  , layoutDescriptorsInScope
  , requiredBindlessCapacities
  , handleTableDescriptors
  , pipelineUniformBufferDescriptors
  , bindlessPipelineSetCount
  , bindlessColorAttachments
  , ordinaryCapacityField
  , updateAfterBindCapacityField
  , readOrdinaryLimit
  , readUpdateAfterBindLimit
  , bindlessCapacityChecks
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

-- | Uniform-buffer descriptors set 0 contributes to the bindless pipeline
--   layout: one vertex-stage uniform buffer
--   ("Engine.Graphics.Vulkan.Descriptor"'s @createUniformDescriptorSetLayout@,
--   paired with the bindless texture layout in
--   "Engine.Graphics.Vulkan.Pipeline.Bindless"). Set 0 is an ORDINARY
--   layout, so unlike the texture array this descriptor is counted by both
--   families of statement.
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

-- | Which descriptors of the bindless pipeline layout a Valid Usage
--   statement counts.
--
--   Vulkan writes each descriptor-count rule twice, once per scope, and
--   BOTH hold at the same time, because each ranges over a DIFFERENT
--   population of this layout's descriptors — which is what
--   'layoutDescriptorsInScope' records for this renderer's layout. Neither
--   statement of a pair is skipped.
--
--   Two populations are not, however, two independent ceilings. The
--   all-set statements are satisfied by the EFFECTIVE MAXIMUM of a paired
--   class — the greater of the ordinary limit and its update-after-bind
--   counterpart — so ordinary headroom can supply the all-set total, and a
--   paired update-after-bind field below the requirement is not on its own
--   a refusal. What stays independent is the smaller
--   non-update-after-bind population, measured against its ordinary limit
--   alone. 'bindlessCapacityChecks' is where both fall out.
data CapacityScope
  = -- | Counts ONLY descriptors in set layouts created WITHOUT
    --   @UPDATE_AFTER_BIND_POOL_BIT@ — VUIDs 03016-03018, 03028-03029, and
    --   @maxPerStageResources@, each of whose limit text says exactly that.
    --   Set 0 is such a layout; the bindless texture layout is NOT.
    ScopeWithoutUpdateAfterBind
  | -- | Counts descriptors in EVERY set layout, with the bit or without —
    --   VUIDs 03022-03027, 03036-03043, whose limit text reads "with or
    --   without".
    ScopeAllSets
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | One descriptor-count rule the bindless pipeline layout must satisfy: a
--   limit, the scope of descriptors its statement counts, how many of this
--   renderer's descriptors fall in that scope, and what the device reports.
data CapacityCheck = CapacityCheck
  { ccField    ∷ Text          -- ^ the limit's @VkPhysicalDevice…@ field name
  , ccScope    ∷ CapacityScope -- ^ which descriptors its statement counts
  , ccRequired ∷ Word32        -- ^ how many of ours are in that scope
  , ccReported ∷ Word32        -- ^ what the device advertises for it
  } deriving (Show, Eq)

-- | Whether one rule is satisfied.
capacityCheckHolds ∷ CapacityCheck → Bool
capacityCheckHolds check = ccReported check ≥ ccRequired check

-- | A descriptor class the bindless pipeline layout puts descriptors into,
--   at one granularity. Each names a PAIR of statements — the ordinary one
--   and its update-after-bind counterpart — except where only one exists.
data BindlessCapacity
  = -- | The texture array, per fragment stage
    --   (@maxPerStageDescriptorSampledImages@ / …@UpdateAfterBindSampledImages@).
    CapPerStageSampledImages
  | -- | The same descriptors as SAMPLERS: a combined image sampler is both.
    CapPerStageSamplers
  | -- | The handle→slot table, per fragment stage.
    CapPerStageStorageBuffers
  | -- | Set 0's uniform buffer, per vertex stage.
    CapPerStageUniformBuffers
  | -- | The busiest stage's aggregate (@maxPerStageResources@ /
    --   @maxPerStageUpdateAfterBindResources@): the fragment stage's array,
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
  | -- | @maxBoundDescriptorSets@ (VUID 00286). Ordinary only: there is no
    --   update-after-bind counterpart.
    CapBoundDescriptorSets
  | -- | @maxUpdateAfterBindDescriptorsInAllPools@. Update-after-bind only:
    --   it governs creating the pool at all, and exceeding it fails pool
    --   creation with @VK_ERROR_FRAGMENTATION@.
    CapDescriptorsInAllPools
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every descriptor class the renderer's layout is measured on. Derived
--   from the type so a constructor added above is required by construction.
requiredBindlessCapacities ∷ [BindlessCapacity]
requiredBindlessCapacities = [minBound .. maxBound]

-- | How many descriptors of one class THIS renderer's pipeline layout puts
--   against a statement of the given scope.
--
--   Read the sampler and sampled-image rows: under
--   'ScopeWithoutUpdateAfterBind' the answer is ZERO. The 16,384-entry
--   texture array lives in a layout created WITH
--   @UPDATE_AFTER_BIND_POOL_BIT@ ('createBindlessDescriptorSetLayout'), so
--   an ordinary sampler or sampled-image statement counts none of it. That
--   statement is still CHECKED — 'bindlessCapacityChecks' emits it like any
--   other — it is simply satisfied by zero, at any reported value, so it
--   can never reject this layout on the array's account.
--
--   That is a fact about the POPULATION an ordinary statement ranges over,
--   not about what its reported value is good for. The all-set total is
--   measured against the effective maximum of the pair, so the ordinary
--   sampler limit's reported figure can still be the one that supplies the
--   array's 16,384 descriptors when the update-after-bind field alone
--   would fall short ('bindlessCapacityChecks').
--
--   Set 0's uniform buffer is the converse and shows the split runs both
--   ways: it sits in an ordinary layout, so it counts in BOTH scopes and is
--   gated under both.
layoutDescriptorsInScope ∷ CapacityScope → BindlessCapacity → Word32
layoutDescriptorsInScope scope cap = case (scope, cap) of
  (ScopeWithoutUpdateAfterBind, CapPerStageSamplers)       → 0
  (ScopeWithoutUpdateAfterBind, CapSetSamplers)            → 0
  (ScopeWithoutUpdateAfterBind, CapPerStageSampledImages)  → 0
  (ScopeWithoutUpdateAfterBind, CapSetSampledImages)       → 0
  (ScopeAllSets, CapPerStageSamplers)                      → maxBindlessTextures
  (ScopeAllSets, CapSetSamplers)                           → maxBindlessTextures
  (ScopeAllSets, CapPerStageSampledImages)                 → maxBindlessTextures
  (ScopeAllSets, CapSetSampledImages)                      → maxBindlessTextures

  (ScopeWithoutUpdateAfterBind, CapPerStageStorageBuffers) → 0
  (ScopeWithoutUpdateAfterBind, CapSetStorageBuffers)      → 0
  (ScopeAllSets, CapPerStageStorageBuffers)                → handleTableDescriptors
  (ScopeAllSets, CapSetStorageBuffers)                     → handleTableDescriptors

  (_, CapPerStageUniformBuffers) → pipelineUniformBufferDescriptors
  (_, CapSetUniformBuffers)      → pipelineUniformBufferDescriptors

  (ScopeWithoutUpdateAfterBind, CapPerStageResources) → bindlessColorAttachments
  (ScopeAllSets, CapPerStageResources) →
    maxBindlessTextures + handleTableDescriptors + bindlessColorAttachments

  (_, CapBoundDescriptorSets)   → bindlessPipelineSetCount
  (_, CapDescriptorsInAllPools) → maxBindlessTextures + handleTableDescriptors

-- | The @VkPhysicalDeviceLimits@ field carrying one class's ORDINARY
--   statement, and 'Nothing' where the class has no ordinary counterpart.
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
--   UPDATE-AFTER-BIND statement, and 'Nothing' where there is none.
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

-- | Every descriptor-count rule the device must satisfy for this layout.
--
--   BOTH statements of a pair are emitted and both are enforced, each
--   against the descriptors ITS OWN scope counts: an ordinary limit and
--   its update-after-bind counterpart are simultaneous @must@s over
--   different populations, so neither check is skipped. Emission is
--   unconditional — no device feature suppresses a rule, since the layout
--   puts descriptors of that class against it either way.
--
--   What the two are NOT is independent ceilings. The all-set check is
--   measured against the EFFECTIVE MAXIMUM of a paired class — the greater
--   of the two reported limits — so ordinary headroom can supply the
--   all-layout total, and a paired update-after-bind field below the
--   requirement is not on its own a refusal. The ordinary check is retained
--   rather than folded in because it ranges over a smaller population that
--   no update-after-bind headroom answers for. A class with no ordinary
--   counterpart — @maxUpdateAfterBindDescriptorsInAllPools@, whose
--   'ordinaryCapacityField' is 'Nothing' — has no pair to maximise over and
--   is enforced at exactly the value the device reports.
--
--   Where a statement counts none of our descriptors its check is present
--   and satisfied by zero, which is what makes \"which limit covered the
--   array?\" answerable in the data rather than in a comment.
bindlessCapacityChecks
  ∷ PhysicalDeviceLimits
  → PhysicalDeviceVulkan12Properties
  → BindlessCapacity
  → [CapacityCheck]
bindlessCapacityChecks base props cap = ordinary <> updateAfterBind
  where
    ordinary =
      [ CapacityCheck
          { ccField    = field
          , ccScope    = ScopeWithoutUpdateAfterBind
          , ccRequired = layoutDescriptorsInScope ScopeWithoutUpdateAfterBind cap
          , ccReported = readOrdinaryLimit base cap
          }
      | Just field ← [ordinaryCapacityField cap] ]
    updateAfterBind =
      [ CapacityCheck
          { ccField    = effectiveField field
          , ccScope    = ScopeAllSets
          , ccRequired = layoutDescriptorsInScope ScopeAllSets cap
          , ccReported = effectiveCapacity
          }
      | Just field ← [updateAfterBindCapacityField cap] ]
    -- The all-layout total is measured against the EFFECTIVE capacity: the
    -- greater of the update-after-bind limit and its ordinary counterpart
    -- where a pair exists, so ordinary headroom can supply it. The ordinary
    -- check above is retained rather than folded in — it constrains a
    -- different, smaller population (the layout's non-update-after-bind set)
    -- and no update-after-bind headroom answers for it.
    effectiveCapacity = case ordinaryCapacityField cap of
      Just _  → max (readUpdateAfterBindLimit props cap) (readOrdinaryLimit base cap)
      Nothing → readUpdateAfterBindLimit props cap
    effectiveField field = case ordinaryCapacityField cap of
      Just ordinaryName → ordinaryName <> " / " <> field <> " (effective maximum)"
      Nothing           → field

-- | Every rule the device must satisfy, across every descriptor class, in
--   declaration order.
reportedBindlessCapacities
  ∷ PhysicalDeviceLimits
  → PhysicalDeviceVulkan12Properties
  → [CapacityCheck]
reportedBindlessCapacities base props =
  concatMap (bindlessCapacityChecks base props) requiredBindlessCapacities
