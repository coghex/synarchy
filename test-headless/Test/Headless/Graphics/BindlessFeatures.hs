-- | The descriptor-indexing contract the bindless renderer runs under
--   (#1282). Before it, the logical device enabled six Vulkan 1.2 features
--   from a hand-written literal while the bindless descriptor-set layout
--   set @DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT@ on a combined image
--   sampler — whose one enabling feature,
--   @descriptorBindingSampledImageUpdateAfterBind@, was not among the six.
--   Every Vulkan-rendering boot therefore built that layout outside the
--   spec's validity contract, and capability evaluation could not see it
--   because it never read a feature boolean at all.
--
--   These specs pin the whole contract without a GPU: the exact field set
--   the device enables, the feature/version/capacity gates that classify a
--   device unsupported, the layout flags each required feature justifies,
--   and the selection rule that keeps a capable candidate ahead of an
--   incapable one whatever their device types.
--
--   Since #1689 the capacity gate is part of that contract. The bindless
--   texture array is FIXED-SIZE — both fragment shaders declare
--   @textures[maxBindlessTextures]@ and index it with @nonuniformEXT@ — so
--   a device is accepted only when it can supply the WHOLE descriptor
--   binding, and the binding is that size on every accepted device. The
--   boundary specs below sit one descriptor either side of each capacity
--   the concrete layout and pool consume.
module Test.Headless.Graphics.BindlessFeatures (spec) where

import UPrelude
import qualified Data.Text as T
import Test.Hspec
import Engine.Graphics.Vulkan.Capability
  (BindlessSupport(..), TextureSystemCapability(..), bindlessCapacityShortfalls
  ,bindlessShortfalls, determineTextureCapability, deviceBindlessFailureMessage
  ,isBindlessSupported, minimumUsableSlots, unsupportedBindlessMessage)
import Engine.Graphics.Vulkan.Device (bindlessCapableBonus, scoreDevice)
import Engine.Graphics.Vulkan.Texture.Limits (maxBindlessTextures)
import Engine.Graphics.Vulkan.Texture.Requirements
  (BindlessCapacity(..), BindlessFeature(..), bindlessCapacityApplies
  ,bindlessCapacityField, bindlessCapacityRequirement, bindlessFeatureField
  ,bindlessTextureBindingFlags, bindlessTextureBindingRequirements
  ,enableBindlessFeature, missingBindlessFeatures, readBindlessFeature
  ,reportedBindlessCapacities, requiredBindlessCapacities
  ,requiredBindlessFeatures, requiredVulkan12Features)
import Engine.Graphics.Vulkan.Texture.System
  (TextureSystemConfig(..), planBindlessDescriptorCount)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceLimits(..))
import Vulkan.Core10.Enums.PhysicalDeviceType (PhysicalDeviceType(..))
import Vulkan.Core12
  (PhysicalDeviceVulkan12Features(..), PhysicalDeviceVulkan12Properties(..))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits
  (DescriptorBindingFlagBits(..))
import Vulkan.Zero (zero)

-- | An update-after-bind properties report that clears every capacity gate
--   with room to spare — the figures the development GPU's MoltenVK driver
--   actually reports, so the baseline is a real device rather than an
--   invented one.
generousProperties ∷ PhysicalDeviceVulkan12Properties
generousProperties = (zero ∷ PhysicalDeviceVulkan12Properties)
  { maxPerStageDescriptorUpdateAfterBindSampledImages  = 1000000
  , maxPerStageDescriptorUpdateAfterBindSamplers       = 500000
  , maxPerStageDescriptorUpdateAfterBindStorageBuffers = 1000000
  , maxPerStageDescriptorUpdateAfterBindUniformBuffers = 1000000
  , maxPerStageUpdateAfterBindResources                = 1000000
  , maxDescriptorSetUpdateAfterBindSampledImages       = 1000000
  , maxDescriptorSetUpdateAfterBindSamplers            = 500000
  , maxDescriptorSetUpdateAfterBindStorageBuffers      = 1000000
  , maxDescriptorSetUpdateAfterBindUniformBuffers      = 1000000
  , maxUpdateAfterBindDescriptorsInAllPools            = 1073741824
  }

-- | The ordinary limits a conformant device is guaranteed to clear: these
--   are Vulkan's own required minima for the three statements that count
--   the bindless pipeline layout's non-update-after-bind set.
generousBaseLimits ∷ PhysicalDeviceLimits
generousBaseLimits = (zero ∷ PhysicalDeviceLimits)
  { maxBoundDescriptorSets             = 4
  , maxPerStageDescriptorUniformBuffers = 12
  , maxDescriptorSetUniformBuffers     = 72
  }

-- | Each required capacity paired with a setter for the
--   @VkPhysicalDeviceVulkan12Properties@ field it names. Written out
--   longhand for the same reason 'absentOne' is: this is the test's own
--   independent statement of the mapping, so a production-side reshuffle
--   has to be reflected here deliberately. The completeness spec below
--   checks the table still covers 'requiredBindlessCapacities' exactly.
capacityFields
  ∷ [( BindlessCapacity
     , Word32 → PhysicalDeviceVulkan12Properties
              → PhysicalDeviceVulkan12Properties )]
capacityFields =
  [ ( CapPerStageSampledImages
    , \n p → p { maxPerStageDescriptorUpdateAfterBindSampledImages = n } )
  , ( CapPerStageSamplers
    , \n p → p { maxPerStageDescriptorUpdateAfterBindSamplers = n } )
  , ( CapPerStageStorageBuffers
    , \n p → p { maxPerStageDescriptorUpdateAfterBindStorageBuffers = n } )
  , ( CapPerStageUniformBuffers
    , \n p → p { maxPerStageDescriptorUpdateAfterBindUniformBuffers = n } )
  , ( CapPerStageResources
    , \n p → p { maxPerStageUpdateAfterBindResources = n } )
  , ( CapSetSampledImages
    , \n p → p { maxDescriptorSetUpdateAfterBindSampledImages = n } )
  , ( CapSetSamplers
    , \n p → p { maxDescriptorSetUpdateAfterBindSamplers = n } )
  , ( CapSetStorageBuffers
    , \n p → p { maxDescriptorSetUpdateAfterBindStorageBuffers = n } )
  , ( CapSetUniformBuffers
    , \n p → p { maxDescriptorSetUpdateAfterBindUniformBuffers = n } )
  , ( CapDescriptorsInAllPools
    , \n p → p { maxUpdateAfterBindDescriptorsInAllPools = n } )
  ]

-- | The same, for the three ORDINARY statements, whose limits live on a
--   different struct entirely. Kept apart from 'capacityFields' on purpose:
--   the separation is the contract — Vulkan never combines an ordinary limit
--   with its update-after-bind counterpart, and a table that could set
--   either through one setter would blur exactly that.
baseCapacityFields
  ∷ [(BindlessCapacity, Word32 → PhysicalDeviceLimits → PhysicalDeviceLimits)]
baseCapacityFields =
  [ ( CapBoundDescriptorSets
    , \n l → l { maxBoundDescriptorSets = n } )
  , ( CapBasePerStageUniformBuffers
    , \n l → l { maxPerStageDescriptorUniformBuffers = n } )
  , ( CapBaseSetUniformBuffers
    , \n l → l { maxDescriptorSetUniformBuffers = n } )
  ]

-- | A feature report advertising every @descriptorBinding…UpdateAfterBind@
--   feature, so every capacity's Valid Usage statement is active and the
--   boundary sweep below covers all ten. The renderer itself requires only
--   the sampled-image one — the other two are the device's own support,
--   which is what conditions VUIDs 03023/03024 and 03037/03039.
generousFeatures ∷ PhysicalDeviceVulkan12Features
generousFeatures = requiredVulkan12Features
  { descriptorBindingUniformBufferUpdateAfterBind = True
  , descriptorBindingStorageBufferUpdateAfterBind = True
  }

-- | A device that clears every bindless gate: the baseline each spec below
--   spoils in exactly one way.
capableSupport ∷ BindlessSupport
capableSupport = supportReporting generousProperties

-- | 'capableSupport' with its capacities taken from a specific report.
supportReporting ∷ PhysicalDeviceVulkan12Properties → BindlessSupport
supportReporting = supportReportingWith generousFeatures

-- | 'supportReporting' for a device advertising a specific feature set, so
--   a spec can spoil which capacities apply as well as what they report.
supportReportingWith ∷ PhysicalDeviceVulkan12Features
                     → PhysicalDeviceVulkan12Properties → BindlessSupport
supportReportingWith feats = supportReportingBase feats generousBaseLimits

-- | The full three-struct baseline, for the specs that spoil an ORDINARY
--   limit rather than an update-after-bind one.
supportReportingBase ∷ PhysicalDeviceVulkan12Features → PhysicalDeviceLimits
                     → PhysicalDeviceVulkan12Properties → BindlessSupport
supportReportingBase feats base props = BindlessSupport
  { bsVulkan12OrHigher              = True
  , bsMaxSampledImagesPerStage      = 128
  , bsMaxDescriptorSetSampledImages = 640
  , bsCapacities  = reportedBindlessCapacities feats base props
  , bsMissingFeatures               = missingBindlessFeatures feats
  }

-- | 'generousProperties' with one capacity's field lowered.
reportingOnly ∷ (Word32 → PhysicalDeviceVulkan12Properties
                        → PhysicalDeviceVulkan12Properties)
              → Word32 → BindlessSupport
reportingOnly setField n = supportReporting (setField n generousProperties)

-- | The diagnostic clause a capacity shortfall must produce, stated here
--   independently of the production wording: the reported count, the field
--   the device reported it for, and the count the renderer requires.
expectedShortfallClause ∷ BindlessCapacity → Word32 → Text
expectedShortfallClause cap reported =
  "the device reports " <> tshow reported <> " " <> bindlessCapacityField cap
    <> ", but the bindless texture array requires "
    <> tshow (bindlessCapacityRequirement cap)

-- | The sampled-image setter, named so the version/limit specs can spoil
--   that one capacity without re-deriving the table.
setSampledImages ∷ Word32 → PhysicalDeviceVulkan12Properties
                          → PhysicalDeviceVulkan12Properties
setSampledImages n p =
  p { maxPerStageDescriptorUpdateAfterBindSampledImages = n }

-- | 'generousProperties' with one named capacity's field set to @n@,
--   looked up through the same table the boundary specs sweep.
loweredTo ∷ Word32 → BindlessCapacity → PhysicalDeviceVulkan12Properties
loweredTo n cap =
  case lookup cap capacityFields of
    Just setField → setField n generousProperties
    Nothing       → generousProperties

-- | The one production configuration: slot 0 held back for the undefined
--   texture ("Engine.Graphics.Vulkan.Init").
productionTextureConfig ∷ TextureSystemConfig
productionTextureConfig = TextureSystemConfig { tscReservedSlots = 1 }

-- | Each required feature paired with a feature struct in which ONLY that
--   field is off. Written out longhand on purpose: this is the test's own
--   independent statement of which struct field each constructor names, so
--   a production-side rename or reshuffle has to be reflected here
--   deliberately rather than passing by construction. The completeness spec
--   below checks the table still covers 'requiredBindlessFeatures' exactly.
absentOne ∷ [(BindlessFeature, PhysicalDeviceVulkan12Features)]
absentOne =
  [ ( FeatShaderSampledImageArrayNonUniformIndexing
    , requiredVulkan12Features
        { shaderSampledImageArrayNonUniformIndexing = False } )
  , ( FeatDescriptorBindingPartiallyBound
    , requiredVulkan12Features
        { descriptorBindingPartiallyBound = False } )
  , ( FeatDescriptorBindingSampledImageUpdateAfterBind
    , requiredVulkan12Features
        { descriptorBindingSampledImageUpdateAfterBind = False } )
  ]

-- | Every device type Vulkan defines, so the ordering specs cover the whole
--   base-score range rather than the two types a dev machine happens to have.
allDeviceTypes ∷ [PhysicalDeviceType]
allDeviceTypes =
  [ PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , PHYSICAL_DEVICE_TYPE_CPU
  , PHYSICAL_DEVICE_TYPE_OTHER
  ]

spec ∷ Spec
spec = describe "Vulkan bindless feature requirements" $ do

  describe "the enabled feature set" $ do
    it "enables exactly the three fields the shaders and layout use" $
      -- Field-for-field, so an extra request is as much a failure as a
      -- missing one: anything enabled here that the renderer does not use
      -- is a request a device may refuse for no benefit.
      requiredVulkan12Features `shouldBe` (zero
        { shaderSampledImageArrayNonUniformIndexing    = True
        , descriptorBindingPartiallyBound              = True
        , descriptorBindingSampledImageUpdateAfterBind = True
        } ∷ PhysicalDeviceVulkan12Features)

    it "enables descriptorBindingSampledImageUpdateAfterBind, the one #1282 omitted" $
      descriptorBindingSampledImageUpdateAfterBind requiredVulkan12Features
        `shouldBe` True

    it "no longer requests the features nothing in the renderer uses" $ do
      -- Fixed-size texture arrays (#975) need no runtime descriptor array
      -- and no variable count; descriptors are never rewritten while
      -- pending; and the aggregate roll-up boolean enables nothing.
      descriptorIndexing requiredVulkan12Features `shouldBe` False
      runtimeDescriptorArray requiredVulkan12Features `shouldBe` False
      descriptorBindingUpdateUnusedWhilePending requiredVulkan12Features
        `shouldBe` False
      descriptorBindingVariableDescriptorCount requiredVulkan12Features
        `shouldBe` False

    it "reports every required feature present in its own enabled set" $
      missingBindlessFeatures requiredVulkan12Features `shouldBe` []

    it "reports every required feature missing from an all-zero struct" $
      missingBindlessFeatures (zero ∷ PhysicalDeviceVulkan12Features)
        `shouldBe` requiredBindlessFeatures

    it "enables each required feature one at a time" $
      forM_ requiredBindlessFeatures $ \feature → do
        let feats = enableBindlessFeature feature
                      (zero ∷ PhysicalDeviceVulkan12Features)
        readBindlessFeature feats feature `shouldBe` True
        missingBindlessFeatures feats
          `shouldBe` filter (≢ feature) requiredBindlessFeatures

  describe "a device missing one required feature" $ do
    it "covers every required feature in the individually-absent table" $
      map fst absentOne `shouldBe` requiredBindlessFeatures

    it "is reported missing exactly that feature, whichever it is" $
      forM_ absentOne $ \(feature, feats) →
        missingBindlessFeatures feats `shouldBe` [feature]

    it "is unsupported, whichever feature it is" $
      forM_ requiredBindlessFeatures $ \feature →
        isBindlessSupported capableSupport { bsMissingFeatures = [feature] }
          `shouldBe` False

    it "falls back out of BindlessTextures, whichever feature it is" $
      forM_ requiredBindlessFeatures $ \feature →
        determineTextureCapability
          capableSupport { bsMissingFeatures = [feature] } 0
            `shouldBe` BoundedTextureArray 256

    it "is named by its Vulkan field name in the startup diagnostic" $
      forM_ requiredBindlessFeatures $ \feature → do
        let support = capableSupport { bsMissingFeatures = [feature] }
            message = unsupportedBindlessMessage support
                        (determineTextureCapability support 0)
        message `shouldSatisfy`
          T.isInfixOf (bindlessFeatureField feature)
        message `shouldSatisfy` T.isInfixOf "Bindless textures are required"

  describe "the version and limit gates" $ do
    it "supports a device that clears all three gates" $
      isBindlessSupported capableSupport `shouldBe` True

    it "still rejects a device below Vulkan 1.2" $
      isBindlessSupported capableSupport { bsVulkan12OrHigher = False }
        `shouldBe` False

    it "still rejects a device with no update-after-bind sampled images" $
      isBindlessSupported (reportingOnly setSampledImages 0) `shouldBe` False

    it "reports a pre-1.2 device on the version alone" $
      -- Below Vulkan 1.2 nothing chains a 1.2 feature struct, so the
      -- feature list is a consequence of the version gate rather than
      -- evidence: the diagnostic must not claim the driver was asked.
      bindlessShortfalls capableSupport
        { bsVulkan12OrHigher = False
        , bsMissingFeatures  = requiredBindlessFeatures
        , bsCapacities       =
            reportedBindlessCapacities generousFeatures zero zero
        } `shouldBe` ["Vulkan 1.2 or higher is required"]

    it "reports nothing at all for a device that clears every gate" $
      bindlessShortfalls capableSupport `shouldBe` []

    it "allocates the shader's array size, not the device-reported ceiling" $
      -- The device report is a gate, never a size: an accepted device gets
      -- the binding both shaders declare, whatever headroom it advertises
      -- above it (#1689).
      determineTextureCapability capableSupport 64
        `shouldBe` BindlessTextures maxBindlessTextures

  describe "the descriptor-capacity gate" $ do
    it "covers every required capacity across the two field tables" $
      (map fst capacityFields <> map fst baseCapacityFields)
        `shouldBe` requiredBindlessCapacities

    it "requires exactly the array size both fragment shaders declare" $
      -- #975's single definition, still single: the texture capacities are
      -- 'maxBindlessTextures' itself, not a number that could drift from it.
      forM_ [CapPerStageSampledImages, CapPerStageSamplers
            ,CapSetSampledImages, CapSetSamplers] $ \cap →
        (cap, bindlessCapacityRequirement cap)
          `shouldBe` (cap, maxBindlessTextures)

    it "covers the uniform buffer set 0 contributes to the same pipeline layout" $ do
      -- Every update-after-bind pipeline-layout statement counts descriptors
      -- across ALL of pSetLayouts, and
      -- 'Engine.Graphics.Vulkan.Pipeline.Bindless' pairs the bindless
      -- texture layout with set 0's vertex-stage uniform buffer — so on a
      -- device that supports descriptorBindingUniformBufferUpdateAfterBind,
      -- a zero update-after-bind uniform limit would pass a texture-only
      -- predicate and then fail pipeline-layout creation (VUID 03023/03037).
      forM_ [CapPerStageUniformBuffers, CapSetUniformBuffers] $ \cap → do
        (cap, bindlessCapacityRequirement cap) `shouldBe` (cap, 1)
        (cap, isBindlessSupported (supportReporting (loweredTo 0 cap)))
          `shouldBe` (cap, False)

    it "accepts a device reporting exactly what every capacity requires" $ do
      -- The threshold itself, on every capacity at once: the accepted side
      -- of the boundary.
      let atThreshold = foldr
            (\(cap, setField) props →
               setField (bindlessCapacityRequirement cap) props)
            generousProperties capacityFields
          baseAtThreshold = foldr
            (\(cap, setField) limits →
               setField (bindlessCapacityRequirement cap) limits)
            generousBaseLimits baseCapacityFields
          support = supportReportingBase generousFeatures
                      baseAtThreshold atThreshold
      bindlessCapacityShortfalls support `shouldBe` []
      isBindlessSupported support `shouldBe` True
      planBindlessDescriptorCount support productionTextureConfig
        `shouldBe` Right maxBindlessTextures

    it "rejects a device one descriptor below any single capacity" $
      -- The rejected side of the same boundary, one capacity at a time, so
      -- a gate silently dropped from the predicate fails here.
      forM_ capacityFields $ \(cap, setField) → do
        let short   = bindlessCapacityRequirement cap - 1
            support = reportingOnly setField short
        (cap, isBindlessSupported support) `shouldBe` (cap, False)
        (cap, determineTextureCapability support 1)
          `shouldBe` (cap, BoundedTextureArray minimumUsableSlots)

    it "never plans a binding smaller than the shader array size" $
      -- Requirement 1 stated directly: whatever an accepted device reports,
      -- and whatever the reservations are, the planned descriptor count is
      -- never below what the shaders index.
      forM_ [0, 1, 64, maxBindlessTextures - minimumUsableSlots] $ \reserved →
        case planBindlessDescriptorCount capableSupport
               (TextureSystemConfig { tscReservedSlots = reserved }) of
          Left failure → expectationFailure $
            "a capable device was refused at " <> show reserved
              <> " reserved slots: " <> T.unpack failure
          Right count → (reserved, count) `shouldSatisfy`
            \(_, n) → n ≥ maxBindlessTextures

    it "names the reported and required counts of whichever capacity fell short" $
      -- #1055's descriptive contract, extended to the capacity gate: device
      -- selection has to say WHICH limit came up short and by how much.
      forM_ capacityFields $ \(cap, setField) → do
        let short   = bindlessCapacityRequirement cap - 1
            support = reportingOnly setField short
        (cap, deviceBindlessFailureMessage support) `shouldSatisfy`
          (T.isInfixOf (expectedShortfallClause cap short) . snd)

    it "rejects a device one below any ORDINARY limit the layout needs" $
      -- The other half of the split. These count only the layout's
      -- non-update-after-bind set — set 0's one uniform buffer, and the two
      -- sets the layout binds — and no update-after-bind headroom
      -- substitutes for them, exactly as none of them substitutes for an
      -- update-after-bind limit.
      forM_ baseCapacityFields $ \(cap, setField) → do
        let short   = bindlessCapacityRequirement cap - 1
            support = supportReportingBase generousFeatures
                        (setField short generousBaseLimits) generousProperties
        (cap, isBindlessSupported support) `shouldBe` (cap, False)
        (cap, deviceBindlessFailureMessage support) `shouldSatisfy`
          (T.isInfixOf (expectedShortfallClause cap short) . snd)

    it "the ordinary limits never see the bindless texture array" $
      -- Not a stylistic preference for two checks over one: the bindless
      -- texture layout is created WITH
      -- UPDATE_AFTER_BIND_POOL_BIT, and an ordinary limit counts "only
      -- descriptors in descriptor set layouts created WITHOUT" that bit. Its
      -- statement therefore counts ZERO of the array's descriptors, so no
      -- value of it — not even maxBound, as below — can make the layout
      -- valid. Reading the larger of the pair would accept a device whose
      -- pipeline layout Vulkan rejects.
      forM_ capacityFields $ \(cap, setField) → do
        let support = supportReportingBase generousFeatures
                        (zero ∷ PhysicalDeviceLimits)
                          { maxBoundDescriptorSets              = maxBound
                          , maxPerStageDescriptorUniformBuffers = maxBound
                          , maxDescriptorSetUniformBuffers      = maxBound
                          , maxPerStageDescriptorSamplers       = maxBound
                          , maxDescriptorSetSamplers            = maxBound
                          , maxPerStageDescriptorSampledImages  = maxBound
                          , maxDescriptorSetSampledImages       = maxBound
                          , maxPerStageDescriptorStorageBuffers = maxBound
                          , maxDescriptorSetStorageBuffers      = maxBound
                          , maxPerStageResources                = maxBound
                          }
                        (setField (bindlessCapacityRequirement cap - 1)
                                  generousProperties)
        (cap, isBindlessSupported support) `shouldBe` (cap, False)

    it "names the same counts on the texture-system failure path" $
      -- The two sites describe one shortfall identically (#1282), so the
      -- capacity refusal reads the same wherever it is hit.
      forM_ capacityFields $ \(cap, setField) → do
        let short   = bindlessCapacityRequirement cap - 1
            support = reportingOnly setField short
        case planBindlessDescriptorCount support productionTextureConfig of
          Right count → expectationFailure $
            "a device short on " <> T.unpack (bindlessCapacityField cap)
              <> " was accepted with " <> show count <> " descriptors"
          Left failure → (cap, failure) `shouldSatisfy`
            (T.isInfixOf (expectedShortfallClause cap short) . snd)

  describe "capacities the device's features leave inactive" $ do
    -- Each update-after-bind pipeline-layout statement is conditional on the
    -- device SUPPORTING the matching descriptorBinding…UpdateAfterBind
    -- feature. A limit whose statement is inactive constrains nothing this
    -- renderer builds, so gating on it would refuse a device whose pipeline
    -- layout Vulkan accepts.
    it "drops a capacity whose feature the device does not advertise" $ do
      let withoutBuffers = requiredVulkan12Features
          reported = map fst (reportedBindlessCapacities withoutBuffers
                                generousBaseLimits generousProperties)
      forM_ [CapPerStageUniformBuffers, CapSetUniformBuffers
            ,CapPerStageStorageBuffers, CapSetStorageBuffers] $ \cap →
        (cap, cap `elem` reported) `shouldBe` (cap, False)
      -- The texture capacities ride the one feature the renderer requires,
      -- and the ordinary statements are unconditional, so neither group is
      -- ever dropped on a device the renderer would accept at all.
      forM_ [CapPerStageSampledImages, CapPerStageSamplers
            ,CapSetSampledImages, CapSetSamplers
            ,CapPerStageResources, CapDescriptorsInAllPools
            ,CapBoundDescriptorSets, CapBasePerStageUniformBuffers
            ,CapBaseSetUniformBuffers] $ \cap →
        (cap, cap `elem` reported) `shouldBe` (cap, True)

    it "accepts a zero buffer limit exactly when its statement is inactive" $
      -- The whole rule in one comparison: identical properties, and only the
      -- device's own feature support decides whether the limit binds.
      forM_ [ (CapPerStageUniformBuffers
              ,requiredVulkan12Features
                 { descriptorBindingUniformBufferUpdateAfterBind = True })
            , (CapSetUniformBuffers
              ,requiredVulkan12Features
                 { descriptorBindingUniformBufferUpdateAfterBind = True })
            , (CapPerStageStorageBuffers
              ,requiredVulkan12Features
                 { descriptorBindingStorageBufferUpdateAfterBind = True })
            , (CapSetStorageBuffers
              ,requiredVulkan12Features
                 { descriptorBindingStorageBufferUpdateAfterBind = True })
            ] $ \(cap, withFeature) → do
        let props = loweredTo 0 cap
        (cap, isBindlessSupported
                (supportReportingWith requiredVulkan12Features props))
          `shouldBe` (cap, True)
        (cap, isBindlessSupported (supportReportingWith withFeature props))
          `shouldBe` (cap, False)

    it "keeps the two unconditional capacities binding whatever the features" $
      -- The aggregate governs any pipeline layout holding an
      -- update-after-bind set, and the pool ceiling governs creating that
      -- pool at all; neither is conditioned on a feature.
      forM_ [CapPerStageResources, CapDescriptorsInAllPools] $ \cap → do
        (cap, bindlessCapacityApplies (zero ∷ PhysicalDeviceVulkan12Features) cap)
          `shouldBe` (cap, True)
        (cap, isBindlessSupported
                (supportReportingWith requiredVulkan12Features
                   (loweredTo (bindlessCapacityRequirement cap - 1) cap)))
          `shouldBe` (cap, False)

  describe "reserved slots and the fixed binding" $ do
    it "reserves indices inside the binding rather than shrinking it" $
      -- The defect #1689 fixed: production reserves slot 0, and that used to
      -- subtract a descriptor from a binding the shaders index in full.
      forM_ [0, 1, 64, maxBindlessTextures - minimumUsableSlots] $ \reserved →
        (reserved, determineTextureCapability capableSupport reserved)
          `shouldBe` (reserved, BindlessTextures maxBindlessTextures)

    it "still refuses when the reservations leave too few application slots" $
      determineTextureCapability capableSupport
        (maxBindlessTextures - minimumUsableSlots + 1)
          `shouldBe` BoundedTextureArray minimumUsableSlots

    it "describes a reservation refusal instead of leaving it unexplained" $ do
      -- A device that clears every capability gate has no shortfall to name,
      -- which used to leave this rejection with a bare message.
      let reserved   = maxBindlessTextures - minimumUsableSlots + 1
          capability = determineTextureCapability capableSupport reserved
          message    = unsupportedBindlessMessage capableSupport capability
      bindlessShortfalls capableSupport `shouldBe` []
      message `shouldSatisfy` T.isInfixOf "slot reservations"
      message `shouldSatisfy` T.isInfixOf (tshow minimumUsableSlots)
      message `shouldSatisfy` T.isInfixOf (tshow maxBindlessTextures)

  describe "layout flags and enabled features" $ do
    it "sets exactly the two flags the bindless texture binding needs" $
      bindlessTextureBindingFlags `shouldBe`
        (DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
         ⌄ DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT)

    it "justifies every binding flag with a required, enabled feature" $
      -- The join that stops the two literals drifting apart again: a flag
      -- the layout sets whose feature is not required, or is required but
      -- not actually turned on for device creation, fails here.
      forM_ bindlessTextureBindingRequirements $ \(flag, feature) → do
        (flag, feature `elem` requiredBindlessFeatures) `shouldBe` (flag, True)
        (flag, readBindlessFeature requiredVulkan12Features feature)
          `shouldBe` (flag, True)

  describe "physical-device selection" $ do
    it "ranks every capable candidate above every incapable one" $ do
      -- The whole point: a discrete GPU this renderer cannot use must never
      -- displace a capable CPU-type or virtual candidate.
      let capable   = [scoreDevice t True True  | t ← allDeviceTypes]
          incapable = [scoreDevice t True False | t ← allDeviceTypes]
      minimum capable `shouldSatisfy` (> maximum incapable)

    it "keeps the discrete-over-integrated preference among capable candidates" $
      scoreDevice PHYSICAL_DEVICE_TYPE_DISCRETE_GPU True True
        `shouldSatisfy`
          (> scoreDevice PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU True True)

    it "keeps the same preference among incapable candidates" $
      scoreDevice PHYSICAL_DEVICE_TYPE_DISCRETE_GPU True False
        `shouldSatisfy`
          (> scoreDevice PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU True False)

    it "scores an unusable device 0 however capable it is" $
      forM_ allDeviceTypes $ \t → do
        scoreDevice t False True `shouldBe` 0
        scoreDevice t False False `shouldBe` 0

    it "keeps the capability bonus above every device-type base score" $
      -- If a base score ever reached the bonus, capability would stop
      -- dominating type and the ordering spec above would be accidental.
      maximum [scoreDevice t True False | t ← allDeviceTypes]
        `shouldSatisfy` (< bindlessCapableBonus)
