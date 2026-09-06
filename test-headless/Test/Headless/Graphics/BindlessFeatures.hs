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
  (BindlessCapacity(..), BindlessFeature(..), CapacityCheck(..)
  ,CapacityScope(..), bindlessFeatureField
  ,bindlessTextureBindingFlags, bindlessTextureBindingRequirements
  ,enableBindlessFeature, layoutDescriptorsInScope, missingBindlessFeatures
  ,bindlessCapacityChecks, ordinaryCapacityField, readBindlessFeature
  ,reportedBindlessCapacities
  ,requiredBindlessCapacities, requiredBindlessFeatures
  ,requiredVulkan12Features, updateAfterBindCapacityField)
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

-- | Vulkan's REQUIRED MINIMA for every ordinary limit the bindless pipeline
--   layout is measured against — the weakest report a conformant device may
--   give. Using the minima rather than generous figures makes a real
--   property visible: the renderer's ordinary-statement requirements are
--   satisfiable by the feeblest conformant Vulkan device, so the only limits
--   that can ever refuse it are the update-after-bind ones the fixed texture
--   array actually strains.
generousBaseLimits ∷ PhysicalDeviceLimits
generousBaseLimits = (zero ∷ PhysicalDeviceLimits)
  { maxBoundDescriptorSets              = 4
  , maxPerStageDescriptorSamplers       = 16
  , maxPerStageDescriptorUniformBuffers = 12
  , maxPerStageDescriptorStorageBuffers = 4
  , maxPerStageDescriptorSampledImages  = 16
  , maxPerStageResources                = 128
  , maxDescriptorSetSamplers            = 96
  , maxDescriptorSetUniformBuffers      = 72
  , maxDescriptorSetStorageBuffers      = 24
  , maxDescriptorSetSampledImages       = 96
  }

-- | Each required capacity paired with a setter for the
--   @VkPhysicalDeviceVulkan12Properties@ field it names. Written out
--   longhand for the same reason 'absentOne' is: this is the test's own
--   independent statement of the mapping, so a production-side reshuffle
--   has to be reflected here deliberately. The completeness spec below
--   checks the table still covers 'requiredBindlessCapacities' exactly.
-- | Each capacity paired with setters for BOTH of its limits — the ordinary
--   one on 'PhysicalDeviceLimits' and the update-after-bind one on
--   'PhysicalDeviceVulkan12Properties'. Written out longhand for the same
--   reason 'absentOne' is: this is the test's own independent statement of
--   which struct field each side names, so a production-side reshuffle has
--   to be reflected here deliberately. The completeness spec below checks
--   the table still covers 'requiredBindlessCapacities' exactly, and that
--   each row supplies exactly the sides production says that capacity has.
capacityFields
  ∷ [( BindlessCapacity
     , Maybe (Word32 → PhysicalDeviceLimits → PhysicalDeviceLimits)
     , Maybe (Word32 → PhysicalDeviceVulkan12Properties
                     → PhysicalDeviceVulkan12Properties) )]
capacityFields =
  [ ( CapPerStageSampledImages
    , Just (\n l → l { maxPerStageDescriptorSampledImages = n })
    , Just (\n p → p { maxPerStageDescriptorUpdateAfterBindSampledImages = n }) )
  , ( CapPerStageSamplers
    , Just (\n l → l { maxPerStageDescriptorSamplers = n })
    , Just (\n p → p { maxPerStageDescriptorUpdateAfterBindSamplers = n }) )
  , ( CapPerStageStorageBuffers
    , Just (\n l → l { maxPerStageDescriptorStorageBuffers = n })
    , Just (\n p → p { maxPerStageDescriptorUpdateAfterBindStorageBuffers = n }) )
  , ( CapPerStageUniformBuffers
    , Just (\n l → l { maxPerStageDescriptorUniformBuffers = n })
    , Just (\n p → p { maxPerStageDescriptorUpdateAfterBindUniformBuffers = n }) )
  , ( CapPerStageResources
    , Just (\n l → l { maxPerStageResources = n })
    , Just (\n p → p { maxPerStageUpdateAfterBindResources = n }) )
  , ( CapSetSampledImages
    , Just (\n l → l { maxDescriptorSetSampledImages = n })
    , Just (\n p → p { maxDescriptorSetUpdateAfterBindSampledImages = n }) )
  , ( CapSetSamplers
    , Just (\n l → l { maxDescriptorSetSamplers = n })
    , Just (\n p → p { maxDescriptorSetUpdateAfterBindSamplers = n }) )
  , ( CapSetStorageBuffers
    , Just (\n l → l { maxDescriptorSetStorageBuffers = n })
    , Just (\n p → p { maxDescriptorSetUpdateAfterBindStorageBuffers = n }) )
  , ( CapSetUniformBuffers
    , Just (\n l → l { maxDescriptorSetUniformBuffers = n })
    , Just (\n p → p { maxDescriptorSetUpdateAfterBindUniformBuffers = n }) )
  , ( CapBoundDescriptorSets
    , Just (\n l → l { maxBoundDescriptorSets = n })
    , Nothing )
  , ( CapDescriptorsInAllPools
    , Nothing
    , Just (\n p → p { maxUpdateAfterBindDescriptorsInAllPools = n }) )
  ]

-- | A feature report advertising every @descriptorBinding…UpdateAfterBind@
--   feature, so every update-after-bind statement is active and the boundary
--   sweep below covers all of them. The renderer itself requires only the
--   sampled-image one — the other two are the device's own support, which is
--   what conditions VUIDs 03023/03024 and 03037/03039.
generousFeatures ∷ PhysicalDeviceVulkan12Features
generousFeatures = requiredVulkan12Features
  { descriptorBindingUniformBufferUpdateAfterBind = True
  , descriptorBindingStorageBufferUpdateAfterBind = True
  }

-- | A device that clears every bindless gate: the baseline each spec below
--   spoils in exactly one way.
capableSupport ∷ BindlessSupport
capableSupport = supportReporting generousProperties

-- | 'capableSupport' with its update-after-bind report replaced.
supportReporting ∷ PhysicalDeviceVulkan12Properties → BindlessSupport
supportReporting = supportReportingWith generousFeatures

-- | 'supportReporting' for a device advertising a specific feature set, so a
--   spec can spoil which statements apply as well as what they report.
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
  , bsCapacities  = reportedBindlessCapacities base props
  , bsMissingFeatures               = missingBindlessFeatures feats
  }

-- | The sampled-image update-after-bind setter, named so the version gates
--   can spoil that one statement without re-deriving the table.
setSampledImages ∷ Word32 → PhysicalDeviceVulkan12Properties
                          → PhysicalDeviceVulkan12Properties
setSampledImages n p =
  p { maxPerStageDescriptorUpdateAfterBindSampledImages = n }

-- | 'generousProperties' with one capacity's UPDATE-AFTER-BIND field set to
--   @n@, looked up through the same table the boundary specs sweep.
loweredTo ∷ Word32 → BindlessCapacity → PhysicalDeviceVulkan12Properties
loweredTo n cap =
  case [setField | (c, _, Just setField) ← capacityFields, c ≡ cap] of
    setField : _ → setField n generousProperties
    []           → generousProperties

-- | 'generousBaseLimits' with one capacity's ORDINARY field set to @n@.
baseLoweredTo ∷ Word32 → BindlessCapacity → PhysicalDeviceLimits
baseLoweredTo n cap =
  case [setField | (c, Just setField, _) ← capacityFields, c ≡ cap] of
    setField : _ → setField n generousBaseLimits
    []           → generousBaseLimits

-- | The diagnostic clause a shortfall must produce, stated here
--   independently of the production wording: the reported count, the field
--   the device reported it for, and the count the renderer requires.
expectedShortfallClause ∷ Text → Word32 → Word32 → Text
expectedShortfallClause field reported required =
  "the device reports " <> tshow reported <> " " <> field
    <> ", but the bindless texture array requires " <> tshow required

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

-- | The one production configuration: slot 0 held back for the undefined
--   texture ("Engine.Graphics.Vulkan.Init").
productionTextureConfig ∷ TextureSystemConfig
productionTextureConfig = TextureSystemConfig { tscReservedSlots = 1 }

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
      isBindlessSupported
        (supportReporting (setSampledImages 0 generousProperties))
          `shouldBe` False

    it "reports a pre-1.2 device on the version alone" $
      -- Below Vulkan 1.2 nothing chains a 1.2 feature struct, so the
      -- feature list is a consequence of the version gate rather than
      -- evidence: the diagnostic must not claim the driver was asked.
      bindlessShortfalls capableSupport
        { bsVulkan12OrHigher = False
        , bsMissingFeatures  = requiredBindlessFeatures
        , bsCapacities       =
            reportedBindlessCapacities zero zero
        } `shouldBe` ["Vulkan 1.2 or higher is required"]

    it "reports nothing at all for a device that clears every gate" $
      bindlessShortfalls capableSupport `shouldBe` []

    it "allocates the shader's array size, not the device-reported ceiling" $
      -- The device report is a gate, never a size: an accepted device gets
      -- the binding both shaders declare, whatever headroom it advertises
      -- above it (#1689).
      determineTextureCapability capableSupport 64
        `shouldBe` BindlessTextures maxBindlessTextures

  describe "the descriptor-count rules" $ do
    it "covers every capacity, on exactly the sides production declares" $ do
      map (\(cap, _, _) → cap) capacityFields `shouldBe` requiredBindlessCapacities
      -- A row that offers a setter production says has no statement (or
      -- omits one it does have) would silently stop testing a rule.
      forM_ capacityFields $ \(cap, base, uab) → do
        (cap, isJust base) `shouldBe` (cap, isJust (ordinaryCapacityField cap))
        (cap, isJust uab)
          `shouldBe` (cap, isJust (updateAfterBindCapacityField cap))

    it "checks BOTH statements of every paired capacity" $
      -- The contract in one assertion: an ordinary limit and its
      -- update-after-bind counterpart are simultaneous rules over different
      -- populations, so both checks are emitted and both are enforced, and
      -- neither is skipped. What the all-set check MEASURES against is the
      -- effective maximum of the pair — see "lets ordinary headroom supply
      -- the all-layout total" below.
      forM_ capacityFields $ \(cap, _, _) → do
        let scopes = map ccScope
              (bindlessCapacityChecks generousBaseLimits generousProperties cap)
            expected = catMaybes
              [ ScopeWithoutUpdateAfterBind <$ ordinaryCapacityField cap
              , ScopeAllSets <$ updateAfterBindCapacityField cap ]
        (cap, scopes) `shouldBe` (cap, expected)

    it "puts zero of the texture array against the ordinary statements" $
      -- The fact the whole split rests on. The bindless texture layout is
      -- created WITH UPDATE_AFTER_BIND_POOL_BIT, and an ordinary statement
      -- counts "only descriptors in descriptor set layouts created WITHOUT"
      -- that bit. The array therefore contributes NOTHING to an ordinary
      -- sampler or sampled-image limit: that rule is still checked, and is
      -- satisfied by zero at any reported value, so it cannot reject the
      -- layout over the array. That is about the POPULATION the statement
      -- ranges over, not about its reported value, which still serves as
      -- the effective maximum the all-set total is measured against.
      forM_ [CapPerStageSamplers, CapSetSamplers
            ,CapPerStageSampledImages, CapSetSampledImages] $ \cap → do
        (cap, layoutDescriptorsInScope ScopeWithoutUpdateAfterBind cap)
          `shouldBe` (cap, 0)
        (cap, layoutDescriptorsInScope ScopeAllSets cap)
          `shouldBe` (cap, maxBindlessTextures)

    it "counts set 0's uniform buffer under BOTH scopes" $
      -- The converse, so the split is visibly not one-directional: set 0 is
      -- an ordinary layout, so its descriptor is counted by the ordinary
      -- statement AND by the update-after-bind one.
      forM_ [CapPerStageUniformBuffers, CapSetUniformBuffers] $ \cap →
        forM_ [ScopeWithoutUpdateAfterBind, ScopeAllSets] $ \scope →
          ((cap, scope), layoutDescriptorsInScope scope cap)
            `shouldBe` ((cap, scope), 1)

    it "requires exactly the array size both fragment shaders declare" $
      -- #975's single definition, still single: the texture rules are
      -- 'maxBindlessTextures' itself, not a number that could drift from it.
      forM_ [CapPerStageSampledImages, CapPerStageSamplers
            ,CapSetSampledImages, CapSetSamplers] $ \cap →
        (cap, layoutDescriptorsInScope ScopeAllSets cap)
          `shouldBe` (cap, maxBindlessTextures)

    it "counts the fragment stage's colour attachment in the aggregate" $ do
      -- maxPerStageResources counts, in its own words, "for the fragment
      -- shader stage the framebuffer color attachments also"; the
      -- update-after-bind form is that same limit re-scoped. Stated as
      -- arithmetic rather than by re-deriving it, so an omitted term fails.
      layoutDescriptorsInScope ScopeAllSets CapPerStageResources
        `shouldBe` maxBindlessTextures + 1 + 1
      isBindlessSupported
        (supportReporting (loweredTo (maxBindlessTextures + 1) CapPerStageResources))
        `shouldBe` False
      isBindlessSupported
        (supportReporting (loweredTo (maxBindlessTextures + 2) CapPerStageResources))
        `shouldBe` True

    it "accepts a device reporting exactly what every rule requires" $ do
      -- The threshold itself, on every rule at once: the accepted side of
      -- the boundary.
      let atThreshold = foldr
            (\(cap, _, uab) props → case uab of
               Nothing       → props
               Just setField →
                 setField (layoutDescriptorsInScope ScopeAllSets cap) props)
            generousProperties capacityFields
          baseAtThreshold = foldr
            (\(cap, base, _) limits → case base of
               Nothing       → limits
               Just setField → setField
                 (layoutDescriptorsInScope ScopeWithoutUpdateAfterBind cap) limits)
            generousBaseLimits capacityFields
          support = supportReportingBase generousFeatures
                      baseAtThreshold atThreshold
      bindlessCapacityShortfalls support `shouldBe` []
      isBindlessSupported support `shouldBe` True
      planBindlessDescriptorCount support productionTextureConfig
        `shouldBe` Right maxBindlessTextures

    it "rejects a device one descriptor below any single rule" $
      -- The rejected side of the same boundary, one rule at a time — both
      -- families — so a gate silently dropped from the predicate fails here.
      -- The all-layout rule is spoiled on BOTH of its limits, since either
      -- one alone can supply that total.
      forM_ capacityFields $ \(cap, base, uab) → do
        forM_ uab $ \_ → do
          let required = layoutDescriptorsInScope ScopeAllSets cap
          when (required > 0) $ do
            let short   = required - 1
                limits  = maybe generousBaseLimits
                            (\f → f short generousBaseLimits) base
                support = supportReportingBase generousFeatures limits
                            (loweredTo short cap)
            (cap, isBindlessSupported support) `shouldBe` (cap, False)
        forM_ base $ \_ → do
          let required =
                layoutDescriptorsInScope ScopeWithoutUpdateAfterBind cap
          when (required > 0) $ do
            let support = supportReportingBase generousFeatures
                            (baseLoweredTo (required - 1) cap) generousProperties
            (cap, isBindlessSupported support) `shouldBe` (cap, False)

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

    it "names the reported and required counts of whichever rule fell short" $
      -- #1055's descriptive contract, extended to the capacity gate: device
      -- selection has to say WHICH limit came up short and by how much.
      forM_ capacityFields $ \(cap, base, uab) →
        forM_ uab $ \_ → do
          let required = layoutDescriptorsInScope ScopeAllSets cap
          when (required > 0) $ do
            let short   = required - 1
                limits  = maybe generousBaseLimits
                            (\f → f short generousBaseLimits) base
                props   = loweredTo short cap
                support = supportReportingBase generousFeatures limits props
                field   = maybe "" ccField (listToMaybe
                            [ c | c ← bindlessCapacityChecks limits props cap
                                , ccScope c ≡ ScopeAllSets ])
            (cap, deviceBindlessFailureMessage support) `shouldSatisfy`
              (T.isInfixOf (expectedShortfallClause field short required) . snd)

    it "names the same counts on the texture-system failure path" $
      -- The two sites describe one shortfall identically (#1282).
      forM_ capacityFields $ \(cap, base, uab) →
        forM_ uab $ \_ → do
          let required = layoutDescriptorsInScope ScopeAllSets cap
          when (required > 0) $ do
            let short   = required - 1
                limits  = maybe generousBaseLimits
                            (\f → f short generousBaseLimits) base
                props   = loweredTo short cap
                support = supportReportingBase generousFeatures limits props
                field   = maybe "" ccField (listToMaybe
                            [ c | c ← bindlessCapacityChecks limits props cap
                                , ccScope c ≡ ScopeAllSets ])
            case planBindlessDescriptorCount support productionTextureConfig of
              Right count → expectationFailure $
                "a device short on " <> T.unpack field <> " was accepted with "
                  <> show count <> " descriptors"
              Left failure → (cap, failure) `shouldSatisfy`
                (T.isInfixOf (expectedShortfallClause field short required) . snd)

    it "lets ordinary headroom supply the all-layout total" $
      -- The all-layout total is measured against the EFFECTIVE capacity —
      -- the greater of a paired class's two limits — so an update-after-bind
      -- figure one below the requirement is not on its own a refusal when
      -- the ordinary limit covers it.
      forM_ capacityFields $ \(cap, base, uab) →
        case (base, uab) of
          (Just setBase, Just setUab) → do
            let required = layoutDescriptorsInScope ScopeAllSets cap
                support  = supportReportingBase generousFeatures
                             (setBase required generousBaseLimits)
                             (setUab (required - 1) generousProperties)
            (cap, isBindlessSupported support) `shouldBe` (cap, True)
          _ → pure ()

    it "still refuses when neither of a pair supplies the total" $
      -- The rejected side of that same rule: ordinary headroom rescues a
      -- shortfall, absent headroom does not.
      forM_ capacityFields $ \(cap, base, uab) →
        case (base, uab) of
          (Just setBase, Just setUab) → do
            let required = layoutDescriptorsInScope ScopeAllSets cap
                short    = required - 1
                support  = supportReportingBase generousFeatures
                             (setBase short generousBaseLimits)
                             (setUab short generousProperties)
            (cap, isBindlessSupported support) `shouldBe` (cap, False)
          _ → pure ()

    it "keeps the ordinary-only check on its own smaller population" $
      -- Retained rather than folded into the maximum: it constrains the
      -- layout's non-update-after-bind set, and no update-after-bind
      -- headroom answers for it.
      forM_ capacityFields $ \(cap, base, _) →
        forM_ base $ \setBase → do
          let required = layoutDescriptorsInScope ScopeWithoutUpdateAfterBind cap
          when (required > 0) $ do
            let support = supportReportingBase generousFeatures
                            (setBase (required - 1) generousBaseLimits)
                            generousProperties
            (cap, isBindlessSupported support) `shouldBe` (cap, False)

  describe "update-after-bind limits and device features" $ do
    it "enforces every update-after-bind limit whatever the features say" $
      -- The layout puts descriptors of each class against its limit however
      -- the device advertises its descriptorBinding…UpdateAfterBind
      -- features, so no feature boolean suppresses a rule. A device
      -- reporting zero on BOTH sides of a uniform- or storage-buffer pair —
      -- nothing for the effective maximum to draw on — is refused even with
      -- every optional feature off.
      forM_ [CapPerStageUniformBuffers, CapSetUniformBuffers
            ,CapPerStageStorageBuffers, CapSetStorageBuffers] $ \cap →
        forM_ [zero ∷ PhysicalDeviceVulkan12Features
              ,requiredVulkan12Features, generousFeatures] $ \feats → do
          -- Both limits at zero, so the effective capacity cannot be
          -- supplied from either side and only the feature report varies.
          let support = supportReportingBase feats (baseLoweredTo 0 cap)
                          (loweredTo 0 cap)
          (cap, isBindlessSupported support) `shouldBe` (cap, False)

    it "emits both rules for every paired capacity, features regardless" $
      -- The check list is a function of the layout, not of the device's
      -- feature report: both statements are present for every paired class.
      forM_ capacityFields $ \(cap, _, _) → do
        let scopes = map ccScope
              (bindlessCapacityChecks generousBaseLimits generousProperties cap)
            expected = catMaybes
              [ ScopeWithoutUpdateAfterBind <$ ordinaryCapacityField cap
              , ScopeAllSets <$ updateAfterBindCapacityField cap ]
        (cap, scopes) `shouldBe` (cap, expected)

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
