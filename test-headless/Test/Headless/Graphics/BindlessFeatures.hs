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
--   the device enables, the feature/version/limit gates that classify a
--   device unsupported, the layout flags each required feature justifies,
--   and the selection rule that keeps a capable candidate ahead of an
--   incapable one whatever their device types.
module Test.Headless.Graphics.BindlessFeatures (spec) where

import UPrelude
import qualified Data.Text as T
import Test.Hspec
import Engine.Graphics.Vulkan.Capability
  (BindlessSupport(..), TextureSystemCapability(..), bindlessShortfalls
  ,determineTextureCapability, isBindlessSupported, unsupportedBindlessMessage)
import Engine.Graphics.Vulkan.Device (bindlessCapableBonus, scoreDevice)
import Engine.Graphics.Vulkan.Texture.Requirements
  (BindlessFeature(..), bindlessFeatureField, bindlessTextureBindingFlags
  ,bindlessTextureBindingRequirements, enableBindlessFeature
  ,missingBindlessFeatures, readBindlessFeature, requiredBindlessFeatures
  ,requiredVulkan12Features)
import Vulkan.Core10.Enums.PhysicalDeviceType (PhysicalDeviceType(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Features(..))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits
  (DescriptorBindingFlagBits(..))
import Vulkan.Zero (zero)

-- | A device that clears every bindless gate: the baseline each spec below
--   spoils in exactly one way.
capableSupport ∷ BindlessSupport
capableSupport = BindlessSupport
  { bsVulkan12OrHigher                = True
  , bsMaxSampledImagesPerStage        = 128
  , bsMaxDescriptorSetSampledImages   = 640
  , bsMaxUpdateAfterBindSampledImages = 1000000
  , bsMissingFeatures                 = []
  }

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
      isBindlessSupported capableSupport { bsMaxUpdateAfterBindSampledImages = 0 }
        `shouldBe` False

    it "reports a pre-1.2 device on the version alone" $
      -- Below Vulkan 1.2 nothing chains a 1.2 feature struct, so the
      -- feature list is a consequence of the version gate rather than
      -- evidence: the diagnostic must not claim the driver was asked.
      bindlessShortfalls capableSupport
        { bsVulkan12OrHigher = False
        , bsMissingFeatures  = requiredBindlessFeatures
        , bsMaxUpdateAfterBindSampledImages = 0
        } `shouldBe` ["Vulkan 1.2 or higher is required"]

    it "reports nothing at all for a device that clears every gate" $
      bindlessShortfalls capableSupport `shouldBe` []

    it "still allocates the device-reported slot count when supported" $
      determineTextureCapability capableSupport 64
        `shouldBe` BindlessTextures (1000000 - 64)

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
