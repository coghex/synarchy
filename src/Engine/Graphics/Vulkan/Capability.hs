{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Capability
  ( BindlessSupport(..)
  , TextureSystemCapability(..)
  , queryBindlessSupport
  , determineTextureCapability
  , describeCapability
  , isBindlessSupported
  , bindlessShortfalls
  , unsupportedBindlessMessage
  , deviceBindlessFailureMessage
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Graphics.Vulkan.Texture.Requirements
  (BindlessFeature, bindlessFeatureField, missingBindlessFeatures
  ,requiredBindlessFeatures)
import Vulkan.Core10
import Vulkan.Core11 (getPhysicalDeviceProperties2, getPhysicalDeviceFeatures2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  (PhysicalDeviceProperties2(..), PhysicalDeviceFeatures2(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Features, PhysicalDeviceVulkan12Properties(..))
import Vulkan.CStruct.Extends

-- | Detailed bindless texture support information
data BindlessSupport = BindlessSupport
  { bsVulkan12OrHigher                    ∷ Bool
  , bsMaxSampledImagesPerStage            ∷ Word32  -- Base limit (128)
  , bsMaxDescriptorSetSampledImages       ∷ Word32  -- Base limit (640)
  -- The UPDATE_AFTER_BIND limits - these are the real bindless limits!
  , bsMaxUpdateAfterBindSampledImages     ∷ Word32  -- Should be 1,000,000
  -- | Which of 'requiredBindlessFeatures' the device does NOT advertise
  --   (#1282). Empty on a device that can run the bindless renderer.
  , bsMissingFeatures                     ∷ [BindlessFeature]
  } deriving (Show, Eq)

-- | What texture system capability we'll use
data TextureSystemCapability
  = BindlessTextures Word32
  | BoundedTextureArray Word32
  deriving (Show, Eq)

-- | Query device for bindless support
-- Must query Vulkan 1.2 properties to get UpdateAfterBind limits, and the
-- Vulkan 1.2 feature booleans the layout and shaders require (#1282) —
-- without those the unsupported-bindless branch cannot see a device that
-- would later fail 'createDevice' with @VK_ERROR_FEATURE_NOT_PRESENT@.
queryBindlessSupport ∷ PhysicalDevice → IO BindlessSupport
queryBindlessSupport pDevice = do
  props ← getPhysicalDeviceProperties pDevice
  let PhysicalDeviceProperties { apiVersion = version, limits = deviceLimits } = props
      major = fromIntegral $ (version `shiftR` 22) ⌃ 0x7F ∷ Int
      minor = fromIntegral $ (version `shiftR` 12) ⌃ 0x3FF ∷ Int
      isVulkan12OrHigher = major > 1 ∨ (major ≡ 1 ∧ minor ≥ 2)

  -- Below Vulkan 1.2 there is no 1.2 struct to chain onto either query: the
  -- device is unsupported on version alone, and every required feature counts
  -- as missing without ever asking the driver about it.
  (props12, missing) ← if isVulkan12OrHigher
    then do
      PhysicalDeviceProperties2 { next = (vk12Props :& ()) }
        ← getPhysicalDeviceProperties2 pDevice
          ∷ IO (PhysicalDeviceProperties2 '[PhysicalDeviceVulkan12Properties])
      PhysicalDeviceFeatures2 { next = (vk12Feats :& ()) }
        ← getPhysicalDeviceFeatures2 pDevice
          ∷ IO (PhysicalDeviceFeatures2 '[PhysicalDeviceVulkan12Features])
      let PhysicalDeviceVulkan12Properties
            { maxPerStageDescriptorUpdateAfterBindSampledImages = maxBindless
            } = vk12Props
      pure (maxBindless, missingBindlessFeatures vk12Feats)
    else return (0, requiredBindlessFeatures)

  pure $ BindlessSupport
    { bsVulkan12OrHigher = isVulkan12OrHigher
    , bsMaxSampledImagesPerStage = maxPerStageDescriptorSampledImages deviceLimits
    , bsMaxDescriptorSetSampledImages = maxDescriptorSetSampledImages deviceLimits
    , bsMaxUpdateAfterBindSampledImages = props12
    , bsMissingFeatures = missing
    }

-- | Check if full bindless is supported
isBindlessSupported ∷ BindlessSupport → Bool
isBindlessSupported bs = bsVulkan12OrHigher bs
                       ∧ null (bsMissingFeatures bs)
                       ∧ bsMaxUpdateAfterBindSampledImages bs > 0

-- | Why a device falls short of the renderer's bindless requirement, in the
--   order the gates apply. Empty exactly when 'isBindlessSupported' holds.
--   A device below Vulkan 1.2 reports only that: the feature and limit
--   findings below it are consequences of the version gate, not independent
--   evidence (nothing was queried to produce them).
bindlessShortfalls ∷ BindlessSupport → [Text]
bindlessShortfalls bs
  | not (bsVulkan12OrHigher bs) = ["Vulkan 1.2 or higher is required"]
  | otherwise = featureShortfall <> limitShortfall
  where
    featureShortfall = case bsMissingFeatures bs of
      []      → []
      missing → ["missing required descriptor-indexing feature(s): "
                  <> T.intercalate ", " (map bindlessFeatureField missing)]
    limitShortfall =
      ["the device reports no update-after-bind sampled-image descriptors"
      | bsMaxUpdateAfterBindSampledImages bs ≡ 0]

-- | The renderer's one bindless-required failure message — #1055's
--   diagnostic contract, factored in #1282 so device selection
--   ("Engine.Graphics.Vulkan.Device") and texture-system creation
--   ("Engine.Graphics.Vulkan.Texture.System") cannot describe the same
--   shortfall differently. The parenthetical is omitted when the device
--   clears every capability gate and only the post-reservation slot budget
--   came up short, which has no shortfall to name.
unsupportedBindlessMessage ∷ BindlessSupport → TextureSystemCapability → Text
unsupportedBindlessMessage support capability =
  "Bindless textures are required, but this device does not meet the \
  \renderer's required bindless capability: "
  <> describeCapability capability
  <> case bindlessShortfalls support of
       []     → ""
       missed → " (" <> T.intercalate "; " missed <> ")"

-- | 'unsupportedBindlessMessage' for a caller with no slot reservations to
--   account for: device selection runs before the texture system exists, so
--   the only question it can answer is whether the device could do bindless
--   at all.
deviceBindlessFailureMessage ∷ BindlessSupport → Text
deviceBindlessFailureMessage support =
  unsupportedBindlessMessage support (determineTextureCapability support 0)

-- | Determine what texture system to use based on support
determineTextureCapability ∷ BindlessSupport → Word32 → TextureSystemCapability
determineTextureCapability support reservedSlots =
  -- UpdateAfterBind limit is the real bindless limit, not the base one
  let maxSlots = bsMaxUpdateAfterBindSampledImages support
      availableSlots = if maxSlots > reservedSlots 
                       then maxSlots - reservedSlots 
                       else 0
      cappedSlots = min availableSlots 1000000
      worthIt = cappedSlots ≥ 256
      
  in if isBindlessSupported support ∧ worthIt
     then BindlessTextures cappedSlots
     else BoundedTextureArray 256

-- | Human-readable description of capability
describeCapability ∷ TextureSystemCapability → Text
describeCapability (BindlessTextures n) = 
  "Bindless textures enabled (max " <> T.pack (show n) <> " slots)"
describeCapability (BoundedTextureArray n) = 
  "Bounded texture array fallback (" <> T.pack (show n) <> " textures)"
