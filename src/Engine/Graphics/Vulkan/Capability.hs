{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Capability
  ( BindlessSupport(..)
  , TextureSystemCapability(..)
  , queryBindlessSupport
  , determineTextureCapability
  , isBindlessSupported
  , minimumUsableSlots
  , bindlessCapacityShortfalls
  , bindlessShortfalls
  , unsupportedBindlessMessage
  , deviceBindlessFailureMessage
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Graphics.Vulkan.Texture.Limits (maxBindlessTextures)
import Engine.Graphics.Vulkan.Texture.Requirements
  (BindlessCapacity, BindlessFeature, bindlessCapacityField
  ,bindlessCapacityRequirement, bindlessFeatureField, missingBindlessFeatures
  ,reportedBindlessCapacities, requiredBindlessCapacities
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
  -- | What the device reports for each capacity that APPLIES to it — the
  --   UPDATE_AFTER_BIND limits governing the bindless pipeline layout and
  --   pool. These are gates, not budgets: the texture array is fixed-size,
  --   so a device supplies every applicable one in full or cannot run this
  --   renderer at all (#1689). Both families are here: the ordinary
  --   'Vulkan.Core10.PhysicalDeviceLimits' statements that govern the
  --   layout's non-update-after-bind set, and the update-after-bind ones
  --   that govern all of it. A capacity whose Valid Usage statement the
  --   device's features do not activate is ABSENT here rather than zero
  --   ('bindlessCapacityApplies'), so it cannot refuse a device Vulkan
  --   would accept.
  , bsCapacities                          ∷ [(BindlessCapacity, Word32)]
  -- | Which of 'requiredBindlessFeatures' the device does NOT advertise
  --   (#1282). Empty on a device that can run the bindless renderer.
  , bsMissingFeatures                     ∷ [BindlessFeature]
  } deriving (Show, Eq)

-- | What texture system capability we'll use. 'BindlessTextures' carries
--   the descriptor count of the combined-image-sampler binding, which is
--   always 'maxBindlessTextures' — see 'determineTextureCapability'.
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
  (capacities, missing) ← if isVulkan12OrHigher
    then do
      PhysicalDeviceProperties2 { next = (vk12Props :& ()) }
        ← getPhysicalDeviceProperties2 pDevice
          ∷ IO (PhysicalDeviceProperties2 '[PhysicalDeviceVulkan12Properties])
      PhysicalDeviceFeatures2 { next = (vk12Feats :& ()) }
        ← getPhysicalDeviceFeatures2 pDevice
          ∷ IO (PhysicalDeviceFeatures2 '[PhysicalDeviceVulkan12Features])
      pure ( reportedBindlessCapacities vk12Feats deviceLimits vk12Props
           , missingBindlessFeatures vk12Feats )
    else return (unqueriedCapacities, requiredBindlessFeatures)

  pure $ BindlessSupport
    { bsVulkan12OrHigher = isVulkan12OrHigher
    , bsMaxSampledImagesPerStage = maxPerStageDescriptorSampledImages deviceLimits
    , bsMaxDescriptorSetSampledImages = maxDescriptorSetSampledImages deviceLimits
    , bsCapacities = capacities
    , bsMissingFeatures = missing
    }

-- | Every required capacity at zero: what a pre-1.2 device reports, in the
--   sense that nothing was measured, so nothing is available. Listing them
--   all is the fail-closed choice; the version gate refuses such a device
--   before any capacity is consulted either way.
unqueriedCapacities ∷ [(BindlessCapacity, Word32)]
unqueriedCapacities = [ (cap, 0) | cap ← requiredBindlessCapacities ]

-- | Every APPLICABLE update-after-bind capacity the device reports below
--   what the fixed-size bindless pipeline layout consumes, each naming the
--   reported count and the count the renderer requires (#1689). Empty on a
--   device that can build the binding at its declared size.
--
--   Driven by 'bsCapacities' rather than 'requiredBindlessCapacities': a
--   capacity whose Valid Usage statement this device's features leave
--   inactive is absent from that list precisely so it cannot produce a
--   shortfall here.
bindlessCapacityShortfalls ∷ BindlessSupport → [Text]
bindlessCapacityShortfalls support =
  [ "the device reports " <> tshow reported <> " " <> bindlessCapacityField cap
      <> ", but the bindless texture array requires " <> tshow required
  | (cap, reported) ← bsCapacities support
  , let required = bindlessCapacityRequirement cap
  , reported < required
  ]

-- | Check if full bindless is supported. The capacity gate is an exact
--   requirement, not a floor to allocate down from: both fragment shaders
--   declare @textures[maxBindlessTextures]@, so a device that cannot supply
--   that whole binding cannot run this renderer (#1689).
isBindlessSupported ∷ BindlessSupport → Bool
isBindlessSupported bs = bsVulkan12OrHigher bs
                       ∧ null (bsMissingFeatures bs)
                       ∧ null (bindlessCapacityShortfalls bs)

-- | Why a device falls short of the renderer's bindless requirement, in the
--   order the gates apply. Empty exactly when 'isBindlessSupported' holds.
--   A device below Vulkan 1.2 reports only that: the feature and capacity
--   findings below it are consequences of the version gate, not independent
--   evidence (nothing was queried to produce them).
bindlessShortfalls ∷ BindlessSupport → [Text]
bindlessShortfalls bs
  | not (bsVulkan12OrHigher bs) = ["Vulkan 1.2 or higher is required"]
  | otherwise = featureShortfall <> bindlessCapacityShortfalls bs
  where
    featureShortfall = case bsMissingFeatures bs of
      []      → []
      missing → ["missing required descriptor-indexing feature(s): "
                  <> T.intercalate ", " (map bindlessFeatureField missing)]

-- | The renderer's one bindless-required failure message — #1055's
--   diagnostic contract, factored in #1282 so device selection
--   ("Engine.Graphics.Vulkan.Device") and texture-system creation
--   ("Engine.Graphics.Vulkan.Texture.System") cannot describe the same
--   shortfall differently. The parenthetical always names a cause: a device
--   that clears every capability gate can only have been refused for the
--   post-reservation slot budget, so that case names itself rather than
--   leaving the rejection undescribed (#1689).
unsupportedBindlessMessage ∷ BindlessSupport → TextureSystemCapability → Text
unsupportedBindlessMessage support capability =
  "Bindless textures are required, but this device does not meet the \
  \renderer's required bindless capability: "
  <> describeCapability capability
  <> " (" <> T.intercalate "; " causes <> ")"
  where
    causes = case bindlessShortfalls support of
      []     → [reservationShortfall]
      missed → missed
    reservationShortfall =
      "the renderer's slot reservations leave fewer than "
        <> tshow minimumUsableSlots <> " of the binding's "
        <> tshow maxBindlessTextures
        <> " descriptors for application textures"

-- | 'unsupportedBindlessMessage' for a caller with no slot reservations to
--   account for: device selection runs before the texture system exists, so
--   the only question it can answer is whether the device could do bindless
--   at all.
deviceBindlessFailureMessage ∷ BindlessSupport → Text
deviceBindlessFailureMessage support =
  unsupportedBindlessMessage support (determineTextureCapability support 0)

-- | Fewest application-usable slots worth standing a bindless system up
--   for. Distinct from the descriptor COUNT: the reservations are indices
--   INSIDE the fixed binding ("Engine.Graphics.Vulkan.Texture.Slot" holds
--   back index 0 for the undefined texture), so they never shrink it.
minimumUsableSlots ∷ Word32
minimumUsableSlots = 256

-- | Determine what texture system to use based on support.
--
--   'BindlessTextures' always carries 'maxBindlessTextures' — the size both
--   fragment shaders declare their @textures[]@ array at, and therefore the
--   only descriptor count that satisfies the descriptor-set interface rule
--   without @runtimeDescriptorArray@ (#1689). The device report decides
--   whether that binding can be built at all ('isBindlessSupported'), never
--   how big it is.
--
--   @reservedSlots@ is a separate question about the same binding: how many
--   of its indices the texture system holds back, leaving the rest for
--   application textures.
determineTextureCapability ∷ BindlessSupport → Word32 → TextureSystemCapability
determineTextureCapability support reservedSlots =
  let usableSlots = if maxBindlessTextures > reservedSlots
                    then maxBindlessTextures - reservedSlots
                    else 0
  in if isBindlessSupported support ∧ usableSlots ≥ minimumUsableSlots
     then BindlessTextures maxBindlessTextures
     else BoundedTextureArray minimumUsableSlots

-- | Human-readable description of capability
describeCapability ∷ TextureSystemCapability → Text
describeCapability (BindlessTextures n) = 
  "Bindless textures enabled (max " <> tshow n <> " slots)"
describeCapability (BoundedTextureArray n) = 
  "Bounded texture array fallback (" <> tshow n <> " textures)"
