{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Capability
  ( BindlessSupport(..)
  , TextureSystemCapability(..)
  , queryBindlessSupport
  , determineTextureCapability
  , describeCapability
  , isBindlessSupported
  ) where

import UPrelude
import Data.Bits (shiftR, (.&.))
import qualified Data.Text as T
import Vulkan.Core10
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceProperties(..))
import Vulkan.Core11 (getPhysicalDeviceProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Properties(..))
import Vulkan.CStruct.Extends
import Vulkan.Zero (zero)

-- | Detailed bindless texture support information
data BindlessSupport = BindlessSupport
  { bsVulkan12OrHigher                    ∷ Bool
  , bsMaxSampledImagesPerStage            ∷ Word32  -- Base limit (128)
  , bsMaxDescriptorSetSampledImages       ∷ Word32  -- Base limit (640)
  -- The UPDATE_AFTER_BIND limits - these are the real bindless limits!
  , bsMaxUpdateAfterBindSampledImages     ∷ Word32  -- Should be 1,000,000
  } deriving (Show, Eq)

-- | What texture system capability we'll use
data TextureSystemCapability
  = BindlessTextures Word32
  | BoundedTextureArray Word32
  deriving (Show, Eq)

-- | Query device for bindless support
-- Must query Vulkan 1.2 properties to get UpdateAfterBind limits
queryBindlessSupport ∷ PhysicalDevice → IO BindlessSupport
queryBindlessSupport pDevice = do
  props ← getPhysicalDeviceProperties pDevice
  let PhysicalDeviceProperties { apiVersion = version, limits = deviceLimits } = props
      major = fromIntegral $ (version `shiftR` 22) .&. 0x7F ∷ Int
      minor = fromIntegral $ (version `shiftR` 12) .&. 0x3FF ∷ Int
      isVulkan12OrHigher = major > 1 ∨ (major ≡ 1 ∧ minor ≥ 2)

  props12 ← if isVulkan12OrHigher
    then do
      PhysicalDeviceProperties2 { next = (vk12Props :& ()) }
        ← getPhysicalDeviceProperties2 pDevice
          ∷ IO (PhysicalDeviceProperties2 '[PhysicalDeviceVulkan12Properties])
      let PhysicalDeviceVulkan12Properties
            { maxPerStageDescriptorUpdateAfterBindSampledImages = maxBindless
            } = vk12Props
      pure maxBindless
    else return 0

  pure $ BindlessSupport
    { bsVulkan12OrHigher = isVulkan12OrHigher
    , bsMaxSampledImagesPerStage = maxPerStageDescriptorSampledImages deviceLimits
    , bsMaxDescriptorSetSampledImages = maxDescriptorSetSampledImages deviceLimits
    , bsMaxUpdateAfterBindSampledImages = props12
    }

-- | Check if full bindless is supported
isBindlessSupported ∷ BindlessSupport → Bool
isBindlessSupported bs = bsVulkan12OrHigher bs ∧ bsMaxUpdateAfterBindSampledImages bs > 0

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
