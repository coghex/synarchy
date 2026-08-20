-- | The Vulkan instance-configuration decision, as a pure function
--   (#1402).
--
--   'Engine.Graphics.Vulkan.Instance.createVulkanInstance' does three IO
--   reads — GLFW's required extensions, the available instance
--   extensions, the available instance layers — and then decides
--   everything else from those three lists and the 'GraphicsConfig'.
--   That decision is what this module owns.
--
--   It exists for the reason
--   "Engine.Graphics.Vulkan.Texture.Requirements" does (#1282): the
--   contract was only observable through a real driver, so the graphical
--   suite pinned what one particular machine happened to offer rather
--   than what production actually requires. Commit @5e54a5ff@ made four
--   extensions optional across @src/@ and touched no test, and the spec
--   went on asserting that @VK_KHR_portability_enumeration@ and
--   @VK_KHR_get_physical_device_properties2@ were always present — an
--   assumption a valid Vulkan installation is free to break.
--
--   Everything below is a pure function of three @[ByteString]@ lists,
--   so a headless spec can pin the whole contract with no GPU, driver,
--   window, or display. Only the GLFW surface extensions are hard
--   requirements, and only for 'InstanceForWindow'; every other
--   capability degrades when absent, because enabling an absent
--   extension fails instance creation with @EXTENSION_NOT_PRESENT@.
module Engine.Graphics.Vulkan.Instance.Plan
  ( InstanceSurfaceUse(..)
  , InstancePlan(..)
  , InstancePlanError(..)
  , planVulkanInstance
  , validationLayerName
  ) where

import UPrelude
import qualified Data.ByteString as BS
import Engine.Graphics.Base (GraphicsConfig(..))
import Vulkan.Core10.Enums.InstanceCreateFlagBits
  (InstanceCreateFlags
  ,InstanceCreateFlagBits(INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR))
import Vulkan.Extensions.VK_EXT_debug_utils
  (pattern EXT_DEBUG_UTILS_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_layer_settings
  (pattern EXT_LAYER_SETTINGS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_portability_enumeration
  (pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
  (pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME)
import Vulkan.Zero (zero)

-- | Whether the instance must be able to present to a window surface.
--   Windowed modes carry the GLFW surface extensions as hard
--   requirements; the offscreen mode (#650) renders to plain images —
--   no surface support, and GLFW may not even be initialized, so it
--   must not be asked for extensions.
data InstanceSurfaceUse = InstanceForWindow | InstanceOffscreen
  deriving (Eq, Show)

-- | Everything 'Engine.Graphics.Vulkan.Instance.createVulkanInstance'
--   needs in order to build the @InstanceCreateInfo@ and its pNext
--   chain. The two list fields are ORDERED: they are handed to Vulkan
--   in exactly this order, so a spec comparing them compares the real
--   configuration rather than a set that happens to have the same
--   members.
data InstancePlan = InstancePlan
  { ipEnabledExtensions ∷ [BS.ByteString]
    -- ^ @enabledExtensionNames@, in order.
  , ipEnabledLayers     ∷ [BS.ByteString]
    -- ^ @enabledLayerNames@, in order.
  , ipCreateFlags       ∷ InstanceCreateFlags
    -- ^ @flags@ — the portability bit, or 'zero'.
  , ipDebugMessenger    ∷ Bool
    -- ^ Whether @VK_EXT_debug_utils@ is enabled: the messenger create
    --   info is chained onto the instance AND a messenger is created
    --   afterwards. False whenever debug mode is off OR the extension
    --   is unavailable.
  , ipValidationLayer   ∷ Bool
    -- ^ Whether @VK_LAYER_KHRONOS_validation@ is enabled. Gated
    --   INDEPENDENTLY of 'ipDebugMessenger': a machine can offer the
    --   layer without the extension, or the extension without the
    --   layer.
  , ipLayerSettings     ∷ Bool
    -- ^ Whether @VK_EXT_layer_settings@ is enabled, and therefore
    --   whether @LayerSettingsCreateInfoEXT@ is chained. Both structs
    --   in the pNext chain require their own extension to be enabled
    --   (spec VUs), which is why this tracks the extension rather than
    --   a separate intent.
  } deriving (Eq, Show)

-- | Why no instance can be configured. The GLFW surface extensions are
--   the only hard requirement production has, so this is the only way
--   planning fails.
newtype InstancePlanError
  = MissingRequiredExtensions [BS.ByteString]
    -- ^ Surface extensions GLFW requires that the driver does not offer.
  deriving (Eq, Show)

-- | The validation layer enabled in debug mode when the platform has it.
validationLayerName ∷ BS.ByteString
validationLayerName = "VK_LAYER_KHRONOS_validation"

-- | Decide the whole instance configuration from the config, the surface
--   use, and the three discovered lists.
--
--   The GLFW list is a hard requirement under 'InstanceForWindow' and is
--   ignored entirely under 'InstanceOffscreen' — neither required nor
--   enabled — which is what lets that mode boot without GLFW ever being
--   initialized.
planVulkanInstance
  ∷ GraphicsConfig
  → InstanceSurfaceUse
  → [BS.ByteString]  -- ^ Extensions GLFW requires for a window surface.
  → [BS.ByteString]  -- ^ Available instance extensions.
  → [BS.ByteString]  -- ^ Available instance layers.
  → Either InstancePlanError InstancePlan
planVulkanInstance config surfaceUse glfwExts availableExts availableLayers
  | not (null missingExts) = Left $ MissingRequiredExtensions missingExts
  | otherwise = Right InstancePlan
      { ipEnabledExtensions = surfaceExts
          <> [EXT_DEBUG_UTILS_EXTENSION_NAME             | debugEnabled]
          <> [KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME | hasPortability]
          <> [KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME | hasProps2]
          <> [EXT_LAYER_SETTINGS_EXTENSION_NAME          | hasLayerSettings]
      , ipEnabledLayers = [validationLayerName | validationEnabled]
      , ipCreateFlags = if hasPortability
                        then INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
                        else zero
      , ipDebugMessenger  = debugEnabled
      , ipValidationLayer = validationEnabled
      , ipLayerSettings   = hasLayerSettings
      }
  where
    surfaceExts = case surfaceUse of
      InstanceForWindow → glfwExts
      InstanceOffscreen → []
    missingExts = filter (not ∘ (`elem` availableExts)) surfaceExts

    hasPortability   = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME `elem` availableExts
    hasLayerSettings = EXT_LAYER_SETTINGS_EXTENSION_NAME `elem` availableExts
    hasProps2        = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME `elem` availableExts
    hasDebugUtils    = EXT_DEBUG_UTILS_EXTENSION_NAME `elem` availableExts

    debugEnabled      = gcDebugMode config ∧ hasDebugUtils
    validationEnabled = gcDebugMode config ∧ validationLayerName `elem` availableLayers
