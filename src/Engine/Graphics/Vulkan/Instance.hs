{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Engine.Graphics.Vulkan.Instance
  ( createVulkanInstance
  , destroyVulkanInstance
  , getAvailableExtensions
  , InstanceSurfaceUse(..)
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM, logWarnM)
import Engine.Core.Error.Exception (ExceptionType(..), InitError(..))
import Engine.Core.Monad
import Engine.Graphics.Base
import qualified Engine.Graphics.Window.GLFW as GLFW
import Foreign.Marshal.Utils (with)
import Engine.Graphics.Vulkan.Instance.Plan
  (InstancePlan(..), InstancePlanError(..), InstanceSurfaceUse(..)
  ,planVulkanInstance)
import Vulkan.Core10
import Vulkan.Core12
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_layer_settings
import Vulkan.Extensions.VK_KHR_portability_enumeration
  (pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME)
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Zero

-- | Create and initialize Vulkan instance with optional debug messenger.
--   Optional extensions (portability enumeration, layer settings, debug
--   utils) and the validation layer are enabled only when the platform
--   actually offers them — enabling an absent extension fails instance
--   creation with EXTENSION_NOT_PRESENT (e.g. the MoltenVK-only ones on
--   Linux). Only the GLFW surface extensions are hard requirements, and
--   only for 'InstanceForWindow'.
createVulkanInstance ∷ GraphicsConfig → InstanceSurfaceUse
                     → EngineM σ (Instance, Maybe DebugUtilsMessengerEXT)
createVulkanInstance config surfaceUse = do
  logDebugM CatVulkan "Initializing Vulkan instance"

  -- InstanceOffscreen must not ASK GLFW for extensions at all: in that
  -- mode GLFW may never have been initialized. 'planVulkanInstance'
  -- separately ignores the list for that mode, so the two agree.
  glfwExts ← case surfaceUse of
    InstanceForWindow → GLFW.getRequiredInstanceExtensions
    InstanceOffscreen → pure []
  (_, exts) ← liftIO $ enumerateInstanceExtensionProperties Nothing
  let availableExts = map extensionName $ V.toList exts
  (_, layers) ← liftIO enumerateInstanceLayerProperties
  let availableLayers = map (\LayerProperties{layerName = ln} → ln) $ V.toList layers

  -- Everything from here is decided purely (#1402); this function only
  -- reports the decision and executes it.
  plan ← case planVulkanInstance config surfaceUse glfwExts
                                 availableExts availableLayers of
    Left (MissingRequiredExtensions missingExts) →
      logAndThrowM CatInit (ExInit ExtensionNotSupported) $
        "Required extensions not available: " <> T.pack (show missingExts)
    Right plan → pure plan

  let debugEnabled     = ipDebugMessenger plan
      hasLayerSettings = ipLayerSettings plan
      hasPortability   = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
                           `elem` ipEnabledExtensions plan

  when (gcDebugMode config ∧ not (ipValidationLayer plan)) $
    logWarnM CatVulkan
      "Debug mode: VK_LAYER_KHRONOS_validation not installed, validation disabled"
  when (gcDebugMode config ∧ not debugEnabled) $
    logWarnM CatVulkan
      "Debug mode: VK_EXT_debug_utils not available, debug messenger disabled"

  logDebugSM CatVulkan "Instance configuration"
    [("glfw_extensions", T.pack $ show $ map BSU.toString glfwExts)
    ,("debug_mode", T.pack $ show $ gcDebugMode config)
    ,("validation", T.pack $ show $ ipValidationLayer plan)
    ,("portability", T.pack $ show hasPortability)
    ,("layer_settings", T.pack $ show hasLayerSettings)]

  inst ← liftIO $ with (1 ∷ Word32) $ \valuePtr → do
    let moltenVkSetting = LayerSettingEXT
          { layerName = "MoltenVK"
          , settingName = "MVK_CONFIG_USE_METAL_ARGUMENT_BUFFERS"
          , type' = LAYER_SETTING_TYPE_UINT32_EXT
          , valueCount = 1
          , values = castPtr valuePtr
          }

        layerSettingsInfo = LayerSettingsCreateInfoEXT
          { settings = V.singleton moltenVkSetting
          }

        baseCreateInfo = zero
          { applicationInfo = Just $ zero
              { applicationName = Just $ BSU.fromString $ T.unpack $ gcAppName config
              , engineName     = Just "Synarchy Engine"
              , apiVersion     = API_VERSION_1_2
              }
          , enabledLayerNames     = V.fromList $ ipEnabledLayers plan
          , enabledExtensionNames = V.fromList $ ipEnabledExtensions plan
          , flags = ipCreateFlags plan
          } ∷ InstanceCreateInfo '[]

    -- The pNext chain is type-indexed, so each shape is its own branch.
    -- Both structs require their extension to be enabled (spec VUs).
    case (hasLayerSettings, debugEnabled) of
      (True, True)   → createInstance
        (baseCreateInfo ::& layerSettingsInfo :& debugUtilsMessengerCreateInfo :& ()) Nothing
      (True, False)  → createInstance
        (baseCreateInfo ::& layerSettingsInfo :& ()) Nothing
      (False, True)  → createInstance
        (baseCreateInfo ::& debugUtilsMessengerCreateInfo :& ()) Nothing
      (False, False) → createInstance baseCreateInfo Nothing

  logDebugM CatVulkan "Vulkan instance created successfully"

  dbgMessenger ← if debugEnabled
    then do
      logDebugM CatVulkan "Creating debug messenger"
      messenger ← createDebugUtilsMessengerEXT inst debugUtilsMessengerCreateInfo Nothing
        `catchError` \err → logAndThrowM CatInit (ExInit VulkanInitFailed) $
          "Failed to create debug messenger: " <> T.pack (show err)
      logDebugM CatVulkan "Debug messenger created successfully"
      return $ Just messenger
    else return Nothing
    
  return (inst, dbgMessenger)

destroyVulkanInstance ∷ (Instance, Maybe DebugUtilsMessengerEXT) → EngineM σ ()
destroyVulkanInstance (inst, mbMessenger) = do
  case mbMessenger of
    Just messenger → liftIO $ destroyDebugUtilsMessengerEXT inst messenger Nothing
    Nothing → return ()
  liftIO $ destroyInstance inst Nothing

getAvailableExtensions ∷ EngineM σ [BS.ByteString]
getAvailableExtensions = do
  (_, exts) ← liftIO $ enumerateInstanceExtensionProperties Nothing
  return $ map extensionName $ V.toList exts

-- | Debug messenger info for validation layers
debugUtilsMessengerCreateInfo ∷ DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
#ifdef DEVELOPMENT
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                    ⌄ DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                    ⌄ DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                    ⌄ DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                    ⌄ DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }
#else
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                    ⌄ DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr }
#endif
