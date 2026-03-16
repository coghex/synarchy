{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Instance
  ( createVulkanInstance
  , destroyVulkanInstance
  , getAvailableExtensions
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logInfoM, logDebugSM)
import Engine.Core.Error.Exception (ExceptionType(..), SystemError(..)
                                   , InitError(..), catchEngine)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Base
import Engine.Graphics.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Foreign.Marshal.Utils (with)
import Vulkan.Core10
import Vulkan.Core12
import Vulkan.Core10.LayerDiscovery (LayerProperties(..))
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_layer_settings
import Vulkan.Extensions.VK_KHR_portability_subset
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Zero

-- | Create Vulkan instance create info with proper portability support
vulkanInstanceCreateInfo ∷ GraphicsConfig → EngineM ε σ (InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT])
vulkanInstanceCreateInfo config = do
  glfwExts ← GLFW.getRequiredInstanceExtensions
  (_count, exts) ← liftIO $ enumerateInstanceExtensionProperties Nothing
  let availableExts = map extensionName $ V.toList exts
  (_count, layers) ← liftIO $ enumerateInstanceLayerProperties
  let availableLayers = map (\LayerProperties{layerName = ln} → ln) $ V.toList layers
  let baseExts = if gcDebugMode config
                 then [EXT_DEBUG_UTILS_EXTENSION_NAME]
                 else []
  let requiredExts = baseExts <> glfwExts
      missingExts = filter (not ∘ (`elem` availableExts)) requiredExts
  unless (null missingExts) $
    logAndThrowM CatInit (ExInit ExtensionNotSupported) $
      "Required extensions not available: " <> T.pack (show missingExts)
  let needsPortability = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME `elem` availableExts
      portabilityExts  = if needsPortability 
                        then [KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME]
                        else []
  let finalExts = V.fromList $ requiredExts <> portabilityExts
  let validationLayer = "VK_LAYER_KHRONOS_validation"
      layers' = if gcDebugMode config then
                  if validationLayer `elem` availableLayers
                    then V.fromList [validationLayer]
                    else V.empty
                else V.empty
  let flags = if needsPortability 
              then INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
              else zero
  pure $ zero
    { applicationInfo = Just $ zero 
        { applicationName = Just $ BSU.fromString $ T.unpack $ gcAppName config
        , engineName     = Just "Synarchy Engine"
        , apiVersion     = API_VERSION_1_0
        }
    , enabledLayerNames     = layers'
    , enabledExtensionNames = finalExts
    , flags                 = flags
    }
    ::& debugUtilsMessengerCreateInfo
    :& ()

-- | Create and initialize Vulkan instance with optional debug messenger
createVulkanInstance ∷ GraphicsConfig → EngineM ε σ (Instance, Maybe DebugUtilsMessengerEXT)
createVulkanInstance config = do
  logDebugM CatVulkan "Initializing Vulkan instance"
  
  glfwExts ← GLFW.getRequiredInstanceExtensions

  logDebugSM CatVulkan "Instance extensions"
    [("glfw_extensions", T.pack $ show $ map (\bs → BSU.toString bs) glfwExts)
    ,("debug_mode", T.pack $ show $ gcDebugMode config)]
  
  let baseExtensions = if gcDebugMode config 
                      then [EXT_DEBUG_UTILS_EXTENSION_NAME]
                      else []
      allExtensions = baseExtensions 
                     <> glfwExts
                     <> [KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME]
                     <> [KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME]  -- Required for macOS
                     <> [EXT_LAYER_SETTINGS_EXTENSION_NAME]  -- for moltenvk config

  logDebugM CatVulkan "Creating Vulkan instance with MoltenVK configuration"
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

        instCreateInfo = zero 
          { applicationInfo = Just $ zero 
              { applicationName = Just $ BSU.fromString $ T.unpack $ gcAppName config
              , engineName     = Just "Synarchy Engine"
              , apiVersion     = API_VERSION_1_2
              }
          , enabledExtensionNames = V.fromList allExtensions
          , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
          }
          ::& layerSettingsInfo
          :& debugUtilsMessengerCreateInfo
          :& ()
    
    createInstance instCreateInfo Nothing
  
  logDebugM CatVulkan "Vulkan instance created successfully"
  
  dbgMessenger ← if gcDebugMode config
    then do
      logDebugM CatVulkan "Creating debug messenger"
      messenger ← createDebugUtilsMessengerEXT inst debugUtilsMessengerCreateInfo Nothing
        `catchEngine` \err → logAndThrowM CatInit (ExInit VulkanInitFailed) $
          "Failed to create debug messenger: " <> T.pack (show err)
      logDebugM CatVulkan "Debug messenger created successfully"
      return $ Just messenger
    else return Nothing
    
  return (inst, dbgMessenger)

destroyVulkanInstance ∷ (Instance, Maybe DebugUtilsMessengerEXT) → EngineM ε σ ()
destroyVulkanInstance (inst, mbMessenger) = do
  case mbMessenger of
    Just messenger → liftIO $ destroyDebugUtilsMessengerEXT inst messenger Nothing
    Nothing → return ()
  liftIO $ destroyInstance inst Nothing

-- | Initialize Vulkan with resource cleanup registration
initVulkan ∷ GraphicsConfig → EngineM ε σ Instance
initVulkan config = do
  (inst, _) ← allocResource destroyVulkanInstance $ createVulkanInstance config
    `catchEngine` \err → logAndThrowM CatInit (ExInit VulkanInitFailed) $
      "Failed to initialize Vulkan: " <> T.pack (show err)
  return inst

getAvailableExtensions ∷ EngineM ε σ [BS.ByteString]
getAvailableExtensions = do
  (_, exts) ← liftIO $ enumerateInstanceExtensionProperties Nothing
  return $ map extensionName $ V.toList exts

getAvailableLayers ∷ EngineM ε σ [BS.ByteString]
getAvailableLayers = do
  (_, layers) ← liftIO $ enumerateInstanceLayerProperties
  return $ map (\LayerProperties{layerName = ln} → ln) $ V.toList layers

filterExtensions ∷ [BS.ByteString] → [BS.ByteString] → EngineM ε σ [BS.ByteString]
filterExtensions available required = do
  let missing = filter (not ∘ (`elem` available)) required
  unless (null missing) $
    logAndThrowM CatInit (ExInit ExtensionNotSupported) $
      "Required extensions not available: " <> T.pack (show missing)
  return required

-- | Debug messenger info for validation layers
debugUtilsMessengerCreateInfo ∷ DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
#ifdef DEVELOPMENT
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                    .|. DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                    .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                    .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                    .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }
#else
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                    .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr }
#endif
