{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Engine.Graphics.Vulkan.Instance
  ( createVulkanInstance
  , destroyVulkanInstance
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Error.Exception
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.Core10
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_KHR_portability_subset
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Zero

-- | Create Vulkan instance create info with proper portability support
vulkanInstanceCreateInfo ∷ GraphicsConfig → EngineM ε σ (InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT])
vulkanInstanceCreateInfo config = do
  -- Get GLFW required extensions
  glfwExts ← GLFW.getRequiredInstanceExtensions
  -- Get all available extensions
  (_count, exts) ← liftIO $ enumerateInstanceExtensionProperties Nothing
  let availableExts = map extensionName $ V.toList exts
  -- Get all available layers
  (_count, layers) ← liftIO $ enumerateInstanceLayerProperties
  let availableLayers = map layerName $ V.toList layers
  -- Basic required extensions (including debug if enabled)
  let baseExts = if gcDebugMode config 
                 then [EXT_DEBUG_UTILS_EXTENSION_NAME]
                 else []
  -- Combine with GLFW extensions
  let requiredExts = baseExts <> glfwExts
      missingExts = filter (not ∘ (`elem` availableExts)) requiredExts
  unless (null missingExts) $
    throwInitError ExtensionNotSupported $ T.pack $
      "Required extensions not available: " ⧺ show missingExts
  -- Check if portability enumeration is available and needed
  let needsPortability = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME `elem` availableExts
      portabilityExts  = if needsPortability 
                        then [KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME]
                        else []
  -- Combine all extensions
  let finalExts = V.fromList $ requiredExts <> portabilityExts
  -- Set up validation layers if debug mode is enabled
  let validationLayer = "VK_LAYER_KHRONOS_validation"
      layers' = if gcDebugMode config then
                  if validationLayer `elem` availableLayers
                    then V.fromList [validationLayer]
                    else V.empty
                else V.empty
  -- Set up flags - include portability bit if needed
  let flags = if needsPortability 
              then INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
              else zero
  -- Create the final instance info
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
  -- Get GLFW required extensions
  glfwExts ← GLFW.getRequiredInstanceExtensions
  
  -- Add macOS required extensions (following madrigal's pattern)
  let baseExtensions = if gcDebugMode config 
                      then [EXT_DEBUG_UTILS_EXTENSION_NAME]
                      else []
      allExtensions = baseExtensions 
                     <> glfwExts
                     <> [KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME]
                     <> [KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME]  -- Required for macOS
      
  let instCreateInfo = zero 
        { applicationInfo = Just $ zero 
            { applicationName = Just $ BSU.fromString $ T.unpack $ gcAppName config
            , engineName     = Just "Synarchy Engine"
            , apiVersion     = API_VERSION_1_0
            }
        , enabledExtensionNames = V.fromList allExtensions
        , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR  -- Required for portability
        }
        ::& debugUtilsMessengerCreateInfo
        :& ()

  inst ← allocResource (\i0 → destroyInstance i0 Nothing)
           $ createInstance instCreateInfo Nothing `catchEngine` \err →
               throwInitError VulkanInitFailed $ T.pack $
                 "Failed to create Vulkan instance: " ⧺ show err
  
  -- Create debug messenger if debug mode is enabled
  dbgMessenger ← if gcDebugMode config 
    then do
      messenger ← createDebugUtilsMessengerEXT inst debugUtilsMessengerCreateInfo Nothing
        `catchEngine` \err → throwInitError VulkanInitFailed $ T.pack $
          "Failed to create debug messenger: " ⧺ show err
      return $ Just messenger
    else return Nothing
    
  return (inst, dbgMessenger)

-- | Clean up Vulkan instance and debug messenger
destroyVulkanInstance ∷ (Instance, Maybe DebugUtilsMessengerEXT) → EngineM ε σ ()
destroyVulkanInstance (inst, mbMessenger) = do
  -- First destroy debug messenger if it exists
  case mbMessenger of
    Just messenger → liftIO $ destroyDebugUtilsMessengerEXT inst messenger Nothing
    Nothing → return ()
  -- Then destroy instance
  liftIO $ destroyInstance inst Nothing

-- | Use this in your main initialization
initVulkan ∷ GraphicsConfig → EngineM ε σ Instance
initVulkan config = do
  (inst, _) ← allocResource destroyVulkanInstance $ createVulkanInstance config
    `catchEngine` \err → throwInitError VulkanInitFailed $ T.pack $
      "Failed to initialize Vulkan: " ⧺ show err
  return inst

getAvailableExtensions ∷ EngineM ε σ [BS.ByteString]
getAvailableExtensions = do
  (_, exts) ← liftIO $ enumerateInstanceExtensionProperties Nothing
  return $ map extensionName $ V.toList exts

getAvailableLayers ∷ EngineM ε σ [BS.ByteString]
getAvailableLayers = do
  (_, layers) ← liftIO $ enumerateInstanceLayerProperties
  return $ map layerName $ V.toList layers

filterExtensions ∷ [BS.ByteString] → [BS.ByteString] → EngineM ε σ [BS.ByteString]
filterExtensions available required = do
  let missing = filter (not ∘ (`elem` available)) required
  unless (null missing) $
    throwInitError ExtensionNotSupported $ T.pack $
      "Required extensions not available"
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
