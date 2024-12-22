{-# LANGUAGE ScopedTypeVariables #-}
module Engine.Graphics.Vulkan.Instance
  ( createVulkanInstance
  , destroyVulkanInstance
  ) where

import UPrelude
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (nullPtr)
import Foreign.C.String (peekCString)
import Engine.Core.Error.Exception
import Engine.Core.Monad
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.Core10
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Zero

createVulkanInstance ∷ GraphicsConfig → EngineM ε σ Instance
createVulkanInstance config = do
  instCI ← vulkanInstanceCreateInfo config
  instance' ← liftIO $ createInstance instCI Nothing
  when (gcDebugMode config) $ do
    messenger ← liftIO $ createDebugUtilsMessengerEXT instance' debugUtilsMessengerCreateInfo Nothing
    return ()
  return instance'

destroyVulkanInstance ∷ Instance → EngineM ε σ ()
destroyVulkanInstance inst = liftIO $ destroyInstance inst Nothing

vulkanInstanceCreateInfo ∷ GraphicsConfig → EngineM ε σ (InstanceCreateInfo '[DebugUtilsMessengerCreateInfoEXT])
vulkanInstanceCreateInfo config = do
  glfwReqExts ← GLFW.getRequiredInstanceExtensions
  availableExts ← getAvailableExtensions
  availableLayers ← getAvailableLayers
  
  let debugExts = if gcDebugMode config 
                  then [EXT_DEBUG_UTILS_EXTENSION_NAME]
                  else []
      allReqExts = glfwReqExts ⧺ debugExts
      
      validationLayers = if gcDebugMode config
                        then [BSU.fromString "VK_LAYER_KHRONOS_validation"]
                        else []
                        
  filteredExts ← filterExtensions availableExts allReqExts
  filteredLayers ← filterExtensions availableLayers validationLayers
  
  let appName = BSU.fromString $ T.unpack $ gcAppName config

  pure $ zero 
    { applicationInfo = Just $ zero 
        { applicationName = Just appName
        , apiVersion = API_VERSION_1_0 
        }
    , enabledLayerNames = V.fromList filteredLayers
    , enabledExtensionNames = V.fromList filteredExts
    }
    ::& debugUtilsMessengerCreateInfo
    :& ()

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
    throwEngineException $ EngineException ExGraphics
      $ "Required extensions not available"
  return required

debugUtilsMessengerCreateInfo ∷ DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT 
                    .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                 .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                 .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }
