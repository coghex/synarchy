{-# LANGUAGE ScopedTypeVariables #-}
module Engine.Graphics.Vulkan.Device
  ( -- * Types
    QueueFamilyIndices(..)
    -- * Device Creation
  , createVulkanDevice
  , destroyVulkanDevice
    -- * Device Selection
  , pickPhysicalDevice
  , findQueueFamilies
  ) where

import UPrelude
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits ((.&.), (.|.), zeroBits)
import Data.Word (Word32)
import qualified Data.Vector as V
import Foreign.Ptr (nullPtr)
import Engine.Core.Error.Exception
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_portability_subset
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2

-- | Indices of queue families we need
data QueueFamilyIndices = QueueFamilyIndices
  { graphicsFamily ∷ Word32  -- ^ Index of graphics queue family
  , presentFamily  ∷ Word32  -- ^ Index of present queue family
  } deriving (Show, Eq)

createVulkanDevice ∷ Instance 
                   → PhysicalDevice 
                   → SurfaceKHR     
                   → EngineM ε σ (Device, DevQueues)
createVulkanDevice inst physicalDevice surface = do
  -- Find queue families
  indices ← findQueueFamilies physicalDevice surface
  
  -- Create unique queue create infos
  let queuePriority = 1.0
      uniqueFamilies = if graphicsFamily indices == presentFamily indices
                      then [graphicsFamily indices]
                      else [graphicsFamily indices, presentFamily indices]
      queueCreateInfos = map (\queueFamily → SomeStruct ((zero ∷ DeviceQueueCreateInfo '[])
        { queueFamilyIndex = queueFamily
        , queuePriorities = V.singleton queuePriority
        })) uniqueFamilies
  
  -- Get device extensions - note how madrigal specifies both extensions upfront
  let deviceExtensions = [ KHR_SWAPCHAIN_EXTENSION_NAME
                        , KHR_PORTABILITY_SUBSET_EXTENSION_NAME  -- Required for macOS
                        ]
  
  -- Create the logical device
  let deviceCreateInfo = (zero ∷ DeviceCreateInfo '[])
        { queueCreateInfos = V.fromList queueCreateInfos
        , enabledExtensionNames = V.fromList deviceExtensions
        , enabledFeatures = Just zero
        }
  
  device ← allocResource (\d0 → destroyDevice d0 Nothing)
             $ createDevice physicalDevice deviceCreateInfo Nothing
  
  -- Get queue handles
  graphicsQ ← getDeviceQueue device (graphicsFamily indices) 0
  presentQ ← getDeviceQueue device (presentFamily indices) 0
  
  let queues = DevQueues
        { graphicsQueue = graphicsQ
        , presentQueue = presentQ
        , graphicsFamIdx = graphicsFamily indices
        , presentFamIdx = presentFamily indices
        }
  
  return (device, queues)

-- | Clean up the logical device
destroyVulkanDevice ∷ Device → EngineM ε σ ()
destroyVulkanDevice device = liftIO $ destroyDevice device Nothing

-- | Pick a suitable physical device (GPU)
pickPhysicalDevice ∷ Instance 
                   → SurfaceKHR  -- ^ Window surface for checking present support
                   → EngineM ε σ PhysicalDevice
pickPhysicalDevice inst surface = do
  -- Get all physical devices
  (_, devices) ← liftIO $ enumeratePhysicalDevices inst
  when (V.null devices) $
    throwInitError DeviceCreationFailed
      "Failed to find GPUs with Vulkan support"
  
  -- Score and pick the best device
  scores ← V.mapM (rateDevice surface) devices
  let ratedDevices = V.zip scores devices
      bestDevice = V.maximumBy (\a b → compare (fst a) (fst b)) ratedDevices
  
  if fst bestDevice == 0
    then throwInitError DeviceCreationFailed
      "Failed to find a suitable GPU"
    else return $ snd bestDevice

-- | Rate a physical device's suitability (higher is better)
rateDevice ∷ SurfaceKHR → PhysicalDevice → EngineM ε σ Int
rateDevice surface device = do
  -- Get device properties
  props ← liftIO $ getPhysicalDeviceProperties device
  
  -- Check for required queue families
  queueFamilies ← findQueueFamilies device surface
  
  -- Check for required device extensions
  extensionsSupported ← checkDeviceExtensionSupport device
  
  -- Base score on device type
  let baseScore = case deviceType props of
        PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   → 1000
        PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU → 100
        _                                   → 10
  
  -- Device is suitable only if it has required features
  if isDeviceSuitable device queueFamilies extensionsSupported
    then return baseScore
    else return 0

-- | Find required queue family indices
findQueueFamilies ∷ PhysicalDevice → SurfaceKHR → EngineM ε σ QueueFamilyIndices
findQueueFamilies device surface = do
  -- Get queue family properties
  props ← liftIO $ getPhysicalDeviceQueueFamilyProperties device
  
  -- Find graphics queue family
  let graphicsIdx = V.findIndex 
        (\p → (queueFlags p) .&. QUEUE_GRAPHICS_BIT /= zeroBits) 
        props
  
  case graphicsIdx of
    Nothing → throwInitError DeviceCreationFailed
      "Could not find graphics queue family"
    Just gIdx → do
      -- Find present queue family
      presentSupport ← V.generateM (V.length props) $ \i → 
        liftIO $ getPhysicalDeviceSurfaceSupportKHR device 
          (fromIntegral i) surface
      
      let presentIdx = V.findIndex id presentSupport
      
      case presentIdx of
        Nothing → throwInitError DeviceCreationFailed
          "Could not find present queue family"
        Just pIdx → return $ QueueFamilyIndices
          { graphicsFamily = fromIntegral gIdx
          , presentFamily = fromIntegral pIdx
          }

-- | Check if a device is suitable for our needs
isDeviceSuitable ∷ PhysicalDevice 
                 → QueueFamilyIndices 
                 → Bool  -- ^ Extension support
                 → Bool
isDeviceSuitable device indices extensionsSupported =
  -- Check if device has all required queue families
  graphicsFamily indices >= 0 && 
  presentFamily indices >= 0 &&
  -- Check if device supports required extensions
  extensionsSupported

-- | Check if device supports required extensions
checkDeviceExtensionSupport ∷ PhysicalDevice → EngineM ε σ Bool
checkDeviceExtensionSupport device = do
  -- Get available extensions
  (_, availableExtensions) ← liftIO $ 
    enumerateDeviceExtensionProperties device Nothing
  
  -- Required extensions
  let requiredExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]
      availableExtNames = map extensionName $ V.toList availableExtensions
  
  -- Check if all required extensions are available
  return $ all (`elem` availableExtNames) requiredExtensions
