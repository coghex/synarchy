{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}
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
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Core.Error.Exception (ExceptionType(..), InitError(..))
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logInfoM, logDebugSM)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Types
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Core12 (PhysicalDeviceVulkan12Features(..))
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
  logDebugM CatVulkan "Finding queue families"
  indices ← findQueueFamilies physicalDevice surface
  
  logDebugSM CatVulkan "Queue families found"
    [("graphics_family", T.pack $ show $ graphicsFamily indices)
    ,("present_family", T.pack $ show $ presentFamily indices)]
  
  let queuePriority = 1.0
      uniqueFamilies = if graphicsFamily indices ≡ presentFamily indices
                      then [graphicsFamily indices]
                      else [graphicsFamily indices, presentFamily indices]
      queueCreateInfos = map (\queueFamily → SomeStruct ((zero ∷ DeviceQueueCreateInfo '[])
        { queueFamilyIndex = queueFamily
        , queuePriorities = V.singleton queuePriority
        })) uniqueFamilies
  
  logDebugSM CatVulkan "Creating device queues"
    [("queue_count", T.pack $ show $ length uniqueFamilies)]
  
  let deviceExtensions = [ KHR_SWAPCHAIN_EXTENSION_NAME
                        , KHR_PORTABILITY_SUBSET_EXTENSION_NAME  -- Required for macOS
                        ]
  
  logDebugSM CatVulkan "Enabled device extensions"
    [("extensions", T.pack $ show $ map (\bs → BS.take 30 bs) deviceExtensions)]
  
  -- Vulkan 1.2 descriptor indexing triggers MoltenVK metal argument buffers
  let vulkan12Features = zero
        { descriptorIndexing = True
        , shaderSampledImageArrayNonUniformIndexing = True
        , runtimeDescriptorArray = True
        , descriptorBindingPartiallyBound = True
        , descriptorBindingUpdateUnusedWhilePending = True
        , descriptorBindingVariableDescriptorCount = True
        } ∷ PhysicalDeviceVulkan12Features
  
  logDebugM CatVulkan "Enabled Vulkan 1.2 descriptor indexing features"
  
  let deviceCreateInfo = (zero ∷ DeviceCreateInfo '[])
        { queueCreateInfos = V.fromList queueCreateInfos
        , enabledExtensionNames = V.fromList deviceExtensions
        , enabledFeatures = Just zero
        }
        ::& vulkan12Features
        :& ()
  
  device ← allocResource (\d0 → destroyDevice d0 Nothing)
             $ createDevice physicalDevice deviceCreateInfo Nothing
  
  logDebugM CatVulkan "Logical device created"
  
  graphicsQ ← getDeviceQueue device (graphicsFamily indices) 0
  presentQ ← getDeviceQueue device (presentFamily indices) 0
  
  logDebugM CatVulkan "Device queues retrieved"
  
  let queues = DevQueues
        { graphicsQueue = graphicsQ
        , presentQueue = presentQ
        , graphicsFamIdx = graphicsFamily indices
        , presentFamIdx = presentFamily indices
        }
  
  return (device, queues)

destroyVulkanDevice ∷ Device → EngineM ε σ ()
destroyVulkanDevice device = liftIO $ destroyDevice device Nothing

-- | Pick a suitable physical device (GPU)
pickPhysicalDevice ∷ Instance 
                   → SurfaceKHR  -- ^ Window surface for checking present support
                   → EngineM ε σ PhysicalDevice
pickPhysicalDevice inst surface = do
  (_, devices) ← liftIO $ enumeratePhysicalDevices inst
  when (V.null devices) $
    logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
      "Failed to find GPUs with Vulkan support"
  
  scores ← V.mapM (rateDevice surface) devices
  let ratedDevices = V.zip scores devices
      bestDevice = V.maximumBy (\a b → compare (fst a) (fst b)) ratedDevices
  
  if fst bestDevice ≡ 0
    then logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
      "Failed to find a suitable GPU"
    else return $ snd bestDevice

-- | Rate a physical device's suitability (higher is better)
rateDevice ∷ SurfaceKHR → PhysicalDevice → EngineM ε σ Int
rateDevice surface device = do
  props ← liftIO $ getPhysicalDeviceProperties device
  queueFamilies ← findQueueFamilies device surface
  extensionsSupported ← checkDeviceExtensionSupport device

  let baseScore = case deviceType props of
        PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   → 1000
        PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU → 100
        _                                   → 10
  
  if isDeviceSuitable device queueFamilies extensionsSupported
    then return baseScore
    else return 0

-- | Find required queue family indices
findQueueFamilies ∷ PhysicalDevice → SurfaceKHR → EngineM ε σ QueueFamilyIndices
findQueueFamilies device surface = do
  props ← liftIO $ getPhysicalDeviceQueueFamilyProperties device

  let graphicsIdx = V.findIndex 
        (\p → (queueFlags p) .&. QUEUE_GRAPHICS_BIT ≢ zeroBits) 
        props
  
  case graphicsIdx of
    Nothing → logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
                           "Could not find graphics queue family"
    Just gIdx → do
      presentSupport ← V.generateM (V.length props) $ \i → 
        liftIO $ getPhysicalDeviceSurfaceSupportKHR device 
          (fromIntegral i) surface
      
      let presentIdx = V.findIndex id presentSupport
      
      case presentIdx of
        Nothing → logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
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
  graphicsFamily indices ≥ 0 ∧
  presentFamily indices ≥ 0 ∧
  extensionsSupported

-- | Check if device supports required extensions
checkDeviceExtensionSupport ∷ PhysicalDevice → EngineM ε σ Bool
checkDeviceExtensionSupport device = do
  (_, availableExtensions) ← liftIO $
    enumerateDeviceExtensionProperties device Nothing

  let requiredExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]
      availableExtNames = map extensionName $ V.toList availableExtensions

  return $ all (`elem` availableExtNames) requiredExtensions
