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
  , scoreDevice
  , bindlessCapableBonus
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Text as T
import Engine.Core.Error.Exception
  (ExceptionType(..), GraphicsError(..), InitError(..))
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logDebugSM, logInfoSM)
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Capability
  (BindlessSupport(..), deviceBindlessFailureMessage, isBindlessSupported
  ,queryBindlessSupport)
import Engine.Graphics.Vulkan.Texture.Requirements (requiredVulkan12Features)
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.Extensions.VK_KHR_portability_subset

-- | Indices of queue families we need
data QueueFamilyIndices = QueueFamilyIndices
  { graphicsFamily ∷ Word32  -- ^ Index of graphics queue family
  , presentFamily  ∷ Word32  -- ^ Index of present queue family
  } deriving (Show, Eq)

-- | @mSurface@ is the presentation surface for windowed modes;
--   'Nothing' for the offscreen mode (#650), which needs no present
--   queue (the present slots alias the graphics ones) and no
--   VK_KHR_swapchain.
createVulkanDevice ∷ Instance 
                   → PhysicalDevice 
                   → Maybe SurfaceKHR
                   → EngineM σ (Device, DevQueues)
createVulkanDevice _inst physicalDevice mSurface = do
  logDebugM CatVulkan "Finding queue families"
  -- pickPhysicalDevice only selects devices whose probe succeeded, so
  -- Nothing here is an invariant violation, not a selection failure.
  indices ← findQueueFamilies physicalDevice mSurface ⌦ \case
    Just ix → pure ix
    Nothing → logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
                "Selected device has no graphics/present queue family"

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
  
  -- VK_KHR_portability_subset only exists on non-conformant
  -- implementations (MoltenVK); the spec requires enabling it iff the
  -- device advertises it. Enabling it unconditionally fails device
  -- creation with EXTENSION_NOT_PRESENT on Linux.
  (_, availableDevExts) ← liftIO $
    enumerateDeviceExtensionProperties physicalDevice Nothing
  let availableDevExtNames = map extensionName $ V.toList availableDevExts
      -- Swapchain only when there is a surface to present to; the
      -- offscreen mode renders to plain images and must not require it.
      deviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME | isJust mSurface]
        <> [KHR_PORTABILITY_SUBSET_EXTENSION_NAME
           | KHR_PORTABILITY_SUBSET_EXTENSION_NAME `elem` availableDevExtNames]

  logDebugSM CatVulkan "Enabled device extensions"
    [("extensions", T.pack $ show $ map (\bs → BS.take 30 bs) deviceExtensions)]
  
  -- Enable exactly the descriptor-indexing features the bindless shaders and
  -- layout actually use ('requiredVulkan12Features'), and only once the
  -- device has been confirmed to advertise them: requesting an unsupported
  -- feature fails createDevice with VK_ERROR_FEATURE_NOT_PRESENT, an
  -- incidental object-creation error standing in for the capability
  -- diagnostic (#1282). pickPhysicalDevice already rejects incapable
  -- devices; re-checking here keeps the guarantee for any direct caller.
  -- Vulkan 1.2 descriptor indexing triggers MoltenVK metal argument buffers.
  support ← liftIO $ queryBindlessSupport physicalDevice
  unless (isBindlessSupported support) $
    logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
      (deviceBindlessFailureMessage support)

  let vulkan12Features = requiredVulkan12Features

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
        { dqGraphicsQueue = graphicsQ
        , dqPresentQueue = presentQ
        , dqGraphicsFamIdx = graphicsFamily indices
        , dqPresentFamIdx = presentFamily indices
        }
  
  return (device, queues)

destroyVulkanDevice ∷ Device → EngineM σ ()
destroyVulkanDevice device = liftIO $ destroyDevice device Nothing

-- | Pick a suitable physical device (GPU). The surface is 'Nothing'
--   offscreen (#650): devices are then rated on graphics capability
--   alone, with no present-support or swapchain-extension demands.
pickPhysicalDevice ∷ Instance 
                   → Maybe SurfaceKHR  -- ^ Window surface for checking present support
                   → EngineM σ PhysicalDevice
pickPhysicalDevice inst mSurface = do
  (_, devices) ← liftIO $ enumeratePhysicalDevices inst
  when (V.null devices) $
    logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
      "Failed to find GPUs with Vulkan support"

  rated ← V.mapM (rateDevice mSurface) devices
  let ratedDevices = V.zip rated devices
      ((bestScore, bestSupport), bestPDevice) =
        V.maximumBy (\a b → compare (fst (fst a)) (fst (fst b))) ratedDevices

  when (bestScore ≡ 0) $
    logAndThrowM CatVulkan (ExInit DeviceCreationFailed)
      "Failed to find a suitable GPU"

  -- 'scoreDevice' ranks every bindless-capable candidate above every
  -- incapable one, so a usable best that still fails the capability check
  -- means NO usable device can run this renderer. Report that as the
  -- bindless-capability diagnostic (#1055's contract) here, rather than
  -- letting createDevice fail with VK_ERROR_FEATURE_NOT_PRESENT (#1282).
  unless (isBindlessSupported bestSupport) $
    logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
      (deviceBindlessFailureMessage bestSupport)

  PhysicalDeviceProperties { deviceName = chosenName }
    ← liftIO $ getPhysicalDeviceProperties bestPDevice
  logInfoSM CatVulkan "Selected physical device"
    [("device", T.pack $ show chosenName)
    ,("score", T.pack $ show bestScore)]
  return bestPDevice

-- | Rate a physical device's suitability (higher is better, 0 = unusable),
--   returning the bindless support it was rated on so 'pickPhysicalDevice'
--   can explain a capability rejection without querying the device twice.
rateDevice ∷ Maybe SurfaceKHR → PhysicalDevice
           → EngineM σ (Int, BindlessSupport)
rateDevice mSurface device = do
  props ← liftIO $ getPhysicalDeviceProperties device
  queueFamilies ← findQueueFamilies device mSurface
  extensionsSupported ← checkDeviceExtensionSupport device (isJust mSurface)
  support ← liftIO $ queryBindlessSupport device

  let usable = isJust queueFamilies ∧ extensionsSupported
      score = scoreDevice (deviceType props) usable (isBindlessSupported support)
      PhysicalDeviceProperties { deviceName = dname } = props

  logDebugSM CatVulkan "Rated physical device"
    [("device", T.pack $ show dname)
    ,("score", T.pack $ show score)
    ,("bindless_capable", T.pack $ show $ isBindlessSupported support)
    ,("missing_features", T.pack $ show $ bsMissingFeatures support)]
  return (score, support)

-- | How much a bindless-capable candidate is worth. Strictly greater than
--   any device-type base score below, which is what makes capability
--   dominate type: the renderer has no non-bindless path, so a discrete GPU
--   missing the descriptor-indexing features must never displace a capable
--   integrated one (#1282).
bindlessCapableBonus ∷ Int
bindlessCapableBonus = 10000

-- | The pure selection rule behind 'rateDevice', over the three facts it
--   depends on: the candidate's type, whether it has the queue families and
--   extensions this boot mode needs, and whether it can run the bindless
--   renderer. Factored out so the capable-over-incapable ordering is pinned
--   by a headless test rather than by whichever GPUs a machine happens to
--   have. 0 means unusable, as before.
scoreDevice ∷ PhysicalDeviceType  -- ^ Discrete, integrated, or other
            → Bool                -- ^ Queue families and extensions present
            → Bool                -- ^ 'isBindlessSupported'
            → Int
scoreDevice dtype usable bindlessCapable
  | not usable = 0
  | otherwise  = baseScore + bonus
  where
    baseScore = case dtype of
      PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   → 1000
      PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU → 100
      _                                   → 10
    bonus = if bindlessCapable then bindlessCapableBonus else 0

-- | Find required queue family indices. Total: Nothing means the device
--   has no graphics queue family or no family that can present to the
--   surface — rateDevice scores such devices 0 so pickPhysicalDevice can
--   skip them instead of aborting startup (hybrid-GPU laptops routinely
--   expose a device with no present support on any family). With no
--   surface (offscreen, #650) nothing ever presents, so the present
--   family aliases the graphics family to keep 'QueueFamilyIndices'
--   (and DevQueues built from it) total.
findQueueFamilies ∷ PhysicalDevice → Maybe SurfaceKHR
  → EngineM σ (Maybe QueueFamilyIndices)
findQueueFamilies device mSurface = do
  props ← liftIO $ getPhysicalDeviceQueueFamilyProperties device

  let graphicsIdx = V.findIndex
        (\p → (queueFlags p) ⌃ QUEUE_GRAPHICS_BIT ≢ zeroBits)
        props

  presentIdx ← case mSurface of
    Nothing → pure graphicsIdx
    Just surface → do
      presentSupport ← V.generateM (V.length props) $ \i →
        liftIO $ getPhysicalDeviceSurfaceSupportKHR device
          (fromIntegral i) surface
      pure $ V.findIndex id presentSupport

  pure $ case (graphicsIdx, presentIdx) of
    (Just gIdx, Just pIdx) → Just $ QueueFamilyIndices
      { graphicsFamily = fromIntegral gIdx
      , presentFamily  = fromIntegral pIdx
      }
    _ → Nothing

-- | Check if device supports required extensions. Swapchain support is
--   only demanded when the device must present (windowed modes).
checkDeviceExtensionSupport ∷ PhysicalDevice → Bool → EngineM σ Bool
checkDeviceExtensionSupport device needsPresent = do
  (_, availableExtensions) ← liftIO $
    enumerateDeviceExtensionProperties device Nothing

  let requiredExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME | needsPresent]
      availableExtNames = map extensionName $ V.toList availableExtensions

  return $ all (`elem` availableExtNames) requiredExtensions
