module Test.Engine.Graphics.Vulkan.Device where

import UPrelude
import Test.Hspec
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import Data.Bits ((.&.), zeroBits)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Engine.Graphics.Vulkan.Device
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Core.Error.Exception

spec ∷ Spec
spec = describe "Vulkan Device" $ do
    describe "Queue Family Selection" $ do
        it "should find queue families" $ do
            -- Get required extensions from GLFW
            rawExts ← GLFW.getRequiredInstanceExtensions
            extensions ← traverse BS.packCString rawExts
            
            -- Add required macOS extensions
            let macExtensions = [ Vk.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
                              , Vk.KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                              ] ⧺ extensions
            
            -- Create a minimal Vulkan instance with macOS support
            let appInfo = Vk.ApplicationInfo
                  { applicationName = Just "Test App"
                  , applicationVersion = Vk.API_VERSION_1_0
                  , engineName = Just "Test Engine"
                  , engineVersion = Vk.API_VERSION_1_0
                  , apiVersion = Vk.API_VERSION_1_0
                  }
            
            -- Convert validation layer name to ByteString
            let validationLayer = BS.pack "VK_LAYER_KHRONOS_validation"
            
            -- Setup create info with proper Vector types and macOS flags
            let createInfo = Vk.InstanceCreateInfo
                  { next = ()
                  , flags = Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
                  , applicationInfo = Just appInfo
                  , enabledLayerNames = V.singleton validationLayer
                  , enabledExtensionNames = V.fromList macExtensions
                  }

            -- Create instance
            inst ← Vk.createInstance createInfo Nothing
            
            -- Find physical devices, handling the result tuple
            (result, devs) ← Vk.enumeratePhysicalDevices inst
            result `shouldBe` Vk.SUCCESS
            V.null devs `shouldBe` False

            -- Get queue family properties for first device
            let device = V.head devs
            props ← Vk.getPhysicalDeviceQueueFamilyProperties device
                    
            -- Find a suitable graphics queue family using proper bit ops
            let hasGraphics prop = 
                    ((Vk.QUEUE_GRAPHICS_BIT .&. Vk.queueFlags prop) /= zeroBits) && (Vk.queueCount prop > 0)
                graphicsFamily = V.findIndex hasGraphics props
                    
            -- Clean up
            graphicsFamily `shouldSatisfy` maybe False (const True)
            Vk.destroyInstance inst Nothing
