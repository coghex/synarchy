module Test.Engine.Graphics.Vulkan.Helpers where
import UPrelude
import Engine.Core.Base
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Graphics.Base
import Engine.Graphics.Vulkan.Instance
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils

createTestInstance ∷ Bool → EngineM ε σ (Instance, Maybe DebugUtilsMessengerEXT)
createTestInstance withDebug = do
    let config = defaultGraphicsConfig 
            { gcDebugMode = withDebug
            , gcAppName = "VulkanTest" }
    createVulkanInstance config
