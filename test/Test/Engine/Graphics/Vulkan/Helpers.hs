-- test/Test/Engine/Graphics/Vulkan/Helpers.hs

-- | The graphical specs' one Vulkan-instance bracket (#1401).
--
--   'createVulkanInstance' deliberately returns a raw handle and
--   registers no cleanup of its own; production pairs it with
--   'destroyVulkanInstance' at the owner
--   ('Engine.Graphics.Vulkan.Init'). The specs are that owner too, so
--   they own instances through this module rather than repeating the
--   pairing at each call site.
module Test.Engine.Graphics.Vulkan.Helpers
  ( withTestInstance
  ) where

import UPrelude
import Engine.Core.Monad (EngineM, EngineM')
import Engine.Core.Resource (allocResource, locally)
import Engine.Graphics.Base (GraphicsConfig)
import Engine.Graphics.Vulkan.Instance
  (InstanceSurfaceUse, createVulkanInstance, destroyVulkanInstance)
import Vulkan.Core10 (Instance)
import Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerEXT)

-- | Run @body@ with a live Vulkan instance and destroy it — debug
--   messenger first — when @body@ exits.
--
--   Cleanup runs on every exit: a normal return, an @EngineM@
--   'Control.Monad.Error.Class.throwError', and a native IO exception
--   such as an hspec assertion failure, because 'allocResource' places
--   it in a 'Control.Exception.finally' around the continuation.
--
--   'locally' is what bounds the scope to @body@, and it is load
--   bearing. 'allocResource' on its own frees when the ENCLOSING
--   continuation exits, so two bare instance allocations inside one
--   @runEngineTest@ would hold both instances live simultaneously and
--   unwind only at the end. Giving each bracket its own continuation
--   makes one @withTestInstance@ finish destroying before the next
--   creates.
--
--   Resources allocated by @body@ unwind first: 'allocResource' inside
--   @body@ (which is how 'Engine.Graphics.Window.GLFW.createWindowSurface'
--   and 'Engine.Graphics.Vulkan.Device.createVulkanDevice' register
--   their own destruction) nests strictly inside this continuation, so
--   a device is destroyed before its surface and both before the
--   instance that owns them.
--
--   The 'GraphicsConfig' stays the caller's: each spec keeps the exact
--   configuration it used before, including the @gcDebugMode@ that
--   'Engine.Core.Defaults.defaultGraphicsConfig' derives from the
--   @DEVELOPMENT@ flag.
withTestInstance ∷ GraphicsConfig → InstanceSurfaceUse
                 → ((Instance, Maybe DebugUtilsMessengerEXT) → EngineM' α)
                 → EngineM σ α
withTestInstance config surfaceUse body = locally $
  allocResource destroyVulkanInstance
                (createVulkanInstance config surfaceUse)
    ⌦ body
