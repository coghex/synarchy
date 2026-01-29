module Engine.Loop.Resource
  ( validateDescriptorState
  , getFrameResources
  , getCommandBuffer
  , getDevice
  , getSwapchain
  , getQueues
  , extractWindow
  , safeVectorIndex
  , safeVectorHead
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Types (DevQueues(..), SwapchainInfo(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Vulkan.Core10 (Device, CommandBuffer)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainKHR)
import GHC.Stack (HasCallStack)

-- | Validate descriptor state is ready for rendering
validateDescriptorState ∷ GraphicsState → EngineM ε σ ()
validateDescriptorState state = case descriptorState state of
    Nothing → throwGraphicsError DescriptorError "No descriptor manager"
    Just dm → when (V.null $ dmActiveSets dm) $
        throwGraphicsError DescriptorError "No active descriptor sets"

-- | Get frame resources for a given frame index
getFrameResources ∷ GraphicsState → Word32 → EngineM ε σ FrameResources
getFrameResources state frameIdx = 
    case safeVectorIndex (frameResources state) (fromIntegral frameIdx) of
        Nothing → throwGraphicsError CommandBufferError $
            "Frame index out of bounds: " <> T.pack (show frameIdx)
        Just res → pure res

-- | Get command buffer from frame resources
getCommandBuffer ∷ FrameResources → EngineM ε σ CommandBuffer
getCommandBuffer resources = 
    case safeVectorHead (frCommandBuffer resources) of
        Nothing → throwGraphicsError CommandBufferError "No command buffer"
        Just cb → pure cb

-- | Get Vulkan device
getDevice ∷ GraphicsState → EngineM ε σ Device
getDevice state = case vulkanDevice state of
    Nothing → throwGraphicsError VulkanDeviceLost "No device"
    Just d  → pure d

-- | Get swapchain
getSwapchain ∷ GraphicsState → EngineM ε σ SwapchainKHR
getSwapchain state = case swapchainInfo state of
    Nothing → throwGraphicsError SwapchainError "No swapchain"
    Just si → pure $ siSwapchain si

-- | Get device queues
getQueues ∷ GraphicsState → EngineM ε σ DevQueues
getQueues state = case deviceQueues state of
    Nothing → throwGraphicsError VulkanDeviceLost "No queues"
    Just q  → pure q

-- | Extract window from graphics state
extractWindow ∷ HasCallStack ⇒ GraphicsState → Either EngineException Window
extractWindow state = case glfwWindow state of
    Nothing  → Left $ EngineException
                  (ExSystem (GLFWError "drawFrame"))
                  "No GLFW window initialized"
                  mkErrorContext
    Just win → Right win

-- | Safe vector access by index
safeVectorIndex ∷ V.Vector a → Int → Maybe a
safeVectorIndex vec idx
  | idx >= 0 && idx < V.length vec = Just (vec V.! idx)
  | otherwise = Nothing

-- | Safe vector head
safeVectorHead ∷ V.Vector a → Maybe a
safeVectorHead vec
  | V.null vec = Nothing
  | otherwise  = Just (V.head vec)
