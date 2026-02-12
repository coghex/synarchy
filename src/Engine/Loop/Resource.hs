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
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..)
                                   , EngineException(..), SystemError(..)
                                   , mkErrorContext)
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Types (DevQueues(..), SwapchainInfo(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Vulkan.Core10 (Device, CommandBuffer)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainKHR)
import GHC.Stack (HasCallStack)

-----------------------------------------------------------
-- Resource Validation and Extraction
-----------------------------------------------------------

-- | Validate descriptor state is ready for rendering
validateDescriptorState ∷ GraphicsState → EngineM ε σ ()
validateDescriptorState state = case descriptorState state of
    Nothing → logAndThrowM CatGraphics (ExGraphics DescriptorError)
        "Descriptor manager is missing in graphics state"
    Just dm → when (V.null $ dmActiveSets dm) $
        logAndThrowM CatGraphics (ExGraphics DescriptorError)
            "No active descriptor sets"

getFrameResources ∷ GraphicsState → Word32 → EngineM ε σ FrameResources
getFrameResources state frameIdx = 
    case safeVectorIndex (frameResources state) (fromIntegral frameIdx) of
        Nothing → logAndThrowM CatGraphics (ExGraphics CommandBufferError) $
            "Frame index out of bounds: " <> T.pack (show frameIdx)
        Just res → pure res

getCommandBuffer ∷ FrameResources → EngineM ε σ CommandBuffer
getCommandBuffer resources = 
    case safeVectorHead (frCommandBuffer resources) of
        Nothing → logAndThrowM CatGraphics (ExGraphics CommandBufferError)
                                           "No command buffer"
        Just cb → pure cb

getDevice ∷ GraphicsState → EngineM ε σ Device
getDevice state = case vulkanDevice state of
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost) "No device"
    Just d  → pure d

getSwapchain ∷ GraphicsState → EngineM ε σ SwapchainKHR
getSwapchain state = case swapchainInfo state of
    Nothing → logAndThrowM CatGraphics (ExGraphics SwapchainError) "No swapchain"
    Just si → pure $ siSwapchain si

getQueues ∷ GraphicsState → EngineM ε σ DevQueues
getQueues state = case deviceQueues state of
    Nothing → logAndThrowM CatGraphics (ExGraphics VulkanDeviceLost) "No queues"
    Just q  → pure q

extractWindow ∷ HasCallStack ⇒ GraphicsState → Either EngineException Window
extractWindow state = case glfwWindow state of
    Nothing  → Left $ EngineException
                  (ExSystem (GLFWError "drawFrame"))
                  "No GLFW window initialized"
                  mkErrorContext
    Just win → Right win

-----------------------------------------------------------
-- Utility Functions
-----------------------------------------------------------

-- | Safe vector access by index
safeVectorIndex ∷ V.Vector a → Int → Maybe a
safeVectorIndex vec idx
  | idx ≥ 0 ∧ idx < V.length vec = Just (vec V.! idx)
  | otherwise = Nothing

safeVectorHead ∷ V.Vector a → Maybe a
safeVectorHead vec
  | V.null vec = Nothing
  | otherwise  = Just (V.head vec)
