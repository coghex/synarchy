module Engine.Core.Types
  ( EngineState(..)
  , EngineEnv(..)
  , EngineError(..)
  , EngineConfig(..)
  , LoggingFunc
  , TextureState(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import qualified Vulkan.Core10 as Vk
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture (TextureState(..))
import Engine.Graphics.Window.Types
import Engine.Core.Queue
import Engine.Event.Types
import Engine.Input.Types

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig     ∷ EngineConfig
  , eventQueue       ∷ Queue Event
  , inputQueue       ∷ Queue InputEvent
  , logQueue         ∷ Queue T.Text
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { frameCount       ∷ Word64
  , engineRunning    ∷ Bool
  , currentTime      ∷ Double
  , deltaTime        ∷ Double
  , frameTimeAccum   ∷ Double
  , lastFrameTime    ∷ Double
  , targetFPS        ∷ Double
  , inputState       ∷ InputState
  , logFunc          ∷ LoggingFunc
  , glfwWindow       ∷ Maybe Window
  , vulkanInstance   ∷ Maybe Vk.Instance
  , vulkanDevice     ∷ Maybe Vk.Device
  , deviceQueues     ∷ Maybe DevQueues
  , vulkanCmdPool    ∷ Maybe Vk.CommandPool
  , vulkanCmdBuffers ∷ Maybe (V.Vector Vk.CommandBuffer)
  , vulkanRenderPass ∷ Maybe Vk.RenderPass
  , textureState     ∷ TextureState
  , descriptorState  ∷ Maybe DescriptorManager
  , pipelineState    ∷ Maybe PipelineState
  , frameResources   ∷ V.Vector FrameResources
  , currentFrame     ∷ Word32
  , framebuffers     ∷ Maybe (V.Vector Vk.Framebuffer)
  , swapchainInfo    ∷ Maybe SwapchainInfo
  , syncObjects      ∷ Maybe SyncObjects
  , vertexBuffer     ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , uniformBuffers   ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  }

type LoggingFunc = Logger.Loc → Logger.LogSource → Logger.LogLevel
                              → Logger.LogStr → IO ()

-- | Engine configuration
data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  }

-- | Engine errors
data EngineError
  = VulkanError   T.Text
  | ResourceError T.Text
  | StateError    T.Text
  | InitError     T.Text
  deriving (Show, Eq)
