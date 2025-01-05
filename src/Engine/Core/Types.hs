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
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture (TextureState(..))

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig     ∷ EngineConfig
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { frameCount       ∷ Word64
  , engineRunning    ∷ Bool
  , currentTime      ∷ Double
  , deltaTime        ∷ Double
  , logFunc          ∷ LoggingFunc
  , vulkanInstance   ∷ Maybe Vk.Instance
  , vulkanDevice     ∷ Maybe Vk.Device
  , vulkanCmdPool    ∷ Maybe Vk.CommandPool
  , vulkanCmdBuffers ∷ Maybe (V.Vector Vk.CommandBuffer)
  , vulkanRenderPass ∷ Maybe Vk.RenderPass
  , textureState     ∷ TextureState
  , descriptorState  ∷ Maybe DescriptorManager
  , pipelineState    ∷ Maybe PipelineState
  , currentFrame     ∷ Word32
  , framebuffers     ∷ Maybe (V.Vector Vk.Framebuffer)
  , swapchainExtent  ∷ Maybe Vk.Extent2D
  , syncObjects      ∷ Maybe SyncObjects
  , vertexBuffer     ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
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
