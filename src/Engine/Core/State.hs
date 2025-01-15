module Engine.Core.State where
import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word64, Word32)
import Engine.Asset.Types
import Engine.Core.Base
import Engine.Core.Types
import Engine.Core.Queue as Q
import Engine.Event.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Window.Types
import Engine.Input.Types
import qualified Vulkan.Core10 as Vk

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig     ∷ EngineConfig
  , eventQueue       ∷ Q.Queue Event
  , inputQueue       ∷ Q.Queue InputEvent
  , logQueue         ∷ Q.Queue T.Text
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { timingState      ∷ TimingState
  , inputState       ∷ InputState
  , logFunc          ∷ LoggingFunc
  , graphicsState    ∷ GraphicsState
  , assetPool        ∷ AssetPool
  , assetConfig      ∷ AssetConfig
  }

data TimingState = TimingState
  { frameCount       ∷ Word64
  , engineRunning    ∷ Bool
  , currentTime      ∷ Double
  , deltaTime        ∷ Double
  , frameTimeAccum   ∷ Double
  , lastFrameTime    ∷ Double
  , targetFPS        ∷ Double
  }

data GraphicsState = GraphicsState
  { glfwWindow       ∷ Maybe Window
  , vulkanInstance   ∷ Maybe Vk.Instance
  , vulkanPDevice    ∷ Maybe Vk.PhysicalDevice
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

