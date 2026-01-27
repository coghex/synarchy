module Engine.Core.State where
import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.IORef (IORef)
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Core.Base
import Engine.Core.Types
import Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types
import Engine.Event.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Capability (TextureSystemCapability(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Texture.Types
import Engine.Graphics.Window.Types
import Engine.Graphics.Camera
import Engine.Graphics.Font.Data
import Engine.Input.Types
import Engine.Input.Bindings
import Engine.Scene.Base
import Engine.Scene.Types
import qualified Vulkan.Core10 as Vk
import UI.Focus (FocusManager)

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig     ∷ EngineConfig
  , eventQueue       ∷ Q.Queue Event
  , inputQueue       ∷ Q.Queue InputEvent
  , logFunc          ∷ LoggingFunc
  , luaToEngineQueue ∷ Q.Queue LuaToEngineMsg
  , luaQueue         ∷ Q.Queue LuaMsg
  , lifecycleRef     ∷ IORef EngineLifecycle
  , assetPoolRef     ∷ IORef AssetPool
  , nextObjectIdRef  ∷ IORef Word32
  , fontCacheRef     ∷ IORef FontCache
  , inputStateRef    ∷ IORef InputState
  , keyBindingsRef   ∷ IORef KeyBindings
  , textBuffersRef   ∷ IORef (Map.Map ObjectId Text)
  , cameraRef        ∷ IORef Camera2D
  , uiCameraRef      ∷ IORef UICamera
  , focusManagerRef  ∷ IORef FocusManager
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { timingState      ∷ TimingState
  , inputState       ∷ InputState
  , graphicsState    ∷ GraphicsState
  , assetPool        ∷ AssetPool
  , assetConfig      ∷ AssetConfig
  , sceneManager     ∷ SceneManager
  }

data EngineLifecycle
  = EngineStarting        -- engine is initializing
  | EngineRunning         -- engine is running normally
  | CleaningUp            -- engine is cleaning up resources
  | EngineStopped         -- engine has stopped
  deriving (Eq, Show)

data TimingState = TimingState
  { frameCount       ∷ Word64
  , currentTime      ∷ Double
  , deltaTime        ∷ Double
  , frameTimeAccum   ∷ Double
  , lastFrameTime    ∷ Double
  , targetFPS        ∷ Double
  }

data GraphicsState = GraphicsState
  { glfwWindow         ∷ Maybe Window
  , vulkanInstance     ∷ Maybe Vk.Instance
  , vulkanPDevice      ∷ Maybe Vk.PhysicalDevice
  , vulkanDevice       ∷ Maybe Vk.Device
  , textureCapability  ∷ Maybe TextureSystemCapability
  , deviceQueues       ∷ Maybe DevQueues
  , vulkanCmdPool      ∷ Maybe Vk.CommandPool
  , vulkanCmdBuffers   ∷ Maybe (V.Vector Vk.CommandBuffer)
  , vulkanRenderPass   ∷ Maybe Vk.RenderPass
  , descriptorState    ∷ Maybe DescriptorManager
  , pipelineState      ∷ Maybe PipelineState
  , frameResources     ∷ V.Vector FrameResources
  , currentFrame       ∷ Word32
  , framebuffers       ∷ Maybe (V.Vector Vk.Framebuffer)
  , swapchainInfo      ∷ Maybe SwapchainInfo
  , syncObjects        ∷ Maybe SyncObjects
  , vertexBuffer       ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , uniformBuffers     ∷ Maybe (V.Vector (Vk.Buffer, Vk.DeviceMemory))
  , textureSystem      ∷ Maybe BindlessTextureSystem
  , bindlessPipeline   ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , bindlessUIPipeline ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , cleanupStatus      ∷ CleanupStatus
  , fontPipeline       ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , fontUIPipeline     ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , fontQuadBuffer     ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , fontDescriptorPool ∷ Maybe Vk.DescriptorPool
  , fontDescriptorLayout ∷ Maybe Vk.DescriptorSetLayout
  -- instance buffers that survive across frames
  , pendingInstanceBuffers ∷ V.Vector (Vk.Buffer, Vk.DeviceMemory)
  }


