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
import Engine.Lua.Types
import Engine.Event.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Window.Types
import Engine.Graphics.Camera
import Engine.Input.Types
import Engine.Scene.Types
import qualified Vulkan.Core10 as Vk

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig     ∷ EngineConfig
  , eventQueue       ∷ Q.Queue Event
  , inputQueue       ∷ Q.Queue InputEvent
  , logQueue         ∷ Q.Queue T.Text
  , luaEventQueue    ∷ Q.Queue LuaEvent
  , luaCommandQueue  ∷ Q.Queue LuaCommand
  , lifecycleRef     ∷ IORef EngineLifecycle
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { timingState      ∷ TimingState
  , inputState       ∷ InputState
  , logFunc          ∷ LoggingFunc
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
  , deviceQueues       ∷ Maybe DevQueues
  , vulkanCmdPool      ∷ Maybe Vk.CommandPool
  , vulkanCmdBuffers   ∷ Maybe (V.Vector Vk.CommandBuffer)
  , vulkanRenderPass   ∷ Maybe Vk.RenderPass
  , textureState       ∷ TextureState
  , descriptorState    ∷ Maybe DescriptorManager
  , pipelineState      ∷ Maybe PipelineState
  , frameResources     ∷ V.Vector FrameResources
  , currentFrame       ∷ Word32
  , framebuffers       ∷ Maybe (V.Vector Vk.Framebuffer)
  , swapchainInfo      ∷ Maybe SwapchainInfo
  , syncObjects        ∷ Maybe SyncObjects
  , vertexBuffer       ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , uniformBuffers     ∷ Maybe (V.Vector (Vk.Buffer, Vk.DeviceMemory))
  , textureArrayStates ∷ Map.Map T.Text TextureArrayState
  , camera2D           ∷ Camera2D
  , cleanupStatus      ∷ CleanupStatus
  }

-- | Asset pool containing all loaded assets
data AssetPool = AssetPool
  { apTextureAtlases    ∷ Map.Map AssetId TextureAtlas
  , apShaderPrograms    ∷ Map.Map AssetId ShaderProgram
  , apAssetPaths        ∷ Map.Map T.Text AssetId
  , apNextId            ∷ Word32
  }

-- | Asset loading configuration
data AssetConfig = AssetConfig
  { acMaxTextureAtlases ∷ Word32
  , acMaxShaderPrograms ∷ Word32
  , acPreloadAssets     ∷ Bool
  , acEnableHotReload   ∷ Bool
  } deriving (Show)


