module Engine.Core.State where
import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef)
import System.Random (StdGen)
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Asset.YamlTextures
import Engine.Core.Log
import Engine.Core.Types
import Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types
import Engine.Event.Types
import Engine.Graphics.Types
import Engine.Graphics.Config
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Capability (TextureSystemCapability(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Cleanup
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
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import UI.Types (UIPageManager)
import UI.Focus (FocusManager)
import Unit.Types (UnitManager)
import Unit.Sim.Types (UnitThreadState)
import Unit.Command.Types (UnitCommand)
import Building.Types (BuildingManager, BuildingGhost)
import Building.Command.Types (BuildingCommand)
import Item.Types (ItemManager)
import World.Types (WorldCommand, WorldManager, FloraCatalog)
import World.Material (MaterialRegistry)
import World.Generate.Config (WorldGenConfig)
import Sim.Command.Types (SimCommand)

data EngineEnv = EngineEnv
  { engineConfig        ∷ EngineConfig
  , videoConfigRef      ∷ IORef VideoConfig
  , windowSizeRef       ∷ IORef (Int, Int)
  , windowStateRef      ∷ IORef WindowState
  , framebufferSizeRef  ∷ IORef (Int, Int)
  , fpsRef              ∷ IORef Double
  , brightnessRef       ∷ IORef Int
  , pixelSnapRef        ∷ IORef Bool
  , textureFilterRef    ∷ IORef TextureFilter
  , eventQueue          ∷ Q.Queue Event
  , inputQueue          ∷ Q.Queue InputEvent
  , loggerRef           ∷ IORef LoggerState
  , luaToEngineQueue    ∷ Q.Queue LuaToEngineMsg
  , luaQueue            ∷ Q.Queue LuaMsg
  , lifecycleRef        ∷ IORef EngineLifecycle
  , assetPoolRef        ∷ IORef AssetPool
  , textureNameRegistryRef ∷ IORef TextureNameRegistry
  , nextObjectIdRef     ∷ IORef Word32
  , fontCacheRef        ∷ IORef FontCache
  , inputStateRef       ∷ IORef InputState
  , keyBindingsRef      ∷ IORef KeyBindings
  , textBuffersRef      ∷ IORef (Map.Map ObjectId Text)
  , cameraRef           ∷ IORef Camera2D
  , uiCameraRef         ∷ IORef UICamera
  , uiManagerRef        ∷ IORef UIPageManager
  , focusManagerRef     ∷ IORef FocusManager
  , worldManagerRef     ∷ IORef WorldManager
  , worldQueue          ∷ Q.Queue WorldCommand
  , sunAngleRef         ∷ IORef Float
  , worldPreviewRef     ∷ IORef (Maybe (Int, Int, BS.ByteString))
  , zoomAtlasDataRef    ∷ IORef (Maybe (Int, Int, BS.ByteString))  -- ^ Pending zoom atlas pixel data for GPU upload
  , worldQuadsRef       ∷ IORef (V.Vector SortableQuad)
  , textureSystemRef    ∷ IORef (Maybe BindlessTextureSystem)
  , textureSizeRef      ∷ IORef (HM.HashMap TextureHandle (Int, Int))
  , defaultFaceMapSlotRef  ∷ IORef Word32
  , floraCatalogRef     ∷ IORef FloraCatalog
  , materialRegistryRef   ∷ IORef MaterialRegistry
  , unitManagerRef      ∷ IORef UnitManager
  , unitQueue           ∷ Q.Queue UnitCommand
  , utsRef              ∷ IORef UnitThreadState
    -- ^ Sim-side per-unit state (position, pose, activity, target,
    --   path, *Until timers). Lives on EngineEnv (not encapsulated in
    --   the unit thread) so the save/load handler can snapshot and
    --   restore it; the unit thread treats it as the sole authority
    --   for movement and timed states.
  , statRNGRef          ∷ IORef StdGen
    -- ^ Runtime RNG for stat rolls. Seeded from system entropy at
    --   startup; not tied to the world seed (stats are non-deterministic
    --   across runs by design).
  , buildingManagerRef  ∷ IORef BuildingManager
  , buildingQueue       ∷ Q.Queue BuildingCommand
  , buildingGhostRef    ∷ IORef (Maybe BuildingGhost)
    -- ^ Single-slot ghost preview during placement mode. Lua sets and
    --   clears via the build_tool module; the render path picks it up
    --   each frame and draws an alpha-blended (and possibly red-tinted)
    --   sprite at the hovered tile.
  , worldGenConfigRef   ∷ IORef WorldGenConfig
  , simQueue           ∷ Q.Queue SimCommand
  , frameCounterRef    ∷ IORef Word64  -- ^ Monotonic frame counter for animations
  , enginePausedRef    ∷ IORef Bool
    -- ^ Global pause flag. When True, threads that advance simulated
    --   state (unit movement, sim ticks) skip their work. Rendering,
    --   input dispatch, command processing, and camera movement keep
    --   running so the player can still interact while paused. Set via
    --   `engine.setPaused` from Lua.
  , gameTimeRef        ∷ IORef Double
    -- ^ Monotonic game-clock in seconds. Advances by real-tick dt
    --   only when `enginePausedRef` is False. All gameplay timestamps
    --   that need to freeze on pause (uiAnimStart, biSpawnedAt,
    --   usReviveUntil) reference this clock instead of POSIX wall-time.
    --   Updated by Unit.Thread.unitLoop once per tick.
  , itemManagerRef     ∷ IORef ItemManager
    -- ^ Registry of all item defs loaded from data/items/*.yaml.
    --   Lua's item.loadYaml writes into this; unit spawn reads from
    --   it to materialise starting_inventory entries.
  } deriving (Eq)

data EngineState = EngineState
  { timingState      ∷ TimingState
  , inputState       ∷ InputState
  , graphicsState    ∷ GraphicsState
  , assetConfig      ∷ AssetConfig
  , sceneManager     ∷ SceneManager
  }

data EngineLifecycle
  = EngineStarting
  | EngineRunning
  | CleaningUp
  | EngineStopped
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
  , vulkanSurface      ∷ Maybe SurfaceKHR
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
  , msaaColorImage     ∷ Maybe (Vk.Image, Vk.DeviceMemory, Vk.ImageView)
  , syncObjects        ∷ Maybe SyncObjects
  , vertexBuffer       ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , uniformBuffers     ∷ Maybe (V.Vector (Vk.Buffer, Vk.DeviceMemory))
  , textureSystem      ∷ Maybe BindlessTextureSystem
  , defaultFaceMapSlot ∷ Word32
  , bindlessPipeline   ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , bindlessUIPipeline ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , fontPipeline       ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , fontUIPipeline     ∷ Maybe (Vk.Pipeline, Vk.PipelineLayout)
  , fontQuadBuffer     ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , fontDescriptorPool ∷ Maybe Vk.DescriptorPool
  , fontDescriptorLayout   ∷ Maybe Vk.DescriptorSetLayout
  , pendingInstanceBuffers ∷ V.Vector (Vk.Buffer, Vk.DeviceMemory)
  , cleanupStatus          ∷ CleanupStatus
  , vulkanCleanup          ∷ Cleanup
  , dynamicVertexBuffer    ∷ Maybe SceneDynamicBuffer
  , textInstanceBuffer     ∷ Maybe TextInstanceBuffer
  }

-- | Cached windowed-mode geometry so we can restore position\/size after fullscreen
data WindowState = WindowState
  { wsWindowedPos  ∷ (Int, Int)   -- ^ Last known windowed position
  , wsWindowedSize ∷ (Int, Int)   -- ^ Last known windowed size (screen coords)
  } deriving (Show)

defaultWindowState ∷ WindowState
defaultWindowState = WindowState
  { wsWindowedPos  = (100, 100)
  , wsWindowedSize = (800, 600)
  }
