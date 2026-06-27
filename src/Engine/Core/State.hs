module Engine.Core.State where
import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef)
import Data.Time.Clock (UTCTime)
import Data.Sequence (Seq)
import Control.Concurrent.STM.TVar (TVar)
import System.Random (StdGen)
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Asset.YamlTextures
import Engine.Core.Log
import Engine.Core.Types
import Engine.Core.Queue as Q
import Engine.PlayerEvent (PlayerEvent, NotificationCfg)
import qualified Combat.Types
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
import Engine.Graphics.Vulkan.Sampler.Types (SamplerCache)
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
import Structure.Palette (TexPalette)
import Item.Types (ItemManager)
import Equipment.Types (EquipmentClassManager)
import Substance.Types (SubstanceManager)
import Infection.Types (InfectionManager)
import World.Types (WorldCommand, WorldManager, FloraCatalog
                   , WorldState, WorldPageId, wmWorlds, wmVisible)
import World.Material (MaterialRegistry)
import World.Generate.Config (WorldGenConfig)
import Sim.Command.Types (SimCommand)

data EngineEnv = EngineEnv
  { engineConfig        ∷ EngineConfig
  , engineStateRef      ∷ IORef EngineState
    -- ^ Main-thread-private mutable engine state (timing / graphics /
    --   scene / asset config). Lives here so 'EngineM' can carry it via
    --   the immutable env instead of a second CPS parameter. Only the
    --   main render thread reads or writes it (see the EngineState
    --   invariant below), so a plain 'IORef' (no STM) is correct.
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
  -- | The page id whose selection the global HUD info panel currently
  --   reflects. 'pollCursorInfo' uses it to force a HUD refresh when the
  --   active world changes (e.g. world.show/hide swaps 'wmVisible' without
  --   touching any cursor field, so the per-world snapshot alone can't
  --   detect the switch — issue #129).
  , hudActivePageRef    ∷ IORef (Maybe WorldPageId)
  , worldQueue          ∷ Q.Queue WorldCommand
  , sunAngleRef         ∷ IORef Float
  , worldPreviewRef     ∷ IORef (Maybe (Int, Int, BS.ByteString))
  , zoomAtlasDataRef    ∷ IORef (Maybe (Int, Int, BS.ByteString))  -- ^ Pending zoom atlas pixel data for GPU upload
  , worldQuadsRef       ∷ IORef (V.Vector SortableQuad)
  , textureSystemRef    ∷ IORef (Maybe BindlessTextureSystem)
  , samplerCacheRef     ∷ IORef SamplerCache
    -- ^ Deduplicated, refcounted Vulkan samplers keyed by 'SamplerKind'.
    --   The engine needs only a handful of distinct sampler configs
    --   (texture nearest/linear + font), so every sampler is acquired
    --   from this cache rather than minted per atlas/font. At most one
    --   'VkSampler' per kind is alive at a time. Destroyed wholesale at
    --   shutdown via 'destroySamplerCache'.
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
  , texPaletteRef       ∷ IORef TexPalette
    -- ^ Save-level texture PALETTE (path↔id). Structure edits store palette
    --   ids; this interns paths → ids at placement and resolves ids → paths
    --   at render. Saved as sdTexPalette, restored on load.
  , texPaletteHandlesRef ∷ IORef (HM.HashMap Int TextureHandle)
    -- ^ Runtime paletteId → texture handle (the "translation table"; NOT
    --   saved — rebuilt per session). Populated at placement (the handle is
    --   already loaded) and lazily after load (Lua re-resolves each palette
    --   path). The structure renderer reads it; a palette id with no entry
    --   yet is skipped (renders once its handle is resolved).
  , buildingQueue       ∷ Q.Queue BuildingCommand
  , combatQueue         ∷ Q.Queue Combat.Types.CombatCommand
    -- ^ Lua / AI → combat thread. Issued via `combat.attack` (and
    --   future combat commands). Drained at the combat thread's tick
    --   rate (60 Hz) by `Combat.Thread.processAllCommands`.
  , combatEventsRef     ∷ IORef (Seq Combat.Types.CombatEvent)
    -- ^ Combat thread → Lua. Resolution produces events; Lua drains
    --   them via `combat.drainEvents` and pipes into the combat-log
    --   UI. Runtime only, not persisted to SaveData.
  , injuryEventsRef     ∷ IORef (Seq Combat.Types.CombatEvent)
    -- ^ NON-combat injury stream (falls / hazards / wound-caused
    --   deaths) → Lua. Reuses the CombatEvent shape (target = victim).
    --   Producers: Unit.Fall, unit.injure, and `injury.emit` from Lua;
    --   drained via `injury.drainEvents` into the injury-log UI.
    --   Runtime only, not persisted.
  , buildingGhostRef    ∷ IORef (Maybe BuildingGhost)
    -- ^ Single-slot ghost preview during placement mode. Lua sets and
    --   clears via the build_tool module; the render path picks it up
    --   each frame and draws an alpha-blended (and possibly red-tinted)
    --   sprite at the hovered tile.
  , worldGenConfigRef   ∷ IORef WorldGenConfig
  , simQueue           ∷ Q.Queue SimCommand
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
  , lastSaveTimeRef    ∷ IORef UTCTime
    -- ^ Wall-clock time of the most recently issued save (see
    --   `Engine.Scripting.Lua.API.Save.saveWorldFn`). Each save clamps
    --   its timestamp to strictly exceed this so back-to-back saves get
    --   monotonically increasing, microsecond-distinct timestamps even
    --   within the same wall millisecond — the save list sorts
    --   newest-first lexicographically and would otherwise misorder ties
    --   (#98). Seeded to the POSIX epoch so the first save always uses
    --   the real wall clock.
  , itemManagerRef     ∷ IORef ItemManager
    -- ^ Registry of all item defs loaded from data/items/*.yaml.
    --   Lua's item.loadYaml writes into this; unit spawn reads from
    --   it to materialise starting_inventory entries.
  , equipmentClassManagerRef ∷ IORef EquipmentClassManager
    -- ^ Registry of equipment classes loaded from data/equipment/*.yaml.
    --   Lua's equipment.loadYaml writes into this; the unit-info v2
    --   equipment section reads it to lay out slot boxes per class.
  , substanceManagerRef ∷ IORef SubstanceManager
    -- ^ Registry of worked-material substances (steel, bronze, leather,
    --   etc.) loaded from data/substances/*.yaml. Carries physical
    --   properties (density, tensile strength, fracture toughness, …)
    --   that the future combat system will consume. Distinct from the
    --   tile-rendering material system (`World.Material`).
  , infectionManagerRef ∷ IORef InfectionManager
    -- ^ Registry of infection defs (staph, gas gangrene, …) loaded from
    --   data/infections/*.yaml. The wound tick selects one (climate +
    --   site weighted) when a wound first festers; its aggressiveness /
    --   curable_by drive growth + cure.
  , eventStoreRef      ∷ TVar (Seq PlayerEvent)
    -- ^ Ring buffer of player-facing events (~1000 entries; oldest
    --   dropped). Per-session only — not serialized to save files.
    --   Multi-writer: world, unit, and Lua threads all push via
    --   'Engine.PlayerEvent.emitEvent'. Read atomically by Lua-side
    --   queries (e.g. the event-log panel).
  , notificationCfgRef ∷ IORef NotificationCfg
    -- ^ Resolved notification settings keyed by category id. Loaded
    --   at boot from 'data/notification_categories.yaml' merged with
    --   'config/notifications.yaml'. Wrapped in an IORef so the
    --   Phase 2 settings tab can update it at runtime (each checkbox
    --   toggle writes both the IORef and the overrides YAML). The
    --   emitEvent read path takes a single 'readIORef' per call —
    --   negligible overhead even from the world thread.
  , notificationOrder  ∷ ![Text]
    -- ^ Registry-order list of category ids, captured at boot from
    --   'data/notification_categories.yaml'. Immutable for the
    --   session — categories can't be added/removed at runtime, only
    --   their flags toggled. The settings tab uses this to render
    --   rows in the YAML order rather than HashMap iteration order.
  , popupQueueRef      ∷ TVar (Seq PlayerEvent)
    -- ^ Events with popup display enabled, waiting to be picked up
    --   by the Lua popup module. The Lua side drains this via the
    --   'LuaShowPopup' broadcast; this TVar exists for inspection /
    --   debug querying and as a Phase 2 stable source for the
    --   notifications panel.
  } deriving (Eq)

-- | Main-thread-private engine state, threaded through 'EngineM'.
--   INVARIANT (audit 2026-06, Tier-1 decision): only the main render
--   thread reads or writes this — worker threads run in plain IO and
--   cannot reach it. Any state that must cross the thread boundary
--   lives in 'EngineEnv' as an 'IORef' instead; never duplicate a
--   field across the two (that was the textureSystem/inputState bug).
data EngineState = EngineState
  { timingState      ∷ TimingState
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

-- | A replaceable GPU texture upload (zoom atlas / world preview).
--   Re-uploaded on every world init/load; the previous generation is
--   destroyed when superseded (Engine.Scripting.Lua.Message) and the
--   last one at engine shutdown. 'ttCleanup' destroys view, image, and
--   memory (explicit — these deliberately do NOT go through
--   allocResource, which would defer destruction to exit) and releases
--   the texture's shared sampler reference back to the sampler cache.
data TransientTexture = TransientTexture
  { ttHandle  ∷ TextureHandle
  , ttCleanup ∷ IO ()
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
  , renderFinishedSems ∷ V.Vector Vk.Semaphore
    -- ^ One per swapchain IMAGE — vkQueuePresentKHR must wait on a
    --   per-image semaphore (image count ≠ frames in flight).
    --   (Re)created with the swapchain; destroyed via vulkanCleanup.
  , swapchainInfo      ∷ Maybe SwapchainInfo
  , msaaColorImage     ∷ Maybe (Vk.Image, Vk.DeviceMemory, Vk.ImageView)  , vertexBuffer       ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
  , uniformBuffers     ∷ Maybe (V.Vector (Vk.Buffer, Vk.DeviceMemory))
  -- textureSystem + defaultFaceMapSlot moved to EngineEnv
  -- (textureSystemRef / defaultFaceMapSlotRef): worker threads read
  -- them, so per the EngineState invariant above they live in EngineEnv.
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
  , dynamicVertexBuffers   ∷ V.Vector (Maybe SceneDynamicBuffer)
    -- ^ One per frame in flight. The frame slot's fence-wait guarantees
    --   the GPU finished with the slot's buffer before the CPU rewrites
    --   or grows (destroys + reallocates) it.
  , textInstanceBuffers    ∷ V.Vector (Maybe TextInstanceBuffer)
    -- ^ Per frame in flight, same discipline as dynamicVertexBuffers.
  , previewTexture         ∷ Maybe TransientTexture
    -- ^ Current world-preview upload; replaced per world init/load.
  , zoomAtlasTexture       ∷ Maybe TransientTexture
    -- ^ Current zoom-atlas upload; replaced per world init/load.
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

-- | The single canonical "active world" resolution rule. Every read of
--   "the current world" must go through this (or 'activeWorldState' /
--   'activeWorldPage') rather than pattern-matching @wmWorlds@/@wmVisible@
--   inline — historically scattered code grabbed the head of @wmWorlds@
--   (registration order) and acted on the wrong world (see epic #101).
--
--   Rule: the first visible world wins. If none are marked visible (a
--   brief mid-transition window) fall back to the head of @wmWorlds@.
--   Returns Nothing when no worlds are registered (main menu) or when the
--   visible head has no backing 'WorldState' yet (do not silently fall
--   through to a different world in that case).
resolveActiveWorld ∷ WorldManager → Maybe (WorldPageId, WorldState)
resolveActiveWorld mgr = case wmVisible mgr of
    (pid:_) → (\ws → (pid, ws)) <$> lookup pid (wmWorlds mgr)
    []      → case wmWorlds mgr of
        (pw:_) → Just pw
        []     → Nothing

-- | 'resolveActiveWorld' over the live 'worldManagerRef', returning the
--   active world's page id together with its state.
activeWorldPage ∷ EngineEnv → IO (Maybe (WorldPageId, WorldState))
activeWorldPage env = resolveActiveWorld <$> readIORef (worldManagerRef env)

-- | The active world's 'WorldState' (its page id discarded). The common
--   case for current-world reads that don't need the page id.
activeWorldState ∷ EngineEnv → IO (Maybe WorldState)
activeWorldState env = fmap snd <$> activeWorldPage env
