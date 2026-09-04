-- | The engine's central state records.
--
--   The export list re-exports "Engine.Core.Lifecycle" alongside this
--   module's own definitions, so 'EngineLifecycle' and
--   'requestEngineCleanup' — moved there by #2283 so the shared worker
--   lifecycle can own the fail-stop transition without importing this
--   module — stay importable from here exactly as before.
module Engine.Core.State
  ( module Engine.Core.State
  , module Engine.Core.Lifecycle
  ) where
import UPrelude
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.Lifecycle
import Data.Time.Clock (UTCTime)
import Data.Sequence (Seq)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TVar (TVar)
import System.Random (StdGen)
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Asset.TextureNameRegistry (TextureNameRegistry)
import Engine.Core.Log
import Engine.Core.Types
import Engine.Core.Queue as Q
import Engine.PlayerEvent (PlayerEvent, EventStore, NotificationCfg)
import qualified Combat.Types
import Engine.ActionOutcome (ActionOutcome)
import Engine.Scripting.Lua.Types
import Engine.Graphics.Solar (SolarBase(..))
import Engine.Graphics.Types
import Engine.Graphics.Config
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Capability (TextureSystemCapability(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Cleanup
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Texture.Types
import Engine.Graphics.Vulkan.Sampler.Types (SamplerCache)
import Engine.Graphics.Window.Types
import Engine.Graphics.Camera
import Engine.Graphics.Font.Data
import Engine.Input.Types
import Engine.Input.Bindings
import qualified Graphics.UI.GLFW as GLFW
import Engine.Scene.Base
import Engine.Scene.Types
import Engine.Scene.Stats (SceneStats)
import qualified Vulkan.Core10 as Vk
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import UI.Types (UIPageManager)
import UI.ShellFocus (FocusManager)
import Unit.Types (UnitManager)
import Unit.Sim.Types (UnitThreadState)
import Unit.Command.Types (UnitCommand)
import Building.Types (BuildingManager, BuildingGhost)
import Building.Command.Types (BuildingCommand)
import Structure.Palette (TexPalette)
import Structure.WallCatalog (StructureWallCatalog)
import Structure.ArtCatalog (StructureArtCatalog)
import Item.Types (ItemManager)
import Equipment.Types (EquipmentClassManager)
import Substance.Types (SubstanceManager)
import Infection.Types (InfectionManager)
import Craft.Types (RecipeManager)
import Location.Types (LocationRegistry)
import LootTable.Types (LootTableRegistry)
import Tutorial.Types (TutorialRegistry)
import World.Types (WorldCommand, WorldManager, FloraCatalog
                   , WorldState, WorldPageId, wmWorlds, wmVisible
                   , BloodTextureHandles)
import World.Material (MaterialRegistry)
import World.Generate.Config (WorldGenConfig)
import Unit.Pathing.Config (PathingConfig)
import Sim.Command.Types (SimCommand)
import Engine.Save.Barrier (SaveBarrier)
import Engine.Load.Status (LoadStatusRef)
import World.Load.Types (StagedSession)

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
  , windowPosRef        ∷ IORef (Int, Int)
    -- ^ The window's screen position as of the last geometry publish the
    --   main render thread made (window creation, a Lua-driven resolution
    --   change, a window-mode switch). It exists so the windowed-geometry
    --   restore (#907) is OBSERVABLE from outside the render thread:
    --   'GLFW.getWindowPos' may only be called on the main thread, so a
    --   Lua-side check cannot read the position itself. Deliberately NOT
    --   a live cursor of where the window is — no GLFW window-position
    --   callback is installed, so a user dragging the window leaves this
    --   stale until the next publish. Diagnostic reads (@debug.getWindowPos@)
    --   drive a publish first; see 'tools/video_window_check.py'.
  , windowStateRef      ∷ IORef WindowState
  , framebufferSizeRef  ∷ IORef (Int, Int)
  , framebufferMinimizeGenRef ∷ IORef Word64
    -- ^ How many times the framebuffer has been observed at zero area
    --   (#1693). Monotonic, never reset, wrap irrelevant at one bump
    --   per minimize.
    --
    --   'framebufferSizeRef' alone cannot express a minimize, because
    --   it holds a LEVEL and a minimize is an EDGE: a @0x0@ event and a
    --   restore to the pre-minimize size can both be drained before the
    --   render thread next looks, leaving the ref exactly where it
    --   started with no trace that the swapchain was ever invalidated.
    --   Only the input thread sees every event, so only the input
    --   thread can record that edge — which is why this is shared
    --   state and not 'Engine.Core.State.GraphicsState'.
    --
    --   Read together with 'framebufferSizeRef' by
    --   "Engine.Graphics.Vulkan.ResizeRequest"; the two reads are not
    --   atomic, and deliberately need not be — see that module.
  , fpsRef              ∷ IORef Double
  , brightnessRef       ∷ IORef Int
  , pixelSnapRef        ∷ IORef Bool
  , textureFilterRef    ∷ IORef TextureFilter
  , inputQueue          ∷ Q.Queue InputEvent
  , inputBarrierNextRef ∷ TVar Int
    -- ^ Monotonic allocator for 'InputBarrier' tokens
    --   ('Engine.Input.Inject.newBarrierToken') — each synthetic
    --   injection call gets its OWN, numerically higher token, never
    --   reused.
  , inputBarrierRef     ∷ TVar Int
    -- ^ The highest 'InputBarrier' token the input thread has FULLY
    --   processed — advanced by 'Engine.Input.Thread.Dispatch.processInput'
    --   strictly after that barrier's turn in 'inputQueue' comes up,
    --   which (FIFO, single consumer, single producer thread — tokens
    --   are allocated and pushed in the same order) is only after
    --   every event pushed ahead of it — including its side effects,
    --   e.g. any 'luaQueue' write — has completed. 'inputQueue'
    --   becoming empty (a separate STM transaction from those writes)
    --   is NOT the same fact and races it (#727). Deliberately a
    --   per-call TOKEN, not a shared count of every processed
    --   barrier: real GLFW input never produces a barrier at all (so
    --   unrelated concurrent activity can't satisfy someone else's
    --   wait), and a stale barrier left behind by an earlier caller
    --   that already gave up waiting (timeout) can't satisfy a LATER
    --   caller's wait for its own, numerically higher token either —
    --   a bare shared counter could (#727 review) — see
    --   'Engine.Input.Inject.waitForBarrier'.
  , loggerRef           ∷ IORef LoggerState
  , luaToEngineQueue    ∷ Q.Queue LuaToEngineMsg
  , luaQueue            ∷ Q.Queue LuaMsg
  , lifecycleRef        ∷ IORef EngineLifecycle
  , assetPoolRef        ∷ IORef AssetPool
  , textureNameRegistryRef ∷ IORef TextureNameRegistry
  , nextObjectIdRef     ∷ IORef Word32
  , nextItemInstanceIdRef ∷ IORef Word64
    -- ^ Monotonic allocator for 'iiInstanceId'. Bumped once per genuine
    --   item creation (rolls / spawns) via 'freshItemInstanceId'; moves
    --   preserve the existing id. Seeded to 1 at startup and ASSIGNED
    --   from 'sdNextItemInstanceId' on load -- a plain write, never a
    --   'max' against the discarded session's value, because #763 made a
    --   load a complete session REPLACEMENT rather than a merge
    --   ('World.Load.Publish'). Post-load items still can't collide with
    --   loaded ones (#67): 'World.Save.Snapshot' validates at SAVE time
    --   that every saved 'iiInstanceId' is below the saved allocator.
  , fontCacheRef        ∷ IORef FontCache
  , inputStateRef       ∷ IORef InputState
  , keyBindingsRef      ∷ IORef KeyBindings
  -- | The exact GLFW key currently being dispatched to Lua @onKeyDown@,
  --   set by the Lua thread for the duration of that broadcast and cleared
  --   after. Lets @engine.keyMatchesAction@ resolve a press to the precise
  --   physical key (which side of a merged modifier) without racing the
  --   input thread's shared state. 'Nothing' outside a key-down dispatch.
  , currentKeyDownRef   ∷ IORef (Maybe GLFW.Key)
  -- | The SCENE-OBJECT text cache @engine.getText@ answers from, keyed
  --   by the scene node's own 'ObjectId'. NOT editable-widget text — that
  --   is @UI.TextBuffer@ inside 'uiManagerRef', a different mechanism
  --   with its own code-point coordinate contract. @boot-process@, and
  --   deliberately not reset by
  --   @World.Load.Publish.resetTransientState@: entries are created and
  --   removed with the scene nodes they describe by
  --   "Engine.Scripting.Lua.Message.Scene", its only writer (#1961), so
  --   a session boundary has nothing left to clear.
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
  -- | Runtime-only whole-session LOAD transaction status (issue #763,
  --   save-overhaul C2) — the load-side counterpart to 'saveBarrierRef'.
  --   Diagnostic and coordination state, never part of 'SaveData'. See
  --   "Engine.Load.Status".
  , loadStatusRef       ∷ LoadStatusRef
  -- | Single-slot handoff for a fully-staged, not-yet-published load
  --   (issue #763): written by the world thread once
  --   'World.Command.Types.WorldLoadTransaction' finishes staging, read
  --   (and cleared) by the world thread again when it processes the
  --   matching 'World.Command.Types.WorldLoadPublish'. Keyed by request
  --   id purely as a defensive cross-check — only one load is ever in
  --   flight at a time (enforced by 'loadStatusRef'). Mirrors the
  --   existing single-slot staging handoff pattern 'zoomAtlasDataRef' /
  --   'worldPreviewRef' already use for the render thread.
  , pendingLoadRef      ∷ IORef (Maybe (Int, StagedSession))
  , worldQueue          ∷ Q.Queue WorldCommand
  , sunAngleRef         ∷ IORef SolarBase
    -- ^ The process-global base sun angle plus whether
    --   @world.setSunAngle@ is currently overriding it (#1869). Per-page
    --   attribution rides with the published quads
    --   ('Engine.Scene.Types.Batch.lqSolar'); this is what page-LESS
    --   geometry and the Lua climate queries read.
  , worldPreviewRef     ∷ IORef (Maybe (Int, Int, BS.ByteString, Word64))
    -- ^ Pending world-preview pixel data for GPU upload, tagged with the
    --   generation it was enqueued under (issue #763;
    --   see 'worldPreviewGenerationRef').
  , worldPreviewGenerationRef ∷ IORef Word64
    -- ^ Monotonic counter bumped once per preview enqueue (never read
    --   back down). The upload handler compares the generation it
    --   dequeued against this counter's CURRENT value at delivery time:
    --   if a newer preview has been enqueued since,
    --   this counter has already moved past the dequeued generation, so
    --   the in-flight (now-stale) upload can tell it must not announce
    --   itself — no live-ref re-read of 'worldPreviewRef' itself is
    --   needed, since the counter only ever increases and a plain read
    --   of it is never torn.
  , zoomAtlasDataRef    ∷ IORef (Maybe (Int, Int, BS.ByteString, [WorldState]))
    -- ^ Pending zoom atlas pixel data for GPU upload, plus the EXACT
    --   'WorldState's it belongs to, captured at the moment it was
    --   enqueued (issue #763): the upload can take
    --   multiple frames, and re-reading 'worldManagerRef' only once
    --   the upload finishes would race a load publish that swaps it
    --   in between — this closes that gap completely rather than
    --   narrowing it, since nothing needs to be re-read from a live
    --   ref at write time at all.
  , screenshotRequestQueue ∷ Q.Queue ScreenshotRequest
    -- ^ Pending debug.captureScreenshot requests (#643). The Lua
    --   thread enqueues; the render thread drains one per frame in
    --   'drawFrame' (copies the swapchain image to a staging buffer
    --   inside that frame's command buffer) and replies on the
    --   request's own queue. Never drained under GPU-less headless
    --   mode — the verb checks 'ecHeadless' first and errors out
    --   without enqueueing.
  , worldQuadsRef       ∷ IORef LayeredQuads
    -- ^ World quads split static (pre-sorted per layer at quad-cache
    --   rebuild) / dynamic (per-tick), written by the world thread,
    --   merged + drawn by the frame loop (#446).
  , sceneStatsRef       ∷ IORef (Maybe SceneStats)
    -- ^ Scene-assembly telemetry (#1921): the per-category scanned /
    --   emitted / elapsed-nanosecond measurements the world thread
    --   publishes at the end of every completed 'updateWorldTiles'
    --   pass, beside the 'worldQuadsRef' those quads land in. Read by
    --   the Lua thread (@debug.getSceneStats()@). 'Nothing' means no
    --   completed pass since the last world teardown, which clears it
    --   at the same two sites that clear 'worldQuadsRef' — so the two
    --   can never disagree about whether a world lifecycle ended.
    --   Transient session telemetry: never serialized.
  , textureSystemRef    ∷ IORef (Maybe BindlessTextureSystem)
  , samplerCacheRef     ∷ IORef SamplerCache
    -- ^ Deduplicated, refcounted Vulkan samplers keyed by 'SamplerKind'.
    --   The engine needs only a handful of distinct sampler configs
    --   (texture nearest/linear + font), so every sampler is acquired
    --   from this cache rather than minted per atlas/font. At most one
    --   'VkSampler' per kind is alive at a time. Destroyed wholesale at
    --   shutdown via 'destroySamplerCache'.
  , textureSizeRef      ∷ IORef (HM.HashMap TextureHandle (Int, Int))
  , maxImageDimensionRef ∷ IORef (Maybe Int)
    -- ^ The physical device's actual @maxImageDimension2D@, published
    --   once by 'Engine.Graphics.Vulkan.Init.initializeVulkanCommon' as
    --   soon as a device exists — for both the windowed and the
    --   offscreen path (issue #2020).
    --
    --   It is here, on the one record every thread can reach, for a
    --   single reason: the world thread must know whether a world's zoom
    --   atlas can exist BEFORE it generates the pixels, and
    --   'GraphicsState' — where 'vulkanPDevice' lives — is
    --   main-render-thread-private (§3 of
    --   @docs\/engineenv_capability_inventory.md@). The worker reads this
    --   value; it never reaches the device.
    --
    --   'Nothing' means "no device limit is available". That is the
    --   EXPECTED and correct state under @--headless@ and @--dump@,
    --   which have no GPU at all; there it means no ceiling applies, not
    --   that something failed. Under a GPU-capable mode it means the
    --   query has not happened yet, and
    --   'Engine.Map.ImageAdmission.resolveMapImageCeiling' turns it into
    --   a refusal rather than letting an unchecked image through. The
    --   boot mode — not this field — is what distinguishes the two.
  , bloodDisposeQueue   ∷ Q.Queue (IORef BloodTextureHandles)
    -- ^ Cross-thread GPU-dispose transport for #606 blood textures owned
    --   by a world page the world thread is removing/replacing (#788).
    --   'uploadBloodTextures' only sweeps pages still in 'wmWorlds', so a
    --   removed page's 'wsBloodTextureHandlesRef' would otherwise be
    --   unreachable and its bindless registrations / Vulkan images /
    --   'textureSizeRef' entries leak. The world-thread teardown sites
    --   enqueue the orphaned page's live handle 'IORef' here (never a
    --   snapshot — read at drain time); the render thread drains it in
    --   'World.Render.BloodQuads.disposeQueuedBloodTextures', disposing
    --   whatever remains and emptying the map. Enqueuing the LIVE ref
    --   keeps it disjoint from any still-in-flight FIFO eviction of the
    --   same map (that sweep frees what it removed; the drain frees the
    --   rest), so the two never double-free. Empty and inert headless
    --   (nothing ever uploads, so nothing is ever enqueued with records).
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
  , structureWallCatalogRef ∷ IORef StructureWallCatalog
    -- ^ Directional wall art per structure pack/variant (#1712), keyed by
    --   texture PATH: the four edge sprites and their sixteen cap facemaps,
    --   with the runtime handle Lua already loaded for each. Registered from
    --   `scripts/structures.lua` out of the pack YAML it read
    --   (`structure.registerWallFamily`); read by `Structure.Render` to draw
    --   a wall with the sprite its edge occupies once the camera rotates.
    --   NOT persisted and never cleared — a load replaces the palette (and so
    --   can reassign ids), which is exactly why this is keyed by path.
  , structureArtCatalogRef ∷ IORef StructureArtCatalog
    -- ^ Per-kind art for every UNPLACED structure piece (#1842), keyed by
    --   PACK NAME: the texture/facemap pair the build AI would place for
    --   each kind a pack offers (floor, ceiling, post, the four wall
    --   edges' sixteen cap facemaps, the wire pack's sixteen connection
    --   variants), with the runtime handle Lua already loaded for each,
    --   plus which kinds carry complete `build:` metadata. Registered
    --   from `scripts/structures.lua` / `scripts/wire.lua` out of the
    --   pack YAML they read (`structure.registerPackArt`); read by the
    --   construction render pass, which cannot call into Lua, to answer
    --   what an unplaced designation would be built with. All or nothing
    --   per pack. NOT persisted and never cleared — like
    --   `structureWallCatalogRef` it is keyed by pack name and holds
    --   paths, neither of which a load's palette replacement invalidates.
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
  , thoughtEventsRef    ∷ IORef (Seq Combat.Types.CombatEvent)
    -- ^ Per-unit thought stream (#351) → Lua. Purely Lua-produced —
    --   scripts/thoughts.lua decides when/what via its data-driven
    --   catalogue + trigger predicates and pushes via `thought.emit`
    --   (target = the thinking unit); drained via `thought.drainEvents`
    --   into scripts/thought_log.lua, which unit_log.lua's Thought tab
    --   reads. Reuses the CombatEvent shape, same as injuryEventsRef.
    --   Runtime only, not persisted.
  , actionOutcomeRef    ∷ IORef (Seq ActionOutcome)
    -- ^ F4 (#646) action-outcome oracle tap: what actually happened to a
    --   player action, even when nothing user-facing fired. Producers:
    --   Lua `debug.recordOutcome` (Layer A input routing, Layer B
    --   Lua-owned commit boundaries) and the engine-side designation
    --   handlers in World.Thread.Command.Cursor (partial-drop counts).
    --   Drained via `debug.drainActionOutcomes` by the playtest harness's
    --   critic; never surfaced to the player. Runtime only, not persisted.
  , buildingGhostRef    ∷ IORef (Maybe BuildingGhost)
    -- ^ Single-slot ghost preview during placement mode. Lua sets and
    --   clears via the build_tool module; the render path picks it up
    --   each frame and draws an alpha-blended (and possibly red-tinted)
    --   sprite at the hovered tile.
  , worldGenConfigRef   ∷ IORef WorldGenConfig
  , pathingConfigRef    ∷ IORef PathingConfig
    -- ^ Unit pathing cost tunables (climb/ramp/fall/river/lake
    --   penalties + replan threshold), loaded from
    --   @config/pathing.yaml@ at init (defaults if absent). In an IORef
    --   so a future settings UI can retune routing live; the movement
    --   tick rereads it each tick.
  , simQueue           ∷ Q.Queue SimCommand
  , enginePausedRef    ∷ IORef Bool
    -- ^ Global pause flag. When True, threads that advance simulated
    --   state (unit movement, sim ticks) skip their work. Rendering,
    --   input dispatch, command processing, and camera movement keep
    --   running so the player can still interact while paused. Set via
    --   `engine.setPaused` from Lua.
  , playerIntentGenRef ∷ MVar Word64
    -- ^ #913: a monotonically increasing PLAYER-INTENT generation, bumped
    --   by the two Lua verbs through which a player expresses "I want
    --   the world's clock to run differently" — an applied
    --   `engine.setPaused` and any `world.setTimeScale` request — and by
    --   nothing else. The engine's OWN writes to `enginePausedRef` /
    --   `wsTimeScaleRef` (auto-pause-on-save, load publish, a
    --   pause-flagged notification) deliberately do NOT bump it.
    --   An autosave snapshots this alongside the pre-request pause and
    --   time scale and only restores them if it still matches on
    --   success, so a player who toggles pause twice during a save's
    --   request window still wins even though the final BOOLEAN is
    --   unchanged.
    --
    --   An 'MVar' rather than an 'IORef' because the counter IS the
    --   mutex: a player's pause/time-scale write and the generation bump
    --   are one critical section, and the autosave's compare-then-restore
    --   takes that same lock (see
    --   'Engine.Core.Capability.WorldSim.withPlayerIntent' /
    --   'restoreIfPlayerIdle'). Two plain refs could not close the window
    --   between the world thread reading a matching generation and
    --   writing the pause flag, in which the Lua thread's own pause write
    --   would simply be overwritten. Runtime-only, never part of
    --   'SaveData'.
  , enginePauseGenRef  ∷ IORef Word64
    -- ^ #1730: a monotonically increasing count of pause assertions made
    --   by an engine source that is INDEPENDENT of whatever save
    --   transaction may be running — a `pause: true` notification
    --   category, an `engine.loadSave` acceptance. It is the companion
    --   `playerIntentGenRef` above is for the player: a pause epoch
    --   records no owner, and `World.Pause.imposePauseHeld` is a
    --   complete no-op once the flag is already set, so without this
    --   counter nothing downstream can tell that a second source still
    --   wants the game paused.
    --
    --   Deliberately NOT bumped by the save path's own pause
    --   (`acceptSaveRequest`'s epoch open and the world thread's
    --   re-assertion, `World.Pause.reassertSavePause`): a save may not
    --   count itself as a reason to decline its own restore. Nor by the
    --   player's `engine.setPaused`, which `playerIntentGenRef` already
    --   records, so a declined restore can name the right reason.
    --
    --   An `IORef` rather than a second `MVar` because it is never read
    --   or written outside the `playerIntentGenRef` critical section:
    --   every epoch transition takes that mutex
    --   (`World.Pause.withEpochLock`), and so do the two sites that
    --   snapshot and compare this counter (`acceptSaveRequest`,
    --   `restoreAfterAutosave`). One mutex over both counters is what
    --   makes the restore decision linearizable against a pause landing
    --   beside it. Runtime-only, never part of `SaveData`.
  , gameTimeRef        ∷ IORef Double
    -- ^ Monotonic game-clock in seconds. Advances by real-tick dt
    --   only when `enginePausedRef` is False. All gameplay timestamps
    --   that need to freeze on pause (uiAnimStart, biSpawnedAt,
    --   usReviveUntil) reference this clock instead of POSIX wall-time.
    --   Updated by Unit.Thread.unitTick once per tick.
    --
    --   It is one PROCESS-wide counter, not a per-world one, so it has
    --   three transitions and no others: seeded at boot with
    --   'Engine.Core.SessionEpoch.freshSessionGameTime'; restored to
    --   that same value by @Unit.Thread.endSessionEpoch@ when Exit to
    --   Menu destroys every world (#2291), so the next game does not
    --   inherit the previous session's accumulated total; and REPLACED
    --   by the save's own @sdGameTime@ on a load publish
    --   ('World.Load.Publish.publishStagedSession'). Creating an
    --   additional page inside a live session is not one of them —
    --   'World.Thread.Command.Init.handleWorldInitCommand' never writes
    --   it. Because the unit tick advances it whenever the engine is
    --   unpaused, without asking whether a page is live, menu time
    --   accrues into it after boot and after an exit alike.
  , saveBarrierRef     ∷ SaveBarrier
    -- ^ Runtime-only coordinated-save transaction state.  It is diagnostic
    -- and synchronization state, never part of 'SaveData'.
  , inputThreadActiveRef ∷ IORef Bool
    -- ^ Issue #763: True once 'Engine.Input.Thread
    --   .startInputThread' has actually launched — headless boot
    --   ('App.Headless') never calls it at all (no GLFW window to
    --   poll), so SaveInput must not be a HARD requirement of every
    --   save/load transaction's owner set; 'saveWorldFn'/
    --   'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged' consult
    --   this to decide whether to include SaveInput, rather than
    --   'waitForOwners' timing out forever waiting for an owner that
    --   can never acknowledge. Runtime-only, never part of 'SaveData'.
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
    -- ^ Registry of all item defs loaded from the data/items/ tree —
    --   recursively, at any depth, since #1232. Lua's item.loadYaml
    --   writes into this; unit spawn reads from it to materialise
    --   starting_inventory entries.
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
  , recipeManagerRef ∷ IORef RecipeManager
    -- ^ Registry of crafting recipes loaded from data/recipes/*.yaml
    --   (#325). Queried + executed via the `craft.*` Lua API; the
    --   craft AI / bill layer (#329) will schedule against it.
  , locationDefsRef    ∷ IORef LocationRegistry
    -- ^ Registry of location defs (premade structures stamped into the
    --   world) loaded from data/locations/*.yaml at boot, after items /
    --   units / buildings. Read back by the `locations` Lua module
    --   (locations.listDefs / getDef / build). Pure data — content ids
    --   are resolved at spawn time by the world-gen overlay (#89/#90).
  , lootTableRegistryRef ∷ IORef LootTableRegistry
    -- ^ Registry of loot tables loaded from data/loot_tables/*.yaml at
    --   boot. Rolled from Lua to resolve a `loot_table` location content
    --   entry's item ids at spawn time (#90). That spawn path calls
    --   `loot.rollFor`, whose draw is a pure function of the world seed,
    --   the placed instance id, and the entry/roll indices (#948) — it
    --   does NOT touch 'statRNGRef'. The plain `loot.roll` still draws
    --   from that shared entropy-seeded generator and remains for
    --   ad-hoc, non-reproducible callers.
  , tutorialRegistryRef ∷ IORef TutorialRegistry
    -- ^ The one active tutorial definition tree, loaded from
    --   data/tutorials/*.yaml at boot (#957). Pure authored data —
    --   structure, presentation text, ordering, and the stable
    --   evaluator keys the Lua tutorial runtime dispatches on; no
    --   progress or completion state lives here. Written only by
    --   `engine.loadTutorialDir`, which loads the WHOLE directory in
    --   one call (that is what makes "exactly one tree" checkable),
    --   and read back read-only through `engine.getTutorialTree`. That
    --   one call writes this field exactly once — the validated tree,
    --   or the explicit empty state on any failure — so it is never
    --   partial and never depends on directory read order.
  , eventStoreRef      ∷ TVar EventStore
    -- ^ Ring buffer of player-facing events (~1000 entries; oldest
    --   dropped), together with the counter naming the next mutation
    --   ('Engine.PlayerEvent.EventStore', #1714). Per-session only —
    --   not serialized to save files.
    --   An STM TVar, so pushes from any thread are safe; the call
    --   sites that actually exist today are the world thread and the
    --   Lua thread, both via 'Engine.PlayerEvent.emitEvent' (no unit-
    --   or combat-thread emitter exists). Read atomically by Lua-side
    --   queries (e.g. the event-log panel).
    --
    --   Rows and counter share this ONE ref so a sequence is assigned
    --   in the same atomic write that commits the row it names, and so
    --   the counter outlives a row reset — see
    --   'Engine.PlayerEvent.clearEventStoreRows'. The rows are reset at
    --   BOTH session boundaries: a load publish
    --   ('World.Load.Publish.resetTransientState') and an Exit to Menu
    --   (@Unit.Thread.endSessionEpoch@, #2291). \"Per-session only\"
    --   above is that promise; before #2291 only the load half of it
    --   was kept, and the previous session's rows stayed renderable and
    --   clickable in the next game.
  , notificationCfgRef ∷ IORef NotificationCfg
    -- ^ Resolved notification settings keyed by category id. Loaded
    --   at boot from 'data/notification_categories.yaml' merged with
    --   'config/notifications.local.yaml' (#786). Wrapped in an IORef so the
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
    -- ^ Events with popup display enabled, appended at the same emit
    --   call site that sends the live 'LuaShowPopup' message on
    --   'luaQueue'. WRITE-ONLY today: that message — not a drain of
    --   this TVar — is how the Lua popup module receives a popup, and
    --   nothing reads this queue back out anywhere. It exists for
    --   inspection / debug querying and as a Phase 2 stable source
    --   for the notifications panel.
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
  , sceneManager     ∷ SceneManager
  }

-- | Per-frame timing measurement, stepped once per frame by
--   'Engine.Loop.Timing.updateFrameTiming' on the main render thread.
--
--   This record does NOT own frame pacing — the live 'VideoConfig' does
--   ('vcVSync' presentation when VSync is on, otherwise 'vcFrameLimit',
--   and no software cap when neither applies). Nothing here caps the
--   frame rate; changing the frame rate means changing 'VideoConfig'.
--
--   The two @fpsWindow*@ fields are the CURRENT FPS SAMPLING WINDOW, not
--   running totals: both reset to zero every time the window reaches one
--   second and its average is published to @fpsRef@.
data TimingState = TimingState
  { fpsWindowFrames  ∷ Word64
    -- ^ Frames counted in the current FPS sampling window.
  , deltaTime        ∷ Double
    -- ^ Seconds elapsed since the previous frame.
  , fpsWindowElapsed ∷ Double
    -- ^ Seconds accumulated in the current FPS sampling window.
  , lastFrameTime    ∷ Double
    -- ^ Timestamp of the previous frame, carried across frames.
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
  , swapchainFbState   ∷ Maybe FramebufferState
    -- ^ The framebuffer state the live swapchain corresponds to
    --   (#1693) — seeded from the state the initial
    --   'Engine.Graphics.Vulkan.Swapchain.createVulkanSwapchain' was
    --   given, and rewritten by every successful
    --   @recreateSwapchainFor@ from the exact state that recreation
    --   decided on. 'Nothing' means no swapchain has been built
    --   (headless\/offscreen, or before Vulkan init), and a zero-area
    --   size records a minimized window.
    --
    --   Deliberately the RAW framebuffer size and never
    --   @siSwapExtent@: 'Engine.Graphics.Vulkan.Swapchain.chooseSwapExtent'
    --   honours the surface's @currentExtent@ and otherwise clamps into
    --   @min\/maxImageExtent@, so the extent can legitimately differ
    --   from the size that was requested. Comparing against the extent
    --   would re-request a recreation forever. The paired minimize
    --   generation is what makes a restore to the PRE-MINIMIZE
    --   dimensions distinguishable from no change at all — see
    --   "Engine.Graphics.Vulkan.ResizeRequest".
  , msaaColorImage     ∷ Maybe (Vk.Image, Vk.DeviceMemory, Vk.ImageView)
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

-- | Cached windowed-mode geometry so we can restore position\/size after
--   fullscreen, plus the window mode the main render thread has actually
--   APPLIED to the GLFW window.
data WindowState = WindowState
  { wsWindowedPos  ∷ (Int, Int)   -- ^ Last known windowed position
  , wsWindowedSize ∷ (Int, Int)   -- ^ Last known windowed size (screen coords)
  , wsAppliedMode  ∷ WindowMode
    -- ^ The mode the main render thread last applied to the window.
    --   Every cache decision keys off THIS, never @vcWindowMode@ (#907):
    --   'Engine.Scripting.Lua.API.Config.setWindowModeFn' publishes the
    --   TARGET mode into the video config on the Lua thread the moment it
    --   enqueues @LuaSetWindowMode@, so by the time the handler runs on
    --   the render thread a frame later the config already reports the
    --   mode being entered rather than the one being left.
  } deriving (Show, Eq)

-- | The pre-window seed. @wsAppliedMode@ starts 'Windowed' because no
--   GLFW window exists yet — 'Engine.Graphics.Window.GLFW.createWindow'
--   overwrites it via 'applyWindowCreation' once one does, and the
--   window-less boot profiles (@--headless@, @--dump@, @--offscreen@)
--   never reach 'Engine.Scripting.Lua.Message.Video.handleSetWindowMode'
--   at all.
--
--   The geometry is a pre-window fallback and nothing more. A boot that
--   comes up 'Windowed' replaces it the first time it switches away, and
--   a boot that comes up 'BorderlessWindowed' (#1731) or 'Fullscreen'
--   (#1882) has 'applyWindowCreation' seed it from the decorated window
--   GLFW just made — precisely because applying either mode at creation
--   consumes that first-switch caching opportunity. That leaves exactly
--   two ways these values are ever read: the window-less profiles
--   above, and the window a 'CreatedPlain' boot is already living in
--   before it first switches away.
defaultWindowState ∷ WindowState
defaultWindowState = WindowState
  { wsWindowedPos  = (100, 100)
  , wsWindowedSize = (800, 600)
  , wsAppliedMode  = Windowed
  }

-- | What 'Engine.Graphics.Window.GLFW.createWindow' actually got GLFW to
--   do — never what the 'Engine.Graphics.Window.Types.WindowConfig'
--   asked for.
--
--   Both non-plain requests degrade gracefully to the plain decorated
--   window GLFW already created when no primary monitor or video mode is
--   available, so a request and its outcome genuinely differ. Modelling
--   the outcome as this sum rather than the config makes the
--   simultaneously-fullscreen-and-borderless state unrepresentable
--   downstream.
data WindowCreationOutcome
  = CreatedPlain
    -- ^ An ordinary decorated window: no special mode was requested, or
    --   the one that was could not be applied.
  | CreatedFullscreen
    -- ^ 'Engine.Graphics.Window.Types.wcFullscreen' was requested AND
    --   applied.
  | CreatedBorderless
    -- ^ 'Engine.Graphics.Window.Types.wcBorderless' was requested AND
    --   applied (#1731).
  deriving (Show, Eq)

-- | The window mode a freshly created GLFW window actually came up in.
--
--   Deliberately keyed on the OUTCOME, not the configured mode:
--   'Engine.Graphics.Window.GLFW.createWindow' degrades a fullscreen or
--   borderless request gracefully to the plain window it just created
--   when no primary monitor or video mode is available. Seeding
--   'wsAppliedMode' from the config would call those cases fullscreen or
--   borderless, and a later 'Windowed' request would then be a real
--   switch restoring a cache no live windowed window ever filled —
--   teleporting the window onto 'defaultWindowState'\'s fallback
--   geometry.
appliedModeAtCreation ∷ WindowCreationOutcome → WindowMode
appliedModeAtCreation CreatedFullscreen = Fullscreen
appliedModeAtCreation CreatedBorderless = BorderlessWindowed
appliedModeAtCreation CreatedPlain      = Windowed

-- | Fold ONE window creation into the render-thread-owned 'WindowState':
--   record the mode GLFW actually came up in, and — for a creation that
--   APPLIED a non-windowed mode — seed the windowed-geometry cache.
--
--   The supplied position and size must be sampled from the live
--   DECORATED window, after 'Engine.Graphics.Window.GLFW.createWindow'
--   made it but before any mode mutation, so they describe the window
--   that actually exists rather than the requested dimensions
--   (configuration persists no position at all).
--
--   Both non-plain outcomes need the seed, for one reason: applying the
--   mode at creation means the first later switch to 'Windowed' is an
--   ENTRY, and 'applyWindowModeTransition' never caches on the way in —
--   so nothing else can ever fill the cache, and that switch would
--   restore 'defaultWindowState'\'s (100,100) \/ 800x600 fallback.
--   'CreatedBorderless' is #1731; 'CreatedFullscreen' is #1882, whose
--   decorated window is the same one, sampled at the same moment, by the
--   same caller.
--
--   'CreatedPlain' is excluded on purpose, and that covers a fullscreen
--   or borderless request that could NOT be applied as well as one never
--   made: such a boot IS the windowed state, so its own first switch
--   away caches the live geometry and a seed here would be inert.
applyWindowCreation ∷ WindowCreationOutcome → (Int, Int) → (Int, Int)
                    → WindowState → WindowState
applyWindowCreation outcome decoratedPos decoratedSize ws = case outcome of
    CreatedPlain → seeded
    _            → seeded { wsWindowedPos  = decoratedPos
                          , wsWindowedSize = decoratedSize }
  where
    seeded = ws { wsAppliedMode = appliedModeAtCreation outcome }

-- | Does switching to @target@ mean LEAVING an applied windowed state?
--   Only then may the live GLFW geometry be captured into the cache:
--
--   * entering 'Windowed' must not cache — that would overwrite the
--     user's geometry with the borderless\/fullscreen geometry the very
--     restore is trying to replace (the #907 symptom), and
--   * moving between the two non-windowed modes must not cache — there
--     is no windowed geometry on screen to record.
leavingWindowedMode ∷ WindowMode → WindowMode → Bool
leavingWindowedMode applied target = applied ≡ Windowed ∧ target ≢ Windowed

-- | Is there nothing to switch — has the render thread already applied
--   this mode?
--
--   A redundant request must NOT re-run the switch. That matters most
--   for 'Windowed', whose branch RESTORES from the geometry cache, and
--   the cache holds nothing meaningful until a real switch away from
--   windowed has filled it: re-applying it to a live windowed window
--   would teleport it onto the default 800x600 at (100,100). Redundant
--   requests are reachable — @scripts/settings/data.lua@'s Defaults path
--   calls @engine.setWindowMode@ unconditionally.
windowModeAlreadyApplied ∷ WindowState → WindowMode → Bool
windowModeAlreadyApplied ws target = wsAppliedMode ws ≡ target

-- | Fold one SUCCESSFULLY applied window-mode switch into the
--   render-thread-owned 'WindowState': cache the supplied live geometry
--   when (and only when) 'leavingWindowedMode' says so, then record the
--   newly applied mode.
--
--   The live position\/size must be sampled BEFORE the GLFW switch, while
--   the window is still in the mode being left. Applying the fold after
--   the switch is what keeps the restore path correct: entering 'Windowed'
--   reads the cache during the switch and never writes it here.
--
--   Purely a function of the applied mode and the target, so a sequence of
--   back-to-back requests folds deterministically in queue order, with no
--   dependence on when the Lua thread published @vcWindowMode@.
applyWindowModeTransition ∷ WindowMode → (Int, Int) → (Int, Int)
                          → WindowState → WindowState
applyWindowModeTransition target livePos liveSize ws
  | leavingWindowedMode (wsAppliedMode ws) target
  = ws { wsWindowedPos  = livePos
       , wsWindowedSize = liveSize
       , wsAppliedMode  = target }
  | otherwise
  = ws { wsAppliedMode = target }

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

-- | 'resolveActiveWorld' over a live world-manager ref, returning the
--   active world's page id together with its state.
--
--   Takes the 'IORef' rather than an 'EngineEnv' so a capability-narrowed
--   consumer (issue #893's @Engine.Core.Capability.WorldSim@) can apply
--   the one canonical resolution rule to @wsWorldManagerRef@ without
--   reaching for the whole environment — the "explicitly narrower
--   handle" shape @docs/engineenv_capability_inventory.md@ §7.6
--   established. 'activeWorldPage' below is the identical operation
--   spelled over an 'EngineEnv', for consumers that still hold one.
activeWorldPageFrom ∷ IORef WorldManager
                    → IO (Maybe (WorldPageId, WorldState))
activeWorldPageFrom ref = resolveActiveWorld <$> readIORef ref

-- | The active world's 'WorldState' (its page id discarded), over a
--   live world-manager ref. The common case for current-world reads
--   that don't need the page id.
activeWorldStateFrom ∷ IORef WorldManager → IO (Maybe WorldState)
activeWorldStateFrom ref = fmap snd <$> activeWorldPageFrom ref

-- | 'resolveActiveWorld' over the live 'worldManagerRef', returning the
--   active world's page id together with its state.
activeWorldPage ∷ EngineEnv → IO (Maybe (WorldPageId, WorldState))
activeWorldPage env = activeWorldPageFrom (worldManagerRef env)

-- | The active world's 'WorldState' (its page id discarded). The common
--   case for current-world reads that don't need the page id.
activeWorldState ∷ EngineEnv → IO (Maybe WorldState)
activeWorldState env = activeWorldStateFrom (worldManagerRef env)

-- | Allocate the next process-unique 'iiInstanceId'. Call ONCE per
--   genuine item creation (a roll / spawn); never when merely moving or
--   copying an existing instance — moves preserve the id. Thread-safe
--   (atomic bump), so it is correct to call from any thread sharing this
--   'EngineEnv'. See 'nextItemInstanceIdRef'.
freshItemInstanceId ∷ EngineEnv → IO Word64
freshItemInstanceId env =
    atomicModifyIORef' (nextItemInstanceIdRef env) (\n → (n + 1, n))

-- | #1910: the ten long-lived inter-thread queues this record owns,
--   each under a stable, low-cardinality name for
--   @debug.getQueueStats()@ to report.
--
--   A name is the field's own module-qualified spelling, which is what
--   keeps it stable and unambiguous: the identifier a reader would grep
--   for is literally the name they see. Nothing that passed THROUGH a
--   queue — an element, an argument, an entity id — ever appears in
--   one, so the label set is fixed at ten and cannot grow with traffic.
--
--   The inventory is assembled here, beside the declarations it names,
--   rather than at a consumer: this module is the capability inventory's
--   permanent definer, so no reader of the telemetry needs unrestricted
--   'EngineEnv' access to obtain it, and a queue field cannot be added,
--   renamed or removed without the list that names it being right here.
--
--   Ephemeral per-call queues — @Engine.Scripting.Lua.API.Screenshot@'s
--   reply queue, and the queues tests build directly — are deliberately
--   absent: they are created and discarded within one operation, so a
--   name for them would identify nothing durable.
engineQueueInventory ∷ EngineEnv → [Q.NamedQueue]
engineQueueInventory env =
  [ Q.namedQueue "Engine.Core.State.inputQueue"
                 (inputQueue env)
  , Q.namedQueue "Engine.Core.State.luaToEngineQueue"
                 (luaToEngineQueue env)
  , Q.namedQueue "Engine.Core.State.luaQueue"
                 (luaQueue env)
  , Q.namedQueue "Engine.Core.State.worldQueue"
                 (worldQueue env)
  , Q.namedQueue "Engine.Core.State.screenshotRequestQueue"
                 (screenshotRequestQueue env)
  , Q.namedQueue "Engine.Core.State.bloodDisposeQueue"
                 (bloodDisposeQueue env)
  , Q.namedQueue "Engine.Core.State.unitQueue"
                 (unitQueue env)
  , Q.namedQueue "Engine.Core.State.buildingQueue"
                 (buildingQueue env)
  , Q.namedQueue "Engine.Core.State.combatQueue"
                 (combatQueue env)
  , Q.namedQueue "Engine.Core.State.simQueue"
                 (simQueue env)
  ]
