{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Engine.Core.Init
  ( initializeEngine
  , initializeEngineWith
  , initializeEngineHeadless
  , initializeEngineHeadlessWith
  , EngineInitResult(..)
  , resolveConfigPath
  , migrateLegacyConfig
  ) where

import UPrelude
import Data.IORef (newIORef)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON)
import Data.Proxy (Proxy(..))
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try, displayException)
import qualified System.Random as Random
import qualified Combat.Types
import Engine.ActionOutcome (emptyActionOutcomeQueue)
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlNotifications (loadNotificationCfg, OverridesFile)
import Engine.Asset.YamlTextures
import Engine.Core.Defaults
import Engine.Core.Log (initLogger, defaultLogConfig, LogConfig(..)
                       , LogBackend(..), LoggerState, logInfo, logWarn
                       , LogCategory(..))
import System.IO (stdout)
import System.Directory (doesFileExist, copyFile)
import Engine.Core.State
import Engine.Save.Barrier (newSaveBarrier)
import Engine.Load.Status (newLoadStatusRef)
import Engine.Scene.Types (emptyLayeredQuads)
import Engine.Graphics.Vulkan.Sampler.Types (emptySamplerCache)
import Engine.Core.Types
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (defaultCamera, defaultUICamera)
import Engine.Graphics.Config (loadVideoConfig, VideoConfig(..), VideoConfigFile)
import Engine.Graphics.Font.Data (defaultFontCache)
import Engine.Input.Bindings (loadKeyBindings, KeyBindingConfig)
import Engine.Input.Types (defaultInputState)
import UI.Focus (createFocusManager)
import UI.Types (emptyUIPageManager)
import Unit.Types (emptyUnitManager)
import Unit.Sim.Types (emptyUnitThreadState)
import Building.Types (emptyBuildingManager)
import Structure.Palette (emptyTexPalette)
import Item.Types (emptyItemManager)
import Equipment.Types (emptyEquipmentClassManager)
import Substance.Types (emptySubstanceManager)
import Infection.Types (emptyInfectionManager)
import Craft.Types (emptyRecipeManager)
import Location.Types (emptyLocationRegistry)
import LootTable.Types (emptyLootTableRegistry)
import World.Types (emptyWorldManager, emptyFloraCatalog)
import World.Material (emptyMaterialRegistry)
import World.Generate.Config (loadWorldGenConfig)
import Unit.Pathing.Config (loadPathingConfig)

data EngineInitResult = EngineInitResult
  { eirEnv ∷ EngineEnv }

-- | Prefer a local runtime config file over its versioned default
--   template (#638). @config/video.local.yaml@ / @config/keybinds.local.yaml@
--   (#786) are gitignored player state written by the settings UI's
--   Save actions; a fresh clone has neither, so boot falls back to the
--   tracked @_default.yaml@ template until the first Save (or a
--   'migrateLegacyConfig' upgrade) creates the local file.
resolveConfigPath ∷ FilePath → FilePath → IO FilePath
resolveConfigPath localPath defaultPath = do
  hasLocal ← doesFileExist localPath
  return $ if hasLocal then localPath else defaultPath

-- | One-time upgrade from the pre-#786 tracked config layout.
--   @config/video.yaml@ / @config/keybinds.yaml@ / @config/notifications.yaml@
--   are kept tracked with content equal to the versioned default/registry
--   (never a real player's values) purely so a readable legacy file
--   always exists at first boot for this to migrate — a plain
--   unmodified update sees no-op-equivalent content here, and a player
--   who actually saved through the old settings UI has a LOCALLY
--   MODIFIED copy of one of these paths, which git itself refuses to
--   silently overwrite during an update (it errors rather than
--   deleting), so their real values survive on disk until the update
--   is resolved by hand — at which point this picks them up.
--
--   If the current-format local file is already present, this never
--   runs — that single existence gate is what makes migration
--   idempotent (the copy it makes on first boot is what every later
--   boot finds) and what guarantees a newer local file always wins
--   over a stale legacy one. The legacy file is validated by decoding
--   it as the SAME type its subsystem's own loader expects (passed in
--   via @Proxy@) rather than merely checking it's syntactically valid
--   YAML — a file that parses fine as YAML but is missing a field the
--   real loader requires (e.g. video's @resolution@) must not be
--   copied and logged as a successful migration, since that would
--   silently mask a load failure the loader would otherwise report,
--   AND permanently block any future migration attempt (the existence
--   gate above would see the copied-but-unusable local file and never
--   look at legacy again). A legacy file that fails this check
--   (malformed, partial, schema-incomplete, or unreadable) is left
--   untouched and logged rather than copied, so it falls back to the
--   versioned default/registry exactly like a missing legacy file.
migrateLegacyConfig ∷ ∀ a. FromJSON a ⇒ Proxy a → LoggerState → FilePath → FilePath → IO ()
migrateLegacyConfig _ logger legacyPath localPath = do
  hasLocal ← doesFileExist localPath
  unless hasLocal $ do
    hasLegacy ← doesFileExist legacyPath
    when hasLegacy $ do
      outcome ← try $ do
        eVal ← Yaml.decodeFileEither legacyPath
        case (eVal ∷ Either Yaml.ParseException a) of
          Left err → ioError $ userError $ show err
          Right _  → copyFile legacyPath localPath
      case (outcome ∷ Either SomeException ()) of
        Right () → logInfo logger CatInit $
          "Migrated legacy config " <> T.pack legacyPath
            <> " -> " <> T.pack localPath
        Left e → logWarn logger CatInit $
          "Legacy config " <> T.pack legacyPath
            <> " could not be migrated (malformed, partial, "
            <> "schema-incomplete, or unreadable); falling back to "
            <> "the versioned default: " <> T.pack (displayException e)

-- | Allocate every 'IORef', queue, and subsystem, then bundle into
--   'EngineEnv'. Logs to stdout (the graphical default).
initializeEngine ∷ IO EngineInitResult
initializeEngine = initializeEngineWith (LogToHandle stdout)

-- | As 'initializeEngine' but with an explicit log backend. Dump mode
--   passes 'stderr' here so the logger is born writing to stderr —
--   init-time logging (e.g. 'loadNotificationCfg') can never reach
--   stdout, which dump mode reserves for clean JSON. Redirecting the
--   backend after init returns would be too late (the line is already
--   emitted).
initializeEngineWith ∷ LogBackend → IO EngineInitResult
initializeEngineWith logBackend = do
  inputQueue ← Q.newQueue
  inputBarrierNextRef ← newTVarIO (0 ∷ Int)
  inputBarrierRef ← newTVarIO (0 ∷ Int)
  worldQueue ← Q.newQueue
  simQueue ← Q.newQueue
  luaToEngineQueue ← Q.newQueue
  engineToLuaQueue ← Q.newQueue
  screenshotRequestQueue ← Q.newQueue
  bloodDisposeQueue ← Q.newQueue
  
  lifecycleRef ← newIORef EngineStarting
  fpsRef ← newIORef 0.0
 
  logger ← initLogger defaultLogConfig { lcBackend = logBackend }
  loggerRef ← newIORef logger
  
  assetPool ← defaultAssetPool
  assetPoolRef ← newIORef assetPool
  nextObjectIdRef ← newIORef 0
  -- Item-instance ids start at 1 (0 is the "unassigned" sentinel); the
  -- counter is restored/max'd from sdNextItemInstanceId on load (#67).
  nextItemInstanceIdRef ← newIORef 1
  texNameRegRef ← newIORef emptyTextureNameRegistry
  
  inputStateRef ← newIORef defaultInputState
  migrateLegacyConfig (Proxy ∷ Proxy KeyBindingConfig) logger
    "config/keybinds.yaml" "config/keybinds.local.yaml"
  keybindsPath ← resolveConfigPath "config/keybinds.local.yaml" "config/keybinds_default.yaml"
  keyBindings ← loadKeyBindings logger keybindsPath
  keyBindingsRef ← newIORef keyBindings
  currentKeyDownRef ← newIORef Nothing

  migrateLegacyConfig (Proxy ∷ Proxy VideoConfigFile) logger
    "config/video.yaml" "config/video.local.yaml"
  videoConfigPath ← resolveConfigPath "config/video.local.yaml" "config/video_default.yaml"
  videoConfig ← loadVideoConfig logger videoConfigPath
  videoConfigRef ← newIORef $ videoConfig
  windowSizeRef ← newIORef (vcWidth videoConfig, vcHeight videoConfig)
  windowStateRef ← newIORef defaultWindowState
  framebufferSizeRef ← newIORef (vcWidth videoConfig, vcHeight videoConfig)
  brightnessRef ← newIORef (vcBrightness videoConfig)
  pixelSnapRef ← newIORef (vcPixelSnap videoConfig)
  textureFilterRef ← newIORef (vcTextureFilter videoConfig)
  
  cameraRef ← newIORef defaultCamera
  uiCameraRef ← newIORef $ defaultUICamera (fromIntegral (vcWidth videoConfig))
                                           (fromIntegral (vcHeight videoConfig))
  uiManagerRef ← newIORef emptyUIPageManager
  worldManagerRef ← newIORef emptyWorldManager
  hudActivePageRef ← newIORef Nothing
  loadStatusRef ← newLoadStatusRef
  pendingLoadRef ← newIORef Nothing
  focusMgrRef ← newIORef createFocusManager
  textBuffersRef ← newIORef Map.empty
  fontCache ← newIORef defaultFontCache
  sunAngleRef ← newIORef 0.25       -- start at noon
  worldPreviewRef ← newIORef Nothing
  zoomAtlasDataRef ← newIORef Nothing
  worldQuadsRef ← newIORef emptyLayeredQuads
  textureSystemRef ← newIORef Nothing
  samplerCacheRef ← newIORef emptySamplerCache
  texSizeRef ← newIORef HM.empty
  defaultFaceMapSlotRef ← newIORef 0
  floraCatRef ← newIORef emptyFloraCatalog
  materialRegistryRef ← newIORef emptyMaterialRegistry
  unitManagerRef ← newIORef emptyUnitManager
  unitQueue ← Q.newQueue
  utsRef ← newIORef emptyUnitThreadState
  statRNGRef ← Random.newStdGen >>= newIORef
  buildingManagerRef ← newIORef emptyBuildingManager
  texPaletteRef ← newIORef emptyTexPalette
  texPaletteHandlesRef ← newIORef HM.empty
  buildingQueue ← Q.newQueue
  buildingGhostRef ← newIORef Nothing
  combatQueue ← Q.newQueue
  combatEventsRef ← newIORef Combat.Types.emptyEventQueue
  injuryEventsRef ← newIORef Combat.Types.emptyEventQueue
  thoughtEventsRef ← newIORef Combat.Types.emptyEventQueue
  actionOutcomeRef ← newIORef emptyActionOutcomeQueue
  worldGenConfig ← loadWorldGenConfig "config/world_gen_default.yaml"
  worldGenConfigRef ← newIORef worldGenConfig
  pathingConfig ← loadPathingConfig logger "config/pathing.yaml"
  pathingConfigRef ← newIORef pathingConfig

  enginePausedRef ← newIORef False
  gameTimeRef     ← newIORef (0 ∷ Double)
  saveBarrierRef  ← newSaveBarrier
  inputThreadActiveRef ← newIORef False
  -- Seeded to the POSIX epoch so the first save uses the real wall
  -- clock; subsequent saves clamp against it for monotonic, distinct
  -- timestamps (#98).
  lastSaveTimeRef ← newIORef (UTCTime (fromGregorian 1970 1 1) 0)
  itemManagerRef  ← newIORef emptyItemManager
  equipmentClassManagerRef ← newIORef emptyEquipmentClassManager
  substanceManagerRef ← newIORef emptySubstanceManager
  infectionManagerRef ← newIORef emptyInfectionManager
  recipeManagerRef ← newIORef emptyRecipeManager
  locationDefsRef ← newIORef emptyLocationRegistry
  lootTableRegistryRef ← newIORef emptyLootTableRegistry
  -- Player Events: load the notification registry (data/) merged
  -- with player overrides (config/), allocate the ring buffer and
  -- popup queue. Both TVars are multi-writer (world/unit/Lua threads
  -- can all push via Engine.PlayerEvent.emitEvent). The cfg IORef
  -- is updated at runtime by the Phase 2 notifications settings tab.
  migrateLegacyConfig (Proxy ∷ Proxy OverridesFile) logger
    "config/notifications.yaml" "config/notifications.local.yaml"
  (notificationCfg0, notificationOrder) ← loadNotificationCfg logger
                        "data/notification_categories.yaml"
                        "config/notifications.local.yaml"
  notificationCfgRef ← newIORef notificationCfg0
  eventStoreRef ← newTVarIO Seq.empty
  popupQueueRef ← newTVarIO Seq.empty
  engineStateRef ← newIORef defaultEngineState
  let env = EngineEnv
        { engineConfig       = defaultEngineConfig
        , engineStateRef     = engineStateRef
        , videoConfigRef     = videoConfigRef
        , windowSizeRef      = windowSizeRef
        , windowStateRef     = windowStateRef
        , framebufferSizeRef = framebufferSizeRef
        , fpsRef             = fpsRef
        , brightnessRef      = brightnessRef
        , pixelSnapRef       = pixelSnapRef
        , textureFilterRef   = textureFilterRef
        , inputQueue         = inputQueue
        , inputBarrierNextRef = inputBarrierNextRef
        , inputBarrierRef    = inputBarrierRef
        , loggerRef          = loggerRef
        , luaToEngineQueue   = luaToEngineQueue
        , luaQueue           = engineToLuaQueue
        , lifecycleRef       = lifecycleRef
        , assetPoolRef       = assetPoolRef
        , textureNameRegistryRef = texNameRegRef
        , nextObjectIdRef    = nextObjectIdRef
        , nextItemInstanceIdRef = nextItemInstanceIdRef
        , fontCacheRef       = fontCache
        , inputStateRef      = inputStateRef
        , keyBindingsRef     = keyBindingsRef
        , currentKeyDownRef  = currentKeyDownRef
        , textBuffersRef     = textBuffersRef
        , cameraRef          = cameraRef
        , uiCameraRef        = uiCameraRef
        , uiManagerRef       = uiManagerRef
        , worldManagerRef    = worldManagerRef
        , hudActivePageRef   = hudActivePageRef
        , loadStatusRef      = loadStatusRef
        , pendingLoadRef     = pendingLoadRef
        , worldQueue         = worldQueue
        , focusManagerRef    = focusMgrRef
        , sunAngleRef        = sunAngleRef
        , worldPreviewRef    = worldPreviewRef
        , zoomAtlasDataRef   = zoomAtlasDataRef
        , screenshotRequestQueue = screenshotRequestQueue
        , worldQuadsRef      = worldQuadsRef
        , textureSystemRef   = textureSystemRef
        , samplerCacheRef    = samplerCacheRef
        , textureSizeRef     = texSizeRef
        , bloodDisposeQueue  = bloodDisposeQueue
        , defaultFaceMapSlotRef = defaultFaceMapSlotRef
        , floraCatalogRef    = floraCatRef
        , materialRegistryRef = materialRegistryRef
        , unitManagerRef     = unitManagerRef
        , unitQueue          = unitQueue
        , utsRef             = utsRef
        , statRNGRef         = statRNGRef
        , buildingManagerRef = buildingManagerRef
        , texPaletteRef      = texPaletteRef
        , texPaletteHandlesRef = texPaletteHandlesRef
        , buildingQueue      = buildingQueue
        , combatQueue        = combatQueue
        , combatEventsRef    = combatEventsRef
        , injuryEventsRef    = injuryEventsRef
        , thoughtEventsRef   = thoughtEventsRef
        , actionOutcomeRef   = actionOutcomeRef
        , buildingGhostRef   = buildingGhostRef
        , worldGenConfigRef  = worldGenConfigRef
        , pathingConfigRef   = pathingConfigRef
        , simQueue          = simQueue
        , enginePausedRef   = enginePausedRef
        , gameTimeRef       = gameTimeRef
        , saveBarrierRef    = saveBarrierRef
        , inputThreadActiveRef = inputThreadActiveRef
        , lastSaveTimeRef   = lastSaveTimeRef
        , itemManagerRef    = itemManagerRef
        , equipmentClassManagerRef = equipmentClassManagerRef
        , substanceManagerRef      = substanceManagerRef
        , infectionManagerRef      = infectionManagerRef
        , recipeManagerRef         = recipeManagerRef
        , locationDefsRef    = locationDefsRef
        , lootTableRegistryRef = lootTableRegistryRef
        , eventStoreRef      = eventStoreRef
        , notificationCfgRef = notificationCfgRef
        , notificationOrder  = notificationOrder
        , popupQueueRef      = popupQueueRef
        }

  pure $ EngineInitResult env

-- | Like 'initializeEngine' but sets 'ecHeadless' — no window or GPU.
--   Logs to stdout (the shell redirects it to a file in the --headless
--   workflow).
initializeEngineHeadless ∷ IO EngineInitResult
initializeEngineHeadless = initializeEngineHeadlessWith (LogToHandle stdout)

-- | As 'initializeEngineHeadless' but with an explicit log backend.
--   Dump mode passes 'stderr' so stdout stays clean JSON.
initializeEngineHeadlessWith ∷ LogBackend → IO EngineInitResult
initializeEngineHeadlessWith logBackend = do
  result ← initializeEngineWith logBackend
  let env = eirEnv result
      headlessEnv = env { engineConfig = (engineConfig env) { ecHeadless = True } }
  pure $ result { eirEnv = headlessEnv }
