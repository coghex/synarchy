{-# LANGUAGE CPP #-}
module Engine.Core.Init
  ( initializeEngine
  , initializeEngineWith
  , initializeEngineHeadless
  , initializeEngineHeadlessWith
  , EngineInitResult(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.IORef (newIORef)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import Control.Concurrent.STM (newTVarIO)
import qualified System.Random as Random
import qualified Combat.Types
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlNotifications (loadNotificationCfg)
import Engine.Asset.YamlTextures
import Engine.Core.Defaults
import Engine.Core.Log (initLogger, defaultLogConfig, LogConfig(..)
                       , LogLevel(..), LogCategory(..), LogBackend(..))
import System.IO (stdout)
import Engine.Core.State
import Engine.Graphics.Vulkan.Sampler.Types (emptySamplerCache)
import Engine.Core.Types
import Engine.Core.Var
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (defaultCamera, defaultUICamera)
import Engine.Graphics.Config (loadVideoConfig, VideoConfig(..)
                              , WindowMode(..))
import Engine.Graphics.Font.Data (defaultFontCache)
import Engine.Input.Bindings (loadKeyBindings)
import Engine.Input.Types (defaultInputState)
import UI.Focus (createFocusManager)
import UI.Types (emptyUIPageManager)
import Unit.Types (emptyUnitManager)
import Unit.Sim.Types (emptyUnitThreadState)
import Building.Types (emptyBuildingManager)
import Item.Types (emptyItemManager)
import Equipment.Types (emptyEquipmentClassManager)
import Substance.Types (emptySubstanceManager)
import World.Types (WorldCommand, emptyWorldManager, emptyFloraCatalog)
import World.Material (emptyMaterialRegistry)
import World.Generate.Config (loadWorldGenConfig)

data EngineInitResult = EngineInitResult
  { eirEnv ∷ EngineEnv }

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
  eventQueue ← Q.newQueue
  inputQueue ← Q.newQueue
  worldQueue ← Q.newQueue
  simQueue ← Q.newQueue
  luaToEngineQueue ← Q.newQueue
  engineToLuaQueue ← Q.newQueue
  
  lifecycleRef ← newIORef EngineStarting
  fpsRef ← newIORef 0.0
 
  logger ← initLogger defaultLogConfig { lcMinLevel = LevelDebug
                                       , lcBackend = logBackend }
  loggerRef ← newIORef logger
  
  assetPool ← defaultAssetPool
  assetPoolRef ← newIORef assetPool
  nextObjectIdRef ← newIORef 0
  texNameRegRef ← newIORef emptyTextureNameRegistry
  
  inputStateRef ← newIORef defaultInputState
  keyBindings ← loadKeyBindings logger "config/keybinds.yaml"
  keyBindingsRef ← newIORef keyBindings
  
  videoConfig ← loadVideoConfig logger "config/video.yaml"
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
  focusMgrRef ← newIORef createFocusManager
  textBuffersRef ← newIORef Map.empty
  fontCache ← newIORef defaultFontCache
  sunAngleRef ← newIORef 0.25       -- start at noon
  worldPreviewRef ← newIORef Nothing
  zoomAtlasDataRef ← newIORef Nothing
  worldQuadsRef ← newIORef (V.empty)
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
  buildingQueue ← Q.newQueue
  buildingGhostRef ← newIORef Nothing
  combatQueue ← Q.newQueue
  combatEventsRef ← newIORef Combat.Types.emptyEventQueue
  worldGenConfig ← loadWorldGenConfig "config/world_gen_default.yaml"
  worldGenConfigRef ← newIORef worldGenConfig

  enginePausedRef ← newIORef False
  gameTimeRef     ← newIORef (0 ∷ Double)
  itemManagerRef  ← newIORef emptyItemManager
  equipmentClassManagerRef ← newIORef emptyEquipmentClassManager
  substanceManagerRef ← newIORef emptySubstanceManager
  -- Player Events: load the notification registry (data/) merged
  -- with player overrides (config/), allocate the ring buffer and
  -- popup queue. Both TVars are multi-writer (world/unit/Lua threads
  -- can all push via Engine.PlayerEvent.emitEvent). The cfg IORef
  -- is updated at runtime by the Phase 2 notifications settings tab.
  (notificationCfg0, notificationOrder) ← loadNotificationCfg logger
                        "data/notification_categories.yaml"
                        "config/notifications.yaml"
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
        , eventQueue         = eventQueue
        , inputQueue         = inputQueue
        , loggerRef          = loggerRef
        , luaToEngineQueue   = luaToEngineQueue
        , luaQueue           = engineToLuaQueue
        , lifecycleRef       = lifecycleRef
        , assetPoolRef       = assetPoolRef
        , textureNameRegistryRef = texNameRegRef
        , nextObjectIdRef    = nextObjectIdRef
        , fontCacheRef       = fontCache
        , inputStateRef      = inputStateRef
        , keyBindingsRef     = keyBindingsRef
        , textBuffersRef     = textBuffersRef
        , cameraRef          = cameraRef
        , uiCameraRef        = uiCameraRef
        , uiManagerRef       = uiManagerRef
        , worldManagerRef    = worldManagerRef
        , worldQueue         = worldQueue
        , focusManagerRef    = focusMgrRef
        , sunAngleRef        = sunAngleRef
        , worldPreviewRef    = worldPreviewRef
        , zoomAtlasDataRef   = zoomAtlasDataRef
        , worldQuadsRef      = worldQuadsRef
        , textureSystemRef   = textureSystemRef
        , samplerCacheRef    = samplerCacheRef
        , textureSizeRef     = texSizeRef
        , defaultFaceMapSlotRef = defaultFaceMapSlotRef
        , floraCatalogRef    = floraCatRef
        , materialRegistryRef = materialRegistryRef
        , unitManagerRef     = unitManagerRef
        , unitQueue          = unitQueue
        , utsRef             = utsRef
        , statRNGRef         = statRNGRef
        , buildingManagerRef = buildingManagerRef
        , buildingQueue      = buildingQueue
        , combatQueue        = combatQueue
        , combatEventsRef    = combatEventsRef
        , buildingGhostRef   = buildingGhostRef
        , worldGenConfigRef  = worldGenConfigRef
        , simQueue          = simQueue
        , enginePausedRef   = enginePausedRef
        , gameTimeRef       = gameTimeRef
        , itemManagerRef    = itemManagerRef
        , equipmentClassManagerRef = equipmentClassManagerRef
        , substanceManagerRef      = substanceManagerRef
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
