{-# LANGUAGE CPP #-}
module Engine.Core.Init
  ( initializeEngine
  , initializeEngineHeadless
  , EngineInitResult(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.IORef (newIORef)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlTextures
import Engine.Core.Defaults
import Engine.Core.Log (initLogger, defaultLogConfig, LogConfig(..)
                       , LogLevel(..), LogCategory(..))
import Engine.Core.State
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
import World.Types (WorldCommand, emptyWorldManager, emptyFloraCatalog)
import World.Material (emptyMaterialRegistry)
import World.Generate.Config (loadWorldGenConfig)

data EngineInitResult = EngineInitResult
  { eirEnv      ∷ EngineEnv
  , eirEnvVar   ∷ Var EngineEnv
  , eirStateVar ∷ Var EngineState
  }

-- | Allocate every 'IORef', queue, and subsystem, then bundle into 'EngineEnv'
initializeEngine ∷ IO EngineInitResult
initializeEngine = do
  eventQueue ← Q.newQueue
  inputQueue ← Q.newQueue
  worldQueue ← Q.newQueue
  simQueue ← Q.newQueue
  luaToEngineQueue ← Q.newQueue
  engineToLuaQueue ← Q.newQueue
  
  lifecycleRef ← newIORef EngineStarting
  fpsRef ← newIORef 0.0
 
  logger ← initLogger defaultLogConfig { lcMinLevel = LevelDebug }
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
  texSizeRef ← newIORef HM.empty
  defaultFaceMapSlotRef ← newIORef 0
  floraCatRef ← newIORef emptyFloraCatalog
  materialRegistryRef ← newIORef emptyMaterialRegistry
  unitManagerRef ← newIORef emptyUnitManager
  unitQueue ← Q.newQueue
  worldGenConfig ← loadWorldGenConfig "config/world_gen_default.yaml"
  worldGenConfigRef ← newIORef worldGenConfig

  frameCounterRef ← newIORef (0 ∷ Word64)
  let env = EngineEnv
        { engineConfig       = defaultEngineConfig
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
        , textureSizeRef     = texSizeRef
        , defaultFaceMapSlotRef = defaultFaceMapSlotRef
        , floraCatalogRef    = floraCatRef
        , materialRegistryRef = materialRegistryRef
        , unitManagerRef     = unitManagerRef
        , unitQueue          = unitQueue
        , worldGenConfigRef  = worldGenConfigRef
        , simQueue          = simQueue
        , frameCounterRef   = frameCounterRef
        }
  
  envVar   ← atomically $ newVar env
  stateVar ← atomically $ newVar defaultEngineState

  pure $ EngineInitResult env envVar stateVar

-- | Like 'initializeEngine' but sets 'ecHeadless' — no window or GPU
initializeEngineHeadless ∷ IO EngineInitResult
initializeEngineHeadless = do
  result ← initializeEngine
  let env = eirEnv result
      headlessEnv = env { engineConfig = (engineConfig env) { ecHeadless = True } }
  envVar ← atomically $ newVar headlessEnv
  pure $ result { eirEnv = headlessEnv, eirEnvVar = envVar }
