{-# LANGUAGE CPP #-}
module Engine.Core.Init
  ( initializeEngine
  , EngineInitResult(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Engine.Asset.Types (defaultAssetPool)
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
import World.Types (WorldCommand, emptyWorldManager)

data EngineInitResult = EngineInitResult
  { eirEnv      ∷ EngineEnv
  , eirEnvVar   ∷ Var EngineEnv
  , eirStateVar ∷ Var EngineState
  }

-----------------------------------------------------------
-- Engine Initialization
-----------------------------------------------------------

initializeEngine ∷ IO EngineInitResult
initializeEngine = do
  eventQueue ← Q.newQueue
  inputQueue ← Q.newQueue
  worldQueue ← Q.newQueue
  luaToEngineQueue ← Q.newQueue
  engineToLuaQueue ← Q.newQueue
  lifecycleRef ← newIORef EngineStarting
  fpsRef ← newIORef 0.0
  logger ← initLogger defaultLogConfig { lcMinLevel = LevelDebug }
  loggerRef ← newIORef logger
  assetPool ← defaultAssetPool
  assetPoolRef ← newIORef assetPool
  nextObjectIdRef ← newIORef 0
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
  sunAngleRef ← newIORef 0.25
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
        }
  envVar   ← atomically $ newVar env
  stateVar ← atomically $ newVar defaultEngineState
  pure $ EngineInitResult env envVar stateVar
