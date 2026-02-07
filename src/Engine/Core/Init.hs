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

-- | Result of engine initialization
data EngineInitResult = EngineInitResult
  { eirEnv      ∷ EngineEnv
  , eirEnvVar   ∷ Var EngineEnv
  , eirStateVar ∷ Var EngineState
  }

-- | Initialize all engine subsystems and create the environment
initializeEngine ∷ IO EngineInitResult
initializeEngine = do
  -- Initialize queues
  eventQueue ← Q.newQueue
  inputQueue ← Q.newQueue
  luaToEngineQueue ← Q.newQueue
  engineToLuaQueue ← Q.newQueue
  
  -- Initialize lifecycle ref
  lifecycleRef ← newIORef EngineStarting
  fpsRef ← newIORef 0.0
  
  -- Initialize logging
  logger ← initLogger defaultLogConfig { lcMinLevel = LevelDebug }
  loggerRef ← newIORef logger
  
  -- Initialize asset pool
  assetPool ← defaultAssetPool
  assetPoolRef ← newIORef assetPool
  nextObjectIdRef ← newIORef 0
  
  -- Initialize input state
  inputStateRef ← newIORef defaultInputState
  keyBindings ← loadKeyBindings logger "config/keybinds.yaml"
  keyBindingsRef ← newIORef keyBindings
  
  -- Load video config
  videoConfig ← loadVideoConfig logger "config/video.yaml"
  videoConfigRef ← newIORef $ videoConfig
  windowSizeRef ← newIORef (vcWidth videoConfig, vcHeight videoConfig)
  windowStateRef ← newIORef defaultWindowState
  framebufferSizeRef ← newIORef (vcWidth videoConfig, vcHeight videoConfig)
  
  -- Create camera references
  cameraRef ← newIORef defaultCamera
  uiCameraRef ← newIORef $ defaultUICamera (fromIntegral (vcWidth videoConfig))
                                           (fromIntegral (vcHeight videoConfig))
  -- Create UI manager references
  uiManagerRef ← newIORef emptyUIPageManager
  -- Create focus manager
  focusMgrRef ← newIORef createFocusManager
  -- text buffers reference
  textBuffersRef ← newIORef Map.empty
  -- font cache reference
  fontCache ← newIORef defaultFontCache
  
  -- Build environment
  let env = EngineEnv
        { engineConfig       = defaultEngineConfig
        , videoConfigRef     = videoConfigRef
        , windowSizeRef      = windowSizeRef
        , windowStateRef     = windowStateRef
        , framebufferSizeRef = framebufferSizeRef
        , fpsRef             = fpsRef
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
        , focusManagerRef    = focusMgrRef
        }
  
  envVar   ← atomically $ newVar env
  stateVar ← atomically $ newVar $ defaultEngineState assetPool
  
  pure $ EngineInitResult env envVar stateVar
