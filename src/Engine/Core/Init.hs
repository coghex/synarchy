{-# LANGUAGE CPP #-}
module Engine.Core.Init
  ( initializeEngine
  , EngineInitResult(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data.IORef (newIORef)
import Engine.Asset.Types (defaultAssetPool)
import Engine.Core.Base
import Engine.Core.Defaults
import Engine.Core.State
import Engine.Core.Types
import Engine.Core.Var
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (defaultCamera, defaultUICamera)
import Engine.Graphics.Config (loadVideoConfig, VideoConfig(..))
import Engine.Input.Bindings (loadKeyBindings)
import Engine.Input.Types (defaultInputState)
import UI.Focus (createFocusManager)

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
  
  -- Initialize logging
  logFunc ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  
  -- Initialize asset pool
  assetPool ← defaultAssetPool
  assetPoolRef ← newIORef assetPool
  nextObjectIdRef ← newIORef 0
  
  -- Initialize input state
  inputStateRef ← newIORef defaultInputState
  keyBindings ← loadKeyBindings "config/keybinds.json"
  keyBindingsRef ← newIORef keyBindings
  
  -- Load video config
  videoConfig ← loadVideoConfig "config/video.yaml"
  
  -- Create camera references
  cameraRef ← newIORef defaultCamera
  uiCameraRef ← newIORef $ defaultUICamera (fromIntegral (vcWidth videoConfig))
                                           (fromIntegral (vcHeight videoConfig))

  -- Create focus manager
  focusMgrRef ← newIORef createFocusManager
  
  -- Build environment
  let env = EngineEnv
        { engineConfig     = defaultEngineConfig
        , eventQueue       = eventQueue
        , inputQueue       = inputQueue
        , logFunc          = logFunc
        , luaToEngineQueue = luaToEngineQueue
        , luaQueue         = engineToLuaQueue
        , lifecycleRef     = lifecycleRef
        , assetPoolRef     = assetPoolRef
        , nextObjectIdRef  = nextObjectIdRef
        , inputStateRef    = inputStateRef
        , keyBindingsRef   = keyBindingsRef
        , cameraRef        = cameraRef
        , uiCameraRef      = uiCameraRef
        , focusManagerRef  = focusMgrRef
        }
  
  envVar   ← atomically $ newVar env
  stateVar ← atomically $ newVar $ defaultEngineState assetPool
  
  pure $ EngineInitResult env envVar stateVar
