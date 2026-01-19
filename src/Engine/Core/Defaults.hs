{-# LANGUAGE CPP #-}
module Engine.Core.Defaults where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as Map
import Engine.Core.Base
import Engine.Core.Types
import Engine.Core.State
import Engine.Asset.Types
import Engine.Input.Types
import Engine.Graphics.Base
import Engine.Graphics.Camera
import Engine.Graphics.Config
import Engine.Graphics.Window.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Scene.Types
import Vulkan.Zero

defaultEngineConfig ∷ EngineConfig
defaultEngineConfig = EngineConfig 
  { windowWidth  = 800
  , windowHeight = 600
  , enableVSync  = True
#ifdef DEVELOPMENT
  , enableDebug  = True
#else
  , enableDebug  = False
#endif
  }

defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig 
  { gcAppName   = T.pack "Vulkan Device Test"
#ifdef DEVELOPMENT
  , gcDebugMode = True
#else
  , gcDebugMode = False
#endif
  , gcWidth     = 800
  , gcHeight    = 600
  , gcMaxFrames = 2
  }

defaultWindowConfig ∷ VideoConfig → WindowConfig
defaultWindowConfig vc = WindowConfig
  { wcWidth     = vcWidth vc
  , wcHeight    = vcHeight vc
  , wcTitle     = T.pack "Synarchy"
  , wcResizable = True
  }

defaultEngineState ∷ AssetPool → EngineState
defaultEngineState pool = EngineState
  { timingState = TimingState
    { frameCount       = 0
    , currentTime      = 0.0
    , deltaTime        = 0.0
    , frameTimeAccum   = 0.0
    , lastFrameTime    = 0.0
    , targetFPS        = 60.0
    }
  , inputState       = defaultInputState
  , graphicsState    = GraphicsState
    { glfwWindow         = Nothing
    , vulkanInstance     = Nothing
    , vulkanPDevice      = Nothing
    , vulkanDevice       = Nothing
    , deviceQueues       = Nothing
    , vulkanCmdPool      = Nothing
    , vulkanCmdBuffers   = Nothing
    , vulkanRenderPass   = Nothing
    , textureState       = (TexturePoolState zero zero, V.empty)
    , descriptorState    = Nothing
    , pipelineState      = Nothing
    , frameResources     = V.empty
    , currentFrame       = 0
    , framebuffers       = Nothing
    , swapchainInfo      = Nothing
    , syncObjects        = Nothing
    , vertexBuffer       = Nothing
    , uniformBuffers     = Nothing
    , textureArrayStates = Map.empty
    , camera2D           = defaultCamera
    , cleanupStatus      = NotStarted
    }
  , assetPool        = pool
  , assetConfig      = AssetConfig 100 100 True True
  , sceneManager     = createSceneManager
  }

