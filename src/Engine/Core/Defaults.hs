{-# LANGUAGE CPP, UnicodeSyntax #-}
module Engine.Core.Defaults where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Types
import Engine.Core.State
import Engine.Asset.Types
import Engine.Graphics.Base
import Engine.Graphics.Config
import Engine.Graphics.Window.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Cleanup (emptyCleanup)
import Engine.Scene.Types

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
  , ecHeadless   = False
  , ecDebugPort  = 8008
  , ecBootProfile = BootNormal
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
  { wcWidth      = vcWidth vc
  , wcHeight     = vcHeight vc
  , wcFullscreen = case (vcWindowMode vc) of
      Fullscreen → True
      _          → False
  , wcTitle      = T.pack "Synarchy"
  , wcResizable  = True
  }

defaultEngineState ∷ EngineState
defaultEngineState = EngineState
  { timingState = TimingState
    { frameCount       = 0
    , currentTime      = 0.0
    , deltaTime        = 0.0
    , frameTimeAccum   = 0.0
    , lastFrameTime    = 0.0
    , targetFPS        = 60.0
    }
  , graphicsState    = GraphicsState
    { glfwWindow             = Nothing
    , vulkanInstance         = Nothing
    , vulkanPDevice          = Nothing
    , vulkanDevice           = Nothing
    , vulkanSurface          = Nothing
    , textureCapability      = Nothing
    , deviceQueues           = Nothing
    , vulkanCmdPool          = Nothing
    , vulkanCmdBuffers       = Nothing
    , vulkanRenderPass       = Nothing
    , descriptorState        = Nothing
    , pipelineState          = Nothing
    , frameResources         = V.empty
    , currentFrame           = 0
    , framebuffers           = Nothing
    , swapchainInfo          = Nothing
    , msaaColorImage         = Nothing
    , renderFinishedSems     = V.empty
    , vertexBuffer           = Nothing
    , uniformBuffers         = Nothing
    , bindlessPipeline       = Nothing
    , bindlessUIPipeline     = Nothing
    , fontPipeline           = Nothing
    , fontUIPipeline         = Nothing
    , fontQuadBuffer         = Nothing
    , fontDescriptorLayout   = Nothing
    , fontDescriptorPool     = Nothing
    , pendingInstanceBuffers = V.empty
    , cleanupStatus          = NotStarted
    , vulkanCleanup          = emptyCleanup
    , dynamicVertexBuffers   = V.replicate (gcMaxFrames defaultGraphicsConfig) Nothing
    , textInstanceBuffers    = V.replicate (gcMaxFrames defaultGraphicsConfig) Nothing
    , previewTexture         = Nothing
    , zoomAtlasTexture       = Nothing
    }
  , assetConfig      = AssetConfig 100 100 True True
  , sceneManager     = createSceneManager
  }
