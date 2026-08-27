{-# LANGUAGE CPP #-}
module Engine.Core.Defaults where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Core.Types
import Engine.Core.State
import Engine.Graphics.Base
import Engine.Graphics.Config
import Engine.Graphics.Window.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Cleanup (emptyCleanup)
import Engine.Scene.Types

-- | The debug console's default TCP port — the one place the value is
--   written (#1086). @app\/Main.hs@ used to repeat the literal at every
--   port-taking dispatch; it now hands the CLI's own @Maybe Int@
--   straight through, and 'App.Boot.patchBootConfig' resolves an absent
--   @--port@ against this.
--
--   CLAUDE.md warns agents off it repeatedly: it is the port the
--   developer's graphical instance binds, so an automated run picks its
--   own (@--port 9008@) rather than colliding here.
defaultDebugPort ∷ Int
defaultDebugPort = 8008

defaultEngineConfig ∷ EngineConfig
defaultEngineConfig = EngineConfig
  { ecHeadless   = False
  , ecDebugPort  = defaultDebugPort
  , ecBootProfile = BootNormal
  -- Matches 'ecHeadless' above: the mode argv selects when it names no
  -- selector flag at all. Every real boot immediately stamps its own
  -- over this (App.Boot.bootConfig / previewBootConfig), and
  -- 'Engine.Core.Init.initializeEngineHeadlessWith' corrects it
  -- alongside 'ecHeadless' for the windowless initializer.
  , ecBootMode = ModeGraphical
  , ecPreviewTarget = Nothing
  , ecPreviewBrowse = Nothing
  }

defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig
  { gcAppName   = T.pack "Synarchy"
#ifdef DEVELOPMENT
  , gcDebugMode = True
#else
  , gcDebugMode = False
#endif
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
    { fpsWindowFrames  = 0
    , deltaTime        = 0.0
    , fpsWindowElapsed = 0.0
    , lastFrameTime    = 0.0
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
    , swapchainFbState       = Nothing
    , msaaColorImage         = Nothing
    , renderFinishedSems     = V.empty
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
  , sceneManager     = createSceneManager
  }
