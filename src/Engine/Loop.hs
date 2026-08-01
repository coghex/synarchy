{-# LANGUAGE CPP #-}
module Engine.Loop
  ( mainLoop
  , mainLoopOffscreen
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Error.Exception (ExceptionType(..), SystemError(..))
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Loop.Timing (updateFrameTiming)
import Engine.Loop.Frame (drawFrame)
import Engine.Loop.Camera (updateCameraPanning, updateCameraMouseDrag
                          , updateCameraZoom)
import Engine.Loop.Mode (LoopMode(..), runLoopMode, frameBudgetMicros)

-- | Windowed main loop. See 'Engine.Loop.Mode.LoopMode' for what this
--   mode does that the others don't.
mainLoop ∷ EngineM σ ()
mainLoop = runLoopMode windowedMode

-- | Offscreen (#650) main loop: the windowed loop minus GLFW — no
--   event polling (input arrives only through the inject verbs, #644)
--   and no window-close check (engine.quit is the only exit). With no
--   vsync'd present to pace frames, each iteration sleeps a ~60 fps
--   budget so an offscreen instance doesn't spin the GPU — several may
--   run in parallel (that is this mode's point).
mainLoopOffscreen ∷ EngineM σ ()
mainLoopOffscreen = runLoopMode offscreenMode

windowedMode ∷ LoopMode σ
windowedMode = LoopMode
  { lmStartingLog   = "Engine starting..."
  , lmRunningLog    = Just "Engine running"
  , lmShutdownLog   = "Engine shutting down..."
  , lmCleaningUpLog = "Engine is cleaning up"
  , lmStoppedLog    = "Engine has stopped"
  , lmPollEvents    = requireWindow *> GLFW.pollEvents
  , lmCameraUpdates = cameraUpdates
  , lmExitRequested = do
        Window glfwWin ← requireWindow
        GLFW.windowShouldClose glfwWin
  , lmEndOfTick     = drawFrame *> updateFrameTiming
  }

offscreenMode ∷ LoopMode σ
offscreenMode = LoopMode
  { lmStartingLog   = "Engine starting..."
  , lmRunningLog    = Just "Engine running"
  , lmShutdownLog   = "Engine shutting down..."
  , lmCleaningUpLog = "Engine is cleaning up"
  , lmStoppedLog    = "Engine has stopped"
  , lmPollEvents    = pure ()
  , lmCameraUpdates = cameraUpdates
  , lmExitRequested = pure False
  , lmEndOfTick     = do
        drawFrame
        -- Frame pacing: no swapchain present blocks offscreen, so
        -- sleep the ~60 fps budget the windowed vsync provides.
        liftIO $ threadDelay frameBudgetMicros
        updateFrameTiming
  }

-- | The per-tick camera integration both rendering modes run, on an
--   unlocked tick only (see 'Engine.Loop.Mode.runGatedByCaptureLock').
cameraUpdates ∷ EngineM σ ()
cameraUpdates = do
    updateCameraPanning
    updateCameraZoom
    updateCameraMouseDrag

-- | The GLFW window the windowed mode cannot run without.
requireWindow ∷ EngineM σ Window
requireWindow = do
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logAndThrowM CatGraphics
                    (ExSystem (GLFWError "requireWindow: "))
                    "No GLFW window available"
        Just w  → pure w
