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
import Engine.Core.Error.Exception (ExceptionType(..), SystemError(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Vulkan.Recreate (recreateSwapchainFor)
import Engine.Graphics.Vulkan.ResizeRequest
  (FramebufferResizeAction(..), noteMinimizedFramebuffer
  , pendingFramebufferResize)
import Engine.Graphics.Types (FramebufferState(..))
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
  , lmEndOfTick     = applyPendingFramebufferResize
                          *> drawFrame *> updateFrameTiming
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

-- | Rebuild the swapchain when the window's framebuffer no longer
--   matches the one the live swapchain was built for (#1693).
--
--   Windowed only, deliberately: this is a field of 'windowedMode'
--   alone, so neither the offscreen loop nor the headless loop can
--   reach 'recreateSwapchainFor' — and offscreen in particular can
--   never take a swapchain path with no window behind it.
--   @App.Preview@ IS covered, because it runs this same loop.
--   Belt and braces: neither non-windowed mode ever seeds
--   'Engine.Core.State.swapchainFbSize', and
--   'pendingFramebufferResize' answers 'ResizeUpToDate' whenever that
--   record is unset.
--
--   Run BEFORE the frame rather than after it, so the frame that
--   follows a resize already presents at the new extent instead of
--   showing one stale-extent frame first.
--
--   Requesting is separate from consuming
--   ("Engine.Graphics.Vulkan.ResizeRequest"): 'pendingFramebufferResize'
--   only reads, and 'recreateSwapchainFor' records the size it was
--   given. A resize that lands while that rebuild runs is therefore
--   still outstanding when this returns, and gets its own recreation
--   next tick.
--
--   'requireWindow' is the same accessor the rest of this mode uses;
--   it cannot fail here, because 'lmPollEvents' and 'lmExitRequested'
--   have both already demanded a window this very tick.
applyPendingFramebufferResize ∷ EngineM σ ()
applyPendingFramebufferResize = do
    action ← pendingFramebufferResize
    case action of
        ResizeUpToDate        → pure ()
        ResizeMinimized  fbSt → noteMinimizedFramebuffer fbSt
        ResizeRecreate   fbSt → do
            let (w, h) = fbsSize fbSt
            logDebugM CatSwapchain $
                "Framebuffer resized to " <> tshow w <> "x" <> tshow h
                <> ", recreating swapchain"
            -- The state that was REQUESTED, not a fresh sample:
            -- serving a request must record exactly the state it was
            -- judged against, or a request could outlive its own
            -- recreation and repeat forever.
            window ← requireWindow
            recreateSwapchainFor window fbSt

-- | The per-tick camera integration both rendering modes run, on an
--   unlocked tick only (see @Engine.Loop.Mode.runGatedByCaptureLock@).
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
