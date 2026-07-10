-- | Offscreen boot path (#650): full Vulkan render with no window —
--   no GLFW at all. The complete engine runs (every worker thread,
--   the real Lua UI stack, the debug console), frames render to
--   offscreen images, debug.captureScreenshot reads them back, and
--   input arrives only through the inject verbs (#644). No focus is
--   stolen and no window appears, so playtest-harness campaigns can
--   run unattended and several instances can run in parallel on
--   distinct ports.
module App.Offscreen
  ( runOffscreen
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (readIORef, writeIORef)
import System.Exit (exitFailure)
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types (EngineConfig(..), BootProfile(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Log (LogCategory(..), shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Graphics.Vulkan.Init (initializeVulkanOffscreen)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoopOffscreen)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import App.Exception (guardNativeExceptions)

-- | Run the engine offscreen: GPU on, window off. The render size
--   defaults to the video-config resolution (matching what a windowed
--   run of the same machine would show) and can be pinned with
--   @--size WxH@ so parallel harness runs are deterministic regardless
--   of local config.
runOffscreen ∷ BootProfile → Maybe Int → Maybe (Int, Int) → IO ()
runOffscreen bootProfile mPort mSize = do
  EngineInitResult env ← initializeEngine

  let env' = case mPort of
        Just p  → env
            { engineConfig = (engineConfig env)
                { ecDebugPort = p
                , ecBootProfile = bootProfile
                } }
        Nothing → env
            { engineConfig = (engineConfig env)
                { ecBootProfile = bootProfile
                } }

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  videoConfig ← readIORef (videoConfigRef env')
  let (w, h) = fromMaybe (vcWidth videoConfig, vcHeight videoConfig) mSize

  -- What GLFW.createWindow does for windowed boots: seed the size refs
  -- and tell the Lua UI its (fixed) framebuffer geometry so layout
  -- runs against the real render size.
  writeIORef (windowSizeRef env') (w, h)
  writeIORef (framebufferSizeRef env') (w, h)
  Q.writeQueue (luaQueue env') (LuaWindowResize w h)
  Q.writeQueue (luaQueue env') (LuaFramebufferResize w h)

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (offscreen)..."
        _ ← initializeVulkanOffscreen (w, h)
        mainLoopOffscreen

        -- Combat first: wound ticks enqueue UnitKill/UnitCollapse onto
        -- the unit queue, so the producer has to stop before the
        -- consumer (unit thread) is torn down inside shutdownEngine.
        liftIO $ shutdownThread combatThreadState
        liftIO $ shutdownThread simThreadState
        shutdownEngine Nothing (Just unitThreadState)
                       (Just worldThreadState) inputThreadState luaThreadState
        logDebugM CatSystem "Offscreen engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread combatThreadState
        shutdownThread simThreadState
        shutdownThread unitThreadState
        shutdownThread inputThreadState
        shutdownThread worldThreadState
        shutdownThread luaThreadState
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env')
        shutdownLogger logger
        exitFailure
    Right _ → pure ()
