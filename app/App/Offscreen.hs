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
import Data.IORef (readIORef, writeIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngine, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types (BootProfile(..))
import Engine.Core.Workers (EngineWorkers(..))
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Graphics.Vulkan.Init (initializeVulkanOffscreen)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoopOffscreen)
import Engine.Loop.Shutdown (ShutdownTargets(..), shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Thread (startWorldThread)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import App.Boot (FatalStream(..), bootConfig, handleBootResult)
import App.Exception (guardNativeExceptions)

-- | Run the engine offscreen: GPU on, window off. The render size
--   defaults to the video-config resolution (matching what a windowed
--   run of the same machine would show) and can be pinned with
--   @--size WxH@ so parallel harness runs are deterministic regardless
--   of local config.
runOffscreen ∷ BootProfile → Maybe Int → Maybe (Int, Int) → IO ()
runOffscreen bootProfile mPort mSize = do
  EngineInitResult env ← initializeEngine

  let env' = bootConfig bootProfile mPort env

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  let workers = EngineWorkers
        { ewCombat = Just combatThreadState
        , ewSim    = Just simThreadState
        , ewUnit   = Just unitThreadState
        , ewWorld  = Just worldThreadState
        , ewInput  = Just inputThreadState
        , ewLua    = Just luaThreadState
        }

  videoConfig ← readIORef (videoConfigRef env')
  let (w, h) = fromMaybe (vcWidth videoConfig, vcHeight videoConfig) mSize

  -- What GLFW.createWindow does for windowed boots: seed the size refs
  -- and tell the Lua UI its (fixed) framebuffer geometry so layout
  -- runs against the real render size.
  writeIORef (windowSizeRef env') (w, h)
  writeIORef (framebufferSizeRef env') (w, h)
  Q.writeQueue (luaQueue env') (LuaWindowResize w h)
  Q.writeQueue (luaQueue env') (LuaFramebufferResize w h)

  let engineAction ∷ EngineM' ()
      engineAction = do
        logInfoM CatSystem "Starting engine (offscreen)..."
        _ ← initializeVulkanOffscreen (w, h)
        mainLoopOffscreen

        shutdownEngine ShutdownTargets { stWindow  = Nothing
                                       , stWorkers = workers }
        logDebugM CatSystem "Offscreen engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStdout env' workers result
