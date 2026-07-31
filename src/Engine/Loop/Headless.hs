module Engine.Loop.Headless
  ( headlessLoop
  , headlessMode
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Engine.Core.Monad
import Engine.Loop.Mode (LoopMode(..), runLoopMode, frameBudgetMicros)

-- | Headless main loop: processes messages without rendering. Lua
--   messages are dispatched through the SAME 'processLuaMessages' as the
--   graphical loop — GPU operations no-op themselves when headless (via
--   'whenGraphical'), so there is no separate headless dispatcher to
--   drift out of sync with the graphical one. For the same reason this
--   loop is a full 'Engine.Save.Barrier.SaveRender' owner: it drains
--   the same queue, so a headless load's publish must wait for it too
--   — see 'Engine.Loop.Mode.runGatedByCaptureLock', which is the one
--   place that handshake is defined and explained.
headlessLoop ∷ EngineM ε σ ()
headlessLoop = runLoopMode headlessMode

-- | The headless mode's answers to 'LoopMode'\'s per-mode differences:
--   no window to pump, no camera to integrate, no frame to draw, and
--   exit only via @engine.quit@ (i.e. the lifecycle). All it does past
--   the shared gated drain is pace itself.
headlessMode ∷ LoopMode ε σ
headlessMode = LoopMode
  { lmStartingLog   = "Headless engine starting..."
  , lmRunningLog    = Nothing
  , lmShutdownLog   = "Headless engine shutting down..."
  , lmCleaningUpLog = "Headless engine cleaning up"
  , lmStoppedLog    = "Headless engine stopped"
  , lmPollEvents    = pure ()
  , lmCameraUpdates = pure ()
  , lmExitRequested = pure False
  , lmEndOfTick     = liftIO $ threadDelay frameBudgetMicros
  }
