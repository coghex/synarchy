-- | Boot-mode plumbing shared by every entry point in @app\/App@: the
--   engine-config patch a mode applies over what
--   'Engine.Core.Init.initializeEngine' loaded, and the fatal-error tail
--   its @Left@ branch runs.
--
--   Both used to be hand-copied into all five boot modules, and the
--   copies had already drifted (#1021): the error paths of
--   'App.Graphical' and 'App.Offscreen' stopped the input thread
--   /before/ the world thread, the reverse of both their own happy paths
--   and of every other copy. Owning the order here is what keeps that
--   from happening again.
module App.Boot
  ( -- * Engine-config patch
    bootConfig
  , previewBootConfig
    -- * Worker teardown
  , BootWorkers(..)
  , shutdownBootWorkers
    -- * Fatal-error tail
  , FatalStream(..)
  , handleBootResult
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (readIORef)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Engine.Core.Error.Exception (EngineException)
import Engine.Core.Log (shutdownLogger)
import Engine.Core.State (EngineEnv, engineConfig, loggerRef)
import Engine.Core.Thread (ThreadState, shutdownThread)
import Engine.Core.Types (EngineConfig(..), BootProfile(..), PreviewBrowse)

-- | The config patch every non-preview boot mode applies: record the
--   boot profile, and take the debug port from the CLI when it supplied
--   one. 'Nothing' leaves 'ecDebugPort' at its configured default.
--
--   @--dump@ passes @Just 0@ deliberately: port 0 tells
--   'Engine.Scripting.Lua.DebugServer.startDebugServer' to start no TCP
--   listener at all.
bootConfig ∷ BootProfile → Maybe Int → EngineEnv → EngineEnv
bootConfig profile mPort = patchBootConfig profile mPort Nothing Nothing

-- | 'bootConfig' for @--preview@ (always 'BootPreview'), plus the two
--   preview-only fields: the requested @category[\/item]@ target and the
--   browsing state @app\/Main.hs@ already resolved (#886).
previewBootConfig ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int
                  → EngineEnv → EngineEnv
previewBootConfig target mBrowse mPort =
    patchBootConfig BootPreview mPort (Just target) mBrowse

-- | The one definition of the patch. Every mode reaches it through
--   'bootConfig' or 'previewBootConfig'; the preview fields stay at
--   their 'Engine.Core.Defaults.defaultEngineConfig' 'Nothing' for
--   every other mode.
patchBootConfig ∷ BootProfile → Maybe Int → Maybe (Text, Maybe Text)
                → Maybe PreviewBrowse → EngineEnv → EngineEnv
patchBootConfig profile mPort mTarget mBrowse env = env
    { engineConfig = base
        { ecDebugPort     = fromMaybe (ecDebugPort base) mPort
        , ecBootProfile   = profile
        , ecPreviewTarget = mTarget
        , ecPreviewBrowse = mBrowse
        } }
  where base = engineConfig env

-- | The worker threads one boot mode started — 'Nothing' for each one
--   its topology never starts (headless and dump run no input thread;
--   preview runs neither world, unit, sim nor combat).
--
--   Build it with record syntax and nothing else: there is no
--   all-'Nothing' value to update from, so @-Wall@'s
--   @-Wmissing-fields@ flags a boot module that forgets a thread it
--   started — a build failure under CI's @-Werror@. The teardown
--   /order/ is not expressible here at all: 'shutdownBootWorkers'
--   owns it.
data BootWorkers = BootWorkers
  { bwCombat ∷ Maybe ThreadState
  , bwSim    ∷ Maybe ThreadState
  , bwUnit   ∷ Maybe ThreadState
  , bwWorld  ∷ Maybe ThreadState
  , bwInput  ∷ Maybe ThreadState
  , bwLua    ∷ Maybe ThreadState
  }

-- | Stop every worker the mode started, in the single order shared by
--   all five boot modes: combat → sim → unit → world → input → Lua.
--
--   Combat and sim lead because they are producers for the unit thread:
--   wound ticks enqueue UnitKill\/UnitCollapse onto the unit queue, so
--   they have to stop before the consumer does. The unit → world →
--   input → Lua tail is exactly the order
--   'Engine.Loop.Shutdown.shutdownEngine' uses in its own worker
--   section, so an error path tears the engine down the same way a
--   clean exit does.
shutdownBootWorkers ∷ BootWorkers → IO ()
shutdownBootWorkers workers = do
    stop bwCombat
    stop bwSim
    stop bwUnit
    stop bwWorld
    stop bwInput
    stop bwLua
  where
    stop field = forM_ (field workers) shutdownThread

-- | Where a boot mode writes its fatal error text. Only @--dump@ uses
--   'FatalToStderr' — a failed dump must never pollute the JSON stdout
--   channel with success-shaped output.
data FatalStream = FatalToStdout | FatalToStderr

-- | The shared tail of every boot mode's 'Engine.Core.Monad.runEngineM'
--   result. On success, nothing happens. On failure: report the error
--   on the mode's stream, tear down every worker it started (in
--   'shutdownBootWorkers' order), flush the logger, and exit non-zero.
handleBootResult ∷ FatalStream → EngineEnv → BootWorkers
                 → Either EngineException () → IO ()
handleBootResult stream env workers result = case result of
    Right _  → pure ()
    Left err → do
        let report = case stream of
                FatalToStdout → putStrLn
                FatalToStderr → hPutStrLn stderr
        report $ displayException err
        shutdownBootWorkers workers
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env)
        shutdownLogger logger
        exitFailure
