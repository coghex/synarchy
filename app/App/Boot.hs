-- | Boot-mode plumbing shared by every entry point in @app\/App@: the
--   engine-config patch a mode applies over what
--   'Engine.Core.Init.initializeEngine' loaded, and the fatal-error tail
--   its @Left@ branch runs.
--
--   Both used to be hand-copied into all five boot modules, and the
--   copies had already drifted (#1021): the error paths of
--   'App.Graphical' and 'App.Offscreen' stopped the input thread
--   /before/ the world thread, the reverse of both their own happy
--   paths and of every other copy. The teardown order itself now lives
--   in 'Engine.Core.Workers' (#1036) — library-side, so the clean exit
--   inside 'Engine.Loop.Shutdown.shutdownEngine' and the fatal tail
--   below run the same one instead of two that have to be kept in
--   agreement.
module App.Boot
  ( -- * Engine-config patch
    bootConfig
  , previewBootConfig
    -- * Fatal-error tail
  , FatalStream(..)
  , handleBootResult
    -- * Required-debug-console boot failure (#1190)
  , luaThreadOrAbort
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (readIORef)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import Engine.Core.Error.Exception (EngineException)
import Engine.Core.Log (shutdownLogger)
import Engine.Core.State (EngineEnv, engineConfig, loggerRef)
import Engine.Core.Thread (ThreadState)
import Engine.Core.Workers (EngineWorkers, WorkerSlot, shutdownEngineWorkers
                           , stopWorkers)
import Engine.Core.Types (EngineConfig(..), BootProfile(..), BootMode(..)
                         , PreviewBrowse)
import Engine.Scripting.Lua.DebugServer (DebugListenerFailure, reportBootCleanup)

-- | The config patch every non-preview boot mode applies: record which
--   boot mode argv selected and the profile it boots with, and take the
--   debug port from the CLI when it supplied one. 'Nothing' leaves
--   'ecDebugPort' at its configured default.
--
--   @--dump@ passes @Just 0@ deliberately: port 0 tells
--   'Engine.Scripting.Lua.DebugServer.startDebugServer' to start no TCP
--   listener at all. That sentinel is honoured for 'ModeDump' (and the
--   two windowed modes) only — the mode recorded here is what lets
--   'Engine.Scripting.Lua.DebugServer.listenerAction' refuse the same 0
--   for @--headless@\/@--offscreen@ (#1190).
bootConfig ∷ BootMode → BootProfile → Maybe Int → EngineEnv → EngineEnv
bootConfig mode profile mPort = patchBootConfig mode profile mPort Nothing Nothing

-- | 'bootConfig' for @--preview@ (always 'ModePreview' \/
--   'BootPreview'), plus the two preview-only fields: the requested
--   @category[\/item]@ target and the browsing state @app\/Main.hs@
--   already resolved (#886).
previewBootConfig ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int
                  → EngineEnv → EngineEnv
previewBootConfig target mBrowse mPort =
    patchBootConfig ModePreview BootPreview mPort (Just target) mBrowse

-- | The one definition of the patch. Every mode reaches it through
--   'bootConfig' or 'previewBootConfig'; the preview fields stay at
--   their 'Engine.Core.Defaults.defaultEngineConfig' 'Nothing' for
--   every other mode.
patchBootConfig ∷ BootMode → BootProfile → Maybe Int
                → Maybe (Text, Maybe Text)
                → Maybe PreviewBrowse → EngineEnv → EngineEnv
patchBootConfig mode profile mPort mTarget mBrowse env = env
    { engineConfig = base
        { ecDebugPort     = fromMaybe (ecDebugPort base) mPort
        , ecBootProfile   = profile
        , ecBootMode      = mode
        , ecPreviewTarget = mTarget
        , ecPreviewBrowse = mBrowse
        } }
  where base = engineConfig env

-- | Where a boot mode writes its fatal error text. Only @--dump@ uses
--   'FatalToStderr' — a failed dump must never pollute the JSON stdout
--   channel with success-shaped output.
data FatalStream = FatalToStdout | FatalToStderr

-- | The shared tail of every boot mode's 'Engine.Core.Monad.runEngineM'
--   result. On success, nothing happens. On failure: report the error
--   on the mode's stream, tear down every worker it started (in
--   @Engine.Core.Workers.allWorkers@ order), flush the logger, and exit
--   non-zero.
handleBootResult ∷ FatalStream → EngineEnv → EngineWorkers
                 → Either EngineException () → IO ()
handleBootResult stream env workers result = case result of
    Right _  → pure ()
    Left err → do
        let report = case stream of
                FatalToStdout → putStrLn
                FatalToStderr → hPutStrLn stderr
        report $ displayException err
        shutdownEngineWorkers workers
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env)
        shutdownLogger logger
        exitFailure

-- | Take the Lua thread a boot mode just started — or, when that mode
--   required a debug console it could not have, abort the boot (#1190).
--
--   @--headless@ and @--offscreen@ have no window and no other
--   interactive surface, so a dead listener used to leave a process
--   that had printed no @READY@ line to wait on, could not be reached
--   through @engine.quit()@, and still held every worker thread it went
--   on to start. This is the tail that turns that into a clean non-zero
--   exit.
--
--   'started' is the workers this mode had ALREADY brought up when it
--   called 'Engine.Scripting.Lua.Thread.startLuaThread' — empty for
--   headless and dump (Lua is their first worker), the input thread for
--   the three modes that start it first — in
--   @Engine.Core.Workers.allWorkers@ teardown order. Passing the real
--   partial set (rather than the full record the mode would have built
--   on success) is what makes the teardown honest: there is no
--   half-constructed 'EngineWorkers' to stop threads that were never
--   forked.
--
--   The Lua state and the failure diagnostic are already handled by
--   'Engine.Scripting.Lua.Thread.startLuaThread' itself. Every mode
--   routes through here, including the three whose
--   'Engine.Scripting.Lua.DebugServer.debugConsolePolicy' is
--   'Engine.Scripting.Lua.DebugServer.ConsoleOptional' and therefore
--   can never reach the 'Left' branch: a mode that later becomes
--   console-required inherits the correct behaviour instead of silently
--   continuing with an inert command queue.
luaThreadOrAbort ∷ EngineEnv → [WorkerSlot]
                 → Either DebugListenerFailure ThreadState → IO ThreadState
luaThreadOrAbort _   _       (Right threadState) = pure threadState
luaThreadOrAbort env started (Left _) = do
    let live = [ slot | slot@(_, Just _) ← started ]
    -- Announced BY 'stopWorkers' as each one is actually stopped, so
    -- the trace is evidence of the teardown rather than a claim about
    -- it (the process is about to exit, which reclaims threads either
    -- way).
    stopWorkers (\name → reportBootCleanup ("stopped the " <> name <> " worker"))
                live
    reportBootCleanup $
        T.pack (show (length live)) <> " worker thread(s) stopped"
    -- Same tail as 'handleBootResult': flush buffered log lines before
    -- exiting, so the boot's own context isn't lost with the process.
    logger ← readIORef (loggerRef env)
    shutdownLogger logger
    exitFailure
