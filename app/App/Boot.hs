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
  ) where

import UPrelude
import Control.Exception (displayException)
import Data.IORef (readIORef)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Engine.Core.Error.Exception (EngineException)
import Engine.Core.Log (shutdownLogger)
import Engine.Core.State (EngineEnv, engineConfig, loggerRef)
import Engine.Core.Workers (EngineWorkers, shutdownEngineWorkers)
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

-- | Where a boot mode writes its fatal error text. Only @--dump@ uses
--   'FatalToStderr' — a failed dump must never pollute the JSON stdout
--   channel with success-shaped output.
data FatalStream = FatalToStdout | FatalToStderr

-- | The shared tail of every boot mode's 'Engine.Core.Monad.runEngineM'
--   result. On success, nothing happens. On failure: report the error
--   on the mode's stream, tear down every worker it started (in
--   'Engine.Core.Workers.allWorkers' order), flush the logger, and exit
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
