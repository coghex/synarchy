-- | The headless suite's own engine-initialization boundary, and the one
--   place a fixture's log backend is chosen (#1925).
--
--   'Engine.Core.Init.initializeEngineHeadless' hard-wires
--   @LogToHandle stdout@, and 'Engine.Core.Log.Types.defaultLogConfig'
--   enables @LevelInfo@ — so every fixture that booted it wrote
--   production Info lines into the test runner's stdout, whether or not
--   the spec concerned logging. Across the suite's ~270 engine boots
--   that is the notification registry, the pathing config and every
--   worker's starting/started pair, none of which any assertion reads.
--
--   The backend has to be chosen BEFORE the engine boots, not after.
--   'Engine.Core.Init.initializeEngineWith' allocates the logger early
--   and hands it straight to @loadKeyBindings@, @loadVideoConfig@,
--   @loadPathingConfig@ and @loadNotificationCfg@, so a fixture that
--   swaps @loggerRef@ once the initializer has returned has already
--   emitted those lines. 'Engine.Core.Init.initializeEngineHeadlessWith'
--   is the seam that admits the choice, and @App.Dump@ has used it for
--   exactly this reason since #1191 — dump mode picks @stderr@ so
--   init-time logging can never reach the JSON on stdout.
--
--   This module is that seam's test-side counterpart. PRODUCTION
--   initialization is untouched: 'Engine.Core.Init.initializeEngineHeadless'
--   still logs to stdout for @App.Headless@, and @App.Dump@ still picks
--   stderr for itself.
--
--   Three entry points, in the order a spec should reach for them:
--
--     * 'initializeEngineHeadlessQuiet' — the default. A fixture that
--       expresses no logging preference uses this and produces no
--       engine log output at all.
--     * 'initializeEngineHeadlessLogging' — a fixture that wants the
--       entries names its own backend, and gets the ones emitted DURING
--       initialization as well as after.
--     * 'newLogCapture' — the concurrency-safe capture backend to pass
--       to the previous one. 'Engine.Core.Thread.startWorkerThreadEither'
--       logs through the shared backend around @forkIO@ and later
--       workers log concurrently, so a capture callback that is not
--       atomic loses entries.
--
--   __Recovering a quiet fixture's output.__ Quiet is the default, not a
--   one-way door: set @SYNARCHY_TEST_LOG@ and rerun. It is read once per
--   boot, before initialization, so it reaches the initialization and
--   worker-startup lines that motivated this module:
--
--   > SYNARCHY_TEST_LOG=stderr cabal test synarchy-test-headless \
--   >   --test-options='--match "Input.Followup"'
--
--   @stderr@ keeps the hspec formatter's own stdout clean and is the
--   value to reach for; @stdout@ restores the pre-#1925 stream exactly.
--   Unset or empty is quiet. Any other value is a hard error naming the
--   accepted set rather than a silent fall back to quiet — a typo in a
--   diagnostic knob must not look like a working one that found nothing.
--
--   The variable steers only 'initializeEngineHeadlessQuiet'. A fixture
--   that named its own backend has expressed a preference an
--   environment variable must not overrule, and overruling it would
--   break the specs that assert on what they captured.
module Test.Headless.Harness.Log
  ( initializeEngineHeadlessQuiet
  , initializeEngineHeadlessLogging
  , resolveFixtureLogBackend
  , quietLogBackend
  , newLogCapture
  , diagnosticLogEnvVar
  ) where

import UPrelude
import Data.Char (isSpace, toLower)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import System.Environment (lookupEnv)
import System.IO (stderr, stdout)

import Engine.Core.Init (initializeEngineHeadlessWith, EngineInitResult(..))
import Engine.Core.Log.Types (LogBackend(..), LogEntry)

-- | The environment variable that turns a quiet fixture back into a
--   logging one. Named here so a spec asserting on the mechanism and
--   the documentation above cannot drift apart.
diagnosticLogEnvVar ∷ String
diagnosticLogEnvVar = "SYNARCHY_TEST_LOG"

-- | A backend that accepts every entry and writes nothing. Total and
--   trivially concurrency-safe.
quietLogBackend ∷ LogBackend
quietLogBackend = LogToCallback (\_ → pure ())

-- | Which backend a preference-free fixture gets, given the current
--   environment. Separated from the boot so a spec can assert on the
--   choice without paying for an engine.
resolveFixtureLogBackend ∷ IO LogBackend
resolveFixtureLogBackend = do
    requested ← lookupEnv diagnosticLogEnvVar
    case map toLower ∘ trim ⊚ requested of
      Nothing       → pure quietLogBackend
      Just ""       → pure quietLogBackend
      Just "quiet"  → pure quietLogBackend
      Just "stderr" → pure (LogToHandle stderr)
      Just "stdout" → pure (LogToHandle stdout)
      Just other    → error $
        diagnosticLogEnvVar ⧺ "=" ⧺ other
          ⧺ " is not a recognized value; use one of quiet, stderr, stdout"
          ⧺ " (unset or empty is quiet)."
  where
    trim = dropWhile isSpace ∘ reverse ∘ dropWhile isSpace ∘ reverse

-- | Boot a headless engine that logs nothing, unless
--   'diagnosticLogEnvVar' asks otherwise. THE default for a headless
--   fixture: every @test-headless@ boot that has no opinion about
--   logging goes through here.
initializeEngineHeadlessQuiet ∷ IO EngineInitResult
initializeEngineHeadlessQuiet =
    initializeEngineHeadlessWith =≪ resolveFixtureLogBackend

-- | Boot a headless engine against an explicit backend, installed
--   before the initializer emits its first entry. This is what makes
--   initialization-time logging observable to a spec at all.
initializeEngineHeadlessLogging ∷ LogBackend → IO EngineInitResult
initializeEngineHeadlessLogging = initializeEngineHeadlessWith

-- | A capture backend plus the reader that drains it, in emission
--   order. The accumulator is updated atomically because worker startup
--   logs across threads.
newLogCapture ∷ IO (LogBackend, IO [LogEntry])
newLogCapture = do
    capturedRef ← newIORef []
    let backend = LogToCallback $ \entry →
          atomicModifyIORef' capturedRef $ \entries → (entry : entries, ())
    pure (backend, reverse ⊚ readIORef capturedRef)
