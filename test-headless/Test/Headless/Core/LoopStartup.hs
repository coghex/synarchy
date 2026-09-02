-- | Regression coverage for issue #1022's shared main-loop startup
--   handshake ('Engine.Loop.Mode.runStartupHandshake').
--
--   All three boot modes used to hand-write their own startup step, and
--   the copies had already diverged: the windowed/offscreen one warned
--   when 'inputQueue' was non-empty at startup, while the headless one
--   flushed the queue and threw the result away. A headless boot starts
--   no input thread, but 'Engine.Scripting.Lua.Thread.Dispatch'\'s
--   @LuaInjectFollowup@ writes that queue from the input injection
--   verbs (#644), so the condition is reachable there too and was
--   simply silent. This drives the REAL headless mode's handshake with
--   a pre-populated queue.
module Test.Headless.Core.LoopStartup (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Monad (runEngineM, EngineM')
import qualified Engine.Core.Queue as Q
import Engine.Core.State
  ( EngineLifecycle(..), lifecycleRef, inputQueue, loggerRef )
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LogCategory(..), LogEntry(..), LogLevel(..)
  )
import Engine.Input.Types (InputEvent(..))
import Engine.Loop.Headless (headlessMode)
import Engine.Loop.Mode (LoopMode(..), runStartupHandshake)

-- | The running line both rendering modes carry ('Engine.Loop').
runningLine ∷ Text
runningLine = "Engine running"

spec ∷ Spec
spec = describe "shared main-loop startup handshake (#1022)" $ do
  it "flushes a non-empty input queue, reports it, and transitions to running" $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    capturedRef ← newIORef []
    testLogger ← initLogger defaultLogConfig
      { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :))
      }
    writeIORef (loggerRef env) testLogger
    writeIORef (lifecycleRef env) EngineStarting
    mapM_ (Q.writeQueue (inputQueue env))
      [InputCharEvent 'a', InputCharEvent 'b', InputCharEvent 'c']

    let action ∷ EngineM' ()
        action = runStartupHandshake headlessMode env
    _ ← runEngineM action env pure

    leftover ← Q.flushQueue (inputQueue env)
    length leftover `shouldBe` 0

    lifecycle ← readIORef (lifecycleRef env)
    lifecycle `shouldBe` EngineRunning

    entries ← readIORef capturedRef
    let warnings = [ e | e ← entries
                       , leLevel e ≡ LevelWarn, leCategory e ≡ CatThread ]
    case warnings of
      [entry] → leMessage entry
        `shouldBe` "Unexpected inputs during startup: 3 events flushed"
      other → expectationFailure $
        "expected exactly one CatThread startup warning, got "
          ⧺ show (length other)

  -- #1283: the promotion used to be an unconditional write, which
  -- silently discarded a shutdown another thread had already
  -- requested. engine.quit() runs on the debug console's own client
  -- thread and only writes CleaningUp, so a quit accepted between the
  -- READY print and this handshake left an engine that had already
  -- acked "shutting down" running forever, unstoppable through the one
  -- control surface a headless boot has.
  forM_ [ (CleaningUp,    "a shutdown requested during startup")
        , (EngineStopped, "an engine that already stopped")
        ] $ \(advanced, label) →
    it ("preserves " ⧺ label ⧺ " instead of overwriting it with running") $ do
      EngineInitResult env ← initializeEngineHeadlessQuiet
      writeIORef (lifecycleRef env) advanced

      let action ∷ EngineM' ()
          action = runStartupHandshake headlessMode env
      _ ← runEngineM action env pure

      readIORef (lifecycleRef env) `shouldReturn` advanced

  it "still promotes a genuinely starting engine, and is idempotent for one already running" $ do
    forM_ [(EngineStarting, EngineRunning), (EngineRunning, EngineRunning)] $
      \(before, after) → do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        writeIORef (lifecycleRef env) before
        let action ∷ EngineM' ()
            action = runStartupHandshake headlessMode env
        _ ← runEngineM action env pure
        readIORef (lifecycleRef env) `shouldReturn` after

  -- #1263: the lifecycle is only half of what the handshake decides.
  -- lmRunningLog belongs to the EngineStarting → EngineRunning
  -- transition, so a handshake whose promotion was REFUSED must not go
  -- on to announce that the engine is running. Headless carries no
  -- running line at all (lmRunningLog = Nothing); both rendering modes
  -- carry Just "Engine running", so the mode under test supplies one.
  forM_ [ (EngineStarting, True,  "announces it when the promotion commits")
        , (CleaningUp,     False, "withholds it when a shutdown already won")
        , (EngineStopped,  False, "withholds it when the engine already stopped")
        ] $ \(before, announced, label) →
    it ("running line: " ⧺ label) $ do
      EngineInitResult env ← initializeEngineHeadlessQuiet
      capturedRef ← newIORef []
      testLogger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :))
          -- The running line is a DEBUG line, suppressed by default.
        , lcDebugCategories = [CatSystem]
        }
      writeIORef (loggerRef env) testLogger
      writeIORef (lifecycleRef env) before

      let action ∷ EngineM' ()
          action = runStartupHandshake
                     headlessMode { lmRunningLog = Just runningLine } env
      _ ← runEngineM action env pure

      entries ← readIORef capturedRef
      any (\e → leMessage e ≡ runningLine) entries `shouldBe` announced
      -- The announcement and the transition must agree.
      readIORef (lifecycleRef env)
        `shouldReturn` (if announced then EngineRunning else before)

  -- The real interleaving, not just the resulting mapping: a quit that
  -- lands WHILE the handshake is running. A separate read-then-write
  -- would still lose this one, which is why the promotion has to be a
  -- single atomic step.
  it "keeps a quit that arrives midway THROUGH the handshake" $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    writeIORef (lifecycleRef env) EngineStarting
    -- Inside the settle the handshake performs before promoting.
    _ ← forkIO $ do
          threadDelay 30000
          writeIORef (lifecycleRef env) CleaningUp

    let action ∷ EngineM' ()
        action = runStartupHandshake headlessMode env
    _ ← runEngineM action env pure

    readIORef (lifecycleRef env) `shouldReturn` CleaningUp
