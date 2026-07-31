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
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM')
import qualified Engine.Core.Queue as Q
import Engine.Core.State
  ( EngineEnv, EngineLifecycle(..), lifecycleRef, inputQueue, loggerRef )
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LogCategory(..), LogEntry(..), LogLevel(..)
  )
import Engine.Input.Types (InputEvent(..))
import Engine.Loop.Headless (headlessMode)
import Engine.Loop.Mode (runStartupHandshake)

spec ∷ Spec
spec = describe "shared main-loop startup handshake (#1022)" $
  it "flushes a non-empty input queue, reports it, and transitions to running" $ do
    EngineInitResult env ← initializeEngineHeadless
    capturedRef ← newIORef []
    testLogger ← initLogger defaultLogConfig
      { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :))
      }
    writeIORef (loggerRef env) testLogger
    writeIORef (lifecycleRef env) EngineStarting
    mapM_ (Q.writeQueue (inputQueue env))
      [InputCharEvent 'a', InputCharEvent 'b', InputCharEvent 'c']

    let action ∷ EngineM' EngineEnv ()
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
