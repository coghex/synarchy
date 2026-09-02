-- | The headless harness's post-action worker health check (#1388).
--
--   'Test.Headless.Harness.withHeadlessEngine' starts a real world
--   worker. When that worker fail-stops it catches its own exception,
--   logs it, and returns without rethrowing, so nothing reaches the
--   hspec example: the suite kept reporting green while every later
--   assertion ran against a dead worker. The harness now reads the
--   worker's own @tsDone@ after the action returns and fails the
--   example if it has already been filled.
--
--   These cases drive that decision directly with synthetic
--   'ThreadState's — an empty @tsDone@ is a live worker, a filled one
--   is a worker that already exited — because the healthy path is all
--   a real engine can exhibit now that #1362 removed the suite's one
--   live crash trigger. Two cases do boot a real engine, to prove the
--   wrapper is actually wired to the check and that the decision is
--   keyed on @tsDone@ rather than on 'EngineLifecycle'.
module Test.Headless.Harness.WorkerHealth (spec) where

import UPrelude
import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, isEmptyMVar)
import Control.Exception (try, evaluate, throwIO, SomeException)
import Data.IORef (newIORef, writeIORef)
import Data.List (isInfixOf)
import Test.Hspec
import Test.Headless.Harness
    ( HeadlessWorker(..)
    , worldWorker
    , checkHeadlessWorkers
    , withHeadlessWorkerCheck
    , withHeadlessEngine
    )
import Engine.Core.Log (LogCategory(..), defaultLogConfig, initLogger)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))

-- | A synthetic 'ThreadState' around the given done-'MVar'. The name,
--   logger and category exist only because 'shutdownThread' needs them
--   to report; nothing here ever shuts one of these down.
syntheticWorker ∷ IO (MVar ()) → IO ThreadState
syntheticWorker mkDone = do
    running ← newIORef ThreadRunning
    tid ← myThreadId
    done ← mkDone
    loggerRef ← newIORef =≪ initLogger defaultLogConfig
    pure ThreadState
        { tsRunning   = running
        , tsThreadId  = tid
        , tsDone      = done
        , tsName      = "Synthetic"
        , tsLoggerRef = loggerRef
        , tsCategory  = CatThread
        }

-- | A worker whose loop is still running: @tsDone@ is empty.
liveWorker ∷ IO ThreadState
liveWorker = syntheticWorker newEmptyMVar

-- | A worker whose loop has already exited: @tsDone@ is filled, exactly
--   as the @finally@ at the fork site leaves it.
exitedWorker ∷ IO ThreadState
exitedWorker = syntheticWorker (newMVar ())

-- | Run the check and report the failure message it produced, if any.
runCheck ∷ [HeadlessWorker] → [(HeadlessWorker, ThreadState)] → IO (Maybe String)
runCheck expectedStopped workers = do
    r ← try (checkHeadlessWorkers expectedStopped workers)
    pure $ case r of
        Right () → Nothing
        Left (e ∷ SomeException) → Just (show e)

spec ∷ Spec
spec = describe "headless harness worker health" $ do

    it "passes when every started worker is still running" $ do
        ts ← liveWorker
        runCheck [] [(worldWorker, ts)] ⌦ (`shouldBe` Nothing)

    it "fails and names the worker that already exited" $ do
        ts ← exitedWorker
        mMsg ← runCheck [] [(worldWorker, ts)]
        case mMsg of
            Nothing  → expectationFailure
                "expected a health failure for an exited worker"
            Just msg → do
                ("worker exited before teardown" `isInfixOf` msg)
                    `shouldBe` True
                ("world" `isInfixOf` msg) `shouldBe` True
                -- tsDone says THAT a thread ended, not why, so the
                -- report has to point at the worker's own crash line.
                ("World thread crashed" `isInfixOf` msg) `shouldBe` True

    it "names every exited worker, not just the first" $ do
        deadA ← exitedWorker
        deadB ← exitedWorker
        mMsg ← runCheck [] [ (HeadlessWorker "alpha", deadA)
                           , (HeadlessWorker "beta",  deadB) ]
        case mMsg of
            Nothing  → expectationFailure "expected a health failure"
            Just msg → do
                ("alpha" `isInfixOf` msg) `shouldBe` True
                ("beta"  `isInfixOf` msg) `shouldBe` True

    it "does not consume tsDone, so teardown still sees it filled" $ do
        -- shutdownThread joins on tsDone (a non-consuming readMVar,
        -- #2165); a probe that TOOK it would make failure-path teardown
        -- wait out its full graceful timeout and then kill a thread
        -- that had already exited, instead of returning at once.
        ts ← exitedWorker
        _ ← runCheck [] [(worldWorker, ts)]
        isEmptyMVar (tsDone ts) ⌦ (`shouldBe` False)

    it "keeps the action's own exception primary over a dead worker" $ do
        ts ← exitedWorker
        r ← try $ withHeadlessWorkerCheck [] [(worldWorker, ts)] $
                 throwIO (userError "the real assertion failure")
        case r of
            Right (_ ∷ ()) → expectationFailure
                "expected the action's own exception to propagate"
            Left (e ∷ SomeException) → do
                ("the real assertion failure" `isInfixOf` show e)
                    `shouldBe` True
                ("worker exited before teardown" `isInfixOf` show e)
                    `shouldBe` False

    it "returns the action's value when every worker is alive" $ do
        ts ← liveWorker
        v ← withHeadlessWorkerCheck [] [(worldWorker, ts)] (evaluate (7 ∷ Int))
        v `shouldBe` 7

    it "the opt-out exempts only the worker it names" $ do
        stopped ← exitedWorker
        other   ← exitedWorker
        -- The named worker is forgiven...
        runCheck [worldWorker] [(worldWorker, stopped)]
            ⌦ (`shouldBe` Nothing)
        -- ...and nothing else is: an unnamed worker that also exited
        -- still fails, so the opt-out cannot be used as a global off
        -- switch.
        mMsg ← runCheck [worldWorker] [ (worldWorker, stopped)
                                      , (HeadlessWorker "other", other) ]
        case mMsg of
            Nothing  → expectationFailure
                "the opt-out silenced a worker it did not name"
            Just msg → do
                -- Exactly the unnamed worker is reported...
                ("worker exited before teardown: other" `isInfixOf` msg)
                    `shouldBe` True
                -- ...and the exempted one is not.
                ("teardown: world" `isInfixOf` msg) `shouldBe` False

    it "the opt-out does not change action-exception precedence" $ do
        ts ← exitedWorker
        r ← try $ withHeadlessWorkerCheck [worldWorker] [(worldWorker, ts)] $
                 throwIO (userError "still primary")
        case r of
            Right (_ ∷ ()) → expectationFailure
                "expected the action's own exception to propagate"
            Left (e ∷ SomeException) →
                ("still primary" `isInfixOf` show e) `shouldBe` True

    it "wraps a real engine: a healthy action passes end to end" $
        withHeadlessEngine (\_ → pure (3 ∷ Int)) ⌦ (`shouldBe` 3)

    it "is keyed on tsDone, not on EngineLifecycle" $ do
        -- CleaningUp has many writers — every worker's crash handler,
        -- the input worker, the debug console, engine.quit()'s Lua
        -- handler, the normal loop and dump shutdown paths — so a
        -- lifecycle-keyed check would fail any test that legitimately
        -- quits the engine. The world worker never reads lifecycleRef,
        -- so it stays alive here and the example must pass.
        r ← try $ withHeadlessEngine $ \env →
                 writeIORef (lifecycleRef env) CleaningUp
        case r of
            Right () → pure ()
            Left (e ∷ SomeException) → expectationFailure $
                "a CleaningUp lifecycle failed the example: " ⧺ show e
