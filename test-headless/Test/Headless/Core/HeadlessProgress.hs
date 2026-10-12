-- | The headless suite's active-context recorder (#2747), driven through
--   its real integration: a synthetic spec is wrapped by
--   'Test.Headless.Progress.recordProgress' and run by Hspec itself, and
--   the examples and fixture hooks inside it read the recorder's state
--   WHILE they run. A model of the recorder kept beside it could agree
--   with itself and still be wrong about what Hspec does, which is the
--   whole risk here: Hspec runs a shared fixture's teardown as its own
--   queued job, after it has already announced the next example.
module Test.Headless.Core.HeadlessProgress (spec) where

import UPrelude
import Control.Exception (bracket)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (isInfixOf)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv, withArgs)
import System.Posix.Signals (sigTERM)
import Test.Hspec
import Test.Hspec.Core.Runner
    (Config(..), Summary(..), defaultConfig, hspecWithResult)
import Test.Headless.Progress

-- | Hspec's own configuration with a formatter that reports nothing, so a
--   synthetic run prints no lines into the suite that hosts it.
silentConfig ∷ Config
silentConfig = defaultConfig { configFormat = Just (\_ → pure (\_ → pure ())) }

-- | Run a spec through the real recorder and Hspec, output discarded.
--   Returns the recorder's state afterwards and Hspec's summary.
runRecorded ∷ ProgressRef → Spec → IO Summary
runRecorded ref s = withArgs []
    (hspecWithResult silentConfig
                     (recordProgress ref s))

-- | A recorder that only keeps its state, plus a log of snapshots the
--   spec's own actions append to while it runs.
fresh ∷ IO (ProgressRef, IORef [(String, Progress)])
fresh = (,) <$> newProgressRef (const (pure ())) <*> newIORef []

-- | An example body that does nothing (a bare @pure ()@ is ambiguous).
noop ∷ IO ()
noop = pure ()

snap ∷ ProgressRef → IORef [(String, Progress)] → String → IO ()
snap ref logRef label = do
    p ← readProgress ref
    modifyIORef' logRef ((label, p) :)

snapshots ∷ IORef [(String, Progress)] → IO [(String, Progress)]
snapshots = fmap reverse ∘ readIORef

at ∷ String → [(String, Progress)] → Progress
at label xs = maybe (error ("no snapshot " ⧺ label)) id (lookup label xs)

spec ∷ Spec
spec = do
    describe "the active example" $ do
        it "is the full describe/it path while the example runs" $ do
            (ref, logRef) ← fresh
            _ ← runRecorded ref $
                describe "Outer" $ describe "Inner" $
                    it "does the thing" (snap ref logRef "body")
            xs ← snapshots logRef
            progressActive (at "body" xs)
                `shouldBe` [["Outer", "Inner", "does the thing"]]

        it "has no preceding example before the first completion, then the previous one" $ do
            (ref, logRef) ← fresh
            _ ← runRecorded ref $ describe "G" $ do
                it "first" (snap ref logRef "first")
                it "second" (snap ref logRef "second")
            xs ← snapshots logRef
            progressPrevious (at "first" xs) `shouldBe` Nothing
            progressPrevious (at "second" xs) `shouldBe` Just ["G", "first"]
            progressCompleted (at "second" xs) `shouldBe` 1

        it "covers the example's per-example setup and teardown, completing it only after" $ do
            (ref, logRef) ← fresh
            _ ← runRecorded ref $ describe "G" $ do
                it "earlier" noop
                around_ (\run → snap ref logRef "setup" ≫ run
                                ≫ snap ref logRef "teardown") $
                    it "hooked" (snap ref logRef "body")
            xs ← snapshots logRef
            forM_ ["setup", "body", "teardown"] $ \l →
                (l, progressActive (at l xs)) `shouldBe` (l, [["G", "hooked"]])
            -- Still running its teardown: the example before it is the
            -- preceding one, and this one is not yet counted.
            progressPrevious (at "teardown" xs) `shouldBe` Just ["G", "earlier"]
            progressCompleted (at "teardown" xs) `shouldBe` 1
            final ← (readProgress ref)
            progressPrevious final `shouldBe` Just ["G", "hooked"]
            progressActive final `shouldBe` []
            progressCompleted final `shouldBe` 2

        it "completes a failing example too, and the run still reports the failure" $ do
            (ref, _) ← fresh
            summary ← runRecorded ref $ describe "G" $ do
                it "fails" (expectationFailure "boom")
                it "passes" noop
            summaryFailures summary `shouldBe` 1
            summaryExamples summary `shouldBe` 2
            final ← readProgress ref
            progressCompleted final `shouldBe` 2
            progressActive final `shouldBe` []

    describe "a shared fixture" $ do
        it "runs its setup inside the first example that needs it" $ do
            (ref, logRef) ← fresh
            _ ← runRecorded ref $
                aroundAll (\use → snap ref logRef "setup" ≫ use ()) $
                    describe "Fixture" $ do
                        it "first" noop
                        it "second" noop
            xs ← snapshots logRef
            progressActive (at "setup" xs) `shouldBe` [["Fixture", "first"]]

        it "tears down as a boundary with no example active, and names the group" $ do
            (ref, logRef) ← fresh
            _ ← runRecorded ref $ describe "Outer" $ do
                aroundAll (\use → use () ≫ snap ref logRef "teardown") $ do
                    describe "Fixture A" $ it "a" noop
                    describe "Fixture B" $ it "b" noop
                -- The example Hspec announces next, while the teardown
                -- above is still in flight. It must not be blamed.
                describe "After" $ it "next" (snap ref logRef "next")
            xs ← snapshots logRef
            let t = at "teardown" xs
            progressActive t `shouldBe` []
            progressPrevious t `shouldBe` Just ["Outer", "Fixture B", "b"]
            case progressBoundary t of
                [b] → do
                    boundaryGroups b `shouldBe` ["Outer"]
                    boundaryCovers b `shouldBe` ["Fixture A", "Fixture B"]
                bs → expectationFailure ("expected one boundary, got " ⧺ show bs)
            -- And it is gone once the teardown returns.
            progressBoundary (at "next" xs) `shouldBe` []
            progressActive (at "next" xs) `shouldBe` [["Outer", "After", "next"]]

    describe "the record" $ do
        it "names the active example, its predecessor and the lane" $ do
            let p = Progress { progressActive = [["A", "B", "c"]]
                             , progressBoundary = []
                             , progressPrevious = Just ["A", "B", "b"]
                             , progressCompleted = 7, progressFinished = False }
                txt = renderProgress "world" p
            lines txt `shouldBe`
                [ "headless-active-context v1"
                , "lane: world"
                , "state: running"
                , "completed examples: 7"
                , "active example: A / B / c"
                , "preceding completed example: A / B / b"
                , "fixture boundary: none" ]

        it "says none for an absent predecessor and names a teardown boundary" $ do
            let p = Progress { progressActive = []
                             , progressBoundary = [Boundary "afterAll_" ["Outer"] ["Fixture A"]]
                             , progressPrevious = Nothing
                             , progressCompleted = 0, progressFinished = False }
                txt = renderProgress "rest" p
            txt `shouldSatisfy` ("preceding completed example: none" `isInfixOf`)
            txt `shouldSatisfy` ("active example: none" `isInfixOf`)
            txt `shouldSatisfy` ("afterAll_ cleanup closing \"Fixture A\" (inside Outer); no example is active" `isInfixOf`)

        it "names the signal in an interrupted run's report" $ do
            let p = Progress [["A", "c"]] [] Nothing 0 False
                txt = interruptReport sigTERM "rest" p
            txt `shouldSatisfy` ("interrupted by signal" `isInfixOf`)
            txt `shouldSatisfy` ("active example: A / c" `isInfixOf`)

    describe "the run wiring" $ do
        it "does nothing at all when the environment does not ask" $
            withEnvFile False $ \file → do
                (ref, logRef) ← fresh
                withProgressFromEnv "rest" $ \instrument → do
                    _ ← withArgs []
                        (hspecWithResult silentConfig
                            (instrument (it "x" (snap ref logRef "x"))))
                    pure ()
                xs ← snapshots logRef
                -- Not wrapped: the recorder never saw the example.
                progressActive (at "x" xs) `shouldBe` []
                doesFileExist file `shouldReturn` False

        it "mirrors the record to the file while running and marks it finished" $
            withEnvFile True $ \file → do
                seen ← newIORef ""
                withProgressFromEnv "world" $ \instrument → do
                    _ ← withArgs []
                        (hspecWithResult silentConfig
                            (instrument $ describe "S" $ it "ex" $
                                readFile file ≫= \t → length t `seq` modifyIORef' seen (const t)))
                    pure ()
                during ← readIORef seen
                during `shouldSatisfy` ("state: running" `isInfixOf`)
                during `shouldSatisfy` ("active example: S / ex" `isInfixOf`)
                after ← readFile file
                after `shouldSatisfy` ("state: finished" `isInfixOf`)
                after `shouldSatisfy` ("completed examples: 1" `isInfixOf`)
                after `shouldSatisfy` ("lane: world" `isInfixOf`)
                doesFileExist (file ⧺ ".tmp") `shouldReturn` False

-- | Run with 'progressFileEnv' pointing at a fresh file (or unset), then
--   restore it and remove the file.
withEnvFile ∷ Bool → (FilePath → IO a) → IO a
withEnvFile on act = do
    tmp ← getTemporaryDirectory
    let file = tmp ⧺ "/synarchy-headless-progress-test.txt"
    old ← lookupEnv progressFileEnv
    let cleanup = do
            maybe (unsetEnv progressFileEnv) (setEnv progressFileEnv) old
            exists ← doesFileExist file
            when exists (removeFile file)
    bracket
        (do if on then setEnv progressFileEnv file
                  else unsetEnv progressFileEnv
            pure file)
        (const cleanup)
        act
