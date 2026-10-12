-- | The headless suite's active-context recorder (#2747, CIR-3 of epic
--   #2742).
--
--   A headless CI run that hangs until the job timeout, or is cancelled,
--   used to leave only the last line the formatter printed. Under
--   @--format=failed-examples@ that line is not even an example: a
--   passing run prints no per-example output at all, so the log named
--   neither the example that stalled nor the one before it.
--
--   This module records, for a run that asks for it, which example is
--   active and which one completed before it, and writes that as a small
--   text record that CI prints when the step did not finish. It prints
--   NOTHING itself, owns no timer and changes no scheduling: it wraps the
--   spec tree's own items and cleanups, so a passing run's output is the
--   formatter's, unchanged.
--
--   What is recorded, and why it is done on the tree rather than from
--   Hspec's formatter events:
--
--     * An EXAMPLE is active from before its per-example setup until after
--       its per-example teardown: 'recordProgress' wraps the item's whole
--       action, so an @around@ hook, a @before@ and the shared fixture's
--       one-time acquisition (which Hspec runs inside the first item that
--       needs it) are all inside it. It is COMPLETE only then, and becomes
--       the preceding example for the next.
--     * A shared-fixture TEARDOWN (@aroundAll@ / @afterAll@) is not any
--       example's work. Hspec runs it as its own job, queued after the
--       group's last example, so by the time it hangs the formatter has
--       already announced the NEXT example. A recorder driven by formatter
--       events would blame that example, which has not started. Each
--       cleanup is wrapped and recorded as a boundary of the group it
--       closes, with no example active.
--
--   The record is written only when 'progressFileEnv' names a file, so a
--   developer's targeted run is unchanged; CI sets it.
module Test.Headless.Progress
    ( -- * The recorder
      Progress(..)
    , Boundary(..)
    , ProgressRef
    , newProgressRef
    , readProgress
    , recordProgress
      -- * The record
    , renderProgress
    , interruptReport
      -- * Wiring a run
    , progressFileEnv
    , withProgressFromEnv
    ) where

import UPrelude
import Control.Exception (SomeException, finally, try)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List (intercalate, delete)
import System.Directory (renameFile)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)
import System.Posix.Signals
    ( Handler(..), installHandler, raiseSignal, sigINT, sigTERM
    , Signal )
import Test.Hspec (Spec)
import Test.Hspec.Core.Spec
    ( Item(..), SpecTree, Tree(..), mapSpecForest )

-- | A shared-fixture boundary that is in progress with no example behind
--   it: a cleanup Hspec queued after a group's last example.
data Boundary = Boundary
    { boundaryKind   ∷ String    -- ^ the hook that registered it, e.g. @afterAll_@
    , boundaryGroups ∷ [String]  -- ^ the groups enclosing the cleanup
    , boundaryCovers ∷ [String]  -- ^ the groups the fixture wraps
    } deriving (Show, Eq)

data Progress = Progress
    { progressActive    ∷ [[String]]    -- ^ full paths, oldest first
    , progressBoundary  ∷ [Boundary]    -- ^ innermost first
    , progressPrevious  ∷ Maybe [String] -- ^ the last COMPLETED example
    , progressCompleted ∷ Int
    , progressFinished  ∷ Bool
    } deriving (Show, Eq)

-- | The recorder's state and the sink each change is mirrored to.
data ProgressRef = ProgressRef
    { refState ∷ IORef Progress
    , refSink  ∷ Progress → IO ()
    }

newProgressRef ∷ (Progress → IO ()) → IO ProgressRef
newProgressRef sink = ProgressRef
    <$> newIORef (Progress [] [] Nothing 0 False)
    <*> pure sink

readProgress ∷ ProgressRef → IO Progress
readProgress = readIORef ∘ refState

-- | Apply a change and mirror the result to the sink. A sink that throws
--   is dropped on the floor: the record must never fail the suite.
update ∷ ProgressRef → (Progress → Progress) → IO ()
update ref f = do
    new ← atomicModifyIORef' (refState ref) (\p → let p' = f p in (p', p'))
    r ← try (refSink ref new) ∷ IO (Either SomeException ())
    either (const (pure ())) pure r

-- | Wrap every example and every fixture cleanup in the spec so each is
--   recorded. The paths are the tree's own, so a describe/it path here is
--   the one the formatter would print.
recordProgress ∷ ProgressRef → Spec → Spec
recordProgress ref = mapSpecForest (map (wrap []))
  where
    wrap ∷ [String] → SpecTree () → SpecTree ()
    wrap groups tree = case tree of
        Node name children →
            Node name (map (wrap (groups ⧺ [name])) children)
        NodeWithCleanup loc cleanup children →
            let boundary = Boundary
                    { boundaryKind   = maybe "cleanup" fst loc
                    , boundaryGroups = groups
                    , boundaryCovers = [ n | Node n _ ← children ] }
                cleanup' = do
                    update ref (\p → p { progressBoundary = boundary : progressBoundary p })
                    cleanup `finally`
                        update ref (\p → p { progressBoundary = delete boundary (progressBoundary p) })
            in NodeWithCleanup loc cleanup' (map (wrap groups) children)
        Leaf item →
            Leaf item { itemExample = \params hook progress → do
                let path = groups ⧺ [itemRequirement item]
                update ref (\p → p { progressActive = progressActive p ⧺ [path] })
                itemExample item params hook progress `finally`
                    update ref (\p -> p { progressActive = delete path (progressActive p)
                                        , progressPrevious = Just path
                                        , progressCompleted = progressCompleted p + 1 }) }

-- * The record

-- | The text CI prints. Stable line prefixes, one fact per line.
renderProgress ∷ String → Progress → String
renderProgress lane p = unlines
    [ "headless-active-context v1"
    , "lane: " ⧺ lane
    , "state: " ⧺ (if progressFinished p then "finished" else "running")
    , "completed examples: " ⧺ show (progressCompleted p)
    , "active example: " ⧺ case progressActive p of
        [] → "none"
        ps → intercalate " ; " (map showPath ps)
    , "preceding completed example: " ⧺ maybe "none" showPath (progressPrevious p)
    , "fixture boundary: " ⧺ case progressBoundary p of
        [] → "none"
        bs → intercalate " ; " (map showBoundary bs)
    ]
  where
    showPath = intercalate " / "
    showBoundary b =
        boundaryKind b ⧺ " cleanup closing "
        ⧺ (if null (boundaryCovers b) then "no named group"
           else intercalate ", " (map show (boundaryCovers b)))
        ⧺ " (inside " ⧺ (if null (boundaryGroups b) then "the suite root"
                         else showPath (boundaryGroups b)) ⧺ "); no example is active"

-- * Wiring a run

-- | The environment variable that turns the recorder on: the file the
--   record is mirrored to. Unset, 'withProgressFromEnv' is the identity.
progressFileEnv ∷ String
progressFileEnv = "SYNARCHY_HEADLESS_PROGRESS_FILE"

-- | Run an action with the spec wrapper for this process, if the
--   environment asks for one. While it runs: every change is mirrored to
--   the file (written beside it and renamed, so a kill never leaves half
--   a record), and SIGINT / SIGTERM print the record to stderr before the
--   signal takes its default effect. That print serves a local interrupt:
--   a CI runner that cancels a job, or whose job timeout expires, does not
--   signal the test process at all (the demonstrations in #2747 show the
--   process simply stopped), so CI reads the FILE, from a step that runs
--   after cancellation. The record is marked finished when the action
--   ends, by return or by exception, so only a run that never got there
--   reads as running.
withProgressFromEnv ∷ String → ((Spec → Spec) → IO a) → IO a
withProgressFromEnv lane body = do
    target ← lookupEnv progressFileEnv
    case target of
        Nothing → body id
        Just path | null path → body id
        Just path → do
            warned ← newIORef False
            let sink p = do
                    r ← try (writeRecord path lane p) ∷ IO (Either SomeException ())
                    case r of
                        Right () → pure ()
                        Left e → do
                            first ← atomicModifyIORef' warned (\w → (True, not w))
                            when first $ hPutStrLn stderr
                                ("headless-active-context: cannot write "
                                 ⧺ path ⧺ ": " ⧺ show e)
            ref ← newProgressRef sink
            update ref id
            olds ← mapM (installPrinter ref lane) [sigINT, sigTERM]
            body (recordProgress ref)
                `finally` do
                    update ref (\p → p { progressFinished = True })
                    forM_ (zip [sigINT, sigTERM] olds) $ \(sig, old) →
                        installHandler sig old Nothing

writeRecord ∷ FilePath → String → Progress → IO ()
writeRecord path lane p = do
    let tmp = path ⧺ ".tmp"
    writeFile tmp (renderProgress lane p)
    renameFile tmp path

-- | What an interrupted run prints: a header naming the signal, then the
--   record.
interruptReport ∷ Signal → String → Progress → String
interruptReport sig lane p =
    "headless run interrupted by signal " ⧺ show sig ⧺ "; active context:\n"
    ⧺ renderProgress lane p

-- | Print the record on this signal, then let the signal take its
--   default effect. Returns the handler it replaced.
installPrinter ∷ ProgressRef → String → Signal → IO Handler
installPrinter ref lane sig = installHandler sig (Catch handler) Nothing
  where
    handler = do
        p ← readProgress ref
        hPutStrLn stderr (interruptReport sig lane p)
        hFlush stderr
        void (installHandler sig Default Nothing)
        raiseSignal sig
