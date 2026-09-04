-- | The dump's fast-settle wait, and the sim-side acknowledgement wait
--   behind it (#2334).
--
--   Both used to be unbounded @takeMVar@s on completions only a normal
--   finish ever filled. A synchronous exception in the world thread's
--   'World.Command.Types.WorldApplyFluids' handler skipped the ack, the
--   sim blocked on it forever, and the dump blocked on the sim — so
--   @tools/world_audit.py@, which runs the dump through @subprocess.run@
--   with no timeout, held its CI job to the 90-minute cap and an
--   operator's shell indefinitely, with neither JSON nor a nonzero exit
--   to show for it.
--
--   Nothing here boots an engine. Both helpers take their clock and
--   their emit/exit signals as parameters precisely so the deadline and
--   worker-death paths can be driven exactly rather than provoked, and
--   the whole module runs in milliseconds.
--
--   Run:
--   @cabal test synarchy-test-headless --test-options='--match "dump fast-settle wait"'@
module Test.Headless.App.DumpSettleWait (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar
    (MVar, newEmptyMVar, newMVar, putMVar, tryReadMVar)
import Control.Exception (ErrorCall(..), throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import System.Timeout (timeout)

import App.Dump
    ( SettleWaitResult(..), SettleWatch(..), awaitFastSettle
    , classifySettleWait, settleWaitFailure )
import Engine.Core.Clock (monotonicSeconds)
import Engine.Core.Error.Exception (SystemError(..))
import Engine.Core.State (EngineLifecycle(..))
import Sim.Command.Types (FastSettleOutcome(..), FastSettleRequest(..))
import Sim.State.Types (SimWorldState, emptySimWorldState)
import Sim.Thread (completeFastSettleWith)
import World.Command.Types (FluidAckOutcome(..))
import World.Page.Types (WorldPageId(..))

spec ∷ Spec
spec = describe "dump fast-settle wait (#2334)" $ do
    simAckSpec
    classifySpec
    awaitSpec
    failureSpec

-- * The sim's per-world acknowledgement wait

pageA, pageB, pageC ∷ WorldPageId
pageA = WorldPageId "settle_wait_a"
pageB = WorldPageId "settle_wait_b"
pageC = WorldPageId "settle_wait_c"

-- | Three worlds' worth of settle input. The state itself is never read
--   by the wait — it is what the emit step would turn into a batch — so
--   the empty one is the honest value here.
threeWorlds ∷ [(WorldPageId, SimWorldState)]
threeWorlds = [ (pageA, emptySimWorldState)
              , (pageB, emptySimWorldState)
              , (pageC, emptySimWorldState) ]

-- | A clock that reads the given instants in order and then repeats the
--   last one forever. Deterministic where a real clock would make the
--   deadline arithmetic a timing assertion.
scriptedClock ∷ [Double] → IO (IO Double)
scriptedClock instants = do
    remaining ← newIORef instants
    pure $ atomicModifyIORef' remaining $ \rest → case rest of
        (t:more@(_:_)) → (more, t)
        [t]            → ([t], t)
        []             → ([], 0)

-- | An emit step that records which worlds it was asked to emit, and
--   acknowledges each with whatever the given function returns —
--   'Nothing' meaning "this world never acknowledges at all".
recordingEmit
    ∷ IORef [WorldPageId]
    → (WorldPageId → Maybe FluidAckOutcome)
    → WorldPageId → SimWorldState → MVar FluidAckOutcome → IO ()
recordingEmit emitted reply pid _ ack = do
    atomicModifyIORef' emitted $ \seen → (seen ⧺ [pid], ())
    forM_ (reply pid) (putMVar ack)

-- | The production clock the two waits are given at their real call
--   sites. Used wherever an example is proving that the wait actually
--   elapses rather than that its arithmetic is right.
realClock ∷ IO Double
realClock = monotonicSeconds

-- | Every example here is bounded, so a regression that restores an
--   unbounded wait fails the example instead of hanging the suite.
specTimeoutMicros ∷ Int
specTimeoutMicros = 3 * 1000 * 1000

bounded ∷ IO α → IO (Maybe α)
bounded = timeout specTimeoutMicros

-- | Drive the settle handler's whole tail exactly as 'Sim.Thread' does,
--   and return what it PUBLISHED to the request's completion.
--
--   Every example below reads 'fsrDone' rather than the acknowledgement
--   wait's return value, because 'fsrDone' is the only thing the dump
--   ever reads: an outcome computed correctly and then never published
--   strands the dump just as completely as one never computed.
--
--   'Nothing' means the handler either never returned inside the bound
--   or returned without publishing — both of which are the stranding
--   this issue is about.
settleVia
    ∷ (WorldPageId → SimWorldState → MVar FluidAckOutcome → IO ())
    → IO Double → Double → [(WorldPageId, SimWorldState)]
    → IO (Maybe FastSettleOutcome)
settleVia emit clock deadline worlds = do
    done ← newEmptyMVar
    let req = FastSettleRequest { fsrDone = done, fsrDeadline = deadline }
    ran ← bounded $ completeFastSettleWith emit clock discardReport req worlds
    case ran of
        Nothing → pure Nothing
        Just () → tryReadMVar done

-- | The report a settle makes when the example is not about reporting.
discardReport ∷ Text → IO ()
discardReport _ = pure ()

simAckSpec ∷ Spec
simAckSpec = describe "the settle handler's published completion" $ do

    it "publishes every world applied once each one acknowledges, having \
       \emitted all of them in order" $ do
        emitted ← newIORef []
        clock ← scriptedClock [0]
        published ← settleVia
            (recordingEmit emitted (const (Just FluidAckApplied)))
            clock 1000 threeWorlds
        published `shouldBe` Just FastSettleApplied
        readIORef emitted `shouldReturn` [pageA, pageB, pageC]

    it "publishes a failed acknowledgement as the settle's own outcome, \
       \naming the world that failed and carrying its cause" $ do
        emitted ← newIORef []
        clock ← scriptedClock [0]
        let reply pid | pid ≡ pageB = Just (FluidAckFailed "handler blew up")
                      | otherwise   = Just FluidAckApplied
        published ← settleVia (recordingEmit emitted reply) clock 1000
                              threeWorlds
        published `shouldBe`
            Just (FastSettleWorldFailed pageB "handler blew up")
        -- Stopped at the failure: the third world's batch was never
        -- emitted, because the settle has already failed and nothing
        -- downstream may read the tiles it would have produced.
        readIORef emitted `shouldReturn` [pageA, pageB]

    it "publishes the deadline outcome for an acknowledgement that never \
       \arrives, naming the world it was waiting on, within the \
       \deadline it was given" $ do
        emitted ← newIORef []
        -- A real clock, and a deadline a fraction of a second out: this
        -- is the wait actually elapsing, not the arithmetic being
        -- scripted around it.
        now ← realClock
        published ← settleVia (recordingEmit emitted (const Nothing))
                              realClock (now + 0.2)
                              [(pageA, emptySimWorldState)]
        published `shouldBe` Just (FastSettleAckDeadline pageA)

    it "spends ONE budget across the worlds instead of restarting it for \
       \each, so N worlds cannot multiply the caller's total wait" $ do
        emitted ← newIORef []
        -- The clock jumps past the deadline between the two worlds. A
        -- wait that re-derived a fresh 30-second budget per world would
        -- still be blocked on the second one when this example's own
        -- 3-second bound expires; a wait spending the SHARED absolute
        -- deadline has nothing left and says so at once.
        clock ← scriptedClock [0, 100]
        let reply pid | pid ≡ pageA = Just FluidAckApplied
                      | otherwise   = Nothing
        published ← settleVia (recordingEmit emitted reply) clock 30
            [(pageA, emptySimWorldState), (pageB, emptySimWorldState)]
        published `shouldBe` Just (FastSettleAckDeadline pageB)
        readIORef emitted `shouldReturn` [pageA, pageB]

    it "treats an already-expired deadline as no budget at all rather \
       \than as no deadline at all" $ do
        emitted ← newIORef []
        -- The floor matters because 'System.Timeout.timeout' reads a
        -- NEGATIVE argument as "wait forever": dropping it would turn
        -- the expired case back into the unbounded wait this replaces.
        clock ← scriptedClock [100]
        published ← settleVia (recordingEmit emitted (const Nothing))
                              clock 30 [(pageA, emptySimWorldState)]
        published `shouldBe` Just (FastSettleAckDeadline pageA)

    it "reports the outcome BEFORE publishing it, so a failing report \
       \leaves the completion empty rather than handing the caller a \
       \success it is about to die behind" $ do
        emitted ← newIORef []
        clock ← scriptedClock [0]
        done ← newEmptyMVar
        let req = FastSettleRequest { fsrDone = done, fsrDeadline = 1000 }
            failingReport _ = throwIO (ErrorCall "reporting blew up")
        raised ← bounded ∘ try $ completeFastSettleWith
            (recordingEmit emitted (const (Just FluidAckApplied)))
            clock failingReport req threeWorlds

        -- The report is reached, and its failure still leaves the
        -- handler so the sim worker fail-stops on it.
        case raised of
            Just (Left (ErrorCall msg)) →
                msg `shouldBe` "reporting blew up"
            Just (Right ()) → expectationFailure
                "the failing report did not reach the caller"
            Nothing → expectationFailure
                "the settle handler never returned"

        -- …and nothing was published, because the publication comes
        -- after every fallible step and this one did not survive.
        tryReadMVar done `shouldReturn` Nothing

-- * The dump's decision

classifySpec ∷ Spec
classifySpec = describe "the settle wait's decision" $ do

    it "keeps waiting while nothing at all is observable" $
        classifySettleWait Nothing False False False False
            `shouldBe` Nothing

    it "settles on a published success when nothing else is showing" $
        classifySettleWait (Just FastSettleApplied) False False False False
            `shouldBe` Just SettleSettled

    it "counts a success published in the final poll window rather than \
       \timing out on it" $
        classifySettleWait (Just FastSettleApplied) False False False True
            `shouldBe` Just SettleSettled

    it "lets a worker exit or a cleanup override a published success, so \
       \a concurrent failure cannot produce partial-success output" $ do
        classifySettleWait (Just FastSettleApplied) True False False False
            `shouldBe` Just SettleWorldExited
        classifySettleWait (Just FastSettleApplied) False True False False
            `shouldBe` Just SettleSimExited
        classifySettleWait (Just FastSettleApplied) False False True False
            `shouldBe` Just SettleCleaningUp

    it "reports a published FAILURE ahead of everything else, because it \
       \is the only observation that names the world" $
        classifySettleWait (Just (FastSettleAckDeadline pageA))
                           True True True True
            `shouldBe` Just (SettleReported (FastSettleAckDeadline pageA))

    it "attributes an unpublished settle to whichever worker exited" $ do
        classifySettleWait Nothing True False False False
            `shouldBe` Just SettleWorldExited
        classifySettleWait Nothing False True False False
            `shouldBe` Just SettleSimExited

    it "blames the world when both workers exited, the world being the \
       \cause the sim's own strand is a symptom of" $
        classifySettleWait Nothing True True False False
            `shouldBe` Just SettleWorldExited

    it "falls back to a generic cleanup when the lifecycle is all there \
       \is, since it carries no cause to attribute" $
        classifySettleWait Nothing False False True False
            `shouldBe` Just SettleCleaningUp

    it "times out only when nothing else is observable" $
        classifySettleWait Nothing False False False True
            `shouldBe` Just SettleTimedOut

-- * The dump's wait

-- | A watch on two workers that are both still running.
liveWatch ∷ IO SettleWatch
liveWatch = do
    sim ← newEmptyMVar
    world ← newEmptyMVar
    pure SettleWatch { swSim = Just sim, swWorld = Just world }

-- | A watch whose named worker has already exited — its @tsDone@ filled,
--   exactly as the fork finalizer leaves it on any exit.
exitedWatch ∷ Bool → IO SettleWatch
exitedWatch simExited = do
    running ← newEmptyMVar
    done ← newMVar ()
    pure $ if simExited
        then SettleWatch { swSim = Just done, swWorld = Just running }
        else SettleWatch { swSim = Just running, swWorld = Just done }

-- | Drive the real wait with a real clock, a deadline far enough out
--   that only the example's own signal can end it.
waitWith
    ∷ SettleWatch → EngineLifecycle → Maybe FastSettleOutcome
    → IO (Maybe SettleWaitResult)
waitWith watch lifecycle mOutcome = do
    lifecycleRef ← newIORef lifecycle
    done ← newEmptyMVar
    forM_ mOutcome (putMVar done)
    now ← realClock
    bounded $ awaitFastSettle realClock lifecycleRef watch done (now + 60)

awaitSpec ∷ Spec
awaitSpec = describe "the dump's bounded settle wait" $ do

    it "ends on the sim worker's exit, with the completion still empty" $ do
        watch ← exitedWatch True
        waitWith watch EngineRunning Nothing
            `shouldReturn` Just SettleSimExited

    it "ends on the world worker's exit, with the completion still empty" $ do
        watch ← exitedWatch False
        waitWith watch EngineRunning Nothing
            `shouldReturn` Just SettleWorldExited

    it "ends on a lifecycle cleanup no worker exit explains" $ do
        watch ← liveWatch
        waitWith watch CleaningUp Nothing
            `shouldReturn` Just SettleCleaningUp

    it "ends at the deadline when the settle simply never completes" $ do
        watch ← liveWatch
        lifecycleRef ← newIORef EngineRunning
        done ← newEmptyMVar
        now ← realClock
        result ← bounded $
            awaitFastSettle realClock lifecycleRef watch done (now + 0.2)
        result `shouldBe` Just SettleTimedOut

    it "ends on the published outcome while both workers are alive" $ do
        watch ← liveWatch
        waitWith watch EngineRunning (Just FastSettleApplied)
            `shouldReturn` Just SettleSettled
        watch' ← liveWatch
        waitWith watch' EngineRunning
                 (Just (FastSettleWorldFailed pageA "boom"))
            `shouldReturn`
            Just (SettleReported (FastSettleWorldFailed pageA "boom"))

    it "cannot mistake a worker a mode never started for one that exited" $ do
        -- Dump starts both, but the wait is written against the record
        -- 'Engine.Core.Workers.EngineWorkers' actually holds, whose
        -- slots are 'Maybe'. An absent slot must read as "still
        -- running", never as an exit.
        let noWorkers = SettleWatch { swSim = Nothing, swWorld = Nothing }
        lifecycleRef ← newIORef EngineRunning
        done ← newEmptyMVar
        now ← realClock
        result ← bounded $
            awaitFastSettle realClock lifecycleRef noWorkers done (now + 0.2)
        result `shouldBe` Just SettleTimedOut

-- * The failure the dump aborts with

-- | Every constructor of 'SettleWaitResult', which the exhaustiveness
--   claim below is only as good as.
allResults ∷ [SettleWaitResult]
allResults =
    [ SettleSettled
    , SettleReported (FastSettleAckDeadline pageA)
    , SettleReported (FastSettleWorldFailed pageA "boom")
    , SettleWorldExited
    , SettleSimExited
    , SettleCleaningUp
    , SettleTimedOut
    ]

-- | Names each result, matched EXHAUSTIVELY. This is the ratchet on
--   'allResults': a new constructor of 'SettleWaitResult' fails to
--   compile here (@-Wincomplete-patterns@ is @-Werror@ in this package)
--   until it is named, and then fails the first example below until
--   'allResults' lists it too. Without it, a constructor could be added
--   with no verdict and the list would still look complete.
resultName ∷ SettleWaitResult → String
resultName result = case result of
    SettleSettled     → "SettleSettled"
    SettleReported _  → "SettleReported"
    SettleWorldExited → "SettleWorldExited"
    SettleSimExited   → "SettleSimExited"
    SettleCleaningUp  → "SettleCleaningUp"
    SettleTimedOut    → "SettleTimedOut"

failureSpec ∷ Spec
failureSpec = describe "the failure the dump aborts with" $ do

    it "covers every constructor a settle wait can return" $
        sort (nub (map resultName allResults)) `shouldBe`
            sort [ "SettleSettled", "SettleReported", "SettleWorldExited"
                 , "SettleSimExited", "SettleCleaningUp", "SettleTimedOut" ]

    it "clears exactly one result and fails every other, which is what \
       \keeps a failed settle from reaching the JSON" $ do
        -- 'runDump' emits its JSON only past a 'Nothing' here; every
        -- 'Just' becomes an 'EngineException' that leaves the engine
        -- action, and @handleBootResult FatalToStderr@ turns that into a
        -- nonzero exit with stdout untouched.
        let cleared = [ r | r ← allResults, isNothing (settleWaitFailure r) ]
        cleared `shouldBe` [SettleSettled]

    it "reports the deadline as a timeout and every worker failure as an \
       \IO error, so the two read differently in the operator's log" $ do
        settleWaitFailure SettleTimedOut `shouldBe`
            Just (TimeoutError
                "dump: sim fast settle did not complete in time")
        settleWaitFailure SettleWorldExited `shouldBe`
            Just (IOError
                "dump: world thread exited before the sim fast settle \
                \completed")
        settleWaitFailure SettleSimExited `shouldBe`
            Just (IOError
                "dump: sim thread exited before the sim fast settle \
                \completed")
        settleWaitFailure SettleCleaningUp `shouldBe`
            Just (IOError
                "dump: engine began shutting down before the sim fast \
                \settle completed")

    it "carries the sim's own outcome through, so the operator learns \
       \which world stalled" $
        settleWaitFailure (SettleReported (FastSettleAckDeadline pageC))
            `shouldBe` Just (IOError
                ("dump: sim fast settle failed: "
                 <> tshow (FastSettleAckDeadline pageC)))
