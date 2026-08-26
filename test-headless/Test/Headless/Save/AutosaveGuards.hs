-- | #913: the two engine-side guarantees an autosave rests on that no
--   end-to-end probe can pin down deterministically — that every
--   terminal failure of an accepted save is REPORTED, and that the
--   player-intent lock genuinely serializes a player's pause against the
--   autosave's conditional restore.
--
--   == Failure feedback
--
--   Every TERMINAL failure of an already-accepted save reports through
--   the @save_load@ notification category, not only the storage-write
--   one.
--
--   Acceptance has already paused the game by the time
--   'World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand' runs,
--   and an autosave deliberately LEAVES it paused on failure (the safety
--   ratchet). A branch that only logged would therefore strand the player
--   paused with no explanation and no save — for an autosave, one they
--   never asked for at that moment and would have no reason to be
--   looking for.
--
--   These cases drive the earliest such branch (the requested page is
--   gone by the time the world thread processes the command) through the
--   REAL command handler on a real engine, in both request flavours, and
--   assert the emitted event. The storage-write branch — the one that
--   already reported before this issue — is covered end to end by
--   @tools\/autosave_probe.py@, which induces a genuine
--   @PhaseDirectoryCreate@ failure and asserts both the event and the
--   paused, zero-scaled ratchet it leaves behind.
--
--   Deliberately does NOT go through @engine.saveWorld@\/'beginSave':
--   what is under test is the world thread's own failure REPORTING, and
--   queueing the command directly is the only way to reach a
--   pre-storage branch on demand.
--
--   == Player-intent lock
--
--   The second guarantee — that a player's pause is serialized against
--   an autosave's conditional restore — is a RACE, so the example that
--   covers it has to establish the interleaving it is asserting about
--   rather than wait for it and hope. #1372: it used to fork the
--   competing restore, sleep 50 ms, and assume the restore had reached
--   the lock by then; a restore that was still unscheduled at the end
--   of that window sailed past every assertion, so the detector failed
--   OPEN and could have gone silently vacuous under CI load.
--
--   The proof is now taken from the RTS's own view of that thread —
--   'awaitLockBoundary' below — which distinguishes \"parked on the
--   lock\" from \"already finished\" as a fact about the thread rather
--   than an inference from elapsed time.
module Test.Headless.Save.AutosaveGuards (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Control.Concurrent (ThreadId, forkIO, threadDelay, yield)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Conc (BlockReason(..), ThreadStatus(..), threadStatus)
import System.Timeout (timeout)
import Engine.Core.Capability.WorldSim
    ( toWorldSimCapability, withPlayerIntent, withPlayerIntentHeld
    , restoreIfPlayerIdle )
import Control.Concurrent.STM (readTVarIO)
import Data.Foldable (toList)
import Engine.Core.State (EngineEnv(..))
import Engine.PlayerEvent (PlayerEvent(..), StoredEvent(..), EventStore(..))
import Test.Headless.Harness (sendWorldCommand)
import World.Page.Types (WorldPageId(..))
import World.Save.Types (AutosaveRequest(..))
import World.Types (WorldCommand(..))

-- | An autosave request whose exact values are irrelevant here: this
--   spec is about the FAILURE report, and a failed transaction never
--   reaches the restoration those values feed.
probeAutosaveRequest ∷ AutosaveRequest
probeAutosaveRequest = AutosaveRequest
    { arPrePaused    = False
    , arPreTimeScale = 1
    , arPausedPage   = Nothing
    , arIntentGen    = 0
    }

saveLoadTexts ∷ EngineEnv → IO [Text]
saveLoadTexts env = do
    events ← esRows <$> readTVarIO (eventStoreRef env)
    pure [ peText e | row ← toList events, let e = seEvent row
                    , peCategory e ≡ "save_load" ]

-- | Queue a save for a page that does not exist and wait for the
--   world thread's report to land in the event log. The page id is
--   unique per case so the @save_load@ category's log coalescing (which
--   folds IDENTICAL consecutive entries) can never make one case's event
--   satisfy another's assertion.
expectFailureReport
    ∷ EngineEnv → Text → Maybe AutosaveRequest → Text → IO ()
expectFailureReport env page mAutosave expectedPrefix = do
    before ← saveLoadTexts env
    sendWorldCommand env
        (WorldSave (WorldPageId page) "unreachable_slot"
                   "2026-07-31T00:00:00.000000Z" [] [] mAutosave)
    landed ← waitForNewEvent 200 (length before)
    landed `shouldBe` True
    texts ← saveLoadTexts env
    let newest = last texts
    (expectedPrefix `T.isPrefixOf` newest) `shouldBe` True
    (page `T.isInfixOf` newest) `shouldBe` True
  where
    -- The command is QUEUED, so there is nothing synchronous to await:
    -- poll (50 ms x attempts) until the world thread's report lands.
    waitForNewEvent ∷ Int → Int → IO Bool
    waitForNewEvent 0 _ = pure False
    waitForNewEvent n baseline = do
        texts ← saveLoadTexts env
        if length texts > baseline
          then pure True
          else threadDelay 50000 >> waitForNewEvent (n - 1) baseline

-- | What the racing restore was doing while the holder still had the
--   player-intent lock. #1372's replacement for a @threadDelay@: the
--   two interesting values are facts the RTS reports about a thread,
--   not conclusions drawn from how long the example waited.
data BoundaryObservation
  = BlockedAtLock
    -- ^ Parked on an MVar, which for this thread can only be the
    --   player-intent generation itself: its one other MVar
    --   (@restored@) is empty when it writes to it, so that 'putMVar'
    --   cannot block, and it touches no others. Reaching this state is
    --   what proves the restore got all the way to
    --   'restoreIfPlayerIdle''s @modifyMVar@ and was turned away by the
    --   mutual exclusion.
  | RanWithoutBlocking
    -- ^ Ran to completion while the lock was held — precisely what
    --   removing that mutual exclusion produces, and on every schedule
    --   rather than a lucky one.
  | DiedUnobserved
    -- ^ Threw before reaching either outcome. A broken example, not a
    --   verdict about the lock.
  | NeverSettled
    -- ^ The watchdog expired first. See 'boundaryWatchdogUs': time can
    --   only FAIL this example, never supply the proof.
  deriving (Eq, Show)

-- | Watch a forked thread until the RTS says which of those happened.
--
--   This is the whole test seam #1372 asks for, and it deliberately
--   lives here rather than in "Engine.Core.Capability.WorldSim": the
--   observation is made from OUTSIDE, against a 'ThreadId', so the
--   capability gains no test-only parameter, no test-only branch, and
--   no production caller ever sees it. 'withPlayerIntent',
--   'withPlayerIntentHeld' and 'restoreIfPlayerIdle' are used here
--   exactly as the engine uses them.
--
--   Why a status poll and not a handshake: a 'putMVar' from the racing
--   thread just before it calls 'restoreIfPlayerIdle' would prove only
--   that it reached the SIGNAL. It can still be descheduled between
--   there and @modifyMVar@, which leaves exactly the ambiguity this
--   example exists to remove. 'yield' between polls hands the
--   capability to that thread and re-reads its real status, so no step
--   here depends on how long anything takes.
awaitLockBoundary ∷ ThreadId → IO BoundaryObservation
awaitLockBoundary tid =
    fromMaybe NeverSettled ⊚ timeout boundaryWatchdogUs poll
  where
    poll = do
        status ← threadStatus tid
        case status of
          ThreadBlocked BlockedOnMVar → pure BlockedAtLock
          ThreadFinished              → pure RanWithoutBlocking
          ThreadDied                  → pure DiedUnobserved
          _                           → yield ≫ poll

-- | Collect a forked thread's own reported outcome, bounded so a wedged
--   example fails instead of hanging the suite. 'Nothing' is always a
--   failure of the example, never a passing outcome.
awaitSignal ∷ MVar α → IO (Maybe α)
awaitSignal = timeout boundaryWatchdogUs ∘ takeMVar

-- | Generous on purpose. Every bound in this module is a HANG guard
--   whose only power is to fail the example; none of them establishes
--   the ordering under test, so making one tight would buy nothing and
--   could expire under load on a perfectly correct run.
boundaryWatchdogUs ∷ Int
boundaryWatchdogUs = 10 * 1000 * 1000

spec ∷ SpecWith EngineEnv
spec = do
    describe "failure feedback" $ do
        it "an accepted AUTOSAVE that fails before storage reports through \
           \save_load, naming itself an autosave" $ \env →
            expectFailureReport env "autosave_feedback_missing_page"
                (Just probeAutosaveRequest) "Autosave failed: "

        it "the same pre-storage failure for a MANUAL save reports through \
           \save_load too, with the manual wording" $ \env →
            expectFailureReport env "manual_feedback_missing_page"
                Nothing "Save failed: "

    -- The window this closes: the world thread reads a matching
    -- generation, the Lua thread THEN applies a pause and bumps, and the
    -- world thread goes on to overwrite it with a value that was already
    -- stale when it was read. Comparing a generation is not enough on
    -- its own -- the comparison and the write have to be one critical
    -- section, which is why the generation is an MVar and not an IORef.
    describe "player-intent lock (the restore/pause race)" $ do
        it "a restore cannot run while a player transition holds the \
           \lock, and does not overwrite it once it lands" $ \env → do
            let wsc = toWorldSimCapability env
            -- A stand-in for enginePausedRef: writing the REAL one from
            -- a spec would disturb every other engine-backed spec
            -- sharing this process. What is under test is the lock, not
            -- which ref it happens to protect.
            witness ← newIORef ("initial" ∷ Text)
            entered ← newEmptyMVar
            release ← newEmptyMVar
            restored ← newEmptyMVar
            holderDone ← newEmptyMVar

            -- The generation the racing restore below has to match,
            -- READ through the capability's own read-only verb instead
            -- of hard-coded. #1372 requirement 6: what stood here was
            --
            --     baseline ← withPlayerIntent wsc (pure ())
            --         `seq` restoreIfPlayerIdle wsc maxBound (pure ())
            --
            -- in which `seq` forced an @IO ()@ to WHNF without ever
            -- running it. That no-op was load-bearing — the restore
            -- below hard-coded @expected = 0@, so the example was only
            -- non-vacuous as long as nothing, here or anywhere else in
            -- this module, bumped the generation first. Reading the
            -- live value retires that trap instead of documenting it:
            -- there is no longer a line whose deadness matters.
            gen0 ← withPlayerIntentHeld wsc pure
            -- Sanity, about the COMPARISON rather than the race: a
            -- generation that is not the live one never restores.
            -- @gen0 + 1@ differs from @gen0@ for every 'Word64',
            -- overflow included.
            baseline ← restoreIfPlayerIdle wsc (gen0 + 1) (pure ())
            baseline `shouldBe` False

            -- Hold the lock inside a player transition...
            _ ← forkIO $ do
                    withPlayerIntent wsc $ do
                        putMVar entered ()
                        takeMVar release
                        writeIORef witness "player"
                    putMVar holderDone ()
            takeMVar entered
            -- ...and race a restore against it. It must BLOCK here
            -- rather than observe the pre-transition generation.
            racer ← forkIO $ do
                    ok ← restoreIfPlayerIdle wsc gen0
                                             (writeIORef witness "autosave")
                    putMVar restored ok

            -- Gather every observation, THEN release, THEN assert.
            -- Raising an expectation before the release would strand
            -- the holder on @release@ and the racer on the lock the
            -- moment one failed — which is exactly the run #1372's
            -- deliberately-broken-lock demonstration has to survive.
            -- Nothing between the fork above and the release below can
            -- throw, so this ordering is the exception safety; no
            -- bracket is needed.
            boundary ← awaitLockBoundary racer
            duringHold ← readIORef witness
            putMVar release ()
            holderFinished ← awaitSignal holderDone
            ranRestore ← awaitSignal restored
            final ← readIORef witness

            -- The two that distinguish the interleavings: with the
            -- mutual exclusion removed from 'restoreIfPlayerIdle' the
            -- racer never blocks, so it reports 'RanWithoutBlocking'
            -- and has already written the witness by now.
            boundary `shouldBe` BlockedAtLock
            duringHold `shouldBe` "initial"
            -- And the two the guarantee is actually about: once the
            -- transition lands, the restore that was waiting on it
            -- finds the generation advanced and declines, leaving the
            -- player's write standing.
            ranRestore `shouldBe` Just False
            final `shouldBe` "player"
            holderFinished `shouldBe` Just ()   -- nothing left wedged
