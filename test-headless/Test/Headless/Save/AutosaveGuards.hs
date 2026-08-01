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
module Test.Headless.Save.AutosaveGuards (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Engine.Core.Capability.WorldSim
    (toWorldSimCapability, withPlayerIntent, restoreIfPlayerIdle)
import Control.Concurrent.STM (readTVarIO)
import Data.Foldable (toList)
import Engine.Core.State (EngineEnv(..))
import Engine.PlayerEvent (PlayerEvent(..))
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
    , arIntentGen    = 0
    }

saveLoadTexts ∷ EngineEnv → IO [Text]
saveLoadTexts env = do
    events ← readTVarIO (eventStoreRef env)
    pure [ peText e | e ← toList events, peCategory e ≡ "save_load" ]

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

            baseline ← withPlayerIntent wsc (pure ())
                `seq` restoreIfPlayerIdle wsc maxBound (pure ())
            baseline `shouldBe` False   -- sanity: a stale generation never restores

            -- Hold the lock inside a player transition...
            _ ← forkIO $ withPlayerIntent wsc $ do
                    putMVar entered ()
                    takeMVar release
                    writeIORef witness "player"
            takeMVar entered
            -- ...and race a restore against it. It must BLOCK here
            -- rather than observe the pre-transition generation.
            _ ← forkIO $ do
                    ok ← restoreIfPlayerIdle wsc 0 (writeIORef witness "autosave")
                    putMVar restored ok
            threadDelay 50000
            duringHold ← readIORef witness
            duringHold `shouldBe` "initial"

            putMVar release ()
            ranRestore ← takeMVar restored
            ranRestore `shouldBe` False
            final ← readIORef witness
            final `shouldBe` "player"
