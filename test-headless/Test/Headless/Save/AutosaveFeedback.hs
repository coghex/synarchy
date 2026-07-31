-- | #913: every TERMINAL failure of an already-accepted save reports
--   through the @save_load@ notification category, not only the storage-
--   write one.
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
module Test.Headless.Save.AutosaveFeedback (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Control.Concurrent (threadDelay)
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
    it "an accepted AUTOSAVE that fails before storage reports through \
       \save_load, naming itself an autosave" $ \env →
        expectFailureReport env "autosave_feedback_missing_page"
            (Just probeAutosaveRequest) "Autosave failed: "

    it "the same pre-storage failure for a MANUAL save reports through \
       \save_load too, with the manual wording" $ \env →
        expectFailureReport env "manual_feedback_missing_page"
            Nothing "Save failed: "
