{-# LANGUAGE Strict #-}
-- | Shared exclusion for "Scenario.Load" (#2713, D-48): a scenario load,
--   an ordinary save, an autosave, an ordinary load and a capture admit at
--   most one at a time; a conflicting request is answered immediately and
--   changes nothing about the operation that holds the line.
--
--   Every example holds a request deterministically at a named seam — never
--   a sleep — so the competing requests are made against a request that is
--   genuinely in flight.
module Test.Headless.Scenario.Load.Exclusion (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Set as Set
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.InputView (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Engine.Save.Barrier
import Scenario.Load.Service (requestScenarioLoad)
import Scenario.Load.Types
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))

spec ∷ SpecWith H
spec = describe "shared exclusion" $ do

    it "rejects every conflicting operation at once while a request is held, changing nothing about it" $ \h → do
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
            barrier = slSaveBarrierRef (toSaveLoadCapability env)
        hold ← newHold
        armSeams h (holdAt PointPreflight hold)
        src ← SourceFile <$> writeScenario "held.yaml" (doc ["units: []"])
        n ← request h src
        pump ← startPump h
        takeMVar (holdReached hold)
        -- the request is accepted, paused, and not finished
        activeBefore ← statusOf h
        genericBefore ← readLoadStatus lsr
        saveBefore ← readSaveStatus barrier
        pausedBefore ← readIORef (enginePausedRef env)
        queuedBefore ← Q.queueStats (ivLuaQueue (toInputViewCapability env))
        pausedBefore `shouldBe` True
        fmap slsStage activeBefore `shouldBe` Just StagePreflight
        -- 1. another scenario load
        requestScenarioLoad env src `shouldReturn` Left BusyLoad
        -- 2. an ordinary load
        lua h "return tostring(engine.loadSave('anything'))" `shouldReturn` "false"
        -- 3. an ordinary save, and 4. an autosave through the same admission
        lua h "return tostring(engine.saveWorld('test_arena', 'manual-x'))" `shouldReturn` "false"
        lua h "return tostring(engine.saveWorld('test_arena', 'autosave_incoming', {autosave = true}))"
            `shouldReturn` "false"
        -- 5. a capture (it claims through the same boundary a save does)
        claimSave lsr (beginSave barrier (Set.fromList [SaveLua])) `shouldReturn` Left BusyLoad
        -- 6. an unpause
        lua h "return tostring(engine.setPaused(false))" `shouldReturn` "false"
        readIORef (enginePausedRef env) `shouldReturn` True
        -- nothing about the active request moved
        statusOf h `shouldReturn` activeBefore
        readLoadStatus lsr `shouldReturn` genericBefore
        readSaveStatus barrier `shouldReturn` saveBefore
        -- no retry was queued behind it
        queuedAfter ← Q.queueStats (ivLuaQueue (toInputViewCapability env))
        Q.qsEnqueued queuedAfter `shouldBe` Q.qsEnqueued queuedBefore
        -- release: the SAME request finishes, with the identity it was given
        putMVar (holdRelease hold) ()
        s ← await h n 60
        stopPump pump
        disarm h
        slsRequestId s `shouldBe` n
        slsOutcome s `shouldBe` Just ScenarioReady
        Just final ← readLoadStatus lsr
        lsRequestId final `shouldBe` n

    it "pauses synchronously at acceptance, before any decoding" $ \h → do
        let env = fxEnv (hFx h)
        writeIORef (enginePausedRef env) False
        src ← SourceFile <$> writeScenario "pause-at-acceptance.yaml" (doc ["units: []"])
        n ← request h src
        -- nothing has been pumped: the request is accepted and not started
        readIORef (enginePausedRef env) `shouldReturn` True
        s ← pumpUntilTerminal h n 60
        slsOutcome s `shouldBe` Just ScenarioReady
        readIORef (enginePausedRef env) `shouldReturn` True

    it "rejects a scenario load while a save holds the barrier, and leaves the save alone" $ \h → do
        let env = fxEnv (hFx h)
            barrier = slSaveBarrierRef (toSaveLoadCapability env)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
        src ← SourceFile <$> writeScenario "during-save.yaml" (doc ["units: []"])
        Right sid ← beginSave barrier (Set.fromList [SaveLua])
        saveBefore ← readSaveStatus barrier
        genericBefore ← readLoadStatus lsr
        requestScenarioLoad env src `shouldReturn` Left BusySave
        readSaveStatus barrier `shouldReturn` saveBefore
        readLoadStatus lsr `shouldReturn` genericBefore
        finishSave barrier sid
        -- once the save ends the same request is admitted
        s ← runLoad h src
        slsOutcome s `shouldBe` Just ScenarioReady

    it "keeps a finished request's result until the next accepted operation replaces it" $ \h → do
        src ← SourceFile <$> writeScenario "kept.yaml" (doc ["units: []"])
        s1 ← runLoad h src
        statusOf h `shouldReturn` Just s1
        -- a rejected request is not an operation
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
        Right sid ← beginSave (slSaveBarrierRef (toSaveLoadCapability env)) (Set.fromList [SaveLua])
        _ ← requestScenarioLoad env src
        statusOf h `shouldReturn` Just s1
        finishSave (slSaveBarrierRef (toSaveLoadCapability env)) sid
        s2 ← runLoad h src
        slsRequestId s2 `shouldBe` slsRequestId s1 + 1
        readScenarioStatus lsr `shouldReturn` Just s2

    it "makes every claimant take the admission lock, so a check and its claim cannot be separated" $ \h → do
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
            barrier = slSaveBarrierRef (toSaveLoadCapability env)
            claimants =
                [ ("claimLoad", void (claimLoad lsr (saveInProgress barrier) "locked"))
                , ("claimScenarioLoad", void (claimScenarioLoad lsr (saveInProgress barrier) "locked"))
                , ("claimSave", void (claimSave lsr (pure ()))) ]
        forM_ claimants $ \(label, claim) → do
            done ← newEmptyMVar
            -- the lock is held by this thread; the claimant must wait for it
            withAdmission lsr $ do
                _ ← forkIO (claim >> putMVar done ())
                threadDelay 100000
                finished ← tryReadMVar done
                (label, isJust finished) `shouldBe` (label, False)
            takeMVar done
            -- clear the line the claimant took
            cur ← readLoadStatus lsr
            forM_ cur $ \l → when (lsOutcome l ≡ Nothing) (finishLoad lsr (lsRequestId l))

    it "admits exactly one of any set of simultaneous claimants (scenario load, capture, ordinary load)" $ \h → do
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
            barrier = slSaveBarrierRef (toSaveLoadCapability env)
            rounds = 300 ∷ Int
            claimants = 6 ∷ Int
        winnersPerRound ← forM [1 .. rounds] $ \_ → do
            gate ← newEmptyMVar
            results ← replicateM claimants newEmptyMVar
            forM_ (zip [0 ∷ Int ..] results) $ \(k, out) → forkIO $ do
                readMVar gate
                r ← case k `mod` 3 of
                    0 → either (const False) (const True)
                            <$> claimScenarioLoad lsr (saveInProgress barrier) "race"
                    1 → either (const False) (either (const False) (const True))
                            <$> claimSave lsr (beginSave barrier (Set.fromList [SaveLua]))
                    _ → either (const False) (const True)
                            <$> claimLoad lsr (saveInProgress barrier) "race"
                putMVar out r
            putMVar gate ()
            outcomes ← mapM readMVar results
            -- clear the line for the next round, whichever kind won
            cur ← readLoadStatus lsr
            forM_ cur $ \l → when (lsOutcome l ≡ Nothing) (finishLoad lsr (lsRequestId l))
            sv ← readSaveStatus barrier
            forM_ sv $ \s → when (ssOutcome s ≡ Nothing) (finishSave barrier (ssRequestId s))
            pure (length (filter id outcomes))
        winnersPerRound `shouldSatisfy` all (≡ 1)
        _ ← atomicModifyIORef' (enginePausedRef env) (\p → (p, ()))
        pure ()
