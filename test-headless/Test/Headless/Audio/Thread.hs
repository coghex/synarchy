module Test.Headless.Audio.Thread (spec) where

import UPrelude
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, finally, evaluate)
import Data.IORef (newIORef, readIORef)
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, mkWeakPtr, deRefWeak)
import System.Timeout (timeout)
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Engine.Audio.Status
import Engine.Audio.Thread
import Engine.Audio.Transport
import Engine.Audio.Types
import Engine.Core.Capability.Audio
import Engine.Core.Capability.Core
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv, loggerRef, lifecycleRef)
import Engine.Core.Log (shutdownLogger)
import Engine.Core.Thread
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Hspec

withWorker ∷ RuntimeConfig → (EngineEnv → AudioCapability → ThreadState → IO ()) → IO ()
withWorker config action = bracket initializeEngineHeadlessQuiet
  (\(EngineInitResult env) → readIORef (loggerRef env) ⌦ shutdownLogger) $ \(EngineInitResult env) → do
    transport ← newAudioTransport config defaultVolumes
    status ← newAudioStatusRef defaultVolumes
    let capability = AudioCapability transport status
    started ← startAudioThread (toCoreCapability env) capability ForcedNull
    case started of
      Nothing → expectationFailure "audio thread was not created"
      Just thread → action env capability thread `finally` shutdownThread thread

awaitStatus ∷ AudioCapability → (AudioStatus → Bool) → IO AudioStatus
awaitStatus capability predicate = do
  let poll = do
        current ← readIORef (acStatusRef capability)
        if predicate current then pure current else threadDelay 1000 >> poll
  result ← timeout 3000000 poll
  maybe (expectationFailure "timed out waiting for audio status" >> fail "audio timeout") pure result

-- Keep the watched value out of the test's stack, and make its sequence depend
-- on IO so GHC cannot lift the key into an immortal constant.
{-# NOINLINE watchedStatus #-}
watchedStatus ∷ IO (AudioStatusRef, Weak AudioStatus, Word64)
watchedStatus = do
  seed ← getMonotonicTimeNSec
  initial ← evaluate $ (initialAudioStatus defaultVolumes) { audioSnapshotSequence = seed }
  ref ← newIORef initial
  weak ← mkWeakPtr initial Nothing
  pure (ref, weak, seed)

spec ∷ Spec
spec = describe "Audio.Thread" $ do
  forM_ [False, True] $ \disabled →
    it ("releases unread snapshot history with " <> if disabled then "disabled updates" else "fresh updates") $ do
      (ref, weak, seed) ← watchedStatus
      replicateM_ 10000 $ do
        next ← if disabled then readIORef ref else pure (initialAudioStatus defaultVolumes)
        publishAudioStatus ref next { audioLifecycle = if disabled then AudioDisabled else AudioRunningNull }
      -- Collect BEFORE reading the latest sequence: a UI/status read must not
      -- be necessary to release any of the ten thousand previous snapshots.
      performMajorGC
      isNothing <$> deRefWeak weak `shouldReturn` True
      latest ← readIORef ref
      audioSnapshotSequence latest `shouldBe` seed + 10000
      audioPublishedNs latest `shouldSatisfy` (> 0)

  it "starts the real null callback, loads shipped cues, and joins idempotently" $
    withWorker defaultRuntimeConfig $ \_ capability thread → do
      initial ← readIORef (acStatusRef capability)
      audioLifecycle initial `shouldBe` AudioRunningNull
      audioCatalogSounds initial `shouldBe` 2
      atomically (enqueuePlay (acTransport capability) "menu_selected" defaultTriggerOptions) `shouldReturn` True
      active ← awaitStatus capability $ \status → maybe False
        (\native → nsCallbacks native > 0 ∧ nsAccepted native > 0) (audioNative status)
      nsSink <$> audioNative active `shouldBe` Just 0
      shutdownThread thread
      shutdownThread thread
      audioLifecycle <$> readIORef (acStatusRef capability) `shouldReturn` AudioStopped
      atomically (enqueuePlay (acTransport capability) "menu_selected" defaultTriggerOptions) `shouldReturn` False

  it "contains startup failure and retains settings controls without stopping the engine" $
    withWorker (defaultRuntimeConfig { rcNative = defaultNativeConfig { ncSampleRate = 0 } }) $
      \env capability _ → do
        before ← readIORef (lifecycleRef env)
        audioLifecycle <$> readIORef (acStatusRef capability) `shouldReturn` AudioDisabled
        atomically $ publishAudioVolumes (acTransport capability) (Volumes 25 50 75)
        void $ awaitStatus capability ((≡ Volumes 25 50 75) ∘ audioVolumes)
        readIORef (lifecycleRef env) `shouldReturn` before
        atomically (enqueuePlay (acTransport capability) "menu_selected" defaultTriggerOptions) `shouldReturn` False

  it "joins the callback and disables producers after forced worker termination" $
    withWorker defaultRuntimeConfig $ \env capability thread → do
      before ← readIORef (lifecycleRef env)
      killThread (tsThreadId thread)
      timeout 3000000 (readMVar $ tsDone thread) `shouldReturn` Just ()
      audioLifecycle <$> readIORef (acStatusRef capability) `shouldReturn` AudioStopped
      readIORef (lifecycleRef env) `shouldReturn` before

  it "drains final controls when producers have stopped" $
    withWorker defaultRuntimeConfig $ \_ capability thread → do
      atomically $ publishAudioVolumes (acTransport capability) (Volumes 30 40 50)
      shutdownThread thread
      audioVolumes <$> readIORef (acStatusRef capability) `shouldReturn` Volumes 30 40 50
