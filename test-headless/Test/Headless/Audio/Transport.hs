module Test.Headless.Audio.Transport (spec) where

import UPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Data.List (sort)
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import Engine.Audio.Transport
import Engine.Audio.Types
import Engine.Graphics.Camera (CameraFacing(..))
import World.Page.Types (WorldPageId(..))
import System.Timeout (timeout)
import Test.Hspec

fixture ∷ Int → IO AudioTransport
fixture capacity = do
  transport ← newAudioTransport (defaultRuntimeConfig { rcEventCapacity = capacity }) defaultVolumes
  atomically $ do
    void $ readAudioBatch transport 1
    setAudioAvailable transport True
  pure transport

listener ∷ Float → ListenerSnapshot
listener x = ListenerSnapshot (WorldPageId "world") x 0 0 FaceSouth 0.5 256 0 0.25 1.2 1.6

requests ∷ AudioBatch → [AudioRequest]
requests = map stampedRequest ∘ batchRequests

spec ∷ Spec
spec = describe "Audio.Transport" $ do
  it "rejects a camera snapshot assembled before a session replacement" $ do
    transport ← newAudioTransport defaultRuntimeConfig defaultVolumes
    epoch ← atomically $ readAudioEpoch transport
    atomically $ resetAudioSession transport
    atomically (publishAudioListenerForEpoch transport epoch Nothing) `shouldReturn` False
  it "drops excess one-shots without blocking while correctness controls remain queued" $ do
    transport ← fixture 2
    timeout 100000 (atomically $ replicateM 3 $ enqueuePlay transport "bear" defaultTriggerOptions)
      `shouldReturn` Just [True, True, False]
    atomically (enqueueStopLoop transport "wind") `shouldReturn` True
    batch ← atomically $ readAudioBatch transport 10
    requests batch `shouldBe` [AudioPlay "bear" defaultTriggerOptions,
      AudioPlay "bear" defaultTriggerOptions, AudioStopLoop "wind"]
    transportQueueDrops (batchStats batch) `shouldBe` 1
    transportEventDepth (batchStats batch) `shouldBe` 0
    transportControlDepth (batchStats batch) `shouldBe` 0
    transportEventPeak (batchStats batch) `shouldBe` 2
    transportControlPeak (batchStats batch) `shouldBe` 1

  it "merges latest listener/volume slots in sequence order without leapfrogging an older event" $ do
    transport ← fixture 8
    atomically $ do
      void $ enqueuePlay transport "first" defaultTriggerOptions
      publishAudioListener transport (Just $ listener 1)
      void $ enqueuePlay transport "second" defaultTriggerOptions
      publishAudioListener transport (Just $ listener 2)
      publishAudioVolumes transport (Volumes 20 30 40)
      publishAudioVolumes transport (Volumes 50 60 70)
    a ← atomically $ readAudioBatch transport 1
    b ← atomically $ readAudioBatch transport 1
    c ← atomically $ readAudioBatch transport 2
    requests a `shouldBe` [AudioPlay "first" defaultTriggerOptions]
    requests b `shouldBe` [AudioPlay "second" defaultTriggerOptions]
    requests c `shouldBe` [AudioListener (Just $ listener 2), AudioVolumes (Volumes 50 60 70)]
    transportListenerCoalesced (batchStats c) `shouldBe` 1
    transportVolumesCoalesced (batchStats c) `shouldBe` 1
    atomically (readAudioVolumes transport) `shouldReturn` Volumes 50 60 70

  it "merges partial loop updates but preserves a stop/restart identity barrier" $ do
    transport ← fixture 8
    let position = AudioPosition (WorldPageId "world") 1 2 3
    atomically $ do
      void $ enqueueUpdateLoop transport "wind" (LoopOptions (Just position) Nothing)
      void $ enqueueUpdateLoop transport "wind" (LoopOptions Nothing $ Just (-3))
      void $ enqueueStopLoop transport "wind"
      void $ enqueueStartLoop transport "wind" "wind_sound" defaultTriggerOptions
      void $ enqueueUpdateLoop transport "wind" (LoopOptions Nothing $ Just 2)
    batch ← atomically $ readAudioBatch transport 10
    requests batch `shouldBe`
      [ AudioUpdateLoop "wind" (LoopOptions (Just position) $ Just (-3)), AudioStopLoop "wind"
      , AudioStartLoop "wind" "wind_sound" defaultTriggerOptions
      , AudioUpdateLoop "wind" (LoopOptions Nothing $ Just 2) ]
    transportLoopCoalesced (batchStats batch) `shouldBe` 1

  it "exposes replacement immediately, drops stale session requests, and preserves process volumes" $ do
    transport ← fixture 8
    atomically $ do
      void $ enqueuePlay transport "old" defaultTriggerOptions
      publishAudioVolumes transport (Volumes 25 50 75)
      setAudioPlayerPaused transport True
      publishAudioListener transport (Just $ listener 1)
      resetAudioSession transport
      void $ enqueuePlay transport "new" defaultTriggerOptions
    first ← atomically $ readAudioBatch transport 1
    batchEpoch first `shouldBe` 1
    requests first `shouldBe` []
    remaining ← atomically $ readAudioBatch transport 10
    requests remaining `shouldBe` [AudioVolumes (Volumes 25 50 75), AudioResetSession,
      AudioPlay "new" defaultTriggerOptions]
    transportStaleDrops (batchStats remaining) `shouldBe` 2

  it "rejects disabled and malformed producers while volume settings remain usable" $ do
    transport ← newAudioTransport defaultRuntimeConfig defaultVolumes
    atomically (enqueuePlay transport "bear" defaultTriggerOptions) `shouldReturn` False
    atomically (setAudioAvailable transport True)
    atomically (enqueuePlay transport "Bad id" defaultTriggerOptions) `shouldReturn` False
    atomically (enqueuePlay transport "bear" $ defaultTriggerOptions { triggerPitch = 0 / 0 })
      `shouldReturn` False
    atomically (enqueueStopLoop transport "") `shouldReturn` False
    atomically $ publishAudioVolumes transport (Volumes (-1) 50 101)
    atomically (readAudioVolumes transport) `shouldReturn` Volumes 0 50 100
    stats ← atomically $ readTransportStats transport
    transportDisabledDrops stats `shouldBe` 1
    transportInvalidDrops stats `shouldBe` 3

  it "assigns unique ordered stamps to concurrent producers" $ do
    transport ← fixture 256
    done ← newEmptyMVar
    replicateM_ 4 $ forkIO $ do
      accepted ← replicateM 25 $ atomically $ enqueuePlay transport "bear" defaultTriggerOptions
      putMVar done accepted
    accepted ← concat <$> replicateM 4 (takeMVar done)
    and accepted `shouldBe` True
    batch ← atomically $ readAudioBatch transport 256
    let stamps = map stampedSequence $ batchRequests batch
    length stamps `shouldBe` 100
    stamps `shouldBe` [1..100]
    stamps `shouldBe` sort stamps
