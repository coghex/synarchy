module Test.Headless.Audio.Preview (spec) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, finally)
import Data.Either (isLeft)
import Data.IORef (readIORef)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing,
  createFileLink)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Engine.Audio.Catalog.Resolve (loadCatalog)
import Engine.Audio.Catalog.Upload
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Engine.Audio.Preview.Catalog
import Engine.Audio.Preview.Discovery
import Engine.Audio.Preview.Types
import Engine.Audio.Status
import Engine.Audio.Thread
import Engine.Audio.Transport
import Engine.Core.Capability.Audio
import Engine.Core.Capability.Core
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log (shutdownLogger)
import Engine.Core.State (loggerRef)
import Engine.Core.Thread (shutdownThread)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Hspec

withCore ∷ (Native → IO α) → IO α
withCore action = withNative defaultNativeConfig ForcedNull action
  ⌦ either (fail ∘ Text.unpack) pure

required ∷ Either Text α → IO α
required = either (fail ∘ Text.unpack) pure

-- Only private files are edited; production catalogs are read from the checkout.
withFile ∷ (FilePath → FilePath → IO ()) → IO ()
withFile action = do
  source ← canonicalizePath "test-headless/data/audio/tone.wav"
  withExclusiveTempDirectory "synarchy-audio-preview" $ \root → do
    let file = root </> "bear audition 雪.wav"
    copyFile source file
    action root file

awaitStatus ∷ AudioCapability → (AudioStatus → Bool) → IO AudioStatus
awaitStatus capability predicate = do
  let poll = do
        current ← readIORef (acStatusRef capability)
        if predicate current then pure current else threadDelay 1000 >> poll
  result ← timeout 5000000 poll
  maybe (fail "timed out waiting for preview audio status") pure result

spec ∷ Spec
spec = describe "Audio.Preview" $ do
  it "resolves an explicit external file from the caller directory, preserving spaces and Unicode" $
    withFile $ \root file → do
      canonical ← canonicalizePath file
      resolvePreviewFile root "bear audition 雪.wav" `shouldReturn` Right canonical
      resolvePreviewFile "/a/different/resource/root" file `shouldReturn` Right canonical
      checkedSamplePath file ≫= (`shouldSatisfy` isLeft)
      resolvePreviewFile root "missing.wav" ≫= (`shouldSatisfy` isLeft)
      resolvePreviewFile root "image.png" ≫= (`shouldSatisfy` isLeft)
      createDirectoryIfMissing True (root </> "directory.wav")
      resolvePreviewFile root "directory.wav" ≫= (`shouldSatisfy` isLeft)

  it "discovers local audio in stable order without following child links or hidden files" $
    withFile $ \root file → do
      let assets = root </> "assets/audio"
      createDirectoryIfMissing True (assets </> "nested")
      copyFile file (assets </> "b.WAV")
      copyFile file (assets </> "nested/a.wav")
      copyFile file (assets </> ".hidden.wav")
      createFileLink file (assets </> "escape.wav")
      discoverAudioFiles assets `shouldReturn` [assets </> "b.WAV", assets </> "nested/a.wav"]

  it "renders both shipped synth cues identically to their first game playback" $ do
    forM_ ["menu_back", "menu_selected"] $ \name → do
      gamePCM ← withCore $ \native → do
        catalog ← loadCatalog defaultRuntimeConfig ⌦ uploadCatalog native defaultRuntimeConfig
        (handle, _) ← maybe (fail "missing shipped cue") pure $ Map.lookup name (uploadedSounds catalog)
        void $ required =<< submitOffline native [PlayNative handle (0, 0, 0) 0 0]
        required =<< renderOffline native 12000
      previewPCM ← withCore $ \native → do
        catalog ← loadPreviewCatalog (PreviewAudioConfig Nothing) native defaultRuntimeConfig
        entry ← maybe (fail "missing preview cue") pure $ find ((≡ name) ∘ paeLabel) (uploadedPreview catalog)
        (handle, _) ← maybe (fail "disabled preview cue") pure $ Map.lookup (paeId entry) (uploadedSounds catalog)
        void $ required =<< submitOffline native [PlayNative handle (0, 0, 0) 0 0]
        required =<< renderOffline native 12000
      previewPCM `shouldBe` gamePCM
      maximum (map abs previewPCM) `shouldSatisfy` (> 0)

  it "decodes an external file through the mixer and isolates a corrupt file from valid synth cues" $
    withFile $ \_ file → do
      corrupt ← canonicalizePath "test-headless/data/audio/corrupt.wav"
      withCore $ \native → do
        catalog ← loadPreviewCatalog (PreviewAudioConfig $ Just file) native defaultRuntimeConfig
        entry ← maybe (fail "missing file entry") pure $ find ((≡ Just file) ∘ paePath) (uploadedPreview catalog)
        paePlayable entry `shouldBe` True
        (handle, _) ← maybe (fail "sample not uploaded") pure $ Map.lookup (paeId entry) (uploadedSounds catalog)
        void $ required =<< submitOffline native [PlayNative handle (0, 0, 0) 0 0]
        pcm ← required =<< renderOffline native 12000
        maximum (map abs pcm) `shouldSatisfy` (> 0.1)
      withCore $ \native → do
        catalog ← loadPreviewCatalog (PreviewAudioConfig $ Just corrupt) native defaultRuntimeConfig
        let entries = uploadedPreview catalog
        map paePlayable (filter ((≡ Just corrupt) ∘ paePath) entries) `shouldBe` [False]
        map paePlayable (filter ((≡ "synth") ∘ paeCategory) entries) `shouldBe` [True, True]

  it "replaces playback, reloads edited PCM without accumulation, preserves volume and joins" $
    withFile $ \_ file → do
      replacement ← canonicalizePath "test-headless/data/audio/stereo.wav"
      withIsolatedResourceRoot $ bracket initializeEngineHeadlessQuiet
        (\(EngineInitResult env) → readIORef (loggerRef env) ⌦ shutdownLogger) $ \(EngineInitResult env) → do
          transport ← newAudioTransport defaultRuntimeConfig defaultVolumes
          statusRef ← newAudioStatusRef defaultVolumes
          let capability = AudioCapability transport statusRef
          started ← startAudioPreviewThread (PreviewAudioConfig $ Just file)
            (toCoreCapability env) capability ForcedNull
          thread ← maybe (fail "preview audio worker absent") pure started
          flip finally (shutdownThread thread) $ do
            initial ← awaitStatus capability ((≡ 1) ∘ audioPreviewRevision)
            audioLifecycle initial `shouldBe` AudioRunningNull
            entry ← maybe (fail "missing external file") pure $
              find ((≡ Just file) ∘ paePath) (audioPreviewEntries initial)
            atomically (enqueuePreviewPlay transport $ paeId entry) `shouldReturn` True
            played ← awaitStatus capability $ \s → maybe False ((> 0) ∘ nsAccepted) (audioNative s)
            atomically (enqueuePreviewPlay transport $ paeId entry) `shouldReturn` True
            void $ awaitStatus capability ((> audioEpoch played) ∘ audioEpoch)
            atomically $ resetAudioSession transport
            void $ awaitStatus capability $ \s → maybe False ((≡ 0) ∘ nsActiveVoices) (audioNative s)
            atomically $ publishAudioVolumes transport (Volumes 37 50 60)
            copyFile replacement file
            atomically $ reloadAudioPreview transport >> resetAudioSession transport
            reloaded ← awaitStatus capability ((≡ 2) ∘ audioPreviewRevision)
            audioVolumes reloaded `shouldBe` Volumes 37 50 60
            nsSampleCount <$> audioNative reloaded `shouldBe` (nsSampleCount <$> audioNative initial)
            nsDecodedFrames <$> audioNative reloaded `shouldBe` ((\n → nsDecodedFrames n - 12000 + 480) <$> audioNative initial)
            nsSink <$> audioNative reloaded `shouldBe` Just 0
            atomically (enqueuePreviewPlay transport $ paeId entry) `shouldReturn` True
            void $ awaitStatus capability $ \s → maybe False ((> 0) ∘ nsAccepted) (audioNative s)
            shutdownThread thread
            audioLifecycle <$> readIORef statusRef `shouldReturn` AudioStopped

  it "retains Reload across newer stops and refuses plays without changing the epoch" $ do
    transport ← newAudioTransport (defaultRuntimeConfig { rcEventCapacity = 1 }) defaultVolumes
    atomically $ setAudioAvailable transport True
    atomically (enqueuePreviewPlay transport "preview_0001") `shouldReturn` True
    before ← atomically $ readAudioEpoch transport
    atomically (enqueuePreviewPlay transport "preview_0002") `shouldReturn` False
    atomically (readAudioEpoch transport) `shouldReturn` before
    atomically $ reloadAudioPreview transport >> resetAudioSession transport
    pending ← atomically $ readAudioEpoch transport
    atomically (enqueuePreviewPlay transport "preview_0001") `shouldReturn` False
    atomically (readAudioEpoch transport) `shouldReturn` pending
    batch ← atomically $ readAudioBatch transport 32
    map stampedRequest (batchRequests batch) `shouldSatisfy` elem AudioReloadPreview
