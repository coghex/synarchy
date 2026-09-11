-- | Optional audio worker. Unlike simulation workers, audio failure disables
-- only audio. The normal ThreadState join still covers native callback teardown.
module Engine.Audio.Thread (startAudioThread, startAudioPreviewThread) where

import UPrelude
import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, SomeAsyncException, try, throwIO,
  fromException, mask, finally, onException, uninterruptibleMask_)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import Engine.Audio.Catalog.Resolve (loadCatalog)
import Engine.Audio.Catalog.Upload (uploadCatalog, uploadWarnings, uploadedRevision)
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import Engine.Audio.Health
import Engine.Audio.Native
import Engine.Audio.Preview.Catalog (loadPreviewCatalog)
import Engine.Audio.Preview.Types
import Engine.Audio.Runtime
import Engine.Audio.Status
import Engine.Audio.Transport
import Engine.Core.Capability.Audio
import Engine.Core.Capability.Core
import Engine.Core.Log (LoggerState, LogCategory(..), logThreadInfo, logThreadWarn)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..), shutdownThread)

-- Diagnostics must not turn optional audio failure into an engine failure.
trySynchronous ∷ IO α → IO (Either SomeException α)
trySynchronous action = try action ⌦ \result → case result of
  Left err | isJust (fromException err ∷ Maybe SomeAsyncException) → throwIO err
  _ → pure result

announce ∷ IORef LoggerState → Bool → Text → IO ()
announce loggerRef warning message = void $ trySynchronous $ do
  logger ← readIORef loggerRef
  (if warning then logThreadWarn else logThreadInfo) logger CatAudio (boundedAudioError message)

startAudioThread ∷ CoreCapability → AudioCapability → Sink → IO (Maybe ThreadState)
startAudioThread = startAudioThreadWith Nothing

startAudioPreviewThread ∷ PreviewAudioConfig → CoreCapability → AudioCapability → Sink → IO (Maybe ThreadState)
startAudioPreviewThread options = startAudioThreadWith (Just options)

startAudioThreadWith ∷ Maybe PreviewAudioConfig → CoreCapability → AudioCapability → Sink → IO (Maybe ThreadState)
startAudioThreadWith preview core capability sink = mask $ \restore → do
  running ← newIORef ThreadRunning
  done ← newEmptyMVar
  ready ← newEmptyMVar
  let transport = acTransport capability
      config = audioRuntimeConfig transport
      logger = ccLoggerRef core
      signalReady = void $ tryPutMVar ready ()
      publish = publishSnapshot capability
      disabled message = do
        atomically $ setAudioAvailable transport False
        previous ← readIORef (acStatusRef capability)
        publish previous { audioLifecycle = AudioDisabled, audioLastError = boundedAudioError message,
          audioPreviewEntries = [], audioPreviewRevision = audioPreviewRevision previous
            + if isJust preview then 1 else 0 }
        signalReady
        announce logger True ("Audio disabled: " <> message)
        reload ← disabledLoop (isJust preview) running capability 0
        when reload body
      finish = do
        atomically $ setAudioAvailable transport False
        previous ← readIORef (acStatusRef capability)
        publish previous { audioLifecycle = AudioStopped }
        signalReady
        putMVar done ()
      body = do
        previous ← readIORef (acStatusRef capability)
        publish previous { audioLifecycle = AudioStarting }
        result ← trySynchronous $ withNative (rcNative config) sink $ \native → do
          loaded ← case preview of
            Nothing → loadCatalog config ⌦ uploadCatalog native config
            Just options → loadPreviewCatalog options native config
          let catalog = loaded { uploadedRevision = if isJust preview
                then audioPreviewRevision previous + 1 else 0 }
          mapM_ (announce logger True) (uploadWarnings catalog)
          volumes ← atomically $ readAudioVolumes transport
          let gain = volumeGain (rcVolumeExponent config)
          void $ requireNative =<< submitOffline native
            [VolumesNative (gain $ volumeMaster volumes) (gain $ volumeWorld volumes)
              (gain $ volumeUI volumes), WorldMixNative 1 0]
          requireNative =<< startNative native
          epoch ← atomically $ readAudioEpoch transport
          let runtime = newAudioRuntime catalog epoch
          nativeStatus ← readNativeStatus native
          stats ← atomically $ readTransportStats transport
          publish $ runtimeStatus volumes stats nativeStatus runtime
          atomically $ setAudioAvailable transport (nsLifecycle nativeStatus ≢ 5)
          signalReady
          announce logger False ("Starting audio worker: " <> tshow (nativeLifecycle $ nsLifecycle nativeStatus))
          now ← getMonotonicTimeNSec
          workerLoop (isJust preview) running capability logger native runtime []
            (WorkerHealth now (nsTransitions nativeStatus) (newHealth nativeStatus) newDiagnosticLimiter 0)
        case result of
          Left err → disabled (tshow err)
          Right (Left err) → disabled err
          Right (Right reload) → when reload body
  forked ← trySynchronous $ uninterruptibleMask_ $
    forkIOWithUnmask $ \unmask → unmask body `finally` finish
  case forked of
    Left err → do
      previous ← readIORef (acStatusRef capability)
      publish previous { audioLifecycle = AudioDisabled, audioLastError = boundedAudioError $ tshow err }
      announce logger True ("Cannot start audio worker: " <> tshow err)
      pure Nothing
    Right thread → do
      let state = ThreadState running thread done "Audio" logger CatAudio
      restore (readMVar ready) `onException` shutdownThread state
      pure (Just state)

requireNative ∷ Either Text α → IO α
requireNative = either (ioError ∘ userError ∘ Text.unpack) pure

-- Every publication, including startup/failure/stop, has one monotonic sequence.
publishSnapshot ∷ AudioCapability → AudioStatus → IO ()
publishSnapshot capability = publishAudioStatus (acStatusRef capability)

data WorkerHealth = WorkerHealth
  { workerPublishAt ∷ !Word64, workerTransitions ∷ !Word64
  , workerHealth ∷ !Health, workerLimiter ∷ !DiagnosticLimiter, workerWarningCount ∷ !Word64 }

workerLoop ∷ Bool → IORef ThreadControl → AudioCapability → IORef LoggerState → Native
  → AudioRuntime → [Stamped] → WorkerHealth → IO Bool
workerLoop preview running capability logger native runtime pending health = do
  stopped ← (≡ ThreadStopped) <$> readIORef running
  let transport = acTransport capability
      config = audioRuntimeConfig transport
      limit = fromIntegral $ ncCommandBatchLimit (rcNative config)
  -- Every producer is already joined when shutdown requests this stop. Drain
  -- bounded batches until empty, including final volume/loop-stop controls.
  (epoch, input) ← atomically $ if null pending
    then do batch ← readAudioBatch transport limit; pure (batchEpoch batch, batchRequests batch)
    else (, pending) <$> readAudioEpoch transport
  if preview ∧ not stopped ∧ any ((≡ AudioReloadPreview) ∘ stampedRequest) input
    then pure True -- withNative joins/destroys the old core before the next boot.
    else continue stopped epoch input transport config
  where
   continue stopped epoch input transport config = do
    (next, remaining, nativeStatus) ← requireNative =<< serviceAudioRuntime native config epoch input runtime
    now ← getMonotonicTimeNSec
    stats ← atomically $ readTransportStats transport
    volumes ← atomically $ readAudioVolumes transport
    atomically $ setAudioAvailable transport (not stopped ∧ nsLifecycle nativeStatus ≢ 5)
    let changed = nsTransitions nativeStatus ≢ workerTransitions health
        due = now ≥ workerPublishAt health
        (observed, healthStatus) = observeHealth config now stats nativeStatus (workerHealth health)
        snapshot = (runtimeStatus volumes stats nativeStatus next) { audioHealth = healthStatus }
        status = if Text.null (nsLastError nativeStatus) then snapshot
          else snapshot { audioLastError = boundedAudioError $ nsLastError nativeStatus }
        interval = round (1e9 * rcRateLimitSeconds config)
        diagnostics = [(tshow reason, name, tshow reason <> ": " <> name)
          | (reason, name) ← runtimeDiagnostics next]
          <> [(message, "", message) | message ← healthWarnings config healthStatus]
        rateLimit (limiter, messages) (reason, name, message) =
          let (updated, emitted) = limitDiagnostic now interval (reason, name) message limiter
          in (updated, maybe messages (: messages) emitted)
        (limiter, messages) = foldl' rateLimit (workerLimiter health, []) diagnostics
        warningCount = workerWarningCount health + fromIntegral (length messages)
    when changed $ announce logger (nsLifecycle nativeStatus ≥ 3)
      ("Audio output: " <> tshow (nativeLifecycle $ nsLifecycle nativeStatus) <> " " <> nsLastError nativeStatus)
    mapM_ (announce logger True) (reverse messages)
    when (due ∨ changed ∨ stopped) $
      publishSnapshot capability status { audioDiagnosticWarnings = warningCount }
    let published = WorkerHealth (if due ∨ changed then now + round (1e9 / rcPublishHz config)
          else workerPublishAt health) (nsTransitions nativeStatus) observed limiter warningCount
        empty = null remaining ∧ transportEventDepth stats ≡ 0 ∧ transportControlDepth stats ≡ 0
    if stopped ∧ empty then False <$ stopNative native
    else do
      when (null remaining) $ waitAudioActivity transport (rcWorkerIdleUs config)
      workerLoop preview running capability logger native next remaining published

disabledLoop ∷ Bool → IORef ThreadControl → AudioCapability → Word64 → IO Bool
disabledLoop preview running capability nextPublish = do
  stopped ← (≡ ThreadStopped) <$> readIORef running
  let transport = acTransport capability
      config = audioRuntimeConfig transport
  batch ← atomically $ readAudioBatch transport (fromIntegral $ ncCommandBatchLimit $ rcNative config)
  volumes ← atomically $ readAudioVolumes transport
  now ← getMonotonicTimeNSec
  let due = now ≥ nextPublish
      stats = batchStats batch
      empty = transportEventDepth stats ≡ 0 ∧ transportControlDepth stats ≡ 0
  when (due ∨ stopped) $ do
    previous ← readIORef (acStatusRef capability)
    publishSnapshot capability previous
      { audioVolumes = volumes, audioEpoch = batchEpoch batch, audioTransportStats = stats }
  if stopped ∧ empty then pure False
  else if preview ∧ not stopped ∧ any ((≡ AudioReloadPreview) ∘ stampedRequest) (batchRequests batch)
    then pure True
  else do
    waitAudioActivity transport (rcWorkerIdleUs config)
    disabledLoop preview running capability (if due then now + round (1e9 / rcPublishHz config) else nextPublish)
