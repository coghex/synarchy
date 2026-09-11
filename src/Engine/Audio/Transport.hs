-- | One abstract shared container. Native handles and catalog maps stay private
-- to the audio worker. High-rate producers never wait for queue capacity.
module Engine.Audio.Transport
  ( AudioTransport, AudioRequest(..), Stamped(..), AudioBatch(..), TransportStats(..), emptyTransportStats
  , newAudioTransport, audioRuntimeConfig, enqueuePlay, enqueueStartLoop, enqueueUpdateLoop
  , enqueueStopLoop, setAudioPlayerPaused, resetAudioSession, wakeAudioShutdown, reloadAudioPreview, enqueuePreviewPlay
  , publishAudioListener, publishAudioListenerForEpoch, publishAudioVolumes, readAudioVolumes, setAudioAvailable
  , readAudioBatch, readAudioEpoch, waitAudioActivity, readTransportStats, coalesceLoopUpdates
  ) where

import UPrelude
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Data.List (minimumBy, sortOn)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime (RuntimeConfig(..))
import Engine.Audio.Types

data AudioRequest
  = AudioPlay Text TriggerOptions
  | AudioStartLoop Text Text TriggerOptions
  | AudioUpdateLoop Text LoopOptions
  | AudioStopLoop Text
  | AudioSetPlayerPaused Bool
  | AudioResetSession
  | AudioShutdownWake
  | AudioListener (Maybe ListenerSnapshot)
  | AudioVolumes Volumes
  | AudioReloadPreview
  deriving (Eq, Show)

data Stamped = Stamped
  { stampedSequence ∷ Word64, stampedEpoch ∷ Word64, stampedRequest ∷ AudioRequest }
  deriving (Eq, Show)

data TransportStats = TransportStats
  { transportQueued ∷ !Word64, transportQueueDrops ∷ !Word64, transportDisabledDrops ∷ !Word64
  , transportInvalidDrops ∷ !Word64, transportStaleDrops ∷ !Word64
  , transportListenerCoalesced ∷ !Word64, transportVolumesCoalesced ∷ !Word64
  , transportLoopCoalesced ∷ !Word64, transportEventDepth ∷ !Int, transportControlDepth ∷ !Int
  , transportEventPeak ∷ !Int, transportControlPeak ∷ !Int
  } deriving (Eq, Show)

emptyTransportStats ∷ TransportStats
emptyTransportStats = TransportStats 0 0 0 0 0 0 0 0 0 0 0 0

data AudioBatch = AudioBatch
  { batchEpoch ∷ Word64, batchRequests ∷ [Stamped], batchStats ∷ TransportStats }
  deriving (Eq, Show)

data AudioTransport = AudioTransport
  { atRuntime ∷ RuntimeConfig
  , atEvents ∷ TBQueue Stamped, atControls ∷ TQueue Stamped
  , atListener ∷ TVar (Maybe Stamped), atVolumes ∷ TVar (Maybe Stamped)
  , atLiveVolumes ∷ TVar Volumes, atSequence ∷ TVar Word64, atEpoch ∷ TVar Word64
  , atAvailable ∷ TVar Bool, atStats ∷ TVar TransportStats
  } deriving (Eq)

newAudioTransport ∷ RuntimeConfig → Volumes → IO AudioTransport
newAudioTransport config volumes = atomically $ AudioTransport config
  <$> newTBQueue (fromIntegral $ max 1 $ rcEventCapacity config) <*> newTQueue
  <*> newTVar Nothing <*> newTVar (Just $ Stamped 0 0 $ AudioVolumes $ clampVolumes volumes)
  <*> newTVar (clampVolumes volumes) <*> newTVar 1 <*> newTVar 0 <*> newTVar False
  <*> newTVar emptyTransportStats

audioRuntimeConfig ∷ AudioTransport → RuntimeConfig
audioRuntimeConfig = atRuntime

stamp ∷ AudioTransport → AudioRequest → STM Stamped
stamp transport request = do
  sequence ← readTVar (atSequence transport)
  epoch ← readTVar (atEpoch transport)
  writeTVar (atSequence transport) $! sequence + 1
  pure $ Stamped sequence epoch request

admit ∷ AudioTransport → Bool → STM Bool
admit transport valid = do
  available ← readTVar (atAvailable transport)
  unless (valid ∧ available) $ modifyTVar' (atStats transport) $ \stats →
    if not valid then stats { transportInvalidDrops = transportInvalidDrops stats + 1 }
    else stats { transportDisabledDrops = transportDisabledDrops stats + 1 }
  pure (valid ∧ available)

enqueuePlay ∷ AudioTransport → Text → TriggerOptions → STM Bool
enqueuePlay transport sound options = do
  valid ← admit transport (validSoundId sound ∧ validTrigger options)
  if not valid then pure False else do
    full ← isFullTBQueue (atEvents transport)
    if full then do
      modifyTVar' (atStats transport) $ \s → s { transportQueueDrops = transportQueueDrops s + 1 }
      pure False
    else do
      stamp transport (AudioPlay sound options) ⌦ writeTBQueue (atEvents transport)
      modifyTVar' (atStats transport) $ \s → s { transportQueued = transportQueued s + 1,
        transportEventDepth = transportEventDepth s + 1,
        transportEventPeak = max (transportEventPeak s) (transportEventDepth s + 1) }
      pure True

control ∷ AudioTransport → AudioRequest → STM ()
control transport request = do
  stamp transport request ⌦ writeTQueue (atControls transport)
  modifyTVar' (atStats transport) $ \s → s { transportQueued = transportQueued s + 1,
    transportControlDepth = transportControlDepth s + 1,
    transportControlPeak = max (transportControlPeak s) (transportControlDepth s + 1) }

checkedControl ∷ AudioTransport → Bool → AudioRequest → STM Bool
checkedControl transport valid request = do
  accepted ← admit transport valid
  when accepted $ control transport request
  pure accepted

enqueueStartLoop ∷ AudioTransport → Text → Text → TriggerOptions → STM Bool
enqueueStartLoop transport loop sound options = checkedControl transport
  (validLoopId loop ∧ validSoundId sound ∧ validTrigger options) (AudioStartLoop loop sound options)

enqueueUpdateLoop ∷ AudioTransport → Text → LoopOptions → STM Bool
enqueueUpdateLoop transport loop options = checkedControl transport
  (validLoopId loop ∧ validLoopOptions options) (AudioUpdateLoop loop options)

enqueueStopLoop ∷ AudioTransport → Text → STM Bool
enqueueStopLoop transport loop = checkedControl transport (validLoopId loop) (AudioStopLoop loop)

-- Engine controls are retained even while native startup is pending/disabled.
setAudioPlayerPaused ∷ AudioTransport → Bool → STM ()
setAudioPlayerPaused transport = control transport ∘ AudioSetPlayerPaused

resetAudioSession ∷ AudioTransport → STM ()
resetAudioSession transport = do
  modifyTVar' (atEpoch transport) (+ 1)
  writeTVar (atListener transport) Nothing
  control transport AudioResetSession

wakeAudioShutdown ∷ AudioTransport → STM ()
wakeAudioShutdown transport = control transport AudioShutdownWake

reloadAudioPreview ∷ AudioTransport → STM ()
reloadAudioPreview transport = do
  setAudioAvailable transport False
  resetAudioSession transport
  control transport AudioReloadPreview

-- Preview replacement is one transaction: a refused play cannot cancel an
-- in-progress reload or advance the session epoch.
enqueuePreviewPlay ∷ AudioTransport → Text → STM Bool
enqueuePreviewPlay transport sound = do
  accepted ← admit transport (validSoundId sound)
  if not accepted then pure False else do
    full ← isFullTBQueue (atEvents transport)
    if full then do
      modifyTVar' (atStats transport) $ \s → s { transportQueueDrops = transportQueueDrops s + 1 }
      pure False
    else do
      resetAudioSession transport
      enqueuePlay transport sound defaultTriggerOptions

publishAudioListener ∷ AudioTransport → Maybe ListenerSnapshot → STM ()
publishAudioListener transport listener = do
  -- Invalid/missing snapshots explicitly clear World rather than keeping an old
  -- listener indefinitely while the renderer has moved elsewhere.
  let valid = listener ⌦ \value → if validListener value then Just value else Nothing
  previous ← readTVar (atListener transport)
  when (isJust previous) $ modifyTVar' (atStats transport) $ \s →
    s { transportListenerCoalesced = transportListenerCoalesced s + 1 }
  stamp transport (AudioListener valid) ⌦ writeTVar (atListener transport) ∘ Just

-- | A camera snapshot assembled outside STM may finish after session publish.
-- Refuse it if its captured epoch is no longer current.
publishAudioListenerForEpoch ∷ AudioTransport → Word64 → Maybe ListenerSnapshot → STM Bool
publishAudioListenerForEpoch transport expected listener = do
  epoch ← readAudioEpoch transport
  if epoch ≢ expected then pure False
  else publishAudioListener transport listener >> pure True

publishAudioVolumes ∷ AudioTransport → Volumes → STM ()
publishAudioVolumes transport volumes = do
  previous ← readTVar (atVolumes transport)
  when (isJust previous) $ modifyTVar' (atStats transport) $ \s →
    s { transportVolumesCoalesced = transportVolumesCoalesced s + 1 }
  let chosen = clampVolumes volumes
  writeTVar (atLiveVolumes transport) chosen
  stamp transport (AudioVolumes chosen) ⌦ writeTVar (atVolumes transport) ∘ Just

readAudioVolumes ∷ AudioTransport → STM Volumes
readAudioVolumes = readTVar ∘ atLiveVolumes

setAudioAvailable ∷ AudioTransport → Bool → STM ()
setAudioAvailable = writeTVar ∘ atAvailable

readTransportStats ∷ AudioTransport → STM TransportStats
readTransportStats = readTVar ∘ atStats

readAudioEpoch ∷ AudioTransport → STM Word64
readAudioEpoch = readTVar ∘ atEpoch

-- | Bounded merge across every lane. A latest-wins slot newer than a queued
-- event stays pending until that event is consumed; it cannot leapfrog it.
readAudioBatch ∷ AudioTransport → Int → STM AudioBatch
readAudioBatch transport requested = do
  epoch ← readTVar (atEpoch transport)
  drained ← takeNext (max 1 $ min 1024 requested) []
  let belongs item = case stampedRequest item of
        AudioVolumes _ → True -- Process-scoped settings survive replacement.
        AudioReloadPreview → True -- A subsequent stop/play must not cancel Reload.
        _ → stampedEpoch item ≡ epoch
      current = filter belongs drained
      compact = coalesceLoopUpdates current
  modifyTVar' (atStats transport) $ \s → s
    { transportStaleDrops = transportStaleDrops s + fromIntegral (length drained - length current)
    , transportLoopCoalesced = transportLoopCoalesced s + fromIntegral (length current - length compact) }
  AudioBatch epoch compact <$> readTransportStats transport
  where
    peekEvent = (Just <$> peekTBQueue (atEvents transport)) `orElse` pure Nothing
    peekControl = (Just <$> peekTQueue (atControls transport)) `orElse` pure Nothing
    takeNext 0 acc = pure (reverse acc)
    takeNext remaining acc = do
      event ← peekEvent
      nextControl ← peekControl
      listener ← readTVar (atListener transport)
      volumes ← readTVar (atVolumes transport)
      let candidates = [(lane, item) | (lane, Just item) ← zip [0 ∷ Int ..] [event, nextControl, listener, volumes]]
      case candidates of
        [] → pure (reverse acc)
        _ → do
          let (lane, item) = minimumBy (comparing $ stampedSequence ∘ snd) candidates
          case lane of
            0 → do
              void $ readTBQueue (atEvents transport)
              modifyTVar' (atStats transport) $ \s → s { transportEventDepth = transportEventDepth s - 1 }
            1 → do
              void $ readTQueue (atControls transport)
              modifyTVar' (atStats transport) $ \s → s { transportControlDepth = transportControlDepth s - 1 }
            2 → writeTVar (atListener transport) Nothing
            _ → writeTVar (atVolumes transport) Nothing
          takeNext (remaining - 1) (item : acc)

-- | Merge partial updates by logical ID, retaining each last update's stamp.
-- Any non-update is an ordering barrier (especially stop/start/reset).
coalesceLoopUpdates ∷ [Stamped] → [Stamped]
coalesceLoopUpdates = go Map.empty
  where
    flush = sortOn stampedSequence ∘ Map.elems
    go pending [] = flush pending
    go pending (item:rest) = case stampedRequest item of
      AudioUpdateLoop name options →
        let merged = case Map.lookup name pending of
              Just old | AudioUpdateLoop _ prior ← stampedRequest old → options
                { loopPosition = loopPosition options <|> loopPosition prior
                , loopGainDb = loopGainDb options <|> loopGainDb prior }
              _ → options
        in go (Map.insert name (item { stampedRequest = AudioUpdateLoop name merged }) pending) rest
      _ → flush pending <> [item] <> go Map.empty rest

waitAudioActivity ∷ AudioTransport → Int → IO ()
waitAudioActivity transport microseconds = do
  deadline ← registerDelay (max 1 microseconds)
  atomically $ do
    stats ← readTransportStats transport
    listener ← readTVar (atListener transport)
    volumes ← readTVar (atVolumes transport)
    expired ← readTVar deadline
    check $ expired ∨ transportEventDepth stats > 0 ∨ transportControlDepth stats > 0
      ∨ isJust listener ∨ isJust volumes
