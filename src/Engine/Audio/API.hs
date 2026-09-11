-- | Semantic producer surface. Success means queued, not guaranteed audible.
module Engine.Audio.API
  ( EnqueueResult(..), playSound, startSoundLoop, updateSoundLoop, stopSoundLoop
  , readAudioStatus, currentVolumes, setVolumes, saveVolumes
  ) where

import UPrelude
import Control.Concurrent.STM (STM, atomically)
import Data.IORef (readIORef)
import Engine.Audio.Config.Player
import Engine.Audio.Status
import Engine.Audio.Transport
import Engine.Audio.Types
import Engine.Core.Capability.Audio

data EnqueueResult = Enqueued | QueueFull | InvalidArguments | AudioUnavailable
  deriving (Eq, Show)

-- The admission and counter reads are one transaction, so another producer
-- cannot change which refusal this operation reports.
enqueue ∷ AudioCapability → (AudioTransport → STM Bool) → IO EnqueueResult
enqueue capability operation = atomically $ do
  let transport = acTransport capability
  before ← readTransportStats transport
  accepted ← operation transport
  after ← readTransportStats transport
  pure $ if accepted then Enqueued
    else if transportInvalidDrops after > transportInvalidDrops before then InvalidArguments
    else if transportQueueDrops after > transportQueueDrops before then QueueFull
    else AudioUnavailable

playSound ∷ AudioCapability → Text → TriggerOptions → IO EnqueueResult
playSound cap sound options = enqueue cap (\transport → enqueuePlay transport sound options)

startSoundLoop ∷ AudioCapability → Text → Text → TriggerOptions → IO EnqueueResult
startSoundLoop cap loop sound options = enqueue cap (\transport → enqueueStartLoop transport loop sound options)

updateSoundLoop ∷ AudioCapability → Text → LoopOptions → IO EnqueueResult
updateSoundLoop cap loop options = enqueue cap (\transport → enqueueUpdateLoop transport loop options)

stopSoundLoop ∷ AudioCapability → Text → IO EnqueueResult
stopSoundLoop cap loop = enqueue cap (\transport → enqueueStopLoop transport loop)

readAudioStatus ∷ AudioCapability → IO AudioStatus
readAudioStatus = readIORef ∘ acStatusRef

currentVolumes ∷ AudioCapability → IO Volumes
currentVolumes = atomically ∘ readAudioVolumes ∘ acTransport

setVolumes ∷ AudioCapability → Volumes → IO ()
setVolumes capability = atomically ∘ publishAudioVolumes (acTransport capability)

saveVolumes ∷ AudioCapability → Volumes → IO (Either Text Volumes)
saveVolumes capability volumes = do
  result ← writeVolumes volumes
  forM_ result (setVolumes capability)
  pure result
