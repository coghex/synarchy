{-# LANGUAGE StrictData #-}
-- | Pointer-free public telemetry. Only the audio worker publishes snapshots.
module Engine.Audio.Status
  ( AudioLifecycle(..), AudioDropReason(..), AudioStatus(..), AudioStatusRef
  , newAudioStatusRef, initialAudioStatus, publishAudioStatus, nativeLifecycle, boundedAudioError
  ) where

import UPrelude
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import GHC.Clock (getMonotonicTimeNSec)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Engine.Audio.Health (AudioHealth(..))
import Engine.Audio.Config.Player (Volumes)
import Engine.Audio.Native (NativeStatus)
import Engine.Audio.Preview.Types (PreviewAudioEntry)
import Engine.Audio.Transport (TransportStats, emptyTransportStats)

data AudioLifecycle = AudioStarting | AudioRunningNull | AudioRunningReal
  | AudioDegradedNull | AudioStopped | AudioDisabled
  deriving (Eq, Show)

data AudioDropReason = UnknownSound | NoListener | WrongPage | MissingLoop
  | LoopConflict | InvalidRequest | VoiceCooldown | VoiceCapacity | PausedWorld
  | StaleSession | VoicePriority
  deriving (Eq, Ord, Enum, Bounded, Show)

data AudioStatus = AudioStatus
  { audioLifecycle ∷ AudioLifecycle
  , audioNative ∷ Maybe NativeStatus
  , audioTransportStats ∷ TransportStats
  , audioDrops ∷ Map.Map AudioDropReason Word64
  , audioVolumes ∷ Volumes, audioEpoch ∷ Word64
  , audioCatalogSounds ∷ Int, audioCatalogWarnings ∷ Int
  , audioSessionResets ∷ Word64, audioLastError ∷ Text
  , audioHealth ∷ AudioHealth, audioSnapshotSequence ∷ Word64, audioPublishedNs ∷ Word64
  , audioDiagnosticWarnings ∷ Word64
  , audioPerSoundSteals ∷ Word64, audioGlobalSteals ∷ Word64
  , audioCatalogEntries ∷ Map.Map Text (Int, Int)
  , audioPreviewEntries ∷ [PreviewAudioEntry]
  , audioPreviewRevision ∷ Word64
  } deriving (Eq, Show)

type AudioStatusRef = IORef AudioStatus

initialAudioStatus ∷ Volumes → AudioStatus
initialAudioStatus volumes = AudioStatus AudioDisabled Nothing
  emptyTransportStats Map.empty volumes 0 0 0 0 ""
  (AudioHealth False 0 0 0 False False) 0 0 0 0 0 Map.empty [] 0

newAudioStatusRef ∷ Volumes → IO AudioStatusRef
newAudioStatusRef = newIORef ∘ initialAudioStatus

-- | Every publication, including startup/failure/stop, has one monotonic sequence.
-- Force the new record AND its fields before returning: even when nobody reads
-- telemetry, neither the sequence nor a disabled-worker update may retain history.
publishAudioStatus ∷ AudioStatusRef → AudioStatus → IO ()
publishAudioStatus ref status = do
  now ← getMonotonicTimeNSec
  atomicModifyIORef' ref $ \previous →
    (status { audioSnapshotSequence = audioSnapshotSequence previous + 1, audioPublishedNs = now }, ())

nativeLifecycle ∷ Word32 → AudioLifecycle
nativeLifecycle 0 = AudioStarting
nativeLifecycle 1 = AudioRunningNull
nativeLifecycle 2 = AudioRunningReal
nativeLifecycle 3 = AudioDegradedNull
nativeLifecycle 4 = AudioStopped
nativeLifecycle _ = AudioDisabled

boundedAudioError ∷ Text → Text
boundedAudioError = Text.take 512
