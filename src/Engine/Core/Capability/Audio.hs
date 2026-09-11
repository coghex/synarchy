-- | The approved audio-transport capability (audio_system_design.md D-25).
-- The two handles alias EngineEnv. Catalogs, PCM, voices, and native device
-- pointers are worker-private and cannot be reached through this projection.
module Engine.Core.Capability.Audio (AudioCapability(..), toAudioCapability) where

import Engine.Audio.Status (AudioStatusRef)
import Engine.Audio.Transport (AudioTransport)
import Engine.Core.State (EngineEnv, audioTransport, audioStatusRef)

data AudioCapability = AudioCapability
  { acTransport ∷ AudioTransport
  , acStatusRef ∷ AudioStatusRef
  }

toAudioCapability ∷ EngineEnv → AudioCapability
toAudioCapability env = AudioCapability
  { acTransport = audioTransport env
  , acStatusRef = audioStatusRef env
  }
