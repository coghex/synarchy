-- | Resolved authored audio. These records are owned by the audio worker.
module Engine.Audio.Catalog.Types where

import UPrelude
import qualified Data.Map.Strict as Map

data Bus = WorldBus | UIBus deriving (Eq, Show)
data Overflow = DropNew | StealOldest deriving (Eq, Show)
data Waveform = Sine | Saw | Square | Triangle | WhiteNoise deriving (Eq, Show)
data FilterMode = Bypass | LowPass | HighPass | BandPass deriving (Eq, Show)

data Policy = Policy
  { policyBus ∷ Bus, policySpatial ∷ Bool
  , policyMinDistance ∷ Float, policyMaxDistance ∷ Float, policyVerticalScale ∷ Float
  , policyGainDb ∷ Float, policyPriority ∷ Word32, policyMaxInstances ∷ Word32
  , policyOverflow ∷ Overflow, policyCooldownMs ∷ Float
  , policyAllowLoop ∷ Bool, policyStopFadeMs ∷ Float, policyFreeze ∷ Bool
  } deriving (Eq, Show)

data Timbre = Timbre
  { timbreWaveform ∷ Waveform, timbreFrequency ∷ Float, timbreRandomPhase ∷ Bool
  , timbreNoiseSeed ∷ Word32, timbreAttackMs ∷ Float, timbreDecayMs ∷ Float
  , timbreSustain ∷ Float, timbreReleaseMs ∷ Float
  , timbreFilter ∷ FilterMode, timbreCutoff ∷ Float, timbreResonance ∷ Float
  , timbreGainDb ∷ Float, timbreGateMs ∷ Float
  } deriving (Eq, Show)

data Source = SampleSource FilePath | SynthSource Timbre deriving (Eq, Show)
data Sound = Sound
  { soundId ∷ Text, soundPolicy ∷ Policy, soundSource ∷ Source }
  deriving (Eq, Show)

data Catalog = Catalog
  { catalogTypes ∷ Map.Map Text Policy
  , catalogInstruments ∷ Map.Map Text Timbre
  , catalogSounds ∷ Map.Map Text Sound
  , catalogWarnings ∷ [Text], catalogCounts ∷ Map.Map Text (Int, Int)
  } deriving (Eq, Show)
