-- | Fixed-width catalog PODs. C compiler-derived offsets are checked by the
-- native static assertions and cross-language round-trip tests.
module Engine.Audio.Native.CatalogPOD
  ( DecodeLimits(..), NativeInstrument(..), NativeSound(..), catalogAbiSizes ) where

import UPrelude
import Foreign.Marshal.Utils (fillBytes)

#include "audio/syn_audio.h"

data DecodeLimits = DecodeLimits
  { dlEncodedBytes ∷ Word64
  , dlFrames ∷ Word64
  , dlPcmBytes ∷ Word64
  } deriving (Eq, Show)

instance Storable DecodeLimits where
  sizeOf _ = #{size syn_audio_decode_limits}
  alignment _ = #{alignment syn_audio_decode_limits}
  peek p = DecodeLimits
    <$> #{peek syn_audio_decode_limits, max_encoded_bytes} p
    <*> #{peek syn_audio_decode_limits, max_frames} p
    <*> #{peek syn_audio_decode_limits, max_pcm_bytes} p
  poke p value = do
    fillBytes p 0 #{size syn_audio_decode_limits}
    #{poke syn_audio_decode_limits, abi_version} p (#{const SYN_AUDIO_ABI} ∷ Word32)
    #{poke syn_audio_decode_limits, struct_size} p (#{size syn_audio_decode_limits} ∷ Word32)
    #{poke syn_audio_decode_limits, max_encoded_bytes} p (dlEncodedBytes value)
    #{poke syn_audio_decode_limits, max_frames} p (dlFrames value)
    #{poke syn_audio_decode_limits, max_pcm_bytes} p (dlPcmBytes value)

data NativeInstrument = NativeInstrument
  { niWaveform ∷ Word32
  , niSeed ∷ Word32
  , niFilter ∷ Word32
  , niRandomPhase ∷ Word32
  , niFrequency ∷ Float
  , niPhase ∷ Float
  , niAttackMs ∷ Float
  , niDecayMs ∷ Float
  , niSustain ∷ Float
  , niReleaseMs ∷ Float
  , niCutoff ∷ Float
  , niQ ∷ Float
  , niGainDb ∷ Float
  , niGateMs ∷ Float
  } deriving (Eq, Show)

instance Storable NativeInstrument where
  sizeOf _ = #{size syn_audio_instrument_desc}
  alignment _ = #{alignment syn_audio_instrument_desc}
  peek p = NativeInstrument
    <$> #{peek syn_audio_instrument_desc, waveform} p
    <*> #{peek syn_audio_instrument_desc, seed} p
    <*> #{peek syn_audio_instrument_desc, filter} p
    <*> #{peek syn_audio_instrument_desc, random_phase} p
    <*> #{peek syn_audio_instrument_desc, frequency} p
    <*> #{peek syn_audio_instrument_desc, phase} p
    <*> #{peek syn_audio_instrument_desc, attack_ms} p
    <*> #{peek syn_audio_instrument_desc, decay_ms} p
    <*> #{peek syn_audio_instrument_desc, sustain} p
    <*> #{peek syn_audio_instrument_desc, release_ms} p
    <*> #{peek syn_audio_instrument_desc, cutoff} p
    <*> #{peek syn_audio_instrument_desc, q} p
    <*> #{peek syn_audio_instrument_desc, gain_db} p
    <*> #{peek syn_audio_instrument_desc, gate_ms} p
  poke p value = do
    fillBytes p 0 #{size syn_audio_instrument_desc}
    #{poke syn_audio_instrument_desc, abi_version} p (#{const SYN_AUDIO_ABI} ∷ Word32)
    #{poke syn_audio_instrument_desc, struct_size} p (#{size syn_audio_instrument_desc} ∷ Word32)
    #{poke syn_audio_instrument_desc, waveform} p (niWaveform value)
    #{poke syn_audio_instrument_desc, seed} p (niSeed value)
    #{poke syn_audio_instrument_desc, filter} p (niFilter value)
    #{poke syn_audio_instrument_desc, random_phase} p (niRandomPhase value)
    #{poke syn_audio_instrument_desc, frequency} p (niFrequency value)
    #{poke syn_audio_instrument_desc, phase} p (niPhase value)
    #{poke syn_audio_instrument_desc, attack_ms} p (niAttackMs value)
    #{poke syn_audio_instrument_desc, decay_ms} p (niDecayMs value)
    #{poke syn_audio_instrument_desc, sustain} p (niSustain value)
    #{poke syn_audio_instrument_desc, release_ms} p (niReleaseMs value)
    #{poke syn_audio_instrument_desc, cutoff} p (niCutoff value)
    #{poke syn_audio_instrument_desc, q} p (niQ value)
    #{poke syn_audio_instrument_desc, gain_db} p (niGainDb value)
    #{poke syn_audio_instrument_desc, gate_ms} p (niGateMs value)

data NativeSound = NativeSound
  { ndSourceKind ∷ Word32
  , ndSourceHandle ∷ Word32
  , ndBus ∷ Word32
  , ndSpatial ∷ Word32
  , ndPriority ∷ Word32
  , ndMaxInstances ∷ Word32
  , ndOverflow ∷ Word32
  , ndCooldownMs ∷ Float
  , ndLoop ∷ Word32
  , ndFreeze ∷ Word32
  , ndSeed ∷ Word32
  , ndMinDistance ∷ Float
  , ndMaxDistance ∷ Float
  , ndVerticalScale ∷ Float
  , ndGainDb ∷ Float
  , ndGateMs ∷ Float
  , ndStopFadeMs ∷ Float
  } deriving (Eq, Show)

instance Storable NativeSound where
  sizeOf _ = #{size syn_audio_sound_desc}
  alignment _ = #{alignment syn_audio_sound_desc}
  peek p = NativeSound
    <$> #{peek syn_audio_sound_desc, source_kind} p
    <*> #{peek syn_audio_sound_desc, source_handle} p
    <*> #{peek syn_audio_sound_desc, bus} p
    <*> #{peek syn_audio_sound_desc, spatial} p
    <*> #{peek syn_audio_sound_desc, priority} p
    <*> #{peek syn_audio_sound_desc, max_instances} p
    <*> #{peek syn_audio_sound_desc, overflow} p
    <*> #{peek syn_audio_sound_desc, cooldown_ms} p
    <*> #{peek syn_audio_sound_desc, loop} p
    <*> #{peek syn_audio_sound_desc, freeze} p
    <*> #{peek syn_audio_sound_desc, seed} p
    <*> #{peek syn_audio_sound_desc, min_distance} p
    <*> #{peek syn_audio_sound_desc, max_distance} p
    <*> #{peek syn_audio_sound_desc, vertical_scale} p
    <*> #{peek syn_audio_sound_desc, gain_db} p
    <*> #{peek syn_audio_sound_desc, gate_ms} p
    <*> #{peek syn_audio_sound_desc, stop_fade_ms} p
  poke p value = do
    fillBytes p 0 #{size syn_audio_sound_desc}
    #{poke syn_audio_sound_desc, abi_version} p (#{const SYN_AUDIO_ABI} ∷ Word32)
    #{poke syn_audio_sound_desc, struct_size} p (#{size syn_audio_sound_desc} ∷ Word32)
    #{poke syn_audio_sound_desc, source_kind} p (ndSourceKind value)
    #{poke syn_audio_sound_desc, source_handle} p (ndSourceHandle value)
    #{poke syn_audio_sound_desc, bus} p (ndBus value)
    #{poke syn_audio_sound_desc, spatial} p (ndSpatial value)
    #{poke syn_audio_sound_desc, priority} p (ndPriority value)
    #{poke syn_audio_sound_desc, max_instances} p (ndMaxInstances value)
    #{poke syn_audio_sound_desc, overflow} p (ndOverflow value)
    #{poke syn_audio_sound_desc, cooldown_ms} p (ndCooldownMs value)
    #{poke syn_audio_sound_desc, loop} p (ndLoop value)
    #{poke syn_audio_sound_desc, freeze} p (ndFreeze value)
    #{poke syn_audio_sound_desc, seed} p (ndSeed value)
    #{poke syn_audio_sound_desc, min_distance} p (ndMinDistance value)
    #{poke syn_audio_sound_desc, max_distance} p (ndMaxDistance value)
    #{poke syn_audio_sound_desc, vertical_scale} p (ndVerticalScale value)
    #{poke syn_audio_sound_desc, gain_db} p (ndGainDb value)
    #{poke syn_audio_sound_desc, gate_ms} p (ndGateMs value)
    #{poke syn_audio_sound_desc, stop_fade_ms} p (ndStopFadeMs value)

catalogAbiSizes ∷ [(Int, Int)]
catalogAbiSizes =
  [ (#{size syn_audio_decode_limits}, #{alignment syn_audio_decode_limits})
  , (#{size syn_audio_instrument_desc}, #{alignment syn_audio_instrument_desc})
  , (#{size syn_audio_sound_desc}, #{alignment syn_audio_sound_desc})
  ]
