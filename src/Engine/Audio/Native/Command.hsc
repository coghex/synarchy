-- | Closed native command vocabulary; no strings, engine records, or pointers.
module Engine.Audio.Native.Command
  ( NativeCommand(..), CommandResult(..), XYZ, Affine3, commandAbiSizes ) where

import UPrelude
import Foreign.Marshal.Utils (fillBytes)

#include "audio/syn_audio.h"

type XYZ = (Float, Float, Float)
type Affine3 = ((Float, Float, Float, Float), (Float, Float, Float, Float),
               (Float, Float, Float, Float))

data NativeCommand
  = PlayNative Word32 XYZ Float Float
  | StartLoopNative Word64 Word32 XYZ Float Float
  | UpdateLoopNative Word64 (Maybe XYZ) (Maybe Float)
  | StopLoopNative Word64
  | RebaseNative Affine3
  | WrapFrameNative XYZ XYZ
  | VolumesNative Float Float Float
  | WorldMixNative Float Float
  | PlayerPauseNative Bool
  | ResetSessionNative
  | ClearWorldNative
  deriving (Eq, Show)

data CommandResult = CommandResult
  { crResult ∷ Word32, crEvictedLoop ∷ Word64, crStealReason ∷ Word32 } deriving (Eq, Show)

instance Storable NativeCommand where
  sizeOf _ = #{size syn_audio_command}
  alignment _ = #{alignment syn_audio_command}
  poke p command = do
    fillBytes p 0 #{size syn_audio_command}
    #{poke syn_audio_command, abi_version} p (#{const SYN_AUDIO_ABI} ∷ Word32)
    #{poke syn_audio_command, struct_size} p (#{size syn_audio_command} ∷ Word32)
    let tag kind = #{poke syn_audio_command, kind} p (kind ∷ Word32)
        sound value = #{poke syn_audio_command, sound_handle} p (value ∷ Word32)
        key value = #{poke syn_audio_command, loop_key} p (value ∷ Word64)
        flags value = #{poke syn_audio_command, flags} p (value ∷ Word32)
        values = pokeArray (#{ptr syn_audio_command, values} p ∷ Ptr Float)
        position (x, y, z) = [x, y, z]
    case command of
      PlayNative handle xyz gain pitch → do
        tag #{const SYN_AUDIO_PLAY}; sound handle; values (position xyz <> [gain, pitch])
      StartLoopNative loop handle xyz gain pitch → do
        tag #{const SYN_AUDIO_START_LOOP}; key loop; sound handle
        values (position xyz <> [gain, pitch])
      UpdateLoopNative loop xyz gain → do
        tag #{const SYN_AUDIO_UPDATE_LOOP}; key loop
        flags $ (if isJust xyz then #{const SYN_AUDIO_HAS_POSITION} else 0)
          ⌄ (if isJust gain then #{const SYN_AUDIO_HAS_GAIN} else 0)
        values (maybe [0, 0, 0] position xyz <> [fromMaybe 0 gain])
      StopLoopNative loop → tag #{const SYN_AUDIO_STOP_LOOP} >> key loop
      RebaseNative ((a,b,c,d), (e,f,g,h), (i,j,k,l)) → do
        tag #{const SYN_AUDIO_REBASE}; values [a,b,c,d,e,f,g,h,i,j,k,l]
      WrapFrameNative u v → tag #{const SYN_AUDIO_WRAP_FRAME} >> values (position u <> position v)
      VolumesNative master world ui → tag #{const SYN_AUDIO_VOLUMES} >> values [master, world, ui]
      WorldMixNative range gain → tag #{const SYN_AUDIO_WORLD_MIX} >> values [range, gain]
      PlayerPauseNative paused → tag #{const SYN_AUDIO_PAUSE} >> flags (if paused then 1 else 0)
      ResetSessionNative → tag #{const SYN_AUDIO_RESET}
      ClearWorldNative → tag #{const SYN_AUDIO_CLEAR_WORLD}
  peek p = do
    tag ← #{peek syn_audio_command, kind} p ∷ IO Word32
    sound ← #{peek syn_audio_command, sound_handle} p
    key ← #{peek syn_audio_command, loop_key} p
    flags ← #{peek syn_audio_command, flags} p ∷ IO Word32
    let v index = peekElemOff (#{ptr syn_audio_command, values} p ∷ Ptr Float) index
        xyz = (,,) <$> v 0 <*> v 1 <*> v 2
        row offset = (,,,) <$> v offset <*> v (offset + 1) <*> v (offset + 2) <*> v (offset + 3)
    case tag of
      #{const SYN_AUDIO_PLAY} → PlayNative sound <$> xyz <*> v 3 <*> v 4
      #{const SYN_AUDIO_START_LOOP} → StartLoopNative key sound <$> xyz <*> v 3 <*> v 4
      #{const SYN_AUDIO_UPDATE_LOOP} → UpdateLoopNative key
        <$> (if flags ⌃ #{const SYN_AUDIO_HAS_POSITION} ≢ 0 then Just <$> xyz else pure Nothing)
        <*> (if flags ⌃ #{const SYN_AUDIO_HAS_GAIN} ≢ 0 then Just <$> v 3 else pure Nothing)
      #{const SYN_AUDIO_STOP_LOOP} → pure (StopLoopNative key)
      #{const SYN_AUDIO_REBASE} → RebaseNative <$> ((,,) <$> row 0 <*> row 4 <*> row 8)
      #{const SYN_AUDIO_WRAP_FRAME} → WrapFrameNative <$> xyz <*> ((,,) <$> v 3 <*> v 4 <*> v 5)
      #{const SYN_AUDIO_VOLUMES} → VolumesNative <$> v 0 <*> v 1 <*> v 2
      #{const SYN_AUDIO_WORLD_MIX} → WorldMixNative <$> v 0 <*> v 1
      #{const SYN_AUDIO_PAUSE} → pure (PlayerPauseNative $ flags ≢ 0)
      #{const SYN_AUDIO_RESET} → pure ResetSessionNative
      #{const SYN_AUDIO_CLEAR_WORLD} → pure ClearWorldNative
      _ → fail "unknown native audio command tag"

instance Storable CommandResult where
  sizeOf _ = #{size syn_audio_command_result}
  alignment _ = #{alignment syn_audio_command_result}
  peek p = CommandResult
    <$> #{peek syn_audio_command_result, result} p
    <*> #{peek syn_audio_command_result, evicted_loop_key} p
    <*> #{peek syn_audio_command_result, steal_reason} p
  poke p result = do
    fillBytes p 0 #{size syn_audio_command_result}
    #{poke syn_audio_command_result, abi_version} p (#{const SYN_AUDIO_ABI} ∷ Word32)
    #{poke syn_audio_command_result, struct_size} p (#{size syn_audio_command_result} ∷ Word32)
    #{poke syn_audio_command_result, result} p (crResult result)
    #{poke syn_audio_command_result, evicted_loop_key} p (crEvictedLoop result)
    #{poke syn_audio_command_result, steal_reason} p (crStealReason result)

commandAbiSizes ∷ [(Int, Int)]
commandAbiSizes =
  [ (#{size syn_audio_command}, #{alignment syn_audio_command})
  , (#{size syn_audio_command_result}, #{alignment syn_audio_command_result}) ]
