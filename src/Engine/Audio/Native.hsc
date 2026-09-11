{-# LANGUAGE ForeignFunctionInterface #-}
-- | The audio worker's opaque native boundary. The callback never enters Haskell.
-- POD offsets come from the C compiler; the layout helper has no Vulkan dependency.
module Engine.Audio.Native
  ( Native, NativeConfig(..), NativeStatus(..), Sink(..)
  , defaultNativeConfig, withNative, startNative, stopNative, serviceNative
  , readNativeStatus, renderOffline, nativeAbiSizes
  , loadSampleNative, addInstrumentNative, addSoundNative, serviceCommandsNative, submitOffline
  , module Engine.Audio.Native.CatalogPOD, module Engine.Audio.Native.Command
  ) where

import UPrelude
import Control.Exception (finally, mask)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString as BS
import Engine.Audio.Native.CatalogPOD
import Engine.Audio.Native.Command

#include "audio/syn_audio.h"

data Core
newtype Native = Native (Ptr Core)
data Sink = ForcedNull | RealWithNullFallback deriving (Eq, Show)

data NativeConfig = NativeConfig
  { ncSampleRate ∷ Word32, ncChunkFrames ∷ Word32
  , ncTargetFillFrames ∷ Word32, ncRingCapacityFrames ∷ Word32
  , ncMaxVoices ∷ Word32, ncCommandBatchLimit ∷ Word32
  , ncPeriodFrames ∷ Word32, ncPeriods ∷ Word32
  , ncRetryInitialMs ∷ Word32, ncRetryMaxMs ∷ Word32
  , ncBusGainMs ∷ Float, ncInstanceGainMs ∷ Float
  , ncPauseOutMs ∷ Float, ncPauseInMs ∷ Float, ncLimiterKnee ∷ Float
  } deriving (Eq, Show)

-- | Defaults selected in audio_system_design.md D-49.
defaultNativeConfig ∷ NativeConfig
defaultNativeConfig = NativeConfig 48000 256 1024 4096 128 256 256 2 1000 30000 20 10 5 10 0.95

instance Storable NativeConfig where
  sizeOf _ = #{size syn_audio_config}
  alignment _ = #{alignment syn_audio_config}
  peek p = NativeConfig
    <$> #{peek syn_audio_config, sample_rate} p
    <*> #{peek syn_audio_config, chunk_frames} p
    <*> #{peek syn_audio_config, target_fill_frames} p
    <*> #{peek syn_audio_config, ring_capacity_frames} p
    <*> #{peek syn_audio_config, max_voices} p
    <*> #{peek syn_audio_config, command_batch_limit} p
    <*> #{peek syn_audio_config, period_frames} p
    <*> #{peek syn_audio_config, periods} p
    <*> #{peek syn_audio_config, retry_initial_ms} p
    <*> #{peek syn_audio_config, retry_max_ms} p
    <*> #{peek syn_audio_config, bus_gain_ms} p
    <*> #{peek syn_audio_config, instance_gain_ms} p
    <*> #{peek syn_audio_config, pause_out_ms} p
    <*> #{peek syn_audio_config, pause_in_ms} p
    <*> #{peek syn_audio_config, limiter_knee} p
  poke p c = do
    #{poke syn_audio_config, abi_version} p (#{const SYN_AUDIO_ABI} ∷ Word32)
    #{poke syn_audio_config, struct_size} p (#{size syn_audio_config} ∷ Word32)
    #{poke syn_audio_config, sample_rate} p (ncSampleRate c)
    #{poke syn_audio_config, chunk_frames} p (ncChunkFrames c)
    #{poke syn_audio_config, target_fill_frames} p (ncTargetFillFrames c)
    #{poke syn_audio_config, ring_capacity_frames} p (ncRingCapacityFrames c)
    #{poke syn_audio_config, max_voices} p (ncMaxVoices c)
    #{poke syn_audio_config, command_batch_limit} p (ncCommandBatchLimit c)
    #{poke syn_audio_config, period_frames} p (ncPeriodFrames c)
    #{poke syn_audio_config, periods} p (ncPeriods c)
    #{poke syn_audio_config, retry_initial_ms} p (ncRetryInitialMs c)
    #{poke syn_audio_config, retry_max_ms} p (ncRetryMaxMs c)
    #{poke syn_audio_config, bus_gain_ms} p (ncBusGainMs c)
    #{poke syn_audio_config, instance_gain_ms} p (ncInstanceGainMs c)
    #{poke syn_audio_config, pause_out_ms} p (ncPauseOutMs c)
    #{poke syn_audio_config, pause_in_ms} p (ncPauseInMs c)
    #{poke syn_audio_config, limiter_knee} p (ncLimiterKnee c)

-- | A copied snapshot. Native pointers never enter public engine telemetry.
data NativeStatus = NativeStatus
  { nsLifecycle ∷ Word32, nsSink ∷ Word32, nsBackend ∷ Word32
  , nsSampleRate ∷ Word32, nsPeriodFrames ∷ Word32
  , nsRingFill ∷ Word32, nsRingMin ∷ Word32, nsRingMax ∷ Word32
  , nsActiveVoices ∷ Word32, nsPeakVoices ∷ Word32, nsActiveLoops ∷ Word32, nsPeakLoops ∷ Word32
  , nsSampleCount ∷ Word32, nsInstrumentCount ∷ Word32, nsSoundCount ∷ Word32
  , nsRenderedFrames ∷ Word64, nsCallbackFrames ∷ Word64
  , nsCallbacks ∷ Word64, nsUnderruns ∷ Word64
  , nsAccepted ∷ Word64, nsDropped ∷ Word64, nsSteals ∷ Word64, nsMissingLoops ∷ Word64
  , nsDecodedFrames ∷ Word64, nsDecodedBytes ∷ Word64
  , nsLimitedSamples ∷ Word64, nsNonfiniteSamples ∷ Word64
  , nsTransitions ∷ Word64, nsServiceNs ∷ Word64, nsMixPeak ∷ Float
  , nsDeviceName ∷ Text, nsLastError ∷ Text
  } deriving (Eq, Show)

peekStatus ∷ Ptr () → IO NativeStatus
peekStatus p = NativeStatus
  <$> #{peek syn_audio_status, lifecycle} p
  <*> #{peek syn_audio_status, sink} p
  <*> #{peek syn_audio_status, backend} p
  <*> #{peek syn_audio_status, sample_rate} p
  <*> #{peek syn_audio_status, period_frames} p
  <*> #{peek syn_audio_status, ring_fill} p
  <*> #{peek syn_audio_status, ring_min} p
  <*> #{peek syn_audio_status, ring_max} p
  <*> #{peek syn_audio_status, active_voices} p
  <*> #{peek syn_audio_status, peak_voices} p
  <*> #{peek syn_audio_status, active_loops} p
  <*> #{peek syn_audio_status, peak_loops} p
  <*> #{peek syn_audio_status, sample_count} p
  <*> #{peek syn_audio_status, instrument_count} p
  <*> #{peek syn_audio_status, sound_count} p
  <*> #{peek syn_audio_status, rendered_frames} p
  <*> #{peek syn_audio_status, callback_frames} p
  <*> #{peek syn_audio_status, callbacks} p
  <*> #{peek syn_audio_status, underruns} p
  <*> #{peek syn_audio_status, accepted} p
  <*> #{peek syn_audio_status, dropped} p
  <*> #{peek syn_audio_status, steals} p
  <*> #{peek syn_audio_status, missing_loops} p
  <*> #{peek syn_audio_status, decoded_frames} p
  <*> #{peek syn_audio_status, decoded_bytes} p
  <*> #{peek syn_audio_status, limited_samples} p
  <*> #{peek syn_audio_status, nonfinite_samples} p
  <*> #{peek syn_audio_status, transitions} p
  <*> #{peek syn_audio_status, service_ns} p
  <*> #{peek syn_audio_status, mix_peak} p
  <*> peekUtf8 128 (#{ptr syn_audio_status, device_name} p)
  <*> peekUtf8 256 (#{ptr syn_audio_status, last_error} p)

peekUtf8 ∷ Int → Ptr CChar → IO Text
peekUtf8 bound p = Text.decodeUtf8With lenientDecode ∘ BS.takeWhile (≢ 0)
  <$> BS.packCStringLen (p, bound)

nativeAbiSizes ∷ (Int, Int)
nativeAbiSizes = (#{size syn_audio_config}, #{size syn_audio_status})

-- | The action is the complete worker lifetime. Cleanup also runs on exceptions;
-- no finalizer can race the worker or leave a device alive until the next GC.
withNative ∷ NativeConfig → Sink → (Native → IO α) → IO (Either Text α)
withNative config sink action = mask $ \restore → alloca $ \cfg → alloca $ \out → do
  poke cfg config
  poke out nullPtr
  result ← c_create cfg (case sink of ForcedNull → 0; RealWithNullFallback → 1) out
  if result ≢ 0 then pure (Left ("audio initialization failed: " <> tshow result))
  else do
    ptr ← peek out
    Right <$> restore (action (Native ptr)) `finally` c_destroy ptr

nativeResult ∷ Ptr Core → Word32 → IO (Either Text ())
nativeResult ptr result
  | result ≡ 0 = pure (Right ())
  | otherwise = do
      -- Only these paths always supply a fresh native diagnostic. Validation
      -- and capacity failures must not inherit an earlier decoder/device error.
      detail ← if result `elem` [3, 5] then c_error ptr ⌦ peekUtf8 256 else pure ""
      let message = case result of
            1 → "invalid argument or ABI layout"; 2 → "allocation failed"
            3 → "device failed"; 4 → "operation is invalid in the current lifecycle"
            5 → "sample decode failed"; 6 → "configured limit exceeded"
            7 → "sample or handle is missing"; _ → "command rejected"
      pure $ Left ("audio error " <> tshow result <> ": " <> message
        <> if Text.null detail then "" else ": " <> detail)

startNative ∷ Native → IO (Either Text ())
startNative (Native ptr) = c_start ptr ⌦ nativeResult ptr

stopNative ∷ Native → IO ()
stopNative (Native ptr) = c_stop ptr

serviceNative ∷ Native → IO (Either Text NativeStatus)
serviceNative native = fmap snd <$> serviceCommandsNative native []

serviceCommandsNative ∷ Native → [NativeCommand] → IO (Either Text ([CommandResult], NativeStatus))
serviceCommandsNative (Native ptr) commands
  | length commands > 1024 = pure (Left "native command batch exceeds ABI limit")
  | otherwise = withArrayLen commands $ \count batch → allocaArray count $ \results →
      allocaBytes #{size syn_audio_status} $ \status → do
        applied ← c_service ptr batch (fromIntegral count) results status ⌦ nativeResult ptr
        case applied of
          Left err → pure (Left err)
          Right () → Right <$> ((,) <$> peekArray count results <*> peekStatus status)

submitOffline ∷ Native → [NativeCommand] → IO (Either Text [CommandResult])
submitOffline (Native ptr) commands
  | length commands > 1024 = pure (Left "native command batch exceeds ABI limit")
  | otherwise = withArrayLen commands $ \count batch → allocaArray count $ \results → do
      applied ← c_submit ptr batch (fromIntegral count) results ⌦ nativeResult ptr
      case applied of
        Left err → pure (Left err)
        Right () → Right <$> peekArray count results

loadSampleNative ∷ Native → FilePath → DecodeLimits → IO (Either Text Word32)
loadSampleNative (Native ptr) path limits
  | '\0' `elem` path = pure (Left "sample path contains NUL")
  | otherwise = BS.useAsCString (Text.encodeUtf8 $ Text.pack path) $ \name →
      with limits $ \budget → handleResult ptr (c_load ptr name budget)

addInstrumentNative ∷ Native → NativeInstrument → IO (Either Text Word32)
addInstrumentNative (Native ptr) instrument = with instrument $ \desc → handleResult ptr (c_instrument ptr desc)

addSoundNative ∷ Native → NativeSound → IO (Either Text Word32)
addSoundNative (Native ptr) sound = with sound $ \desc → handleResult ptr (c_sound ptr desc)

handleResult ∷ Ptr Core → (Ptr Word32 → IO Word32) → IO (Either Text Word32)
handleResult ptr action = alloca $ \out → do
  poke out 0
  result ← action out ⌦ nativeResult ptr
  case result of
    Left err → pure (Left err)
    Right () → Right <$> peek out

readNativeStatus ∷ Native → IO NativeStatus
readNativeStatus (Native ptr) = allocaBytes #{size syn_audio_status} $ \out →
  c_status ptr out >> peekStatus out

renderOffline ∷ Native → Int → IO (Either Text [Float])
renderOffline (Native ptr) frames
  | frames < 0 ∨ frames > 1048576 = pure (Left "offline frame count out of range")
  | otherwise = allocaArray (frames * 2) $ \out → do
      result ← c_offline ptr out (fromIntegral frames) nullPtr ⌦ nativeResult ptr
      case result of
        Left err → pure (Left err)
        Right () → Right <$> peekArray (frames * 2) out

foreign import ccall safe "syn_audio_create" c_create
  ∷ Ptr NativeConfig → Word32 → Ptr (Ptr Core) → IO Word32
foreign import ccall safe "syn_audio_start" c_start ∷ Ptr Core → IO Word32
foreign import ccall safe "syn_audio_stop" c_stop ∷ Ptr Core → IO ()
foreign import ccall safe "syn_audio_destroy" c_destroy ∷ Ptr Core → IO ()
foreign import ccall safe "syn_audio_service" c_service
  ∷ Ptr Core → Ptr NativeCommand → Word32 → Ptr CommandResult → Ptr () → IO Word32
foreign import ccall safe "syn_audio_submit_offline" c_submit
  ∷ Ptr Core → Ptr NativeCommand → Word32 → Ptr CommandResult → IO Word32
foreign import ccall safe "syn_audio_load_sample" c_load
  ∷ Ptr Core → Ptr CChar → Ptr DecodeLimits → Ptr Word32 → IO Word32
foreign import ccall safe "syn_audio_add_instrument" c_instrument
  ∷ Ptr Core → Ptr NativeInstrument → Ptr Word32 → IO Word32
foreign import ccall safe "syn_audio_add_sound" c_sound
  ∷ Ptr Core → Ptr NativeSound → Ptr Word32 → IO Word32
foreign import ccall unsafe "syn_audio_get_status" c_status ∷ Ptr Core → Ptr () → IO ()
foreign import ccall safe "syn_audio_render_offline" c_offline
  ∷ Ptr Core → Ptr Float → Word32 → Ptr () → IO Word32
foreign import ccall unsafe "syn_audio_last_error" c_error ∷ Ptr Core → IO (Ptr CChar)
