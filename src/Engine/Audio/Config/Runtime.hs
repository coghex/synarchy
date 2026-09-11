-- | Strict, atomic engine tuning. Player Settings never writes this family.
module Engine.Audio.Config.Runtime
  ( RuntimeConfig(..), defaultRuntimeConfig, parseRuntimeConfig, loadRuntimeConfig ) where

import UPrelude
import Data.Aeson (Value, (.:))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Engine.Audio.Config.Parse
import Engine.Audio.Native (NativeConfig(..), defaultNativeConfig)

data RuntimeConfig = RuntimeConfig
  { rcNative ∷ NativeConfig
  , rcWorkerIdleUs ∷ Int, rcEventCapacity ∷ Int, rcControlBacklogWarn ∷ Int
  , rcCloseRangeScale ∷ Float, rcFarRangeScale ∷ Float
  , rcCloseGainDb ∷ Float, rcFarGainDb ∷ Float
  , rcMaxEncodedMiB ∷ Word32, rcMaxAssetSeconds ∷ Float
  , rcMaxTotalSeconds ∷ Float, rcMaxDecodedMiB ∷ Word32
  , rcVolumeExponent ∷ Float, rcPublishHz ∷ Float
  , rcUnderrunsPerMinuteWarn ∷ Int, rcServiceBudgetFractionWarn ∷ Float
  , rcRateLimitSeconds ∷ Float
  } deriving (Eq, Show)

defaultRuntimeConfig ∷ RuntimeConfig
defaultRuntimeConfig = RuntimeConfig defaultNativeConfig 1000 2048 64
  0.85 1.50 1.5 (-2) 16 15 120 64 2 10 3 0.50 10

parseRuntimeConfig ∷ Value → Parser RuntimeConfig
parseRuntimeConfig value = do
  top ← strictObject "audio runtime" ["schema_version", "audio_runtime"] value
  version ← top .: "schema_version" ∷ Parser Int
  unless (version ≡ 1) $ fail "audio runtime: unsupported schema_version"
  root ← fieldObject top "audio_runtime"
    ["mix", "transport", "device", "smoothing", "spatial", "limiter", "assets", "player_curve", "telemetry"]
  mix ← fieldObject root "mix" ["sample_rate_hz", "render_chunk_frames", "target_fill_frames",
    "ring_capacity_frames", "max_voices", "command_batch_limit", "worker_idle_wait_us"]
  transport ← fieldObject root "transport" ["event_capacity", "control_backlog_warn"]
  device ← fieldObject root "device" ["period_frames", "periods", "retry_initial_ms", "retry_max_ms"]
  smooth ← fieldObject root "smoothing" ["bus_gain_ms", "instance_gain_ms", "pause_out_ms", "pause_in_ms"]
  spatial ← fieldObject root "spatial" ["close_range_scale", "far_range_scale", "close_gain_db", "far_gain_db"]
  limiter ← fieldObject root "limiter" ["knee"]
  assets ← fieldObject root "assets" ["max_encoded_mib", "max_asset_seconds", "max_total_seconds", "max_decoded_mib"]
  curve ← fieldObject root "player_curve" ["exponent"]
  telemetry ← fieldObject root "telemetry"
    ["publish_hz", "underruns_per_minute_warn", "service_budget_fraction_warn", "rate_limit_seconds"]
  native ← NativeConfig
    <$> integerRange mix "sample_rate_hz" 22050 96000
    <*> integerRange mix "render_chunk_frames" 64 1024
    <*> integerRange mix "target_fill_frames" 128 1048576
    <*> integerRange mix "ring_capacity_frames" 256 2097152
    <*> integerRange mix "max_voices" 16 1024
    <*> integerRange mix "command_batch_limit" 16 1024
    <*> integerRange device "period_frames" 0 1024
    <*> integerRange device "periods" 0 4
    <*> integerRange device "retry_initial_ms" 1 300000
    <*> integerRange device "retry_max_ms" 1 300000
    <*> finiteRange smooth "bus_gain_ms" 0.001 10000
    <*> finiteRange smooth "instance_gain_ms" 0.001 10000
    <*> finiteRange smooth "pause_out_ms" 0.001 10000
    <*> finiteRange smooth "pause_in_ms" 0.001 10000
    <*> finiteRange limiter "knee" 0.5 1
  let chunk = ncChunkFrames native
      target = ncTargetFillFrames native
      ring = ncRingCapacityFrames native
  unless (chunk ⌃ (chunk - 1) ≡ 0 ∧ target ≥ 2 * chunk
    ∧ target `mod` chunk ≡ 0 ∧ ring ≥ target + 2 * chunk ∧ ring `mod` chunk ≡ 0) $
    fail "audio runtime: invalid chunk/target/ring relationship"
  unless ((ncPeriodFrames native ≡ 0 ∨ ncPeriodFrames native ≥ 64)
    ∧ (ncPeriods native ≡ 0 ∨ ncPeriods native ≥ 2)
    ∧ ncRetryInitialMs native ≤ ncRetryMaxMs native ∧ ncLimiterKnee native < 1) $
    fail "audio runtime: invalid device retry/period or limiter relationship"
  result ← RuntimeConfig native
    <$> integerRange mix "worker_idle_wait_us" 250 5000
    <*> integerRange transport "event_capacity" 128 65536
    <*> integerRange transport "control_backlog_warn" 1 65536
    <*> finiteRange spatial "close_range_scale" 0.001 100
    <*> finiteRange spatial "far_range_scale" 0.001 100
    <*> finiteRange spatial "close_gain_db" (-96) 24
    <*> finiteRange spatial "far_gain_db" (-96) 24
    <*> integerRange assets "max_encoded_mib" 1 1024
    <*> finiteRange assets "max_asset_seconds" 0.001 3600
    <*> finiteRange assets "max_total_seconds" 0.001 86400
    <*> integerRange assets "max_decoded_mib" 1 4096
    <*> finiteRange curve "exponent" 1 4
    <*> finiteRange telemetry "publish_hz" 0.1 100
    <*> integerRange telemetry "underruns_per_minute_warn" 1 1000000
    <*> finiteRange telemetry "service_budget_fraction_warn" 0.001 100
    <*> finiteRange telemetry "rate_limit_seconds" 0.001 3600
  unless (rcCloseRangeScale result ≤ rcFarRangeScale result) $
    fail "audio runtime: close range exceeds far range"
  pure result

loadRuntimeConfig ∷ IO (RuntimeConfig, [Text])
loadRuntimeConfig = do
  decoded ← Yaml.decodeFileEither "config/audio_runtime.yaml"
  let parsed = case decoded of
        Left err → Left (show err)
        Right value → parseEither parseRuntimeConfig value
  pure $ case parsed of
    Left err → (defaultRuntimeConfig, ["audio runtime: using compiled defaults: " <> Text.pack err])
    Right config → (config, [])
