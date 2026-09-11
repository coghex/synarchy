-- | Parse one complete policy/timbre and strict partial overrides.
module Engine.Audio.Catalog.Yaml
  ( parsePolicy, parseTimbre, parseIdentifier, parseAssetPath, withPatch ) where

import UPrelude
import Data.Aeson (Object, Value(..), (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import qualified Data.Text as Text
import qualified System.FilePath.Posix as Path
import Engine.Audio.Catalog.Schema
import Engine.Audio.Catalog.Types
import Engine.Audio.Config.Parse
import Engine.Audio.Config.Runtime (RuntimeConfig(..))
import Engine.Audio.Native (NativeConfig(..))

parseIdentifier ∷ Text → Parser Text
parseIdentifier value = do
  let asciiLower c = c ≥ 'a' ∧ c ≤ 'z'
      rest c = asciiLower c ∨ (c ≥ '0' ∧ c ≤ '9') ∨ c ≡ '_'
  unless (Text.length value ≤ 64 ∧ case Text.uncons value of
    Just (c, suffix) → asciiLower c ∧ Text.all rest suffix
    Nothing → False) $ fail "audio identifier must match [a-z][a-z0-9_]{0,63}"
  pure value

parseAssetPath ∷ FilePath → Parser FilePath
parseAssetPath path = do
  unless (not (Path.isAbsolute path) ∧ "assets/audio/" `Text.isPrefixOf` Text.pack path
    ∧ Path.normalise path ≡ path ∧ ".." `notElem` Path.splitDirectories path
    ∧ '\\' `notElem` path ∧ '\0' `notElem` path
    ∧ map toLower (Path.takeExtension path) `elem` [".wav", ".flac", ".mp3"]) $
    fail "audio sample path must be normalized under assets/audio/ and end in .wav/.flac/.mp3"
  pure path

withPatch ∷ Schema → Object → Text → Value → Parser Object
withPatch schema container key fallback = do
  base ← validatePatch schema fallback
  patch ← validatePatch schema (fromMaybe (Object KM.empty) $ KM.lookup (Key.fromText key) container)
  pure $ mergeObjects patch base

parsePolicy ∷ RuntimeConfig → Object → Parser Policy
parsePolicy config p = do
  spatial ← fieldObject p "spatial"
    ["mode", "min_distance_tiles", "max_distance_tiles", "rolloff", "vertical_scale"]
  concurrency ← fieldObject p "concurrency" ["max_instances", "overflow", "cooldown_ms"]
  loop ← fieldObject p "loop" ["allowed", "stop_fade_ms"]
  void $ enumValue spatial "rolloff" [("linear", ())]
  result ← Policy
    <$> enumValue p "bus" [("world", WorldBus), ("ui", UIBus)]
    <*> enumValue spatial "mode" [("world", True), ("non_spatial", False)]
    <*> finiteRange spatial "min_distance_tiles" 0 1024
    <*> finiteRange spatial "max_distance_tiles" 0 4096
    <*> finiteRange spatial "vertical_scale" 0 8
    <*> finiteRange p "gain_db" (-96) 24
    <*> integerRange p "priority" 0 100
    <*> integerRange concurrency "max_instances" 1 (ncMaxVoices $ rcNative config)
    <*> enumValue concurrency "overflow" [("drop_new", DropNew), ("steal_oldest", StealOldest)]
    <*> finiteRange concurrency "cooldown_ms" 0 60000
    <*> loop .: "allowed"
    <*> finiteRange loop "stop_fade_ms" 0 5000
    <*> enumValue p "player_pause" [("freeze", True), ("continue", False)]
  unless (policyMaxDistance result > policyMinDistance result) $
    fail "max_distance_tiles must exceed min_distance_tiles"
  when (policyBus result ≡ UIBus ∧ (policySpatial result ∨ policyFreeze result)) $
    fail "UI sounds must be non_spatial and player_pause: continue"
  pure result

parseTimbre ∷ RuntimeConfig → Object → Parser Timbre
parseTimbre config t = do
  generator ← fieldObject t "generator" ["waveform", "frequency_hz", "start_phase", "noise_seed"]
  envelope ← fieldObject t "envelope" ["attack_ms", "decay_ms", "sustain_level", "release_ms"]
  filt ← fieldObject t "filter" ["mode", "cutoff_hz", "resonance_q"]
  let limit = 0.45 * fromIntegral (ncSampleRate $ rcNative config)
  Timbre
    <$> enumValue generator "waveform" [("sine", Sine), ("saw", Saw), ("square", Square),
      ("triangle", Triangle), ("white_noise", WhiteNoise)]
    <*> finiteRange generator "frequency_hz" 20 (min 20000 limit)
    <*> enumValue generator "start_phase" [("reset", False), ("random", True)]
    <*> integerRange generator "noise_seed" 0 maxBound
    <*> finiteRange envelope "attack_ms" 0 30000
    <*> finiteRange envelope "decay_ms" 0 30000
    <*> finiteRange envelope "sustain_level" 0 1
    <*> finiteRange envelope "release_ms" 0 30000
    <*> enumValue filt "mode" [("bypass", Bypass), ("low_pass", LowPass), ("high_pass", HighPass), ("band_pass", BandPass)]
    <*> finiteRange filt "cutoff_hz" 20 limit
    <*> finiteRange filt "resonance_q" 0.1 20
    <*> finiteRange t "gain_db" (-96) 24
    <*> finiteRange t "default_gate_ms" 0 60000
