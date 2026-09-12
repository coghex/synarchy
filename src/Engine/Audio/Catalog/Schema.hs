-- | Strict recursive patches and complete compiled authoring defaults.
module Engine.Audio.Catalog.Schema
  ( Schema(..), policySchema, timbreSchema, validatePatch, mergeObjects
  , defaultPolicyValue, defaultTimbreValue
  ) where

import UPrelude
import Data.Aeson (Value(..), Object, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Engine.Audio.Config.Parse (strictObject)

data Schema = Leaf | Fields [(Text, Schema)]

fields ∷ [Text] → Schema
fields = Fields ∘ map (, Leaf)

policySchema, timbreSchema ∷ Schema
policySchema = Fields
  [ ("bus", Leaf), ("gain_db", Leaf), ("priority", Leaf), ("player_pause", Leaf)
  , ("spatial", fields ["mode", "min_distance_tiles", "max_distance_tiles", "rolloff", "vertical_scale"])
  , ("concurrency", fields ["max_instances", "overflow", "cooldown_ms"])
  , ("loop", fields ["allowed", "stop_fade_ms"])
  ]
timbreSchema = Fields
  [ ("generator", fields ["waveform", "frequency_hz", "start_phase", "noise_seed"])
  , ("envelope", fields ["attack_ms", "decay_ms", "sustain_level", "release_ms"])
  , ("filter", fields ["mode", "cutoff_hz", "resonance_q"])
  , ("gain_db", Leaf), ("default_gate_ms", Leaf)
  ]

validatePatch ∷ Schema → Value → Parser Object
validatePatch (Fields schema) value = do
  o ← strictObject "audio patch" (map fst schema) value
  forM_ (KM.toList o) $ \(key, child) → do
    when (child ≡ Null) $ fail ("explicit null is not an audio override: " <> show key)
    case lookup (Key.toText key) schema of
      Just nested@(Fields _) → void (validatePatch nested child)
      _ → pure ()
  pure o
validatePatch Leaf _ = fail "internal audio schema expected an object"

mergeObjects ∷ Object → Object → Object
mergeObjects = KM.unionWith combine
  where
    -- Left is the override. A leaf replaces; a mapping recursively merges.
    combine (Object newer) (Object older) = Object (mergeObjects newer older)
    combine newer _ = newer

defaultPolicyValue ∷ Value
defaultPolicyValue = object
  [ "bus" .= ("world" ∷ Text), "gain_db" .= (0 ∷ Int), "priority" .= (50 ∷ Int)
  , "player_pause" .= ("freeze" ∷ Text)
  , "spatial" .= object ["mode" .= ("world" ∷ Text), "min_distance_tiles" .= (1 ∷ Int)
      , "max_distance_tiles" .= (40 ∷ Int), "rolloff" .= ("linear" ∷ Text), "vertical_scale" .= (1 ∷ Int)]
  , "concurrency" .= object ["max_instances" .= (8 ∷ Int), "overflow" .= ("steal_oldest" ∷ Text)
      , "cooldown_ms" .= (0 ∷ Int)]
  , "loop" .= object ["allowed" .= False, "stop_fade_ms" .= (20 ∷ Int)]
  ]

defaultTimbreValue ∷ Value
defaultTimbreValue = object
  [ "generator" .= object ["waveform" .= ("sine" ∷ Text), "frequency_hz" .= (440 ∷ Int)
      , "start_phase" .= ("reset" ∷ Text), "noise_seed" .= (1831565813 ∷ Word32)]
  , "envelope" .= object ["attack_ms" .= (2 ∷ Int), "decay_ms" .= (20 ∷ Int)
      , "sustain_level" .= (0.5 ∷ Float), "release_ms" .= (35 ∷ Int)]
  , "filter" .= object ["mode" .= ("bypass" ∷ Text), "cutoff_hz" .= (1000 ∷ Int)
      , "resonance_q" .= (0.707 ∷ Float)]
  , "gain_db" .= (0 ∷ Int), "default_gate_ms" .= (45 ∷ Int)
  ]
