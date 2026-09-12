-- | Per-entry isolation, duplicate refusal, and deterministic dependency closure.
module Engine.Audio.Catalog.Resolve (resolveCatalog, loadCatalog) where

import UPrelude
import Data.Aeson (Value(..), Object, (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Bifunctor (first)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import System.FilePath (takeExtension)
import Engine.Audio.Catalog.Schema
import Engine.Audio.Catalog.Types
import Engine.Audio.Catalog.Yaml
import Engine.Audio.Config.Parse
import Engine.Audio.Config.Runtime (RuntimeConfig)

-- Kept with parsed parents so overrides extend the original resolved object.
type Parent α = (α, Object)

entries ∷ Text → Value → Either String [(Int, Value)]
entries family = parseEither $ \value → do
  top ← strictObject "audio catalog" ["schema_version", family] value
  version ← top .: "schema_version" ∷ Parser Int
  unless (version ≡ 1) $ fail "unsupported audio schema_version"
  items ← top .: Key.fromText family ∷ Parser [Value]
  pure $ zip [1..] items

entryId ∷ Value → Maybe Text
entryId (Object o) = parseMaybe (.: "id") o
entryId _ = Nothing

data Family α = Family
  { familyValid ∷ Map.Map Text α, familyReasons ∷ Map.Map Text Text
  , familyFileError ∷ Maybe Text, familyWarnings ∷ [Text], familyDisabled ∷ Int }

parseFamily ∷ Text → (Value → Parser (Text, α)) → Either Text Value → Family α
parseFamily family parser input = case input ⌦ first Text.pack ∘ entries family of
  Left err → Family Map.empty Map.empty (Just $ path <> ": " <> err) [path <> ": " <> err] 0
  Right items →
    let counts = Map.fromListWith (+) [(name, 1 ∷ Int) | (_, item) ← items, Just name ← [entryId item]]
        step (valid, reasons, warnings) (index, item) =
          let name = fromMaybe "<missing id>" (entryId item)
              origin = path <> "[" <> tshow index <> "] " <> name <> ": "
              parsed = if Map.findWithDefault 0 name counts > 1
                then Left "duplicate ID; every definition of this ID is disabled"
                else parseEither parser item
          in case parsed of
            Left err → let reason = origin <> Text.pack err
              in (valid, Map.insert name reason reasons, reason : warnings)
            Right (key, result) → (Map.insert key result valid, reasons, warnings)
        (valid, reasons, warnings) = foldl step (Map.empty, Map.empty, []) items
    in Family valid reasons Nothing (reverse warnings) (length items - Map.size valid)
  where path = "data/audio/" <> family <> ".yaml"

parseType ∷ RuntimeConfig → Value → Parser (Text, Parent Policy)
parseType config value = do
  o ← strictObject "sound type" ["id", "policy"] value
  name ← o .: "id" ⌦ parseIdentifier
  patch ← withPatch policySchema o "policy" defaultPolicyValue
  policy ← parsePolicy config patch
  pure (name, (policy, patch))

parseInstrument ∷ RuntimeConfig → Value → Parser (Text, Parent Timbre)
parseInstrument config value = do
  o ← strictObject "instrument" ["id", "timbre"] value
  name ← o .: "id" ⌦ parseIdentifier
  patch ← withPatch timbreSchema o "timbre" defaultTimbreValue
  timbre ← parseTimbre config patch
  pure (name, (timbre, patch))

dependency ∷ Text → Text → Family α → Parser α
dependency kind name family = case Map.lookup name (familyValid family) of
  Just value → pure value
  Nothing → fail $ Text.unpack $ kind <> " " <> name <> " → "
    <> fromMaybe (fromMaybe "definition is missing" $ familyFileError family)
         (Map.lookup name $ familyReasons family)

parseSound ∷ RuntimeConfig → Family (Parent Policy) → Family (Parent Timbre)
  → Value → Parser (Text, Sound)
parseSound config types instruments value = do
  o ← strictObject "sound" ["id", "type", "source", "policy"] value
  name ← o .: "id" ⌦ parseIdentifier
  typeName ← o .: "type" ⌦ parseIdentifier
  (_, basePolicy) ← dependency "sound type" typeName types
  policy ← withPatch policySchema o "policy" (Object basePolicy) ⌦ parsePolicy config
  sources ← fieldObject o "source" ["sample", "synth"]
  source ← case KM.toList sources of
    [("sample", sample)] → do
      fields ← strictObject "sample" ["path"] sample
      path ← fields .: "path" ⌦ parseAssetPath
      when (map toLower (takeExtension path) ≡ ".mp3" ∧ policyAllowLoop policy) $
        fail "MP3 sources cannot allow loops"
      pure (SampleSource path)
    [("synth", synth)] → do
      fields ← strictObject "synth" ["instrument", "gate_ms", "timbre"] synth
      instrumentName ← fields .: "instrument" ⌦ parseIdentifier
      (_, baseTimbre) ← dependency "instrument" instrumentName instruments
      timbre ← withPatch timbreSchema fields "timbre" (Object baseTimbre) ⌦ parseTimbre config
      gate ← if KM.member "gate_ms" fields then finiteRange fields "gate_ms" 0 60000
             else pure (timbreGateMs timbre)
      pure (SynthSource $ timbre { timbreGateMs = gate })
    _ → fail "sound source requires exactly one sample or synth branch"
  pure (name, Sound name policy source)

resolveCatalog ∷ RuntimeConfig → Value → Value → Value → Catalog
resolveCatalog config typeValues instrumentValues soundValues =
  resolveFamilies config (Right typeValues) (Right instrumentValues) (Right soundValues)

resolveFamilies ∷ RuntimeConfig → Either Text Value → Either Text Value → Either Text Value → Catalog
resolveFamilies config typeValues instrumentValues soundValues =
  let types = parseFamily "sound_types" (parseType config) typeValues
      instruments = parseFamily "instruments" (parseInstrument config) instrumentValues
      sounds = parseFamily "sounds" (parseSound config types instruments) soundValues
  in Catalog (fst <$> familyValid types) (fst <$> familyValid instruments) (familyValid sounds)
       (familyWarnings types <> familyWarnings instruments <> familyWarnings sounds)
       (Map.fromList [("types", counts types), ("instruments", counts instruments), ("sounds", counts sounds)])
  where counts family = (Map.size $ familyValid family, familyDisabled family)

loadCatalog ∷ RuntimeConfig → IO Catalog
loadCatalog config = do
  let readFamily family = first tshow <$> Yaml.decodeFileEither ("data/audio/" <> family <> ".yaml")
  types ← readFamily "sound_types"
  instruments ← readFamily "instruments"
  sounds ← readFamily "sounds"
  pure $ resolveFamilies config types instruments sounds
