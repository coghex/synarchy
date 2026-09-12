module Test.Headless.Audio.Catalog (spec) where

import UPrelude
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import Engine.Audio.Catalog.Resolve
import Engine.Audio.Catalog.Types
import Engine.Audio.Config.Runtime (defaultRuntimeConfig)
import Test.Hspec

family ∷ Text → [Value] → Value
family name values = object [Key.fromText name .= values, "schema_version" .= (1 ∷ Int)]

resolve ∷ [Value] → [Value] → [Value] → Catalog
resolve types instruments sounds = resolveCatalog defaultRuntimeConfig
  (family "sound_types" types) (family "instruments" instruments) (family "sounds" sounds)

kind ∷ Text → Value
kind name = object ["id" .= name]

sample ∷ Text → Text → Text → Value
sample name typeName path = object ["id" .= name, "type" .= typeName,
  "source" .= object ["sample" .= object ["path" .= path]]]

spec ∷ Spec
spec = describe "Audio.Catalog" $ do
  it "accepts the complete authoring-guide examples with the real catalog parser" $ do
    guide ← TextIO.readFile "docs/audio_authoring.md"
    let blocks [] = []
        blocks ("```yaml":rest) = let (body, remaining) = break (≡ "```") rest
          in Text.unlines body : blocks (drop 1 remaining)
        blocks (_:rest) = blocks rest
    values ← mapM (either (fail ∘ show) pure ∘ Yaml.decodeEither' ∘ Text.encodeUtf8) (blocks $ Text.lines guide)
    case values of
      [types, instruments, sounds] → do
        let catalog = resolveCatalog defaultRuntimeConfig types instruments sounds
        catalogWarnings catalog `shouldBe` []
        Map.keys (catalogSounds catalog) `shouldBe` ["ui_ping"]
      _ → expectationFailure "authoring guide must contain three complete YAML catalog examples"

  it "disables every duplicate parent and its dependents while retaining a valid sibling" $ do
    let catalog = resolve [kind "duplicate", kind "duplicate", kind "good"] []
          [sample "bad" "duplicate" "assets/audio/a.wav", sample "good" "good" "assets/audio/b.wav"]
    Map.keys (catalogTypes catalog) `shouldBe` ["good"]
    Map.keys (catalogSounds catalog) `shouldBe` ["good"]
    length (catalogWarnings catalog) `shouldBe` 3
    Map.lookup "types" (catalogCounts catalog) `shouldBe` Just (1, 2)
    Map.lookup "sounds" (catalogCounts catalog) `shouldBe` Just (1, 1)

  it "merges nested sound overrides without losing other inherited leaves" $ do
    let typeValue = object ["id" .= ("world" ∷ Text), "policy" .= object
          ["concurrency" .= object ["max_instances" .= (2 ∷ Int), "cooldown_ms" .= (750 ∷ Int)]]]
        soundValue = object ["id" .= ("sample" ∷ Text), "type" .= ("world" ∷ Text),
          "policy" .= object ["concurrency" .= object ["max_instances" .= (3 ∷ Int)]],
          "source" .= object ["sample" .= object ["path" .= ("assets/audio/a.wav" ∷ Text)]]]
        catalog = resolve [typeValue] [] [soundValue]
    catalogWarnings catalog `shouldBe` []
    case Map.lookup "sample" (catalogSounds catalog) of
      Nothing → expectationFailure "valid sample disabled"
      Just sound → do
        policyMaxInstances (soundPolicy sound) `shouldBe` 3
        policyCooldownMs (soundPolicy sound) `shouldBe` 750

  it "does not let an invalid synth instrument suppress unrelated sample sources" $ do
    let badInstrument = object ["id" .= ("bad" ∷ Text), "timbre" .= object
          ["filter" .= object ["cutoff_hz" .= (100000 ∷ Int)]]]
        badSound = object ["id" .= ("synth" ∷ Text), "type" .= ("world" ∷ Text),
          "source" .= object ["synth" .= object ["instrument" .= ("bad" ∷ Text)]]]
        catalog = resolve [kind "world"] [badInstrument]
          [badSound, sample "sample" "world" "assets/audio/a.wav"]
    Map.keys (catalogSounds catalog) `shouldBe` ["sample"]
    length (catalogWarnings catalog) `shouldBe` 2
    last (catalogWarnings catalog) `shouldSatisfy` (\warning →
      Text.isInfixOf "synth" warning ∧ Text.isInfixOf "instrument bad" warning
      ∧ Text.isInfixOf "cutoff_hz" warning ∧ Text.isInfixOf "instruments.yaml[1]" warning)

  it "rejects explicit null and unknown nested keys, retaining unrelated definitions" $ do
    let badType = object ["id" .= ("null_policy" ∷ Text), "policy" .= Null]
        typo = object ["id" .= ("typo" ∷ Text), "policy" .= object
          ["concurrency" .= object ["cooldwon_ms" .= (200 ∷ Int)]]]
        catalog = resolve [badType, typo, Bool True, kind "good"] [] []
    Map.keys (catalogTypes catalog) `shouldBe` ["good"]
    length (catalogWarnings catalog) `shouldBe` 3

  it "refuses traversal, dual sources, and MP3 loop declarations" $ do
    let loopType = object ["id" .= ("loop" ∷ Text), "policy" .= object
          ["loop" .= object ["allowed" .= True]]]
        dual = object ["id" .= ("dual" ∷ Text), "type" .= ("world" ∷ Text),
          "source" .= object ["sample" .= object [], "synth" .= object []]]
        catalog = resolve [kind "world", loopType] []
          [sample "traversal" "world" "assets/audio/../secret.wav",
           sample "loop" "loop" "assets/audio/loop.MP3", dual]
    Map.null (catalogSounds catalog) `shouldBe` True
    length (catalogWarnings catalog) `shouldBe` 3

  it "requires UI content to remain nonspatial and pause-continuing" $ do
    let invalid = object ["id" .= ("ui" ∷ Text), "policy" .= object ["bus" .= ("ui" ∷ Text)]]
        catalog = resolve [invalid] [] []
    Map.null (catalogTypes catalog) `shouldBe` True
    length (catalogWarnings catalog) `shouldBe` 1
