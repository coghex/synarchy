module Test.Headless.Audio.Config (spec) where

import UPrelude
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import qualified Data.Yaml as Yaml
import Engine.Audio.Config.Player
import Engine.Audio.Config.Runtime
import System.Directory (doesFileExist)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Hspec

spec ∷ Spec
spec = describe "Audio.Config" $ do
  it "loads the complete tracked runtime without falling back" $
    loadRuntimeConfig `shouldReturn` (defaultRuntimeConfig, [])

  it "rejects unknown top-level fields instead of silently ignoring them" $ do
    value ← Yaml.decodeFileThrow "config/audio_runtime.yaml" ∷ IO Value
    case value of
      Object top → parseEither parseRuntimeConfig (Object $ KM.insert "typo" (Bool True) top)
        `shouldSatisfy` isLeft
      _ → expectationFailure "runtime fixture must be an object"

  it "falls back atomically when a partial runtime file has a plausible changed rate" $
    withIsolatedResourceRoot $ do
      Yaml.encodeFile "config/audio_runtime.yaml" $ object
        ["schema_version" .= (1 ∷ Int), "audio_runtime" .= object
          ["mix" .= object ["sample_rate_hz" .= (44100 ∷ Int)]]]
      (config, warnings) ← loadRuntimeConfig
      config `shouldBe` defaultRuntimeConfig
      warnings `shouldSatisfy` (not ∘ null)

  it "keeps valid volume siblings while refusing fractional and out-of-range values" $ do
    let base = Volumes 70 40 20
        patch = object ["audio" .= object
          ["master_volume" .= (12.5 ∷ Double), "world_volume" .= (35 ∷ Int), "ui_volume" .= (101 ∷ Int)]]
    overlayVolumes base patch `shouldBe` Volumes 70 35 20
    overlayVolumes base Null `shouldBe` base

  it "writes only changed keys, reloads them, and removes the file on return to defaults" $
    withIsolatedResourceRoot $ do
      Yaml.encodeFile "config/audio_default.yaml" $ object ["audio" .= object
        ["master_volume" .= (20 ∷ Int), "world_volume" .= (40 ∷ Int), "ui_volume" .= (60 ∷ Int)]]
      writeVolumes (Volumes 50 40 60) `shouldReturn` Right (Volumes 50 40 60)
      written ← Yaml.decodeFileThrow "config/audio.local.yaml" ∷ IO Value
      written `shouldBe` object ["audio" .= object ["master_volume" .= (50 ∷ Int)]]
      loadSavedVolumes `shouldReturn` Volumes 50 40 60
      writeVolumes (Volumes 20 40 60) `shouldReturn` Right (Volumes 20 40 60)
      doesFileExist "config/audio.local.yaml" `shouldReturn` False

  it "makes zero a hard mute and applies the selected squared-amplitude curve" $ do
    volumeGain 2 0 `shouldBe` 0
    volumeGain 2 50 `shouldBe` 0.25
    volumeGain 2 100 `shouldBe` 1
    clampVolumes (Volumes (-20) 300 52) `shouldBe` Volumes 0 100 52
