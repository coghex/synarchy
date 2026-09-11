module Test.Headless.Audio.Upload (spec) where

import UPrelude
import Data.Aeson (Value, object, (.=))
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing,
                         createFileLink, withCurrentDirectory)
import System.FilePath ((</>))
import Engine.Audio.Catalog.Resolve
import Engine.Audio.Catalog.Types
import Engine.Audio.Catalog.Upload
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)
import Test.Hspec

withSamples ∷ IO α → IO α
withSamples action = do
  fixture ← canonicalizePath "test-headless/data/audio/tone.wav"
  withExclusiveTempDirectory "synarchy-audio-upload" $ \root → do
    createDirectoryIfMissing True (root </> "assets/audio")
    copyFile fixture (root </> "assets/audio/a.wav")
    copyFile fixture (root </> "assets/audio/b.wav")
    copyFile fixture (root </> "outside.wav")
    createFileLink (root </> "outside.wav") (root </> "assets/audio/escape.wav")
    createFileLink (root </> "assets/audio/a.wav") (root </> "assets/audio/alias.wav")
    withCurrentDirectory root action

withCore ∷ (Native → IO ()) → IO ()
withCore action = withNative defaultNativeConfig ForcedNull action
  ⌦ either (expectationFailure ∘ Text.unpack) pure

sample ∷ Text → Text → Value
sample name path = object ["id" .= name, "type" .= ("world" ∷ Text),
  "source" .= object ["sample" .= object ["path" .= path]]]

catalog ∷ [Value] → Catalog
catalog sounds = resolveCatalog defaultRuntimeConfig
  (object ["schema_version" .= (1 ∷ Int), "sound_types" .= [object ["id" .= ("world" ∷ Text)]]])
  (object ["schema_version" .= (1 ∷ Int), "instruments" .= ([] ∷ [Value])])
  (object ["schema_version" .= (1 ∷ Int), "sounds" .= sounds])

spec ∷ Spec
spec = describe "Audio.Upload" $ do
  it "rejects traversal and escaping symlinks at the actual decode boundary" $ withSamples $ do
    checkedSamplePath "assets/audio/../audio/a.wav" ≫= (`shouldSatisfy` isLeft)
    checkedSamplePath "assets/audio/escape.wav" ≫= (`shouldSatisfy` isLeft)
    checkedSamplePath "assets/audio/missing.wav" ≫= (`shouldSatisfy` isLeft)
    checked ← checkedSamplePath "assets/audio/alias.wav"
    resolved ← canonicalizePath "assets/audio/a.wav"
    checked `shouldBe` Right resolved

  it "decodes shared PCM once and spends aggregate budget in sorted path order" $ withSamples $ withCore $ \core → do
    let config = defaultRuntimeConfig { rcMaxTotalSeconds = 0.3 }
        authored = catalog [sample "first_name" "assets/audio/b.wav",
          sample "second_name" "assets/audio/a.wav", sample "third_name" "assets/audio/a.wav",
          sample "alias_name" "assets/audio/alias.wav"]
    result ← uploadCatalog core config authored
    Map.keys (uploadedSounds result) `shouldBe` ["alias_name", "second_name", "third_name"]
    length (uploadWarnings result) `shouldBe` 1
    Map.lookup "sounds" (uploadedCounts result) `shouldBe` Just (3, 1)
    Map.lookup "assets" (uploadedCounts result) `shouldBe` Just (2, 1)
    uploadWarnings result `shouldSatisfy` any (Text.isInfixOf "sound first_name → assets/audio/b.wav")
    status ← readNativeStatus core
    nsSampleCount status `shouldBe` 1
    nsDecodedFrames status `shouldBe` 12000
    nsDecodedBytes status `shouldBe` 96000
    let handles = map (fst ∘ snd) $ Map.toAscList (uploadedSounds result)
    handles `shouldBe` [1, 2, 3]

  it "disables a missing sample without suppressing valid sibling playback" $ withSamples $ withCore $ \core → do
    result ← uploadCatalog core defaultRuntimeConfig $ catalog
      [sample "missing" "assets/audio/missing.wav", sample "valid" "assets/audio/a.wav"]
    Map.keys (uploadedSounds result) `shouldBe` ["valid"]
    length (uploadWarnings result) `shouldBe` 1
    case Map.lookup "valid" (uploadedSounds result) of
      Nothing → expectationFailure "valid sample was disabled"
      Just (handle, _) → do
        submitOffline core [PlayNative handle (0, 0, 0) 0 0] `shouldReturn` Right [CommandResult 0 0 0]
        rendered ← renderOffline core 12000
        case rendered of
          Left err → expectationFailure (Text.unpack err)
          Right pcm → maximum (map abs pcm) `shouldSatisfy` (> 0.1)
        nsActiveVoices <$> readNativeStatus core `shouldReturn` 0
