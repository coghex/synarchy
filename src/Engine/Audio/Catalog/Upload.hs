-- | Startup-only catalog upload. Paths and budgets are resolved before playback.
module Engine.Audio.Catalog.Upload
  ( UploadedCatalog(..), uploadCatalog, uploadCatalogWithResolver, checkedSamplePath, nativeTimbre, nativeSound ) where

import UPrelude
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.Char (ord)
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (splitDirectories)
import Engine.Audio.Catalog.Types
import Engine.Audio.Catalog.Yaml (parseAssetPath)
import Engine.Audio.Config.Runtime
import Engine.Audio.Native
import Engine.Audio.Preview.Types

data UploadedCatalog = UploadedCatalog
  { uploadedSounds ∷ Map.Map Text (Word32, Sound)
  , uploadWarnings ∷ [Text], uploadedCounts ∷ Map.Map Text (Int, Int)
  , uploadedPreview ∷ [PreviewAudioEntry]
  , uploadedRevision ∷ Word64
  } deriving (Eq, Show)

-- | Compare path components, not a string prefix (audio-other is not audio).
-- Return the resolved path so native decode opens the file we checked, not the
-- authored symlink. The resource-root directory is selected before engine boot.
checkedSamplePath ∷ FilePath → IO (Either Text FilePath)
checkedSamplePath authored | Left err ← parseEither parseAssetPath authored = pure (Left $ Text.pack err)
checkedSamplePath authored = do
  result ← try @IOException $ do
    root ← canonicalizePath "assets/audio"
    resolved ← canonicalizePath authored
    exists ← doesFileExist resolved
    let base = splitDirectories root
        inside = take (length base) (splitDirectories resolved) ≡ base
    pure $ if not inside then Left "resolved sample path escapes assets/audio"
      else if not exists then Left "sample file is missing"
      else Right resolved
  pure $ either (Left ∘ tshow) id result

nativeTimbre ∷ Timbre → NativeInstrument
nativeTimbre t = NativeInstrument
  { niWaveform = case timbreWaveform t of Sine → 0; Saw → 1; Square → 2; Triangle → 3; WhiteNoise → 4
  , niSeed = timbreNoiseSeed t
  , niFilter = case timbreFilter t of Bypass → 0; LowPass → 1; HighPass → 2; BandPass → 3
  , niRandomPhase = if timbreRandomPhase t then 1 else 0
  , niFrequency = timbreFrequency t, niPhase = 0
  , niAttackMs = timbreAttackMs t, niDecayMs = timbreDecayMs t
  , niSustain = timbreSustain t, niReleaseMs = timbreReleaseMs t
  , niCutoff = timbreCutoff t, niQ = timbreResonance t
  , niGainDb = timbreGainDb t, niGateMs = timbreGateMs t
  }

-- SoundIds are ASCII by the catalog contract. Fixed FNV-1a avoids process hashes
-- and seeds reproducible per-voice streams without sharing gameplay randomness.
soundSeed ∷ Text → Word32
soundSeed = Text.foldl' (\hash char → (hash `xor` fromIntegral (ord char)) * 16777619) 2166136261

nativeSound ∷ Word32 → Sound → NativeSound
nativeSound handle sound =
  let policy = soundPolicy sound
      bit flag = if flag then 1 else 0
  in NativeSound
    { ndSourceKind = case soundSource sound of SampleSource _ → 0; SynthSource _ → 1
    , ndSourceHandle = handle
    , ndBus = case policyBus policy of WorldBus → 0; UIBus → 1
    , ndSpatial = bit (policySpatial policy), ndPriority = policyPriority policy
    , ndMaxInstances = policyMaxInstances policy
    , ndOverflow = case policyOverflow policy of DropNew → 0; StealOldest → 1
    , ndCooldownMs = policyCooldownMs policy, ndLoop = bit (policyAllowLoop policy)
    , ndFreeze = bit (policyFreeze policy), ndSeed = soundSeed (soundId sound)
    , ndMinDistance = policyMinDistance policy, ndMaxDistance = policyMaxDistance policy
    , ndVerticalScale = policyVerticalScale policy, ndGainDb = policyGainDb policy
    , ndGateMs = case soundSource sound of SampleSource _ → 0; SynthSource t → timbreGateMs t
    , ndStopFadeMs = policyStopFadeMs policy
    }

uploadCatalog ∷ Native → RuntimeConfig → Catalog → IO UploadedCatalog
uploadCatalog = uploadCatalogWithResolver checkedSamplePath

-- | Preview alone supplies an explicit-file resolver. Production always uses
-- checkedSamplePath; opening the authoring browser does not relax game assets.
uploadCatalogWithResolver ∷ (FilePath → IO (Either Text FilePath))
  → Native → RuntimeConfig → Catalog → IO UploadedCatalog
uploadCatalogWithResolver resolvePath native config catalog = do
  let paths = Set.toAscList $ Set.fromList
        [path | sound ← Map.elems (catalogSounds catalog), SampleSource path ← [soundSource sound]]
  (samples, _) ← foldM load (Map.empty, Map.empty) paths
  (sounds, warnings, _) ← foldM (register samples) (Map.empty, [], [])
    (Map.toAscList $ catalogSounds catalog)
  let validAssets = length [() | Right _ ← Map.elems samples]
      rejected = Map.size (catalogSounds catalog) - Map.size sounds
      soundCounts = (Map.size sounds, snd (Map.findWithDefault (0, 0) "sounds" $ catalogCounts catalog) + rejected)
      counts = Map.insert "assets" (validAssets, Map.size samples - validAssets)
        $ Map.insert "sounds" soundCounts (catalogCounts catalog)
  pure $ UploadedCatalog sounds (catalogWarnings catalog <> warnings) counts [] 0
  where
    rate = fromIntegral $ ncSampleRate (rcNative config)
    frameLimit seconds = floor (realToFrac seconds * rate ∷ Double) ∷ Word64
    totalFrames = frameLimit $ rcMaxTotalSeconds config
    totalBytes = fromIntegral (rcMaxDecodedMiB config) * 1024 * 1024
    load (samples, resolvedSamples) path = do
      status ← readNativeStatus native
      let remainingFrames = totalFrames - min totalFrames (nsDecodedFrames status)
          remainingBytes = totalBytes - min totalBytes (nsDecodedBytes status)
          limit = min remainingFrames (frameLimit $ rcMaxAssetSeconds config)
          budget = DecodeLimits (fromIntegral (rcMaxEncodedMiB config) * 1024 * 1024) limit remainingBytes
      checked ← resolvePath path
      result ← case checked of
        Left err → pure (Left err)
        Right resolved | Just handle ← Map.lookup resolved resolvedSamples → pure (Right handle)
                       | limit ≡ 0 ∨ remainingBytes < 8 → pure (Left "resident sample budget exhausted")
                       | otherwise → loadSampleNative native resolved budget
      let detailed = either (Left ∘ (<> ("; remaining frames=" <> tshow remainingFrames
            <> ", bytes=" <> tshow remainingBytes))) Right result
          shared = case (checked, result) of
            (Right resolved, Right handle) → Map.insert resolved handle resolvedSamples
            _ → resolvedSamples
      pure (Map.insert path detailed samples, shared)
    register samples (sounds, warnings, timbres) (name, sound) = do
      (source, nextTimbres) ← case soundSource sound of
        SampleSource path → pure (Map.findWithDefault (Left "sample was not loaded") path samples, timbres)
        SynthSource timbre → case lookup timbre timbres of
          Just handle → pure (Right handle, timbres)
          Nothing → do
            result ← addInstrumentNative native (nativeTimbre timbre)
            pure (result, either (const timbres) (\handle → timbres <> [(timbre, handle)]) result)
      registered ← case source of
        Left err → pure (Left err)
        Right handle → addSoundNative native (nativeSound handle sound)
      pure $ case registered of
        Left err → (sounds, warnings <> ["sound " <> name <> " → " <> sourceName (soundSource sound)
          <> ": " <> err], nextTimbres)
        Right handle → (Map.insert name (handle, sound) sounds, warnings, nextTimbres)
    sourceName (SampleSource path) = Text.pack path
    sourceName (SynthSource _) = "resolved synth instrument"
