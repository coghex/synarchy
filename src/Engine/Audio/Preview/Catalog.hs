-- | Preview uses the shipped synthesizer/decoder and a disposable catalog.
-- Reload destroys the previous native core before loading another bounded set.
module Engine.Audio.Preview.Catalog (loadPreviewCatalog) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.FilePath (makeRelative, takeFileName)
import Engine.Audio.Catalog.Resolve (loadCatalog)
import Engine.Audio.Catalog.Types
import Engine.Audio.Catalog.Upload
import Engine.Audio.Config.Runtime
import Engine.Audio.Native (Native)
import Engine.Audio.Preview.Discovery
import Engine.Audio.Preview.Types

-- Authoring auditions are interface feedback without a world/listener. Keep
-- timbre, envelope, source, authored gain and one-shot gate exactly intact.
previewPolicy ∷ Policy → Policy
previewPolicy policy = policy { policyBus = UIBus, policySpatial = False,
  policyFreeze = False, policyCooldownMs = 0, policyAllowLoop = False,
  policyMaxInstances = 1, policyOverflow = StealOldest }

loadPreviewCatalog ∷ PreviewAudioConfig → Native → RuntimeConfig → IO UploadedCatalog
loadPreviewCatalog options native config = do
  authored ← loadCatalog config
  discovered ← discoverAudioFiles "assets/audio"
  let synths = take 256 [sound | sound ← Map.elems (catalogSounds authored),
                         SynthSource _ ← [soundSource sound]]
      paths = take 256 $ Set.toAscList $ Set.fromList $ discovered <>
        [path | sound ← Map.elems (catalogSounds authored), SampleSource path ← [soundSource sound]]
      filePaths = maybe paths (\path → path : filter (≢ path) paths) (pacFile options)
      policy = Policy UIBus False 1 40 1 0 80 1 StealOldest 0 False 10 False
      raw = [(soundId sound, "synth", Nothing, sound) | sound ← synths] <>
        [(Text.pack $ if Just path ≡ pacFile options then takeFileName path
          else makeRelative "assets/audio" path, "files", Just path,
          Sound "" policy (SampleSource path)) | path ← filePaths]
      identified = [(key, label, category, path,
          sound { soundId = if category ≡ "synth" then soundId sound else key,
            soundPolicy = previewPolicy $ soundPolicy sound })
        | (index, (label, category, path, sound)) ← zip [1 ∷ Int ..] raw,
          let key = "preview_" <> Text.justifyRight 4 '0' (tshow index)]
      catalog = authored { catalogSounds = Map.fromList [(key, sound) | (key, _, _, _, sound) ← identified] }
      resolve path | Just path ≡ pacFile options = resolvePreviewFile "." path
                   | otherwise = checkedSamplePath path
  uploaded ← uploadCatalogWithResolver resolve native config catalog
  pure uploaded { uploadedPreview = [PreviewAudioEntry key label category path
      (Map.member key $ uploadedSounds uploaded) | (key, label, category, path, _) ← identified] }
