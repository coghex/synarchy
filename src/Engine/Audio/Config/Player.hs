-- | Independent sparse player volumes, published through the durable config writer.
module Engine.Audio.Config.Player
  ( Volumes(..), defaultVolumes, clampVolumes, volumeGain
  , overlayVolumes, loadDefaultVolumes, loadSavedVolumes, writeVolumes
  ) where

import UPrelude
import Data.Aeson (Value(..), (.=), object, fromJSON, Result(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Yaml as Yaml
import Engine.Core.ConfigWrite (writeConfigYaml, removeConfigFile)

data Volumes = Volumes
  { volumeMaster ∷ Int, volumeWorld ∷ Int, volumeUI ∷ Int }
  deriving (Eq, Show)

defaultVolumes ∷ Volumes
defaultVolumes = Volumes 100 100 100

clampVolumes ∷ Volumes → Volumes
clampVolumes (Volumes master world ui) = Volumes (clamp 0 100 master) (clamp 0 100 world) (clamp 0 100 ui)

volumeGain ∷ Float → Int → Float
volumeGain curvePower value = (fromIntegral (clamp 0 100 value) / 100) ** curvePower

overlayVolumes ∷ Volumes → Value → Volumes
overlayVolumes base (Object top)
  | Just (Object values) ← KM.lookup "audio" top =
      let readValue key old = case KM.lookup key values of
            Just raw → case fromJSON raw of
              Success n | n ≥ 0 ∧ n ≤ 100 → n
              _ → old
            Nothing → old
      in Volumes (readValue "master_volume" $ volumeMaster base)
                 (readValue "world_volume" $ volumeWorld base)
                 (readValue "ui_volume" $ volumeUI base)
overlayVolumes base _ = base

readLayer ∷ FilePath → Volumes → IO Volumes
readLayer path base = Yaml.decodeFileEither path ⌦ \decoded →
  pure $ either (const base) (overlayVolumes base) decoded

loadDefaultVolumes ∷ IO Volumes
loadDefaultVolumes = readLayer "config/audio_default.yaml" defaultVolumes

loadSavedVolumes ∷ IO Volumes
loadSavedVolumes = loadDefaultVolumes ⌦ readLayer "config/audio.local.yaml"

writeVolumes ∷ Volumes → IO (Either Text Volumes)
writeVolumes requested = do
  defaults ← loadDefaultVolumes
  let chosen = clampVolumes requested
      fields = [("master_volume", volumeMaster), ("world_volume", volumeWorld), ("ui_volume", volumeUI)]
      differences = [key .= getter chosen | (key, getter) ← fields, getter chosen ≢ getter defaults]
  result ← if null differences
    then fmap (const ()) <$> removeConfigFile "config/audio.local.yaml"
    else writeConfigYaml "config/audio.local.yaml" (object ["audio" .= object differences])
  pure $ chosen <$ result
