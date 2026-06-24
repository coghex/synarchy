{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
-- | YAML loader for data/infections/*.yaml. Mirrors Engine.Asset.YamlSubstance.
--   The on-disk schema is documented in data/infections/bacteria.yaml.
module Engine.Asset.YamlInfection
    ( InfectionYamlDef(..)
    , InfectionYamlFile(..)
    , loadInfectionYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | YAML shape for one infection entry. Only `id` + `name` + `category`
--   are required; everything else has a sensible default so a terse entry
--   still loads.
data InfectionYamlDef = InfectionYamlDef
    { iyId            ∷ !Text
    , iyName          ∷ !Text
    , iyIcon          ∷ !Text
    , iyCategory      ∷ !Text
    , iySites         ∷ ![Text]
    , iyBaseWeight    ∷ !Float
    , iyTempMin       ∷ !Float
    , iyTempMax       ∷ !Float
    , iyMoistMin      ∷ !Float
    , iyMoistMax      ∷ !Float
    , iyAggressiveness ∷ !Float
    , iyInfectability  ∷ !Float
    , iyCurableBy     ∷ ![Text]
    , iyCureRate      ∷ !Float
    , iyWoundInfectable ∷ !Bool
    , iyEffects       ∷ ![Text]
    , iyTransmissibility ∷ !Float
    , iyTransmission  ∷ ![Text]
    } deriving (Show, Eq, Generic)

-- | Nested `climate: { temp: [min,max], moisture: [min,max] }`. Each band
--   defaults to the full plausible range when omitted.
data ClimateBandYaml = ClimateBandYaml
    { cbTemp  ∷ ![Float]
    , cbMoist ∷ ![Float]
    } deriving (Show, Eq, Generic)

instance FromJSON ClimateBandYaml where
    parseJSON = withObject "ClimateBandYaml" $ \v → ClimateBandYaml
        ⊚ v .:? "temp"     .!= [-50, 50]
        ⊛ v .:? "moisture" .!= [0, 1]

instance FromJSON InfectionYamlDef where
    parseJSON = withObject "InfectionYamlDef" $ \v → do
        band ← v .:? "climate" .!= ClimateBandYaml [-50, 50] [0, 1]
        let (tMin, tMax) = pair (-50) 50 (cbTemp band)
            (mMin, mMax) = pair 0 1 (cbMoist band)
        InfectionYamlDef
            ⊚ v .:  "id"
            ⊛ v .:  "name"
            ⊛ v .:? "icon"            .!= "bacterial_infection"
            ⊛ v .:  "category"
            ⊛ v .:? "site"            .!= []
            ⊛ v .:? "base_weight"     .!= 1.0
            ⊛ pure tMin
            ⊛ pure tMax
            ⊛ pure mMin
            ⊛ pure mMax
            ⊛ v .:? "aggressiveness"  .!= 1.0
            ⊛ v .:? "infectability"   .!= 1.0
            ⊛ v .:? "curable_by"      .!= []
            ⊛ v .:? "cure_rate"       .!= 1.0
            ⊛ v .:? "wound_infectable" .!= True
            ⊛ v .:? "effects"         .!= []
            ⊛ v .:? "transmissibility" .!= 0.0
            ⊛ v .:? "transmission"    .!= []
      where
        pair lo hi xs = case xs of
            (a:b:_) → (a, b)
            [a]     → (a, hi)
            _       → (lo, hi)

newtype InfectionYamlFile = InfectionYamlFile
    { iyfInfections ∷ [InfectionYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON InfectionYamlFile where
    parseJSON = withObject "InfectionYamlFile" $ \v → InfectionYamlFile
        ⊚ v .: "infections"

loadInfectionYaml ∷ LoggerState → FilePath → IO [InfectionYamlDef]
loadInfectionYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse infection YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right f → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (iyfInfections f)))
                <> " infections from " <> T.pack path
            return (iyfInfections f)
