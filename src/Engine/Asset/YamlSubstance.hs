{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlSubstance
    ( SubstanceYamlDef(..)
    , SubstanceYamlFile(..)
    , loadSubstanceYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlList)

-- | YAML shape for a substance entry. Only `name`, `density`, `tensile_strength`,
--   and `fracture_toughness` are required; the others default to 0
--   ("not modelled" — combat formulas should fall back to derived
--   approximations or use other available properties).
data SubstanceYamlDef = SubstanceYamlDef
    { syName              ∷ !Text
    , syDensity           ∷ !Float
    , syTensileStrength   ∷ !Float
    , syYieldStrength     ∷ !Float
    , syShearStrength     ∷ !Float
    , syFractureToughness ∷ !Float
    , syHardness          ∷ !Float
    , syStabResistance    ∷ !Float
    , sySlashResistance   ∷ !Float
    , syBluntResistance   ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON SubstanceYamlDef where
    parseJSON = withObject "SubstanceYamlDef" $ \v → SubstanceYamlDef
        ⊚ v .:  "name"
        ⊛ v .:  "density"
        ⊛ v .:  "tensile_strength"
        ⊛ v .:? "yield_strength"      .!= 0
        ⊛ v .:? "shear_strength"      .!= 0
        ⊛ v .:  "fracture_toughness"
        ⊛ v .:? "hardness"            .!= 0
        ⊛ v .:? "stab_resistance"     .!= 0
        ⊛ v .:? "slash_resistance"    .!= 0
        ⊛ v .:? "blunt_resistance"    .!= 0

newtype SubstanceYamlFile = SubstanceYamlFile
    { syfSubstances ∷ [SubstanceYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON SubstanceYamlFile where
    parseJSON = withObject "SubstanceYamlFile" $ \v → SubstanceYamlFile
        ⊚ v .: "substances"

loadSubstanceYaml ∷ LoggerState → FilePath → IO [SubstanceYamlDef]
loadSubstanceYaml logger = loadYamlList logger "substance" "substances" syfSubstances
