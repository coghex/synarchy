{-# LANGUAGE Strict, DeriveGeneric #-}
-- | Vegetation definitions loaded from @data/vegetation.yaml@.
module Engine.Asset.YamlVegetation
    ( -- * YAML types
      VegetationDef(..)
    , VegetationFile(..)
      -- * Loading
    , loadVegetationYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), (.:), withObject)
import Engine.Asset.YamlList (loadYamlList)
import Engine.Core.Log (LoggerState)

-- | Variant IDs are @id_start .. id_start + len - 1@
data VegetationDef = VegetationDef
    { vdIdStart  ∷ Word8
    , vdName     ∷ Text
    , vdVariants ∷ [Text]
    } deriving (Show, Eq, Generic)

instance FromJSON VegetationDef where
    parseJSON = withObject "VegetationDef" $ \v → VegetationDef
        ⊚ v .: "id_start"
        ⊛ v .: "name"
        ⊛ v .: "variants"

data VegetationFile = VegetationFile
    { vfVegetation ∷ [VegetationDef]
    } deriving (Show, Eq, Generic)

instance FromJSON VegetationFile where
    parseJSON = withObject "VegetationFile" $ \v → VegetationFile
        ⊚ v .: "vegetation"

-- * YAML parsing

loadVegetationYaml ∷ LoggerState → FilePath → IO [VegetationDef]
loadVegetationYaml logger =
    loadYamlList logger "vegetation" "vegetation types" vfVegetation
