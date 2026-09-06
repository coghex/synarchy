{-# LANGUAGE Strict, DeriveGeneric #-}
-- | Vegetation definitions loaded from @data/vegetation/*.yaml@, one
--   caller-supplied file at a time: both entry points take the path as a
--   parameter, so enumerating that directory and folding the results
--   together belongs to the caller. The production path is
--   'World.ZoomMap.ColorPalette.buildColorPalette'; the other consumer is
--   the @engine.loadVegetationYaml@ Lua binding.
module Engine.Asset.YamlVegetation
    ( -- * YAML types
      VegetationDef(..)
    , VegetationFile(..)
      -- * Loading
    , loadVegetationYaml
    , loadVegetationYamlOutcome
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), (.:), withObject)
import Engine.Asset.YamlList (loadYamlListOutcome)
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

-- | 'loadVegetationYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadVegetationYaml'.
loadVegetationYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [VegetationDef])
loadVegetationYamlOutcome logger =
    loadYamlListOutcome logger "vegetation" "vegetation types" vfVegetation

loadVegetationYaml ∷ LoggerState → FilePath → IO [VegetationDef]
loadVegetationYaml logger path =
    fromMaybe [] ⊚ loadVegetationYamlOutcome logger path
