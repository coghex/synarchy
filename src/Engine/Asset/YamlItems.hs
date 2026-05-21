{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlItems
    ( ItemYamlDef(..)
    , ItemYamlContainer(..)
    , ItemYamlFood(..)
    , ItemYamlRollSpec(..)
    , ItemYamlWeapon(..)
    , ItemYamlFile(..)
    , loadItemYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | Optional container block. Items without this can't hold a fluid.
data ItemYamlContainer = ItemYamlContainer
    { iycCapacity ∷ !Float
    , iycHolds    ∷ !Text
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContainer where
    parseJSON = withObject "ItemYamlContainer" $ \v → ItemYamlContainer
        ⊚ v .:  "capacity"
        ⊛ v .:? "holds" .!= "water"

-- | Optional food block. Items without this can't be eaten.
data ItemYamlFood = ItemYamlFood
    { iyfNutrition ∷ !Float    -- ^ kcal restored per item
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlFood where
    parseJSON = withObject "ItemYamlFood" $ \v → ItemYamlFood
        ⊚ v .: "nutrition"

-- | (min, max) range for a rolled spec — used by both quality and
--   condition. Interpreted as a normal distribution clamped to the
--   range. Reads as `{ min: 50, max: 75 }` in YAML.
data ItemYamlRollSpec = ItemYamlRollSpec
    { iyrsMin ∷ !Float
    , iyrsMax ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlRollSpec where
    parseJSON = withObject "ItemYamlRollSpec" $ \v → ItemYamlRollSpec
        ⊚ v .: "min"
        ⊛ v .: "max"

-- | Optional weapon block on an item def. Geometric + material
--   reference; material physical properties live in the
--   SubstanceManager and get joined at use time.
data ItemYamlWeapon = ItemYamlWeapon
    { iywBladeLength    ∷ !Float
    , iywBaseSharpness  ∷ !Float
    , iywStabEff        ∷ !Float
    , iywSlashEff       ∷ !Float
    , iywBluntEff       ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlWeapon where
    parseJSON = withObject "ItemYamlWeapon" $ \v → ItemYamlWeapon
        ⊚ v .:  "blade_length"
        ⊛ v .:  "base_sharpness"
        ⊛ v .:? "stab_effectiveness"   .!= 0
        ⊛ v .:? "slash_effectiveness"  .!= 0
        ⊛ v .:? "blunt_effectiveness"  .!= 0

data ItemYamlDef = ItemYamlDef
    { iydName        ∷ !Text
    , iydDisplayName ∷ !Text
    , iydSprite      ∷ !Text                       -- ^ texture path
    , iydWeight      ∷ !Float                      -- ^ empty weight (kg)
    , iydKind        ∷ !Text                       -- ^ equipment slot kind;
                                                   --   defaults to "misc"
    , iydCategory    ∷ !Text                       -- ^ inventory tab;
                                                   --   defaults to "Misc"
    , iydMake        ∷ !Text                       -- ^ crafting tradition;
                                                   --   defaults to ""
    , iydMaterial    ∷ !Text                       -- ^ substance name;
                                                   --   defaults to ""
    , iydQuality     ∷ !(Maybe ItemYamlRollSpec)   -- ^ quality roll range
    , iydCondition   ∷ !(Maybe ItemYamlRollSpec)   -- ^ condition roll range
    , iydContainer   ∷ !(Maybe ItemYamlContainer)
    , iydFood        ∷ !(Maybe ItemYamlFood)
    , iydWeapon      ∷ !(Maybe ItemYamlWeapon)
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlDef where
    parseJSON = withObject "ItemYamlDef" $ \v → ItemYamlDef
        ⊚ v .:  "name"
        ⊛ v .:? "display_name" .!= ""
        ⊛ v .:  "sprite"
        ⊛ v .:? "weight"       .!= 0.0
        ⊛ v .:? "kind"         .!= "misc"
        ⊛ v .:? "category"     .!= "Misc"
        ⊛ v .:? "make"         .!= ""
        ⊛ v .:? "material"     .!= ""
        ⊛ v .:? "quality"
        ⊛ v .:? "condition"
        ⊛ v .:? "container"
        ⊛ v .:? "food"
        ⊛ v .:? "weapon"

newtype ItemYamlFile = ItemYamlFile
    { iyfItems ∷ [ItemYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlFile where
    parseJSON = withObject "ItemYamlFile" $ \v → ItemYamlFile
        ⊚ v .: "items"

loadItemYaml ∷ LoggerState → FilePath → IO [ItemYamlDef]
loadItemYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse item YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right uf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (iyfItems uf)))
                <> " item definitions from " <> T.pack path
            return (iyfItems uf)
