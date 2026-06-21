{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlItems
    ( ItemYamlDef(..)
    , ItemYamlWeight(..)
    , ItemYamlContainer(..)
    , ItemYamlContent(..)
    , ItemYamlFood(..)
    , ItemYamlRollSpec(..)
    , ItemYamlWeapon(..)
    , ItemYamlArmor(..)
    , ItemYamlBuff(..)
    , ItemYamlFile(..)
    , loadItemYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | Optional container block. Items without this can't hold a fluid.
data ItemYamlContainer = ItemYamlContainer
    { iycCapacity   ∷ !Float
    , iycHolds      ∷ !Text
    , iycFillWeight ∷ !Float   -- kg per fill unit (1.0 = litres/water)
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContainer where
    parseJSON = withObject "ItemYamlContainer" $ \v → ItemYamlContainer
        ⊚ v .:  "capacity"
        ⊛ v .:? "holds" .!= "water"
        ⊛ v .:? "fill_weight" .!= 1.0

-- | One entry in an item-container's default contents (first-aid kit /
--   toolbox): which item, how many, and an optional fill for fillable
--   contents (a pill bottle's count, a fluid bottle's litres).
data ItemYamlContent = ItemYamlContent
    { iycoItem  ∷ !Text
    , iycoCount ∷ !Int
    , iycoFill  ∷ !(Maybe Float)
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContent where
    parseJSON = withObject "ItemYamlContent" $ \v → ItemYamlContent
        ⊚ v .:  "item"
        ⊛ v .:? "count" .!= 1
        ⊛ v .:? "fill"

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

-- | One stat-modifier conferred by equipping an item.
--   YAML shape: `{ stat: perception, amount: 1, scales_with_condition: true }`.
data ItemYamlBuff = ItemYamlBuff
    { iybStat                ∷ !Text
    , iybAmount              ∷ !Float
    , iybScalesWithCondition ∷ !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlBuff where
    parseJSON = withObject "ItemYamlBuff" $ \v → ItemYamlBuff
        ⊚ v .:  "stat"
        ⊛ v .:  "amount"
        ⊛ v .:? "scales_with_condition" .!= False

-- | Optional weapon block on an item def. Geometric + material
--   reference; material physical properties live in the
--   SubstanceManager and get joined at use time.
data ItemYamlWeapon = ItemYamlWeapon
    { iywBladeLength    ∷ !Float
    , iywBaseSharpness  ∷ !Float
    , iywStabEff        ∷ !Float
    , iywSlashEff       ∷ !Float
    , iywBluntEff       ∷ !Float
    , iywWeaponClass    ∷ !Text   -- ^ skill name (dagger/unarmed/…)
    , iywAttackCooldown ∷ !Float  -- ^ seconds between swings
    , iywLength         ∷ !Float  -- ^ cm total; 0 ⇒ use blade_length
    , iywCenterOfMass   ∷ !Float  -- ^ 0..1 from grip
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlWeapon where
    parseJSON = withObject "ItemYamlWeapon" $ \v → ItemYamlWeapon
        ⊚ v .:  "blade_length"
        ⊛ v .:  "base_sharpness"
        ⊛ v .:? "stab_effectiveness"   .!= 0
        ⊛ v .:? "slash_effectiveness"  .!= 0
        ⊛ v .:? "blunt_effectiveness"  .!= 0
        ⊛ v .:? "weapon_class"         .!= "unarmed"
        ⊛ v .:? "attack_cooldown"      .!= 1.5
        ⊛ v .:? "length"               .!= 0
        ⊛ v .:? "center_of_mass"       .!= 0.5

-- | Optional armour block on an item def. The protective material is
--   the item's top-level `material`; this adds the thickness and the
--   body parts it covers.
data ItemYamlArmor = ItemYamlArmor
    { iyaThickness ∷ !Float
    , iyaCovers    ∷ ![Text]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlArmor where
    parseJSON = withObject "ItemYamlArmor" $ \v → ItemYamlArmor
        ⊚ v .:? "thickness" .!= 1.0
        ⊛ v .:? "covers"    .!= []

-- | Weight as declared in YAML: a plain number (every instance the
--   same) or @{mean, range}@ for per-instance truncated-normal rolls
--   (raw gems vary per find).
data ItemYamlWeight
    = WeightFixed !Float
    | WeightSpec !Float !Float   -- ^ mean, range
    deriving (Show, Eq, Generic)

instance FromJSON ItemYamlWeight where
    parseJSON v = case v of
        Aeson.Object o → WeightSpec
            ⊚ o .:  "mean"
            ⊛ o .:? "range" .!= 0.0
        _ → WeightFixed <$> parseJSON v

data ItemYamlDef = ItemYamlDef
    { iydName        ∷ !Text
    , iydDisplayName ∷ !Text
    , iydSprite      ∷ !Text                       -- ^ texture path
    , iydWeight      ∷ !ItemYamlWeight             -- ^ empty weight (kg)
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
    , iydContents    ∷ ![ItemYamlContent]            -- ^ item-container defaults
    , iydFood        ∷ !(Maybe ItemYamlFood)
    , iydWeapon      ∷ !(Maybe ItemYamlWeapon)
    , iydArmor       ∷ !(Maybe ItemYamlArmor)
    , iydUnequippable ∷ !Bool
    , iydBuffs       ∷ ![ItemYamlBuff]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlDef where
    parseJSON = withObject "ItemYamlDef" $ \v → ItemYamlDef
        ⊚ v .:  "name"
        ⊛ v .:? "display_name" .!= ""
        ⊛ v .:  "sprite"
        ⊛ v .:? "weight"       .!= WeightFixed 0.0
        ⊛ v .:? "kind"         .!= "misc"
        ⊛ v .:? "category"     .!= "Misc"
        ⊛ v .:? "make"         .!= ""
        ⊛ v .:? "material"     .!= ""
        ⊛ v .:? "quality"
        ⊛ v .:? "condition"
        ⊛ v .:? "container"
        ⊛ v .:? "contents"     .!= []
        ⊛ v .:? "food"
        ⊛ v .:? "weapon"
        ⊛ v .:? "armor"
        ⊛ v .:? "unequippable" .!= False
        ⊛ v .:? "buffs"        .!= []

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
