{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlItems
    ( ItemYamlDef(..)
    , ItemYamlWeight(..)
    , ItemYamlContainer(..)
    , ItemYamlStorage(..)
    , ItemYamlContent(..)
    , ItemYamlFood(..)
    , ItemYamlRollSpec(..)
    , ItemYamlQualityTier(..)
    , ItemYamlWeapon(..)
    , ItemYamlArmor(..)
    , ItemYamlBuff(..)
    , ItemYamlFile(..)
    , loadItemYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlList)

-- | Parse a REQUIRED authored physical quantity that must be a finite,
--   strictly positive number, diagnosing every rejection BY DEFINITION
--   NAME (#1233 requirement 2).
--
--   Naming the definition is the whole reason this exists rather than a
--   bare @v .: key@ plus a check. 'Engine.Asset.YamlList.loadYamlList'
--   supplies the failing FILE path in its warning, but an ordinary Aeson
--   field error only reaches for a JSON path like @$.items[7].bulk@ —
--   an index nobody can map back to a definition in a 14-entry file
--   without counting. So every fault below (absent, wrong type, zero,
--   negative, non-finite) is raised as ONE uniformly-shaped message
--   carrying @defName@, and the two halves together name the file AND
--   the definition.
--
--   Taking the whole 'Aeson.Value' rather than decoding to 'Float'
--   first is also deliberate: YAML's @.nan@/@.inf@ resolve to STRINGS
--   (the yaml package's scalar resolver only recognizes ordinary
--   numeric syntax), so decoding first would surface those as a type
--   error naming neither the definition nor what was actually wrong.
--   The finiteness check still has to run AFTER narrowing, because a
--   perfectly ordinary @1.0e+100@ is a valid 'Scientific' that becomes
--   'Infinity' in the engine's 32-bit 'Float' field.
requirePositiveQuantity
    ∷ Text        -- ^ the definition's @name@, for the diagnostic
    → Text        -- ^ what the quantity is, e.g. @"bulk"@
    → Text        -- ^ its unit, e.g. @"litres"@
    → Aeson.Object
    → Text        -- ^ the YAML key to read
    → Aeson.Parser Float
requirePositiveQuantity defName label unit v key = do
    mval ← v .:? Key.fromText key
    case mval of
        Nothing  → bad "is required and has no default"
        Just val → case val of
            Aeson.Number s →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then bad ("must be finite, got " <> tshow val)
                     else if f ≤ 0
                       then bad ("must be strictly positive, got " <> tshow f)
                       else pure f
            _ → bad ("must be a number of " <> unit <> ", got " <> tshow val)
  where
    tshow ∷ Show a ⇒ a → Text
    tshow = T.pack ∘ show
    bad why = fail ∘ T.unpack $
        "item definition '" <> defName <> "': " <> label <> " (key '"
        <> key <> "', " <> unit <> ") " <> why

-- | Optional container block. Items without this can't hold a fluid.
data ItemYamlContainer = ItemYamlContainer
    { iycCapacity    ∷ !Float
    , iycHolds       ∷ !Text
    , iycFillWeight  ∷ !Float   -- kg per fill unit (1.0 = litres/water)
    , iycDefaultFill ∷ !Float   -- fill when the spawn site gives none
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContainer where
    parseJSON = withObject "ItemYamlContainer" $ \v → ItemYamlContainer
        ⊚ v .:  "capacity"
        ⊛ v .:? "holds" .!= "water"
        ⊛ v .:? "fill_weight" .!= 1.0
        ⊛ v .:? "default_fill" .!= 0.0

-- | Optional @storage:@ block — portable ITEM-storage capacity (#1233).
--   Entirely separate from 'ItemYamlContainer' above: that one is a
--   homogeneous fluid/pill FILL, this one is an inventory of nested
--   items. A definition may carry both; neither inherits the other's
--   defaults or validation (@docs\/portable_loot_containers.md@ D-12).
--
--   BOTH capacities are required whenever the block is present, and
--   neither has a default — an omitted @weight_capacity@ is a
--   half-authored container, not "unlimited", and silently defaulting
--   either one would invent a physical limit the designer never chose.
--   They are also independent of each other and of the item's own
--   external @bulk@; see 'parseItemYamlStorage'.
data ItemYamlStorage = ItemYamlStorage
    { iysWeightCapacity ∷ !Float   -- ^ kg of contents (storage.weight_capacity)
    , iysBulkCapacity   ∷ !Float   -- ^ litres of contents (storage.bulk_capacity)
    } deriving (Show, Eq, Generic)

-- | Parse a @storage:@ block, threading the OWNING definition's name
--   through so a bad capacity is diagnosed the same way a bad top-level
--   @bulk@ is. There is deliberately no 'FromJSON' instance: the name is
--   not reachable from inside one, and a nameless
--   @Error in $.items[7].storage.bulk_capacity@ is the diagnostic
--   requirement 2 exists to rule out.
parseItemYamlStorage ∷ Text → Aeson.Value → Aeson.Parser ItemYamlStorage
parseItemYamlStorage defName =
    withObject "ItemYamlStorage" $ \v → ItemYamlStorage
        ⊚ requirePositiveQuantity defName "storage weight capacity"
              "kilograms" v "weight_capacity"
        ⊛ requirePositiveQuantity defName "storage bulk capacity"
              "litres" v "bulk_capacity"

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

-- | Optional food block. Items without this can't be eaten. The
--   calories live under a `nutrition:` sub-object so future diet work
--   (protein / fat / carbohydrate / micronutrients) can add sibling
--   keys without restructuring the schema or bumping the save version.
data ItemYamlFood = ItemYamlFood
    { iyfCalories      ∷ !Float  -- ^ kcal per item (food.nutrition.calories)
    , iyfCaloriesPerKg ∷ !Float  -- ^ kcal per kg of fill for BULK food
                                 --   (food.nutrition.calories_per_kg)
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlFood where
    parseJSON = withObject "ItemYamlFood" $ \v → do
        nut ← v .: "nutrition"
        ItemYamlFood ⊚ nut .:? "calories" .!= 0.0
                     ⊛ nut .:? "calories_per_kg" .!= 0.0

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

-- | One override band in an item's `quality_tiers:` list (#345) — a
--   (threshold, label) pair; see 'Item.Types.QualityTier'. Reads as
--   `{ min: 90, label: excellent }` in YAML.
data ItemYamlQualityTier = ItemYamlQualityTier
    { iyqtMin   ∷ !Float
    , iyqtLabel ∷ !Text
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlQualityTier where
    parseJSON = withObject "ItemYamlQualityTier" $ \v → ItemYamlQualityTier
        ⊚ v .: "min"
        ⊛ v .: "label"

-- | One stat-modifier conferred by equipping an item.
--   YAML shape: `{ stat: perception, amount: 1, scales_with_condition: true }`.
--   `percent` is fractional and matches the unit-level modifiers block
--   (0.1 = +10%); a buff can declare `amount`, `percent`, or both
--   (#392). Both default to 0 so either can stand alone.
data ItemYamlBuff = ItemYamlBuff
    { iybStat                ∷ !Text
    , iybAmount              ∷ !Float
    , iybPercent             ∷ !Float
    , iybScalesWithCondition ∷ !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlBuff where
    parseJSON = withObject "ItemYamlBuff" $ \v → ItemYamlBuff
        ⊚ v .:  "stat"
        ⊛ v .:? "amount"  .!= 0.0
        ⊛ v .:? "percent" .!= 0.0
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
    , iydBulk        ∷ !Float                      -- ^ external bulk (litres),
                                                   --   REQUIRED: finite,
                                                   --   strictly positive, no
                                                   --   default (#1233)
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
    , iydQualityTiers ∷ ![ItemYamlQualityTier]     -- ^ quality→label
                                                   --   overrides (#345);
                                                   --   [] ⇒ default set
    , iydContainer   ∷ !(Maybe ItemYamlContainer)
    , iydContents    ∷ ![ItemYamlContent]            -- ^ item-container defaults
    , iydStorage     ∷ !(Maybe ItemYamlStorage)      -- ^ portable item-storage
                                                     --   capacity (#1233)
    , iydFood        ∷ !(Maybe ItemYamlFood)
    , iydWeapon      ∷ !(Maybe ItemYamlWeapon)
    , iydArmor       ∷ !(Maybe ItemYamlArmor)
    , iydUnequippable ∷ !Bool
    , iydBuffs       ∷ ![ItemYamlBuff]
    , iydInsulation  ∷ !Float                      -- ^ thermal insulation when
                                                   --   worn (slows heat loss);
                                                   --   defaults to 0
    } deriving (Show, Eq, Generic)

-- | Every entry under @items:@ is a PHYSICAL item — this schema has no
--   abstract/incorporeal item class — so @bulk@ is unconditionally
--   required (#1233 requirement 1) and the definition is rejected
--   outright when it is missing or not a finite positive number
--   (requirement 2).
--
--   The name is read FIRST and the two physical blocks parsed
--   monadically against it, so every rejection names the offending
--   definition rather than a bare JSON index; everything else keeps its
--   original applicative shape and its original defaults.
instance FromJSON ItemYamlDef where
    parseJSON = withObject "ItemYamlDef" $ \v → do
        name     ← v .: "name"
        bulk     ← requirePositiveQuantity name "bulk" "litres" v "bulk"
        mStorage ← v .:? "storage"
        storage  ← traverse (parseItemYamlStorage name) mStorage
        ItemYamlDef name
            ⊚ v .:? "display_name" .!= ""
            ⊛ v .:  "sprite"
            ⊛ v .:? "weight"       .!= WeightFixed 0.0
            ⊛ pure bulk
            ⊛ v .:? "kind"         .!= "misc"
            ⊛ v .:? "category"     .!= "Misc"
            ⊛ v .:? "make"         .!= ""
            ⊛ v .:? "material"     .!= ""
            ⊛ v .:? "quality"
            ⊛ v .:? "condition"
            ⊛ v .:? "quality_tiers" .!= []
            ⊛ v .:? "container"
            ⊛ v .:? "contents"     .!= []
            ⊛ pure storage
            ⊛ v .:? "food"
            ⊛ v .:? "weapon"
            ⊛ v .:? "armor"
            ⊛ v .:? "unequippable" .!= False
            ⊛ v .:? "buffs"        .!= []
            ⊛ v .:? "insulation"   .!= 0.0

newtype ItemYamlFile = ItemYamlFile
    { iyfItems ∷ [ItemYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlFile where
    parseJSON = withObject "ItemYamlFile" $ \v → ItemYamlFile
        ⊚ v .: "items"

loadItemYaml ∷ LoggerState → FilePath → IO [ItemYamlDef]
loadItemYaml logger =
    loadYamlList logger "item" "item definitions" iyfItems
