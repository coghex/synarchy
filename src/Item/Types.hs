{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Item.Types
    ( ItemDef(..)
    , ItemContainer(..)
    , ItemFood(..)
    , ItemWeapon(..)
    , ItemInstance(..)
    , ItemManager(..)
    , emptyItemManager
    , lookupItemDef
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Engine.Asset.Handle (TextureHandle(..))

-- | Container properties — items with a Just here can hold a fluid
--   (water, future: lava/beer/etc). Non-containers (a hammer, food)
--   have Nothing.
data ItemContainer = ItemContainer
    { icCapacity ∷ !Float   -- ^ max volume (litres for water-class)
    , icHolds    ∷ !Text    -- ^ what fluid it holds: "water" / etc.
    } deriving (Show, Eq, Generic, Serialize)

-- | Weapon-specific stats. Combined with the item's `idMaterial`
--   substance properties at combat-time. Material lives at the top
--   level on ItemDef now (armor needs it too); this block only
--   carries weapon-shape stats and per-attack-type effectiveness.
data ItemWeapon = ItemWeapon
    { iwBladeLength    ∷ !Float   -- ^ cm
    , iwBaseSharpness  ∷ !Float   -- ^ engineering scale: lower = sharper.
                                  --   Effective sharpness is modulated
                                  --   by per-instance condition.
    , iwStabEff        ∷ !Float   -- ^ 0..1; how well this weapon shape
                                  --   delivers stabbing attacks.
    , iwSlashEff       ∷ !Float   -- ^ 0..1; slashing effectiveness.
    , iwBluntEff       ∷ !Float   -- ^ 0..1; blunt effectiveness.
    } deriving (Show, Eq)

-- | Food properties — items with a Just here restore hunger when
--   eaten. ifNutrition is the kcal value, clamped against the eater's
--   max_hunger (excess is wasted, item is still consumed).
data ItemFood = ItemFood
    { ifNutrition ∷ !Float   -- ^ kcal restored per item consumed
    } deriving (Show, Eq, Generic, Serialize)

-- | Immutable item definition — one per type loaded from YAML.
data ItemDef = ItemDef
    { idName        ∷ !Text             -- ^ unique key, e.g. "canteen_steel_2l"
    , idDisplayName ∷ !Text             -- ^ shown in UI
    , idTexture     ∷ !TextureHandle    -- ^ inventory sprite (UI use)
    , idWeight      ∷ !Float            -- ^ empty weight in kg
    , idKind        ∷ !Text             -- ^ equipment-slot kind ("weapon",
                                        --   "headwear", …). "misc" for
                                        --   non-equippable items. Matched
                                        --   against EquipmentSlot.esKind
                                        --   when equipping.
    , idCategory    ∷ !Text             -- ^ inventory-tab category
                                        --   ("Weapons", "Armor",
                                        --   "Supplies", "Misc"). Drives
                                        --   which dynamic tab the item
                                        --   appears under in the unit
                                        --   info inventory list. Default
                                        --   "Misc" — distinct from kind
                                        --   so a category can contain
                                        --   multiple kinds (Armor =
                                        --   helmet + gauntlets + …).
    , idMake        ∷ !Text             -- ^ design / crafting tradition
                                        --   the item descends from (e.g.
                                        --   "acolyte"). Empty for items
                                        --   without a known maker.
                                        --   Shown in the inventory
                                        --   tooltip's hint line.
    , idMaterial    ∷ !Text             -- ^ substance name (e.g. "steel",
                                        --   "leather"). Resolved against
                                        --   SubstanceManager when combat
                                        --   needs physical / resistance
                                        --   properties. Empty for items
                                        --   that don't have a single
                                        --   primary material (or that
                                        --   don't participate in combat).
    , idQualitySpec   ∷ !(Maybe (Float, Float))
      -- ^ (min, max) % range for quality rolls at spawn. Interpreted
      --   as a normal distribution centered at (min+max)/2 with
      --   stddev (max-min)/6, clamped to [min, max]. Nothing ⇒ spawn
      --   at 100%.
    , idConditionSpec ∷ !(Maybe (Float, Float))
      -- ^ (min, max) % range for condition rolls at spawn. Same
      --   distribution shape as quality. Condition degrades with use;
      --   quality is fixed for the item's lifetime.
    , idContainer   ∷ !(Maybe ItemContainer)
    , idFood        ∷ !(Maybe ItemFood)
    , idWeapon      ∷ !(Maybe ItemWeapon)
      -- ^ Weapon stats for items with kind="weapon". Nothing for
      --   everything else.
    } deriving (Show, Eq)

-- | Per-unit instance. References its def by name; currentFill is for
--   containers, quality is rolled once at spawn (immutable), condition
--   is rolled at spawn and degrades with use.
data ItemInstance = ItemInstance
    { iiDefName     ∷ !Text
    , iiCurrentFill ∷ !Float    -- ^ litres held; 0 for non-containers
    , iiQuality     ∷ !Float    -- ^ 0..100; how well-made this instance
                                --   is. Multiplicative on effective
                                --   sharpness / damage / armor value.
    , iiCondition   ∷ !Float    -- ^ 0..100; current wear. Degrades with
                                --   use. 0 = broken.
    } deriving (Show, Eq, Generic, Serialize)

-- | Engine-wide registry of all loaded item defs.
newtype ItemManager = ItemManager
    { imDefs ∷ HM.HashMap Text ItemDef
    } deriving (Show, Eq)

emptyItemManager ∷ ItemManager
emptyItemManager = ItemManager HM.empty

lookupItemDef ∷ Text → ItemManager → Maybe ItemDef
lookupItemDef name (ItemManager m) = HM.lookup name m
