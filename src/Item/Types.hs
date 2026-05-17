{-# LANGUAGE Strict, UnicodeSyntax #-}
module Item.Types
    ( ItemDef(..)
    , ItemContainer(..)
    , ItemFood(..)
    , ItemInstance(..)
    , ItemManager(..)
    , emptyItemManager
    , lookupItemDef
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))

-- | Container properties — items with a Just here can hold a fluid
--   (water, future: lava/beer/etc). Non-containers (a hammer, food)
--   have Nothing.
data ItemContainer = ItemContainer
    { icCapacity ∷ !Float   -- ^ max volume (litres for water-class)
    , icHolds    ∷ !Text    -- ^ what fluid it holds: "water" / etc.
    } deriving (Show, Eq)

-- | Food properties — items with a Just here restore hunger when
--   eaten. ifNutrition is the kcal value, clamped against the eater's
--   max_hunger (excess is wasted, item is still consumed).
data ItemFood = ItemFood
    { ifNutrition ∷ !Float   -- ^ kcal restored per item consumed
    } deriving (Show, Eq)

-- | Immutable item definition — one per type loaded from YAML.
data ItemDef = ItemDef
    { idName        ∷ !Text             -- ^ unique key, e.g. "canteen_steel_2l"
    , idDisplayName ∷ !Text             -- ^ shown in UI
    , idTexture     ∷ !TextureHandle    -- ^ inventory sprite (UI use)
    , idWeight      ∷ !Float            -- ^ empty weight in kg
    , idContainer   ∷ !(Maybe ItemContainer)
    , idFood        ∷ !(Maybe ItemFood)
    } deriving (Show, Eq)

-- | Per-unit instance. References its def by name; currentFill is the
--   only mutable field for v1 (durability, custom names, charges land
--   later as the system grows).
data ItemInstance = ItemInstance
    { iiDefName     ∷ !Text
    , iiCurrentFill ∷ !Float    -- ^ litres held; 0 for non-containers
    } deriving (Show, Eq)

-- | Engine-wide registry of all loaded item defs.
newtype ItemManager = ItemManager
    { imDefs ∷ HM.HashMap Text ItemDef
    } deriving (Show, Eq)

emptyItemManager ∷ ItemManager
emptyItemManager = ItemManager HM.empty

lookupItemDef ∷ Text → ItemManager → Maybe ItemDef
lookupItemDef name (ItemManager m) = HM.lookup name m
