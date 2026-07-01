{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric #-}
module LootTable.Types
    ( LootTableEntry(..)
    , LootTableDef(..)
    , LootTableRegistry(..)
    , emptyLootTableRegistry
    , registerLootTable
    , lookupLootTable
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM

-- | One weighted entry in a loot table — an item def id and its
--   relative weight (not a probability; weights are summed and scaled
--   at roll time, see 'LootTable.Roll.rollLootTable').
data LootTableEntry = LootTableEntry
    { lteId     ∷ !Text
    , lteWeight ∷ !Float
    } deriving (Show, Eq, Generic)

-- | A named loot table (data/loot_tables/*.yaml), rolled by a
--   `loot_table` location content entry (#90).
data LootTableDef = LootTableDef
    { ltdId      ∷ !Text
    , ltdEntries ∷ ![LootTableEntry]
    } deriving (Show, Eq, Generic)

-- | Engine-wide registry of loot table defs loaded from
--   data/loot_tables/. One def per file (unlike the location/item/unit
--   registries, which allow several defs per file) — each file's `id`
--   is the whole document, not a member of a wrapping list.
newtype LootTableRegistry = LootTableRegistry
    { ltrDefs ∷ HM.HashMap Text LootTableDef
    } deriving (Show, Eq)

emptyLootTableRegistry ∷ LootTableRegistry
emptyLootTableRegistry = LootTableRegistry HM.empty

registerLootTable ∷ LootTableDef → LootTableRegistry → LootTableRegistry
registerLootTable def (LootTableRegistry defs) =
    LootTableRegistry (HM.insert (ltdId def) def defs)

lookupLootTable ∷ Text → LootTableRegistry → Maybe LootTableDef
lookupLootTable tid (LootTableRegistry defs) = HM.lookup tid defs
