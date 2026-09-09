{-# LANGUAGE Strict, DeriveGeneric #-}
-- | Loot PROFILES (#2499, epic #1231 PLC-12) — the contextual cargo
--   distribution a portable container realizes from (design D-2, D-6).
--
--   Deliberately NOT a second spelling of "LootTable.Types". A loot
--   TABLE is one weighted draw: exactly one of its entries wins, and a
--   weight is relative to its siblings. A loot PROFILE rolls every
--   entry INDEPENDENTLY against its own absolute appearance @chance@,
--   and each entry that appears contributes a lot sized by the
--   profile's @quantity_multiplier@ and the entry's own
--   @quantity_factor@. Nothing of the entry shape or the roll carries
--   over, which is why this is its own registry rather than a variant
--   of the loot-table one.
--
--   Pure authored data. Rolling, lot admission and realization are
--   PLC-13's, and nothing consumes a profile yet.
module LootProfile.Types
    ( LootProfileEntry(..)
    , LootProfileDef(..)
    , LootProfileRegistry(..)
    , emptyLootProfileRegistry
    , registerLootProfile
    , lookupLootProfile
    , lootProfileIds
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM

-- | One entry of a loot profile: an item def id, the INDEPENDENT
--   probability that this entry appears at all (@0@–@1@ inclusive), and
--   the whole-number factor sizing the lot when it does.
--
--   @lpeChance@ is absolute, unlike 'LootTable.Types.lteWeight': two
--   entries at @0.30@ each appear 30% of the time, independently of
--   one another and of how many entries the profile has.
data LootProfileEntry = LootProfileEntry
    { lpeItem           ∷ !Text
    , lpeChance         ∷ !Float
    , lpeQuantityFactor ∷ !Int
    } deriving (Show, Eq, Generic)

-- | A named loot profile (@data/loot_profiles/*.yaml@). @lpdEntries@
--   keeps the AUTHORED order: PLC-13's admission shuffles a seeded copy
--   of it, so the authored sequence is the stable input that shuffle is
--   a function of, and reordering the file is a content change.
--
--   The multiplier bounds are an inclusive whole-number range with
--   @1 ≤ min ≤ max@ — kept as two fields rather than a pair so the
--   invariant is visible at every use site.
data LootProfileDef = LootProfileDef
    { lpdId            ∷ !Text
    , lpdMultiplierMin ∷ !Int
    , lpdMultiplierMax ∷ !Int
    , lpdEntries       ∷ ![LootProfileEntry]
    } deriving (Show, Eq, Generic)

-- | Engine-wide registry of loot profile defs loaded from
--   @data/loot_profiles/@. One def per file, exactly like
--   'LootTable.Types.LootTableRegistry' — each file's @id@ IS the whole
--   document, not a member of a wrapping list.
newtype LootProfileRegistry = LootProfileRegistry
    { lprDefs ∷ HM.HashMap Text LootProfileDef
    } deriving (Show, Eq)

emptyLootProfileRegistry ∷ LootProfileRegistry
emptyLootProfileRegistry = LootProfileRegistry HM.empty

-- | Insert or REPLACE by profile id, the same policy
--   'LootTable.Types.registerLootTable' follows: the @engine.load*Yaml@
--   verbs stay publicly callable and keep insert/replace semantics, so
--   a second file (or a second call) naming an existing profile wins.
--   The caller logs the replacement; nothing here is silent about it
--   because nothing here can see that a previous def existed.
registerLootProfile ∷ LootProfileDef → LootProfileRegistry → LootProfileRegistry
registerLootProfile def (LootProfileRegistry defs) =
    LootProfileRegistry (HM.insert (lpdId def) def defs)

lookupLootProfile ∷ Text → LootProfileRegistry → Maybe LootProfileDef
lookupLootProfile pid (LootProfileRegistry defs) = HM.lookup pid defs

-- | Every registered profile id, ASCENDING. A 'HM.HashMap' has no
--   meaningful traversal order, so the sort is what makes
--   @loot.listProfiles()@ answer the same list twice in one session and
--   across two processes.
lootProfileIds ∷ LootProfileRegistry → [Text]
lootProfileIds (LootProfileRegistry defs) = sort (HM.keys defs)
