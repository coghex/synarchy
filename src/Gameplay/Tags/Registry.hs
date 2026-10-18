-- | The sparse gameplay tag registry (#2700, epic #2698 SCN-02).
--
--   Tag assignments live HERE, not on the objects: an untagged unit,
--   building, location, plant, item or tile has no entry at all, and no
--   object record carries a tag field. Each of the six categories keeps
--   two ordered indexes:
--
--     [forward] target → its nonempty tag set. This is the AUTHORITY.
--     [reverse] tag → its nonempty target set. Always derivable from
--       the forward map; kept so a tag's members are one lookup away.
--
--   Each category is a "Gameplay.Tags.Index" 'CategoryIndex', whose one
--   mutation updates both directions together and drops a record the
--   moment it would become empty, so the two indexes cannot disagree
--   and no empty record can survive. 'fromAssignments' takes forward
--   assignments only and DERIVES the reverse index, so a caller can
--   never supply a competing authority.
--
--   This is a pure, in-memory value. Since #2701 the unit, building and
--   location categories are OWNED by live state — the unit and building
--   managers and each page's location table — and a live query
--   assembles a registry from them with 'registryFromCategories'
--   ("Gameplay.Tags.Live"). Plants, items, tiles and Lua access are
--   later integration work; see @docs\/gameplay_tags.md@.
module Gameplay.Tags.Registry
    ( -- * The registry
      TagRegistry
    , emptyTagRegistry
    , nullTagRegistry
      -- * Mutation
    , addTags
    , addTag
    , removeTags
    , removeTag
    , replaceTags
    , clearTags
      -- * Inspection
    , tagsOf
    , hasTag
    , assignments
      -- * Rebuild and invariants
    , fromAssignments
    , registryFromCategories
    , registryInvariantViolations
    , RegistryCounts(..)
    , registryCounts
      -- * Per-category indexes (read-only, for the query evaluator)
    , CategoryIndex
    , indexMembers
    , registryUnits
    , registryBuildings
    , registryLocations
    , registryPlants
    , registryItems
    , registryTiles
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Set as Set
import Building.Types (BuildingId(..))
import Gameplay.Tags.Index
import Gameplay.Tags.Types
import Unit.Types.Manager (UnitId)

-- * Per-category index

-- | What is wrong with one index, if anything. The index type itself
--   lives in "Gameplay.Tags.Index" (#2701), where it is just as
--   abstract.
indexViolations ∷ Ord k ⇒ TagCategory → CategoryIndex k → [Text]
indexViolations cat ix =
    [ tshow cat <> ": reverse index disagrees with the forward assignments"
    | categoryIncoherent ix ]

-- * The registry

-- | Every gameplay tag assignment, separated by category. 'Eq' is
--   structural, and two registries holding the same assignments are
--   structurally equal however they were built.
data TagRegistry = TagRegistry
    { trUnits     ∷ !(CategoryIndex UnitId)
    , trBuildings ∷ !(CategoryIndex BuildingId)
    , trLocations ∷ !(CategoryIndex LocationTarget)
    , trPlants    ∷ !(CategoryIndex PlantTarget)
    , trItems     ∷ !(CategoryIndex ItemTarget)
    , trTiles     ∷ !(CategoryIndex TileTarget)
    } deriving (Show, Eq)

-- | 'BuildingId' has no 'NFData' instance of its own. It is a newtype
--   over 'Word32' and map keys and set elements are stored in WHNF, so
--   forcing the building index's values and reverse sets is enough.
instance NFData TagRegistry where
    rnf (TagRegistry u b l p i t) =
        rnf u `seq` rnf (mapCategoryKeysMonotonic unBuildingId b)
        `seq` rnf l `seq` rnf p `seq` rnf i `seq` rnf t

emptyTagRegistry ∷ TagRegistry
emptyTagRegistry = TagRegistry emptyCategoryIndex emptyCategoryIndex
                               emptyCategoryIndex emptyCategoryIndex
                               emptyCategoryIndex emptyCategoryIndex

-- | The registry a live query runs against: every category's authority
--   lives with an owner — the unit and building managers, each page's
--   location table (#2701), the session's item index (#2703), and each
--   page's tile, occurrence and plot stores (#2705).
registryFromCategories ∷ CategoryIndex UnitId → CategoryIndex BuildingId
                       → CategoryIndex LocationTarget → CategoryIndex ItemTarget
                       → CategoryIndex PlantTarget → CategoryIndex TileTarget
                       → TagRegistry
registryFromCategories u b l i p t = emptyTagRegistry
    { trUnits = u, trBuildings = b, trLocations = l, trItems = i
    , trPlants = p, trTiles = t }

-- | No assignments at all.
nullTagRegistry ∷ TagRegistry → Bool
nullTagRegistry r = r ≡ emptyTagRegistry

registryUnits     ∷ TagRegistry → CategoryIndex UnitId
registryUnits     = trUnits
registryBuildings ∷ TagRegistry → CategoryIndex BuildingId
registryBuildings = trBuildings
registryLocations ∷ TagRegistry → CategoryIndex LocationTarget
registryLocations = trLocations
registryPlants    ∷ TagRegistry → CategoryIndex PlantTarget
registryPlants    = trPlants
registryItems     ∷ TagRegistry → CategoryIndex ItemTarget
registryItems     = trItems
registryTiles     ∷ TagRegistry → CategoryIndex TileTarget
registryTiles     = trTiles

-- | Rewrite one target's tag set from its current one. Every public
--   mutation is this with a different set function.
modifyTargetTags ∷ (Set.Set GameplayTag → Set.Set GameplayTag)
                 → TagTarget → TagRegistry → TagRegistry
modifyTargetTags f target r = case target of
    TargetUnit k     → r { trUnits     = go k (trUnits r) }
    TargetBuilding k → r { trBuildings = go k (trBuildings r) }
    TargetLocation k → r { trLocations = go k (trLocations r) }
    TargetPlant k    → r { trPlants    = go k (trPlants r) }
    TargetItem k     → r { trItems     = go k (trItems r) }
    TargetTile k     → r { trTiles     = go k (trTiles r) }
  where
    go ∷ Ord k ⇒ k → CategoryIndex k → CategoryIndex k
    go = modifyCategoryTags f

-- | Add every tag in the set; tags the target already carries are
--   unchanged (membership is a set, so repeats are idempotent).
addTags ∷ TagTarget → Set.Set GameplayTag → TagRegistry → TagRegistry
addTags target ts = modifyTargetTags (Set.union ts) target

addTag ∷ TagTarget → GameplayTag → TagRegistry → TagRegistry
addTag target t = addTags target (Set.singleton t)

-- | Remove every tag in the set; tags the target does not carry are
--   ignored. Removing the last tag removes the target's entry.
removeTags ∷ TagTarget → Set.Set GameplayTag → TagRegistry → TagRegistry
removeTags target ts = modifyTargetTags (`Set.difference` ts) target

removeTag ∷ TagTarget → GameplayTag → TagRegistry → TagRegistry
removeTag target t = removeTags target (Set.singleton t)

-- | Make the set the target's complete tag set. The empty set is
--   'clearTags'.
replaceTags ∷ TagTarget → Set.Set GameplayTag → TagRegistry → TagRegistry
replaceTags target ts = modifyTargetTags (const ts) target

-- | Remove every tag and the target's entry. Other targets sharing
--   those tags keep them.
clearTags ∷ TagTarget → TagRegistry → TagRegistry
clearTags target = modifyTargetTags (const Set.empty) target

-- | A target's tags; empty for an untagged target.
tagsOf ∷ TagTarget → TagRegistry → Set.Set GameplayTag
tagsOf target r = case target of
    TargetUnit k     → categoryTagsOf k (trUnits r)
    TargetBuilding k → categoryTagsOf k (trBuildings r)
    TargetLocation k → categoryTagsOf k (trLocations r)
    TargetPlant k    → categoryTagsOf k (trPlants r)
    TargetItem k     → categoryTagsOf k (trItems r)
    TargetTile k     → categoryTagsOf k (trTiles r)

hasTag ∷ TagTarget → GameplayTag → TagRegistry → Bool
hasTag target t r = Set.member t (tagsOf target r)

-- | The authoritative forward assignments, in the documented result
--   order (category, then key). Every set is nonempty.
assignments ∷ TagRegistry → [(TagTarget, Set.Set GameplayTag)]
assignments r = concat
    [ part TargetUnit     (trUnits r)
    , part TargetBuilding (trBuildings r)
    , part TargetLocation (trLocations r)
    , part TargetPlant    (trPlants r)
    , part TargetItem     (trItems r)
    , part TargetTile     (trTiles r) ]
  where
    part ∷ (k → TagTarget) → CategoryIndex k → [(TagTarget, Set.Set GameplayTag)]
    part wrap ix = [ (wrap k, ts) | (k, ts) ← categoryAssignments ix ]

-- | Build a registry from forward assignments alone, deriving every
--   reverse index. A target listed more than once receives the union of
--   its sets; empty sets contribute nothing. For any registry @r@,
--   @fromAssignments (assignments r) ≡ r@.
fromAssignments ∷ [(TagTarget, Set.Set GameplayTag)] → TagRegistry
fromAssignments xs = TagRegistry
    { trUnits     = categoryFromAssignments [ (k, ts) | (TargetUnit k, ts) ← xs ]
    , trBuildings = categoryFromAssignments [ (k, ts) | (TargetBuilding k, ts) ← xs ]
    , trLocations = categoryFromAssignments [ (k, ts) | (TargetLocation k, ts) ← xs ]
    , trPlants    = categoryFromAssignments [ (k, ts) | (TargetPlant k, ts) ← xs ]
    , trItems     = categoryFromAssignments [ (k, ts) | (TargetItem k, ts) ← xs ]
    , trTiles     = categoryFromAssignments [ (k, ts) | (TargetTile k, ts) ← xs ]
    }

-- | Every broken index invariant, as text; empty when the registry is
--   coherent. The public API cannot produce a violation — this exists
--   so tests can say so.
registryInvariantViolations ∷ TagRegistry → [Text]
registryInvariantViolations r = concat
    [ indexViolations UnitCategory     (trUnits r)
    , indexViolations BuildingCategory (trBuildings r)
    , indexViolations LocationCategory (trLocations r)
    , indexViolations PlantCategory    (trPlants r)
    , indexViolations ItemCategory     (trItems r)
    , indexViolations TileCategory     (trTiles r) ]

-- | Structural size of one category: forward entries (tagged targets),
--   reverse entries (distinct tags in use) and memberships
--   (target, tag) pairs.
data RegistryCounts = RegistryCounts
    { rcForwardEntries ∷ !Int
    , rcReverseEntries ∷ !Int
    , rcMemberships    ∷ !Int
    } deriving (Show, Eq)

-- | 'RegistryCounts' per category, in category order.
registryCounts ∷ TagRegistry → [(TagCategory, RegistryCounts)]
registryCounts r =
    [ (UnitCategory,     counts (trUnits r))
    , (BuildingCategory, counts (trBuildings r))
    , (LocationCategory, counts (trLocations r))
    , (PlantCategory,    counts (trPlants r))
    , (ItemCategory,     counts (trItems r))
    , (TileCategory,     counts (trTiles r)) ]
  where
    counts ∷ CategoryIndex k → RegistryCounts
    counts ix = let c = categoryCounts ix in RegistryCounts
        { rcForwardEntries = ccForwardEntries c
        , rcReverseEntries = ccReverseEntries c
        , rcMemberships    = ccMemberships c
        }
