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
--   Every mutation goes through 'setTargetTags', which updates both
--   directions together and drops a record the moment it would become
--   empty, so the two indexes cannot disagree and no empty record can
--   survive. 'fromAssignments' is the rebuild path a later persistence
--   layer uses: it takes forward assignments only and DERIVES the
--   reverse index, so a caller can never supply a competing authority.
--
--   This is a pure, in-memory value. Runtime ownership, persistence,
--   lifecycle hooks and Lua access are later integration work; see
--   @docs\/gameplay_tags.md@.
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Building.Types (BuildingId(..))
import Gameplay.Tags.Types
import Unit.Types.Manager (UnitId)

-- * Per-category index

-- | One category's two indexes. Abstract: only this module can change
--   either map, and only through 'setIndexTags'.
data CategoryIndex k = CategoryIndex
    { ciForward ∷ !(Map.Map k (NonEmptySet GameplayTag))
    , ciReverse ∷ !(Map.Map GameplayTag (NonEmptySet k))
    } deriving (Show, Eq)

emptyIndex ∷ CategoryIndex k
emptyIndex = CategoryIndex Map.empty Map.empty

-- | The members of one tag in one category; empty for a tag nobody
--   carries (a missing tag name denotes the empty set).
indexMembers ∷ Ord k ⇒ GameplayTag → CategoryIndex k → Set.Set k
indexMembers t ix = maybe Set.empty nonEmptySetToSet (Map.lookup t (ciReverse ix))

indexTags ∷ Ord k ⇒ k → CategoryIndex k → Set.Set GameplayTag
indexTags k ix = maybe Set.empty nonEmptySetToSet (Map.lookup k (ciForward ix))

-- | Set one target's complete tag set, updating both indexes. The one
--   mutation every public operation reduces to.
setIndexTags ∷ Ord k ⇒ k → Set.Set GameplayTag → CategoryIndex k → CategoryIndex k
setIndexTags k new ix
    | new ≡ old = ix
    | otherwise = CategoryIndex fwd' rev'
  where
    old     = indexTags k ix
    fwd'    = case nonEmptySet new of
        Nothing → Map.delete k (ciForward ix)
        Just ne → Map.insert k ne (ciForward ix)
    removed = Set.difference old new
    added   = Set.difference new old
    rev1    = foldl' (\m t → Map.update (deleteNonEmptySet k) t m)
                     (ciReverse ix) (Set.toList removed)
    rev'    = foldl' (\m t → Map.alter (Just . addMember) t m)
                     rev1 (Set.toList added)
    addMember = maybe (singletonNonEmptySet k) (insertNonEmptySet k)

-- | The reverse index a forward map implies.
deriveReverse ∷ Ord k
              ⇒ Map.Map k (NonEmptySet GameplayTag)
              → Map.Map GameplayTag (NonEmptySet k)
deriveReverse fwd =
    Map.mapMaybe nonEmptySet $ Map.fromListWith Set.union
        [ (t, Set.singleton k)
        | (k, ts) ← Map.toList fwd
        , t ← Set.toList (nonEmptySetToSet ts) ]

indexFromForward ∷ Ord k ⇒ Map.Map k (Set.Set GameplayTag) → CategoryIndex k
indexFromForward raw =
    let fwd = Map.mapMaybe nonEmptySet raw
    in CategoryIndex fwd (deriveReverse fwd)

-- | What is wrong with one index, if anything: a forward/reverse
--   disagreement. Empty records are unrepresentable ('NonEmptySet').
indexViolations ∷ (Ord k, Show k) ⇒ TagCategory → CategoryIndex k → [Text]
indexViolations cat ix =
    [ tshow cat <> ": reverse index disagrees with the forward assignments"
    | ciReverse ix ≢ deriveReverse (ciForward ix) ]

instance NFData k ⇒ NFData (CategoryIndex k) where
    rnf (CategoryIndex f r) = rnf f `seq` rnf r

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
        rnf u `seq` rnfBuildings b `seq` rnf l `seq` rnf p `seq` rnf i `seq` rnf t
      where
        rnfBuildings (CategoryIndex f r) =
            rnf (Map.elems f) `seq` rnf (Map.keys r)
            `seq` rnf (fmap unBuildingId . Set.toList . nonEmptySetToSet
                         <$> Map.elems r)

emptyTagRegistry ∷ TagRegistry
emptyTagRegistry = TagRegistry emptyIndex emptyIndex emptyIndex
                               emptyIndex emptyIndex emptyIndex

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
    go k ix = setIndexTags k (f (indexTags k ix)) ix

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
    TargetUnit k     → indexTags k (trUnits r)
    TargetBuilding k → indexTags k (trBuildings r)
    TargetLocation k → indexTags k (trLocations r)
    TargetPlant k    → indexTags k (trPlants r)
    TargetItem k     → indexTags k (trItems r)
    TargetTile k     → indexTags k (trTiles r)

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
    part wrap ix = [ (wrap k, nonEmptySetToSet ts)
                   | (k, ts) ← Map.toAscList (ciForward ix) ]

-- | Build a registry from forward assignments alone, deriving every
--   reverse index. A target listed more than once receives the union of
--   its sets; empty sets contribute nothing. For any registry @r@,
--   @fromAssignments (assignments r) ≡ r@.
fromAssignments ∷ [(TagTarget, Set.Set GameplayTag)] → TagRegistry
fromAssignments xs = TagRegistry
    { trUnits     = indexFromForward (collect [ (k, ts) | (TargetUnit k, ts) ← xs ])
    , trBuildings = indexFromForward (collect [ (k, ts) | (TargetBuilding k, ts) ← xs ])
    , trLocations = indexFromForward (collect [ (k, ts) | (TargetLocation k, ts) ← xs ])
    , trPlants    = indexFromForward (collect [ (k, ts) | (TargetPlant k, ts) ← xs ])
    , trItems     = indexFromForward (collect [ (k, ts) | (TargetItem k, ts) ← xs ])
    , trTiles     = indexFromForward (collect [ (k, ts) | (TargetTile k, ts) ← xs ])
    }
  where
    collect ∷ Ord k ⇒ [(k, Set.Set GameplayTag)] → Map.Map k (Set.Set GameplayTag)
    collect = Map.fromListWith Set.union

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
    counts ix = RegistryCounts
        { rcForwardEntries = Map.size (ciForward ix)
        , rcReverseEntries = Map.size (ciReverse ix)
        , rcMemberships    = sum (Set.size . nonEmptySetToSet <$> Map.elems (ciForward ix))
        }
