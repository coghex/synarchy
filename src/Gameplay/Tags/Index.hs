-- | One category's gameplay tag index (#2700), split out of
--   "Gameplay.Tags.Registry" by #2701 so a live owner can hold its own
--   category without importing the target vocabulary.
--
--   An index keeps two ordered maps:
--
--     [forward] key → its nonempty tag set. This is the AUTHORITY.
--     [reverse] tag → its nonempty key set. Always derivable from the
--       forward map; kept so a tag's members are one lookup away.
--
--   'CategoryIndex' is abstract. Every mutation reduces to
--   'setCategoryTags', which updates both directions together and drops
--   a record the moment it would become empty, so the two maps cannot
--   disagree and no empty record can survive. The rebuild path,
--   'categoryFromAssignments', takes forward assignments only and
--   DERIVES the reverse map, so no caller can supply a competing
--   authority.
--
--   The key type is the owner's own identity: 'Unit.Types.Manager.UnitId'
--   in 'Unit.Types.Manager.UnitManager', 'Building.Types.BuildingId' in
--   'Building.Types.BuildingManager', and the page-local
--   'Location.Instance.LocationInstanceId' in a page's
--   'Location.Instance.LocationInstances'. "Gameplay.Tags.Registry"
--   assembles them into one queryable 'Gameplay.Tags.Registry.TagRegistry'.
module Gameplay.Tags.Index
    ( CategoryIndex
    , emptyCategoryIndex
    , nullCategoryIndex
      -- * Mutation
    , setCategoryTags
    , modifyCategoryTags
    , deleteCategoryTarget
    , restrictCategoryIndex
      -- * Inspection
    , categoryTagsOf
    , indexMembers
    , categoryForward
    , categoryAssignments
    , categoryTargets
      -- * Rebuild, combination and invariants
    , categoryFromAssignments
    , mapCategoryKeysMonotonic
    , unionDisjointCategories
    , categoryIncoherent
    , CategoryCounts(..)
    , categoryCounts
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gameplay.Tags.Tag

-- | One category's two indexes. Abstract: only this module can change
--   either map, and only through 'setCategoryTags'.
data CategoryIndex k = CategoryIndex
    { ciForward ∷ !(Map.Map k (NonEmptySet GameplayTag))
    , ciReverse ∷ !(Map.Map GameplayTag (NonEmptySet k))
    } deriving (Show, Eq)

instance NFData k ⇒ NFData (CategoryIndex k) where
    rnf (CategoryIndex f r) = rnf f `seq` rnf r

emptyCategoryIndex ∷ CategoryIndex k
emptyCategoryIndex = CategoryIndex Map.empty Map.empty

-- | No assignments at all.
nullCategoryIndex ∷ CategoryIndex k → Bool
nullCategoryIndex = Map.null . ciForward

-- * Mutation

-- | Set one key's complete tag set, updating both maps. The one
--   mutation every other operation reduces to. The empty set removes
--   the key's entry.
setCategoryTags ∷ Ord k ⇒ k → Set.Set GameplayTag → CategoryIndex k → CategoryIndex k
setCategoryTags k new ix
    | new ≡ old = ix
    | otherwise = CategoryIndex fwd' rev'
  where
    old     = categoryTagsOf k ix
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

-- | Rewrite one key's tag set from its current one.
modifyCategoryTags ∷ Ord k
                   ⇒ (Set.Set GameplayTag → Set.Set GameplayTag)
                   → k → CategoryIndex k → CategoryIndex k
modifyCategoryTags f k ix = setCategoryTags k (f (categoryTagsOf k ix)) ix

-- | Remove a key's entry and its reverse memberships. Other keys that
--   share those tags keep them.
deleteCategoryTarget ∷ Ord k ⇒ k → CategoryIndex k → CategoryIndex k
deleteCategoryTarget k = setCategoryTags k Set.empty

-- | Keep only the keys the predicate accepts — the bulk removal a page
--   retirement or a load scrub performs. Untouched when nothing is
--   removed, so the common no-op costs one traversal and no rebuild.
restrictCategoryIndex ∷ Ord k ⇒ (k → Bool) → CategoryIndex k → CategoryIndex k
restrictCategoryIndex keep ix
    | Map.null dropped = ix
    | otherwise = foldl' (flip deleteCategoryTarget) ix (Map.keys dropped)
  where
    dropped = Map.filterWithKey (\k _ → not (keep k)) (ciForward ix)

-- * Inspection

-- | A key's tags; empty for an untagged key.
categoryTagsOf ∷ Ord k ⇒ k → CategoryIndex k → Set.Set GameplayTag
categoryTagsOf k ix = maybe Set.empty nonEmptySetToSet (Map.lookup k (ciForward ix))

-- | The members of one tag; empty for a tag nobody carries (a missing
--   tag name denotes the empty set).
indexMembers ∷ Ord k ⇒ GameplayTag → CategoryIndex k → Set.Set k
indexMembers t ix = maybe Set.empty nonEmptySetToSet (Map.lookup t (ciReverse ix))

-- | The authoritative forward map. Every set is nonempty.
categoryForward ∷ CategoryIndex k → Map.Map k (Set.Set GameplayTag)
categoryForward = Map.map nonEmptySetToSet . ciForward

-- | The authoritative forward assignments, ascending by key.
categoryAssignments ∷ CategoryIndex k → [(k, Set.Set GameplayTag)]
categoryAssignments = Map.toAscList . categoryForward

-- | Every tagged key.
categoryTargets ∷ CategoryIndex k → Set.Set k
categoryTargets = Map.keysSet . ciForward

-- * Rebuild, combination and invariants

-- | The reverse map a forward map implies.
deriveReverse ∷ Ord k
              ⇒ Map.Map k (NonEmptySet GameplayTag)
              → Map.Map GameplayTag (NonEmptySet k)
deriveReverse fwd =
    Map.mapMaybe nonEmptySet $ Map.fromListWith Set.union
        [ (t, Set.singleton k)
        | (k, ts) ← Map.toList fwd
        , t ← Set.toList (nonEmptySetToSet ts) ]

-- | Build an index from forward assignments alone, deriving the reverse
--   map. A key listed more than once receives the union of its sets;
--   empty sets contribute nothing. For any index @ix@,
--   @categoryFromAssignments (categoryAssignments ix) ≡ ix@.
categoryFromAssignments ∷ Ord k ⇒ [(k, Set.Set GameplayTag)] → CategoryIndex k
categoryFromAssignments xs =
    let fwd = Map.mapMaybe nonEmptySet (Map.fromListWith Set.union xs)
    in CategoryIndex fwd (deriveReverse fwd)

-- | Re-key an index. The function MUST be strictly monotonic (it is
--   used to lift one page's 'Location.Instance.LocationInstanceId' keys
--   into that page's 'Gameplay.Tags.Types.LocationTarget's), which is
--   what lets both maps keep their structure without a re-sort.
mapCategoryKeysMonotonic ∷ (k → k') → CategoryIndex k → CategoryIndex k'
mapCategoryKeysMonotonic f (CategoryIndex fwd rev) = CategoryIndex
    (Map.mapKeysMonotonic f fwd)
    (Map.map (mapNonEmptySetMonotonic f) rev)

-- | Combine indexes whose key sets are pairwise DISJOINT (distinct
--   pages' locations). A key present in more than one input receives
--   the union of its sets, so a violated precondition still yields a
--   coherent index rather than a corrupt one.
unionDisjointCategories ∷ Ord k ⇒ [CategoryIndex k] → CategoryIndex k
unionDisjointCategories [ix] = ix
unionDisjointCategories ixs  = categoryFromAssignments (concatMap categoryAssignments ixs)

-- | Whether the reverse map disagrees with the one the forward map
--   implies. Empty records are unrepresentable ('NonEmptySet'), so this
--   is the only invariant the public operations could break, and none
--   of them does.
categoryIncoherent ∷ Ord k ⇒ CategoryIndex k → Bool
categoryIncoherent ix = ciReverse ix ≢ deriveReverse (ciForward ix)

-- | Structural size: forward entries (tagged keys), reverse entries
--   (distinct tags in use) and memberships ((key, tag) pairs).
data CategoryCounts = CategoryCounts
    { ccForwardEntries ∷ !Int
    , ccReverseEntries ∷ !Int
    , ccMemberships    ∷ !Int
    } deriving (Show, Eq)

categoryCounts ∷ CategoryIndex k → CategoryCounts
categoryCounts ix = CategoryCounts
    { ccForwardEntries = Map.size (ciForward ix)
    , ccReverseEntries = Map.size (ciReverse ix)
    , ccMemberships    = sum (Set.size . nonEmptySetToSet <$> Map.elems (ciForward ix))
    }
