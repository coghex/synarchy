-- | Live gameplay tags for units, buildings and locations (#2701, epic
--   #2698 SCN-03): the internal Haskell operations that edit, inspect
--   and query the assignments their OWNERS hold.
--
--   === Ownership
--
--   Each category's authoritative assignments live with the state that
--   owns the objects, as a "Gameplay.Tags.Index" 'CategoryIndex'
--   (capability inventory §6.4(a)) — never on an instance and never on
--   'Engine.Core.State.EngineEnv':
--
--     [units] 'Unit.Types.Manager.umTags', beside 'umInstances'.
--     [buildings] 'Building.Types.bmTags', beside 'bmInstances'.
--     [locations] 'Location.Instance.lisTags', beside each page's
--       instance table inside its 'World.Generate.Types.WorldGenParams'.
--
--   So the owners' existing synchronization IS the tags'
--   synchronization. Every write here is ONE 'atomicModifyIORef'' of
--   the owning ref that checks the target exists and edits its tags in
--   the same step — the same discipline every other writer of those
--   refs already follows — so an edit can neither race an instance's
--   removal nor create an assignment for a target that does not exist
--   (it answers 'TagEditNoTarget' and changes nothing). The removal
--   paths clear tags inside their own modify: 'Unit.Types.Manager.retirePageUnits',
--   'Building.Types.retirePageBuildings', the destroy and clear-all
--   handlers, and the load publish that replaces the managers whole. A
--   location's tags leave with its page's table.
--
--   A location edit resolves its page ONCE, through
--   'World.State.Types.wmWorlds', and modifies that page's own
--   'wsGenParamsRef'. Every page initialization builds a fresh
--   'World.State.Types.WorldState', so a write that resolved a page
--   before it was destroyed or replaced lands on the departed
--   incarnation and can never reach the replacement.
--
--   === Consistent reads
--
--   'captureLiveTagView' reads each owner ref exactly once. Within a
--   category the forward and reverse indexes and the query universe
--   therefore come from one value, so a query never sees a tag whose
--   target is outside its universe or an index pair mid-update. Across
--   categories (and across pages) the reads are separate: the view is
--   per-owner consistent, not one global instant, which is the same
--   guarantee every other cross-manager read in the engine has.
--
--   Nothing here is reachable from Lua; the public verbs are SCN-06.
module Gameplay.Tags.Live
    ( -- * Edits
      TagEdit(..)
    , applyTagEdit
    , TagEditResult(..)
      -- * Pure owner transitions
    , editUnitTags
    , editBuildingTags
    , editLocationTags
      -- * Pure inspection
    , unitTagsIn
    , buildingTagsIn
    , locationTagsIn
      -- * Live edits
    , mutateUnitTags
    , mutateBuildingTags
    , mutateLocationTags
      -- * Live reads and queries
    , LiveTagView(..)
    , captureLiveTagView
    , liveTagRegistry
    , liveTagUniverse
    , runLiveTagQuery
    , liveTagAssignments
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Building.Types (BuildingManager(..), BuildingId)
import Gameplay.Tags.Index
import Gameplay.Tags.Query (QueryUniverse, QueryResult, TagQuery, TargetSets(..), evaluateQuery)
import Gameplay.Tags.Registry (TagRegistry, registryFromCategories, assignments)
import Gameplay.Tags.Types
import Location.Instance (LocationInstances(..), LocationInstanceId)
import Unit.Types.Manager (UnitManager(..), UnitId)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId)
import World.State.Types (WorldManager(..), WorldState(..))

-- * Edits

-- | One edit to one target's tag set, with "Gameplay.Tags.Registry"'s
--   meanings: add and remove are set union and difference (repeats
--   idempotent, absent tags ignored), replace makes the set the
--   target's complete set, and clear (or replacing with the empty set)
--   removes the target's entry. Shared tag names stay on every other
--   target.
data TagEdit
    = AddTagsEdit     !(Set.Set GameplayTag)
    | RemoveTagsEdit  !(Set.Set GameplayTag)
    | ReplaceTagsEdit !(Set.Set GameplayTag)
    | ClearTagsEdit
    deriving (Show, Eq)

applyTagEdit ∷ TagEdit → Set.Set GameplayTag → Set.Set GameplayTag
applyTagEdit = \case
    AddTagsEdit ts     → Set.union ts
    RemoveTagsEdit ts  → (`Set.difference` ts)
    ReplaceTagsEdit ts → const ts
    ClearTagsEdit      → const Set.empty

-- | Whether an edit found its target. A missing target is not an error
--   to raise but a fact to report: it changes nothing, and in
--   particular never creates an assignment.
data TagEditResult = TagEditApplied | TagEditNoTarget
    deriving (Show, Eq)

-- * Pure owner transitions

-- | 'Nothing' when the manager holds no such unit.
editUnitTags ∷ UnitId → TagEdit → UnitManager → Maybe UnitManager
editUnitTags uid edit um
    | HM.member uid (umInstances um) =
        Just um { umTags = modifyCategoryTags (applyTagEdit edit) uid (umTags um) }
    | otherwise = Nothing

-- | 'Nothing' when the manager holds no such building. A footprint
--   reservation is not a building yet, so it cannot be tagged.
editBuildingTags ∷ BuildingId → TagEdit → BuildingManager → Maybe BuildingManager
editBuildingTags bid edit bm
    | HM.member bid (bmInstances bm) =
        Just bm { bmTags = modifyCategoryTags (applyTagEdit edit) bid (bmTags bm) }
    | otherwise = Nothing

-- | 'Nothing' when the page's table holds no such location.
editLocationTags ∷ LocationInstanceId → TagEdit → LocationInstances
                 → Maybe LocationInstances
editLocationTags lid edit lis
    | HM.member lid (lisById lis) =
        Just lis { lisTags = modifyCategoryTags (applyTagEdit edit) lid (lisTags lis) }
    | otherwise = Nothing

-- * Pure inspection

-- | A unit's tags; 'Nothing' when there is no such unit, which is
--   different from an existing untagged unit's empty set.
unitTagsIn ∷ UnitManager → UnitId → Maybe (Set.Set GameplayTag)
unitTagsIn um uid
    | HM.member uid (umInstances um) = Just (categoryTagsOf uid (umTags um))
    | otherwise = Nothing

buildingTagsIn ∷ BuildingManager → BuildingId → Maybe (Set.Set GameplayTag)
buildingTagsIn bm bid
    | HM.member bid (bmInstances bm) = Just (categoryTagsOf bid (bmTags bm))
    | otherwise = Nothing

locationTagsIn ∷ LocationInstances → LocationInstanceId → Maybe (Set.Set GameplayTag)
locationTagsIn lis lid
    | HM.member lid (lisById lis) = Just (categoryTagsOf lid (lisTags lis))
    | otherwise = Nothing

-- * Live edits

-- | One atomic modify of the unit manager.
mutateUnitTags ∷ IORef UnitManager → UnitId → TagEdit → IO TagEditResult
mutateUnitTags ref uid edit = atomicModifyIORef' ref $ \um →
    maybe (um, TagEditNoTarget) (, TagEditApplied) (editUnitTags uid edit um)

-- | One atomic modify of the building manager.
mutateBuildingTags ∷ IORef BuildingManager → BuildingId → TagEdit → IO TagEditResult
mutateBuildingTags ref bid edit = atomicModifyIORef' ref $ \bm →
    maybe (bm, TagEditNoTarget) (, TagEditApplied) (editBuildingTags bid edit bm)

-- | Resolve the page once, then one atomic modify of that page's
--   params. A page that is not live, or has no params yet, has no
--   locations to tag.
mutateLocationTags ∷ IORef WorldManager → LocationTarget → TagEdit → IO TagEditResult
mutateLocationTags wmRef (LocationTarget page lid) edit = do
    wm ← readIORef wmRef
    case lookup page (wmWorlds wm) of
        Nothing → pure TagEditNoTarget
        Just ws → atomicModifyIORef' (wsGenParamsRef ws) $ \case
            Nothing → (Nothing, TagEditNoTarget)
            Just params →
                case editLocationTags lid edit (wgpLocationInstances params) of
                    Nothing   → (Just params, TagEditNoTarget)
                    Just lis' → (Just params { wgpLocationInstances = lis' }, TagEditApplied)

-- * Live reads and queries

-- | One read of every owner (see the module haddock for what that
--   guarantees). Pages without params yet contribute nothing.
data LiveTagView = LiveTagView
    { ltvUnits     ∷ !UnitManager
    , ltvBuildings ∷ !BuildingManager
    , ltvLocations ∷ ![(WorldPageId, LocationInstances)]
      -- ^ In 'wmWorlds' order.
    }

captureLiveTagView ∷ IORef UnitManager → IORef BuildingManager
                   → IORef WorldManager → IO LiveTagView
captureLiveTagView umRef bmRef wmRef = do
    um ← readIORef umRef
    bm ← readIORef bmRef
    wm ← readIORef wmRef
    locs ← forM (wmWorlds wm) $ \(pid, ws) →
        fmap (\p → (pid, wgpLocationInstances p)) <$> readIORef (wsGenParamsRef ws)
    pure LiveTagView
        { ltvUnits = um, ltvBuildings = bm, ltvLocations = catMaybes locs }

-- | The view's assignments as one queryable registry. O(1) for units
--   and buildings; locations are lifted to page-qualified targets.
liveTagRegistry ∷ LiveTagView → TagRegistry
liveTagRegistry v = registryFromCategories
    (umTags (ltvUnits v))
    (bmTags (ltvBuildings v))
    (unionDisjointCategories
        [ mapCategoryKeysMonotonic (LocationTarget pid) (lisTags lis)
        | (pid, lis) ← ltvLocations v ])

-- | Every EXISTING target, tagged or not: the universe a query ranges
--   over, so untagged objects appear in negative results.
liveTagUniverse ∷ LiveTagView → QueryUniverse
liveTagUniverse v = mempty
    { tsUnits     = Set.fromList (HM.keys (umInstances (ltvUnits v)))
    , tsBuildings = Set.fromList (HM.keys (bmInstances (ltvBuildings v)))
    , tsLocations = Set.fromList
        [ LocationTarget pid lid
        | (pid, lis) ← ltvLocations v, lid ← HM.keys (lisById lis) ]
    }

-- | Evaluate against one captured view. The caller narrows the
--   categories with 'Gameplay.Tags.Query.selectCategories' on the
--   universe it passes in, or takes 'liveTagUniverse' whole.
runLiveTagQuery ∷ LiveTagView → (QueryUniverse → QueryUniverse) → TagQuery
                → QueryResult
runLiveTagQuery v narrow =
    evaluateQuery (liveTagRegistry v) (narrow (liveTagUniverse v))

-- | Every assignment in the view, in the documented result order.
liveTagAssignments ∷ LiveTagView → [(TagTarget, Set.Set GameplayTag)]
liveTagAssignments = assignments . liveTagRegistry

