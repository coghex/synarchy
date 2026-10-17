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
--   === Physical items (#2703)
--
--   Item assignments live with the SESSION owner,
--   'World.State.Types.wmItemTags', keyed by 'Item.Types.iiInstanceId',
--   because an item moves between owners and pages without changing
--   identity. Edits and the query universe resolve through the
--   canonical live enumeration ("World.Item.Locate"): ground, carried,
--   equipped, worn, stored, delivered and recursively nested items, and
--   never a remembered copy, a definition or the id-zero sentinel.
--   Moves never touch the index; destruction is
--   "Gameplay.Tags.ItemLifecycle"'s.
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
    , itemTagsIn
      -- * Live edits
    , mutateUnitTags
    , mutateBuildingTags
    , mutateLocationTags
    , mutateItemTags
      -- * Live reads and queries
    , LiveTagView(..)
    , captureLiveTagView
    , liveTagRegistry
    , liveTagUniverse
    , liveItemTargets
    , onPage
    , runLiveTagQuery
    , liveTagAssignments
      -- * Committing a new item tree's assignments
    , commitItemTreeTags
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Building.Types (BuildingInstance(..))
import Item.Ground (GroundItems)
import Item.Types (ItemInstance(..))
import Unit.Types.Instance (UnitInstance(..))
import World.Item.Locate (sessionGroundItems, sessionItemInstances)
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

-- | A live item's tags; 'Nothing' when the canonical live enumeration
--   finds no such item (#2703) — a remembered copy, a destroyed item
--   and the id-zero sentinel included.
itemTagsIn ∷ LiveTagView → ItemTarget → Maybe (Set.Set GameplayTag)
itemTagsIn v t
    | Set.member t (liveItemTargets v) = Just (categoryTagsOf t (ltvItems v))
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

-- | An item edit (#2703). The target must be a LIVE item, as the
--   canonical enumeration ("World.Item.Locate") sees it — ground,
--   carried, equipped, worn, stored, delivered, or nested at any depth —
--   and, with @Just page@, one on that page. The item's owners live
--   behind several refs, so the existence check cannot share one atomic
--   step with the write the way a unit or building edit does. Instead
--   the write is one 'atomicModifyIORef'' of the session owner and the
--   item is located AGAIN afterwards: if a destruction removed it in
--   between, the assignment is withdrawn and the answer is
--   'TagEditNoTarget', so no edit leaves a tag on a dead item whichever
--   way the race falls (the destruction's own forget covers the other
--   order). A session replacement between the check and the write
--   refuses the write.
mutateItemTags ∷ IORef UnitManager → IORef BuildingManager → IORef WorldManager
               → Maybe WorldPageId → ItemTarget → TagEdit → IO TagEditResult
mutateItemTags umRef bmRef wmRef scope t edit = do
    before ← captureLiveTagView umRef bmRef wmRef
    if not (live before)
        then pure TagEditNoTarget
        else do
            let epoch = wmSessionEpoch (ltvManager before)
            wrote ← atomicModifyIORef' wmRef $ \mgr →
                if wmSessionEpoch mgr ≢ epoch then (mgr, False)
                else ( mgr { wmItemTags =
                               modifyCategoryTags (applyTagEdit edit) t (wmItemTags mgr) }
                     , True )
            if not wrote then pure TagEditNoTarget else do
                after ← captureLiveTagView umRef bmRef wmRef
                if live after then pure TagEditApplied else do
                    atomicModifyIORef' wmRef $ \mgr →
                        ( if wmSessionEpoch mgr ≢ epoch then mgr
                          else mgr { wmItemTags = deleteCategoryTarget t (wmItemTags mgr) }
                        , () )
                    pure TagEditNoTarget
  where
    live v = Set.member t (maybe id (\p → (`Set.intersection` liveItemTargetsOn v p)) scope
                                      (liveItemTargets v))

-- * Live reads and queries

-- | One read of every owner (see the module haddock for what that
--   guarantees). Pages without params yet contribute nothing.
data LiveTagView = LiveTagView
    { ltvUnits     ∷ !UnitManager
    , ltvBuildings ∷ !BuildingManager
    , ltvLocations ∷ ![(WorldPageId, LocationInstances)]
      -- ^ In 'wmWorlds' order.
    , ltvManager   ∷ !WorldManager
      -- ^ The session owner as read, for its item index (#2703).
    , ltvGround    ∷ ![(WorldPageId, GroundItems)]
      -- ^ Every live page's ground items, in 'wmWorlds' order.
    }

-- | The view's item index.
ltvItems ∷ LiveTagView → CategoryIndex ItemTarget
ltvItems = wmItemTags . ltvManager

captureLiveTagView ∷ IORef UnitManager → IORef BuildingManager
                   → IORef WorldManager → IO LiveTagView
captureLiveTagView umRef bmRef wmRef = do
    um ← readIORef umRef
    bm ← readIORef bmRef
    wm ← readIORef wmRef
    locs ← forM (wmWorlds wm) $ \(pid, ws) →
        fmap (\p → (pid, wgpLocationInstances p)) <$> readIORef (wsGenParamsRef ws)
    ground ← sessionGroundItems wm
    pure LiveTagView
        { ltvUnits = um, ltvBuildings = bm, ltvLocations = catMaybes locs
        , ltvManager = wm, ltvGround = ground }

-- | The view's assignments as one queryable registry. O(1) for units
--   and buildings; locations are lifted to page-qualified targets.
liveTagRegistry ∷ LiveTagView → TagRegistry
liveTagRegistry v = registryFromCategories
    (umTags (ltvUnits v))
    (bmTags (ltvBuildings v))
    (unionDisjointCategories
        [ mapCategoryKeysMonotonic (LocationTarget pid) (lisTags lis)
        | (pid, lis) ← ltvLocations v ])
    (ltvItems v)

-- | Every EXISTING target, tagged or not: the universe a query ranges
--   over, so untagged objects appear in negative results.
liveTagUniverse ∷ LiveTagView → QueryUniverse
liveTagUniverse v = mempty
    { tsUnits     = Set.fromList (HM.keys (umInstances (ltvUnits v)))
    , tsBuildings = Set.fromList (HM.keys (bmInstances (ltvBuildings v)))
    , tsLocations = Set.fromList
        [ LocationTarget pid lid
        | (pid, lis) ← ltvLocations v, lid ← HM.keys (lisById lis) ]
    , tsItems     = liveItemTargets v
    }

-- | Every live physical item in the session, by identity: what the
--   canonical enumeration walks, never a remembered copy, a definition
--   or a display group. Same-definition items are distinct targets.
liveItemTargets ∷ LiveTagView → Set.Set ItemTarget
liveItemTargets v = Set.fromList
    [ t | (_, i) ← liveItems v, Just t ← [mkItemTarget (iiInstanceId i)] ]

-- | The live items on one page.
liveItemTargetsOn ∷ LiveTagView → WorldPageId → Set.Set ItemTarget
liveItemTargetsOn v page = Set.fromList
    [ t | (p, i) ← liveItems v, p ≡ page, Just t ← [mkItemTarget (iiInstanceId i)] ]

liveItems ∷ LiveTagView → [(WorldPageId, ItemInstance)]
liveItems v = sessionItemInstances (ltvGround v) (ltvBuildings v) (ltvUnits v)

-- | Narrow a universe to one page — the caller's selected live
--   context. Units and buildings by their page, locations by the page
--   that owns them, items by the page holding them now.
onPage ∷ LiveTagView → WorldPageId → QueryUniverse → QueryUniverse
onPage v page u = u
    { tsUnits = Set.filter (\k → (uiPage ⊚ HM.lookup k (umInstances (ltvUnits v))) ≡ Just page)
                           (tsUnits u)
    , tsBuildings = Set.filter (\k → (biPage ⊚ HM.lookup k (bmInstances (ltvBuildings v))) ≡ Just page)
                               (tsBuildings u)
    , tsLocations = Set.filter ((≡ page) . ltPage) (tsLocations u)
    , tsItems = Set.intersection (tsItems u) (liveItemTargetsOn v page)
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


-- * Committing a new item tree's assignments

-- | Install explicit assignments for a newly materialized item tree
--   (#2703) — the attachment boundary later scenario construction
--   needs. ALL OR NOTHING: every target must be LIVE when this runs
--   (the tree committed to an owner first), or nothing is installed
--   and the uncommitted ids come back, so a rejected or never-committed
--   tree leaves no assignment behind. New items carry no tags of their
--   own; this is the only way one gets any at birth.
commitItemTreeTags ∷ IORef UnitManager → IORef BuildingManager → IORef WorldManager
                   → [(ItemTarget, Set.Set GameplayTag)] → IO (Either [ItemTarget] ())
commitItemTreeTags umRef bmRef wmRef assigned = do
    v ← captureLiveTagView umRef bmRef wmRef
    let liveSet = liveItemTargets v
        missing = [ t | (t, _) ← assigned, not (Set.member t liveSet) ]
        epoch   = wmSessionEpoch (ltvManager v)
    if not (null missing) then pure (Left missing) else do
        atomicModifyIORef' wmRef $ \mgr →
            ( if wmSessionEpoch mgr ≢ epoch then mgr
              else mgr { wmItemTags = foldl' (\ix (t, ts) → modifyCategoryTags (Set.union ts) t ix)
                                             (wmItemTags mgr) assigned }
            , () )
        pure (Right ())
