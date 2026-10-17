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
    , mutateItemTagsWith
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
    , commitItemTreeTagsWith
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
--   and, with @Just page@, one on that page.
--
--   The item's owners sit behind several refs, so the existence check
--   cannot share one atomic step with the write the way a unit or
--   building edit does. What makes it safe instead is
--   'World.State.Types.wmItemDestroyGen': every destruction's forget
--   bumps it in the same atomic step that removes the destroyed ids'
--   tags ("Gameplay.Tags.ItemLifecycle"), and this edit reads it BEFORE
--   it locates the item and writes only if it is unchanged. So either
--   no destruction finished in between — any removal of the target
--   after the check is followed by its own forget, which removes this
--   write — or one did and the edit retries against a fresh view. A
--   missing item is never treated as destroyed: an item a transfer has
--   momentarily detached is simply not editable at that instant, and
--   nothing it already carries is touched. A session replacement in
--   between refuses the edit.
mutateItemTags ∷ IORef UnitManager → IORef BuildingManager → IORef WorldManager
               → Maybe WorldPageId → ItemTarget → TagEdit → IO TagEditResult
mutateItemTags = mutateItemTagsWith (pure ())

-- | 'mutateItemTags' with an action run between the decision and the
--   commit — the window a racing destruction or detach lands in. The
--   test seam for that interleaving; production passes @pure ()@.
mutateItemTagsWith ∷ IO () → IORef UnitManager → IORef BuildingManager → IORef WorldManager
                   → Maybe WorldPageId → ItemTarget → TagEdit → IO TagEditResult
mutateItemTagsWith between umRef bmRef wmRef scope t edit =
    fromMaybe TagEditNoTarget ⊚ guardedItemWrite between umRef bmRef wmRef (\v →
        if not (Set.member t (maybe id (\p → (`Set.intersection` liveItemTargetsOn v p)) scope
                                       (liveItemTargets v)))
            then Left TagEditNoTarget
            else Right (modifyCategoryTags (applyTagEdit edit) t, TagEditApplied))

-- | The shared discipline of every item-index WRITE: read the session
--   epoch and the destruction generation, capture a view, decide, and
--   commit only if neither moved — retrying a bounded number of times
--   when a destruction raced the decision. 'Nothing' when the session
--   was replaced or the retries ran out; the decision's own refusal
--   otherwise.
guardedItemWrite ∷ IO () → IORef UnitManager → IORef BuildingManager → IORef WorldManager
                 → (LiveTagView → Either α (CategoryIndex ItemTarget → CategoryIndex ItemTarget, α))
                 → IO (Maybe α)
guardedItemWrite between umRef bmRef wmRef decide = go (16 ∷ Int)
  where
    go 0 = pure Nothing
    go n = do
        m0 ← readIORef wmRef
        v  ← captureLiveTagView umRef bmRef wmRef
        case decide v of
            Left refusal → pure (Just refusal)
            Right (change, answer) → do
                between
                outcome ← atomicModifyIORef' wmRef $ \mgr →
                    if wmSessionEpoch mgr ≢ wmSessionEpoch m0 then (mgr, WriteReplaced)
                    else if wmItemDestroyGen mgr ≢ wmItemDestroyGen m0 then (mgr, WriteRaced)
                    else (mgr { wmItemTags = change (wmItemTags mgr) }, WriteDone)
                case outcome of
                    WriteDone     → pure (Just answer)
                    WriteReplaced → pure Nothing
                    WriteRaced    → go (n - 1)

data WriteOutcome = WriteDone | WriteRaced | WriteReplaced

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
--   needs. ALL OR NOTHING: every target must be LIVE (the tree committed
--   to an owner first), or nothing is installed and the uncommitted ids
--   come back. The write takes 'mutateItemTags'' guard, so a tree
--   destroyed after the check either has its forget run after this
--   write (removing it) or forces a retry against a fresh view; a
--   session replacement, or a destruction race that never settles,
--   installs nothing and answers every target. New items carry no tags
--   of their own; this is the only way one gets any at birth.
commitItemTreeTags ∷ IORef UnitManager → IORef BuildingManager → IORef WorldManager
                   → [(ItemTarget, Set.Set GameplayTag)] → IO (Either [ItemTarget] ())
commitItemTreeTags = commitItemTreeTagsWith (pure ())

-- | 'commitItemTreeTags' with the same test seam as 'mutateItemTagsWith'.
commitItemTreeTagsWith ∷ IO () → IORef UnitManager → IORef BuildingManager → IORef WorldManager
                       → [(ItemTarget, Set.Set GameplayTag)] → IO (Either [ItemTarget] ())
commitItemTreeTagsWith between umRef bmRef wmRef assigned =
    fromMaybe (Left (map fst assigned)) ⊚ guardedItemWrite between umRef bmRef wmRef (\v →
        let liveSet = liveItemTargets v
            missing = [ t | (t, _) ← assigned, not (Set.member t liveSet) ]
        in if not (null missing) then Left (Left missing)
           else Right ( \ix → foldl' (\acc (t, ts) → modifyCategoryTags (Set.union ts) t acc)
                                      ix assigned
                      , Right () ))
