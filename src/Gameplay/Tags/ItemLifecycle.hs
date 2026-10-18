-- | Removing physical-item gameplay tags when the items actually cease
--   to exist (#2703, SCN-04 of epic #2698).
--
--   Item tags live with the session owner
--   ('World.State.Types.wmItemTags'), keyed by
--   'Item.Types.iiInstanceId'. A move never touches them — the id is
--   unchanged, so the tags simply follow — which is what makes a
--   transfer's temporary detach, a refused move and a rolled-back
--   batch tag-neutral by construction. Only DESTRUCTION removes
--   assignments, and it does so here, at the production sites that
--   destroy items:
--
--     * a destroyed, cleared or retired unit or building (its carried,
--       equipped, worn, stored and delivered trees go with it — none of
--       those paths spills anything);
--     * a destroyed or replaced page's ground items, and Exit-to-Menu;
--     * explicit ground removal and lava's destruction of ground items;
--     * consumption: a discrete meal, a bulk sack eaten dry,
--       @unit.removeItem@, crafting and repair inputs, a bandage drawn
--       from a kit, a power item placed as a node, construction
--       materials paid into a receipt.
--
--   Each site diffs the item ids its own atomic step removed
--   ('destroyedItemIds') — so a surviving sibling, a surviving container
--   and every item a consumption merely re-valued keep their tags — and
--   forgets exactly those AFTER the removal, the same order
--   'Unit.Transfer.Live.retireTransferOrdersEverywhere' and the
--   building demolition cleanups use, so no reader observes a tag
--   removed from an item that is still there.
--
--   __The epoch guard.__ Item ids are unique and never reused within a
--   session, so a forget that lands a moment after its removal can
--   only ever name a dead item. Across a load they are not: a restored
--   session reuses ids. 'withDestroyedItems' reads
--   'World.State.Types.wmSessionEpoch' BEFORE the removal and the
--   forget is refused if a session replacement has bumped it since, so
--   a cleanup computed in the outgoing session can never strip a tag
--   from the incoming one.
module Gameplay.Tags.ItemLifecycle
    ( -- * Item trees
      itemTreeIds
    , itemForestIds
    , destroyedItemIds
    , unitItems
    , buildingItems
    , retiredUnitItemIds
    , retiredBuildingItemIds
    , groundItemIds
      -- * Forgetting
    , withDestroyedItems
    , itemTagEpoch
    , forgetItemTags
    , forgetAllItemTags
      -- * Late ground placement
    , placeOnGround
      -- * One-owner consumption steps
    , unitConsumption
    , buildingConsumption
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Item.Types (ItemInstance)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingInstance(..), BuildingManager(..), BuildingId)
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Unit.Types.Manager (UnitManager(..), UnitId)
import Gameplay.Tags.Index (emptyCategoryIndex, restrictCategoryIndex)
import Gameplay.Tags.Tag (itemTargetId)
import Scenario.Items.Refs (restrictItemRefs, emptyItemRefs)
import Item.Types (ItemInstance(..))
import Unit.Types.Instance (UnitInstance(..))
import World.State.Types (WorldManager(..), WorldState(..))

-- * Item trees

-- | Every physical id in one tree, the root included. Id 0 is the
--   never-minted sentinel and is never an identity.
itemTreeIds ∷ ItemInstance → HS.HashSet Word64
itemTreeIds = itemForestIds . pure

itemForestIds ∷ [ItemInstance] → HS.HashSet Word64
itemForestIds = go HS.empty
  where
    go acc [] = acc
    go acc (i : rest) =
        let acc' = if iiInstanceId i ≡ 0 then acc else HS.insert (iiInstanceId i) acc
        in go (go acc' (iiContents i)) rest

-- | The ids present before one step and absent after it: what a
--   consumption or destruction step destroyed. Anything the step only
--   re-valued (a fill, a condition, a temperature) is still present and
--   so is not in the result.
destroyedItemIds ∷ [ItemInstance] → [ItemInstance] → HS.HashSet Word64
destroyedItemIds before after = HS.difference (itemForestIds before) (itemForestIds after)

-- | Every root a unit owns: carried, equipped and worn.
unitItems ∷ UnitInstance → [ItemInstance]
unitItems u = uiInventory u ⧺ HM.elems (uiEquipment u) ⧺ uiAccessories u

-- | Every root a building owns: delivered materials and loose storage.
buildingItems ∷ BuildingInstance → [ItemInstance]
buildingItems b = concat (HM.elems (biMaterialsDelivered b)) ⧺ biStorage b

-- | Every item id carried, equipped or worn by a unit the step removed
--   (present in the first manager, absent from the second). An owner's
--   removal on these paths spills nothing, so everything it held is
--   destroyed with it.
retiredUnitItemIds ∷ UnitManager → UnitManager → HS.HashSet Word64
retiredUnitItemIds before after = itemForestIds $ concatMap unitItems $
    HM.elems (HM.difference (umInstances before) (umInstances after))

-- | The building counterpart: delivered materials and storage of every
--   building the step removed.
retiredBuildingItemIds ∷ BuildingManager → BuildingManager → HS.HashSet Word64
retiredBuildingItemIds before after = itemForestIds $ concatMap buildingItems $
    HM.elems (HM.difference (bmInstances before) (bmInstances after))

-- | Every item id lying on one page's ground, nested contents included.
groundItemIds ∷ GroundItems → HS.HashSet Word64
groundItemIds = itemForestIds . map giInst . HM.elems . gisItems

-- * Forgetting

-- | Run a destroying step that answers which item ids it destroyed, and
--   forget those ids' tags afterwards — unless the session was replaced
--   in between (see the module haddock).
withDestroyedItems ∷ IORef WorldManager → IO (α, HS.HashSet Word64) → IO α
withDestroyedItems wmRef step = do
    epoch ← wmSessionEpoch ⊚ readIORef wmRef
    (result, destroyed) ← step
    forgetItemTags wmRef epoch destroyed
    pure result

-- | The session epoch a destroying step reads BEFORE its removal, to
--   guard the forget that follows it.
itemTagEpoch ∷ IORef WorldManager → IO Word64
itemTagEpoch wmRef = wmSessionEpoch ⊚ readIORef wmRef

-- | Remove every assignment naming one of these ids, from both
--   indexes, in one atomic step; a no-op for an empty set, an untagged
--   id or a replaced session. The same step forgets the items'
--   scenario references (#2709, 'World.State.Types.wmItemScenarioRefs'):
--   the reference is as mortal as the tags, and sharing the step is
--   what keeps the two from ever disagreeing about which items exist.
forgetItemTags ∷ IORef WorldManager → Word64 → HS.HashSet Word64 → IO ()
forgetItemTags wmRef epoch ids
    | HS.null ids = pure ()
    | otherwise = atomicModifyIORef' wmRef $ \mgr →
        if wmSessionEpoch mgr ≢ epoch
            then (mgr, ())
            else ( mgr { wmItemTags = restrictCategoryIndex
                           (\t → not (HS.member (itemTargetId t) ids))
                           (wmItemTags mgr)
                       , wmItemScenarioRefs = restrictItemRefs
                           (\i → not (HS.member i ids))
                           (wmItemScenarioRefs mgr)
                         -- Bumped even when none of the ids was tagged:
                         -- an edit that located one of them before this
                         -- removal must not commit after it.
                       , wmItemDestroyGen = wmItemDestroyGen mgr + 1 }
                 , () )

-- | Session teardown: no item survives it, so no assignment or
--   scenario reference does.
forgetAllItemTags ∷ WorldManager → WorldManager
forgetAllItemTags mgr = mgr { wmItemTags = emptyCategoryIndex
                            , wmItemScenarioRefs = emptyItemRefs }

-- * One-owner consumption steps

-- | Wrap one PURE step on the unit manager that CONSUMES from these
--   units' items, so it also answers what those units no longer hold.
--   The caller keeps its own @atomicModifyIORef'@ and hands the whole
--   thing to 'withDestroyedItems':
--
--   > withDestroyedItems wmRef $ atomicModifyIORef' umRef $
--   >     unitConsumption [uid] step
--
--   Only for a step that destroys and never moves an item out of those
--   units: a moved item would be absent too. Items the step only
--   re-valued in place keep their ids, and so their tags.
unitConsumption ∷ [UnitId] → (UnitManager → (UnitManager, α))
                → UnitManager → (UnitManager, (α, HS.HashSet Word64))
unitConsumption uids step um =
    let (um', r) = step um
        held m = concatMap unitItems
            [ u | uid ← uids, Just u ← [HM.lookup uid (umInstances m)] ]
    in (um', (r, destroyedItemIds (held um) (held um')))

-- | 'unitConsumption' for one building's delivered materials and
--   storage.
buildingConsumption ∷ BuildingId → (BuildingManager → (BuildingManager, α))
                    → BuildingManager → (BuildingManager, (α, HS.HashSet Word64))
buildingConsumption bid step bm =
    let (bm', r) = step bm
        held m = maybe [] buildingItems (HM.lookup bid (bmInstances m))
    in (bm', (r, destroyedItemIds (held bm) (held bm')))

-- * Late ground placement

-- | Lay an item a caller already took from its owner onto a page's
--   ground — a drop, or a pickup's rollback — and forget the tree's tags
--   if that page is no longer live (#2703).
--
--   The caller resolved @ws@ before it popped the item, and a page
--   teardown can land in between: the departed 'WorldState' still takes
--   the insert, but nothing will ever reach it again, so the item is
--   destroyed. The check runs AFTER the insert and compares the ground
--   ref itself (a replacement reuses the page NAME, never the ref).
--   Teardown removes the page from 'wmWorlds' BEFORE it reads the
--   ground it forgets, so every interleaving is covered: a check that
--   still sees the page live ran before that removal, so the insert is
--   in teardown's read; one that sees it gone forgets here. @epoch@ is
--   the session epoch read before the pop.
placeOnGround ∷ IORef WorldManager → Word64 → WorldState → ItemInstance
              → Float → Float → IO Int
placeOnGround wmRef epoch ws it x y = do
    gid ← atomicModifyIORef' (wsGroundItemsRef ws) (spawnGroundItem it x y)
    live ← any ((≡ wsGroundItemsRef ws) . wsGroundItemsRef . snd) . wmWorlds
               ⊚ readIORef wmRef
    unless live $ forgetItemTags wmRef epoch (itemTreeIds it)
    pure gid
