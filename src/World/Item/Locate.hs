{-# LANGUAGE Strict #-}
-- | Find ONE live 'Item.Types.ItemInstance' anywhere in the session by
--   its 'Item.Types.iiInstanceId' (#2512, epic #1231 PLC-7, D-24).
--
--   The portable-knowledge layer ("Item.Knowledge") keys on an instance
--   id and nothing else, because a crate moves — ground → a unit's
--   inventory → a building's storage → back — and its record must
--   follow it without being copied. So every verb over that layer needs
--   the same question answered first: WHERE is this instance right now,
--   and what does it currently look like? This module is that one
--   answer, and it is the only place in the engine that asks it
--   session-wide.
--
--   __It walks the canonical container set, never a private one.__ The
--   containers visited per page are exactly what
--   'World.Save.Types.pageItemContainers' enumerates (ground items;
--   each unit's inventory, equipped slots and accessories; each
--   building's delivered materials and loose storage) and the nesting
--   is exactly 'World.Save.Types.flattenItemInstances' — the save
--   system's single item walk (#1090). A container added to a unit or a
--   building therefore reaches this locator from the same one edit that
--   makes the save system see it, and the two cannot drift apart.
--
--   The live shapes are adapted to that enumeration through
--   'World.Save.Types.toUnitSnapshot' / 'toBuildingSnapshot' — the
--   SAME per-page projections the save capture path uses, which is what
--   keeps "the items on page P" meaning one thing across both. Both
--   managers are session-global and filtered by page inside those
--   projections, so a hidden page's crate is found exactly like a
--   visible one's.
--
--   __Remembered instances are not live and never resolve here.__ A
--   'Item.Knowledge.ContentsObservation' holds COPIES; asking this
--   locator for one of their ids answers 'Nothing' unless that id also
--   names something still physically present, which is precisely what
--   makes an observation historical.
module World.Item.Locate
    ( LocatedItem(..)
    , sessionGroundItems
    , sessionItemInstances
    , locateItemInstanceIn
    , sessionLiveItemIds
    , pageLiveItemInstances
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Data.IORef (readIORef)
import Building.Types (BuildingManager)
import Item.Ground (GroundItems)
import Item.Types (ItemInstance(..))
import Unit.Types (UnitManager)
import World.Page.Types (WorldPageId)
import World.Save.Types
    ( ItemWalkOrder(..), flattenItemInstances, pageItemContainers
    , toBuildingSnapshot, toUnitSnapshot )
import World.State.Types (WorldManager(..), WorldState(..))

-- | A found instance and the page it was found on. Both halves matter:
--   the caller usually needs the instance (to weigh it, or to read its
--   contents and capacity) AND the page (to report where it is, or to
--   act on the right world).
data LocatedItem = LocatedItem
    { liPage     ∷ !WorldPageId
    , liInstance ∷ !ItemInstance
    } deriving (Show, Eq)

-- | Read every live page's ground items, in the manager's own page
--   order. The one IO step the walk needs — ground items are the only
--   item container that lives behind a per-page
--   'World.State.Types.wsGroundItemsRef' rather than in a
--   session-global manager — so every function below stays pure.
sessionGroundItems ∷ WorldManager → IO [(WorldPageId, GroundItems)]
sessionGroundItems mgr =
    forM (wmWorlds mgr) $ \(pid, ws) →
        (,) pid <$> readIORef (wsGroundItemsRef ws)

-- | Every live item instance on ONE page, each container's outer items
--   and everything recursively nested inside them.
pageLiveItemInstances
    ∷ BuildingManager → UnitManager → (WorldPageId, GroundItems)
    → [ItemInstance]
pageLiveItemInstances bm um (pid, ground) =
    [ i
    | (_, insts) ← pageItemContainers ItemsGroundFirst
                       (const ground)
                       (const (toUnitSnapshot pid um))
                       (const (toBuildingSnapshot pid bm))
                       ()
    , inst ← insts
    , i    ← flattenItemInstances inst ]

-- | Every live item instance in the whole session, tagged with its
--   page.
sessionItemInstances
    ∷ [(WorldPageId, GroundItems)] → BuildingManager → UnitManager
    → [(WorldPageId, ItemInstance)]
sessionItemInstances ground bm um =
    [ (fst page, i)
    | page ← ground
    , i    ← pageLiveItemInstances bm um page ]

-- | The instance with this id, and the page holding it — or 'Nothing'
--   when nothing live in the session carries it.
--
--   Ids are unique across a valid session
--   ('World.Save.Snapshot.allItemInstanceIds' rejects a duplicate at
--   the save boundary), so the first match is THE match. Id 0 is the
--   never-minted sentinel and is refused outright rather than allowed
--   to match a legacy unstamped item.
locateItemInstanceIn
    ∷ [(WorldPageId, GroundItems)] → BuildingManager → UnitManager
    → Word64 → Maybe LocatedItem
locateItemInstanceIn _ _ _ 0 = Nothing
locateItemInstanceIn ground bm um iid = listToMaybe
    [ LocatedItem pid i
    | (pid, i) ← sessionItemInstances ground bm um
    , iiInstanceId i ≡ iid ]

-- | Every live instance id in the session, as the set the load
--   boundary's portable-knowledge scrub tests membership against.
sessionLiveItemIds
    ∷ [(WorldPageId, GroundItems)] → BuildingManager → UnitManager
    → HS.HashSet Word64
sessionLiveItemIds ground bm um =
    HS.fromList [ iiInstanceId i | (_, i) ← sessionItemInstances ground bm um ]
