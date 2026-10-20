{-# LANGUAGE Strict #-}
-- | The owner adapters of scenario item construction (#2709, SCN-09 of
--   epic #2698): the shapes each kind of item owner holds, built from
--   the scenario entries that describe them.
--
--   An owner adapter does exactly three things and nothing else: it
--   gives every owned entry its document path, builds the owner's item
--   collections through "Scenario.Items.Build" in ONE fixed order, and
--   hands them back in the owner's own runtime shape. It does not
--   initialize a unit's body, place a building or location, decide what
--   a unit with no stated inventory carries, install anything into a
--   live manager or attach tags — those belong to the later unit
--   (SCN-10), building/location (SCN-12) and session (SCN-13) slices,
--   which call these functions for the item half of their owner.
--
--   __Omission stays visible.__ Each collection comes back as an
--   'Scenario.Types.Authored': 'Omitted' tells the owner's slice to apply
--   its own definition default (a unit's starting kit), @Authored []@
--   tells it the owner holds nothing. Conflating the two is how a
--   captured, emptied unit would be handed a fresh kit.
--
--   __Order is fixed, never a map's.__ A unit's equipment is keyed by
--   slot, and a hash map's iteration order is unspecified; building the
--   slots in sorted order is what keeps the allocated instance ids, and
--   so every later diagnostic and mapping, the same from run to run.
module Scenario.Items.Owners
    ( -- * Unit
      UnitItems(..)
    , buildUnitItems
      -- * Building
    , BuildingItems(..)
    , buildBuildingItems
      -- * Ground
    , GroundPlacement(..)
    , buildGroundItems
    , buildGroundItemsAt
    , installGroundItems
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import Item.Ground (GroundItems, spawnGroundItem)
import Item.Types (ItemInstance(..))
import Scenario.Decode.Monad (indexPath, keyPath)
import Scenario.Items.Build
import Scenario.Types

-- * Unit

-- | The item collections of one unit, as 'Unit.Types.Instance.UnitInstance'
--   holds them.
data UnitItems = UnitItems
    { uitInventory   ∷ !(Authored [ItemInstance])
    , uitEquipment   ∷ !(Authored (HM.HashMap Text ItemInstance))
      -- ^ Slot → item. A slot whose item was rejected is absent; the
      --   slot NAMES are the decoder's to validate against the unit
      --   definition's equipment slots.
    , uitAccessories ∷ !(Authored [ItemInstance])
    }

-- | Build a unit's inventory, equipment and accessories (in that order,
--   equipment by sorted slot name). @path@ is the unit's own document
--   path (@units[2]@).
buildUnitItems ∷ ItemBuildEnv → Text → UnitEntry → IO (UnitItems, ItemBuild)
buildUnitItems env path u = do
    let invPath = keyPath path "inventory"
        eqPath = keyPath path "equipment"
        accPath = keyPath path "accessories"
        inv = [ (indexPath invPath i, e) | (i, e) ← zip [0 ..] (authoredOr [] (ueInventory u)) ]
        eq = [ (keyPath eqPath slot, e)
             | (slot, e) ← sortOn fst (HM.toList (authoredOr HM.empty (ueEquipment u))) ]
        acc = [ (indexPath accPath i, e) | (i, e) ← zip [0 ..] (authoredOr [] (ueAccessories u)) ]
    built ← buildItemForest env (inv ⧺ eq ⧺ acc)
    let (invT, rest) = splitAt (length inv) (ibTrees built)
        (eqT, accT) = splitAt (length eq) rest
        slots = map fst (sortOn fst (HM.toList (authoredOr HM.empty (ueEquipment u))))
    return
        ( UnitItems
            { uitInventory = fmap (const (catMaybes invT)) (ueInventory u)
            , uitEquipment = fmap (const (HM.fromList
                  [ (slot, t) | (slot, Just t) ← zip slots eqT ])) (ueEquipment u)
            , uitAccessories = fmap (const (catMaybes accT)) (ueAccessories u)
            }
        , built )

-- * Building

-- | The item collections of one building.
data BuildingItems = BuildingItems
    { bitStorage   ∷ !(Authored [ItemInstance])
      -- ^ 'Building.Types.biStorage': exact contents, with no capacity
      --   check (D-57).
    , bitDelivered ∷ !(Authored (HM.HashMap Text [ItemInstance]))
      -- ^ 'Building.Types.biMaterialsDelivered': the delivered items
      --   grouped by definition, in stated order within each group. Which
      --   definitions are valid MATERIALS of the building is the
      --   decoder's catalog check, not this adapter's.
    }

-- | Build a building's storage then its delivered materials. @path@ is
--   the building's own document path (@buildings[0]@).
buildBuildingItems ∷ ItemBuildEnv → Text → BuildingEntry → IO (BuildingItems, ItemBuild)
buildBuildingItems env path b = do
    let stPath = keyPath path "storage"
        dlPath = keyPath path "materials_delivered"
        st = [ (indexPath stPath i, e) | (i, e) ← zip [0 ..] (authoredOr [] (beStorage b)) ]
        dl = [ (indexPath dlPath i, e) | (i, e) ← zip [0 ..] (authoredOr [] (beMaterialsDelivered b)) ]
    built ← buildItemForest env (st ⧺ dl)
    let (stT, dlT) = splitAt (length st) (ibTrees built)
        grouped = HM.fromListWith (flip (⧺))
            [ (iiDefName t, [t]) | Just t ← dlT ]
    return
        ( BuildingItems
            { bitStorage = fmap (const (catMaybes stT)) (beStorage b)
            , bitDelivered = fmap (const grouped) (beMaterialsDelivered b)
            }
        , built )

-- * Ground

-- | One item lying on the ground, ready to place.
data GroundPlacement = GroundPlacement
    { gpX    ∷ !Float
    , gpY    ∷ !Float
    , gpItem ∷ !ItemInstance
    }

-- | Build ground items, @ground_items[i]@ in stated order. A rejected
--   entry yields 'Nothing' in its slot. No salvage wear and no
--   quality/condition roll beyond the entry's own: this is not
--   @item.spawnGround@.
buildGroundItems ∷ ItemBuildEnv → [GroundItemEntry] → IO ([Maybe GroundPlacement], ItemBuild)
buildGroundItems env entries =
    buildGroundItemsAt env [ (indexPath "ground_items" i, g) | (i, g) ← zip [0 ..] entries ]

-- | 'buildGroundItems' for entries that carry the path they stood at in the
--   document (#2713): a rejected entry is absent from the decoded list, so a
--   diagnostic a later phase raises must name the entry by its source path,
--   not by its index among the survivors.
buildGroundItemsAt ∷ ItemBuildEnv → [(Text, GroundItemEntry)] → IO ([Maybe GroundPlacement], ItemBuild)
buildGroundItemsAt env pathed = do
    let roots = [ (p, giItem g) | (p, g) ← pathed ]
        entries = map snd pathed
    built ← buildItemForest env roots
    return
        ( zipWith (\g t → (\inst → GroundPlacement (giX g) (giY g) inst) <$> t)
                  entries (ibTrees built)
        , built )

-- | Lay placements onto a page's ground store with the ordinary row
--   allocator ('Item.Ground.spawnGroundItem'). Ground-row ids are the
--   store's own page-local slots and are returned in placement order;
--   they are not the items' identity and nothing here records them as
--   such.
installGroundItems ∷ [GroundPlacement] → GroundItems → (GroundItems, [Int])
installGroundItems ps g0 = fmap reverse (foldl' step (g0, []) ps)
  where
    step (g, gids) p =
        let (g', gid) = spawnGroundItem (gpItem p) (gpX p) (gpY p) g
        in (g', gid : gids)
