{-# LANGUAGE Strict #-}
-- | The save/load path's known-entity id sets, derived purely from a
--   decoded 'SaveData' (issue #764, save-overhaul C3). Split out of
--   "Engine.Scripting.Lua.API.Save" by issue #985: nothing here takes an
--   @EngineEnv@, so this module stays outside the save/load path's
--   permanent full-access exception
--   (@docs\/engineenv_capability_inventory.md@ §6.1).
module Engine.Scripting.Lua.API.Save.Integrity
    ( flattenItemInstanceIds'
    , knownEntitiesFromSaveData
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingId(..))
import Craft.Bills (CraftBills(..), BillId(..))
import Item.Ground (GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..))
import Location.Instance
    (LocationInstance(..), LocationInstanceId(..), instancesToList)
import Unit.Types (UnitId(..))
import World.Generate.Types (WorldGenParams(..))
import World.Save.Integrity (KnownEntities(..))
import World.Save.Types
    ( SaveData(..), WorldPageSave(..)
    , BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )

-- | Every item-instance id reachable from one 'ItemInstance', including
--   ones nested (recursively) in 'iiContents' — mirrors
--   "World.Save.Snapshot"'s 'World.Save.Snapshot.allItemInstanceIds',
--   just over the legacy 'SaveData'/'WorldPageSave' shape this module
--   still works with rather than a 'World.Save.Snapshot.SessionSnapshot'.
flattenItemInstanceIds' ∷ ItemInstance → [Word64]
flattenItemInstanceIds' i =
    iiInstanceId i : concatMap flattenItemInstanceIds' (iiContents i)

-- | The known-entity id sets (issue #764, save-overhaul C3) every
--   Lua-declared reference is cross-validated against — see
--   "World.Save.Integrity"'s 'KnownEntities' haddock for why
--   @craft_bill@/@ground_item@ are tracked PER PAGE (per-page
--   allocators) while unit/building/item-instance stay session-wide
--   (global allocators). Built once per load from the SAME decoded
--   'SaveData' the existing missing-def-reference ladder already reads
--   (in "Engine.Scripting.Lua.API.Save"'s @continueLoad@), never from
--   live state.
knownEntitiesFromSaveData ∷ SaveData → KnownEntities
knownEntitiesFromSaveData sd = KnownEntities
    { keUnits = HS.fromList
        [ fromIntegral (unUnitId uid)
        | w ← pages, uid ← HM.keys (usnInstances (wpsUnits w)) ]
    , keBuildings = HS.fromList
        [ fromIntegral (unBuildingId bid)
        | w ← pages, bid ← HM.keys (bsnInstances (wpsBuildings w)) ]
    , keBillsByPage = HM.fromList
        [ (wpsPageId w, HS.fromList
              [ fromIntegral (unBillId bid)
              | bid ← HM.keys (cbsBills (wpsCraftBills w)) ])
        | w ← pages ]
    , keItemInstances = HS.fromList (map fromIntegral (concatMap pageItemIds pages))
    , keGroundItemsByPage = HM.fromList
        [ (wpsPageId w, HS.fromList (HM.keys (gisItems (wpsGroundItems w))))
        | w ← pages ]
    , keLocationsByPage = HM.fromList
        [ (wpsPageId w, HS.fromList
              [ unLocationInstanceId (liId inst)
              | inst ← instancesToList
                    (wgpLocationInstances (wpsGenParams w)) ])
        | w ← pages ]
    , keUnitPage = HM.fromList
        [ (fromIntegral (unUnitId uid), wpsPageId w)
        | w ← pages, uid ← HM.keys (usnInstances (wpsUnits w)) ]
    , keNextUnitId = maybe 0 (fromIntegral . usnNextId . wpsUnits) (listToMaybe pages)
    , keNextBuildingId =
        maybe 0 (fromIntegral . bsnNextId . wpsBuildings) (listToMaybe pages)
    , keNextItemId = fromIntegral (sdNextItemInstanceId sd)
    }
  where
    pages = sdWorlds sd
    pageItemIds w =
        concatMap (flattenItemInstanceIds' . giInst)
                  (HM.elems (gisItems (wpsGroundItems w)))
        ⧺ concatMap unitItemIds (HM.elems (usnInstances (wpsUnits w)))
        ⧺ concatMap buildingItemIds (HM.elems (bsnInstances (wpsBuildings w)))
    unitItemIds u =
        concatMap flattenItemInstanceIds' (uisInventory u)
        ⧺ concatMap flattenItemInstanceIds' (HM.elems (uisEquipped u))
        ⧺ concatMap flattenItemInstanceIds' (uisAccessories u)
    buildingItemIds b =
        concatMap (concatMap flattenItemInstanceIds')
                  (HM.elems (bisMaterialsDelivered b))
        ⧺ concatMap flattenItemInstanceIds' (bisStorage b)
