{-# LANGUAGE Strict #-}
-- | The save/load path's known-entity id sets, derived purely from a
--   decoded 'SaveData' (issue #764, save-overhaul C3). Split out of
--   "Engine.Scripting.Lua.API.Save" by issue #985: nothing here takes an
--   @EngineEnv@, so this module stays outside the save/load path's
--   permanent full-access exception
--   (@docs\/engineenv_capability_inventory.md@ §6.1).
module Engine.Scripting.Lua.API.Save.Integrity
    ( knownEntitiesFromSaveData
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingId(..))
import Craft.Bills (CraftBills(..), BillId(..))
import Item.Ground (GroundItems(..), sanitizeGroundItems)
import Item.Types (ItemInstance(..))
import Location.Instance
    (LocationInstance(..), LocationInstanceId(..), instancesToList)
import Unit.Types (UnitId(..))
import World.Generate.Types (WorldGenParams(..))
import World.Save.Integrity (KnownEntities(..))
import World.Save.Types
    ( SaveData(..), WorldPageSave(..)
    , BuildingSnapshot(..), UnitSnapshot(..)
    , ItemWalkOrder(..), pageItemContainers, flattenItemInstances )

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
        [ (wpsPageId w, HS.fromList (HM.keys (gisItems (liveGroundItems w))))
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
    -- #2336: the entries load staging is about to DROP are not part of
    -- the restored session, so they are not known entities either.
    -- 'World.Load.Stage' sanitizes the very same 'wpsGroundItems' with
    -- this very same function, so the two cannot disagree about which
    -- entries survive — which is the whole reason #1589 has the
    -- load-time "does this edge resolve?" answer and the reconcile-time
    -- "should this reference be cleared?" answer come from ONE
    -- 'KnownEntities'. Leaving a dropped id in would tell
    -- @scripts/unit_ai_reconcile.lua@ that a repair job, pickup order or
    -- forage target still points at something, when staging installed
    -- no such item.
    --
    -- Neither consumer refuses the load over it: 'luaReferenceErrors'
    -- is a logged diagnostic at both boundaries, so a Lua edge naming a
    -- dropped item is reported and cleared, never fatal.
    liveGroundItems = fst ∘ sanitizeGroundItems ∘ wpsGroundItems
    -- The save system's single item walk (#1090), over the legacy
    -- 'SaveData'/'WorldPageSave' page shape this module still works
    -- with rather than a 'World.Save.Snapshot.SessionSnapshot' — the
    -- same enumeration 'World.Save.Snapshot.allItemInstanceIds' and
    -- 'World.Save.Types.missingItemDefReferences' walk, with the same
    -- ground substitution as above so a dropped entry's instance id is
    -- not session-wide "known" either.
    pageItemIds w =
        [ iiInstanceId i
        | (_, insts) ← pageItemContainers ItemsGroundFirst
                           liveGroundItems wpsUnits wpsBuildings w
        , inst ← insts
        , i    ← flattenItemInstances inst ]
