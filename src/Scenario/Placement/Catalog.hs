{-# LANGUAGE Strict #-}
-- | The placement half of the scenario definition catalog (#2712, SCN-12
--   of epic #2698): which buildings, locations, structure packs and loot
--   profiles a scenario may name, read off the LIVE registries.
--
--   "Scenario.Decode" validates a building, location or structure entry
--   against these rows; the construction adapters
--   ("Scenario.Placement.Building", "Scenario.Placement.Location",
--   "Scenario.Placement.Structure") and capture read the same registries,
--   so the decoder and the adapters can never disagree about a footprint
--   or a slot. Plain data: the runtime builds it from the loaded
--   registries and the tests build fixtures.
module Scenario.Placement.Catalog
    ( buildingCatalogOf
    , buildingCatalogEntryOf
    , locationCatalogOf
    , locationCatalogEntryOf
    , packCatalogsOf
    , lootProfileCatalogOf
    , withPlacementCatalog
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Building.Types (BuildingDef(..))
import Location.Bounds (RelBounds(..))
import Location.Instance (containerSlotsFromDef, locationDefHasEncounter, significantItemsFromDef
                         , LocationContainerSlot(..), LocationSignificantItem(..))
import Location.Types (LocationDef(..), LocationRegistry, allLocations)
import LootProfile.Types (LootProfileRegistry, lootProfileIds)
import Power.Base (PowerNodeSpec(..))
import Scenario.Types
import Structure.ArtCatalog
    ( AppearanceKey(..), PackArt(..), StructureArtCatalog(..), pieceKindName )

-- | A building definition's catalog row: its inclusive footprint offsets
--   from the anchor, its build work, the material items it consumes and —
--   for a power-storage building only — its capacity.
buildingCatalogEntryOf ∷ BuildingDef → BuildingCatalogEntry
buildingCatalogEntryOf d = BuildingCatalogEntry
    { bcFootprint = Footprint 0 0 (bdTileW d - 1) (bdTileH d - 1)
    , bcBuildWork = bdBuildWork d
    , bcMaterials = HS.fromList (HM.keys (bdMaterials d))
    , bcPowerCapacity = case bdPowerNode d of
        Just (PowerNodeStorage wh) → Just wh
        _                          → Nothing
    }

buildingCatalogOf ∷ HM.HashMap Text BuildingDef → HM.HashMap Text BuildingCatalogEntry
buildingCatalogOf = HM.map buildingCatalogEntryOf

-- | A location definition's catalog row. The slot lists are the ones
--   'Location.Instance' derives for an instance (so slot N here is slot N
--   there): the item definition each significant slot requires, and the
--   container item definition and default loot profile of each container
--   slot.
locationCatalogEntryOf ∷ LocationDef → LocationCatalogEntry
locationCatalogEntryOf d = LocationCatalogEntry
    { lcFootprint = let RelBounds x0 y0 x1 y1 = ldBounds d in Footprint x0 y0 x1 y1
    , lcSignificantSlots = map lsiItemDefName (significantItemsFromDef d)
    , lcContainerSlots = map lcsItemDefName slots
    , lcContainerProfiles = [ fromMaybe "" (lcsProfile s) | s ← slots ]
    , lcHasEncounter = locationDefHasEncounter d
    }
  where slots = containerSlotsFromDef d

locationCatalogOf ∷ LocationRegistry → HM.HashMap Text LocationCatalogEntry
locationCatalogOf reg = HM.fromList
    [ (ldId d, locationCatalogEntryOf d) | d ← allLocations reg ]

-- | The structure packs a scenario may name: pack → the piece KINDS it
--   declares (@floor@, @ceiling@, @wall@, @post@, @wire@), and pack → the
--   variant names its art declares ('pkVariantArt').
packCatalogsOf ∷ StructureArtCatalog
               → (HM.HashMap Text (HS.HashSet Text), HM.HashMap Text (HS.HashSet Text))
packCatalogsOf cat =
    ( HM.map (\p → HS.fromList (map pieceKindName (S.toList (pkKinds p)))) packs
    , HM.map (\p → HS.fromList [ v | AppearanceKey (Just v) _ ← M.keys (pkVariantArt p) ]) packs )
  where packs = sacPacks cat

lootProfileCatalogOf ∷ LootProfileRegistry → HS.HashSet Text
lootProfileCatalogOf = HS.fromList ∘ lootProfileIds

-- | Fill a catalog's placement rows from the live registries.
withPlacementCatalog ∷ HM.HashMap Text BuildingDef → LocationRegistry
                     → StructureArtCatalog → LootProfileRegistry
                     → ScenarioCatalog → ScenarioCatalog
withPlacementCatalog buildings locations art profiles cat =
    let (kinds, variants) = packCatalogsOf art
    in cat { catBuildings = buildingCatalogOf buildings
           , catLocations = locationCatalogOf locations
           , catStructurePacks = kinds
           , catPackVariants = variants
           , catLootProfiles = lootProfileCatalogOf profiles }
