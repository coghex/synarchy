-- | The entity + entity-adjacent component FAÇADE (issue #760,
--   save-overhaul B2; split into owners by #2150).
--
--   Five page-scoped components live behind this name, all validated
--   against the @"world-pages"@ authority (requirement 8). Their
--   registry-declared dependencies, which are the contract the assembly
--   order is derived from:
--
--   - @"buildings"@ (required) — depends on @"world-pages"@ +
--     @"core-session"@.
--   - @"units"@ (required) — depends on @"world-pages"@ +
--     @"core-session"@.
--   - @"unit-sim"@ (required) — depends on @"world-pages"@ + @"units"@:
--     a sim state must have a matching unit (the orphan check runs at
--     whole-session assembly).
--   - @"craft-bills"@ (required) — depends on @"world-pages"@ +
--     @"buildings"@.
--   - @"power-nodes"@ (required) — depends on @"world-pages"@ +
--     @"buildings"@.
--
--   The @"buildings"@/@"units"@ dependency on @"core-session"@ is the
--   global 'Building.Types.BuildingId'/'Unit.Types.UnitId' allocator:
--   neither component carries a per-page next-id counter, so assembly
--   must run after the one component that owns those allocators.
--   @"craft-bills"@/@"power-nodes"@ depend on @"buildings"@ for ORDERING
--   only, not as a hard orphan reject — a demolished station's lingering
--   bill or node is tolerated gameplay behaviour (see the
--   "World.Save.Snapshot" haddock).
--
--   This module declares nothing. It re-exports the five components'
--   contracts from three owners, so a downstream module keeps importing
--   one name and does not have to learn the internal topology. The
--   owners are SIBLINGS: none imports another, and none imports this
--   façade.
--
--   ["World.Save.Component.EntitySnapshots"]
--     The per-page entity INSTANCE snapshots — @"buildings"@ and
--     @"units"@: their current and frozen instance DTOs, the
--     'StatModifierDTO'/'WoundDTO'/'ScarDTO' leaves units embed, the
--     page/collection shapes, the v1 migrations, both codecs and both
--     assembly functions.
--
--   ["World.Save.Component.EntitySimulation"]
--     The per-page unit SIMULATION state — @"unit-sim"@: the current
--     move-target and sim-state DTOs, the frozen pre-#1217 v1 shapes,
--     all three page-slice versions, every conversion and migration
--     between them, the codec and the assembly function.
--
--   ["World.Save.Component.EntitySystems"]
--     The per-page ATTACHED-SYSTEM registries — @"craft-bills"@ and
--     @"power-nodes"@: their current and frozen bill/queue and
--     node/registry DTOs, the v1 migrations, the two allocator
--     validators, both codecs and both assembly functions.
--
--   Why three owners and not five, one per component: the split follows
--   how the contracts actually evolve, not the registry's component
--   count. @"buildings"@ and @"units"@ moved together through the #1233
--   item-tree freeze and share the same allocator-above-slices rule;
--   @"craft-bills"@ and @"power-nodes"@ are the only two carrying a
--   per-page allocator and a 'csValidate' (#1667), and they gained both
--   in one change. @"unit-sim"@ alone has three live versions.
--   @tech_debt.md@'s TD-10, the finding this split came from, sketched
--   five focused modules but anticipated the judgement call — it notes
--   that 'StatModifierDTO'/'WoundDTO'/'ScarDTO' could stay colocated
--   with 'UnitInstanceDTO' and that "extra separation should be
--   justified by review clarity rather than line count alone". Three
--   owners is that call; it is recorded here so a later reader does not
--   re-open it as an oversight.
--
--   The split moved declarations only. No wire fact — constructor order,
--   record-field order, field type, strictness, constructor arity,
--   'Data.Serialize.Serialize' derivation, or current-versus-historical
--   status — changed, no component specification changed, and each
--   declaration and function still has exactly one definition. The one
--   helper the owners shared, the deterministic page ordering every
--   codec encodes its slices in, is single-sourced in
--   "World.Save.PageOrder" rather than copied into each.
module World.Save.Component.Entities
    ( buildingsCodec
    , unitsCodec
    , unitSimCodec
    , craftBillsCodec
    , powerNodesCodec
    , PageBuildingsDTO(..)
    , BuildingsDTO(..)
    , BuildingInstanceDTO(..)
    , fromBuildingInstanceDTO
    , BuildingInstanceDTOv1(..)
    , PageBuildingsDTOv1(..)
    , BuildingsDTOv1(..)
    , toBuildingInstanceDTOv1
    , migrateBuildingsDTOv1
    , PageUnitsDTO(..)
    , UnitsDTO(..)
    , UnitInstanceDTO(..)
    , StatModifierDTO(..)
    , WoundDTO(..)
    , ScarDTO(..)
    , toUnitInstanceDTO
    , fromUnitInstanceDTO
    , UnitInstanceDTOv1(..)
    , PageUnitsDTOv1(..)
    , UnitsDTOv1(..)
    , toUnitInstanceDTOv1
    , migrateUnitsDTOv1
    , PageSimDTO(..)
    , UnitSimDTO(..)
    , PageSimDTOv1(..)
    , UnitSimDTOv1(..)
    , PageSimDTOv2(..)
    , UnitSimDTOv2(..)
    , migratePageSimDTOv1
    , migrateUnitSimDTOv1
    , migratePageSimDTOv2
    , migrateUnitSimDTOv2
    , UnitSimStateDTO(..)
    , MoveTargetDTO(..)
    , UnitSimStateDTOv1(..)
    , MoveTargetDTOv1(..)
    , toUnitSimStateDTOv1
    , migrateUnitSimStateDTOv1
    , PageCraftBillsDTO(..)
    , CraftBillsDTO(..)
    , CraftBillDTO(..)
    , BillQueueDTO(..)
    , CraftBillDTOv1(..)
    , BillQueueDTOv1(..)
    , PageCraftBillsDTOv1(..)
    , CraftBillsDTOv1(..)
    , migrateCraftBillDTOv1
    , migrateCraftBillsDTOv1
    , PagePowerNodesDTO(..)
    , PowerNodesDTO(..)
    , PowerNodeDTO(..)
    , NodeRegistryDTO(..)
    , PowerNodeDTOv1(..)
    , NodeRegistryDTOv1(..)
    , PagePowerNodesDTOv1(..)
    , PowerNodesDTOv1(..)
    , migratePowerNodeDTOv1
    , migratePowerNodesDTOv1
    , toUnitSimStateDTO
    , fromUnitSimStateDTO
    , toCraftBillDTO
    , toBillQueueDTO
    , fromBillQueueDTO
    , toNodeRegistryDTO
    , fromNodeRegistryDTO
    , applyBuildings
    , applyUnits
    , applyUnitSim
    , applyCraftBills
    , applyPowerNodes
    , validateCraftBills
    , validatePowerNodes
    ) where

import World.Save.Component.EntitySnapshots
import World.Save.Component.EntitySimulation
import World.Save.Component.EntitySystems
