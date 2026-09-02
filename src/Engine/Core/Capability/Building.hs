-- | The buildings half of the @units-buildings-combat@ capability
--   (epic #537, issue #896 — E6b): the three fields
--   'docs/engineenv_capability_inventory.md' §7.5 held back from E6a
--   (#895) as the separable building domain — @buildingManagerRef@,
--   @buildingQueue@ and @buildingGhostRef@. Together with
--   "Engine.Core.Capability.UnitCombat" this clears §6.2's
--   @units-buildings-combat@ row completely.
--
--   __\"Building\" is a domain, not a thread__ (§2.2, and §5's own
--   note on @buildingManagerRef@). There is no building thread: the
--   command queue below is drained on @UnitThread@, inside
--   "Unit.Thread"'s tick, by
--   'Building.Thread.Command.processAllBuildingCommands'. That is
--   precisely why the record is separate from
--   'Engine.Core.Capability.UnitCombat.UnitCombatCapability' rather
--   than folded into it: a module that only spawns or queries
--   buildings has no business reaching the unit roster or the combat
--   event streams, and @Unit.Thread@ — the one genuinely mixed
--   consumer — takes both records instead of the whole environment.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here); this record's own
--   field prefix is @bc@.
--
--   == No split view is needed here
--
--   §3.1's main-only\/worker-safe split (the shape
--   "Engine.Core.Capability.Render" needs for @engineStateRef@) does
--   not apply: none of the three fields is confined to a single
--   thread. All three are already reached from more than one
--   execution role — see the per-field contracts below — so one record
--   serves every consumer.
--
--   == Thread-access contracts that ride along (§5)
--
--   This record grants no new read or write authority; it only removes
--   the ability to reach fields a building consumer has no business
--   touching. The per-field contracts §5's @units-buildings-combat@
--   table records still hold exactly as written, and the two with the
--   least obvious shapes — the queue's drained-on-@UnitThread@ reality
--   and the ghost slot's transient, re-established-every-tick role —
--   are restated on their fields below.
--
--   Like the other capability modules, this one imports only the narrow
--   slice of @Engine.Core.State@ it needs (the bare 'EngineEnv' type
--   plus the three field accessors) rather than @EngineEnv(..)@ or a
--   bare import, so it is not itself a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.Building
  ( BuildingCapability(..)
  , toBuildingCapability
  ) where

import UPrelude
import Data.IORef (IORef)
import Engine.Core.Queue as Q
import Building.Types (BuildingManager, BuildingGhost)
import Building.Command.Types (BuildingCommand)
import Engine.Core.State
  ( EngineEnv
  , buildingManagerRef, buildingQueue, buildingGhostRef
  )

-- | The buildings slice of @units-buildings-combat@: the placed
--   building roster (with its def catalogue and selection), the
--   building command queue, and the single-slot placement ghost. See
--   'docs/engineenv_capability_inventory.md' §5
--   @units-buildings-combat@ and §7.5.
data BuildingCapability = BuildingCapability
  { bcBuildingManagerRef ∷ IORef BuildingManager
    -- ^ Session-replaced, multi-writer. Holds the placed
    --   'Building.Types.BuildingInstance' map (build progress,
    --   delivered materials, storage contents, spawn-roster
    --   countdown), the @bdDefs@ catalogue loaded from
    --   @data/structure_packs/*.yaml@, @bmSelected@, and — since
    --   #2091 — @bmDestructions@, the transient render-only
    --   presentations of demolished buildings (captured and pruned by
    --   the drain on @UnitThread@, read by the render pass). Written by
    --   @UnitThread@ (via "Building.Thread.Command", drained on that
    --   thread), @WorldThread@ (load publish) and @LuaThread@
    --   (@building.select@\/@building.deselect@, and every
    --   progress\/materials\/storage verb); read by those same roles
    --   plus @WorldThread@'s cursor-quad, power and item-temperature
    --   passes.
  , bcBuildingQueue      ∷ Q.Queue BuildingCommand
    -- ^ Spawn\/destroy\/clear-all commands. __Drained on
    --   @UnitThread@__, not on a thread of its own — there is no
    --   building thread (§2.2), so "Unit.Thread"'s tick calls
    --   'Building.Thread.Command.processAllBuildingCommands' after its
    --   own command\/movement work and before its save-barrier
    --   acknowledgements. Produced by @LuaThread@
    --   (@building.spawn@\/@building.destroy@, @power.placeNode@) and
    --   cleared by @WorldThread@'s load publish
    --   (@discardStaleQueues@).
  , bcBuildingGhostRef   ∷ IORef (Maybe BuildingGhost)
    -- ^ Single-slot placement preview. Transient by design: the Lua
    --   build tool re-establishes it via @building.setGhost@ on every
    --   tick the placement mode is armed and drops it with
    --   @building.clearGhost@, so it is never persisted and a load
    --   publish always clears it. Written by @LuaThread@ (the build
    --   tool) and @WorldThread@ (load publish); read by @WorldThread@
    --   when it builds the preview quad.
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toBuildingCapability ∷ EngineEnv → BuildingCapability
toBuildingCapability env = BuildingCapability
  { bcBuildingManagerRef = buildingManagerRef env
  , bcBuildingQueue      = buildingQueue env
  , bcBuildingGhostRef   = buildingGhostRef env
  }
