-- | The units-and-combat half of the @units-buildings-combat@
--   capability (epic #537, issue #895 — E6a): the ten fields
--   'docs/engineenv_capability_inventory.md' §7.5 identifies as the
--   part that moved first, separate from the three building fields
--   E6b (#896) migrated onto "Engine.Core.Capability.Building".
--
--   Units and combat move __together__ because they already share
--   @unitQueue@\/@combatQueue@'s producer\/consumer relationship and
--   the shutdown-ordering dependency §5 documents on both rows: the
--   combat thread is a @unitQueue@ producer, so it is torn down
--   __before__ the unit thread that drains it
--   (@app\/App\/Graphical.hs@ and its @Offscreen@\/@Headless@ peers).
--   Splitting them would put the two halves of that one contract in
--   two different records.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here); this record's own
--   field prefix is @uc@.
--
--   == What this record deliberately does not carry
--
--   The three __building__ fields — @buildingManagerRef@,
--   @buildingQueue@ and @buildingGhostRef@ — are a conceptually
--   separate domain (§5's own note: \"Building\" is a domain, not a
--   thread; its commands are drained on @UnitThread@). §7.5 assigned
--   them to E6b (#896), which has since landed
--   "Engine.Core.Capability.Building" over exactly those three; per
--   #889's \"no unused capability records ahead of need\", applied
--   field-by-field, they are absent here rather than
--   present-but-unused. A module that needs both halves takes both
--   records — "Unit.Thread" is the worked example: it drains the
--   building command queue on the unit thread (there is no building
--   thread), and since #896 hands
--   @Building.Thread.Command.processAllBuildingCommands@ that narrow
--   record plus the logger and world\/sim view rather than its whole
--   environment.
--
--   == @statRNGRef@ is shared, and stays shared
--
--   §7.5 flags @statRNGRef@ as the one field of this group with four
--   writer roles — @UnitThread@, @CombatThread@, @WorldThread@
--   (dig-yield rolls) and @LuaThread@ — and asks whether it needs its
--   own tiny capability. It does not: the established
--   __explicit-narrow-parameter__ rule covers it. A world-side
--   consumer that has no other business with units or combat takes the
--   live 'System.Random.StdGen' ref (and, where it also enqueues unit
--   work, the live @unitQueue@) as an ordinary function parameter
--   rather than adopting this whole record —
--   "World.Thread.Command.Edit.Dig" is the worked example, and it is
--   the same shape @Engine.Input.Callback@ already used to avoid
--   needing an input record in #892.
--
--   == Thread-access contracts that ride along (§5)
--
--   This record grants no new read or write authority — it only removes
--   the ability to reach fields a unit\/combat consumer has no business
--   touching. The per-field contracts §5's @units-buildings-combat@
--   table records still hold exactly as written; the ones with the
--   least obvious shapes (the shutdown ordering, the four event
--   streams' producer\/drain roles, and @statRNGRef@'s four-role
--   sharing) are restated on their fields below.
--
--   Like the other capability modules, this one imports only the narrow
--   slice of @Engine.Core.State@ it needs (the bare 'EngineEnv' type
--   plus the ten field accessors) rather than @EngineEnv(..)@ or a
--   bare import, so it is not itself a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.UnitCombat
  ( UnitCombatCapability(..)
  , toUnitCombatCapability
  ) where

-- (No `import UPrelude`: every field's type is named explicitly below,
-- so nothing from the prelude is in use — the same shape
-- "Engine.Core.Capability.ContentRegistries" already has.)
import Data.IORef (IORef)
import Data.Sequence (Seq)
import System.Random (StdGen)
import Engine.Core.Queue as Q
import qualified Combat.Types
import Engine.ActionOutcome (ActionOutcome)
import Unit.Command.Types (UnitCommand)
import Unit.Types (UnitManager)
import Unit.Sim.Types (UnitThreadState)
import Unit.Pathing.Config (PathingConfig)
import Engine.Core.State
  ( EngineEnv
  , unitManagerRef, unitQueue, utsRef, statRNGRef, treatRNGRef, combatQueue
  , combatEventsRef, injuryEventsRef, thoughtEventsRef, actionOutcomeRef
  , pathingConfigRef
  )

-- | The units-and-combat slice of @units-buildings-combat@: the unit
--   roster, the unit and combat command queues, the sim-side per-unit
--   thread state, the runtime stat RNG, the medical-treatment RNG,
--   the three @CombatEvent@-shaped
--   event streams, the F4 action-outcome tap, and the pathing
--   tunables. See 'docs/engineenv_capability_inventory.md' §5
--   @units-buildings-combat@ and §7.5.
data UnitCombatCapability = UnitCombatCapability
  { ucUnitManagerRef   ∷ IORef UnitManager
    -- ^ Session-replaced, multi-writer. Written by @UnitThread@
    --   (@Thread.Command.Lifecycle@\/@Command.Pose@), @CombatThread@
    --   (wound application, periodic wound ticks, weapon wear — all via
    --   @atomicModifyIORef'@), @WorldThread@ (load publish) and
    --   @LuaThread@ (@unit.spawn@'s unit-id allocation); read by those
    --   same four roles.
  , ucUnitQueue        ∷ Q.Queue UnitCommand
    -- ^ Drained by @UnitThread@ only; produced by @CombatThread@
    --   (@UnitKill@\/@UnitCollapse@ from wound ticks and resolution
    --   events), @WorldThread@ (basic\/dig\/terrain edits, and the load
    --   publish's stale-queue discard) and @LuaThread@ (@unit.spawn@).
    --   __Shutdown ordering:__ the combat thread is a producer here, so
    --   it is stopped __before__ the unit thread that consumes this
    --   queue — see @app\/App\/Graphical.hs@ and the identical
    --   rationale on 'ucCombatQueue'.
  , ucUtsRef           ∷ IORef UnitThreadState
    -- ^ Sim-side per-unit state (position, pose, activity, target,
    --   path, @*Until@ timers). Single-thread-owned by @UnitThread@
    --   outside a load publish (@WorldThread@) or a save capture
    --   (@WorldThread@, read-only) — it lives on 'EngineEnv' rather
    --   than inside the unit thread precisely so those two can reach
    --   it. Also read by @LuaThread@ for @unit.getInfo@.
  , ucStatRNGRef       ∷ IORef StdGen
    -- ^ Runtime RNG for stat rolls, seeded from system entropy at
    --   startup — deliberately __not__ world-seeded, so stats are
    --   non-deterministic across runs. Shared by all four of
    --   @UnitThread@ (spawn, climb), @CombatThread@ (resolution, wound
    --   ticks), @WorldThread@ (dig-yield rolls) and @LuaThread@
    --   (foraging); every roll both reads and advances the generator,
    --   and there is no cross-writer ordering guarantee beyond each
    --   individual roll's own atomicity. World-side consumers take it
    --   as an explicit narrow parameter instead of adopting this
    --   record — see this module's header.
  , ucTreatRNGRef      ∷ IORef StdGen
    -- ^ Runtime RNG for MEDICAL TREATMENT rolls and nothing else
    --   (#2297), seeded from system entropy at startup exactly like
    --   'ucStatRNGRef'. Single-writer by contract: only
    --   @LuaThread@'s @Engine.Scripting.Lua.API.Units.Medical@ touches
    --   it, so its claim-then-advance protocol cannot hand the same
    --   generator to two consumers the way sharing 'ucStatRNGRef'
    --   would. See the field's own note on 'EngineEnv' for why a
    --   treatment cannot claim from the four-writer pool.
  , ucCombatQueue      ∷ Q.Queue Combat.Types.CombatCommand
    -- ^ Lua \/ AI → combat thread, drained at the combat thread's 60 Hz
    --   tick by @Combat.Thread.processAllCommands@; produced by
    --   @LuaThread@ (@combat.attack@) and cleared by @WorldThread@'s
    --   load publish. The combat thread is the consumer here but the
    --   __producer__ for 'ucUnitQueue', which is what fixes the
    --   shutdown order between the two threads.
  , ucCombatEventsRef  ∷ IORef (Seq Combat.Types.CombatEvent)
    -- ^ Combat thread → Lua. Produced by @CombatThread@ (resolution
    --   events, wound ticks) and by @LuaThread@'s own
    --   @combat.emitDeath@; reset to empty by @WorldThread@'s load
    --   publish. __Drained__ by @LuaThread@ via @combat.drainEvents@
    --   into the combat-log UI — a streaming consumer, so a test that
    --   drains it by hand while that panel script is loaded races it.
    --   Runtime only, never persisted.
  , ucInjuryEventsRef  ∷ IORef (Seq Combat.Types.CombatEvent)
    -- ^ NON-combat injury stream (falls, hazards, wound-caused deaths)
    --   → Lua, reusing the @CombatEvent@ shape with the victim in
    --   @target@. Produced by @UnitThread@ (falls), @LuaThread@
    --   (@unit.injure@, @injury.emit@) and reset by @WorldThread@'s
    --   load publish; __drained__ by @LuaThread@ via
    --   @injury.drainEvents@ into the injury-log UI, with the same
    --   streaming-consumer caveat as 'ucCombatEventsRef'. Runtime only.
  , ucThoughtEventsRef ∷ IORef (Seq Combat.Types.CombatEvent)
    -- ^ Per-unit thought stream (#351) → Lua, same @CombatEvent@ shape
    --   again (target = the thinking unit). Purely @LuaThread@-produced
    --   (@scripts/thoughts.lua@ via @thought.emit@) and reset by
    --   @WorldThread@'s load publish; __drained__ by @LuaThread@ via
    --   @thought.drainEvents@. Runtime only.
  , ucActionOutcomeRef ∷ IORef (Seq ActionOutcome)
    -- ^ F4 (#646) action-outcome oracle tap — what actually happened to
    --   a player action, even when nothing user-facing fired. Produced
    --   by @LuaThread@ (@debug.recordOutcome@), @WorldThread@ (the
    --   designation cursor handlers' partial-drop counts, and the load
    --   publish's reset) and @InputThread@ (key\/click routing
    --   outcomes); __drained__ by @LuaThread@ via
    --   @debug.drainActionOutcomes@ for the playtest harness's critic.
    --   Never surfaced to the player; runtime only.
  , ucPathingConfigRef ∷ IORef PathingConfig
    -- ^ Unit pathing cost tunables (climb\/ramp\/fall\/river\/lake
    --   penalties + replan threshold), loaded once at boot from
    --   @config/pathing.yaml@. No writers today — it is an 'IORef' so a
    --   future settings UI can retune routing live, and the movement
    --   tick rereads it every tick. Read by @UnitThread@ only.
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toUnitCombatCapability ∷ EngineEnv → UnitCombatCapability
toUnitCombatCapability env = UnitCombatCapability
  { ucUnitManagerRef   = unitManagerRef env
  , ucUnitQueue        = unitQueue env
  , ucUtsRef           = utsRef env
  , ucStatRNGRef       = statRNGRef env
  , ucTreatRNGRef      = treatRNGRef env
  , ucCombatQueue      = combatQueue env
  , ucCombatEventsRef  = combatEventsRef env
  , ucInjuryEventsRef  = injuryEventsRef env
  , ucThoughtEventsRef = thoughtEventsRef env
  , ucActionOutcomeRef = actionOutcomeRef env
  , ucPathingConfigRef = pathingConfigRef env
  }
