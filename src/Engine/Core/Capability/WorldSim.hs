-- | The world\/sim\/time half of the @world-sim-render-handoff@
--   capability (epic #537, issue #893 — E5a): the nine fields
--   'docs/engineenv_capability_inventory.md' §7.4 identifies as the
--   part that "can move on its own", separate from the seven coupled
--   render-handoff fields E5b (#894) migrates once
--   "Engine.Core.Capability.Render" (#891, E3) is in place.
--
--   Follows E1's convention verbatim (see
--   "Engine.Core.Capability.Core" for the full statement of it): one
--   record named @\<Name\>Capability@ with fields prefixed by the
--   record's own initials (here @ws@), one total one-way
--   @to\<Name\>Capability@ projection, every field the exact same live
--   'IORef'\/'Engine.Core.Queue.Queue' 'EngineEnv' already carries
--   (never a copy or a reconstruction), and no import of any consumer
--   of this module.
--
--   == What this record deliberately does not carry
--
--   The seven __coupled render-handoff__ fields — @worldPreviewRef@,
--   @worldPreviewGenerationRef@, @zoomAtlasDataRef@, @worldQuadsRef@,
--   @bloodDisposeQueue@, @texPaletteRef@ and @texPaletteHandlesRef@ —
--   are the world thread's staging surface for @MainRender@ GPU
--   uploads and the structure-palette translation table, so their
--   consumers straddle this capability and @render-gpu-asset@. §7.4
--   assigns them to E5b (#894); per #889's "no unused capability
--   records ahead of need", applied field-by-field, they are absent
--   here rather than present-but-unused. The four §6.2 modules that
--   genuinely need them keep their temporary full-access entry until
--   #894 lands: @Engine.Scripting.Lua.API.Structure@, @World.Thread@,
--   @World.Thread.Command.Basic@ and @World.Thread.Command.Init@.
--
--   == Thread-access contracts that ride along (§5)
--
--   This record grants no new read or write authority — it only removes
--   the ability to reach fields a world\/sim consumer has no business
--   touching. The per-field contracts §5's @world-sim-render-handoff@
--   table records still hold exactly as written, and the two with the
--   least obvious shapes are restated on their fields below:
--   'wsGameTimeRef' has a concrete, enumerated multi-role reader set
--   (deliberately not @AnyThread@), and 'wsEnginePausedRef' is
--   authoritative over any Lua-side copy of the pause flag.
--
--   Like the other capability modules, this one imports only the narrow
--   slice of @Engine.Core.State@ it needs (the bare 'EngineEnv' type
--   plus the nine field accessors) rather than @EngineEnv(..)@ or a
--   bare import, so it is not itself a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.WorldSim
  ( WorldSimCapability(..)
  , toWorldSimCapability
  ) where

import UPrelude
import Data.IORef (IORef)
import Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand)
import World.Generate.Config (WorldGenConfig)
import World.Material (MaterialRegistry)
import World.Types (WorldCommand, WorldManager, FloraCatalog)
import Engine.Core.State
  ( EngineEnv
  , worldManagerRef, worldQueue, sunAngleRef, floraCatalogRef
  , materialRegistryRef, worldGenConfigRef, gameTimeRef, enginePausedRef
  , simQueue
  )

-- | The world\/sim\/time slice of @world-sim-render-handoff@: the world
--   page manager, the world and sim command queues, the derived sun
--   angle, the flora and material registries, the global worldgen
--   tunables, the game clock, and the global pause flag. See
--   'docs/engineenv_capability_inventory.md' §5
--   @world-sim-render-handoff@ and §7.4.
data WorldSimCapability = WorldSimCapability
  { wsWorldManagerRef     ∷ IORef WorldManager
    -- ^ Session-replaced. Written by @WorldThread@ (world
    --   init\/load\/edit commands and load publish); read from
    --   @UnitThread@, @CombatThread@, @WorldThread@, @LuaThread@ and
    --   @MainRender@ alike.
  , wsWorldQueue          ∷ Q.Queue WorldCommand
    -- ^ Drained by @WorldThread@ only; produced by @LuaThread@,
    --   @SimThread@, @WorldThread@ (deferred-command re-enqueue) and
    --   @MainRender@ (the @--dump@ driver).
  , wsSunAngleRef         ∷ IORef Float
    -- ^ Written by @WorldThread@ (derived from world time) and by
    --   @LuaThread@'s direct @world.setSunAngle@ override; read by
    --   @LuaThread@ and @MainRender@ (lighting).
  , wsFloraCatalogRef     ∷ IORef FloraCatalog
    -- ^ Populated from YAML by @LuaThread@'s content load; read by
    --   @WorldThread@ and @LuaThread@.
  , wsMaterialRegistryRef ∷ IORef MaterialRegistry
    -- ^ Session-replaced, multi-writer: populated per world init\/load
    --   by @WorldThread@ and registered into by @LuaThread@ from the
    --   same YAML content; read by @UnitThread@, @WorldThread@,
    --   @LuaThread@ and @MainRender@ (the @--dump@ driver).
  , wsWorldGenConfigRef   ∷ IORef WorldGenConfig
    -- ^ Global worldgen tunables — distinct from a specific world's
    --   own @wpsGenParams@. Read by @WorldThread@, read\/written by
    --   @LuaThread@.
  , wsGameTimeRef         ∷ IORef Double
    -- ^ Monotonic-while-unpaused game clock, persisted exactly.
    --   Written by @UnitThread@ (once per unpaused tick) and
    --   @WorldThread@ (load publish); read by a concrete, enumerated
    --   set of roles — @InputThread@, @CombatThread@, @WorldThread@,
    --   @UnitThread@ and @LuaThread@ — for event\/log timestamping.
    --   §5 enumerates them rather than writing @AnyThread@ on purpose:
    --   that identifier is reserved for a field with an explicitly
    --   documented unrestricted-access contract, and this field has no
    --   such contract, just a wide but ordinary reader set.
  , wsEnginePausedRef     ∷ IORef Bool
    -- ^ Global pause flag, persisted exactly and __authoritative over
    --   any Lua-side copy__. Written by @LuaThread@
    --   (@engine.setPaused@) and @WorldThread@ (load publish — a load
    --   always comes up paused). @WorldThread@\/@UnitThread@\/
    --   @SimThread@\/@CombatThread@ skip advancing simulated state
    --   while it is true; @MainRender@ keeps rendering and dispatching
    --   input regardless.
  , wsSimQueue            ∷ Q.Queue SimCommand
    -- ^ Drained by @SimThread@ only; produced by @WorldThread@ (chunk
    --   loading, basic\/sync\/UI commands, and the load publish's stale
    --   queue discard) and @MainRender@ (the @--dump@ driver).
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toWorldSimCapability ∷ EngineEnv → WorldSimCapability
toWorldSimCapability env = WorldSimCapability
  { wsWorldManagerRef     = worldManagerRef env
  , wsWorldQueue          = worldQueue env
  , wsSunAngleRef         = sunAngleRef env
  , wsFloraCatalogRef     = floraCatalogRef env
  , wsMaterialRegistryRef = materialRegistryRef env
  , wsWorldGenConfigRef   = worldGenConfigRef env
  , wsGameTimeRef         = gameTimeRef env
  , wsEnginePausedRef     = enginePausedRef env
  , wsSimQueue            = simQueue env
  }
