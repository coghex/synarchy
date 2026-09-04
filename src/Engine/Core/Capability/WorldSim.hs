-- | The world\/sim\/time half of the @world-sim-render-handoff@
--   capability (epic #537, issue #893 — E5a): the fields
--   'docs/engineenv_capability_inventory.md' §7.4 identifies as the
--   part that "can move on its own", separate from the seven coupled
--   render-handoff fields E5b (#894) migrates once
--   "Engine.Core.Capability.Render" (#891, E3) is in place.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here); this record's own
--   field prefix is @ws@.
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
--   plus its own field accessors) rather than @EngineEnv(..)@ or a
--   bare import, so it is not itself a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.WorldSim
  ( WorldSimCapability(..)
  , toWorldSimCapability
  , withPlayerIntent
  , withPlayerIntentHeld
  , restoreIfPlayerIdle
  ) where

import UPrelude
import Control.Concurrent.MVar (MVar, modifyMVar, withMVar)
import Data.IORef (IORef)
import Engine.Graphics.Solar (SolarBase(..))
import Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand)
import World.Generate.Config (WorldGenConfig)
import World.Material (MaterialRegistry)
import World.Types (WorldCommand, WorldManager, FloraCatalog)
import Engine.Core.State
  ( EngineEnv
  , worldManagerRef, worldQueue, sunAngleRef, floraCatalogRef
  , materialRegistryRef, worldGenConfigRef, gameTimeRef, enginePausedRef
  , playerIntentGenRef, enginePauseGenRef, simQueue
  )

-- | The world\/sim\/time slice of @world-sim-render-handoff@: the world
--   page manager, the world and sim command queues, the derived sun
--   angle, the flora and material registries, the global worldgen
--   tunables, the game clock, the global pause flag, and (#913) the
--   player-intent generation that flag and the per-page time scale share.
--   See 'docs/engineenv_capability_inventory.md' §5
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
  , wsSunAngleRef         ∷ IORef SolarBase
    -- ^ Written by @WorldThread@ (derived from the visible head page's
    --   world time) and by @LuaThread@'s direct @world.setSunAngle@
    --   override; read by @LuaThread@ and @MainRender@ (lighting), and
    --   by @WorldThread@ again when it builds a frame's per-page solar
    --   table (#1869).
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
    -- ^ Monotonic-while-unpaused game clock, persisted exactly —
    --   monotonic WITHIN a session, that is: the two session boundaries
    --   replace the value outright. Written by @UnitThread@ (once per
    --   unpaused tick, and once more at the Exit-to-Menu boundary, where
    --   @Unit.Thread.endSessionEpoch@ restores
    --   'Engine.Core.SessionEpoch.freshSessionGameTime' — #2291) and
    --   @WorldThread@ (load publish, which installs the save's own
    --   @sdGameTime@); read by a concrete, enumerated
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
  , wsPlayerIntentGenRef  ∷ MVar Word64
    -- ^ #913's player-intent generation, sitting here because the two
    --   verbs that bump it are exactly the two clock-authority verbs
    --   this record already owns: @engine.setPaused@ (writing
    --   'wsEnginePausedRef') and @world.setTimeScale@ (queueing onto
    --   'wsWorldQueue'). Bumped by @LuaThread@ only, at the moment the
    --   player expresses the intent; read by @WorldThread@ at the end
    --   of an autosave transaction. An 'MVar' because it doubles as the
    --   MUTEX serializing those transitions against that read-then-write
    --   restore — see 'withPlayerIntent' \/ 'restoreIfPlayerIdle' and
    --   'Engine.Core.State's field haddock (which also covers why
    --   engine-internal pause\/scale writes must NOT bump it).
  , wsEnginePauseGenRef   ∷ IORef Word64
    -- ^ #1730's engine-pause generation: how many times a pause source
    --   INDEPENDENT of any running save has asserted a pause. Bumped by
    --   @WorldThread@ and @LuaThread@ alike through
    --   'World.Pause.imposePause' (a @pause: true@ notification
    --   category, an @engine.loadSave@ acceptance); read by @LuaThread@
    --   at an autosave's acceptance and by @WorldThread@ at its restore.
    --   Never touched outside the 'wsPlayerIntentGenRef' critical
    --   section — every epoch transition and both of those sites hold
    --   that mutex — which is what makes \"has anyone else paused since
    --   acceptance?\" linearizable against the restore that asks it.
    --   Deliberately NOT bumped by the save's own pause or by the
    --   player's, so a declined restore can name its real reason. See
    --   'Engine.Core.State's field haddock.
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
  , wsPlayerIntentGenRef  = playerIntentGenRef env
  , wsEnginePauseGenRef   = enginePauseGenRef env
  , wsSimQueue            = simQueue env
  }

-- | Record ONE player-intent transition: apply the caller's own
--   pause\/time-scale write and advance the generation, as a single
--   critical section.
--
--   The write must happen INSIDE the lock, not merely near it. An
--   autosave's conditional restore ('restoreIfPlayerIdle') takes the
--   same lock, so without this the world thread could read a matching
--   generation, the Lua thread could then apply its pause and bump, and
--   the world thread would still go on to overwrite the player's pause
--   with its own stale pre-save value — the exact "the player wins"
--   guarantee the generation exists to provide.
--
--   Only the Lua thread calls this, but the lock is what makes it
--   correct against the world thread, not against other Lua callers.
withPlayerIntent ∷ WorldSimCapability → IO α → IO α
withPlayerIntent wsc act =
  modifyMVar (wsPlayerIntentGenRef wsc) $ \g → do
    result ← act
    pure (g + 1, result)

-- | Run @act@ with the current generation, holding the lock but NOT
--   advancing it — the shape a save's own acceptance needs: it reads the
--   state it is about to replace and replaces it in the same breath, and
--   its own writes are engine-imposed, not player intent.
withPlayerIntentHeld ∷ WorldSimCapability → (Word64 → IO α) → IO α
withPlayerIntentHeld wsc = withMVar (wsPlayerIntentGenRef wsc)

-- | Run @act@ only if NO player-intent transition has been recorded
--   since @expected@, holding the same lock those transitions take —
--   so one can neither slip in unseen after the comparison nor be lost
--   to a write that follows it.
--
--   'Nothing' means the player won and @act@ never ran; 'Just' carries
--   whatever @act@ decided. The result is deliberately the ACTION's
--   rather than a bare \"it ran\" flag: #1730 gave the autosave restore
--   a SECOND reason to decline (an independent engine pause source
--   asserted one during the window, 'wsEnginePauseGenRef'), and that
--   comparison has to happen inside this same critical section — a
--   caller that read the counter outside it could be overtaken by the
--   very pause it is checking for. Keeping the two reasons distinct in
--   the result is what lets the caller report the right one.
restoreIfPlayerIdle ∷ WorldSimCapability → Word64 → IO α → IO (Maybe α)
restoreIfPlayerIdle wsc expected act =
  modifyMVar (wsPlayerIntentGenRef wsc) $ \g →
    if g ≢ expected
      then pure (g, Nothing)
      else (\r → (g, Just r)) ⊚ act
