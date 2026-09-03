{-# LANGUAGE UnicodeSyntax #-}
-- | The @save-load-coordination@ capability (epic #537, issue #899 —
--   E8, the epic's final child): the five coordination handles
--   'docs/engineenv_capability_inventory.md' §5's
--   @save-load-coordination@ table groups — the save barrier every
--   state-owner thread acknowledges, the load-status lifecycle, the
--   single-slot staged-load handoff, the monotonic last-save clamp,
--   and the global item-instance allocator.
--
--   __This is the coordination state, not the transaction.__ §7.8
--   deliberately leaves the save\/load transaction's own dominant
--   modules (@World.Thread.Command.Save@, @.WriteWorld@,
--   @World.Load.Stage@, @World.Load.Publish@,
--   @Engine.Scripting.Lua.API.Save@) as __permanent__ §6.1
--   whole-session orchestration exceptions: a save or load observes and
--   replaces every capability's state atomically, so narrowing them
--   would only reconstruct an env-shaped aggregate one level down. What
--   this record narrows is the much larger set of NON-permanent
--   touchpoints — the per-tick @ownerGated@ checks and
--   @acknowledgeCurrent@ acknowledgments the world\/input\/sim\/unit\/
--   combat loops make, and the allocator's any-thread bump.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here); this record's own
--   field prefix is @sl@.
--
--   One record, not a §3.1-style full\/view pair: none of these five
--   handles is private to a single thread the way @engineStateRef@ is
--   to @MainRender@. 'slSaveBarrierRef' is read AND written by every
--   state-owner role by design (§5 lists six), 'slLoadStatusRef' and
--   'slPendingLoadRef' by @WorldThread@\/@LuaThread@, and
--   'slNextItemInstanceIdRef' is explicitly @AnyThread@. The one field
--   with a single-role contract, 'slLastSaveTimeRef' (@LuaThread@
--   only), is a plain monotonic clamp with no privileged pointer behind
--   it, so documenting the restriction on the field is sufficient —
--   §3.1's rule is about a record handing a thread a way to reach state
--   another thread privately owns, and there is no such state here.
--
--   The save\/load contracts these handles carry are unchanged by this
--   projection and remain in force: the barrier's per-owner
--   acknowledgment roles ("Engine.Save.Barrier"), 'slPendingLoadRef'\'s
--   single-slot request-id keying, 'slLastSaveTimeRef'\'s
--   monotonic-ordering role (#98), and the allocator's GLOBAL
--   (not per-page) identity scope (#67).
--
--   Like the other capability modules, this one imports only the narrow
--   slice of @Engine.Core.State@ it needs (the bare 'EngineEnv' type
--   plus the five field accessors) rather than @EngineEnv(..)@ or a
--   bare import, so it is not itself a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.SaveLoad
  ( SaveLoadCapability(..)
  , toSaveLoadCapability
  ) where

import UPrelude
import Data.IORef (IORef)
import Data.Time.Clock (UTCTime)
import Engine.Load.Status (LoadStatusRef)
import Engine.Save.Barrier (SaveBarrier)
import World.Load.Types (StagedSession)
import Engine.Core.State
  ( EngineEnv
  , loadStatusRef, pendingLoadRef, saveBarrierRef, lastSaveTimeRef
  , nextItemInstanceIdRef
  )

-- | The @save-load-coordination@ capability: the save barrier, the
--   load-status lifecycle, the staged-load handoff slot, the last-save
--   monotonic clamp, and the item-instance allocator. See
--   'docs/engineenv_capability_inventory.md' §5
--   @save-load-coordination@ and §7.8.
data SaveLoadCapability = SaveLoadCapability
  { slLoadStatusRef         ∷ LoadStatusRef
    -- ^ Boot-process. The 12-phase load lifecycle
    --   (@engine.getLoadStatus()@). Opaque and internally synchronized
    --   — see "Engine.Load.Status". Read by @WorldThread@\/@MainRender@\/
    --   @LuaThread@, written by @WorldThread@\/@LuaThread@. Diagnostic
    --   only, never serialized; it is also what enforces that only ONE
    --   load is ever in flight.
  , slPendingLoadRef        ∷ IORef (Maybe (Int, StagedSession))
    -- ^ Transient-handoff. The single staged-load slot: written by
    --   @WorldThread@ when a staged-load transaction finishes staging,
    --   read and cleared when the matching publish command runs, and
    --   cleared by @LuaThread@ on a load-publish failure path before
    --   the prepared-but-never-applied Lua load is aborted. Keyed by
    --   request id defensively — the id is the whole reason a stale
    --   publish cannot apply a session the requester already abandoned.
  , slSaveBarrierRef        ∷ SaveBarrier
    -- ^ Boot-process. The save\/load quiesce barrier every state-owner
    --   thread acknowledges — @WorldThread@, @UnitThread@,
    --   @CombatThread@, @SimThread@, @InputThread@, @MainRender@ and
    --   @LuaThread@ each check 'Engine.Save.Barrier.ownerGated' in
    --   their own loop and answer with
    --   'Engine.Save.Barrier.acknowledgeCurrent' under their OWN
    --   'Engine.Save.Barrier.SaveOwner' tag. Opaque and internally
    --   synchronized; the per-owner acknowledgment roles are the
    --   barrier's contract, not this record's. Coordination only, never
    --   serialized.
  , slLastSaveTimeRef       ∷ IORef UTCTime
    -- ^ Boot-process. @LuaThread@-only. Each save is clamped strictly
    --   past this value so two saves in the same clock tick still order
    --   monotonically (#98).
  , slNextItemInstanceIdRef ∷ IORef Word64
    -- ^ Session-replaced. The GLOBAL item-instance id allocator — one
    --   identity space across every page, not a per-page counter — bumped
    --   atomically from any thread via
    --   'Engine.Core.State.freshItemInstanceId'. Persisted exactly and
    --   ASSIGNED from the save on load — never max'd against the
    --   replaced session, whose ids are no longer live (#67/#763). The
    --   no-collision guarantee comes from the SAVE side instead:
    --   'World.Save.Snapshot' rejects a snapshot holding an id at or
    --   above its own allocator.
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toSaveLoadCapability ∷ EngineEnv → SaveLoadCapability
toSaveLoadCapability env = SaveLoadCapability
  { slLoadStatusRef         = loadStatusRef env
  , slPendingLoadRef        = pendingLoadRef env
  , slSaveBarrierRef        = saveBarrierRef env
  , slLastSaveTimeRef       = lastSaveTimeRef env
  , slNextItemInstanceIdRef = nextItemInstanceIdRef env
  }
