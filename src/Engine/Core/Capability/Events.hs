-- | The event\/notification\/popup half of the @ui-hud-events@
--   capability (epic #537, issue #898 — E7b): exactly the four fields
--   'docs/engineenv_capability_inventory.md' SS7.7 splits off from the
--   UI\/focus\/HUD half (#897, "Engine.Core.Capability.Ui"), in SS5's
--   own table order.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here).
--
--   == Field prefix
--
--   The convention prefixes fields with the record's own initials,
--   appending a @c@ for a single-word name (@cc@\/@rc@\/@ic@ for
--   @Core@\/@Render@\/@Input@), giving __@ec@__ here — free of every
--   other landed record's prefix (@cc@, @cr@, @ic@, @iv@, @rc@, @rv@,
--   @uc@, @uic@, @ws@).
--
--   == No thread-private field, so no split record
--
--   Unlike @render-gpu-asset@ (SS3.1) and @input-lua-transport@
--   (SS7.3), this capability owns nothing one thread privately owns.
--   'ecEventStoreRef' and 'ecPopupQueueRef' are multi-writer STM
--   TVars, 'ecNotificationCfgRef' is read on ANY thread from the
--   @emitEvent@ path, and 'ecNotificationOrder' is an immutable boot
--   value. So there is one record here, not a main-only\/worker-safe
--   pair, and @tools/engine_env_capability_audit.py@ needs no import
--   boundary for it beyond the SS6 ratchet.
--
--   #1714's event-log sequence counter is progress state, but it is
--   NOT a fifth field: it rides inside 'ecEventStoreRef'\'s own value,
--   which is what keeps sequence assignment and the store mutation one
--   atomic STM write.
--
--   == Concurrency contract these handles carry (SS5)
--
--   The two 'TVar's are genuinely multi-writer and are only ever
--   touched inside @atomically@; 'ecNotificationCfgRef' takes a single
--   'Data.IORef.readIORef' per emit (negligible even from the world
--   thread) and is updated with @atomicModifyIORef'@ by the settings
--   tab. Projecting this record changes none of that: it hands out the
--   same containers, so the atomicity discipline lives at the call
--   sites exactly as before.
--
--   == Lifecycle (SS5, and @World.Load.Publish.resetTransientState@)
--
--   'ecEventStoreRef' and 'ecPopupQueueRef' are @session-replaced@ and
--   ARE emptied of rows by a load publish
--   (@World.Load.Publish.resetTransientState@) — a loaded session
--   starts with no event history and no pending popups. 'ecEventStoreRef'
--   is emptied at the OTHER session boundary too, when Exit to Menu
--   destroys every world (@Unit.Thread.endSessionEpoch@, #2291); until
--   that issue only the load half existed, so the previous session's
--   rows stayed renderable — and clickable — in the next game. The event
--   store's sequence counter deliberately survives BOTH resets (#1714),
--   so a row emitted after either still outranks any cursor held from
--   before it. 'ecNotificationCfgRef' and 'ecNotificationOrder' are
--   @boot-process@: both come from the boot-time notification registry
--   merge and survive a load untouched (the player's per-category
--   preferences are a setting, not session state).
--
--   Like the other capability modules, this one imports only the
--   narrow slice of @Engine.Core.State@ it needs (the bare 'EngineEnv'
--   type plus its four field accessors) rather than @EngineEnv(..)@ or
--   a bare import, so it is not itself a full-@EngineEnv@-access
--   consumer under the SS6 ratchet.
module Engine.Core.Capability.Events
  ( EventsCapability(..)
  , toEventsCapability
  ) where

import UPrelude
import Data.IORef (IORef)
import Data.Sequence (Seq)
import Control.Concurrent.STM.TVar (TVar)
import Engine.PlayerEvent (PlayerEvent, EventStore, NotificationCfg)
import Engine.Core.State
  ( EngineEnv
  , eventStoreRef, notificationCfgRef, notificationOrder, popupQueueRef
  )

-- | The event\/notification\/popup slice of @ui-hud-events@: the
--   player-event ring buffer, the resolved per-category notification
--   settings, the boot-captured category display order, and the
--   popup queue. See 'docs/engineenv_capability_inventory.md' SS5
--   @ui-hud-events@ and SS7.7.
data EventsCapability = EventsCapability
  { -- | Ring buffer of player-facing events, capped at
    --   'Engine.PlayerEvent.eventStoreCap' (~1000 entries; oldest
    --   dropped), carried together with the mutation-sequence counter
    --   in one 'Engine.PlayerEvent.EventStore' (#1714). Multi-writer
    --   STM: @WorldThread@ and @LuaThread@ push through
    --   'Engine.PlayerEvent.Emit'; @LuaThread@ reads it for
    --   @engine.getEventLog()@. @session-replaced@ — a load publish and
    --   an Exit to Menu (#2291) each clear its ROWS
    --   ('Engine.PlayerEvent.clearEventStoreRows') while the counter
    --   keeps counting, so no sequence is reissued within one engine
    --   process. Never serialized to a save.
    ecEventStoreRef      ∷ TVar EventStore
    -- | Resolved notification settings keyed by category id, merged
    --   at boot from @data/notification_categories.yaml@ +
    --   @config/notifications.local.yaml@ (#786). Read on
    --   @AnyThread@ (the @emitEvent@ gating path takes one
    --   'Data.IORef.readIORef' per call); written on @LuaThread@ by
    --   the settings tab's per-category toggles. @boot-process@ — a
    --   load publish leaves it alone.
  , ecNotificationCfgRef ∷ IORef NotificationCfg
    -- | Registry-order category ids captured once at boot, so the
    --   settings tab renders rows in YAML order rather than HashMap
    --   iteration order. A plain immutable value, not a ref:
    --   categories cannot be added or removed at runtime, only their
    --   flags toggled. @boot-process@.
  , ecNotificationOrder  ∷ [Text]
    -- | Popup-enabled events, appended at the same emit call site
    --   that sends the live @LuaShowPopup@ message. __Write-only
    --   today__: nothing reads this TVar back out — popup DELIVERY is
    --   the separate @luaQueue@ message, not a drain of this queue.
    --   It exists for inspection\/debug querying and as a Phase 2
    --   stable source for the notifications panel.
    --   @session-replaced@ — a load publish resets it to empty.
  , ecPopupQueueRef      ∷ TVar (Seq PlayerEvent)
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toEventsCapability ∷ EngineEnv → EventsCapability
toEventsCapability env = EventsCapability
  { ecEventStoreRef      = eventStoreRef env
  , ecNotificationCfgRef = notificationCfgRef env
  , ecNotificationOrder  = notificationOrder env
  , ecPopupQueueRef      = popupQueueRef env
  }
