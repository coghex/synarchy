{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.PlayerEvent
    ( PlayerEvent(..)
    , StoredEvent(..)
    , EventStore(..)
    , emptyEventStore
    , clearEventStoreRows
    , eventStoreHighWater
    , CategoryCfg(..)
    , NotificationCfg
    , eventStoreCap
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | A player-visible event. Routed to up-to-three surfaces (log,
--   popup, pause) by 'Engine.PlayerEvent.emitEvent', gated on the
--   player's per-category preferences in 'NotificationCfg'.
data PlayerEvent = PlayerEvent
    { peCategory ∷ !Text          -- ^ Registry id, e.g. "save_load".
    , peText     ∷ !Text          -- ^ Player-visible message.
    , peGameTime ∷ !Double        -- ^ 'gameTimeRef' at emit time.
    , peSource   ∷ !Text          -- ^ Subsystem tag for dev debug, e.g.
                                  --   "World.Save". Not displayed in P1.
    , peCoords   ∷ !(Maybe (Int, Int))
                                  -- ^ Optional grid coordinates set
                                  --   by 'emitEventAt'. The popup
                                  --   module makes a line carrying
                                  --   coords clickable; 'Nothing' for
                                  --   events without a natural location
                                  --   (e.g. save success), whose lines
                                  --   are non-clickable.
                                  --
                                  --   A coordinate pair is meaningless
                                  --   without the page it indexes, so
                                  --   setting this ALWAYS also sets
                                  --   'peSourcePage' — see there for
                                  --   the attribution rule and for what
                                  --   a click actually does with the
                                  --   pair (#1588).
    , peUid      ∷ !(Maybe Word32)
                                  -- ^ Optional unit this event is ABOUT
                                  --   (set via 'engine.emitEventForUnit').
                                  --   Lets the per-unit log panel filter
                                  --   event-log entries to one unit.
                                  --   'Nothing' for world/global events.
    , peSourcePage ∷ !(Maybe Text)
                                  -- ^ The world page (raw
                                  --   'World.Page.Types.WorldPageId'
                                  --   text) this event concerns — and,
                                  --   whenever 'peCoords' is set, the
                                  --   page those coordinates are
                                  --   indexed in. The stored event
                                  --   outlives the emit instant and can
                                  --   be replayed from the event log
                                  --   long after the player has
                                  --   switched pages, so the coordinate
                                  --   frame travels WITH the coordinate
                                  --   rather than being re-guessed at
                                  --   click time (#1588).
                                  --
                                  --   Filled by
                                  --   'Engine.PlayerEvent.Emit.emitEventFullOnPage',
                                  --   which owns the one attribution
                                  --   rule: an explicit page from the
                                  --   emitter wins (#780's location
                                  --   discovery, which ticks every
                                  --   loaded page and so knows a page
                                  --   the active-page snapshot would get
                                  --   wrong); otherwise a
                                  --   coords-carrying emit snapshots the
                                  --   canonically resolved ACTIVE page
                                  --   ('Engine.Core.State.resolveActiveWorld');
                                  --   otherwise this stays 'Nothing'.
                                  --   No emitter has to opt in.
                                  --
                                  --   'Nothing' therefore means "no page
                                  --   is known" — a coords-free event, or
                                  --   a coords-carrying one emitted with
                                  --   no world registered at all (the
                                  --   main menu). Consumers must treat
                                  --   that as NOT-the-active-page rather
                                  --   than defaulting to it: the popup's
                                  --   coordinate line refuses to pan
                                  --   unless this page is the active one,
                                  --   so a wrong-world pan is
                                  --   unrepresentable instead of merely
                                  --   discouraged.
    , peCount    ∷ !Int
                                  -- ^ How many identical emits (same
                                  --   category + text + uid) have
                                  --   coalesced into this entry. Starts
                                  --   at 1; the log shows "msg (xN)" for
                                  --   N>1. 'peGameTime' tracks the MOST
                                  --   RECENT of the coalesced emits, so a
                                  --   repeating failure (a stuck unit)
                                  --   stays one log line that bumps its
                                  --   count + timestamp instead of
                                  --   flooding the log.
    } deriving (Show, Eq, Generic)
-- No Serialize derivation: events are per-session and never saved.

-- | One row of the log ring: a 'PlayerEvent' plus the store's own
--   mutation sequence (#1714).
--
--   The sequence is metadata ABOUT the mutation that produced this row,
--   not part of the event, so it lives in this wrapper rather than on
--   'PlayerEvent'. That placement is the contract, not a style
--   preference: the coalescing key
--   ('Engine.PlayerEvent.Emit.pushBounded'\'s @sameEntry@) compares
--   'PlayerEvent' fields and the @LuaShowPopup@ broadcast carries the
--   same bare fields, and neither can grow a sequence dependency by
--   accident when the sequence is not a field they can see.
data StoredEvent = StoredEvent
    { seSequence ∷ !Int
      -- ^ The store's mutation sequence for THIS row. Positive,
      --   assigned consecutively from 1 in commit order, and stamped
      --   inside the same STM transaction that writes the row, so a
      --   multi-writer race cannot interleave two rows onto one number
      --   or leave a number unaccounted for. A coalesced replacement is
      --   a fresh mutation and takes a fresh sequence; an untouched row
      --   keeps the one it already had.
    , seEvent    ∷ !PlayerEvent
      -- ^ The player-visible event, byte-for-byte what it always was.
    } deriving (Show, Eq, Generic)

-- | The whole log-ring container: the bounded rows plus the counter
--   that names the next mutation (#1714).
--
--   Rows and counter share ONE 'Control.Concurrent.STM.TVar.TVar' so
--   that assignment and the store mutation are the same atomic write,
--   and so that the counter survives operations that discard rows.
--   Rows are discarded at each of the two session boundaries, both
--   through 'clearEventStoreRows':
--   'World.Load.Publish.resetTransientState' on a load publish, and
--   @Unit.Thread.endSessionEpoch@ when Exit to Menu destroys every world
--   (#2291). The counter deliberately keeps counting across BOTH, so a
--   row emitted after either is still NEWER than any cursor an observer
--   retained from before it, and no sequence is ever handed out twice in
--   one engine process.
data EventStore = EventStore
    { esRows         ∷ !(Seq StoredEvent)
      -- ^ Oldest-first, capped at 'eventStoreCap'; oldest dropped.
    , esNextSequence ∷ !Int
      -- ^ The sequence the next committed mutation will take. Starts
      --   at 1 and only ever increases.
    } deriving (Show, Eq, Generic)

-- | A fresh store: no rows, and the first mutation will be sequence 1.
emptyEventStore ∷ EventStore
emptyEventStore = EventStore { esRows = Seq.empty, esNextSequence = 1 }

-- | The highest sequence this store has ever COMMITTED, independent of
--   which rows it still holds (0 before the first mutation).
--
--   Read separately from the rows because the two can disagree, and
--   only in one direction: 'clearEventStoreRows' removes rows without
--   touching the counter, so after either session boundary that calls it
--   — a load publish, or an Exit to Menu — the ring can be empty while
--   mutations newer than an observer's cursor have genuinely been
--   committed. An observer that inferred the high-water mark from the
--   rows alone would see \"nothing here\" and report no loss at all —
--   permanently, if no later row happens to arrive.
eventStoreHighWater ∷ EventStore → Int
eventStoreHighWater st = esNextSequence st - 1

-- | Discard every row but KEEP the sequence counter. Two callers, one
--   per session boundary: 'World.Load.Publish.resetTransientState' on a
--   load publish, and @Unit.Thread.endSessionEpoch@ on Exit to Menu
--   (#2291) — a session the player left must not leave clickable rows
--   behind for the next one, whose pages can reuse its page ids.
--   Resetting the counter here would reissue sequences an observer had
--   already seen, which is exactly the silent-loss failure #1714 exists
--   to remove.
clearEventStoreRows ∷ EventStore → EventStore
clearEventStoreRows st = st { esRows = Seq.empty }

-- | One row of the notification registry, with the player's three
--   per-category switches resolved on top of the YAML defaults.
--   Immutable for the session in Phase 1.
data CategoryCfg = CategoryCfg
    { ccId          ∷ !Text
    , ccDisplayName ∷ !Text
    , ccDescription ∷ !Text
    , ccTextColor   ∷ !(Float, Float, Float, Float)  -- ^ RGBA 0–1
    , ccLog         ∷ !Bool                          -- ^ append to log ring
    , ccPopup       ∷ !Bool                          -- ^ queue popup
    , ccPause       ∷ !Bool                          -- ^ flip enginePausedRef
    , ccPopupCoalesceWindow ∷ !Double
      -- ^ When >0, repeated events of this category within this
      --   many wall-seconds collapse into the same popup line
      --   (count bumped, text replaced with the most recent
      --   event's text). Events outside the window start a new
      --   line in the same popup. 0 (default) disables coalescing —
      --   every event spawns a fresh popup. Read by the Lua popup
      --   module via @getNotificationCfg@.
    , ccLogCoalesceWindow ∷ !Double
      -- ^ When >0, repeated LOG entries of this category within this
      --   many GAME-seconds collapse into one event-log row with an
      --   incremented count. 0 (default) disables log coalescing so
      --   every emit keeps its own history entry.
    } deriving (Show, Eq, Generic)

-- | The notification registry as held in 'EngineEnv'. Keyed by
--   category id (see 'ccId'). Built at boot from
--   @data/notification_categories.yaml@ merged with
--   @config/notifications.local.yaml@ (#786).
type NotificationCfg = HM.HashMap Text CategoryCfg

-- | Maximum entries kept in the event-log ring buffer. Oldest entries
--   are dropped first when the buffer overflows.
eventStoreCap ∷ Int
eventStoreCap = 1000
