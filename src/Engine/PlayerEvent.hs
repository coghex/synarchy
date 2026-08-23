{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.PlayerEvent
    ( PlayerEvent(..)
    , CategoryCfg(..)
    , NotificationCfg
    , eventStoreCap
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM

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
