{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
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
                                  --   coords clickable (click pans the
                                  --   camera there); 'Nothing' for
                                  --   events without a natural location
                                  --   (e.g. save success), whose lines
                                  --   are non-clickable.
    , peUid      ∷ !(Maybe Word32)
                                  -- ^ Optional unit this event is ABOUT
                                  --   (set via 'engine.emitEventForUnit').
                                  --   Lets the per-unit log panel filter
                                  --   event-log entries to one unit.
                                  --   'Nothing' for world/global events.
    , peSourcePage ∷ !(Maybe Text)
                                  -- ^ Optional world page (raw
                                  --   'World.Page.Types.WorldPageId'
                                  --   text) this event concerns — set
                                  --   by emitters whose event can fire
                                  --   on a page other than whichever is
                                  --   currently active/visible (e.g.
                                  --   location discovery, #780, which
                                  --   ticks every loaded page, including
                                  --   hidden ones). Every other emitter
                                  --   leaves this 'Nothing'. A caller
                                  --   that also sets 'peCoords' MUST
                                  --   only do so when this page is the
                                  --   one currently active — a popup
                                  --   click pans the camera on the
                                  --   ACTIVE page, so coords for a
                                  --   hidden-page event would silently
                                  --   pan to the wrong place; omit
                                  --   'peCoords' instead for those.
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
