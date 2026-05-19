{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.PlayerEvent
    ( PlayerEvent(..)
    , PopupButton(..)
    , PopupAction(..)
    , CategoryCfg(..)
    , NotificationCfg
    , eventStoreCap
    , defaultPopupButtons
    , popupActionTag
    , parsePopupAction
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
    , peButtons  ∷ ![PopupButton] -- ^ Buttons from the resolved
                                  --   'CategoryCfg.ccButtons'. The
                                  --   popup module renders these and
                                  --   silently skips action buttons
                                  --   that need data this event
                                  --   doesn't have (e.g. 'ActGoTo'
                                  --   when 'peCoords' is 'Nothing').
    , peCoords   ∷ !(Maybe (Int, Int))
                                  -- ^ Optional grid coordinates set
                                  --   by 'emitEventAt'. Consumed by
                                  --   'ActGoTo' buttons; 'Nothing'
                                  --   for events without a natural
                                  --   location (e.g. save success).
    } deriving (Show, Eq, Generic)
-- No Serialize derivation: events are per-session and never saved.

-- | A single popup button. Forward-compat surface — Phase 1 only
--   ever uses 'defaultPopupButtons' and the Lua side hardcodes the
--   OK-dismiss behavior. Phase 2 wires real action routing.
data PopupButton = PopupButton
    { pbLabel  ∷ !Text
    , pbAction ∷ !PopupAction
    } deriving (Show, Eq, Generic)

data PopupAction
    = ActDismiss
    -- ^ Close the popup.
    | ActGoTo
    -- ^ Pan camera to the event's @(gx, gy)@ payload (set by
    --   'emitEventAt'). If the event has no payload, the Lua side
    --   skips this button entirely rather than rendering a dead one.
    -- P2+ extensions (not implemented):
    --   ActLuaCallback !Text   — invoke a registered Lua handler
    --   ActChain ![PopupAction]
    deriving (Show, Eq, Generic)

-- | Wire-format tag for a 'PopupAction', sent to the Lua popup
--   module via 'LuaShowPopup'. Keep in sync with @scripts/popup.lua@.
popupActionTag ∷ PopupAction → Text
popupActionTag ActDismiss = "dismiss"
popupActionTag ActGoTo    = "go_to"

-- | Inverse of 'popupActionTag'; returns 'Nothing' for unknown tags
--   so the YAML loader can warn instead of silently coercing.
parsePopupAction ∷ Text → Maybe PopupAction
parsePopupAction "dismiss" = Just ActDismiss
parsePopupAction "go_to"   = Just ActGoTo
parsePopupAction _         = Nothing

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
    , ccButtons     ∷ ![PopupButton]
      -- ^ Buttons rendered at the bottom of the popup. From the
      --   category's YAML @buttons:@ section, defaulting to
      --   'defaultPopupButtons' (one OK→Dismiss) if omitted. Action
      --   buttons that need event-specific data (e.g. @ActGoTo@) are
      --   silently skipped by the Lua popup module when the event
      --   has no payload.
    , ccCoalesceWindow ∷ !Double
      -- ^ When >0, repeated events of this category within this
      --   many wall-seconds collapse into the same popup line
      --   (count bumped, text replaced with the most recent
      --   event's text). Events outside the window start a new
      --   line in the same popup. 0 (default) disables coalescing —
      --   every event spawns a fresh popup. Read by the Lua popup
      --   module via @getNotificationCfg@.
    } deriving (Show, Eq, Generic)

-- | The notification registry as held in 'EngineEnv'. Keyed by
--   category id (see 'ccId'). Built at boot from
--   @data/notification_categories.yaml@ merged with
--   @config/notifications.yaml@.
type NotificationCfg = HM.HashMap Text CategoryCfg

-- | Maximum entries kept in the event-log ring buffer. Oldest entries
--   are dropped first when the buffer overflows.
eventStoreCap ∷ Int
eventStoreCap = 1000

-- | The default button set assigned to every event emitted in Phase 1.
defaultPopupButtons ∷ [PopupButton]
defaultPopupButtons = [PopupButton "OK" ActDismiss]
