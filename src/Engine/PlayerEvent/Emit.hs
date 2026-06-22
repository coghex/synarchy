{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.PlayerEvent.Emit
    ( -- * Re-exports of the data types
      module Engine.PlayerEvent
      -- * Emission and read APIs
    , emitEvent
    , emitEventAt
    , emitEventFull
    , readEventLog
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Word (Word32)
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent.STM (STM, atomically, readTVarIO)
import Control.Concurrent.STM.TVar (TVar, modifyTVar')
import qualified Engine.Core.Queue as Q
import Engine.Core.Log (logWarn, LogCategory(..))
import Engine.Core.State (EngineEnv(..))
import Engine.PlayerEvent
import Engine.Scripting.Lua.Types (LuaMsg(..))

-- | Emit a player-visible event. Honors the player's per-category
--   notification settings: appends to the log ring, queues a popup,
--   and/or sets 'enginePausedRef' to True.
--
--   If @category@ is not in the registry, the event is dropped and a
--   dev-log warning is written under 'CatEvent'. This is the
--   loud-fail path for typos — better than silently swallowing.
--
--   Thread-safe: 'eventStoreRef' and 'popupQueueRef' are STM TVars,
--   the Lua queue is internally STM-backed, and the pause flag is a
--   single atomic 'writeIORef'. Safe to call from world, unit, and
--   Lua threads concurrently.
emitEvent ∷ EngineEnv
          → Text     -- ^ category id (e.g. "save_load")
          → Text     -- ^ source tag for dev debug (e.g. "World.Save")
          → Text     -- ^ player-visible text
          → IO ()
emitEvent env category source eventText =
    emitEventAt env category source eventText Nothing

-- | Like 'emitEvent', but with optional grid coordinates. The
--   coordinates are routed to the Lua popup so 'ActGoTo' buttons can
--   pan the camera. Categories whose YAML 'buttons' include a
--   @go_to@ entry get the button rendered iff the caller passed
--   coords; categories with only @dismiss@ buttons ignore the
--   payload entirely.
emitEventAt ∷ EngineEnv
            → Text                  -- ^ category id
            → Text                  -- ^ source tag (dev debug)
            → Text                  -- ^ player-visible text
            → Maybe (Int, Int)      -- ^ optional grid coords
            → IO ()
emitEventAt env category source eventText mCoords =
    emitEventFull env category source eventText mCoords Nothing

-- | Like 'emitEventAt', but the event can also name the UNIT it's about
--   (set via @engine.emitEventForUnit@). The uid is carried on the
--   stored 'PlayerEvent' (peUid) so the per-unit log panel can filter
--   event-log entries to a single unit. Coords and uid are independent —
--   pass either, both, or neither.
emitEventFull ∷ EngineEnv
              → Text                  -- ^ category id
              → Text                  -- ^ source tag (dev debug)
              → Text                  -- ^ player-visible text
              → Maybe (Int, Int)      -- ^ optional grid coords
              → Maybe Word32          -- ^ optional unit this is about
              → IO ()
emitEventFull env category source eventText mCoords mUid = do
    cfgMap ← readIORef (notificationCfgRef env)
    case HM.lookup category cfgMap of
        Nothing → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatEvent $
                "emitEvent: unknown category '" <> category
                  <> "' from " <> source <> "; event dropped: "
                  <> eventText
        Just cfg → do
            now ← readIORef (gameTimeRef env)
            let ev = PlayerEvent
                    { peCategory = category
                    , peText     = eventText
                    , peGameTime = now
                    , peSource   = source
                    , peButtons  = ccButtons cfg
                    , peCoords   = mCoords
                    , peUid      = mUid
                    }
            when (ccLog cfg) $
                atomically $ pushBounded (eventStoreRef env) ev
            when (ccPopup cfg) $ do
                atomically $ modifyTVar' (popupQueueRef env) (|> ev)
                let (r, g, b, a) = ccTextColor cfg
                    buttonPairs  = map
                        (\b → (pbLabel b, popupActionTag (pbAction b)))
                        (ccButtons cfg)
                Q.writeQueue (luaQueue env)
                    (LuaShowPopup category eventText r g b a
                        buttonPairs mCoords)
            when (ccPause cfg) $
                writeIORef (enginePausedRef env) True

-- | Append an event to a bounded ring buffer. Oldest entries are
--   dropped when the buffer would exceed 'eventStoreCap'.
pushBounded ∷ TVar (Seq PlayerEvent) → PlayerEvent → STM ()
pushBounded ref ev = modifyTVar' ref $ \s →
    let s' = s |> ev
        excess = Seq.length s' - eventStoreCap
    in if excess > 0 then Seq.drop excess s' else s'

-- | Snapshot of the event log. Returns events oldest-first; the Lua
--   side reverses if it wants newest-on-top.
readEventLog ∷ EngineEnv → IO [PlayerEvent]
readEventLog env = toList <$> readTVarIO (eventStoreRef env)
