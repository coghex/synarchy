{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.PlayerEvent.Emit
    ( -- * Re-exports of the data types
      module Engine.PlayerEvent
      -- * Emission and read APIs
    , emitEvent
    , emitEventAt
    , emitEventFull
    , emitEventFullOnPage
    , readEventLog
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
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
--   coordinates are routed to the Lua popup, which makes that popup
--   line clickable — clicking pans the camera to @(gx, gy)@. Events
--   emitted without coords produce non-clickable lines.
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
emitEventFull env category source eventText mCoords mUid =
    emitEventFullOnPage env category source eventText mCoords mUid Nothing

-- | Like 'emitEventFull', but also tags the stored event with the WORLD
--   PAGE it concerns ('peSourcePage', #780) — for an emitter whose
--   event can fire on a page other than whichever is currently active/
--   visible (location discovery ticks every loaded page, including
--   hidden ones, so its discovering unit/location may not be on the
--   page the player is looking at). A caller passing 'Just' coords
--   here MUST only do so when @mSourcePage@ names the currently ACTIVE
--   page (or is 'Nothing') — the popup click-to-pan targets the active
--   page, so a hidden page's coords would silently pan to the wrong
--   place; pass 'Nothing' coords instead for those.
emitEventFullOnPage ∷ EngineEnv
                    → Text                  -- ^ category id
                    → Text                  -- ^ source tag (dev debug)
                    → Text                  -- ^ player-visible text
                    → Maybe (Int, Int)      -- ^ optional grid coords
                    → Maybe Word32          -- ^ optional unit this is about
                    → Maybe Text            -- ^ optional source world page
                    → IO ()
emitEventFullOnPage env category source eventText mCoords mUid mSourcePage = do
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
                    , peCoords   = mCoords
                    , peUid      = mUid
                    , peSourcePage = mSourcePage
                    , peCount    = 1
                    }
            when (ccLog cfg) $
                atomically $
                    pushBounded (ccLogCoalesceWindow cfg) (eventStoreRef env) ev
            when (ccPopup cfg) $ do
                atomically $ modifyTVar' (popupQueueRef env) (|> ev)
                let (r, g, b, a) = ccTextColor cfg
                Q.writeQueue (luaQueue env)
                    (LuaShowPopup category eventText r g b a mCoords)
            when (ccPause cfg) $
                writeIORef (enginePausedRef env) True

-- | Append an event to a bounded ring buffer. When @window > 0@, identical
--   repeats within that many GAME-seconds coalesce Dwarf-Fortress style:
--   bump the count, refresh the timestamp, and move the row to the tail.
--   Outside the window, or when @window <= 0@, every emit keeps its own
--   history row. Oldest entries are dropped when the buffer exceeds
--   'eventStoreCap'.
pushBounded ∷ Double → TVar (Seq PlayerEvent) → PlayerEvent → STM ()
pushBounded window ref ev = modifyTVar' ref $ \s →
    let coalesced = case findCoalescedIndex s of
            Just i  →
                let old = Seq.index s i
                in Seq.deleteAt i s |> ev { peCount = peCount old + 1 }
            Nothing → s |> ev
        excess = Seq.length coalesced - eventStoreCap
    in if excess > 0 then Seq.drop excess coalesced else coalesced
  where
    findCoalescedIndex s
        | window <= 0 = Nothing
        | otherwise   = Seq.findIndexR (sameEntryWithin ev) s

    sameEntryWithin a b =
        let dt = peGameTime a - peGameTime b
        in sameEntry a b ∧ dt >= 0 ∧ dt <= window

    sameEntry a b = peCategory a == peCategory b
                  ∧ peText a == peText b
                  ∧ peUid a == peUid b
                  ∧ peSourcePage a == peSourcePage b

-- | Snapshot of the event log. Returns events oldest-first; the Lua
--   side reverses if it wants newest-on-top.
readEventLog ∷ EngineEnv → IO [PlayerEvent]
readEventLog env = toList <$> readTVarIO (eventStoreRef env)
