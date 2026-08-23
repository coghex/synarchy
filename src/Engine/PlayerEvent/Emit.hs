{-# LANGUAGE Strict #-}
module Engine.PlayerEvent.Emit
    ( -- * Re-exports of the data types
      module Engine.PlayerEvent
      -- * Emission and read APIs
    , emitEvent
    , emitEventAt
    , emitEventFull
    , emitEventFullOnPage
    , readEventLog
      -- * The one page-attribution rule
    , resolveEventPage
    ) where

import UPrelude
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.Events
    (EventsCapability(..), toEventsCapability)
import Engine.Core.Capability.InputView
    (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent.STM (STM, atomically, readTVarIO)
import Control.Concurrent.STM.TVar (TVar, modifyTVar')
import qualified Engine.Core.Queue as Q
import Engine.Core.Log (logWarn, LogCategory(..))
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import Engine.PlayerEvent
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Page.Types (WorldPageId(..))

-- | Emit a player-visible event. Honors the player's per-category
--   notification settings: appends to the log ring, queues a popup,
--   and/or sets 'enginePausedRef' to True.
--
--   If @category@ is not in the registry, the event is dropped and a
--   dev-log warning is written under 'CatEvent'. This is the
--   loud-fail path for typos — better than silently swallowing.
--
--   Thread-safe as a PRIMITIVE: 'ecEventStoreRef' and
--   'ecPopupQueueRef' are STM TVars, the Lua queue is internally
--   STM-backed, and the pause flag is a single atomic 'writeIORef',
--   so concurrent callers on any thread are safe. That is a property
--   of the primitive, not a claim about who calls it: the only call
--   sites that exist today are on the @WorldThread@
--   ("World.Thread.Discovery", @World.Thread.Command.Save.WriteWorld@)
--   and the @LuaThread@ ("Engine.Scripting.Lua.API.PlayerEvent",
--   @Engine.Scripting.Lua.API.Save@) — no unit- or combat-thread
--   emitter exists.
emitEvent ∷ EngineEnv
          → Text     -- ^ category id (e.g. "save_load")
          → Text     -- ^ source tag for dev debug (e.g. "World.Save")
          → Text     -- ^ player-visible text
          → IO ()
emitEvent env category source eventText =
    emitEventAt env category source eventText Nothing

-- | Like 'emitEvent', but with optional grid coordinates. The
--   coordinates are routed to the Lua popup, which makes that popup
--   line clickable — clicking pans the camera to @(gx, gy)@ when the
--   event's page is still the active one, and reports the location as
--   unavailable when it is not (#1588). Events emitted without coords
--   produce non-clickable lines.
--
--   The caller does NOT have to know or pass its page: the coordinates
--   are attributed to the active page automatically (see
--   'resolveEventPage'), so a coordinate emitted here can never be
--   replayed against a different world's grid.
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

-- | Like 'emitEventFull', but the caller may also name the WORLD PAGE
--   the event concerns ('peSourcePage', #780) — for an emitter whose
--   event can fire on a page other than whichever is currently active/
--   visible (location discovery ticks every loaded page, including
--   hidden ones, so its discovering unit/location may not be on the
--   page the player is looking at).
--
--   This is where every emit path converges, so it is where the one
--   page-attribution rule lives ('resolveEventPage', #1588): an
--   explicit @mSourcePage@ wins, an otherwise-unattributed
--   coords-carrying emit snapshots the active page, and anything else
--   stays unattributed. Passing 'Just' coords together with a hidden
--   page is no longer a hazard the caller has to avoid — the resulting
--   coordinate names that hidden page, and the popup refuses to pan
--   until it is the active one — so location discovery's own
--   coords-when-active/coords-free-when-hidden split is now its own
--   editorial choice about what to show, not a safety requirement.
--
--   The ONE effective page is used everywhere the event goes: the log
--   ring ('peSourcePage'), the popup queue, and the 'LuaShowPopup'
--   broadcast — so immediate delivery and event-log replay cannot drop
--   different metadata.
emitEventFullOnPage ∷ EngineEnv
                    → Text                  -- ^ category id
                    → Text                  -- ^ source tag (dev debug)
                    → Text                  -- ^ player-visible text
                    → Maybe (Int, Int)      -- ^ optional grid coords
                    → Maybe Word32          -- ^ optional unit this is about
                    → Maybe Text            -- ^ optional source world page
                    → IO ()
emitEventFullOnPage env category source eventText mCoords mUid mSourcePage = do
    let events   = toEventsCapability env
        worldSim = toWorldSimCapability env
    cfgMap ← readIORef (ecNotificationCfgRef events)
    case HM.lookup category cfgMap of
        Nothing → do
            logger ← readIORef (ccLoggerRef (toCoreCapability env))
            logWarn logger CatEvent $
                "emitEvent: unknown category '" <> category
                  <> "' from " <> source <> "; event dropped: "
                  <> eventText
        Just cfg → do
            now ← readIORef (wsGameTimeRef worldSim)
            -- Resolved ONCE, before either surface is written, so the
            -- stored event, the popup queue entry and the Lua broadcast
            -- can never disagree about which page the coordinates
            -- belong to (#1588) — not even if the active page changed
            -- between two reads.
            effectivePage ← resolveEventPage worldSim mCoords mSourcePage
            let ev = PlayerEvent
                    { peCategory = category
                    , peText     = eventText
                    , peGameTime = now
                    , peSource   = source
                    , peCoords   = mCoords
                    , peUid      = mUid
                    , peSourcePage = effectivePage
                    , peCount    = 1
                    }
            when (ccLog cfg) $
                atomically $
                    pushBounded (ccLogCoalesceWindow cfg)
                        (ecEventStoreRef events) ev
            when (ccPopup cfg) $ do
                atomically $ modifyTVar' (ecPopupQueueRef events) (|> ev)
                let (r, g, b, a) = ccTextColor cfg
                Q.writeQueue (ivLuaQueue (toInputViewCapability env))
                    (LuaShowPopup category eventText r g b a mCoords
                                  effectivePage)
            when (ccPause cfg) $
                writeIORef (wsEnginePausedRef worldSim) True

-- | The ONE page-attribution rule behind 'peSourcePage' (#1588).
--
--   Split out and exported so the rule has a NAME the three call-site
--   haddocks above can point at instead of restating it three times and
--   drifting; 'emitEventFullOnPage' is its only caller, and adding a
--   second one would mean a second emit path, which is exactly what
--   requirement 8 forbids.
--
--   In precedence order:
--
--   1. An explicit page from the emitter wins. Only #780's location
--      discovery passes one, and it is the emitter that genuinely knows
--      better than the active-page snapshot does: it ticks every loaded
--      page, hidden ones included.
--   2. Otherwise, an event carrying COORDINATES is attributed to the
--      canonically resolved active page
--      ('Engine.Core.State.resolveActiveWorld', reached through
--      'activeWorldPageFrom' so this module keeps its narrowed
--      'WorldSimCapability' view). This is the automatic half: every
--      @emitEventAt@ \/ @emitEventForUnit@ caller — Lua and Haskell
--      alike — gets a self-describing coordinate with no code change.
--   3. Otherwise 'Nothing'. A coords-free event has no coordinate frame
--      to name, and a coords-carrying one emitted with no world
--      registered (the main menu) genuinely has no page: attributing it
--      to something would be inventing the very fact this exists to
--      carry.
--
--   Case 2 is deliberately gated on the coords rather than run
--   unconditionally: an event with no location must stay
--   non-clickable, and handing it a page would only invite a consumer
--   to treat it as pannable.
resolveEventPage ∷ WorldSimCapability
                 → Maybe (Int, Int)   -- ^ the emit's coords, if any
                 → Maybe Text         -- ^ the emitter's explicit page, if any
                 → IO (Maybe Text)
resolveEventPage _   _        (Just pg) = pure (Just pg)
resolveEventPage _   Nothing  Nothing   = pure Nothing
resolveEventPage wsc (Just _) Nothing   =
    fmap (unWorldPageId ∘ fst) <$> activeWorldPageFrom (wsWorldManagerRef wsc)

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

    sameEntry a b = peCategory a ≡ peCategory b
                  ∧ peText a ≡ peText b
                  ∧ peUid a ≡ peUid b
                  ∧ peSourcePage a ≡ peSourcePage b

-- | Snapshot of the event log. Returns events oldest-first; the Lua
--   side reverses if it wants newest-on-top.
readEventLog ∷ EngineEnv → IO [PlayerEvent]
readEventLog env = toList <$> readTVarIO (ecEventStoreRef (toEventsCapability env))
