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
    , readEventLogProgress
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
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Control.Concurrent.STM (STM, atomically, readTVarIO)
import Control.Concurrent.STM.TVar (TVar, modifyTVar')
import qualified Engine.Core.Queue as Q
import Engine.Core.Log (logWarn, LogCategory(..))
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import Engine.PlayerEvent
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Page.Types (WorldPageId(..))
import World.Pause (imposePause)

-- | Emit a player-visible event. Honors the player's per-category
--   notification settings: appends to the log ring, sends a popup,
--   and/or sets 'enginePausedRef' to True. Those three switches are
--   independent — @ccPopup@ delivers a popup whether or not @ccLog@
--   also stores the event.
--
--   If @category@ is not in the registry, the event is dropped and a
--   dev-log warning is written under 'CatEvent'. This is the
--   loud-fail path for typos — better than silently swallowing.
--
--   Thread-safe as a PRIMITIVE: 'ecEventStoreRef' is an STM TVar,
--   the Lua queue is internally
--   STM-backed, and the pause is one 'World.Pause.imposePause' call,
--   whose flag write is a single atomic read-modify-write, so
--   concurrent callers on any thread are safe. That is a property
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
--
--   When it carries BOTH, the page is taken from the NAMED UNIT rather
--   than snapshotted from the active one (#1666). An event about a unit
--   states that unit's position, and a unit's coordinates are in the
--   frame of the page it stands on ('uiPage') — which need not be the
--   active page, since a unit-attributed emitter can run for an
--   off-page unit. Attributing those coordinates to whichever world the
--   player happens to be looking at is the same wrong-page match the
--   ground verbs refuse to make, and #1588's popup would then offer to
--   pan the WRONG world to them.
--
--   This is 'resolveEventPage' case 1 — an explicit page from the
--   emitter — reached through the same single 'emitEventFullOnPage'
--   entry point, not a second emit path or a fourth precedence rule.
--   Everything else is unchanged: a coords-free unit event still names
--   no page (attributing one would invent a frame it does not have),
--   and a unit that is no longer in the manager falls through to case
--   2's active-page snapshot, exactly as before.
emitEventFull ∷ EngineEnv
              → Text                  -- ^ category id
              → Text                  -- ^ source tag (dev debug)
              → Text                  -- ^ player-visible text
              → Maybe (Int, Int)      -- ^ optional grid coords
              → Maybe Word32          -- ^ optional unit this is about
              → IO ()
emitEventFull env category source eventText mCoords mUid = do
    mPage ← case (mCoords, mUid) of
        (Just _, Just uid) → unitEventPage env (UnitId uid)
        _                  → pure Nothing
    emitEventFullOnPage env category source eventText mCoords mUid mPage

-- | The page a live unit stands on, or 'Nothing' when it is not in the
--   manager (already dead by the time its event fires).
--
--   Reads @uiPage@ directly rather than through
--   'Engine.Scripting.Lua.API.Units.Page.unitOwningWorldState': that
--   resolver answers "which live WorldState may I mutate?", while this
--   one answers "which frame are these coordinates in?" — a question
--   the recorded id answers honestly whether or not the page is still
--   loaded, and a page name is all 'peSourcePage' stores.
unitEventPage ∷ EngineEnv → UnitId → IO (Maybe Text)
unitEventPage env uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure $ unWorldPageId ∘ uiPage <$> HM.lookup uid (umInstances um)

-- | Like 'emitEventFull', but the caller may also name the WORLD PAGE
--   the event concerns ('peSourcePage', #780) — for an emitter whose
--   event can fire on a page other than whichever is currently active/
--   visible (location discovery ticks every loaded page, including
--   hidden ones, so its discovering unit/location may not be on the
--   page the player is looking at).
--
--   This is where every emit path converges, so it is where the one
--   page-attribution rule lives ('resolveEventPage', #1588): an
--   explicit @mSourcePage@ wins — including the one 'emitEventFull'
--   derives from a named unit (#1666) — an otherwise-unattributed
--   coords-carrying emit snapshots the active page, and anything else
--   stays unattributed. Passing 'Just' coords together with a hidden
--   page is no longer a hazard the caller has to avoid — the resulting
--   coordinate names that hidden page, and the popup refuses to pan
--   until it is the active one — so location discovery's own
--   coords-when-active/coords-free-when-hidden split is now its own
--   editorial choice about what to show, not a safety requirement.
--
--   The ONE effective page is used everywhere the event goes: the log
--   ring ('peSourcePage') and the 'LuaShowPopup' broadcast — so
--   immediate delivery and event-log replay cannot drop different
--   metadata.
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
            -- stored event and the Lua broadcast can never disagree
            -- about which page the coordinates belong to (#1588) —
            -- not even if the active page changed between two reads.
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
            -- Independent of 'ccLog' above: the message IS the popup,
            -- and #2285 removed the write-only TVar that used to be
            -- appended beside it, so this is the only place a
            -- popup-enabled event goes when logging is off.
            when (ccPopup cfg) $ do
                let (r, g, b, a) = ccTextColor cfg
                Q.writeQueue (ivLuaQueue (toInputViewCapability env))
                    (LuaShowPopup category eventText r g b a mCoords
                                  effectivePage)
            -- #1599: the pause and the paused page's clock are ONE
            -- pair, and 'World.Pause' is what maintains it. A bare
            -- flag write here left the page's chosen speed nowhere the
            -- resume path could find it, so the player's own Space
            -- resume dropped them back to 1x.
            when (ccPause cfg) $ imposePause worldSim

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
--   1. An explicit page from the emitter wins. Two emitters pass one,
--      and each genuinely knows better than the active-page snapshot
--      does: #780's location discovery, which ticks every loaded page,
--      hidden ones included; and 'emitEventFull' whenever an event
--      names BOTH a unit and coordinates (#1666), because those
--      coordinates are in that unit's own page's frame.
--   2. Otherwise, an event carrying COORDINATES is attributed to the
--      canonically resolved active page
--      ('Engine.Core.State.resolveActiveWorld', reached through
--      'activeWorldPageFrom' so this module keeps its narrowed
--      'WorldSimCapability' view). This is the automatic half: every
--      @emitEventAt@ caller — Lua and Haskell alike — gets a
--      self-describing coordinate with no code change, and so does a
--      unit-attributed one whose unit is already gone.
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
--
--   Every COMMITTED mutation — a plain append and a coalesced
--   replacement alike — takes the store's next sequence, stamped here
--   inside the very 'STM' transaction that writes the row (#1714).
--   That is the whole point of holding rows and counter in one 'TVar':
--   two concurrent emitters cannot be handed the same number, and a
--   number can never name a mutation that did not commit. Rows the
--   transaction leaves alone — the untouched prefix, and everything the
--   coalesce hit did not move — keep the sequences they already had.
--
--   An eviction is NOT a mutation and consumes no sequence: dropping
--   the overflowing prefix only removes rows, so the sequences it takes
--   with it stay permanently absent, which is exactly the evidence an
--   observer needs to notice the loss instead of reading it as "nothing
--   happened".
pushBounded ∷ Double → TVar EventStore → PlayerEvent → STM ()
pushBounded window ref ev = modifyTVar' ref $ \st →
    let s   = esRows st
        n   = esNextSequence st
        row = StoredEvent { seSequence = n, seEvent = ev }
        coalesced = case findCoalescedIndex s of
            Just i  →
                let old = seEvent (Seq.index s i)
                in Seq.deleteAt i s
                     |> row { seEvent = ev { peCount = peCount old + 1 } }
            Nothing → s |> row
        excess = Seq.length coalesced - eventStoreCap
    in st { esRows = if excess > 0 then Seq.drop excess coalesced
                                   else coalesced
          , esNextSequence = n + 1 }
  where
    findCoalescedIndex s
        | window <= 0 = Nothing
        | otherwise   = Seq.findIndexR (sameEntryWithin ev ∘ seEvent) s

    sameEntryWithin a b =
        let dt = peGameTime a - peGameTime b
        in sameEntry a b ∧ dt >= 0 ∧ dt <= window

    -- The coalescing key is spelled out over 'PlayerEvent' fields
    -- rather than derived, so #1714's sequence metadata — which lives
    -- on the 'StoredEvent' wrapper, not on the event — is structurally
    -- unable to join it.
    sameEntry a b = peCategory a ≡ peCategory b
                  ∧ peText a ≡ peText b
                  ∧ peUid a ≡ peUid b
                  ∧ peSourcePage a ≡ peSourcePage b

-- | Snapshot of the event log. Returns rows oldest-first, each carrying
--   the store's mutation sequence ('seSequence', #1714) alongside the
--   event; the Lua side reverses if it wants newest-on-top.
--
--   There is deliberately no sequence-free variant: a reader that threw
--   the sequence away would be back to identifying rows by value, which
--   is the ambiguity #1714 removed.
readEventLog ∷ EngineEnv → IO [StoredEvent]
readEventLog env =
    toList ∘ esRows <$> readTVarIO (ecEventStoreRef (toEventsCapability env))

-- | The rows AND the highest sequence the store has committed, from
--   ONE read of the store (#1714).
--
--   The high-water mark is not derivable from the rows: after a load
--   publish the ring is empty while the counter has kept counting, so
--   this is the only way an observer can tell \"nothing has happened\"
--   from \"everything that happened was discarded\".
--
--   The single 'readTVarIO' is the contract, not an optimisation.
--   Reading rows and counter separately lets an emitter commit BETWEEN
--   them, and the resulting pair is a lie in the one direction that
--   matters: the mark names a mutation the rows do not show, so an
--   observer reports the still-retained row as lost, advances its
--   cursor past it, and then suppresses it on every later read — the
--   row is gone from the trace permanently. Callers that need both must
--   come through here rather than pairing 'readEventLog' with a second
--   read.
readEventLogProgress ∷ EngineEnv → IO ([StoredEvent], Int)
readEventLogProgress env = do
    st ← readTVarIO (ecEventStoreRef (toEventsCapability env))
    pure (toList (esRows st), eventStoreHighWater st)
