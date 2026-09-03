{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.PlayerEvent
    ( emitEventFn
    , emitEventAtFn
    , emitEventForUnitFn
    , getEventLogFn
    , getEventLogProgressFn
    , getNotificationCfgFn
    , setNotificationOverridesFn
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import qualified HsLua as Lua
import Engine.Asset.YamlNotifications (writeNotificationOverrides)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.Events (EventsCapability(..), toEventsCapability)
import Engine.Core.Log (LogCategory(..), logWarn)
import Engine.Core.State (EngineEnv)
import Engine.PlayerEvent (CategoryCfg(..))
import Engine.PlayerEvent.Emit (PlayerEvent(..), StoredEvent(..)
                               , emitEvent, emitEventAt
                               , emitEventFull, readEventLog
                               , readEventLogProgress)

-- | @engine.emitEvent(category, text)@ — fire a player-visible event
--   from Lua. Returns nothing. Unknown categories drop with a dev
--   warning (see 'Engine.PlayerEvent.Emit.emitEvent').
emitEventFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
emitEventFn env = do
    catArg  ← Lua.tostring 1
    textArg ← Lua.tostring 2
    case (catArg, textArg) of
        (Just catBS, Just textBS) →
            Lua.liftIO $ emitEvent env
                (TE.decodeUtf8Lenient catBS)
                "Lua"
                (TE.decodeUtf8Lenient textBS)
        _ → return ()
    return 0

-- | @engine.emitEventAt(category, text, gx, gy)@ — fire a popup with
--   a location payload. The popup makes that event's line clickable —
--   clicking it pans the camera to @(gx, gy)@ if the event's page is
--   still the active one, and reports the location as unavailable if it
--   is not (#1588). Events emitted without coordinates produce
--   non-clickable lines.
--
--   Lua does not pass, and cannot pass, a page here: the coordinates
--   are attributed to the active page automatically inside
--   'Engine.PlayerEvent.Emit.resolveEventPage'.
emitEventAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
emitEventAtFn env = do
    catArg  ← Lua.tostring 1
    textArg ← Lua.tostring 2
    gxArg   ← Lua.tointeger 3
    gyArg   ← Lua.tointeger 4
    case (catArg, textArg, gxArg, gyArg) of
        (Just catBS, Just textBS, Just gx, Just gy) →
            Lua.liftIO $ emitEventAt env
                (TE.decodeUtf8Lenient catBS)
                "Lua"
                (TE.decodeUtf8Lenient textBS)
                (Just (fromIntegral gx, fromIntegral gy))
        _ → return ()
    return 0

-- | @engine.emitEventForUnit(category, text, uid [, gx, gy])@ — fire an
--   event tagged with the UNIT it concerns, so the per-unit log panel
--   can filter it. @gx@/@gy@ are optional; when they are given, the
--   event's page is the NAMED UNIT's own page rather than the active
--   one (#1666, 'emitEventFull'), because a unit event's coordinates
--   are in the frame of the page that unit stands on — which is not
--   always the page the player is looking at. Used by
--   unit-attributable emitters (survival warnings/criticals, unit
--   events) that already know the uid.
emitEventForUnitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
emitEventForUnitFn env = do
    catArg  ← Lua.tostring 1
    textArg ← Lua.tostring 2
    uidArg  ← Lua.tointeger 3
    gxArg   ← Lua.tointeger 4
    gyArg   ← Lua.tointeger 5
    case (catArg, textArg, uidArg) of
        (Just catBS, Just textBS, Just uid) → do
            let mCoords = case (gxArg, gyArg) of
                    (Just gx, Just gy) → Just (fromIntegral gx, fromIntegral gy)
                    _                  → Nothing
            Lua.liftIO $ emitEventFull env
                (TE.decodeUtf8Lenient catBS)
                "Lua"
                (TE.decodeUtf8Lenient textBS)
                mCoords
                (Just (fromIntegral uid))
        _ → return ()
    return 0

-- | @engine.getEventLog()@ — return the event-log ring buffer as a
--   Lua array of @{sequence, category, text, gameTime, source, uid,
--   count, coords, page}@ tables, oldest-first. @coords@ is either
--   @{x, y}@ or @nil@. @page@ is the source world-page id string, or
--   @nil@ when the event names no page at all.
--
--   @sequence@ (#1714) is the store's own mutation number for the row:
--   a Lua INTEGER, never a decimal string, positive, assigned
--   consecutively from 1 in commit order for the lifetime of one engine
--   process, and increasing across the returned array. It is present on
--   every row without exception, which is what lets an observer say
--   \"rows 11-14 were committed and are gone\" instead of guessing from
--   row values; the playtest oracle
--   (@tools\/playtest\/engine.py@) treats a row that lacks it as a
--   contract violation rather than falling back to value matching.
--   Purely additive: every field above keeps its previous value and
--   meaning, and the sequence takes no part in log coalescing (whose
--   key is spelled out over 'PlayerEvent' fields in
--   'Engine.PlayerEvent.Emit').
--
--   The two travel together (#1588): every coords-carrying event also
--   carries the page those coordinates are indexed in, so a row click
--   can decide whether the location is reachable from the world the
--   player is currently looking at instead of panning the active page
--   blindly. A page WITHOUT coords is still possible and still
--   meaningful — that is #780's location discovery on a hidden page,
--   which tags the event so it stays attributable while deliberately
--   showing no location.
--
--   Sufficient payload for the event-log panel to re-pop the popup from
--   a row click without a second engine round-trip; @page@ is what
--   makes that replay carry the same metadata a live popup does.
getEventLogFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getEventLogFn env = do
    rows ← Lua.liftIO $ readEventLog env
    pushEventRows rows
    return 1

-- | Push one 'StoredEvent' list as the Lua row array both event-log
--   verbs return.
--
--   Shared rather than duplicated: @getEventLog@ and
--   @getEventLogProgress@ must hand a consumer byte-identical rows, and
--   a second copy of this is exactly where a field would go missing
--   from one of them.
pushEventRows ∷ [StoredEvent] → Lua.LuaE Lua.Exception ()
pushEventRows rows = do
    Lua.newtable
    forM_ (zip [1..] rows) $ \(i, row) → do
        let ev = seEvent row
        Lua.newtable
        -- sequence: the store's mutation number for this row (#1714).
        -- Pushed FIRST so a row can never be built without it.
        Lua.pushinteger (fromIntegral (seSequence row))
        Lua.setfield (-2) "sequence"
        Lua.pushstring (TE.encodeUtf8 (peCategory ev))
        Lua.setfield (-2) "category"
        Lua.pushstring (TE.encodeUtf8 (peText ev))
        Lua.setfield (-2) "text"
        Lua.pushnumber (Lua.Number (peGameTime ev))
        Lua.setfield (-2) "gameTime"
        Lua.pushstring (TE.encodeUtf8 (peSource ev))
        Lua.setfield (-2) "source"
        -- uid: the unit this event is about (engine.emitEventForUnit), or
        -- nil. The per-unit log panel filters on this.
        case peUid ev of
            Just u  → Lua.pushinteger (fromIntegral u)
            Nothing → Lua.pushnil
        Lua.setfield (-2) "uid"
        -- Coalesced-repeat count (1 unless identical emits merged).
        Lua.pushinteger (fromIntegral (peCount ev))
        Lua.setfield (-2) "count"

        -- coords: either a {x, y} subtable or nil. nil means the
        -- event was emitted via emitEvent (no location) — repop
        -- leaves the line non-clickable just like the first spawn did.
        case peCoords ev of
            Just (gx, gy) → do
                Lua.newtable
                Lua.pushinteger (fromIntegral gx)
                Lua.setfield (-2) "x"
                Lua.pushinteger (fromIntegral gy)
                Lua.setfield (-2) "y"
                Lua.setfield (-2) "coords"
            Nothing → do
                Lua.pushnil
                Lua.setfield (-2) "coords"

        -- page: the world page this event's coords are indexed in
        -- (#1588), or the page a coords-free discovery event concerns
        -- (#780); nil when the event names no page.
        case peSourcePage ev of
            Just pg → Lua.pushstring (TE.encodeUtf8 pg)
            Nothing → Lua.pushnil
        Lua.setfield (-2) "page"

        Lua.rawseti (-2) i

-- | @engine.getEventLogProgress()@ — the event log and how far the
--   store has got, from ONE snapshot: @{rows = <getEventLog() array>,
--   highest = <integer>}@ (#1714).
--
--   @highest@ is the highest mutation sequence the store has COMMITTED
--   this process (@0@ before the first one), independent of which rows
--   survive. That independence is the point: a load publish empties the
--   ring without resetting the counter, so an observer reading rows
--   alone sees an empty log after a load and concludes nothing
--   happened, when in fact every mutation since its last read was
--   discarded.
--
--   __Why this is one verb and not two.__ Pairing @getEventLog()@ with
--   a separate high-water read lets an emitter commit BETWEEN them, and
--   the resulting pair lies in the one direction that matters: @highest@
--   names a mutation the rows do not show. An observer then reports the
--   still-retained row as lost, advances its cursor past it, and
--   suppresses it on every later read — the row never reaches the
--   trace. 'Engine.PlayerEvent.Emit.readEventLogProgress' takes both
--   from a single read of the store, so the pair is always internally
--   consistent: with rows present, @highest@ IS the last row's
--   @sequence@.
--
--   @rows@ is byte-identical to @engine.getEventLog()@'s array — the
--   same builder produces both — so a consumer needing only rows can
--   keep using the simpler verb.
getEventLogProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getEventLogProgressFn env = do
    (rows, highest) ← Lua.liftIO $ readEventLogProgress env
    Lua.newtable
    pushEventRows rows
    Lua.setfield (-2) "rows"
    Lua.pushinteger (fromIntegral highest)
    Lua.setfield (-2) "highest"
    return 1

-- | @engine.getNotificationCfg()@ — return all categories in
--   registry order as a Lua array of
--   @{id, displayName, description, textColor={r,g,b,a},
--     log, popup, pause}@ tables. The settings tab uses this to
--   build the per-category rows.
getNotificationCfgFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getNotificationCfgFn env = do
    let events = toEventsCapability env
    cfgMap ← Lua.liftIO $ readIORef (ecNotificationCfgRef events)
    let order = ecNotificationOrder events
    Lua.newtable
    forM_ (zip [1..] order) $ \(i, catId) →
        case HM.lookup catId cfgMap of
            Nothing → return ()  -- shouldn't happen; order from same map
            Just c → do
                Lua.newtable
                Lua.pushstring (TE.encodeUtf8 (ccId c))
                Lua.setfield (-2) "id"
                Lua.pushstring (TE.encodeUtf8 (ccDisplayName c))
                Lua.setfield (-2) "displayName"
                Lua.pushstring (TE.encodeUtf8 (ccDescription c))
                Lua.setfield (-2) "description"
                Lua.pushboolean (ccLog   c); Lua.setfield (-2) "log"
                Lua.pushboolean (ccPopup c); Lua.setfield (-2) "popup"
                Lua.pushboolean (ccPause c); Lua.setfield (-2) "pause"
                -- text color as {r, g, b, a} subtable
                let (r, g, b, a) = ccTextColor c
                Lua.newtable
                Lua.pushnumber (Lua.Number (realToFrac r))
                Lua.setfield (-2) "r"
                Lua.pushnumber (Lua.Number (realToFrac g))
                Lua.setfield (-2) "g"
                Lua.pushnumber (Lua.Number (realToFrac b))
                Lua.setfield (-2) "b"
                Lua.pushnumber (Lua.Number (realToFrac a))
                Lua.setfield (-2) "a"
                Lua.setfield (-2) "textColor"
                -- coalesceWindow: popup wall-seconds (0 = disabled)
                Lua.pushnumber (Lua.Number (ccPopupCoalesceWindow c))
                Lua.setfield (-2) "coalesceWindow"
                Lua.rawseti (-2) i
    return 1

-- | @engine.setNotificationOverrides(overrides)@ — apply per-category
--   overrides and persist to @config/notifications.local.yaml@. The
--   @overrides@ table is shaped @{ catId = {log=b, popup=b, pause=b}
--   ... }@; missing categories and missing fields are left alone.
--   Unknown category ids are ignored with a dev-log warning.
--
--   Returns @true@ when the file was durably replaced and @false@ when
--   the write failed (#2202), logging the path and the cause at warning
--   level; a filesystem failure never raises a Lua error. The live merge
--   below is NOT rolled back on a failed write — the in-memory config is
--   what routes the next emit, the YAML is the next-session record — and
--   'Engine.Asset.YamlNotifications.writeNotificationOverrides' states
--   that policy on the writer itself.
setNotificationOverridesFn ∷ EngineEnv
                           → Lua.LuaE Lua.Exception Lua.NumResults
setNotificationOverridesFn env = do
    isTab ← Lua.istable 1
    if not isTab
        then do
            Lua.pushboolean False
            return 1
        else do
            updates ← readOverridesTable
            logger ← Lua.liftIO $ readIORef (ccLoggerRef (toCoreCapability env))
            updated ← Lua.liftIO $ atomicModifyIORef'
                          (ecNotificationCfgRef (toEventsCapability env)) $ \cfg →
                let merged = HM.foldrWithKey
                        (\catId (mLog, mPopup, mPause) acc →
                            case HM.lookup catId acc of
                                Nothing → acc
                                Just cur → HM.insert catId
                                    (cur
                                        { ccLog   = maybe (ccLog cur)   id mLog
                                        , ccPopup = maybe (ccPopup cur) id mPopup
                                        , ccPause = maybe (ccPause cur) id mPause
                                        }) acc)
                        cfg updates
                in (merged, merged)
            -- Warn (loud-fail) on unknown category ids so the Lua
            -- side notices a typo before it costs the player a save
            -- failure that never pops.
            let unknownIds = filter
                    (\k → not (HM.member k updated))
                    (HM.keys updates)
            Lua.liftIO $ forM_ unknownIds $ \k →
                logWarn logger CatEvent $
                    "setNotificationOverrides: unknown category '"
                      <> k <> "'; ignored"
            -- Persist after the merge so the YAML reflects whatever
            -- known categories survived. Write errors don't roll back
            -- the in-memory update — the in-memory cfg is what
            -- routes the next emit; the YAML is the next-session
            -- record.
            written ← Lua.liftIO $ writeNotificationOverrides
                "config/notifications.local.yaml" updated
            case written of
                Right () → pure ()
                Left err → Lua.liftIO $ logWarn logger CatEvent $
                    "setNotificationOverrides: " <> err
            Lua.pushboolean (either (const False) (const True) written)
            return 1

-- | Read a Lua table of shape
--   @{ catId = {log=bool, popup=bool, pause=bool}, … }@ at stack
--   index 1 into a HashMap of @(maybeLog, maybePopup, maybePause)@.
--   Missing fields stay 'Nothing' so the merge above only overwrites
--   what the caller specified.
readOverridesTable
    ∷ Lua.LuaE Lua.Exception
        (HM.HashMap Text (Maybe Bool, Maybe Bool, Maybe Bool))
readOverridesTable = do
    -- iterate the outer table at index 1
    Lua.pushvalue 1   -- copy outer table to top
    Lua.pushnil       -- first key
    loop HM.empty
  where
    loop acc = do
        more ← Lua.next (-2)
        if not more
            then do
                Lua.pop 1  -- pop the outer-table copy
                return acc
            else do
                -- Check the key's type instead of converting it:
                -- lua_tolstring on a numeric key mutates it in place,
                -- and next() then errors with "invalid key to 'next'".
                keyTy ← Lua.ltype (-2)
                mk ← if keyTy ≡ Lua.TypeString
                         then Lua.tostring (-2)
                         else return Nothing
                innerIsTab ← Lua.istable (-1)
                case mk of
                    Just kb | innerIsTab → do
                        l ← readBoolField "log"
                        p ← readBoolField "popup"
                        z ← readBoolField "pause"
                        Lua.pop 1  -- pop inner table (keep key)
                        loop (HM.insert (TE.decodeUtf8Lenient kb) (l, p, z) acc)
                    _ → do
                        Lua.pop 1
                        loop acc

-- | Read an optional boolean field from the inner table at top of
--   stack. Returns 'Nothing' if absent so the merge preserves the
--   existing value for fields the caller didn't specify.
readBoolField ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Bool)
readBoolField name = do
    _ ← Lua.getfield (-1) (Lua.Name name)
    isNil ← Lua.isnil (-1)
    isB   ← Lua.isboolean (-1)
    result ← if isNil || not isB
                then return Nothing
                else Just <$> Lua.toboolean (-1)
    Lua.pop 1
    return result
