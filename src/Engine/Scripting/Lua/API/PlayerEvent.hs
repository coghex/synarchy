{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.PlayerEvent
    ( emitEventFn
    , emitEventAtFn
    , emitEventForUnitFn
    , getEventLogFn
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
import Engine.Core.Log (LogCategory(..), logWarn)
import Engine.Core.State (EngineEnv(..))
import Engine.PlayerEvent (CategoryCfg(..), PopupButton(..)
                          , popupActionTag)
import Engine.PlayerEvent.Emit (PlayerEvent(..), emitEvent, emitEventAt
                               , emitEventFull, readEventLog)

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
                (TE.decodeUtf8 catBS)
                "Lua"
                (TE.decodeUtf8 textBS)
        _ → return ()
    return 0

-- | @engine.emitEventAt(category, text, gx, gy)@ — fire a popup with
--   a location payload. If the category's YAML buttons include
--   @go_to@, the popup renders a "Go To" button that pans the
--   camera to @(gx, gy)@. Categories without a @go_to@ button
--   ignore the payload.
emitEventAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
emitEventAtFn env = do
    catArg  ← Lua.tostring 1
    textArg ← Lua.tostring 2
    gxArg   ← Lua.tointeger 3
    gyArg   ← Lua.tointeger 4
    case (catArg, textArg, gxArg, gyArg) of
        (Just catBS, Just textBS, Just gx, Just gy) →
            Lua.liftIO $ emitEventAt env
                (TE.decodeUtf8 catBS)
                "Lua"
                (TE.decodeUtf8 textBS)
                (Just (fromIntegral gx, fromIntegral gy))
        _ → return ()
    return 0

-- | @engine.emitEventForUnit(category, text, uid [, gx, gy])@ — fire an
--   event tagged with the UNIT it concerns, so the per-unit log panel
--   can filter it. @gx@/@gy@ are optional (same location payload as
--   'emitEventAt'). Used by unit-attributable emitters (survival
--   warnings/criticals, unit events) that already know the uid.
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
                (TE.decodeUtf8 catBS)
                "Lua"
                (TE.decodeUtf8 textBS)
                mCoords
                (Just (fromIntegral uid))
        _ → return ()
    return 0

-- | @engine.getEventLog()@ — return the event-log ring buffer as a
--   Lua array of @{category, text, gameTime, source, buttons,
--   coords}@ tables, oldest-first. @buttons@ is an array of
--   @{label, action}@ pairs (per the event's resolved category cfg
--   at emit time); @coords@ is either @{x, y}@ or @nil@. Sufficient
--   payload for the event-log panel to re-pop the popup from a row
--   click without a second engine round-trip.
getEventLogFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getEventLogFn env = do
    events ← Lua.liftIO $ readEventLog env
    Lua.newtable
    forM_ (zip [1..] events) $ \(i, ev) → do
        Lua.newtable
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

        -- buttons array (same shape as getNotificationCfg's buttons
        -- field) — needed for the event-log panel's row click to
        -- spawn a popup with the original button set.
        Lua.newtable
        forM_ (zip [1..] (peButtons ev)) $ \(j, btn) → do
            Lua.newtable
            Lua.pushstring (TE.encodeUtf8 (pbLabel btn))
            Lua.setfield (-2) "label"
            Lua.pushstring (TE.encodeUtf8 (popupActionTag (pbAction btn)))
            Lua.setfield (-2) "action"
            Lua.rawseti (-2) j
        Lua.setfield (-2) "buttons"

        -- coords: either a {x, y} subtable or nil. nil means the
        -- event was emitted via emitEvent (no location) — repop
        -- should suppress any 'go_to' buttons just like the first
        -- spawn did.
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

        Lua.rawseti (-2) i
    return 1

-- | @engine.getNotificationCfg()@ — return all categories in
--   registry order as a Lua array of
--   @{id, displayName, description, textColor={r,g,b,a},
--     log, popup, pause}@ tables. The settings tab uses this to
--   build the per-category rows.
getNotificationCfgFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getNotificationCfgFn env = do
    cfgMap ← Lua.liftIO $ readIORef (notificationCfgRef env)
    let order = notificationOrder env
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
                -- buttons array: { {label, action}, ... }
                Lua.newtable
                forM_ (zip [1..] (ccButtons c)) $ \(j, btn) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (pbLabel btn))
                    Lua.setfield (-2) "label"
                    Lua.pushstring
                        (TE.encodeUtf8 (popupActionTag (pbAction btn)))
                    Lua.setfield (-2) "action"
                    Lua.rawseti (-2) j
                Lua.setfield (-2) "buttons"
                -- coalesceWindow: popup wall-seconds (0 = disabled)
                Lua.pushnumber (Lua.Number (ccPopupCoalesceWindow c))
                Lua.setfield (-2) "coalesceWindow"
                Lua.rawseti (-2) i
    return 1

-- | @engine.setNotificationOverrides(overrides)@ — apply per-category
--   overrides and persist to @config/notifications.yaml@. The
--   @overrides@ table is shaped @{ catId = {log=b, popup=b, pause=b}
--   ... }@; missing categories and missing fields are left alone.
--   Unknown category ids are ignored with a dev-log warning.
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
            logger ← Lua.liftIO $ readIORef (loggerRef env)
            updated ← Lua.liftIO $ atomicModifyIORef'
                          (notificationCfgRef env) $ \cfg →
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
            Lua.liftIO $ writeNotificationOverrides
                "config/notifications.yaml" updated
            Lua.pushboolean True
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
                if innerIsTab && mk /= Nothing
                    then do
                        l ← readBoolField "log"
                        p ← readBoolField "popup"
                        z ← readBoolField "pause"
                        Lua.pop 1  -- pop inner table (keep key)
                        let Just kb = mk
                        loop (HM.insert (TE.decodeUtf8 kb) (l, p, z) acc)
                    else do
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
