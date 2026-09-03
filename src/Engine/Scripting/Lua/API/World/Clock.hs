{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.Clock
    ( worldSetTextureFn
    , worldSetCameraFn
    , worldSetSunAngleFn
    , worldSetTimeFn
    , worldSetDateFn
    , worldGetDateFn
    , worldGetSeedFn
    , worldSetTimeScaleFn
    , worldGetTimeScaleFn
    , worldGetActiveWorldIdFn
    , worldSetMapModeFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), withPlayerIntent)
import Engine.Core.State (activeWorldStateFrom)
import Engine.Graphics.Solar (overriddenSolar)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Material (parseTextureType)
import World.Types
import World.Render.Zoom.Types (textToMapMode)

-- | world.setTexture(pageId, textureType, textureHandle)
worldSetTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureTypeArg ← Lua.tostring 2
    textureHandleArg ← Lua.tointeger 3

    case (pageIdArg, textureTypeArg, textureHandleArg) of
        (Just pageIdBS, Just typeBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texType = parseTextureType (TE.decodeUtf8Lenient typeBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) (WorldSetTexture pageId texType texHandle)
        _ → pure ()

    return 0

-- | world.setCamera(pageId, x, y)
worldSetCameraFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCameraFn wsc = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just (Lua.Number x), Just (Lua.Number y)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc)
                (WorldSetCamera pageId (realToFrac x) (realToFrac y))
        _ → pure ()

    return 0

-- | @world.setSunAngle(angle)@ — a direct override of the sun angle
--   (0..1) that bypasses the time system.
--
--   It takes no page argument, and deliberately still does not (#1869).
--   Its meaning under per-page solar attribution is the one it has
--   always had, stated:
--
--     * It is a PROCESS-GLOBAL render\/query override. While it is in
--       force, EVERY rendered page takes @angle@ as its base sun angle
--       — each still dividing by its OWN circumference, so the
--       longitude spread across a page is unchanged — and page-less
--       geometry and @world.getLocalSunAngle@ read it too.
--     * It mutates no page's @wsTimeRef@. No clock moves; nothing about
--       it is persisted.
--     * It lasts until the next visible-page clock publication
--       overwrites it, i.e. until the next world tick that has a
--       visible page ('World.Thread.Time'). That is precisely how long
--       it lasted before #1869, when it wrote the same single scalar
--       the tick republished.
worldSetSunAngleFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSunAngleFn wsc = do
    angleArg ← Lua.tonumber 1

    case angleArg of
        Just (Lua.Number angle) → Lua.liftIO $ do
            atomicModifyIORef' (wsSunAngleRef wsc) $ \_ →
                (overriddenSolar (realToFrac angle), ())
        _ → pure ()

    return 0

-- | world.setTime(pageId, hour, minute)
-- Set the world clock. The world thread will compute sun angle from this.
worldSetTimeFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeFn wsc = do
    pageIdArg ← Lua.tostring 1
    hourArg   ← Lua.tointeger 2
    minuteArg ← Lua.tointeger 3

    case (pageIdArg, hourArg, minuteArg) of
        (Just pageIdBS, Just h, Just m) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc)
                (WorldSetTime pageId (fromIntegral h) (fromIntegral m))
        _ → pure ()

    return 0

-- | world.setDate(pageId, year, month, day)
-- Set the world date. Currently unused for sun angle (placeholder for seasons).
worldSetDateFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetDateFn wsc = do
    pageIdArg ← Lua.tostring 1
    yearArg   ← Lua.tointeger 2
    monthArg  ← Lua.tointeger 3
    dayArg    ← Lua.tointeger 4

    case (pageIdArg, yearArg, monthArg, dayArg) of
        (Just pageIdBS, Just y, Just mo, Just d) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc)
                (WorldSetDate pageId (fromIntegral y) (fromIntegral mo) (fromIntegral d))
        _ → pure ()

    return 0

-- | world.getDate(pageId) → {year, month, day, dayOfYear, absoluteDay} | nil
-- Reads the named world's calendar date directly from 'wsDateRef'.
-- dayOfYear is the zero-based year-relative ordinal (what the flora
-- annual cycle selects on); absoluteDay is whole days since the world
-- epoch (the #332 flora growth clock). nil when the pageId isn't
-- registered. The date advances on its own now (midnight rollover in
-- tickWorldTime) — this is how tests observe it.
worldGetDateFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetDateFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    (date, calendar) ← Lua.liftIO $ do
                        d ← readIORef (wsDateRef ws)
                        paramsM ← readIORef (wsGenParamsRef ws)
                        pure (d, maybe defaultCalendarConfig wgpCalender paramsM)
                    let WorldDate y mo d = date
                    Lua.newtable
                    Lua.pushinteger (fromIntegral y)
                    Lua.setfield (-2) "year"
                    Lua.pushinteger (fromIntegral mo)
                    Lua.setfield (-2) "month"
                    Lua.pushinteger (fromIntegral d)
                    Lua.setfield (-2) "day"
                    Lua.pushinteger (fromIntegral
                        (worldDateToDayOfYear calendar date))
                    Lua.setfield (-2) "dayOfYear"
                    Lua.pushinteger (fromIntegral
                        (worldAbsoluteDay calendar date))
                    Lua.setfield (-2) "absoluteDay"
                Nothing → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

-- | world.getSeed([pageId]) → seed or nil
-- The generation seed of the given (default: active) world page. Added
-- for the playtest harness (#647): the session trace records the real
-- seed of whatever world the player created through the UI, so a
-- session with a randomized seed is still diagnosable and a replay's
-- world divergence is detectable. nil while no world (or no gen
-- params yet) exists.
worldGetSeedFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSeedFn wsc = do
    pageIdArg ← Lua.tostring 1
    mWs ← Lua.liftIO $ case pageIdArg of
        Just pageIdBS → do
            mgr ← readIORef (wsWorldManagerRef wsc)
            pure (lookup (WorldPageId (TE.decodeUtf8Lenient pageIdBS)) (wmWorlds mgr))
        Nothing → activeWorldStateFrom (wsWorldManagerRef wsc)
    mParams ← Lua.liftIO $ case mWs of
        Just ws → readIORef (wsGenParamsRef ws)
        Nothing → pure Nothing
    case mParams of
        Just params → Lua.pushinteger (fromIntegral (wgpSeed params))
        Nothing → Lua.pushnil
    return 1

-- | world.setTimeScale(pageId, scale) → true | false, diagnostic
--
-- Set how fast time passes: game-minutes per real-second.
-- 1.0 = real-time, 60.0 = 1 game-hour per real-second, 0.0 = paused.
--
-- The second argument must be an actual Lua @number@ — a numeric STRING
-- such as @"1"@ is refused, because 'Lua.tonumber' would coerce it and
-- a typo'd payload would then reprogram the world clock. It is converted
-- to the clock's authoritative 'Float' storage and only then classified,
-- so a Lua number that is finite as a 'Double' but overflows on the way
-- into 'Float' (@1e300@) is refused too.
--
-- The accepted domain is 'World.Time.Scale.classifyTimeScale': finite,
-- non-negative, and at most 'World.Time.Scale.maxTimeScale'. That
-- ceiling is a representation-safety bound derived from the clock's own
-- constants — the largest scale whose worst-case tick still floors to a
-- representable day count — NOT a gameplay speed limit; every shipped
-- caller sits orders of magnitude below it. Both signed zeros are
-- accepted and pause the page clock.
--
-- Returns exactly one result, @true@, when the request is queued.
-- Returns exactly two, @false@ and a diagnostic string, when it is
-- refused — a refusal never raises, so a caller that ignores the result
-- (@scripts\/pause.lua@, every probe) is unaffected. A refused call
-- queues nothing, touches neither the live scale nor a pause epoch's
-- resume scale, leaves @world.getTimeScale@ reading what it read before,
-- and does NOT bump the player-intent generation: validation happens
-- before 'withPlayerIntent' is entered, not inside it.
--
-- Page-not-found is unchanged: a well-formed pageId naming an
-- unregistered page still queues and still returns @true@; the world
-- thread logs it when the command is drained.
worldSetTimeScaleFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeScaleFn wsc = do
    -- The type check comes first and is not 'Lua.tonumber': that
    -- conversion accepts a numeric string, which is not a scale the
    -- caller meant to set. TypeNumber covers BOTH Lua number subtypes,
    -- so an integer literal -- what every shipped caller passes -- is a
    -- number here exactly as a float literal is.
    scaleTy ← Lua.ltype 2
    if scaleTy ≢ Lua.TypeNumber
        then do
            tyName ← TE.decodeUtf8Lenient ⊚ Lua.typename scaleTy
            refuseTimeScale $ "time scale must be a number, got " <> tyName
                <> "; the existing time scale is left unchanged."
        else do
            scaleArg ← Lua.tonumber 2
            case scaleArg of
                Nothing → refuseTimeScale
                    "time scale must be a number; the existing time \
                    \scale is left unchanged."
                Just (Lua.Number d) →
                    let stored = realToFrac d ∷ Float
                    in case classifyTimeScale stored of
                        Left refusal →
                            refuseTimeScale (describeTimeScaleRefusal refusal stored)
                        Right scale → do
                            pageIdArg ← Lua.tostring 1
                            case pageIdArg of
                                Nothing → refuseTimeScale
                                    "page id must be a string; the \
                                    \existing time scale is left unchanged."
                                Just pageIdBS → do
                                    Lua.liftIO $ queueTimeScale wsc
                                        (WorldPageId (TE.decodeUtf8Lenient pageIdBS))
                                        scale
                                    Lua.pushboolean True
                                    return 1

-- | The refusal half of 'worldSetTimeScaleFn''s return contract: two
--   results, @false@ and the diagnostic, and no side effect whatsoever.
refuseTimeScale ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
refuseTimeScale diagnostic = do
    Lua.pushboolean False
    Lua.pushstring (TE.encodeUtf8 diagnostic)
    return 2

-- | Queue an ACCEPTED time-scale request as one player intent.
--
--   #913: bump at REQUEST time, not when the world thread eventually
--   applies the command. During a save the world thread is inside the
--   save transaction and cannot drain this queue at all, so a
--   handler-side bump would land AFTER the autosave already decided
--   whether to restore -- exactly the window the generation exists to
--   cover. Every caller of this verb is expressing player intent
--   (scripts/pause.lua's resume, a speed control, the debug console);
--   the engine's own internal clock writes go straight to wsTimeScaleRef
--   and never come through here.
--
--   Only reached for a scale 'classifyTimeScale' accepted, so a refused
--   request never advances that generation (#2280).
queueTimeScale ∷ WorldSimCapability → WorldPageId → Float → IO ()
queueTimeScale wsc pageId scale =
    withPlayerIntent wsc $
        Q.writeQueue (wsWorldQueue wsc) (WorldSetTimeScale pageId scale)

-- | world.getTimeScale(pageId) → number
-- Reads the named world's current time scale directly from
-- 'wsTimeScaleRef'. Returns 1.0 if the pageId isn't registered
-- (matches the engine's default scale).
worldGetTimeScaleFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetTimeScaleFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    s ← Lua.liftIO $ readIORef (wsTimeScaleRef ws)
                    Lua.pushnumber (Lua.Number (realToFrac s))
                Nothing →
                    Lua.pushnumber (Lua.Number 1.0)
        Nothing →
            Lua.pushnumber (Lua.Number 1.0)
    return 1

-- | world.getActiveWorldId() → string | nil
-- Returns the pageId of the first visible world, falling back to the
-- first world in 'wmWorlds' if none are marked visible (e.g. mid-
-- transition). Returns nil when no worlds are registered (main menu).
-- Lua callers use this to target "the current world" without
-- hardcoding "main_world" or "test_arena".
worldGetActiveWorldIdFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetActiveWorldIdFn wsc = do
    mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
    let active = case wmVisible mgr of
            (pageId:_) → Just pageId
            []         → case wmWorlds mgr of
                ((pageId, _):_) → Just pageId
                []              → Nothing
    case active of
        Just (WorldPageId t) → Lua.pushstring (TE.encodeUtf8 t)
        Nothing              → Lua.pushnil
    return 1

-- | world.setMapMode(pageId, mode)
worldSetMapModeFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMapModeFn wsc = do
    pageIdArg ← Lua.tostring 1
    modeArg   ← Lua.tostring 2

    case (pageIdArg, modeArg) of
        (Just pageIdBS, Just modeBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                mode = textToMapMode (TE.decodeUtf8Lenient modeBS)
            Q.writeQueue (wsWorldQueue wsc)
                (WorldSetMapMode pageId mode)
        _ → pure ()

    return 0
