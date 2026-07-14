{-# LANGUAGE Strict, UnicodeSyntax #-}
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
import Engine.Core.State (EngineEnv(..), activeWorldState)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Material (parseTextureType)
import World.Types

-- | world.setTexture(pageId, textureType, textureHandle)
worldSetTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureTypeArg ← Lua.tostring 2
    textureHandleArg ← Lua.tointeger 3

    case (pageIdArg, textureTypeArg, textureHandleArg) of
        (Just pageIdBS, Just typeBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texType = parseTextureType (TE.decodeUtf8Lenient typeBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) (WorldSetTexture pageId texType texHandle)
        _ → pure ()

    return 0

-- | world.setCamera(pageId, x, y)
worldSetCameraFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCameraFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just (Lua.Number x), Just (Lua.Number y)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetCamera pageId (realToFrac x) (realToFrac y))
        _ → pure ()

    return 0

-- | world.setSunAngle(angle)
-- Direct override of sun angle (0..1), bypasses time system
worldSetSunAngleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSunAngleFn env = do
    angleArg ← Lua.tonumber 1

    case angleArg of
        Just (Lua.Number angle) → Lua.liftIO $ do
            atomicModifyIORef' (sunAngleRef env) $ \_ → (realToFrac angle, ())
        _ → pure ()

    return 0

-- | world.setTime(pageId, hour, minute)
-- Set the world clock. The world thread will compute sun angle from this.
worldSetTimeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeFn env = do
    pageIdArg ← Lua.tostring 1
    hourArg   ← Lua.tointeger 2
    minuteArg ← Lua.tointeger 3

    case (pageIdArg, hourArg, minuteArg) of
        (Just pageIdBS, Just h, Just m) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetTime pageId (fromIntegral h) (fromIntegral m))
        _ → pure ()

    return 0

-- | world.setDate(pageId, year, month, day)
-- Set the world date. Currently unused for sun angle (placeholder for seasons).
worldSetDateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetDateFn env = do
    pageIdArg ← Lua.tostring 1
    yearArg   ← Lua.tointeger 2
    monthArg  ← Lua.tointeger 3
    dayArg    ← Lua.tointeger 4

    case (pageIdArg, yearArg, monthArg, dayArg) of
        (Just pageIdBS, Just y, Just mo, Just d) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env)
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
worldGetDateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetDateFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
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
worldGetSeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSeedFn env = do
    pageIdArg ← Lua.tostring 1
    mWs ← Lua.liftIO $ case pageIdArg of
        Just pageIdBS → do
            mgr ← readIORef (worldManagerRef env)
            pure (lookup (WorldPageId (TE.decodeUtf8Lenient pageIdBS)) (wmWorlds mgr))
        Nothing → activeWorldState env
    mParams ← Lua.liftIO $ case mWs of
        Just ws → readIORef (wsGenParamsRef ws)
        Nothing → pure Nothing
    case mParams of
        Just params → Lua.pushinteger (fromIntegral (wgpSeed params))
        Nothing → Lua.pushnil
    return 1

-- | world.setTimeScale(pageId, scale)
-- Set how fast time passes: game-minutes per real-second.
-- 1.0 = real-time, 60.0 = 1 game-hour per real-second, 0.0 = paused
worldSetTimeScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeScaleFn env = do
    pageIdArg ← Lua.tostring 1
    scaleArg  ← Lua.tonumber 2

    case (pageIdArg, scaleArg) of
        (Just pageIdBS, Just (Lua.Number s)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetTimeScale pageId (realToFrac s))
        _ → pure ()

    return 0

-- | world.getTimeScale(pageId) → number
-- Reads the named world's current time scale directly from
-- 'wsTimeScaleRef'. Returns 1.0 if the pageId isn't registered
-- (matches the engine's default scale).
worldGetTimeScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetTimeScaleFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
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
worldGetActiveWorldIdFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetActiveWorldIdFn env = do
    mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
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
worldSetMapModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMapModeFn env = do
    pageIdArg ← Lua.tostring 1
    modeArg   ← Lua.tostring 2

    case (pageIdArg, modeArg) of
        (Just pageIdBS, Just modeBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                mode = textToMapMode (TE.decodeUtf8Lenient modeBS)
            Q.writeQueue (worldQueue env)
                (WorldSetMapMode pageId mode)
        _ → pure ()

    return 0
