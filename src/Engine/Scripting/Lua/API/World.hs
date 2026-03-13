{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World
    ( worldGetGenDefaultsFn
    , worldSetGenConfigFn
    , worldInitFn
    , worldInitArenaFn
    , worldInitArenaDoneFn
    , worldOpenArenaFn
    , worldShowFn
    , worldHideFn
    , worldSetTextureFn
    , worldSetCameraFn
    , worldSetSunAngleFn
    , worldSetTimeFn
    , worldSetDateFn
    , worldSetTimeScaleFn
    , worldSetMapModeFn
    , worldSetZoomCursorHoverFn
    , worldSetZoomCursorSelectFn
    , worldClearZoomCursorSelectFn
    , worldSetZoomCursorSelectTextureFn
    , worldSetZoomCursorHoverTextureFn
    , worldSetWorldCursorSelectTextureFn
    , worldSetWorldCursorHoverTextureFn
    , worldSetWorldCursorHoverFn
    , worldSetWorldCursorSelectFn
    , worldClearWorldCursorSelectFn
    , worldSetWorldCursorSelectBgTextureFn
    , worldSetWorldCursorHoverBgTextureFn
    , worldSetToolModeFn
    , worldGetInitProgressFn
    , worldWaitForInitFn
    , worldDestroyFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Material (parseTextureType)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Tool.Types (ToolMode(..), textToToolMode)
import World.Render.Zoom.Types (ZoomMapMode(..), textToMapMode)
import World.Generate.Config

-- | world.getGenDefaults() → table
--   Returns the world generation config as a Lua table.
worldGetGenDefaultsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetGenDefaultsFn env = do
    cfg ← Lua.liftIO $ readIORef (worldGenConfigRef env)
    Lua.newtable
    -- Top-level params
    case wgcSeed cfg of
        Just s  → do Lua.pushstring (TE.encodeUtf8 (T.pack (show s)))
                     Lua.setfield (Lua.nth 2) "seed"
        Nothing → pure ()
    Lua.pushinteger (fromIntegral (wgcWorldSize cfg))
    Lua.setfield (Lua.nth 2) "world_size"
    Lua.pushinteger (fromIntegral (wgcPlateCount cfg))
    Lua.setfield (Lua.nth 2) "plate_count"
    Lua.pushnumber (Lua.Number (realToFrac (wgcErosionIntensity cfg)))
    Lua.setfield (Lua.nth 2) "erosion_intensity"
    Lua.pushnumber (Lua.Number (realToFrac (wgcVolcanicActivity cfg)))
    Lua.setfield (Lua.nth 2) "volcanic_activity"
    -- Calendar sub-table
    let cal = wgcCalendar cfg
    Lua.newtable
    Lua.pushinteger (fromIntegral (cyDaysPerMonth cal))
    Lua.setfield (Lua.nth 2) "days_per_month"
    Lua.pushinteger (fromIntegral (cyMonthsPerYear cal))
    Lua.setfield (Lua.nth 2) "months_per_year"
    Lua.pushinteger (fromIntegral (cyHoursPerDay cal))
    Lua.setfield (Lua.nth 2) "hours_per_day"
    Lua.pushinteger (fromIntegral (cyMinutesPerHour cal))
    Lua.setfield (Lua.nth 2) "minutes_per_hour"
    Lua.setfield (Lua.nth 2) "calendar"
    -- Sun sub-table
    let sun = wgcSun cfg
    Lua.newtable
    Lua.pushnumber (Lua.Number (realToFrac (syTiltAngle sun)))
    Lua.setfield (Lua.nth 2) "tilt_angle"
    Lua.pushnumber (Lua.Number (realToFrac (syDayLength sun)))
    Lua.setfield (Lua.nth 2) "day_length"
    Lua.setfield (Lua.nth 2) "sun"
    -- Moon sub-table
    let moon = wgcMoon cfg
    Lua.newtable
    Lua.pushinteger (fromIntegral (myCycleDays moon))
    Lua.setfield (Lua.nth 2) "cycle_days"
    Lua.pushnumber (Lua.Number (realToFrac (myPhaseOffset moon)))
    Lua.setfield (Lua.nth 2) "phase_offset"
    Lua.setfield (Lua.nth 2) "moon"
    -- Climate sub-table
    let cl = wgcClimate cfg
    Lua.newtable
    Lua.pushinteger (fromIntegral (clIterations cl))
    Lua.setfield (Lua.nth 2) "iterations"
    Lua.pushnumber (Lua.Number (realToFrac (clCoriolisScale cl)))
    Lua.setfield (Lua.nth 2) "coriolis_scale"
    Lua.pushnumber (Lua.Number (realToFrac (clWindDrag cl)))
    Lua.setfield (Lua.nth 2) "wind_drag"
    Lua.pushnumber (Lua.Number (realToFrac (clThermalInertia cl)))
    Lua.setfield (Lua.nth 2) "thermal_inertia"
    Lua.pushnumber (Lua.Number (realToFrac (clOrographicScale cl)))
    Lua.setfield (Lua.nth 2) "orographic_scale"
    Lua.pushnumber (Lua.Number (realToFrac (clEvapScale cl)))
    Lua.setfield (Lua.nth 2) "evap_scale"
    Lua.pushnumber (Lua.Number (realToFrac (clAlbedoFeedback cl)))
    Lua.setfield (Lua.nth 2) "albedo_feedback"
    Lua.pushnumber (Lua.Number (realToFrac (clThcThreshold cl)))
    Lua.setfield (Lua.nth 2) "thc_threshold"
    Lua.setfield (Lua.nth 2) "climate"
    return 1

-- | world.setGenConfig(table)
--   Updates the world generation config from a Lua table.
--   Only updates fields that are present in the table.
worldSetGenConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetGenConfigFn env = do
    -- Arg 1 is a table on the stack
    let getIntField ∷ Lua.Name → Int → Lua.LuaE Lua.Exception Int
        getIntField name def = do
            Lua.getfield (Lua.nth 1) name
            mi ← Lua.tointeger Lua.top
            Lua.pop 1
            pure $ maybe def fromIntegral mi
        getFloatField ∷ Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getFloatField name def = do
            Lua.getfield (Lua.nth 1) name
            mn ← Lua.tonumber Lua.top
            Lua.pop 1
            pure $ case mn of
                Just (Lua.Number n) → realToFrac n
                _                   → def
        getSubInt ∷ Lua.Name → Lua.Name → Int → Lua.LuaE Lua.Exception Int
        getSubInt tbl name def = do
            Lua.getfield (Lua.nth 1) tbl
            isT ← Lua.istable Lua.top
            if isT
                then do
                    Lua.getfield (Lua.nth 1) name
                    mi ← Lua.tointeger Lua.top
                    Lua.pop 2
                    pure $ maybe def fromIntegral mi
                else do
                    Lua.pop 1
                    pure def
        getSubFloat ∷ Lua.Name → Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getSubFloat tbl name def = do
            Lua.getfield (Lua.nth 1) tbl
            isT ← Lua.istable Lua.top
            if isT
                then do
                    Lua.getfield (Lua.nth 1) name
                    mn ← Lua.tonumber Lua.top
                    Lua.pop 2
                    pure $ case mn of
                        Just (Lua.Number n) → realToFrac n
                        _                   → def
                else do
                    Lua.pop 1
                    pure def

    oldCfg ← Lua.liftIO $ readIORef (worldGenConfigRef env)
    let oldCal = wgcCalendar oldCfg
        oldSun = wgcSun oldCfg
        oldMoon = wgcMoon oldCfg
        oldCl  = wgcClimate oldCfg

    -- Top-level
    plateCount ← getIntField "plate_count" (wgcPlateCount oldCfg)
    worldSize  ← getIntField "world_size"  (wgcWorldSize oldCfg)
    erosionInt ← getFloatField "erosion_intensity" (wgcErosionIntensity oldCfg)
    volcanicAct ← getFloatField "volcanic_activity" (wgcVolcanicActivity oldCfg)

    -- Calendar
    dpm  ← getSubInt "calendar" "days_per_month"   (cyDaysPerMonth oldCal)
    mpy  ← getSubInt "calendar" "months_per_year"  (cyMonthsPerYear oldCal)
    hpd  ← getSubInt "calendar" "hours_per_day"    (cyHoursPerDay oldCal)
    mphr ← getSubInt "calendar" "minutes_per_hour" (cyMinutesPerHour oldCal)

    -- Sun
    tilt ← getSubFloat "sun" "tilt_angle" (syTiltAngle oldSun)
    dayL ← getSubFloat "sun" "day_length" (syDayLength oldSun)

    -- Moon
    cyc  ← getSubInt   "moon" "cycle_days"   (myCycleDays oldMoon)
    poff ← getSubFloat "moon" "phase_offset" (myPhaseOffset oldMoon)

    -- Climate
    iters  ← getSubInt   "climate" "iterations"       (clIterations oldCl)
    corio  ← getSubFloat "climate" "coriolis_scale"   (clCoriolisScale oldCl)
    wdrag  ← getSubFloat "climate" "wind_drag"        (clWindDrag oldCl)
    therm  ← getSubFloat "climate" "thermal_inertia"  (clThermalInertia oldCl)
    orog   ← getSubFloat "climate" "orographic_scale" (clOrographicScale oldCl)
    evap   ← getSubFloat "climate" "evap_scale"       (clEvapScale oldCl)
    albedo ← getSubFloat "climate" "albedo_feedback"  (clAlbedoFeedback oldCl)
    thc    ← getSubFloat "climate" "thc_threshold"    (clThcThreshold oldCl)

    let newCfg = oldCfg
            { wgcWorldSize  = worldSize
            , wgcPlateCount = plateCount
            , wgcErosionIntensity = erosionInt
            , wgcVolcanicActivity = volcanicAct
            , wgcCalendar   = CalendarYaml dpm mpy hpd mphr
            , wgcSun        = SunYaml tilt dayL
            , wgcMoon       = MoonYaml cyc poff
            , wgcClimate    = ClimateYaml iters corio wdrag therm orog evap albedo thc
            }
    Lua.liftIO $ writeIORef (worldGenConfigRef env) newCfg
    return 0

-- | world.init(pageId, seed, worldSizeInChunks)
worldInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitFn env = do
    pageIdArg ← Lua.tostring 1
    seedArg   ← Lua.tointeger 2
    sizeArg   ← Lua.tointeger 3
    platesArg ← Lua.tointeger 4
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                seed   = maybe 42 fromIntegral seedArg
                size   = maybe 64 fromIntegral sizeArg
                plates = maybe 10 fromIntegral platesArg
            Q.writeQueue (worldQueue env) (WorldInit pageId seed size plates)
        Nothing → pure ()
    
    return 0

-- | world.initArena(pageId) — create flat test arena, no geology
worldInitArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8 bs)
            Nothing → WorldPageId "test_arena"    -- default when called with no args
    Lua.liftIO $ Q.writeQueue (worldQueue env) (WorldInitArena pageId)
    return 0

-- | world.initArenaDone(pageId) — signal that all arena textures have been sent
worldInitArenaDoneFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaDoneFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8 bs)
            Nothing → WorldPageId "test_arena"
    Lua.liftIO $ Q.writeQueue (worldQueue env) (WorldInitArenaDone pageId)
    return 0

-- | world.openArena() — convenience function that broadcasts to Lua
worldOpenArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldOpenArenaFn env = do
    Lua.liftIO $ Q.writeQueue (luaQueue env) (LuaOpenArena)
    return 0

-- | world.show(pageId)
worldShowFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldShowFn env = do
    pageIdArg ← Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldShow pageId)
        Nothing → pure ()
    
    return 0

-- | world.hide(pageId)
worldHideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHideFn env = do
    pageIdArg ← Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldHide pageId)
        Nothing → pure ()
    
    return 0

-- | world.setTexture(pageId, textureType, textureHandle)
worldSetTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureTypeArg ← Lua.tostring 2
    textureHandleArg ← Lua.tointeger 3
    
    case (pageIdArg, textureTypeArg, textureHandleArg) of
        (Just pageIdBS, Just typeBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texType = parseTextureType (TE.decodeUtf8 typeBS)
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
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
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
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
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
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetDate pageId (fromIntegral y) (fromIntegral mo) (fromIntegral d))
        _ → pure ()

    return 0

-- | world.setTimeScale(pageId, scale)
-- Set how fast time passes: game-minutes per real-second.
-- 1.0 = real-time, 60.0 = 1 game-hour per real-second, 0.0 = paused
worldSetTimeScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetTimeScaleFn env = do
    pageIdArg ← Lua.tostring 1
    scaleArg  ← Lua.tonumber 2

    case (pageIdArg, scaleArg) of
        (Just pageIdBS, Just (Lua.Number s)) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env)
                (WorldSetTimeScale pageId (realToFrac s))
        _ → pure ()

    return 0

-- | world.setMapMode(pageId, mode)
worldSetMapModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMapModeFn env = do
    pageIdArg ← Lua.tostring 1
    modeArg   ← Lua.tostring 2

    case (pageIdArg, modeArg) of
        (Just pageIdBS, Just modeBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                mode = textToMapMode (TE.decodeUtf8 modeBS)
            Q.writeQueue (worldQueue env)
                (WorldSetMapMode pageId mode)
        _ → pure ()

    return 0

-- | world.setZoomCursorHover(pageId, x, y)
worldSetZoomCursorHoverFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3

    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

-- | world.setZoomCursorSelect(pageId, x, y)
worldSetZoomCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetZoomCursorSelect pageId
        _ → pure ()
    return 0

-- | world.clearZoomCursorDeselect(pageId)
worldClearZoomCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearZoomCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorDeselect pageId
        _ → pure ()
    return 0

worldSetZoomCursorSelectTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorSelectTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetZoomCursorHoverTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetZoomCursorHoverTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2

    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetZoomCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHover pageId (round x) (round y)
        _ → pure ()
    return 0

worldSetWorldCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetWorldCursorSelect pageId
        _ → pure ()
    return 0

worldClearWorldCursorSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearWorldCursorSelectFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldSetWorldCursorDeselect pageId
        _ → pure ()
    return 0

worldSetWorldCursorSelectTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorSelectTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHoverTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorHoverBgTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorHoverBgTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorHoverBgTexture pageId texHandle
        _ → pure ()
    return 0

worldSetWorldCursorSelectBgTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetWorldCursorSelectBgTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetWorldCursorSelectBgTexture pageId texHandle
        _ → pure ()
    return 0

worldSetToolModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetToolModeFn env = do
    pageIdArg ← Lua.tostring 1
    toolModeArg ← Lua.tostring 2
    case (pageIdArg, toolModeArg) of
        (Just pageIdBS, Just toolModeBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                toolMode = TE.decodeUtf8 toolModeBS
            Q.writeQueue (worldQueue env) $
                WorldSetToolMode pageId $ textToToolMode toolMode
        _ → pure ()
    return 0

-- | world.getInitProgress() → (phase, current, total, stage)
--   phase: 0=idle, 1=setup, 2=chunks, 3=done
--   current/total: numeric progress within current phase
--   stage: human-readable string ("idle", "setup", "chunks", "done")
--
--   Returns 4 values for backward compatibility: existing Lua scripts
--   use `local phase, current, total = world.getInitProgress()` and
--   the 4th value (stage) is simply ignored by those callers.
worldGetInitProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetInitProgressFn env = do
    manager ← Lua.liftIO $ readIORef (worldManagerRef env)
    case wmWorlds manager of
        ((_, worldState):_) → do
            phase ← Lua.liftIO $ readIORef (wsLoadPhaseRef worldState)
            case phase of
                LoadIdle → do
                    Lua.pushinteger 0
                    Lua.pushinteger 0
                    Lua.pushinteger 0
                    Lua.pushstring "idle"
                LoadPhase1 current total → do
                    Lua.pushinteger 1
                    Lua.pushinteger (fromIntegral current)
                    Lua.pushinteger (fromIntegral total)
                    Lua.pushstring "setup"
                LoadPhase2 remaining total → do
                    Lua.pushinteger 2
                    Lua.pushinteger (fromIntegral (total - remaining))
                    Lua.pushinteger (fromIntegral total)
                    Lua.pushstring "chunks"
                LoadDone → do
                    Lua.pushinteger 3
                    Lua.pushinteger 1
                    Lua.pushinteger 1
                    Lua.pushstring "done"
            return 4
        [] → do
            Lua.pushinteger 0
            Lua.pushinteger 0
            Lua.pushinteger 0
            Lua.pushstring "idle"
            return 4

-- | world.waitForInit(timeout_seconds) → table (same as getInitProgress)
--   Blocks until world generation is complete or timeout is reached.
--   Default timeout: 600 seconds (10 minutes).
--   Returns the final progress table.
worldWaitForInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldWaitForInitFn env = do
    timeoutArg ← Lua.tointeger 1
    let timeoutSec = case timeoutArg of
            Just t | t > 0 → fromIntegral t ∷ Int
            _              → 600
        maxIter = timeoutSec * 4  -- poll at 250ms intervals
    Lua.liftIO $ waitLoop maxIter
    worldGetInitProgressFn env
  where
    waitLoop 0 = return ()
    waitLoop n = do
        manager ← readIORef (worldManagerRef env)
        case wmWorlds manager of
            ((_, ws):_) → do
                phase ← readIORef (wsLoadPhaseRef ws)
                case phase of
                    LoadDone → return ()
                    _        → do
                        threadDelay 250000
                        waitLoop (n - 1)
            [] → do
                threadDelay 250000
                waitLoop (n - 1)

-- | world.destroy(pageId)
-- Removes the world from the world manager entirely, freeing its state.
worldDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDestroyFn env = do
    pageIdArg ← Lua.tostring 1
    
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldDestroy pageId)
        Nothing → pure ()
    
    return 0
