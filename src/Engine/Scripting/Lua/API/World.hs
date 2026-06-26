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
    , worldGetTimeScaleFn
    , worldGetActiveWorldIdFn
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
    , worldSelectTileFn
    , worldSetWorldCursorSelectBgTextureFn
    , worldSetWorldCursorHoverBgTextureFn
    , worldSetMineAnchorFn
    , worldClearMineAnchorFn
    , worldDesignateMineFn
    , worldSetMineDesignateTextureFn
    , worldGetMineDesignationCountFn
    , worldNearestMineDesignationFn
    , worldGetDigInfoAtFn
    , worldGetSpoilInfoFn
    , worldGetGemInfoAtFn
    , worldDebugTileQuadsFn
    , worldDigTileFn
    , worldAddTileFn
    , worldListMaterialsFn
    , worldGetMineDesignationAtFn
    , worldSetToolModeFn
    , worldGetToolModeFn
    , worldGetInitProgressFn
    , worldWaitForInitFn
    , worldDestroyFn
    , worldDeleteTileFn
    , worldSetFluidTileFn
    , worldSetSlopeFn
    , worldSetCellFn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), activeWorldState)
import Engine.Core.Log (LogCategory(..), logWarn)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Material (parseTextureType)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Fluid.Types (FluidType(..))
import World.Generate.Coordinates (globalToChunk)
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..)
                      , materialIdByName)
import World.Gem (gemChanceAt)
import World.Spoil.Logic (spoilBlockedAt)
import World.Spoil.Types (SpoilPile(..))
import World.Mine.Types (MineDesignation(..))
import World.Tool.Types (ToolMode(..), textToToolMode)
import World.Render.Zoom.Types (ZoomMapMode(..), textToMapMode)
import World.Render.Quads (renderWorldQuads)
import World.Render.Camera.Types (WorldCameraSnapshot(..))
import World.Render.Textures (getTileTexture)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Generate.Config
import World.Plate (defaultPlatesFor)

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
    Lua.pushinteger (fromIntegral (wgcWaterfallQuantum cfg))
    Lua.setfield (Lua.nth 2) "waterfall_quantum"
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
    -- Resources sub-table
    let res = wgcResources cfg
    Lua.newtable
    Lua.pushnumber (Lua.Number (realToFrac (ryOreAbundance res)))
    Lua.setfield (Lua.nth 2) "ore_abundance"
    Lua.pushnumber (Lua.Number (realToFrac (ryIronAbundance res)))
    Lua.setfield (Lua.nth 2) "iron_abundance"
    Lua.pushnumber (Lua.Number (realToFrac (ryCopperAbundance res)))
    Lua.setfield (Lua.nth 2) "copper_abundance"
    Lua.setfield (Lua.nth 2) "resources"
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
    -- Timeline sub-table
    let tl = wgcTimeline cfg
    Lua.newtable
    Lua.pushinteger (fromIntegral (tyEonCount tl))
    Lua.setfield (Lua.nth 2) "eon_count"
    Lua.pushinteger (fromIntegral (tyEraCount tl))
    Lua.setfield (Lua.nth 2) "era_count"
    Lua.pushinteger (fromIntegral (tyPeriodMin tl))
    Lua.setfield (Lua.nth 2) "period_min"
    Lua.pushinteger (fromIntegral (tyPeriodMax tl))
    Lua.setfield (Lua.nth 2) "period_max"
    Lua.pushinteger (fromIntegral (tyEpochMin tl))
    Lua.setfield (Lua.nth 2) "epoch_min"
    Lua.pushinteger (fromIntegral (tyEpochMax tl))
    Lua.setfield (Lua.nth 2) "epoch_max"
    Lua.pushinteger (fromIntegral (tyAgeMin tl))
    Lua.setfield (Lua.nth 2) "age_min"
    Lua.pushinteger (fromIntegral (tyAgeMax tl))
    Lua.setfield (Lua.nth 2) "age_max"
    Lua.setfield (Lua.nth 2) "timeline"
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
        oldRes = wgcResources oldCfg
        oldTl  = wgcTimeline oldCfg

    -- Top-level
    plateCount ← getIntField "plate_count" (wgcPlateCount oldCfg)
    worldSize  ← getIntField "world_size"  (wgcWorldSize oldCfg)
    erosionInt ← getFloatField "erosion_intensity" (wgcErosionIntensity oldCfg)
    volcanicAct ← getFloatField "volcanic_activity" (wgcVolcanicActivity oldCfg)
    waterfallQ ← getIntField "waterfall_quantum" (wgcWaterfallQuantum oldCfg)

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

    -- Resources
    oreAb  ← getSubFloat "resources" "ore_abundance"    (ryOreAbundance oldRes)
    ironAb ← getSubFloat "resources" "iron_abundance"   (ryIronAbundance oldRes)
    copAb  ← getSubFloat "resources" "copper_abundance" (ryCopperAbundance oldRes)

    -- Climate
    iters  ← getSubInt   "climate" "iterations"       (clIterations oldCl)
    corio  ← getSubFloat "climate" "coriolis_scale"   (clCoriolisScale oldCl)
    wdrag  ← getSubFloat "climate" "wind_drag"        (clWindDrag oldCl)
    therm  ← getSubFloat "climate" "thermal_inertia"  (clThermalInertia oldCl)
    orog   ← getSubFloat "climate" "orographic_scale" (clOrographicScale oldCl)
    evap   ← getSubFloat "climate" "evap_scale"       (clEvapScale oldCl)
    albedo ← getSubFloat "climate" "albedo_feedback"  (clAlbedoFeedback oldCl)
    thc    ← getSubFloat "climate" "thc_threshold"    (clThcThreshold oldCl)

    -- Timeline depth
    tlEon  ← getSubInt "timeline" "eon_count"   (tyEonCount oldTl)
    tlEra  ← getSubInt "timeline" "era_count"   (tyEraCount oldTl)
    tlPMin ← getSubInt "timeline" "period_min"  (tyPeriodMin oldTl)
    tlPMax ← getSubInt "timeline" "period_max"  (tyPeriodMax oldTl)
    tlEMin ← getSubInt "timeline" "epoch_min"   (tyEpochMin oldTl)
    tlEMax ← getSubInt "timeline" "epoch_max"   (tyEpochMax oldTl)
    tlAMin ← getSubInt "timeline" "age_min"     (tyAgeMin oldTl)
    tlAMax ← getSubInt "timeline" "age_max"     (tyAgeMax oldTl)

    let newCfg = normalizeWorldGenConfig $ oldCfg
            { wgcWorldSize  = worldSize
            , wgcPlateCount = plateCount
            , wgcErosionIntensity = erosionInt
            , wgcVolcanicActivity = volcanicAct
            , wgcWaterfallQuantum = waterfallQ
            , wgcCalendar   = CalendarYaml dpm mpy hpd mphr
            , wgcSun        = SunYaml tilt dayL
            , wgcMoon       = MoonYaml cyc poff
            , wgcClimate    = ClimateYaml iters corio wdrag therm orog evap albedo thc
            , wgcResources  = ResourcesYaml oreAb ironAb copAb
            , wgcTimeline   = TimelineYaml tlEon tlEra tlPMin tlPMax tlEMin tlEMax tlAMin tlAMax
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
                rawSize = maybe 64 fromIntegral sizeArg
                size = normalizeWorldSize rawSize
                -- Plate count scales with worldSize when caller
                -- doesn't supply one — fixes the "10 plates for any
                -- world" issue (audit #17). Explicit user values
                -- still honored after minimum-count normalization.
                rawPlates = maybe (defaultPlatesFor size) fromIntegral platesArg
                plates = normalizePlateCount rawPlates
            when (size /= rawSize ∨ plates /= rawPlates) $ do
                logger ← readIORef (loggerRef env)
                logWarn logger CatWorld $
                    "world.init normalized worldgen inputs: worldSize "
                    <> T.pack (show rawSize) <> " → "
                    <> T.pack (show size) <> ", plateCount "
                    <> T.pack (show rawPlates) <> " → "
                    <> T.pack (show plates)
                    <> " (worldSize minimum/multiple "
                    <> T.pack (show minimumWorldSize)
                    <> ", plateCount min 1)."
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

-- | world.getTimeScale(pageId) → number
-- Reads the named world's current time scale directly from
-- 'wsTimeScaleRef'. Returns 1.0 if the pageId isn't registered
-- (matches the engine's default scale).
worldGetTimeScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetTimeScaleFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
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

-- | world.selectTile(pageId, gx, gy) — atomically mark the column at
--   (gx, gy) as the world's selected tile, using the chunk's surface
--   z. Unlike setWorldCursorSelect (which races with per-tick mouse
--   hover updates), this is direct: a one-shot selection that doesn't
--   touch the cursor position. Used by the right-click → Info context
--   menu to highlight the right-clicked tile after the mouse has
--   already moved into the menu.
worldSelectTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSelectTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSelectTileByCoord pageId (round gx) (round gy)
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

-- * Mine designation tool

-- | world.setMineAnchor(pageId, gx, gy) — anchor the designation
--   rectangle at the given tile (mine tool first click).
worldSetMineAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMineAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetMineAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | world.clearMineAnchor(pageId) — cancel the pending rectangle.
worldClearMineAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearMineAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldClearMineAnchor pageId
        _ → pure ()
    return 0

-- | world.designateMine(pageId, x1, y1, x2, y2) — commit the
--   rectangle (corners in either order; mine tool second click).
worldDesignateMineFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDesignateMineFn env = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldDesignateMine pageId (round x1) (round y1)
                                          (round x2) (round y2)
        _ → pure ()
    return 0

-- | world.setMineDesignateTexture(pageId, texHandle) — marker texture
--   for committed designations.
worldSetMineDesignateTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMineDesignateTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetMineDesignateTexture pageId texHandle
        _ → pure ()
    return 0

-- | world.nearestMineDesignation(pageId, x, y) → gx, gy, dist | nil
--   Nearest designated tile to (x, y) by Euclidean distance — the
--   "distance to the nearest dig job" term in the dig utility. Linear
--   scan of the designation map (synchronous read).
worldNearestMineDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldNearestMineDesignationFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    let dist2 (gx, gy) =
                            let dx = fromIntegral gx - ux
                                dy = fromIntegral gy - uy
                            in dx * dx + dy * dy
                        best = foldl' (\acc k → case acc of
                                  Nothing → Just (k, dist2 k)
                                  Just (_, d) | dist2 k < d → Just (k, dist2 k)
                                  _ → acc)
                                Nothing (HM.keys m)
                    case best of
                        Just ((gx, gy), d2) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            return 3
                        Nothing → do
                            Lua.pushnil
                            return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.getDigInfoAt(pageId, gx, gy)
--     → matId, pickSpeed, shovelSpeed, spoilBlocked | nil
--   Material being dug at a designated tile (the column's material at
--   the designation's z) and its per-tool dig-rate multipliers. nil
--   when the tile isn't designated or its chunk isn't loaded.
--   spoilBlocked is true when the material produces spoil and the
--   surrounding piles have no room — the dig command will refuse, so
--   the AI should skip the tile instead of digging in place forever.
worldGetDigInfoAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetDigInfoAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                gx = round gxN ∷ Int
                gy = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    desigs ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    tileData ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
                    piles ← Lua.liftIO $ readIORef (wsSpoilRef ws)
                    let mInfo = do
                            md ← HM.lookup (gx, gy) desigs
                            let (coord, (lx, ly)) = globalToChunk gx gy
                            lc ← lookupChunk coord tileData
                            let col = lcTiles lc V.! columnIndex lx ly
                                relZ = mdZ md - ctStartZ col
                            if relZ ≥ 0 ∧ relZ < VU.length (ctMats col)
                                then pure (ctMats col VU.! relZ, mdZ md)
                                else Nothing
                    case mInfo of
                        Nothing → Lua.pushnil >> return 1
                        Just (matId, digZ) → do
                            let props = getMaterialProps registry
                                            (MaterialId matId)
                                -- Blocked check from the tile center;
                                -- the per-tick gate re-checks with the
                                -- digger's real position.
                                blocked = case mpDigSpoil props of
                                    Nothing → False
                                    Just spoilName →
                                        case materialIdByName registry
                                                 spoilName of
                                            Nothing → False
                                            Just spoilId →
                                                spoilBlockedAt tileData
                                                    desigs piles spoilId
                                                    digZ
                                                    ( fromIntegral gx + 0.5
                                                    , fromIntegral gy + 0.5 )
                                                    (gx, gy)
                            Lua.pushinteger (fromIntegral matId)
                            Lua.pushnumber (Lua.Number
                                (realToFrac (mpPickSpeed props)))
                            Lua.pushnumber (Lua.Number
                                (realToFrac (mpShovelSpeed props)))
                            Lua.pushboolean blocked
                            return 4

-- | world.addTile(pageId, gx, gy, material) → bool
--   Raise the column at (gx, gy) one z of the named material (string
--   name or numeric id). Queued onto the world thread; lands in the
--   edit log via WeAddTile, so it persists like any player edit.
--   Debug terrain placement. Returns false when the material can't
--   be resolved.
worldAddTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldAddTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    -- 4: material name (string) or id (number).
    matName ← Lua.tostring 4
    matNum  ← Lua.tonumber 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
            let mMat = case (matNum, matName) of
                    (Just (Lua.Number n), _) | n ≥ 1 ∧ n ≤ 255 →
                        Just (MaterialId (round n))
                    (_, Just nameBS) →
                        materialIdByName registry
                            (TE.decodeUtf8 nameBS)
                    _ → Nothing
            case mMat of
                Nothing → Lua.pushboolean False >> return 1
                Just mat → do
                    let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                    Lua.liftIO $ Q.writeQueue (worldQueue env) $
                        WorldAddTile pageId (round gx) (round gy) mat
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | world.listMaterials() → array of {id, name}
--   Every registered (non-default) material, sorted by id. Drives the
--   debug overlay's Terrain placement list.
worldListMaterialsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldListMaterialsFn env = do
    registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
    let mats = [ (i, mpName props)
               | i ← [1 ∷ Int .. 255]
               , let props = getMaterialProps registry
                                 (MaterialId (fromIntegral i))
               , mpName props ≠ "unknown" ]
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] mats) $ \(n, (i, name)) → do
        Lua.newtable
        Lua.pushinteger (fromIntegral i)
        Lua.setfield (Lua.nth 2) "id"
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (Lua.nth 2) "name"
        Lua.rawseti (Lua.nth 2) (fromIntegral n)
    return 1

-- | world.debugTileQuads(pageId, matId) → {total, matQuads, sample}
--   Introspection: run the REAL chunk-quad pass with the current
--   camera and report how many quads sample the given material's
--   tile texture (plus the first match's rect). Splits "the cell
--   isn't producing a quad" from "the quad reaches the GPU and the
--   shader/sort hides it" without a GUI debugger — same idea as
--   item.debugQuads.
worldDebugTileQuadsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDebugTileQuadsFn env = do
    pageIdArg ← Lua.tostring 1
    matArg ← Lua.tonumber 2
    case (pageIdArg, matArg) of
        (Just pageIdBS, Just (Lua.Number matN)) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    (quads, texH) ← Lua.liftIO $ do
                        camera ← readIORef (cameraRef env)
                        (fbW, fbH) ← readIORef (framebufferSizeRef env)
                        textures ← readIORef (wsTexturesRef ws)
                        let snap = WorldCameraSnapshot
                                { wcsPosition = camPosition camera
                                , wcsZoom     = camZoom camera
                                , wcsZSlice   = camZSlice camera
                                , wcsFbSize   = (fbW, fbH)
                                , wcsFacing   = camFacing camera
                                }
                        qs ← renderWorldQuads env ws 1.0 snap
                        pure (qs, getTileTexture textures
                                      (round matN ∷ Word8))
                    let matches = V.filter (\q → sqTexture q ≡ texH) quads
                    Lua.newtable
                    Lua.pushinteger (fromIntegral (V.length quads))
                    Lua.setfield (Lua.nth 2) "total"
                    Lua.pushinteger (fromIntegral (V.length matches))
                    Lua.setfield (Lua.nth 2) "matQuads"
                    case matches V.!? 0 of
                        Nothing → pure ()
                        Just q → do
                            let Vec2 qx qy = pos (sqV0 q)
                            Lua.newtable
                            Lua.pushnumber (Lua.Number (realToFrac qx))
                            Lua.setfield (Lua.nth 2) "x"
                            Lua.pushnumber (Lua.Number (realToFrac qy))
                            Lua.setfield (Lua.nth 2) "y"
                            Lua.pushnumber (Lua.Number
                                (realToFrac (sqSortKey q)))
                            Lua.setfield (Lua.nth 2) "sort"
                            Lua.setfield (Lua.nth 2) "sample"
                    return 1
        _ → Lua.pushnil >> return 1

-- | world.getGemInfoAt(pageId, gx, gy) → {gem, chance} | nil
--   What the seeded gem region field says about a tile: the gem the
--   region hosts and the per-completed-tile find chance at
--   perception 1.0. nil for barren regions. Debug/validation (and a
--   future prospecting skill could surface this in-game).
worldGetGemInfoAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetGemInfoAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    paramsM ← Lua.liftIO $ readIORef (wsGenParamsRef ws)
                    let seed = maybe 0 (fromIntegral ∘ wgpSeed) paramsM
                    case gemChanceAt seed (round gxN, round gyN) 1.0 of
                        Nothing → Lua.pushnil >> return 1
                        Just (gem, chance) → do
                            Lua.newtable
                            Lua.pushstring (TE.encodeUtf8 gem)
                            Lua.setfield (Lua.nth 2) "gem"
                            Lua.pushnumber (Lua.Number
                                (realToFrac chance))
                            Lua.setfield (Lua.nth 2) "chance"
                            return 1
        _ → Lua.pushnil >> return 1

-- | world.getSpoilInfo(pageId) → {piles, totalFill} | nil
--   Debug/validation view of the spoil overlay: pile (vertex) count
--   and the summed fractional fill in corner-units. Promoted cells
--   are NOT in here (they're real terrain via WeAddTile) — the G4
--   conservation harness combines this with terrain deltas.
worldGetSpoilInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSpoilInfoFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Nothing → Lua.pushnil >> return 1
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    piles ← Lua.liftIO $ readIORef (wsSpoilRef ws)
                    let sumC (a, b, c, d) = a + b + c + d
                        total = sum [ sumC (spFill p)
                                    | p ← HM.elems piles ]
                    Lua.newtable
                    Lua.pushinteger (fromIntegral (HM.size piles))
                    Lua.setfield (Lua.nth 2) "piles"
                    Lua.pushnumber (Lua.Number (realToFrac total))
                    Lua.setfield (Lua.nth 2) "totalFill"
                    return 1

-- | world.digTile(pageId, gx, gy, ux, uy, amount, minerSkill,
--   perception) —
--   apply dig progress to the designated tile. (ux, uy) is the
--   digger's tile-space position (drain order); amount is pre-scaled
--   by tool × material speed (see getDigInfoAt). minerSkill (the
--   current digger's mining skill; optional, defaults 0) scales the
--   per-tick chunk-yield fill — pass it every tick so a mid-dig
--   handoff uses the new digger's rate.
worldDigTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDigTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    uxArg ← Lua.tonumber 4
    uyArg ← Lua.tonumber 5
    amtArg ← Lua.tonumber 6
    skillArg ← Lua.tonumber 7
    percepArg ← Lua.tonumber 8
    case (pageIdArg, gxArg, gyArg, uxArg, uyArg, amtArg) of
        (Just pageIdBS, Just gx, Just gy, Just ux, Just uy, Just amt) →
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                    skill = case skillArg of
                        Just (Lua.Number s) → realToFrac s
                        _                   → 0
                    percep = case percepArg of
                        Just (Lua.Number s) → realToFrac s
                        _                   → 1.0
                Q.writeQueue (worldQueue env) $
                    WorldDigTile pageId (round gx) (round gy)
                                 (realToFrac ux) (realToFrac uy)
                                 (realToFrac amt) skill percep
        _ → pure ()
    return 0

-- | world.getMineDesignationAt(pageId, gx, gy)
--     → z, cNW, cNE, cSE, cSW | nil
--   Designation state at a tile, including corner dig progress (the
--   AI's "how far along is this tile" query).
worldGetMineDesignationAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMineDesignationAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    case HM.lookup (round gxN, round gyN) m of
                        Nothing → Lua.pushnil >> return 1
                        Just md → do
                            let (a, b, c, d) = mdCorners md
                            Lua.pushinteger (fromIntegral (mdZ md))
                            Lua.pushnumber (Lua.Number (realToFrac a))
                            Lua.pushnumber (Lua.Number (realToFrac b))
                            Lua.pushnumber (Lua.Number (realToFrac c))
                            Lua.pushnumber (Lua.Number (realToFrac d))
                            return 5
        _ → Lua.pushnil >> return 1

-- | world.getMineDesignationCount(pageId) → n — number of designated
--   tiles. Reads the ref directly (synchronous; for HUD readouts and
--   headless tests).
worldGetMineDesignationCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMineDesignationCountFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → do
                    Lua.pushinteger 0
                    return 1
        _ → do
            Lua.pushinteger 0
            return 1

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

-- | world.getToolMode() → "info" | "default" | "mine" | nil
--   Returns the current tool mode for the first visible world, or nil
--   if no world is active. Reads `wsToolModeRef` directly so callers
--   see the world thread's view of the tool state.
worldGetToolModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetToolModeFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just ws → do
            tm ← Lua.liftIO $ readIORef (wsToolModeRef ws)
            let s = case tm of
                    InfoTool    → "info"
                    DefaultTool → "default"
                    MineTool    → "mine"
            Lua.pushstring s
            return 1
        Nothing → do
            Lua.pushnil
            return 1

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
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just worldState → do
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
        Nothing → do
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
        mWs ← activeWorldState env
        case mWs of
            Just ws → do
                phase ← readIORef (wsLoadPhaseRef ws)
                case phase of
                    LoadDone → return ()
                    _        → do
                        threadDelay 250000
                        waitLoop (n - 1)
            Nothing → do
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

-- | world.deleteTile(pageId, gx, gy) → bool
-- Enqueues a dig-1-Z-down edit at the given tile. The actual mutation
-- happens on the next world-thread tick, so this returns true once
-- enqueued (not once applied).
worldDeleteTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDeleteTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                Q.writeQueue (worldQueue env)
                    (WorldDeleteTile pageId (fromIntegral gx) (fromIntegral gy))
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setFluidTile(pageId, gx, gy, kind) → bool
-- Places one tile of fluid on top of the column at (gx, gy). `kind` is
-- one of "water" (Lake) / "lava" (Lava) / "river" (River) / "ocean"
-- (Ocean); unknown values fall back to "water". Debug-tool affordance:
-- lets the arena have water sources without waiting for procedural
-- generation.
worldSetFluidTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetFluidTileFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    kindArg   ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            let fluidType = case kindArg of
                    Just kBS → case TE.decodeUtf8 kBS of
                        "lava"  → Lava
                        "river" → River
                        "ocean" → Ocean
                        _       → Lake     -- "water" / default
                    Nothing → Lake
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                Q.writeQueue (worldQueue env) $
                    WorldSetFluidTile pageId
                        (fromIntegral gx) (fromIntegral gy) fluidType
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setCell(pageId, gx, gy, z, material) → bool
--   Set the single 3D cell at (gx,gy,z) to a material — the locations
--   primitive for carving interior air, walls, ceilings, staircases.
--   `material` is a string name, a numeric id, or "air"/0 to clear the
--   cell. Queued onto the world thread; lands in the edit log via
--   WeSetCell so it persists like any player edit. Grows the column up
--   to reach z; a z below the column floor is dropped (warns). Returns
--   false when the material can't be resolved.
worldSetCellFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCellFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    -- 5: material name (string), numeric id, or "air"/0 for air.
    matName ← Lua.tostring 5
    matNum  ← Lua.tonumber 5
    case (pageIdArg, gxArg, gyArg, zArg) of
        (Just pageIdBS, Just gx, Just gy, Just z) → do
            registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
            let mMat = case (matNum, matName) of
                    (Just (Lua.Number n), _) | n ≥ 0 ∧ n ≤ 255 →
                        Just (MaterialId (round n))
                    (_, Just nameBS) → case TE.decodeUtf8 nameBS of
                        "air" → Just (MaterialId 0)
                        name  → materialIdByName registry name
                    _ → Nothing
            case mMat of
                Nothing → Lua.pushboolean False >> return 1
                Just mat → do
                    let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                    Lua.liftIO $ Q.writeQueue (worldQueue env) $
                        WorldSetCell pageId
                            (fromIntegral gx) (fromIntegral gy)
                            (fromIntegral z) mat
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | world.setSlope(pageId, gx, gy, z, bits) → bool
--   Set the walkable-ramp slope bitmask of the tile at (gx,gy,z).
--   Bits: 0=N 1=E 2=S 3=W; a set bit marks that cardinal neighbour as a
--   1-z ramp down (so a unit can walk up it instead of climbing). addTile
--   only ever makes flat tops (slope 0 = cliff), so this is the only way
--   to author a walkable ramp — exists for the movement test harness.
worldSetSlopeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSlopeFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    bitsArg   ← Lua.tointeger 5
    case (pageIdArg, gxArg, gyArg, zArg, bitsArg) of
        (Just pageIdBS, Just gx, Just gy, Just z, Just bits) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                Q.writeQueue (worldQueue env) $
                    WorldSetSlope pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (fromIntegral z)
                        (fromIntegral bits)  -- → Word8 truncates to low 8 bits
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1
