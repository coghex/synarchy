{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World.GenConfig
    ( worldGetGenDefaultsFn
    , worldSetGenConfigFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
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
    -- The config table starts on top of the Lua stack. hslua's 'Lua.nth N'
    -- is top-relative, so 'Lua.nth 1' means "current top", not "argument 1".
    -- The nested helpers below rely on that: once getfield pushes a subtable,
    -- another 'Lua.nth 1' addresses the pushed subtable.
    let getIntField ∷ Lua.Name → Int → Lua.LuaE Lua.Exception Int
        getIntField name def = do
            _ ← Lua.getfield (Lua.nth 1) name
            mi ← Lua.tointeger Lua.top
            Lua.pop 1
            pure $ maybe def fromIntegral mi
        getFloatField ∷ Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getFloatField name def = do
            _ ← Lua.getfield (Lua.nth 1) name
            mn ← Lua.tonumber Lua.top
            Lua.pop 1
            pure $ case mn of
                Just (Lua.Number n) → realToFrac n
                _                   → def
        getSubInt ∷ Lua.Name → Lua.Name → Int → Lua.LuaE Lua.Exception Int
        getSubInt tbl name def = do
            _ ← Lua.getfield (Lua.nth 1) tbl
            isT ← Lua.istable Lua.top
            if isT
                then do
                    _ ← Lua.getfield (Lua.nth 1) name
                    mi ← Lua.tointeger Lua.top
                    Lua.pop 2
                    pure $ maybe def fromIntegral mi
                else do
                    Lua.pop 1
                    pure def
        getSubFloat ∷ Lua.Name → Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getSubFloat tbl name def = do
            _ ← Lua.getfield (Lua.nth 1) tbl
            isT ← Lua.istable Lua.top
            if isT
                then do
                    _ ← Lua.getfield (Lua.nth 1) name
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
