{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.GenConfig
    ( worldGetGenDefaultsFn
    , worldSetGenConfigFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import World.Generate.Config

-- | world.getGenDefaults() → table
--   Returns the world generation config as a Lua table.
worldGetGenDefaultsFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetGenDefaultsFn wsc = do
    cfg ← Lua.liftIO $ readIORef (wsWorldGenConfigRef wsc)
    Lua.newtable
    -- Top-level params
    case wgcSeed cfg of
        Just s  → do Lua.pushstring (TE.encodeUtf8 (tshow s))
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

-- | @world.setGenConfig(table)@ → @true@ | @false, diagnostic@
--
--   Updates the world generation config from a Lua table. Only fields
--   PRESENT in the table are updated; an absent field inherits the
--   current value, exactly as it always did.
--
--   #2288 gave the verb a return contract and a domain. Every
--   floating-point setting is narrowed to its stored 'Float' and judged
--   against "World.Generate.Config.Domain" — the same domain the YAML
--   loader applies — and ONE out-of-domain field refuses the WHOLE
--   update: nothing is written, and the call answers @false@ plus a
--   diagnostic naming the field and the rejected value. An accepted
--   update answers @true@.
--
--   Two shapes of bad floating input are distinguished:
--
--   * A field ABSENT from the table (or from an absent sub-table)
--     inherits the current configuration's value. That is the verb's
--     partial-update contract and is not an error.
--   * A field PRESENT but not coercible to a number refuses the update.
--     It cannot mean "inherit": the caller wrote something there. The
--     helper used to fold both cases into the default, so a typo
--     silently generated a different world.
--
--   Numeric STRINGS stay accepted, deliberately unlike
--   @world.setTimeScale@'s stricter argument check (#2280): this table
--   is assembled from create-world text boxes, and @tonumber@ has
--   always been what reads them.
--
--   The INTEGER settings are untouched by all of this (#2288 is scoped
--   to the floating-point ones): they keep the read they always had, in
--   which a present but uncoercible value falls back to the current
--   configuration rather than refusing.
worldSetGenConfigFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetGenConfigFn wsc = do
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
        getFloatField ∷ Lua.Name → Text → Float
                      → Lua.LuaE Lua.Exception (Either Text Float)
        getFloatField name field def = do
            ty ← Lua.getfield (Lua.nth 1) name
            r ← readFloat ty field def
            Lua.pop 1
            pure r
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
        getSubFloat ∷ Lua.Name → Lua.Name → Text → Float
                    → Lua.LuaE Lua.Exception (Either Text Float)
        getSubFloat tbl name field def = do
            _ ← Lua.getfield (Lua.nth 1) tbl
            isT ← Lua.istable Lua.top
            if isT
                then do
                    ty ← Lua.getfield (Lua.nth 1) name
                    r ← readFloat ty field def
                    Lua.pop 2
                    pure r
                else do
                    Lua.pop 1
                    pure (Right def)

    oldCfg ← Lua.liftIO $ readIORef (wsWorldGenConfigRef wsc)
    let oldCal = wgcCalendar oldCfg
        oldSun = wgcSun oldCfg
        oldMoon = wgcMoon oldCfg
        oldCl  = wgcClimate oldCfg
        oldRes = wgcResources oldCfg
        oldTl  = wgcTimeline oldCfg

    -- Top-level
    plateCount ← getIntField "plate_count" (wgcPlateCount oldCfg)
    worldSize  ← getIntField "world_size"  (wgcWorldSize oldCfg)
    erosionInt ← getFloatField "erosion_intensity" fieldErosionIntensity
                               (wgcErosionIntensity oldCfg)
    volcanicAct ← getFloatField "volcanic_activity" fieldVolcanicActivity
                                (wgcVolcanicActivity oldCfg)
    waterfallQ ← getIntField "waterfall_quantum" (wgcWaterfallQuantum oldCfg)

    -- Calendar
    dpm  ← getSubInt "calendar" "days_per_month"   (cyDaysPerMonth oldCal)
    mpy  ← getSubInt "calendar" "months_per_year"  (cyMonthsPerYear oldCal)
    hpd  ← getSubInt "calendar" "hours_per_day"    (cyHoursPerDay oldCal)
    mphr ← getSubInt "calendar" "minutes_per_hour" (cyMinutesPerHour oldCal)

    -- Sun
    tilt ← getSubFloat "sun" "tilt_angle" fieldTiltAngle (syTiltAngle oldSun)
    dayL ← getSubFloat "sun" "day_length" fieldDayLength (syDayLength oldSun)

    -- Moon
    cyc  ← getSubInt   "moon" "cycle_days" (myCycleDays oldMoon)
    poff ← getSubFloat "moon" "phase_offset" fieldPhaseOffset
                       (myPhaseOffset oldMoon)

    -- Resources
    oreAb  ← getSubFloat "resources" "ore_abundance" fieldOreAbundance
                         (ryOreAbundance oldRes)
    ironAb ← getSubFloat "resources" "iron_abundance" fieldIronAbundance
                         (ryIronAbundance oldRes)
    copAb  ← getSubFloat "resources" "copper_abundance" fieldCopperAbundance
                         (ryCopperAbundance oldRes)

    -- Climate
    iters  ← getSubInt   "climate" "iterations" (clIterations oldCl)
    corio  ← getSubFloat "climate" "coriolis_scale" fieldCoriolisScale
                         (clCoriolisScale oldCl)
    wdrag  ← getSubFloat "climate" "wind_drag" fieldWindDrag
                         (clWindDrag oldCl)
    therm  ← getSubFloat "climate" "thermal_inertia" fieldThermalInertia
                         (clThermalInertia oldCl)
    orog   ← getSubFloat "climate" "orographic_scale" fieldOrographicScale
                         (clOrographicScale oldCl)
    evap   ← getSubFloat "climate" "evap_scale" fieldEvapScale
                         (clEvapScale oldCl)
    albedo ← getSubFloat "climate" "albedo_feedback" fieldAlbedoFeedback
                         (clAlbedoFeedback oldCl)
    thc    ← getSubFloat "climate" "thc_threshold" fieldThcThreshold
                         (clThcThreshold oldCl)

    -- Timeline depth
    tlEon  ← getSubInt "timeline" "eon_count"   (tyEonCount oldTl)
    tlEra  ← getSubInt "timeline" "era_count"   (tyEraCount oldTl)
    tlPMin ← getSubInt "timeline" "period_min"  (tyPeriodMin oldTl)
    tlPMax ← getSubInt "timeline" "period_max"  (tyPeriodMax oldTl)
    tlEMin ← getSubInt "timeline" "epoch_min"   (tyEpochMin oldTl)
    tlEMax ← getSubInt "timeline" "epoch_max"   (tyEpochMax oldTl)
    tlAMin ← getSubInt "timeline" "age_min"     (tyAgeMin oldTl)
    tlAMax ← getSubInt "timeline" "age_max"     (tyAgeMax oldTl)

    -- Assembly in 'Either', so the FIRST unreadable float short-circuits
    -- before a candidate configuration exists at all. Every read above
    -- has already happened; this only decides what to do with them.
    let assembled = do
            ei ← erosionInt
            va ← volcanicAct
            ta ← tilt
            dl ← dayL
            po ← poff
            oa ← oreAb
            ia ← ironAb
            ca ← copAb
            cs ← corio
            wd ← wdrag
            ti ← therm
            os ← orog
            es ← evap
            af ← albedo
            tt ← thc
            pure $ normalizeWorldGenConfig $ oldCfg
                { wgcWorldSize  = worldSize
                , wgcPlateCount = plateCount
                , wgcErosionIntensity = ei
                , wgcVolcanicActivity = va
                , wgcWaterfallQuantum = waterfallQ
                , wgcCalendar   = CalendarYaml dpm mpy hpd mphr
                , wgcSun        = SunYaml ta dl
                , wgcMoon       = MoonYaml cyc po
                , wgcClimate    = ClimateYaml iters cs wd ti os es af tt
                , wgcResources  = ResourcesYaml oa ia ca
                , wgcTimeline   = TimelineYaml tlEon tlEra tlPMin tlPMax
                                               tlEMin tlEMax tlAMin tlAMax
                }
    case assembled of
        Left diagnostic → refuseGenConfig diagnostic
        Right candidate → case worldGenConfigRejections candidate of
            (r : _) → refuseGenConfig (describeWorldGenRejection r)
            []      → do
                Lua.liftIO $ writeIORef (wsWorldGenConfigRef wsc) candidate
                Lua.pushboolean True
                return 1

-- | Read the float at the top of the stack, given the type
--   'Lua.getfield' just reported.
--
--   Narrowed with 'narrowWorldGenFloat' rather than @realToFrac@, so
--   the domain check downstream judges exactly the 'Float' that would be
--   stored — including the infinity a finite Lua number such as @1e40@
--   becomes on the way in, which is the reported exploit.
readFloat ∷ Lua.Type → Text → Float
          → Lua.LuaE Lua.Exception (Either Text Float)
readFloat Lua.TypeNil _ def = pure (Right def)
readFloat ty field _ = do
    mn ← Lua.tonumber Lua.top
    case mn of
        Just (Lua.Number n) → pure (Right (narrowWorldGenFloat n))
        Nothing → do
            tyName ← TE.decodeUtf8Lenient ⊚ Lua.typename ty
            pure (Left (field <> " must be a number, got " <> tyName))

-- | The refusal half of 'worldSetGenConfigFn''s return contract: two
--   results, @false@ and the diagnostic, and no side effect whatsoever.
--   The stored configuration is exactly what it was.
refuseGenConfig ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
refuseGenConfig reason = do
    Lua.pushboolean False
    Lua.pushstring (TE.encodeUtf8
        (reason <> "; the world generation configuration is left unchanged."))
    return 2
