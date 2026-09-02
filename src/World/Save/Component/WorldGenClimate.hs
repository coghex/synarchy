{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Worldgen generation and climate LEAF DTOs (issue #2098 owner split of
--   the #760 frozen worldgen graph).
--
--   This owner holds the small generation/configuration mirrors
--   ('TectonicPlateDTO', 'CalendarConfigDTO', 'SunConfigDTO',
--   'MoonConfigDTO', 'ClimateParamsDTO', 'TimelineParamsDTO',
--   'OreLeversDTO') and the complete recursively frozen 'ClimateState'
--   tree. Nothing here depends on any other worldgen owner, which is what
--   makes it the bottom of the owner order
--   (climate ← names/locations ← current ← historical).
--
--   The frozen-DTO boundary rule these declarations implement is stated
--   ONCE, in "World.Save.Component.Types"; the worldgen-specific reading
--   of it — what is frozen here and what is deliberately reused as a leaf
--   — stays in the façade "World.Save.Component.WorldGen".
--
--   Every declaration below is a positional cereal wire contract: field
--   order, field type, strictness and the 'Serialize' derivation are the
--   stored bytes. Moving one between owners changes ownership only.
module World.Save.Component.WorldGenClimate
    ( TectonicPlateDTO(..)
    , CalendarConfigDTO(..)
    , SunConfigDTO(..)
    , MoonConfigDTO(..)
    , ClimateParamsDTO(..)
    , TimelineParamsDTO(..)
    , OreLeversDTO(..)
    , SeasonalClimateDTO(..)
    , RegionClimateDTO(..)
    , ClimateGridDTO(..)
    , OceanCellDTO(..)
    , WaterMassDTO(..)
    , OceanCurrentDTO(..)
    , ThermohalineCellDTO(..)
    , OceanGridDTO(..)
    , WindCellDTO(..)
    , MoistureCellDTO(..)
    , PressureSystemDTO(..)
    , AtmoGridDTO(..)
    , SurfaceBudgetDTO(..)
    , ClimateStateDTO(..)
    , toTectonicPlateDTO
    , fromTectonicPlateDTO
    , toCalendarConfigDTO
    , fromCalendarConfigDTO
    , toSunConfigDTO
    , fromSunConfigDTO
    , toMoonConfigDTO
    , fromMoonConfigDTO
    , toClimateParamsDTO
    , fromClimateParamsDTO
    , toTimelineParamsDTO
    , fromTimelineParamsDTO
    , toOreLeversDTO
    , fromOreLeversDTO
    , toSeasonalClimateDTO
    , fromSeasonalClimateDTO
    , toRegionClimateDTO
    , fromRegionClimateDTO
    , toClimateGridDTO
    , fromClimateGridDTO
    , toOceanCellDTO
    , fromOceanCellDTO
    , toWaterMassDTO
    , fromWaterMassDTO
    , toOceanCurrentDTO
    , fromOceanCurrentDTO
    , toThermohalineCellDTO
    , fromThermohalineCellDTO
    , toOceanGridDTO
    , fromOceanGridDTO
    , toWindCellDTO
    , fromWindCellDTO
    , toMoistureCellDTO
    , fromMoistureCellDTO
    , toPressureSystemDTO
    , fromPressureSystemDTO
    , toAtmoGridDTO
    , fromAtmoGridDTO
    , toSurfaceBudgetDTO
    , fromSurfaceBudgetDTO
    , toClimateStateDTO
    , fromClimateStateDTO
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Plate.Types (TectonicPlate(..))
import World.Material.Id (MaterialId)
import World.Time.Types
    (CalendarConfig(..), SunConfig(..), MoonConfig(..))
import World.Geology.Timeline.Types (TimelineParams(..))
import World.Weather.Types
    ( ClimateParams(..), ClimateState(..), ClimateGrid(..), ClimateCoord
    , RegionClimate(..), SeasonalClimate(..), OceanGrid(..), OceanCell(..)
    , WaterMass(..), OceanCurrent(..), ThermohalineCell(..), AtmoGrid(..)
    , WindCell(..), MoistureCell(..), PressureSystem(..), PressureType
    , SurfaceType, SurfaceBudget(..) )
import World.Geology.Ore.Types (OreLevers(..))

-- Small config / param records --------------------------------------

-- | Frozen mirror of 'TectonicPlate'. 'MaterialId' is a leaf id.
data TectonicPlateDTO = TectonicPlateDTO
    { pltCenterX  ∷ !Int
    , pltCenterY  ∷ !Int
    , pltIsLand   ∷ !Bool
    , pltBaseElev ∷ !Int
    , pltMaterial ∷ !MaterialId
    , pltDensity  ∷ !Float
    , pltDriftX   ∷ !Float
    , pltDriftY   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toTectonicPlateDTO ∷ TectonicPlate → TectonicPlateDTO
toTectonicPlateDTO p = TectonicPlateDTO
    { pltCenterX  = plateCenterX p, pltCenterY  = plateCenterY p
    , pltIsLand   = plateIsLand p,  pltBaseElev = plateBaseElev p
    , pltMaterial = plateMaterial p, pltDensity = plateDensity p
    , pltDriftX   = plateDriftX p,  pltDriftY   = plateDriftY p }

fromTectonicPlateDTO ∷ TectonicPlateDTO → TectonicPlate
fromTectonicPlateDTO d = TectonicPlate
    { plateCenterX  = pltCenterX d, plateCenterY  = pltCenterY d
    , plateIsLand   = pltIsLand d,  plateBaseElev = pltBaseElev d
    , plateMaterial = pltMaterial d, plateDensity = pltDensity d
    , plateDriftX   = pltDriftX d,  plateDriftY   = pltDriftY d }

-- | Frozen mirror of 'CalendarConfig'.
data CalendarConfigDTO = CalendarConfigDTO
    { calDaysPerMonth   ∷ !Int
    , calMonthsPerYear  ∷ !Int
    , calHoursPerDay    ∷ !Int
    , calMinutesPerHour ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toCalendarConfigDTO ∷ CalendarConfig → CalendarConfigDTO
toCalendarConfigDTO c = CalendarConfigDTO
    (ccDaysPerMonth c) (ccMonthsPerYear c) (ccHoursPerDay c) (ccMinutesPerHour c)

fromCalendarConfigDTO ∷ CalendarConfigDTO → CalendarConfig
fromCalendarConfigDTO d = CalendarConfig
    (calDaysPerMonth d) (calMonthsPerYear d) (calHoursPerDay d) (calMinutesPerHour d)

-- | Frozen mirror of 'SunConfig'.
data SunConfigDTO = SunConfigDTO
    { sunTiltAngle ∷ !Float
    , sunDayLength ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toSunConfigDTO ∷ SunConfig → SunConfigDTO
toSunConfigDTO c = SunConfigDTO (scTiltAngle c) (scDayLength c)

fromSunConfigDTO ∷ SunConfigDTO → SunConfig
fromSunConfigDTO d = SunConfig (sunTiltAngle d) (sunDayLength d)

-- | Frozen mirror of 'MoonConfig'.
data MoonConfigDTO = MoonConfigDTO
    { moonCycleDays   ∷ !Int
    , moonPhaseOffset ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toMoonConfigDTO ∷ MoonConfig → MoonConfigDTO
toMoonConfigDTO c = MoonConfigDTO (mcCycleDays c) (mcPhaseOffset c)

fromMoonConfigDTO ∷ MoonConfigDTO → MoonConfig
fromMoonConfigDTO d = MoonConfig (moonCycleDays d) (moonPhaseOffset d)

-- | Frozen mirror of 'ClimateParams'.
data ClimateParamsDTO = ClimateParamsDTO
    { cpmIterations      ∷ !Int
    , cpmCoriolisScale   ∷ !Float
    , cpmWindDrag        ∷ !Float
    , cpmThermalInertia  ∷ !Float
    , cpmOrographicScale ∷ !Float
    , cpmEvapScale       ∷ !Float
    , cpmAlbedoFeedback  ∷ !Float
    , cpmThcThreshold    ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toClimateParamsDTO ∷ ClimateParams → ClimateParamsDTO
toClimateParamsDTO c = ClimateParamsDTO
    (cpIterations c) (cpCoriolisScale c) (cpWindDrag c) (cpThermalInertia c)
    (cpOrographicScale c) (cpEvapScale c) (cpAlbedoFeedback c) (cpThcThreshold c)

fromClimateParamsDTO ∷ ClimateParamsDTO → ClimateParams
fromClimateParamsDTO d = ClimateParams
    (cpmIterations d) (cpmCoriolisScale d) (cpmWindDrag d) (cpmThermalInertia d)
    (cpmOrographicScale d) (cpmEvapScale d) (cpmAlbedoFeedback d) (cpmThcThreshold d)

-- | Frozen mirror of 'TimelineParams'.
data TimelineParamsDTO = TimelineParamsDTO
    { tpmEonCount  ∷ !Int
    , tpmEraCount  ∷ !Int
    , tpmPeriodMin ∷ !Int
    , tpmPeriodMax ∷ !Int
    , tpmEpochMin  ∷ !Int
    , tpmEpochMax  ∷ !Int
    , tpmAgeMin    ∷ !Int
    , tpmAgeMax    ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toTimelineParamsDTO ∷ TimelineParams → TimelineParamsDTO
toTimelineParamsDTO t = TimelineParamsDTO
    (tlpEonCount t) (tlpEraCount t) (tlpPeriodMin t) (tlpPeriodMax t)
    (tlpEpochMin t) (tlpEpochMax t) (tlpAgeMin t) (tlpAgeMax t)

fromTimelineParamsDTO ∷ TimelineParamsDTO → TimelineParams
fromTimelineParamsDTO d = TimelineParams
    (tpmEonCount d) (tpmEraCount d) (tpmPeriodMin d) (tpmPeriodMax d)
    (tpmEpochMin d) (tpmEpochMax d) (tpmAgeMin d) (tpmAgeMax d)

-- | Frozen mirror of 'OreLevers'.
data OreLeversDTO = OreLeversDTO
    { olvGlobal ∷ !Float
    , olvIron   ∷ !Float
    , olvCopper ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toOreLeversDTO ∷ OreLevers → OreLeversDTO
toOreLeversDTO o = OreLeversDTO (olGlobal o) (olIron o) (olCopper o)

fromOreLeversDTO ∷ OreLeversDTO → OreLevers
fromOreLeversDTO d = OreLevers (olvGlobal d) (olvIron d) (olvCopper d)

-- Climate-state tree ------------------------------------------------

-- | Frozen mirror of 'SeasonalClimate'.
data SeasonalClimateDTO = SeasonalClimateDTO
    { seaSummer ∷ !Float
    , seaWinter ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toSeasonalClimateDTO ∷ SeasonalClimate → SeasonalClimateDTO
toSeasonalClimateDTO s = SeasonalClimateDTO (scSummer s) (scWinter s)

fromSeasonalClimateDTO ∷ SeasonalClimateDTO → SeasonalClimate
fromSeasonalClimateDTO d = SeasonalClimate (seaSummer d) (seaWinter d)

-- | Frozen mirror of 'RegionClimate'. Its 'SeasonalClimate' fields recurse
--   through 'SeasonalClimateDTO'.
data RegionClimateDTO = RegionClimateDTO
    { rgcAirTemp        ∷ !SeasonalClimateDTO
    , rgcHumidity       ∷ !Float
    , rgcPrecipitation  ∷ !SeasonalClimateDTO
    , rgcPrecipType     ∷ !Float
    , rgcEvaporation    ∷ !Float
    , rgcCloudCover     ∷ !Float
    , rgcPressure       ∷ !Float
    , rgcWindDir        ∷ !Float
    , rgcWindSpeed      ∷ !Float
    , rgcOrographicLift ∷ !Float
    , rgcContinentality ∷ !Float
    , rgcAlbedo         ∷ !Float
    , rgcElevAvg        ∷ !Int
    , rgcWaterTable     ∷ !SeasonalClimateDTO
    } deriving (Show, Eq, Generic, Serialize)

toRegionClimateDTO ∷ RegionClimate → RegionClimateDTO
toRegionClimateDTO r = RegionClimateDTO
    { rgcAirTemp        = toSeasonalClimateDTO (rcAirTemp r)
    , rgcHumidity       = rcHumidity r
    , rgcPrecipitation  = toSeasonalClimateDTO (rcPrecipitation r)
    , rgcPrecipType     = rcPrecipType r
    , rgcEvaporation    = rcEvaporation r
    , rgcCloudCover     = rcCloudCover r
    , rgcPressure       = rcPressure r
    , rgcWindDir        = rcWindDir r
    , rgcWindSpeed      = rcWindSpeed r
    , rgcOrographicLift = rcOrographicLift r
    , rgcContinentality = rcContinentality r
    , rgcAlbedo         = rcAlbedo r
    , rgcElevAvg        = rcElevAvg r
    , rgcWaterTable     = toSeasonalClimateDTO (rcWaterTable r)
    }

fromRegionClimateDTO ∷ RegionClimateDTO → RegionClimate
fromRegionClimateDTO d = RegionClimate
    { rcAirTemp        = fromSeasonalClimateDTO (rgcAirTemp d)
    , rcHumidity       = rgcHumidity d
    , rcPrecipitation  = fromSeasonalClimateDTO (rgcPrecipitation d)
    , rcPrecipType     = rgcPrecipType d
    , rcEvaporation    = rgcEvaporation d
    , rcCloudCover     = rgcCloudCover d
    , rcPressure       = rgcPressure d
    , rcWindDir        = rgcWindDir d
    , rcWindSpeed      = rgcWindSpeed d
    , rcOrographicLift = rgcOrographicLift d
    , rcContinentality = rgcContinentality d
    , rcAlbedo         = rgcAlbedo d
    , rcElevAvg        = rgcElevAvg d
    , rcWaterTable     = fromSeasonalClimateDTO (rgcWaterTable d)
    }

-- | Frozen mirror of 'ClimateGrid'. 'ClimateCoord' keys are leaf
--   coordinates (like 'ChunkCoord').
data ClimateGridDTO = ClimateGridDTO
    { cgdRegions ∷ !(HM.HashMap ClimateCoord RegionClimateDTO)
    , cgdSize    ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toClimateGridDTO ∷ ClimateGrid → ClimateGridDTO
toClimateGridDTO g = ClimateGridDTO (HM.map toRegionClimateDTO (cgRegions g)) (cgSize g)

fromClimateGridDTO ∷ ClimateGridDTO → ClimateGrid
fromClimateGridDTO d = ClimateGrid (HM.map fromRegionClimateDTO (cgdRegions d)) (cgdSize d)

-- | Frozen mirror of 'OceanCell'.
data OceanCellDTO = OceanCellDTO
    { occTemperature ∷ !SeasonalClimateDTO
    , occSalinity    ∷ !Float
    , occDepth       ∷ !Int
    , occCurrentDir  ∷ !Float
    , occCurrentSpd  ∷ !Float
    , occUpwelling   ∷ !Float
    , occIceCover    ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toOceanCellDTO ∷ OceanCell → OceanCellDTO
toOceanCellDTO c = OceanCellDTO
    (toSeasonalClimateDTO (ocTemperature c)) (ocSalinity c) (ocDepth c)
    (ocCurrentDir c) (ocCurrentSpd c) (ocUpwelling c) (ocIceCover c)

fromOceanCellDTO ∷ OceanCellDTO → OceanCell
fromOceanCellDTO d = OceanCell
    (fromSeasonalClimateDTO (occTemperature d)) (occSalinity d) (occDepth d)
    (occCurrentDir d) (occCurrentSpd d) (occUpwelling d) (occIceCover d)

-- | Frozen mirror of 'WaterMass'.
data WaterMassDTO = WaterMassDTO
    { wmsDensity     ∷ !Float
    , wmsTemperature ∷ !Float
    , wmsSalinity    ∷ !Float
    , wmsFlowDir     ∷ !Float
    , wmsFlowSpd     ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toWaterMassDTO ∷ WaterMass → WaterMassDTO
toWaterMassDTO w = WaterMassDTO
    (wmDensity w) (wmTemperature w) (wmSalinity w) (wmFlowDir w) (wmFlowSpd w)

fromWaterMassDTO ∷ WaterMassDTO → WaterMass
fromWaterMassDTO d = WaterMass
    (wmsDensity d) (wmsTemperature d) (wmsSalinity d) (wmsFlowDir d) (wmsFlowSpd d)

-- | Frozen mirror of 'OceanCurrent'. 'ClimateCoord' path elements are leaves.
data OceanCurrentDTO = OceanCurrentDTO
    { ocuName     ∷ !Text
    , ocuPath     ∷ ![ClimateCoord]
    , ocuWarm     ∷ !Bool
    , ocuStrength ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toOceanCurrentDTO ∷ OceanCurrent → OceanCurrentDTO
toOceanCurrentDTO c = OceanCurrentDTO (ocName c) (ocPath c) (ocWarm c) (ocStrength c)

fromOceanCurrentDTO ∷ OceanCurrentDTO → OceanCurrent
fromOceanCurrentDTO d = OceanCurrent (ocuName d) (ocuPath d) (ocuWarm d) (ocuStrength d)

-- | Frozen mirror of 'ThermohalineCell'.
data ThermohalineCellDTO = ThermohalineCellDTO
    { thlSinkRegion   ∷ !ClimateCoord
    , thlUpwellRegion ∷ !ClimateCoord
    , thlStrength     ∷ !Float
    , thlDeepTemp     ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toThermohalineCellDTO ∷ ThermohalineCell → ThermohalineCellDTO
toThermohalineCellDTO t = ThermohalineCellDTO
    (thcSinkRegion t) (thcUpwellRegion t) (thcStrength t) (thcDeepTemp t)

fromThermohalineCellDTO ∷ ThermohalineCellDTO → ThermohalineCell
fromThermohalineCellDTO d = ThermohalineCell
    (thlSinkRegion d) (thlUpwellRegion d) (thlStrength d) (thlDeepTemp d)

-- | Frozen mirror of 'OceanGrid'.
data OceanGridDTO = OceanGridDTO
    { ogdCells     ∷ !(HM.HashMap ClimateCoord OceanCellDTO)
    , ogdDeepWater ∷ !(HM.HashMap ClimateCoord WaterMassDTO)
    , ogdCurrents  ∷ ![OceanCurrentDTO]
    , ogdThcCells  ∷ ![ThermohalineCellDTO]
    } deriving (Show, Eq, Generic, Serialize)

toOceanGridDTO ∷ OceanGrid → OceanGridDTO
toOceanGridDTO g = OceanGridDTO
    (HM.map toOceanCellDTO (ogCells g)) (HM.map toWaterMassDTO (ogDeepWater g))
    (map toOceanCurrentDTO (ogCurrents g)) (map toThermohalineCellDTO (ogThcCells g))

fromOceanGridDTO ∷ OceanGridDTO → OceanGrid
fromOceanGridDTO d = OceanGrid
    (HM.map fromOceanCellDTO (ogdCells d)) (HM.map fromWaterMassDTO (ogdDeepWater d))
    (map fromOceanCurrentDTO (ogdCurrents d)) (map fromThermohalineCellDTO (ogdThcCells d))

-- | Frozen mirror of 'WindCell'.
data WindCellDTO = WindCellDTO
    { wndDir      ∷ !Float
    , wndSpeed    ∷ !Float
    , wndPressure ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toWindCellDTO ∷ WindCell → WindCellDTO
toWindCellDTO w = WindCellDTO (wcDir w) (wcSpeed w) (wcPressure w)

fromWindCellDTO ∷ WindCellDTO → WindCell
fromWindCellDTO d = WindCell (wndDir d) (wndSpeed d) (wndPressure d)

-- | Frozen mirror of 'MoistureCell'.
data MoistureCellDTO = MoistureCellDTO
    { moiFlux   ∷ !Float
    , moiSource ∷ !Float
    , moiSink   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toMoistureCellDTO ∷ MoistureCell → MoistureCellDTO
toMoistureCellDTO m = MoistureCellDTO (mcFlux m) (mcSource m) (mcSink m)

fromMoistureCellDTO ∷ MoistureCellDTO → MoistureCell
fromMoistureCellDTO d = MoistureCell (moiFlux d) (moiSource d) (moiSink d)

-- | Frozen mirror of 'PressureSystem'. 'PressureType' is an append-only
--   enum leaf.
data PressureSystemDTO = PressureSystemDTO
    { prsCenter   ∷ !ClimateCoord
    , prsType     ∷ !PressureType
    , prsRadius   ∷ !Int
    , prsStrength ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toPressureSystemDTO ∷ PressureSystem → PressureSystemDTO
toPressureSystemDTO p = PressureSystemDTO
    (psCenter p) (psType p) (psRadius p) (psStrength p)

fromPressureSystemDTO ∷ PressureSystemDTO → PressureSystem
fromPressureSystemDTO d = PressureSystem
    (prsCenter d) (prsType d) (prsRadius d) (prsStrength d)

-- | Frozen mirror of 'AtmoGrid'.
data AtmoGridDTO = AtmoGridDTO
    { agdWind     ∷ !(HM.HashMap ClimateCoord WindCellDTO)
    , agdMoisture ∷ !(HM.HashMap ClimateCoord MoistureCellDTO)
    , agdSystems  ∷ ![PressureSystemDTO]
    } deriving (Show, Eq, Generic, Serialize)

toAtmoGridDTO ∷ AtmoGrid → AtmoGridDTO
toAtmoGridDTO g = AtmoGridDTO
    (HM.map toWindCellDTO (agWind g)) (HM.map toMoistureCellDTO (agMoisture g))
    (map toPressureSystemDTO (agSystems g))

fromAtmoGridDTO ∷ AtmoGridDTO → AtmoGrid
fromAtmoGridDTO d = AtmoGrid
    (HM.map fromWindCellDTO (agdWind d)) (HM.map fromMoistureCellDTO (agdMoisture d))
    (map fromPressureSystemDTO (agdSystems d))

-- | Frozen mirror of 'SurfaceBudget'. 'SurfaceType' is an append-only
--   enum leaf.
data SurfaceBudgetDTO = SurfaceBudgetDTO
    { srfSurfaceType ∷ !SurfaceType
    , srfAlbedo      ∷ !Float
    , srfNetMoisture ∷ !Float
    , srfRunoff      ∷ !Float
    , srfSnowpack    ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toSurfaceBudgetDTO ∷ SurfaceBudget → SurfaceBudgetDTO
toSurfaceBudgetDTO s = SurfaceBudgetDTO
    (sbSurfaceType s) (sbAlbedo s) (sbNetMoisture s) (sbRunoff s) (sbSnowpack s)

fromSurfaceBudgetDTO ∷ SurfaceBudgetDTO → SurfaceBudget
fromSurfaceBudgetDTO d = SurfaceBudget
    (srfSurfaceType d) (srfAlbedo d) (srfNetMoisture d) (srfRunoff d) (srfSnowpack d)

-- | Frozen mirror of 'ClimateState' (the full climate-sim state).
data ClimateStateDTO = ClimateStateDTO
    { clsClimate    ∷ !ClimateGridDTO
    , clsOcean      ∷ !OceanGridDTO
    , clsAtmo       ∷ !AtmoGridDTO
    , clsSurface    ∷ !(HM.HashMap ClimateCoord SurfaceBudgetDTO)
    , clsGlobalCO2  ∷ !Float
    , clsGlobalTemp ∷ !Float
    , clsSolarConst ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toClimateStateDTO ∷ ClimateState → ClimateStateDTO
toClimateStateDTO c = ClimateStateDTO
    { clsClimate    = toClimateGridDTO (csClimate c)
    , clsOcean      = toOceanGridDTO (csOcean c)
    , clsAtmo       = toAtmoGridDTO (csAtmo c)
    , clsSurface    = HM.map toSurfaceBudgetDTO (csSurface c)
    , clsGlobalCO2  = csGlobalCO2 c
    , clsGlobalTemp = csGlobalTemp c
    , clsSolarConst = csSolarConst c
    }

fromClimateStateDTO ∷ ClimateStateDTO → ClimateState
fromClimateStateDTO d = ClimateState
    { csClimate    = fromClimateGridDTO (clsClimate d)
    , csOcean      = fromOceanGridDTO (clsOcean d)
    , csAtmo       = fromAtmoGridDTO (clsAtmo d)
    , csSurface    = HM.map fromSurfaceBudgetDTO (clsSurface d)
    , csGlobalCO2  = clsGlobalCO2 d
    , csGlobalTemp = clsGlobalTemp d
    , csSolarConst = clsSolarConst d
    }
