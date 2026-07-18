{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Frozen worldgen-parameter DTOs (issue #760, save-overhaul B2 — review
--   round 6). The @"world-pages"@ component embeds a page's
--   'WorldGenParams', a mutable runtime record whose nested worldgen
--   config/state sub-records evolve as features land. Per the component
--   frozen-DTO boundary rule (see 'World.Save.Component.Types' — the ONE
--   place that rule is stated), each of those nested records that is
--   itself LIVE runtime state with no independent save-governance
--   discipline is mirrored here by a component-owned DTO with an explicit,
--   field-by-field @to…@/@from…@ conversion, so a field added / dropped /
--   reordered on any live record surfaces as a compile error in its
--   conversion rather than as silent byte drift in a shipped v1 save.
--
--   Frozen here (live records lacking their own documented save-schema
--   discipline):
--
--   - 'WorldGenParams'  → 'WorldGenParamsDTO'
--   - 'TectonicPlate'   → 'TectonicPlateDTO'
--   - 'CalendarConfig'  → 'CalendarConfigDTO'
--   - 'SunConfig'       → 'SunConfigDTO'
--   - 'MoonConfig'      → 'MoonConfigDTO'
--   - 'ClimateParams'   → 'ClimateParamsDTO'
--   - 'TimelineParams'  → 'TimelineParamsDTO'
--   - 'OreLevers'       → 'OreLeversDTO'
--   - 'ClimateState'    → 'ClimateStateDTO', recursively: 'ClimateGrid',
--     'RegionClimate', 'SeasonalClimate', 'OceanGrid', 'OceanCell',
--     'WaterMass', 'OceanCurrent', 'ThermohalineCell', 'AtmoGrid',
--     'WindCell', 'MoistureCell', 'PressureSystem', 'SurfaceBudget'.
--     The recursion is transitive precisely because a shallow
--     'ClimateStateDTO' that re-embedded live 'OceanGrid'/'AtmoGrid'
--     would still drift when THOSE gained a field — the boundary rule's
--     "recurse the freezing as needed" clause.
--
--   Reused as LEAVES, NOT mirrored, exactly per the boundary rule:
--
--   - 'GeoTimeline' — a bulk immutable worldgen-OUTPUT table that carries
--     its OWN in-source, documented positional-save-schema freeze
--     discipline (its fields are annotated with the save version each
--     landed in — @gtCoastal@ "save v25", @gtSeabed@ "save v26",
--     @gtWorldOcean@ "save v27", @gtOreDeposits@ "save v30", and an
--     explicit "keep field order stable (save schema)" contract), so it
--     is already an independently-governed persistence shape whose change
--     requires a deliberate schema bump — the SAME leaf exception the
--     boundary rule grants 'BuildingInstanceSnapshot'/
--     'UnitInstanceSnapshot'. Freezing its ~30-type tree would be no safer
--     and pure gold-plating. This is the exact line separating it from
--     'ClimateState', which carries no such in-source discipline and is
--     therefore frozen above.
--   - The content-collection aliases 'OceanMap' (@HashSet ChunkCoord@),
--     'OceanDistMap' (@HashMap ChunkCoord Int@) and 'LocationOverlay'
--     (@HashMap ChunkCoord Text@), and the location-flag sets
--     (@HashSet ChunkCoord@) — content-reference collections with no
--     record identity that could gain / reorder a field.
--   - Leaf id / coordinate / enum references inside the frozen records:
--     'MaterialId' (TectonicPlate), 'ClimateCoord' (a coordinate like
--     'ChunkCoord'), and the append-only enums 'PressureType' /
--     'SurfaceType'.
--
--   Every DTO's field order EXACTLY mirrors its live record's declaration
--   order (and 'WorldGenParams''s manual 'Serialize' field order), so the
--   derived cereal layout is byte-identical to embedding the live records
--   directly — the frozen tracked fixture in
--   "Test.Headless.World.Save.Components" stays valid across this change.
module World.Save.Component.WorldGen
    ( WorldGenParamsDTO(..)
    , TectonicPlateDTO(..)
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
    , toWorldGenParamsDTO
    , fromWorldGenParamsDTO
    , toClimateStateDTO
    , fromClimateStateDTO
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Generate.Types (WorldGenParams(..), withVolcanoCtx)
import World.Magma.Types (emptyVolcanoCtx)
import World.Plate.Types (TectonicPlate(..))
import World.Material.Id (MaterialId)
import World.Time.Types
    (CalendarConfig(..), SunConfig(..), MoonConfig(..))
import World.Geology.Timeline.Types (GeoTimeline, TimelineParams(..))
import World.Ocean.Types (OceanMap, OceanDistMap)
import World.Weather.Types
    ( ClimateParams(..), ClimateState(..), ClimateGrid(..), ClimateCoord
    , RegionClimate(..), SeasonalClimate(..), OceanGrid(..), OceanCell(..)
    , WaterMass(..), OceanCurrent(..), ThermohalineCell(..), AtmoGrid(..)
    , WindCell(..), MoistureCell(..), PressureSystem(..), PressureType
    , SurfaceType, SurfaceBudget(..) )
import World.Geology.Ore.Types (OreLevers(..))
import Location.Overlay.Types (LocationOverlay)
import World.Chunk.Types (ChunkCoord)

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

-- WorldGenParams ----------------------------------------------------

-- | Frozen mirror of 'WorldGenParams' (a mutable runtime record that
--   gains fields as worldgen features land — the #89/#90/#424/#780
--   location flags are the recent examples). Field order matches
--   'WorldGenParams''s MANUAL 'Serialize' instance exactly (every field
--   except the transient @wgpVolcanoCtx@, which that instance also skips
--   and rebuilds on load), so the derived cereal layout here is
--   byte-identical to embedding the record directly. Each nested live
--   config/state record is a frozen DTO (see this module's haddock);
--   'GeoTimeline' and the content-collection aliases are reused as leaves.
data WorldGenParamsDTO = WorldGenParamsDTO
    { gpSeed                    ∷ !Word64
    , gpWorldSize               ∷ !Int
    , gpPlateCount              ∷ !Int
    , gpPlates                  ∷ ![TectonicPlateDTO]
    , gpCalender                ∷ !CalendarConfigDTO
    , gpSunConfig               ∷ !SunConfigDTO
    , gpMoonConfig              ∷ !MoonConfigDTO
    , gpGeoTimeline             ∷ !GeoTimeline
    , gpOceanMap                ∷ !OceanMap
    , gpOceanDist               ∷ !OceanDistMap
    , gpClimateParams           ∷ !ClimateParamsDTO
    , gpClimateState            ∷ !ClimateStateDTO
    , gpErosionIntensity        ∷ !Float
    , gpVolcanicActivity        ∷ !Float
    , gpLavaPoolDepth           ∷ !Int
    , gpLavaPoolRadius          ∷ !Int
    , gpWaterfallQuantum        ∷ !Int
    , gpOreLevers               ∷ !OreLeversDTO
    , gpTimelineParams          ∷ !TimelineParamsDTO
    , gpLocationOverlay         ∷ !LocationOverlay
    , gpLocationContentsSpawned ∷ !(HS.HashSet ChunkCoord)
    , gpLocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gpLocationDiscovered      ∷ !(HS.HashSet ChunkCoord)
    } deriving (Show, Eq, Generic, Serialize)

toWorldGenParamsDTO ∷ WorldGenParams → WorldGenParamsDTO
toWorldGenParamsDTO p = WorldGenParamsDTO
    { gpSeed                    = wgpSeed p
    , gpWorldSize               = wgpWorldSize p
    , gpPlateCount              = wgpPlateCount p
    , gpPlates                  = map toTectonicPlateDTO (wgpPlates p)
    , gpCalender                = toCalendarConfigDTO (wgpCalender p)
    , gpSunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gpMoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gpGeoTimeline             = wgpGeoTimeline p
    , gpOceanMap                = wgpOceanMap p
    , gpOceanDist               = wgpOceanDist p
    , gpClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gpClimateState            = toClimateStateDTO (wgpClimateState p)
    , gpErosionIntensity        = wgpErosionIntensity p
    , gpVolcanicActivity        = wgpVolcanicActivity p
    , gpLavaPoolDepth           = wgpLavaPoolDepth p
    , gpLavaPoolRadius          = wgpLavaPoolRadius p
    , gpWaterfallQuantum        = wgpWaterfallQuantum p
    , gpOreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gpTimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gpLocationOverlay         = wgpLocationOverlay p
    , gpLocationContentsSpawned = wgpLocationContentsSpawned p
    , gpLocationStamped         = wgpLocationStamped p
    , gpLocationDiscovered      = wgpLocationDiscovered p
    }

-- | Rebuild the live record from the DTO, restoring the transient
--   @wgpVolcanoCtx@ via 'withVolcanoCtx' exactly the way the manual
--   'Serialize' instance's @get@ does (from seed / world-size / plates /
--   timeline). Adding a field to 'WorldGenParams' (or any nested frozen
--   record) breaks THIS construction — the conscious reconciliation the
--   boundary rule asks for.
fromWorldGenParamsDTO ∷ WorldGenParamsDTO → WorldGenParams
fromWorldGenParamsDTO d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gpSeed d
    , wgpWorldSize               = gpWorldSize d
    , wgpPlateCount              = gpPlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gpPlates d)
    , wgpCalender                = fromCalendarConfigDTO (gpCalender d)
    , wgpSunConfig               = fromSunConfigDTO (gpSunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gpMoonConfig d)
    , wgpGeoTimeline             = gpGeoTimeline d
    , wgpOceanMap                = gpOceanMap d
    , wgpOceanDist               = gpOceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gpClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gpClimateState d)
    , wgpErosionIntensity        = gpErosionIntensity d
    , wgpVolcanicActivity        = gpVolcanicActivity d
    , wgpLavaPoolDepth           = gpLavaPoolDepth d
    , wgpLavaPoolRadius          = gpLavaPoolRadius d
    , wgpWaterfallQuantum        = gpWaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gpOreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gpTimelineParams d)
    , wgpLocationOverlay         = gpLocationOverlay d
    , wgpLocationContentsSpawned = gpLocationContentsSpawned d
    , wgpLocationStamped         = gpLocationStamped d
    , wgpLocationDiscovered      = gpLocationDiscovered d
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }
