{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Frozen worldgen-parameter DTOs (issue #760, save-overhaul B2).
--   The @"world-pages"@ component embeds a page's
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
--   - 'RiverName'       → 'RiverNameDTO' (and its table → 'RiverNamesDTO')
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
    , WorldGenParamsDTOv1(..)
    , WorldGenParamsDTOv2(..)
    , WorldGenParamsDTOv3(..)
    , RiverNameDTO(..)
    , RiverNamesDTO(..)
    , AbsBoundsDTO(..)
    , LocationInstanceDTO(..)
    , LocationInstancesDTO(..)
    , LocationInstanceDTOv1(..)
    , LocationInstancesDTOv1(..)
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
    , fromWorldGenParamsDTOv1
    , toWorldGenParamsDTOv1
    , fromWorldGenParamsDTOv2
    , toWorldGenParamsDTOv2
    , fromWorldGenParamsDTOv3
    , toWorldGenParamsDTOv3
    , toRiverNamesDTO
    , fromRiverNamesDTO
    , toLocationInstancesDTO
    , fromLocationInstancesDTO
    , toLocationInstancesDTOv1
    , fromLocationInstancesDTOv1
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
import Location.Bounds (AbsBounds(..))
import World.Base (GeoFeatureId)
import World.River.Naming (RiverName(..), RiverNames(..), emptyRiverNames)
import Location.Instance
    ( LocationInstance(..), LocationInstances(..), LocationInstanceId
    , LocationLifecycle, pendingLegacyFlags, instancesToList
    , isDiscoveredLifecycle )
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

-- Placed-location instances (#911) -----------------------------------

-- | Frozen mirror of 'Location.Bounds.AbsBounds'. An inclusive tile box
--   is definitionally four coordinates, so this could arguably be a
--   leaf like 'ChunkCoord' — it is frozen anyway because it is reached
--   only through 'LocationInstanceDTO', whose whole point is that the
--   live location record WILL gain fields (encounter, loot, and cleared
--   state are the next expedition-loop issues), and freezing the pair
--   together keeps the boundary in one place.
data AbsBoundsDTO = AbsBoundsDTO
    { abdMinX ∷ !Int
    , abdMinY ∷ !Int
    , abdMaxX ∷ !Int
    , abdMaxY ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toAbsBoundsDTO ∷ AbsBounds → AbsBoundsDTO
toAbsBoundsDTO b = AbsBoundsDTO (abMinX b) (abMinY b) (abMaxX b) (abMaxY b)

fromAbsBoundsDTO ∷ AbsBoundsDTO → AbsBounds
fromAbsBoundsDTO d = AbsBounds (abdMinX d) (abdMinY d) (abdMaxX d) (abdMaxY d)

-- | Frozen mirror of 'Location.Instance.LocationInstance' — a LIVE
--   gameplay record by construction (its lifecycle and content-spawn
--   flag are mutated in place, and the expedition arc adds encounter /
--   loot / progression fields to it), so the boundary rule
--   ("World.Save.Component.Types") requires an explicit field-by-field
--   conversion rather than embedding it. 'LocationInstanceId' is a leaf
--   id and 'LocationLifecycle' a payload-free append-only enum, both
--   reused as-is exactly like 'ChunkCoord' / 'ZoomMapMode'.
--
--   This is the CURRENT shape, carried by @world-pages@ v4, with the
--   English gloss #1101 stores beside the display name.
--   'LocationInstanceDTOv1' below is the frozen pre-#1101 shape.
data LocationInstanceDTO = LocationInstanceDTO
    { lidId              ∷ !LocationInstanceId
    , lidDefId           ∷ !Text
    , lidChunk           ∷ !ChunkCoord
    , lidAnchorX         ∷ !Int
    , lidAnchorY         ∷ !Int
    , lidBounds          ∷ !AbsBoundsDTO
    , lidDiscoveryMargin ∷ !Int
    , lidDisplayName     ∷ !Text
    , lidGloss           ∷ !(Maybe Text)
    , lidLifecycle       ∷ !LocationLifecycle
    , lidContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstanceDTO ∷ LocationInstance → LocationInstanceDTO
toLocationInstanceDTO i = LocationInstanceDTO
    { lidId              = liId i
    , lidDefId           = liDefId i
    , lidChunk           = liChunk i
    , lidAnchorX         = fst (liAnchor i)
    , lidAnchorY         = snd (liAnchor i)
    , lidBounds          = toAbsBoundsDTO (liBounds i)
    , lidDiscoveryMargin = liDiscoveryMargin i
    , lidDisplayName     = liDisplayName i
    , lidGloss           = liGloss i
    , lidLifecycle       = liLifecycle i
    , lidContentsSpawned = liContentsSpawned i
    }

fromLocationInstanceDTO ∷ LocationInstanceDTO → LocationInstance
fromLocationInstanceDTO d = LocationInstance
    { liId              = lidId d
    , liDefId           = lidDefId d
    , liChunk           = lidChunk d
    , liAnchor          = (lidAnchorX d, lidAnchorY d)
    , liBounds          = fromAbsBoundsDTO (lidBounds d)
    , liDiscoveryMargin = lidDiscoveryMargin d
    , liDisplayName     = lidDisplayName d
    , liGloss           = lidGloss d
    , liLifecycle       = lidLifecycle d
    , liContentsSpawned = lidContentsSpawned d
    }

-- | Frozen mirror of the per-page instance table: its allocator plus
--   its instances. 'Location.Instance.lisPendingLegacy' has no field
--   here on purpose — it is a transient v1-migration carry that can
--   never be true of anything on disk, so 'fromLocationInstancesDTO'
--   always rebuilds it as 'Nothing'.
data LocationInstancesDTO = LocationInstancesDTO
    { lisdNextId ∷ !Int
    , lisdById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTO)
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstancesDTO ∷ LocationInstances → LocationInstancesDTO
toLocationInstancesDTO l = LocationInstancesDTO
    { lisdNextId = lisNextId l
    , lisdById   = HM.map toLocationInstanceDTO (lisById l)
    }

fromLocationInstancesDTO ∷ LocationInstancesDTO → LocationInstances
fromLocationInstancesDTO d = LocationInstances
    { lisNextId        = lisdNextId d
    , lisById          = HM.map fromLocationInstanceDTO (lisdById d)
    , lisPendingLegacy = Nothing
    }

-- | The FROZEN pre-#1101 instance shape, preserved verbatim for
--   decode-only backward compatibility: everything the current DTO
--   carries except the gloss. Never edited — a further change freezes
--   the CURRENT shape as 'LocationInstanceDTOv2' instead (frozen-DTO
--   boundary rule).
data LocationInstanceDTOv1 = LocationInstanceDTOv1
    { lid1Id              ∷ !LocationInstanceId
    , lid1DefId           ∷ !Text
    , lid1Chunk           ∷ !ChunkCoord
    , lid1AnchorX         ∷ !Int
    , lid1AnchorY         ∷ !Int
    , lid1Bounds          ∷ !AbsBoundsDTO
    , lid1DiscoveryMargin ∷ !Int
    , lid1DisplayName     ∷ !Text
    , lid1Lifecycle       ∷ !LocationLifecycle
    , lid1ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | A pre-#1101 instance keeps the name it was stored with, EXACTLY —
--   it was rendered once when the instance was placed and is never
--   re-derived (#1101 requirements 4 and 7) — and decodes with NO gloss.
--   A gloss is the English reading of a generated name; a stored label
--   has no such reading, and inventing one would attach a meaning to a
--   location that never had it.
fromLocationInstanceDTOv1 ∷ LocationInstanceDTOv1 → LocationInstance
fromLocationInstanceDTOv1 d = LocationInstance
    { liId              = lid1Id d
    , liDefId           = lid1DefId d
    , liChunk           = lid1Chunk d
    , liAnchor          = (lid1AnchorX d, lid1AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid1Bounds d)
    , liDiscoveryMargin = lid1DiscoveryMargin d
    , liDisplayName     = lid1DisplayName d
    , liGloss           = Nothing
    , liLifecycle       = lid1Lifecycle d
    , liContentsSpawned = lid1ContentsSpawned d
    }

-- | The FROZEN pre-#1101 instance table. Structurally identical to
--   'LocationInstancesDTO' but over the frozen per-instance shape.
data LocationInstancesDTOv1 = LocationInstancesDTOv1
    { lisd1NextId ∷ !Int
    , lisd1ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen table — the round-trip partner every frozen
--   DTO version's tests build fixture bytes with (the same reason
--   'toWorldGenParamsDTOv1' exists).
toLocationInstancesDTOv1 ∷ LocationInstances → LocationInstancesDTOv1
toLocationInstancesDTOv1 l = LocationInstancesDTOv1
    { lisd1NextId = lisNextId l
    , lisd1ById   = HM.map toV1 (lisById l)
    }
  where
    toV1 i = LocationInstanceDTOv1
        { lid1Id              = liId i
        , lid1DefId           = liDefId i
        , lid1Chunk           = liChunk i
        , lid1AnchorX         = fst (liAnchor i)
        , lid1AnchorY         = snd (liAnchor i)
        , lid1Bounds          = toAbsBoundsDTO (liBounds i)
        , lid1DiscoveryMargin = liDiscoveryMargin i
        , lid1DisplayName     = liDisplayName i
        , lid1Lifecycle       = liLifecycle i
        , lid1ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv1 ∷ LocationInstancesDTOv1 → LocationInstances
fromLocationInstancesDTOv1 d = LocationInstances
    { lisNextId        = lisd1NextId d
    , lisById          = HM.map fromLocationInstanceDTOv1 (lisd1ById d)
    , lisPendingLegacy = Nothing
    }

-- WorldGenParams ----------------------------------------------------

-- River names (#1102) ------------------------------------------------

-- | Frozen mirror of 'World.River.Naming.RiverName'. Small and stable
--   today, but it is a LIVE record on a live per-page table — a later
--   naming issue could give a river a second reading or a provenance of
--   its own — so it gets the same explicit conversion every other live
--   record on 'WorldGenParams' does rather than being embedded.
data RiverNameDTO = RiverNameDTO
    { rvdDisplayName ∷ !Text
    , rvdGloss       ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'World.River.Naming.RiverNames'.
--   'World.Base.GeoFeatureId' is a leaf id newtype over 'Int', reused
--   as-is exactly like 'LocationInstanceId' / 'ChunkCoord'.
newtype RiverNamesDTO = RiverNamesDTO
    { rvdById ∷ HM.HashMap GeoFeatureId RiverNameDTO }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

toRiverNameDTO ∷ RiverName → RiverNameDTO
toRiverNameDTO n = RiverNameDTO
    { rvdDisplayName = rvnDisplayName n
    , rvdGloss       = rvnGloss n
    }

-- | Rebuild the live record. A stored name is carried across EXACTLY —
--   never re-rendered from the page's language, which is the whole
--   point of #708 principle 5 (a river named under one catalogue keeps
--   that name even after the catalogue grows).
fromRiverNameDTO ∷ RiverNameDTO → RiverName
fromRiverNameDTO d = RiverName
    { rvnDisplayName = rvdDisplayName d
    , rvnGloss       = rvdGloss d
    }

toRiverNamesDTO ∷ RiverNames → RiverNamesDTO
toRiverNamesDTO = RiverNamesDTO . HM.map toRiverNameDTO . rvnById

fromRiverNamesDTO ∷ RiverNamesDTO → RiverNames
fromRiverNamesDTO = RiverNames . HM.map fromRiverNameDTO . rvdById

-- | Frozen mirror of 'WorldGenParams' (a mutable runtime record that
--   gains fields as worldgen features land — the #89/#90/#424/#780
--   location flags were the recent examples, and #911 replaced two of
--   them with the instance table below). Field order matches
--   'WorldGenParams''s declaration order (every field except the
--   transient @wgpVolcanoCtx@, which its manual 'Serialize' instance
--   also skips and rebuilds on load). Each nested live config/state
--   record is a frozen DTO (see this module's haddock); 'GeoTimeline'
--   and the content-collection aliases are reused as leaves.
--
--   This is the CURRENT shape, carried by @world-pages@ v5: it adds
--   #1102's per-page river-name table beside the location instances.
--   'WorldGenParamsDTOv3' below is the frozen shape @world-pages@ v4
--   carries (#1101's per-instance gloss, no river names),
--   'WorldGenParamsDTOv2' the frozen shape v2 and v3 carry (the #911
--   instance table, no gloss), and 'WorldGenParamsDTOv1' the frozen
--   pre-#911 shape (three chunk-keyed location sets); all three are
--   decode-only.
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
    , gpLocationInstances       ∷ !LocationInstancesDTO
    , gpLocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gpRiverNames              ∷ !RiverNamesDTO
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
    , gpLocationInstances       = toLocationInstancesDTO (wgpLocationInstances p)
    , gpLocationStamped         = wgpLocationStamped p
    , gpRiverNames              = toRiverNamesDTO (wgpRiverNames p)
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
    , wgpLocationInstances       = fromLocationInstancesDTO (gpLocationInstances d)
    , wgpLocationStamped         = gpLocationStamped d
    , wgpRiverNames              = fromRiverNamesDTO (gpRiverNames d)
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#1102 worldgen params (@world-pages@ v4) ----------------

-- | The FROZEN pre-#1102 shape of 'WorldGenParamsDTO', preserved
--   verbatim for decode-only backward compatibility: identical to the
--   current type except that it carries no river-name table. This is
--   what @world-pages@ v4 (#1101) encoded. Never edited; a further
--   gen-params change freezes the CURRENT type as a v4 instead
--   (frozen-DTO boundary rule).
data WorldGenParamsDTOv3 = WorldGenParamsDTOv3
    { gp3Seed                    ∷ !Word64
    , gp3WorldSize               ∷ !Int
    , gp3PlateCount              ∷ !Int
    , gp3Plates                  ∷ ![TectonicPlateDTO]
    , gp3Calender                ∷ !CalendarConfigDTO
    , gp3SunConfig               ∷ !SunConfigDTO
    , gp3MoonConfig              ∷ !MoonConfigDTO
    , gp3GeoTimeline             ∷ !GeoTimeline
    , gp3OceanMap                ∷ !OceanMap
    , gp3OceanDist               ∷ !OceanDistMap
    , gp3ClimateParams           ∷ !ClimateParamsDTO
    , gp3ClimateState            ∷ !ClimateStateDTO
    , gp3ErosionIntensity        ∷ !Float
    , gp3VolcanicActivity        ∷ !Float
    , gp3LavaPoolDepth           ∷ !Int
    , gp3LavaPoolRadius          ∷ !Int
    , gp3WaterfallQuantum        ∷ !Int
    , gp3OreLevers               ∷ !OreLeversDTO
    , gp3TimelineParams          ∷ !TimelineParamsDTO
    , gp3LocationOverlay         ∷ !LocationOverlay
    , gp3LocationInstances       ∷ !LocationInstancesDTO
    , gp3LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen v3 gen params, the round-trip partner
--   'toWorldGenParamsDTOv1'/'toWorldGenParamsDTOv2' already provide for
--   the older shapes — how a test builds real pre-#1102 bytes to
--   migrate from. Production never writes this shape.
toWorldGenParamsDTOv3 ∷ WorldGenParams → WorldGenParamsDTOv3
toWorldGenParamsDTOv3 p = WorldGenParamsDTOv3
    { gp3Seed                    = wgpSeed p
    , gp3WorldSize               = wgpWorldSize p
    , gp3PlateCount              = wgpPlateCount p
    , gp3Plates                  = map toTectonicPlateDTO (wgpPlates p)
    , gp3Calender                = toCalendarConfigDTO (wgpCalender p)
    , gp3SunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gp3MoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gp3GeoTimeline             = wgpGeoTimeline p
    , gp3OceanMap                = wgpOceanMap p
    , gp3OceanDist               = wgpOceanDist p
    , gp3ClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gp3ClimateState            = toClimateStateDTO (wgpClimateState p)
    , gp3ErosionIntensity        = wgpErosionIntensity p
    , gp3VolcanicActivity        = wgpVolcanicActivity p
    , gp3LavaPoolDepth           = wgpLavaPoolDepth p
    , gp3LavaPoolRadius          = wgpLavaPoolRadius p
    , gp3WaterfallQuantum        = wgpWaterfallQuantum p
    , gp3OreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gp3TimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gp3LocationOverlay         = wgpLocationOverlay p
    , gp3LocationInstances       = toLocationInstancesDTO
                                      (wgpLocationInstances p)
    , gp3LocationStamped         = wgpLocationStamped p
    }

-- | Rebuild the live record from a v3 DTO. Every field rides across
--   untouched and the river-name table comes back EMPTY — a save
--   written before #1102 named no rivers, and a name is never inferred
--   after the fact (#1102 requirements 5 and 6). Its rivers keep their
--   ids, which are derived from the timeline it already carries.
fromWorldGenParamsDTOv3 ∷ WorldGenParamsDTOv3 → WorldGenParams
fromWorldGenParamsDTOv3 d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gp3Seed d
    , wgpWorldSize               = gp3WorldSize d
    , wgpPlateCount              = gp3PlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gp3Plates d)
    , wgpCalender                = fromCalendarConfigDTO (gp3Calender d)
    , wgpSunConfig               = fromSunConfigDTO (gp3SunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gp3MoonConfig d)
    , wgpGeoTimeline             = gp3GeoTimeline d
    , wgpOceanMap                = gp3OceanMap d
    , wgpOceanDist               = gp3OceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gp3ClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gp3ClimateState d)
    , wgpErosionIntensity        = gp3ErosionIntensity d
    , wgpVolcanicActivity        = gp3VolcanicActivity d
    , wgpLavaPoolDepth           = gp3LavaPoolDepth d
    , wgpLavaPoolRadius          = gp3LavaPoolRadius d
    , wgpWaterfallQuantum        = gp3WaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gp3OreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gp3TimelineParams d)
    , wgpLocationOverlay         = gp3LocationOverlay d
    , wgpLocationInstances       = fromLocationInstancesDTO
                                      (gp3LocationInstances d)
    , wgpLocationStamped         = gp3LocationStamped d
    , wgpRiverNames              = emptyRiverNames
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#1101 worldgen params (@world-pages@ v2 / v3) -----------

-- | The FROZEN pre-#1101 shape of 'WorldGenParamsDTO', preserved
--   verbatim for decode-only backward compatibility: identical to the
--   current type except that its instance table is the frozen
--   'LocationInstancesDTOv1', whose instances carry no gloss. This is
--   what @world-pages@ v2 (#911) and v3 (#1092) both encoded — #1092
--   changed the page IDENTITY only, so both versions share one gen-params
--   shape. Never edited; a further gen-params change freezes the CURRENT
--   type as a v3 instead (frozen-DTO boundary rule).
data WorldGenParamsDTOv2 = WorldGenParamsDTOv2
    { gp2Seed                    ∷ !Word64
    , gp2WorldSize               ∷ !Int
    , gp2PlateCount              ∷ !Int
    , gp2Plates                  ∷ ![TectonicPlateDTO]
    , gp2Calender                ∷ !CalendarConfigDTO
    , gp2SunConfig               ∷ !SunConfigDTO
    , gp2MoonConfig              ∷ !MoonConfigDTO
    , gp2GeoTimeline             ∷ !GeoTimeline
    , gp2OceanMap                ∷ !OceanMap
    , gp2OceanDist               ∷ !OceanDistMap
    , gp2ClimateParams           ∷ !ClimateParamsDTO
    , gp2ClimateState            ∷ !ClimateStateDTO
    , gp2ErosionIntensity        ∷ !Float
    , gp2VolcanicActivity        ∷ !Float
    , gp2LavaPoolDepth           ∷ !Int
    , gp2LavaPoolRadius          ∷ !Int
    , gp2WaterfallQuantum        ∷ !Int
    , gp2OreLevers               ∷ !OreLeversDTO
    , gp2TimelineParams          ∷ !TimelineParamsDTO
    , gp2LocationOverlay         ∷ !LocationOverlay
    , gp2LocationInstances       ∷ !LocationInstancesDTOv1
    , gp2LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen v2 gen params, the round-trip partner
--   'toWorldGenParamsDTOv1' already provides for v1 — how a test builds
--   real pre-#1101 bytes to migrate from.
toWorldGenParamsDTOv2 ∷ WorldGenParams → WorldGenParamsDTOv2
toWorldGenParamsDTOv2 p = WorldGenParamsDTOv2
    { gp2Seed                    = wgpSeed p
    , gp2WorldSize               = wgpWorldSize p
    , gp2PlateCount              = wgpPlateCount p
    , gp2Plates                  = map toTectonicPlateDTO (wgpPlates p)
    , gp2Calender                = toCalendarConfigDTO (wgpCalender p)
    , gp2SunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gp2MoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gp2GeoTimeline             = wgpGeoTimeline p
    , gp2OceanMap                = wgpOceanMap p
    , gp2OceanDist               = wgpOceanDist p
    , gp2ClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gp2ClimateState            = toClimateStateDTO (wgpClimateState p)
    , gp2ErosionIntensity        = wgpErosionIntensity p
    , gp2VolcanicActivity        = wgpVolcanicActivity p
    , gp2LavaPoolDepth           = wgpLavaPoolDepth p
    , gp2LavaPoolRadius          = wgpLavaPoolRadius p
    , gp2WaterfallQuantum        = wgpWaterfallQuantum p
    , gp2OreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gp2TimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gp2LocationOverlay         = wgpLocationOverlay p
    , gp2LocationInstances       = toLocationInstancesDTOv1
                                      (wgpLocationInstances p)
    , gp2LocationStamped         = wgpLocationStamped p
    }

-- | Rebuild the live record from a v2 DTO. Every field rides across
--   untouched; each stored instance keeps its exact stored name and
--   gains NO gloss ('fromLocationInstanceDTOv1').
fromWorldGenParamsDTOv2 ∷ WorldGenParamsDTOv2 → WorldGenParams
fromWorldGenParamsDTOv2 d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gp2Seed d
    , wgpWorldSize               = gp2WorldSize d
    , wgpPlateCount              = gp2PlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gp2Plates d)
    , wgpCalender                = fromCalendarConfigDTO (gp2Calender d)
    , wgpSunConfig               = fromSunConfigDTO (gp2SunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gp2MoonConfig d)
    , wgpGeoTimeline             = gp2GeoTimeline d
    , wgpOceanMap                = gp2OceanMap d
    , wgpOceanDist               = gp2OceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gp2ClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gp2ClimateState d)
    , wgpErosionIntensity        = gp2ErosionIntensity d
    , wgpVolcanicActivity        = gp2VolcanicActivity d
    , wgpLavaPoolDepth           = gp2LavaPoolDepth d
    , wgpLavaPoolRadius          = gp2LavaPoolRadius d
    , wgpWaterfallQuantum        = gp2WaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gp2OreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gp2TimelineParams d)
    , wgpLocationOverlay         = gp2LocationOverlay d
    , wgpLocationInstances       = fromLocationInstancesDTOv1
                                      (gp2LocationInstances d)
    , wgpLocationStamped         = gp2LocationStamped d
    , wgpRiverNames              = emptyRiverNames
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#911 worldgen params (@world-pages@ v1) ------------------

-- | The FROZEN v1 shape of 'WorldGenParamsDTO', preserved verbatim for
--   decode-only backward compatibility: before #911 a page's placed
--   locations were three chunk-keyed sets rather than an instance
--   table. Never edited — a further schema change adds a v3 type
--   instead (frozen-DTO boundary rule). Decoded by @world-pages@ v1 and
--   by the legacy B1 path ("World.Save.Compat.SessionV90", whose v90
--   bytes embed exactly these fields).
data WorldGenParamsDTOv1 = WorldGenParamsDTOv1
    { gp1Seed                    ∷ !Word64
    , gp1WorldSize               ∷ !Int
    , gp1PlateCount              ∷ !Int
    , gp1Plates                  ∷ ![TectonicPlateDTO]
    , gp1Calender                ∷ !CalendarConfigDTO
    , gp1SunConfig               ∷ !SunConfigDTO
    , gp1MoonConfig              ∷ !MoonConfigDTO
    , gp1GeoTimeline             ∷ !GeoTimeline
    , gp1OceanMap                ∷ !OceanMap
    , gp1OceanDist               ∷ !OceanDistMap
    , gp1ClimateParams           ∷ !ClimateParamsDTO
    , gp1ClimateState            ∷ !ClimateStateDTO
    , gp1ErosionIntensity        ∷ !Float
    , gp1VolcanicActivity        ∷ !Float
    , gp1LavaPoolDepth           ∷ !Int
    , gp1LavaPoolRadius          ∷ !Int
    , gp1WaterfallQuantum        ∷ !Int
    , gp1OreLevers               ∷ !OreLeversDTO
    , gp1TimelineParams          ∷ !TimelineParamsDTO
    , gp1LocationOverlay         ∷ !LocationOverlay
    , gp1LocationContentsSpawned ∷ !(HS.HashSet ChunkCoord)
    , gp1LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gp1LocationDiscovered      ∷ !(HS.HashSet ChunkCoord)
    } deriving (Show, Eq, Generic, Serialize)

-- | Rebuild the live record from a v1 DTO. The instance table comes
--   back EMPTY with the v1 chunk flags held pending
--   ('Location.Instance.pendingLegacyFlags'): turning them into
--   instances needs each definition's bounds / margin / label, and no
--   component decoder has the location registry. The load path resolves
--   them at its content-validation stage — the same stage that already
--   rejects a save naming an unregistered location def — before
--   anything is published.
fromWorldGenParamsDTOv1 ∷ WorldGenParamsDTOv1 → WorldGenParams
fromWorldGenParamsDTOv1 d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gp1Seed d
    , wgpWorldSize               = gp1WorldSize d
    , wgpPlateCount              = gp1PlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gp1Plates d)
    , wgpCalender                = fromCalendarConfigDTO (gp1Calender d)
    , wgpSunConfig               = fromSunConfigDTO (gp1SunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gp1MoonConfig d)
    , wgpGeoTimeline             = gp1GeoTimeline d
    , wgpOceanMap                = gp1OceanMap d
    , wgpOceanDist               = gp1OceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gp1ClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gp1ClimateState d)
    , wgpErosionIntensity        = gp1ErosionIntensity d
    , wgpVolcanicActivity        = gp1VolcanicActivity d
    , wgpLavaPoolDepth           = gp1LavaPoolDepth d
    , wgpLavaPoolRadius          = gp1LavaPoolRadius d
    , wgpWaterfallQuantum        = gp1WaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gp1OreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gp1TimelineParams d)
    , wgpLocationOverlay         = gp1LocationOverlay d
    , wgpLocationInstances       =
        pendingLegacyFlags (gp1LocationDiscovered d) (gp1LocationContentsSpawned d)
    , wgpLocationStamped         = gp1LocationStamped d
    , wgpRiverNames              = emptyRiverNames
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- | Encode a live record into the FROZEN v1 shape. Production NEVER
--   writes v1 — 'worldPagesCodec' always encodes the current version —
--   so this exists solely so the migration tests (and the tracked v90
--   fixture builder) can synthesize a genuine pre-#911 payload to
--   decode. The three location chunk sets are reconstructed from the
--   instance table, which is what a pre-#911 build would have written.
toWorldGenParamsDTOv1 ∷ WorldGenParams → WorldGenParamsDTOv1
toWorldGenParamsDTOv1 p = WorldGenParamsDTOv1
    { gp1Seed                    = wgpSeed p
    , gp1WorldSize               = wgpWorldSize p
    , gp1PlateCount              = wgpPlateCount p
    , gp1Plates                  = map toTectonicPlateDTO (wgpPlates p)
    , gp1Calender                = toCalendarConfigDTO (wgpCalender p)
    , gp1SunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gp1MoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gp1GeoTimeline             = wgpGeoTimeline p
    , gp1OceanMap                = wgpOceanMap p
    , gp1OceanDist               = wgpOceanDist p
    , gp1ClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gp1ClimateState            = toClimateStateDTO (wgpClimateState p)
    , gp1ErosionIntensity        = wgpErosionIntensity p
    , gp1VolcanicActivity        = wgpVolcanicActivity p
    , gp1LavaPoolDepth           = wgpLavaPoolDepth p
    , gp1LavaPoolRadius          = wgpLavaPoolRadius p
    , gp1WaterfallQuantum        = wgpWaterfallQuantum p
    , gp1OreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gp1TimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gp1LocationOverlay         = wgpLocationOverlay p
    , gp1LocationContentsSpawned = HS.fromList
        [ liChunk i | i ← instances, liContentsSpawned i ]
    , gp1LocationStamped         = wgpLocationStamped p
    , gp1LocationDiscovered      = HS.fromList
        [ liChunk i | i ← instances, isDiscoveredLifecycle (liLifecycle i) ]
    }
  where instances = instancesToList (wgpLocationInstances p)
