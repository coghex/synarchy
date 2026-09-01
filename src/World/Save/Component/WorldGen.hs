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
    , WorldGenParamsDTOv4(..)
    , WorldGenParamsDTOv5(..)
    , WorldGenParamsDTOv6(..)
    , WorldGenParamsDTOv7(..)
    , NameExprDTO(..)
    , EtymologySourceDTO(..)
    , RiverNameDTO(..)
    , RiverNamesDTO(..)
    , RiverNameDTOv1(..)
    , RiverNamesDTOv1(..)
    , AbsBoundsDTO(..)
    , LocationInstanceDTO(..)
    , LocationInstancesDTO(..)
    , LocationInstanceDTOv1(..)
    , LocationInstancesDTOv1(..)
    , LocationInstanceDTOv2(..)
    , LocationInstancesDTOv2(..)
    , LocationInstanceDTOv3(..)
    , LocationInstancesDTOv3(..)
    , LocationInstanceDTOv4(..)
    , LocationInstancesDTOv4(..)
    , LocationInstanceDTOv5(..)
    , LocationInstancesDTOv5(..)
    , LocationSignificantItemDTO(..)
    , LocationEncounterOccupantDTO(..)
    , LocationEncounterDTO(..)
    , LocationEncounterDTOv1(..)
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
    , fromWorldGenParamsDTOv4
    , toWorldGenParamsDTOv4
    , fromWorldGenParamsDTOv5
    , toWorldGenParamsDTOv5
    , fromWorldGenParamsDTOv6
    , toWorldGenParamsDTOv6
    , fromWorldGenParamsDTOv7
    , toWorldGenParamsDTOv7
    , toEtymologySourceDTO
    , fromEtymologySourceDTO
    , toRiverNamesDTO
    , toLocationInstancesDTOv3
    , toLocationInstancesDTOv4
    , toLocationInstancesDTOv5
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
import Language.Etymology.Source (EtymologySource(..))
import Language.Generated.Types
    ( GeneratorVersion(..), LangSeed(..), LanguageProvenance(..)
    , generatorVersionInt, langSeedWord )
import Language.Semantic.Types (ConceptId, GramNumber, NameExpr(..))
import World.River.Naming (RiverName(..), RiverNames(..), emptyRiverNames)
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstances(..), LocationInstanceId
    , LocationSignificantItem(..)
    , LocationLifecycle, pendingLegacyFlags, instancesToList
    , isDiscoveredLifecycle )
import World.Chunk.Types (ChunkCoord)
import Unit.Types.Manager (UnitId)
import World.Save.Reference (SamePageRef(..))

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

-- Etymology sources (#1104) ------------------------------------------

-- | Frozen mirror of 'Language.Semantic.Types.NameExpr'. Its OWN
--   constructor order is the wire contract, decoupled from the live
--   sum's, exactly like 'World.Save.Component.Page.WorldEditDTO':
--   adding a live constructor makes 'toNameExprDTO' non-exhaustive (a
--   compile error under @-Werror@) rather than silently shifting every
--   stored expression's tag.
--
--   'ConceptId' is reused as a leaf (a newtype over 'Text', like
--   'World.Material.Id.MaterialId'), and 'GramNumber' as a payload-free
--   append-only enum (like 'ZoomMapMode' / 'LocationLifecycle').
data NameExprDTO
    = BareD !ConceptId
    | ModifierD !ConceptId !ConceptId
    | OfD !ConceptId !GramNumber !ConceptId
    | PossessiveD !ConceptId !ConceptId
    deriving (Show, Eq, Generic, Serialize)

toNameExprDTO ∷ NameExpr → NameExprDTO
toNameExprDTO (Bare c)         = BareD c
toNameExprDTO (Modifier m h)   = ModifierD m h
toNameExprDTO (Of h n c)       = OfD h n c
toNameExprDTO (Possessive o h) = PossessiveD o h

fromNameExprDTO ∷ NameExprDTO → NameExpr
fromNameExprDTO (BareD c)         = Bare c
fromNameExprDTO (ModifierD m h)   = Modifier m h
fromNameExprDTO (OfD h n c)       = Of h n c
fromNameExprDTO (PossessiveD o h) = Possessive o h

-- | Frozen mirror of 'Language.Etymology.Source.EtymologySource' (#1104):
--   the expression a generated name was rendered from, plus the language
--   that rendered it.
--
--   The provenance is stored FLAT (seed and version as primitives) rather
--   than as a nested DTO, so this type is reachable from both the page
--   identity and the per-page location / river tables without either
--   half of the component graph having to import the other's leaf.
--   Seed and version still travel together in ONE optional record, so a
--   decode can never produce a seed without a version — the same
--   invariant "World.Save.Component.Page"'s 'LanguageProvenanceDTO'
--   protects.
data EtymologySourceDTO = EtymologySourceDTO
    { esdExpr    ∷ !NameExprDTO
    , esdSeed    ∷ !Word64
    , esdVersion ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toEtymologySourceDTO ∷ EtymologySource → EtymologySourceDTO
toEtymologySourceDTO e = EtymologySourceDTO
    { esdExpr    = toNameExprDTO (esExpr e)
    , esdSeed    = langSeedWord (lpSeed (esLanguage e))
    , esdVersion = generatorVersionInt (lpVersion (esLanguage e))
    }

fromEtymologySourceDTO ∷ EtymologySourceDTO → EtymologySource
fromEtymologySourceDTO d = EtymologySource
    { esExpr     = fromNameExprDTO (esdExpr d)
    , esLanguage = LanguageProvenance
        { lpSeed    = LangSeed (esdSeed d)
        , lpVersion = GeneratorVersion (esdVersion d)
        }
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

-- | Frozen mirror of one persisted encounter occupant. The unit id is a
--   typed same-page reference: absence is tolerated by the shared integrity
--   graph, while resolution on a different page is a hard load error.
data LocationEncounterOccupantDTO = LocationEncounterOccupantDTO
    { leodUnitId              ∷ !(SamePageRef UnitId)
    , leodHomeX               ∷ !Float
    , leodHomeY               ∷ !Float
    , leodEngaged             ∷ !Bool
    , leodReturning           ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of one persisted encounter. This is the CURRENT
--   (@world-pages@ v9) shape: #916's completion and episode state
--   WITHOUT the per-encounter clearance-notice flag, which #917 moved
--   onto the instance ('LocationInstanceDTO.lidClearEventEmitted') so a
--   location authoring significant items and no encounter has one too.
--   'LocationEncounterDTOv1' below is the frozen v8 shape that still
--   carries it.
data LocationEncounterDTO = LocationEncounterDTO
    { ledRolledCount        ∷ !Int
    , ledOccupants          ∷ ![LocationEncounterOccupantDTO]
    , ledRosterComplete     ∷ !Bool
    , ledDeathOnlyClearance ∷ !Bool
    , ledActivated          ∷ !Bool
    , ledEpisodeActive      ∷ !Bool
    , ledAggressionAnnounced ∷ !Bool
    , ledDisengageAnnounced  ∷ !Bool
    , ledCleared            ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN @world-pages@ v8 encounter shape (#916 through #917),
--   preserved verbatim: every current field plus the
--   @clear_event_emitted@ flag #917 generalized onto the instance.
--   Decode-only; 'fromLocationInstanceDTOv5' lifts the flag to where it
--   now lives, so a historical save neither replays nor loses its
--   deferred clearance notice. Never edited.
data LocationEncounterDTOv1 = LocationEncounterDTOv1
    { led1RolledCount        ∷ !Int
    , led1Occupants          ∷ ![LocationEncounterOccupantDTO]
    , led1RosterComplete     ∷ !Bool
    , led1DeathOnlyClearance ∷ !Bool
    , led1Activated          ∷ !Bool
    , led1EpisodeActive      ∷ !Bool
    , led1AggressionAnnounced ∷ !Bool
    , led1DisengageAnnounced  ∷ !Bool
    , led1Cleared            ∷ !Bool
    , led1ClearEventEmitted  ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

toLocationEncounterDTOv1 ∷ Bool → LocationEncounter → LocationEncounterDTOv1
toLocationEncounterDTOv1 clearEventEmitted e = LocationEncounterDTOv1
    { led1RolledCount        = leRolledCount e
    , led1Occupants          = map toOccupantDTO (leOccupants e)
    , led1RosterComplete     = leRosterComplete e
    , led1DeathOnlyClearance = leDeathOnlyClearance e
    , led1Activated          = leActivated e
    , led1EpisodeActive      = leEpisodeActive e
    , led1AggressionAnnounced = leAggressionAnnounced e
    , led1DisengageAnnounced  = leDisengageAnnounced e
    , led1Cleared            = leCleared e
    , led1ClearEventEmitted  = clearEventEmitted
    }

fromLocationEncounterDTOv1 ∷ LocationEncounterDTOv1 → LocationEncounter
fromLocationEncounterDTOv1 d = LocationEncounter
    { leRolledCount        = led1RolledCount d
    , leOccupants          = map fromOccupantDTO (led1Occupants d)
    , leRosterComplete     = led1RosterComplete d
    , leDeathOnlyClearance = led1DeathOnlyClearance d
    , leActivated          = led1Activated d
    , leEpisodeActive      = led1EpisodeActive d
    , leAggressionAnnounced = led1AggressionAnnounced d
    , leDisengageAnnounced  = led1DisengageAnnounced d
    , leCleared            = led1Cleared d
    }

-- | The ONE occupant conversion every encounter DTO version shares —
--   'LocationEncounterOccupantDTO' is unchanged by #917, so the current
--   and frozen shapes must not grow two copies of it that could drift.
toOccupantDTO ∷ LocationEncounterOccupant → LocationEncounterOccupantDTO
toOccupantDTO o = LocationEncounterOccupantDTO
    { leodUnitId              = SamePageRef (leoUnitId o)
    , leodHomeX               = fst (leoHome o)
    , leodHomeY               = snd (leoHome o)
    , leodEngaged             = leoEngaged o
    , leodReturning           = leoReturning o
    }

fromOccupantDTO ∷ LocationEncounterOccupantDTO → LocationEncounterOccupant
fromOccupantDTO o = LocationEncounterOccupant
    { leoUnitId              = unSamePageRef (leodUnitId o)
    , leoHome                = (leodHomeX o, leodHomeY o)
    , leoEngaged             = leodEngaged o
    , leoReturning           = leodReturning o
    }

toLocationEncounterDTO ∷ LocationEncounter → LocationEncounterDTO
toLocationEncounterDTO e = LocationEncounterDTO
    { ledRolledCount        = leRolledCount e
    , ledOccupants          = map toOccupantDTO (leOccupants e)
    , ledRosterComplete     = leRosterComplete e
    , ledDeathOnlyClearance = leDeathOnlyClearance e
    , ledActivated          = leActivated e
    , ledEpisodeActive      = leEpisodeActive e
    , ledAggressionAnnounced = leAggressionAnnounced e
    , ledDisengageAnnounced  = leDisengageAnnounced e
    , ledCleared            = leCleared e
    }

fromLocationEncounterDTO ∷ LocationEncounterDTO → LocationEncounter
fromLocationEncounterDTO d = LocationEncounter
    { leRolledCount        = ledRolledCount d
    , leOccupants          = map fromOccupantDTO (ledOccupants d)
    , leRosterComplete     = ledRosterComplete d
    , leDeathOnlyClearance = ledDeathOnlyClearance d
    , leActivated          = ledActivated d
    , leEpisodeActive      = ledEpisodeActive d
    , leAggressionAnnounced = ledAggressionAnnounced d
    , leDisengageAnnounced  = ledDisengageAnnounced d
    , leCleared            = ledCleared d
    }

-- | Frozen mirror of one 'Location.Instance.LocationSignificantItem'
--   (#917). @lsidInstanceId@ is a typed same-page reference to the
--   PHYSICAL item ('Item.Types.iiInstanceId'), absent until the content
--   spawn binds one — see "World.Save.Integrity" for what its
--   resolution is required to look like on each side of the taken
--   latch.
data LocationSignificantItemDTO = LocationSignificantItemDTO
    { lsidSlot        ∷ !Int
    , lsidItemDefName ∷ !Text
    , lsidInstanceId  ∷ !(Maybe (SamePageRef Word64))
    , lsidTaken       ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

toLocationSignificantItemDTO
    ∷ LocationSignificantItem → LocationSignificantItemDTO
toLocationSignificantItemDTO e = LocationSignificantItemDTO
    { lsidSlot        = lsiSlot e
    , lsidItemDefName = lsiItemDefName e
    , lsidInstanceId  = SamePageRef <$> lsiInstanceId e
    , lsidTaken       = lsiTaken e
    }

fromLocationSignificantItemDTO
    ∷ LocationSignificantItemDTO → LocationSignificantItem
fromLocationSignificantItemDTO d = LocationSignificantItem
    { lsiSlot        = lsidSlot d
    , lsiItemDefName = lsidItemDefName d
    , lsiInstanceId  = unSamePageRef <$> lsidInstanceId d
    , lsiTaken       = lsidTaken d
    }

-- | Frozen mirror of 'Location.Instance.LocationInstance' — a LIVE
--   gameplay record by construction (its lifecycle and content-spawn
--   flag are mutated in place, and the expedition arc adds encounter /
--   loot / progression fields to it), so the boundary rule
--   ("World.Save.Component.Types") requires an explicit field-by-field
--   conversion rather than embedding it. 'LocationInstanceId' is a leaf
--   id and 'LocationLifecycle' a payload-free append-only enum, both
--   reused as-is exactly like 'ChunkCoord' / 'ZoomMapMode'.
--
--   This is the CURRENT shape, carried by @world-pages@ v9: #1101's
--   English gloss beside the display name and #1104's optional
--   etymology source, no discovery margin since #1230, #916's optional
--   persistent encounter, and #917's guaranteed significant-item
--   obligations plus the generalized clearance-notice latch.
--   'LocationInstanceDTOv5' below is the frozen pre-#917 shape (v8),
--   'LocationInstanceDTOv4' the pre-#916 one (v7),
--   'LocationInstanceDTOv3' the pre-#1230 shape (v6),
--   'LocationInstanceDTOv2' the pre-#1104 one and
--   'LocationInstanceDTOv1' the pre-#1101 one.
data LocationInstanceDTO = LocationInstanceDTO
    { lidId              ∷ !LocationInstanceId
    , lidDefId           ∷ !Text
    , lidChunk           ∷ !ChunkCoord
    , lidAnchorX         ∷ !Int
    , lidAnchorY         ∷ !Int
    , lidBounds          ∷ !AbsBoundsDTO
    , lidDisplayName     ∷ !Text
    , lidGloss           ∷ !(Maybe Text)
    , lidEtymology       ∷ !(Maybe EtymologySourceDTO)
    , lidLifecycle       ∷ !LocationLifecycle
    , lidContentsSpawned ∷ !Bool
    , lidEncounter       ∷ !(Maybe LocationEncounterDTO)
    , lidSignificant     ∷ ![LocationSignificantItemDTO]
    , lidClearEventEmitted ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstanceDTO ∷ LocationInstance → LocationInstanceDTO
toLocationInstanceDTO i = LocationInstanceDTO
    { lidId              = liId i
    , lidDefId           = liDefId i
    , lidChunk           = liChunk i
    , lidAnchorX         = fst (liAnchor i)
    , lidAnchorY         = snd (liAnchor i)
    , lidBounds          = toAbsBoundsDTO (liBounds i)
    , lidDisplayName     = liDisplayName i
    , lidGloss           = liGloss i
    , lidEtymology       = toEtymologySourceDTO <$> liEtymology i
    , lidLifecycle       = liLifecycle i
    , lidContentsSpawned = liContentsSpawned i
    , lidEncounter       = toLocationEncounterDTO <$> liEncounter i
    , lidSignificant     = map toLocationSignificantItemDTO (liSignificant i)
    , lidClearEventEmitted = liClearEventEmitted i
    }

fromLocationInstanceDTO ∷ LocationInstanceDTO → LocationInstance
fromLocationInstanceDTO d = LocationInstance
    { liId              = lidId d
    , liDefId           = lidDefId d
    , liChunk           = lidChunk d
    , liAnchor          = (lidAnchorX d, lidAnchorY d)
    , liBounds          = fromAbsBoundsDTO (lidBounds d)
    , liDisplayName     = lidDisplayName d
    , liGloss           = lidGloss d
    , liEtymology       = fromEtymologySourceDTO <$> lidEtymology d
    , liLifecycle       = lidLifecycle d
    , liContentsSpawned = lidContentsSpawned d
    , liEncounter       = fromLocationEncounterDTO <$> lidEncounter d
    , liSignificant     = map fromLocationSignificantItemDTO (lidSignificant d)
    , liClearEventEmitted = lidClearEventEmitted d
    }

-- | The FROZEN @world-pages@ v8 location shape (#916 through #917),
--   preserved verbatim: every current field except #917's significant
--   obligations and instance-level clearance-notice latch, with the
--   notice still nested inside the encounter
--   ('LocationEncounterDTOv1').
--
--   Historical instances migrate with @liSignificant = []@ — inventing
--   an obligation from today's YAML would leave a materialized world
--   owing an item it never spawned, which nothing could ever take —
--   and their notice is lifted OUT of the encounter to where it now
--   lives, so a save carrying a defeated-but-unannounced ruin still
--   announces it exactly once, and one that already announced does not
--   announce again. A v8 instance with no encounter authored no
--   clearance condition and never cleared, so it decodes unspent and
--   'Location.Instance.locationAuthorsClearance' keeps it that way.
data LocationInstanceDTOv5 = LocationInstanceDTOv5
    { lid5Id              ∷ !LocationInstanceId
    , lid5DefId           ∷ !Text
    , lid5Chunk           ∷ !ChunkCoord
    , lid5AnchorX         ∷ !Int
    , lid5AnchorY         ∷ !Int
    , lid5Bounds          ∷ !AbsBoundsDTO
    , lid5DisplayName     ∷ !Text
    , lid5Gloss           ∷ !(Maybe Text)
    , lid5Etymology       ∷ !(Maybe EtymologySourceDTO)
    , lid5Lifecycle       ∷ !LocationLifecycle
    , lid5ContentsSpawned ∷ !Bool
    , lid5Encounter       ∷ !(Maybe LocationEncounterDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

fromLocationInstanceDTOv5 ∷ LocationInstanceDTOv5 → LocationInstance
fromLocationInstanceDTOv5 d = LocationInstance
    { liId              = lid5Id d
    , liDefId           = lid5DefId d
    , liChunk           = lid5Chunk d
    , liAnchor          = (lid5AnchorX d, lid5AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid5Bounds d)
    , liDisplayName     = lid5DisplayName d
    , liGloss           = lid5Gloss d
    , liEtymology       = fromEtymologySourceDTO <$> lid5Etymology d
    , liLifecycle       = lid5Lifecycle d
    , liContentsSpawned = lid5ContentsSpawned d
    , liEncounter       = fromLocationEncounterDTOv1 <$> lid5Encounter d
    , liSignificant     = []
    , liClearEventEmitted =
        maybe False led1ClearEventEmitted (lid5Encounter d)
    }

data LocationInstancesDTOv5 = LocationInstancesDTOv5
    { lisd5NextId ∷ !Int
    , lisd5ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv5)
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstancesDTOv5 ∷ LocationInstances → LocationInstancesDTOv5
toLocationInstancesDTOv5 l = LocationInstancesDTOv5
    { lisd5NextId = lisNextId l
    , lisd5ById   = HM.map toV5 (lisById l)
    }
  where
    toV5 i = LocationInstanceDTOv5
        { lid5Id              = liId i
        , lid5DefId           = liDefId i
        , lid5Chunk           = liChunk i
        , lid5AnchorX         = fst (liAnchor i)
        , lid5AnchorY         = snd (liAnchor i)
        , lid5Bounds          = toAbsBoundsDTO (liBounds i)
        , lid5DisplayName     = liDisplayName i
        , lid5Gloss           = liGloss i
        , lid5Etymology       = toEtymologySourceDTO <$> liEtymology i
        , lid5Lifecycle       = liLifecycle i
        , lid5ContentsSpawned = liContentsSpawned i
        , lid5Encounter       =
            toLocationEncounterDTOv1 (liClearEventEmitted i) <$> liEncounter i
        }

fromLocationInstancesDTOv5 ∷ LocationInstancesDTOv5 → LocationInstances
fromLocationInstancesDTOv5 d = LocationInstances
    { lisNextId        = lisd5NextId d
    , lisById          = HM.map fromLocationInstanceDTOv5 (lisd5ById d)
    , lisPendingLegacy = Nothing
    }

-- | Frozen mirror of the per-page instance table: its allocator plus
--   its instances. 'Location.Instance.lisPendingLegacy' has no field
--   here on purpose — it is a transient v1-migration carry that can
--   never be true of anything on disk, so @fromLocationInstancesDTO@
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

-- | The FROZEN @world-pages@ v7 location shape (#1230 through #916),
--   preserved verbatim: every current field except the encounter added in
--   v8. Historical instances migrate with @liEncounter = Nothing@; inventing
--   a roll during load would make the loader/content version choose gameplay.
data LocationInstanceDTOv4 = LocationInstanceDTOv4
    { lid4Id              ∷ !LocationInstanceId
    , lid4DefId           ∷ !Text
    , lid4Chunk           ∷ !ChunkCoord
    , lid4AnchorX         ∷ !Int
    , lid4AnchorY         ∷ !Int
    , lid4Bounds          ∷ !AbsBoundsDTO
    , lid4DisplayName     ∷ !Text
    , lid4Gloss           ∷ !(Maybe Text)
    , lid4Etymology       ∷ !(Maybe EtymologySourceDTO)
    , lid4Lifecycle       ∷ !LocationLifecycle
    , lid4ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

fromLocationInstanceDTOv4 ∷ LocationInstanceDTOv4 → LocationInstance
fromLocationInstanceDTOv4 d = LocationInstance
    { liId              = lid4Id d
    , liDefId           = lid4DefId d
    , liChunk           = lid4Chunk d
    , liAnchor          = (lid4AnchorX d, lid4AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid4Bounds d)
    , liDisplayName     = lid4DisplayName d
    , liGloss           = lid4Gloss d
    , liEtymology       = fromEtymologySourceDTO <$> lid4Etymology d
    , liLifecycle       = lid4Lifecycle d
    , liContentsSpawned = lid4ContentsSpawned d
    , liEncounter       = Nothing
    , liSignificant     = []
    , liClearEventEmitted = False
    }

data LocationInstancesDTOv4 = LocationInstancesDTOv4
    { lisd4NextId ∷ !Int
    , lisd4ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv4)
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstancesDTOv4 ∷ LocationInstances → LocationInstancesDTOv4
toLocationInstancesDTOv4 l = LocationInstancesDTOv4
    { lisd4NextId = lisNextId l
    , lisd4ById   = HM.map toV4 (lisById l)
    }
  where
    toV4 i = LocationInstanceDTOv4
        { lid4Id              = liId i
        , lid4DefId           = liDefId i
        , lid4Chunk           = liChunk i
        , lid4AnchorX         = fst (liAnchor i)
        , lid4AnchorY         = snd (liAnchor i)
        , lid4Bounds          = toAbsBoundsDTO (liBounds i)
        , lid4DisplayName     = liDisplayName i
        , lid4Gloss           = liGloss i
        , lid4Etymology       = toEtymologySourceDTO <$> liEtymology i
        , lid4Lifecycle       = liLifecycle i
        , lid4ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv4 ∷ LocationInstancesDTOv4 → LocationInstances
fromLocationInstancesDTOv4 d = LocationInstances
    { lisNextId        = lisd4NextId d
    , lisById          = HM.map fromLocationInstanceDTOv4 (lisd4ById d)
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
--   and its stored @discovery_margin@ is DROPPED (#1230): the live
--   record has no such field any more, and the wire shape above is
--   frozen, so the value is decoded off the wire and discarded rather
--   than the historical bytes being edited.
--
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
    , liDisplayName     = lid1DisplayName d
    , liGloss           = Nothing
    , liEtymology       = Nothing
    , liLifecycle       = lid1Lifecycle d
    , liContentsSpawned = lid1ContentsSpawned d
    , liEncounter       = Nothing
    , liSignificant     = []
    , liClearEventEmitted = False
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
        , lid1DiscoveryMargin = historicalDiscoveryMargin
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

-- | The FROZEN pre-#1104 instance shape, preserved verbatim for
--   decode-only backward compatibility: everything the current DTO
--   carries except the etymology source. This is what @world-pages@ v4
--   (#1101) and v5 (#1102) both encoded — #1102 changed the PAGE's
--   river table, not the instance — so both versions share one instance
--   shape. Never edited; a further change freezes the CURRENT shape as
--   a v3 instead (frozen-DTO boundary rule).
data LocationInstanceDTOv2 = LocationInstanceDTOv2
    { lid2Id              ∷ !LocationInstanceId
    , lid2DefId           ∷ !Text
    , lid2Chunk           ∷ !ChunkCoord
    , lid2AnchorX         ∷ !Int
    , lid2AnchorY         ∷ !Int
    , lid2Bounds          ∷ !AbsBoundsDTO
    , lid2DiscoveryMargin ∷ !Int
    , lid2DisplayName     ∷ !Text
    , lid2Gloss           ∷ !(Maybe Text)
    , lid2Lifecycle       ∷ !LocationLifecycle
    , lid2ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | A pre-#1104 instance keeps its stored name AND gloss exactly, its
--   stored @discovery_margin@ is dropped (#1230, exactly as in
--   'fromLocationInstanceDTOv1'), and
--   decodes with NO etymology source. The expression behind a name was
--   simply not recorded then; inventing one would attach a fabricated
--   derivation to a real location, which #1104 requirement 1 forbids as
--   explicitly as #1101 forbids inventing a gloss.
fromLocationInstanceDTOv2 ∷ LocationInstanceDTOv2 → LocationInstance
fromLocationInstanceDTOv2 d = LocationInstance
    { liId              = lid2Id d
    , liDefId           = lid2DefId d
    , liChunk           = lid2Chunk d
    , liAnchor          = (lid2AnchorX d, lid2AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid2Bounds d)
    , liDisplayName     = lid2DisplayName d
    , liGloss           = lid2Gloss d
    , liEtymology       = Nothing
    , liLifecycle       = lid2Lifecycle d
    , liContentsSpawned = lid2ContentsSpawned d
    , liEncounter       = Nothing
    , liSignificant     = []
    , liClearEventEmitted = False
    }

-- | The FROZEN pre-#1104 instance table. Structurally identical to
--   'LocationInstancesDTO' but over the frozen per-instance shape.
data LocationInstancesDTOv2 = LocationInstancesDTOv2
    { lisd2NextId ∷ !Int
    , lisd2ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv2)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen table — the round-trip partner every frozen
--   DTO version's tests build fixture bytes with.
toLocationInstancesDTOv2 ∷ LocationInstances → LocationInstancesDTOv2
toLocationInstancesDTOv2 l = LocationInstancesDTOv2
    { lisd2NextId = lisNextId l
    , lisd2ById   = HM.map toV2 (lisById l)
    }
  where
    toV2 i = LocationInstanceDTOv2
        { lid2Id              = liId i
        , lid2DefId           = liDefId i
        , lid2Chunk           = liChunk i
        , lid2AnchorX         = fst (liAnchor i)
        , lid2AnchorY         = snd (liAnchor i)
        , lid2Bounds          = toAbsBoundsDTO (liBounds i)
        , lid2DiscoveryMargin = historicalDiscoveryMargin
        , lid2DisplayName     = liDisplayName i
        , lid2Gloss           = liGloss i
        , lid2Lifecycle       = liLifecycle i
        , lid2ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv2 ∷ LocationInstancesDTOv2 → LocationInstances
fromLocationInstancesDTOv2 d = LocationInstances
    { lisNextId        = lisd2NextId d
    , lisById          = HM.map fromLocationInstanceDTOv2 (lisd2ById d)
    , lisPendingLegacy = Nothing
    }

-- | The FROZEN pre-#1230 instance shape, preserved verbatim for
--   decode-only backward compatibility: everything the current DTO
--   carries PLUS the @discovery_margin@ the live record used to store
--   (#911) and lost when reveal became sight-based. This is what
--   @world-pages@ v6 (#1104) encoded. Never edited; a further change
--   freezes the CURRENT shape as a v4 instead (frozen-DTO boundary
--   rule).
data LocationInstanceDTOv3 = LocationInstanceDTOv3
    { lid3Id              ∷ !LocationInstanceId
    , lid3DefId           ∷ !Text
    , lid3Chunk           ∷ !ChunkCoord
    , lid3AnchorX         ∷ !Int
    , lid3AnchorY         ∷ !Int
    , lid3Bounds          ∷ !AbsBoundsDTO
    , lid3DiscoveryMargin ∷ !Int
    , lid3DisplayName     ∷ !Text
    , lid3Gloss           ∷ !(Maybe Text)
    , lid3Etymology       ∷ !(Maybe EtymologySourceDTO)
    , lid3Lifecycle       ∷ !LocationLifecycle
    , lid3ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | A pre-#1230 instance carries EVERYTHING across unchanged —
--   allocator id, definition id, chunk, anchor, bounds, display name,
--   gloss, etymology source, lifecycle and content-spawn flag — and
--   drops exactly one thing: its stored discovery margin, which has no
--   live counterpart any more (#1230 requirement 11). Reveal is
--   sight-based against 'liBounds', so the halo the margin described
--   describes nothing; it is decoded off the wire and discarded rather
--   than being remapped onto some other field.
fromLocationInstanceDTOv3 ∷ LocationInstanceDTOv3 → LocationInstance
fromLocationInstanceDTOv3 d = LocationInstance
    { liId              = lid3Id d
    , liDefId           = lid3DefId d
    , liChunk           = lid3Chunk d
    , liAnchor          = (lid3AnchorX d, lid3AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid3Bounds d)
    , liDisplayName     = lid3DisplayName d
    , liGloss           = lid3Gloss d
    , liEtymology       = fromEtymologySourceDTO <$> lid3Etymology d
    , liLifecycle       = lid3Lifecycle d
    , liContentsSpawned = lid3ContentsSpawned d
    , liEncounter       = Nothing
    , liSignificant     = []
    , liClearEventEmitted = False
    }

-- | The FROZEN pre-#1230 instance table. Structurally identical to
--   'LocationInstancesDTO' but over the frozen per-instance shape.
data LocationInstancesDTOv3 = LocationInstancesDTOv3
    { lisd3NextId ∷ !Int
    , lisd3ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv3)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen table — the round-trip partner every frozen
--   DTO version's tests build fixture bytes with.
toLocationInstancesDTOv3 ∷ LocationInstances → LocationInstancesDTOv3
toLocationInstancesDTOv3 l = LocationInstancesDTOv3
    { lisd3NextId = lisNextId l
    , lisd3ById   = HM.map toV3 (lisById l)
    }
  where
    toV3 i = LocationInstanceDTOv3
        { lid3Id              = liId i
        , lid3DefId           = liDefId i
        , lid3Chunk           = liChunk i
        , lid3AnchorX         = fst (liAnchor i)
        , lid3AnchorY         = snd (liAnchor i)
        , lid3Bounds          = toAbsBoundsDTO (liBounds i)
        , lid3DiscoveryMargin = historicalDiscoveryMargin
        , lid3DisplayName     = liDisplayName i
        , lid3Gloss           = liGloss i
        , lid3Etymology       = toEtymologySourceDTO <$> liEtymology i
        , lid3Lifecycle       = liLifecycle i
        , lid3ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv3 ∷ LocationInstancesDTOv3 → LocationInstances
fromLocationInstancesDTOv3 d = LocationInstances
    { lisNextId        = lisd3NextId d
    , lisById          = HM.map fromLocationInstanceDTOv3 (lisd3ById d)
    , lisPendingLegacy = Nothing
    }

-- | The discovery margin every FROZEN instance encoder writes (#1230).
--
--   Those encoders exist only to build fixture bytes for the historical
--   wire shapes, and they are handed a LIVE 'LocationInstance', which no
--   longer records a margin at all. There is therefore nothing truthful
--   to copy: zero is written as the honest "no margin recorded", and it
--   is never read back — every @fromLocationInstanceDTOv{1,2,3}@ drops
--   the field. A fixture that needs to prove a NONZERO historical margin
--   survives its migration constructs the frozen DTO directly instead,
--   which is exactly what the v6→v7 migration test does.
historicalDiscoveryMargin ∷ Int
historicalDiscoveryMargin = 0

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
    , rvdEtymology   ∷ !(Maybe EtymologySourceDTO)
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
    , rvdEtymology   = toEtymologySourceDTO <$> rvnEtymology n
    }

-- | Rebuild the live record. A stored name is carried across EXACTLY —
--   never re-rendered from the page's language, which is the whole
--   point of #708 principle 5 (a river named under one catalogue keeps
--   that name even after the catalogue grows).
fromRiverNameDTO ∷ RiverNameDTO → RiverName
fromRiverNameDTO d = RiverName
    { rvnDisplayName = rvdDisplayName d
    , rvnGloss       = rvdGloss d
    , rvnEtymology   = fromEtymologySourceDTO <$> rvdEtymology d
    }

toRiverNamesDTO ∷ RiverNames → RiverNamesDTO
toRiverNamesDTO = RiverNamesDTO . HM.map toRiverNameDTO . rvnById

fromRiverNamesDTO ∷ RiverNamesDTO → RiverNames
fromRiverNamesDTO = RiverNames . HM.map fromRiverNameDTO . rvdById

-- | The FROZEN pre-#1104 river-name shape (@world-pages@ v5), preserved
--   verbatim for decode-only backward compatibility: the stored name and
--   its gloss, no etymology source. Never edited; a further change
--   freezes the CURRENT shape as a v2 instead (frozen-DTO boundary rule).
data RiverNameDTOv1 = RiverNameDTOv1
    { rvd1DisplayName ∷ !Text
    , rvd1Gloss       ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the pre-#1104 per-page table.
newtype RiverNamesDTOv1 = RiverNamesDTOv1
    { rvd1ById ∷ HM.HashMap GeoFeatureId RiverNameDTOv1 }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | A pre-#1104 river keeps its stored name and gloss EXACTLY and
--   decodes with no etymology source — the same honest absence a
--   pre-#1104 location instance decodes with.
fromRiverNameDTOv1 ∷ RiverNameDTOv1 → RiverName
fromRiverNameDTOv1 d = RiverName
    { rvnDisplayName = rvd1DisplayName d
    , rvnGloss       = rvd1Gloss d
    , rvnEtymology   = Nothing
    }

-- | Encoder for the frozen table — the round-trip partner a frozen-DTO
--   fixture is built with.
toRiverNamesDTOv1 ∷ RiverNames → RiverNamesDTOv1
toRiverNamesDTOv1 = RiverNamesDTOv1 . HM.map toV1 . rvnById
  where
    toV1 n = RiverNameDTOv1
        { rvd1DisplayName = rvnDisplayName n
        , rvd1Gloss       = rvnGloss n
        }

fromRiverNamesDTOv1 ∷ RiverNamesDTOv1 → RiverNames
fromRiverNamesDTOv1 = RiverNames . HM.map fromRiverNameDTOv1 . rvd1ById

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
--   This is the CURRENT shape, carried by @world-pages@ v7: #1102's
--   per-page river-name table beside the location instances, both
--   carrying #1104's optional etymology source, over the #1230 instance
--   shape that no longer stores a discovery margin.
--   'WorldGenParamsDTOv5' below is the frozen shape @world-pages@ v6
--   carries (the same tables, instances still carrying that margin),
--   'WorldGenParamsDTOv4' the frozen shape v5
--   carries (river names and instances, no etymology),
--   'WorldGenParamsDTOv3' the frozen shape v4 carries (#1101's
--   per-instance gloss, no river names), 'WorldGenParamsDTOv2' the
--   frozen shape v2 and v3 carry (the #911 instance table, no gloss),
--   and 'WorldGenParamsDTOv1' the frozen pre-#911 shape (three
--   chunk-keyed location sets); all five are decode-only.
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

-- Frozen pre-#916 worldgen params (@world-pages@ v7) ----------------

-- | The FROZEN world-pages v7 worldgen shape. It is byte-for-byte the former
--   current shape, except that its location table is named explicitly as the frozen
--   pre-encounter 'LocationInstancesDTOv4'.
data WorldGenParamsDTOv6 = WorldGenParamsDTOv6
    { gp6Seed                    ∷ !Word64
    , gp6WorldSize               ∷ !Int
    , gp6PlateCount              ∷ !Int
    , gp6Plates                  ∷ ![TectonicPlateDTO]
    , gp6Calender                ∷ !CalendarConfigDTO
    , gp6SunConfig               ∷ !SunConfigDTO
    , gp6MoonConfig              ∷ !MoonConfigDTO
    , gp6GeoTimeline             ∷ !GeoTimeline
    , gp6OceanMap                ∷ !OceanMap
    , gp6OceanDist               ∷ !OceanDistMap
    , gp6ClimateParams           ∷ !ClimateParamsDTO
    , gp6ClimateState            ∷ !ClimateStateDTO
    , gp6ErosionIntensity        ∷ !Float
    , gp6VolcanicActivity        ∷ !Float
    , gp6LavaPoolDepth           ∷ !Int
    , gp6LavaPoolRadius          ∷ !Int
    , gp6WaterfallQuantum        ∷ !Int
    , gp6OreLevers               ∷ !OreLeversDTO
    , gp6TimelineParams          ∷ !TimelineParamsDTO
    , gp6LocationOverlay         ∷ !LocationOverlay
    , gp6LocationInstances       ∷ !LocationInstancesDTOv4
    , gp6LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gp6RiverNames              ∷ !RiverNamesDTO
    } deriving (Show, Eq, Generic, Serialize)

toWorldGenParamsDTOv6 ∷ WorldGenParams → WorldGenParamsDTOv6
toWorldGenParamsDTOv6 p = WorldGenParamsDTOv6
    { gp6Seed                    = wgpSeed p
    , gp6WorldSize               = wgpWorldSize p
    , gp6PlateCount              = wgpPlateCount p
    , gp6Plates                  = map toTectonicPlateDTO (wgpPlates p)
    , gp6Calender                = toCalendarConfigDTO (wgpCalender p)
    , gp6SunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gp6MoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gp6GeoTimeline             = wgpGeoTimeline p
    , gp6OceanMap                = wgpOceanMap p
    , gp6OceanDist               = wgpOceanDist p
    , gp6ClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gp6ClimateState            = toClimateStateDTO (wgpClimateState p)
    , gp6ErosionIntensity        = wgpErosionIntensity p
    , gp6VolcanicActivity        = wgpVolcanicActivity p
    , gp6LavaPoolDepth           = wgpLavaPoolDepth p
    , gp6LavaPoolRadius          = wgpLavaPoolRadius p
    , gp6WaterfallQuantum        = wgpWaterfallQuantum p
    , gp6OreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gp6TimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gp6LocationOverlay         = wgpLocationOverlay p
    , gp6LocationInstances       = toLocationInstancesDTOv4
                                      (wgpLocationInstances p)
    , gp6LocationStamped         = wgpLocationStamped p
    , gp6RiverNames              = toRiverNamesDTO (wgpRiverNames p)
    }

fromWorldGenParamsDTOv6 ∷ WorldGenParamsDTOv6 → WorldGenParams
fromWorldGenParamsDTOv6 d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gp6Seed d
    , wgpWorldSize               = gp6WorldSize d
    , wgpPlateCount              = gp6PlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gp6Plates d)
    , wgpCalender                = fromCalendarConfigDTO (gp6Calender d)
    , wgpSunConfig               = fromSunConfigDTO (gp6SunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gp6MoonConfig d)
    , wgpGeoTimeline             = gp6GeoTimeline d
    , wgpOceanMap                = gp6OceanMap d
    , wgpOceanDist               = gp6OceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gp6ClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gp6ClimateState d)
    , wgpErosionIntensity        = gp6ErosionIntensity d
    , wgpVolcanicActivity        = gp6VolcanicActivity d
    , wgpLavaPoolDepth           = gp6LavaPoolDepth d
    , wgpLavaPoolRadius          = gp6LavaPoolRadius d
    , wgpWaterfallQuantum        = gp6WaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gp6OreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gp6TimelineParams d)
    , wgpLocationOverlay         = gp6LocationOverlay d
    , wgpLocationInstances       = fromLocationInstancesDTOv4
                                      (gp6LocationInstances d)
    , wgpLocationStamped         = gp6LocationStamped d
    , wgpRiverNames              = fromRiverNamesDTO (gp6RiverNames d)
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#917 worldgen params (@world-pages@ v8) -----------------

-- | The FROZEN @world-pages@ v8 gen-params shape: identical to
--   'WorldGenParamsDTO' except that its location table is the frozen
--   pre-significant-contents 'LocationInstancesDTOv5'. Never edited; a
--   further schema change freezes the CURRENT shape as
--   @WorldGenParamsDTOv8@ instead (frozen-DTO boundary rule).
data WorldGenParamsDTOv7 = WorldGenParamsDTOv7
    { gp7Seed                    ∷ !Word64
    , gp7WorldSize               ∷ !Int
    , gp7PlateCount              ∷ !Int
    , gp7Plates                  ∷ ![TectonicPlateDTO]
    , gp7Calender                ∷ !CalendarConfigDTO
    , gp7SunConfig               ∷ !SunConfigDTO
    , gp7MoonConfig              ∷ !MoonConfigDTO
    , gp7GeoTimeline             ∷ !GeoTimeline
    , gp7OceanMap                ∷ !OceanMap
    , gp7OceanDist               ∷ !OceanDistMap
    , gp7ClimateParams           ∷ !ClimateParamsDTO
    , gp7ClimateState            ∷ !ClimateStateDTO
    , gp7ErosionIntensity        ∷ !Float
    , gp7VolcanicActivity        ∷ !Float
    , gp7LavaPoolDepth           ∷ !Int
    , gp7LavaPoolRadius          ∷ !Int
    , gp7WaterfallQuantum        ∷ !Int
    , gp7OreLevers               ∷ !OreLeversDTO
    , gp7TimelineParams          ∷ !TimelineParamsDTO
    , gp7LocationOverlay         ∷ !LocationOverlay
    , gp7LocationInstances       ∷ !LocationInstancesDTOv5
    , gp7LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gp7RiverNames              ∷ !RiverNamesDTO
    } deriving (Show, Eq, Generic, Serialize)

toWorldGenParamsDTOv7 ∷ WorldGenParams → WorldGenParamsDTOv7
toWorldGenParamsDTOv7 p = WorldGenParamsDTOv7
    { gp7Seed                     = wgpSeed p
    , gp7WorldSize                = wgpWorldSize p
    , gp7PlateCount               = wgpPlateCount p
    , gp7Plates                   = map toTectonicPlateDTO (wgpPlates p)
    , gp7Calender                 = toCalendarConfigDTO (wgpCalender p)
    , gp7SunConfig                = toSunConfigDTO (wgpSunConfig p)
    , gp7MoonConfig               = toMoonConfigDTO (wgpMoonConfig p)
    , gp7GeoTimeline              = wgpGeoTimeline p
    , gp7OceanMap                 = wgpOceanMap p
    , gp7OceanDist                = wgpOceanDist p
    , gp7ClimateParams            = toClimateParamsDTO (wgpClimateParams p)
    , gp7ClimateState             = toClimateStateDTO (wgpClimateState p)
    , gp7ErosionIntensity         = wgpErosionIntensity p
    , gp7VolcanicActivity         = wgpVolcanicActivity p
    , gp7LavaPoolDepth            = wgpLavaPoolDepth p
    , gp7LavaPoolRadius           = wgpLavaPoolRadius p
    , gp7WaterfallQuantum         = wgpWaterfallQuantum p
    , gp7OreLevers                = toOreLeversDTO (wgpOreLevers p)
    , gp7TimelineParams           = toTimelineParamsDTO (wgpTimelineParams p)
    , gp7LocationOverlay          = wgpLocationOverlay p
    , gp7LocationInstances        = toLocationInstancesDTOv5 (wgpLocationInstances p)
    , gp7LocationStamped          = wgpLocationStamped p
    , gp7RiverNames               = toRiverNamesDTO (wgpRiverNames p)
    }

fromWorldGenParamsDTOv7 ∷ WorldGenParamsDTOv7 → WorldGenParams
fromWorldGenParamsDTOv7 d = withVolcanoCtx WorldGenParams
    { wgpSeed                     = gp7Seed d
    , wgpWorldSize                = gp7WorldSize d
    , wgpPlateCount               = gp7PlateCount d
    , wgpPlates                   = map fromTectonicPlateDTO (gp7Plates d)
    , wgpCalender                 = fromCalendarConfigDTO (gp7Calender d)
    , wgpSunConfig                = fromSunConfigDTO (gp7SunConfig d)
    , wgpMoonConfig               = fromMoonConfigDTO (gp7MoonConfig d)
    , wgpGeoTimeline              = gp7GeoTimeline d
    , wgpOceanMap                 = gp7OceanMap d
    , wgpOceanDist                = gp7OceanDist d
    , wgpClimateParams            = fromClimateParamsDTO (gp7ClimateParams d)
    , wgpClimateState             = fromClimateStateDTO (gp7ClimateState d)
    , wgpErosionIntensity         = gp7ErosionIntensity d
    , wgpVolcanicActivity         = gp7VolcanicActivity d
    , wgpLavaPoolDepth            = gp7LavaPoolDepth d
    , wgpLavaPoolRadius           = gp7LavaPoolRadius d
    , wgpWaterfallQuantum         = gp7WaterfallQuantum d
    , wgpOreLevers                = fromOreLeversDTO (gp7OreLevers d)
    , wgpTimelineParams           = fromTimelineParamsDTO (gp7TimelineParams d)
    , wgpLocationOverlay          = gp7LocationOverlay d
    , wgpLocationInstances        = fromLocationInstancesDTOv5 (gp7LocationInstances d)
    , wgpLocationStamped          = gp7LocationStamped d
    , wgpRiverNames               = fromRiverNamesDTO (gp7RiverNames d)
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#1230 worldgen params (@world-pages@ v6) ----------------

-- | The FROZEN pre-#1230 shape of 'WorldGenParamsDTO', preserved
--   verbatim for decode-only backward compatibility: identical to the
--   current type except that its location instances are the frozen
--   'LocationInstancesDTOv3', whose per-instance shape still carries a
--   @discovery_margin@. This is what @world-pages@ v6 (#1104) encoded.
--   Never edited; a further gen-params change freezes the CURRENT type
--   as a v6 instead (frozen-DTO boundary rule).
data WorldGenParamsDTOv5 = WorldGenParamsDTOv5
    { gp5Seed                    ∷ !Word64
    , gp5WorldSize               ∷ !Int
    , gp5PlateCount              ∷ !Int
    , gp5Plates                  ∷ ![TectonicPlateDTO]
    , gp5Calender                ∷ !CalendarConfigDTO
    , gp5SunConfig               ∷ !SunConfigDTO
    , gp5MoonConfig              ∷ !MoonConfigDTO
    , gp5GeoTimeline             ∷ !GeoTimeline
    , gp5OceanMap                ∷ !OceanMap
    , gp5OceanDist               ∷ !OceanDistMap
    , gp5ClimateParams           ∷ !ClimateParamsDTO
    , gp5ClimateState            ∷ !ClimateStateDTO
    , gp5ErosionIntensity        ∷ !Float
    , gp5VolcanicActivity        ∷ !Float
    , gp5LavaPoolDepth           ∷ !Int
    , gp5LavaPoolRadius          ∷ !Int
    , gp5WaterfallQuantum        ∷ !Int
    , gp5OreLevers               ∷ !OreLeversDTO
    , gp5TimelineParams          ∷ !TimelineParamsDTO
    , gp5LocationOverlay         ∷ !LocationOverlay
    , gp5LocationInstances       ∷ !LocationInstancesDTOv3
    , gp5LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gp5RiverNames              ∷ !RiverNamesDTO
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen v6 shape — the round-trip partner a
--   frozen-DTO fixture is built with (the same reason
--   'toWorldGenParamsDTOv4' exists). Every instance it writes carries
--   'historicalDiscoveryMargin', since the live record it reads from no
--   longer has one.
toWorldGenParamsDTOv5 ∷ WorldGenParams → WorldGenParamsDTOv5
toWorldGenParamsDTOv5 p = WorldGenParamsDTOv5
    { gp5Seed                    = wgpSeed p
    , gp5WorldSize               = wgpWorldSize p
    , gp5PlateCount              = wgpPlateCount p
    , gp5Plates                  = map toTectonicPlateDTO (wgpPlates p)
    , gp5Calender                = toCalendarConfigDTO (wgpCalender p)
    , gp5SunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gp5MoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gp5GeoTimeline             = wgpGeoTimeline p
    , gp5OceanMap                = wgpOceanMap p
    , gp5OceanDist               = wgpOceanDist p
    , gp5ClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gp5ClimateState            = toClimateStateDTO (wgpClimateState p)
    , gp5ErosionIntensity        = wgpErosionIntensity p
    , gp5VolcanicActivity        = wgpVolcanicActivity p
    , gp5LavaPoolDepth           = wgpLavaPoolDepth p
    , gp5LavaPoolRadius          = wgpLavaPoolRadius p
    , gp5WaterfallQuantum        = wgpWaterfallQuantum p
    , gp5OreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gp5TimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gp5LocationOverlay         = wgpLocationOverlay p
    , gp5LocationInstances       = toLocationInstancesDTOv3 (wgpLocationInstances p)
    , gp5LocationStamped         = wgpLocationStamped p
    , gp5RiverNames              = toRiverNamesDTO (wgpRiverNames p)
    }

-- | Decode the frozen v6 shape. Everything rides across untouched
--   except each location instance's stored discovery margin, which is
--   dropped ('fromLocationInstanceDTOv3'): #1230 made reveal
--   sight-based against the instance's own bounds, so the margin has no
--   live counterpart to restore into. Names, glosses, etymology
--   sources, lifecycles, content-spawn flags, river names, terrain and
--   climate are all unchanged.
fromWorldGenParamsDTOv5 ∷ WorldGenParamsDTOv5 → WorldGenParams
fromWorldGenParamsDTOv5 d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gp5Seed d
    , wgpWorldSize               = gp5WorldSize d
    , wgpPlateCount              = gp5PlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gp5Plates d)
    , wgpCalender                = fromCalendarConfigDTO (gp5Calender d)
    , wgpSunConfig               = fromSunConfigDTO (gp5SunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gp5MoonConfig d)
    , wgpGeoTimeline             = gp5GeoTimeline d
    , wgpOceanMap                = gp5OceanMap d
    , wgpOceanDist               = gp5OceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gp5ClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gp5ClimateState d)
    , wgpErosionIntensity        = gp5ErosionIntensity d
    , wgpVolcanicActivity        = gp5VolcanicActivity d
    , wgpLavaPoolDepth           = gp5LavaPoolDepth d
    , wgpLavaPoolRadius          = gp5LavaPoolRadius d
    , wgpWaterfallQuantum        = gp5WaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gp5OreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gp5TimelineParams d)
    , wgpLocationOverlay         = gp5LocationOverlay d
    , wgpLocationInstances       = fromLocationInstancesDTOv3 (gp5LocationInstances d)
    , wgpLocationStamped         = gp5LocationStamped d
    , wgpRiverNames              = fromRiverNamesDTO (gp5RiverNames d)
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#1104 worldgen params (@world-pages@ v5) ----------------

-- | The FROZEN pre-#1104 shape of 'WorldGenParamsDTO', preserved
--   verbatim for decode-only backward compatibility: identical to the
--   current type except that its location instances and river names are
--   the frozen pre-etymology 'LocationInstancesDTOv2' /
--   'RiverNamesDTOv1'. This is what @world-pages@ v5 (#1102) encoded.
--   Never edited; a further gen-params change freezes the CURRENT type
--   as a v5 instead (frozen-DTO boundary rule).
data WorldGenParamsDTOv4 = WorldGenParamsDTOv4
    { gp4Seed                    ∷ !Word64
    , gp4WorldSize               ∷ !Int
    , gp4PlateCount              ∷ !Int
    , gp4Plates                  ∷ ![TectonicPlateDTO]
    , gp4Calender                ∷ !CalendarConfigDTO
    , gp4SunConfig               ∷ !SunConfigDTO
    , gp4MoonConfig              ∷ !MoonConfigDTO
    , gp4GeoTimeline             ∷ !GeoTimeline
    , gp4OceanMap                ∷ !OceanMap
    , gp4OceanDist               ∷ !OceanDistMap
    , gp4ClimateParams           ∷ !ClimateParamsDTO
    , gp4ClimateState            ∷ !ClimateStateDTO
    , gp4ErosionIntensity        ∷ !Float
    , gp4VolcanicActivity        ∷ !Float
    , gp4LavaPoolDepth           ∷ !Int
    , gp4LavaPoolRadius          ∷ !Int
    , gp4WaterfallQuantum        ∷ !Int
    , gp4OreLevers               ∷ !OreLeversDTO
    , gp4TimelineParams          ∷ !TimelineParamsDTO
    , gp4LocationOverlay         ∷ !LocationOverlay
    , gp4LocationInstances       ∷ !LocationInstancesDTOv2
    , gp4LocationStamped         ∷ !(HS.HashSet ChunkCoord)
    , gp4RiverNames              ∷ !RiverNamesDTOv1
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen v5 shape — the round-trip partner a
--   frozen-DTO fixture is built with (the same reason
--   'toWorldGenParamsDTOv3' exists).
toWorldGenParamsDTOv4 ∷ WorldGenParams → WorldGenParamsDTOv4
toWorldGenParamsDTOv4 p = WorldGenParamsDTOv4
    { gp4Seed                    = wgpSeed p
    , gp4WorldSize               = wgpWorldSize p
    , gp4PlateCount              = wgpPlateCount p
    , gp4Plates                  = map toTectonicPlateDTO (wgpPlates p)
    , gp4Calender                = toCalendarConfigDTO (wgpCalender p)
    , gp4SunConfig               = toSunConfigDTO (wgpSunConfig p)
    , gp4MoonConfig              = toMoonConfigDTO (wgpMoonConfig p)
    , gp4GeoTimeline             = wgpGeoTimeline p
    , gp4OceanMap                = wgpOceanMap p
    , gp4OceanDist               = wgpOceanDist p
    , gp4ClimateParams           = toClimateParamsDTO (wgpClimateParams p)
    , gp4ClimateState            = toClimateStateDTO (wgpClimateState p)
    , gp4ErosionIntensity        = wgpErosionIntensity p
    , gp4VolcanicActivity        = wgpVolcanicActivity p
    , gp4LavaPoolDepth           = wgpLavaPoolDepth p
    , gp4LavaPoolRadius          = wgpLavaPoolRadius p
    , gp4WaterfallQuantum        = wgpWaterfallQuantum p
    , gp4OreLevers               = toOreLeversDTO (wgpOreLevers p)
    , gp4TimelineParams          = toTimelineParamsDTO (wgpTimelineParams p)
    , gp4LocationOverlay         = wgpLocationOverlay p
    , gp4LocationInstances       = toLocationInstancesDTOv2
                                      (wgpLocationInstances p)
    , gp4LocationStamped         = wgpLocationStamped p
    , gp4RiverNames              = toRiverNamesDTOv1 (wgpRiverNames p)
    }

-- | Decode the frozen v5 shape. Names and glosses — the page's
--   locations' and its rivers' alike — carry across EXACTLY, and every
--   one of them comes back with NO etymology source: the expression
--   behind a name was not recorded before #1104, and it is never
--   inferred from the name, the gloss, or the definition afterwards
--   (#1104 requirement 1).
fromWorldGenParamsDTOv4 ∷ WorldGenParamsDTOv4 → WorldGenParams
fromWorldGenParamsDTOv4 d = withVolcanoCtx WorldGenParams
    { wgpSeed                    = gp4Seed d
    , wgpWorldSize               = gp4WorldSize d
    , wgpPlateCount              = gp4PlateCount d
    , wgpPlates                  = map fromTectonicPlateDTO (gp4Plates d)
    , wgpCalender                = fromCalendarConfigDTO (gp4Calender d)
    , wgpSunConfig               = fromSunConfigDTO (gp4SunConfig d)
    , wgpMoonConfig              = fromMoonConfigDTO (gp4MoonConfig d)
    , wgpGeoTimeline             = gp4GeoTimeline d
    , wgpOceanMap                = gp4OceanMap d
    , wgpOceanDist               = gp4OceanDist d
    , wgpClimateParams           = fromClimateParamsDTO (gp4ClimateParams d)
    , wgpClimateState            = fromClimateStateDTO (gp4ClimateState d)
    , wgpErosionIntensity        = gp4ErosionIntensity d
    , wgpVolcanicActivity        = gp4VolcanicActivity d
    , wgpLavaPoolDepth           = gp4LavaPoolDepth d
    , wgpLavaPoolRadius          = gp4LavaPoolRadius d
    , wgpWaterfallQuantum        = gp4WaterfallQuantum d
    , wgpOreLevers               = fromOreLeversDTO (gp4OreLevers d)
    , wgpTimelineParams          = fromTimelineParamsDTO (gp4TimelineParams d)
    , wgpLocationOverlay         = gp4LocationOverlay d
    , wgpLocationInstances       = fromLocationInstancesDTOv2
                                      (gp4LocationInstances d)
    , wgpLocationStamped         = gp4LocationStamped d
    , wgpRiverNames              = fromRiverNamesDTOv1 (gp4RiverNames d)
    , wgpVolcanoCtx              = emptyVolcanoCtx
    }

-- Frozen pre-#1102 worldgen params (@world-pages@ v4) ----------------

-- | The FROZEN pre-#1102 shape of 'WorldGenParamsDTO', preserved
--   verbatim for decode-only backward compatibility: identical to the
--   current type except that it carries no river-name table, and that
--   its instance table is the frozen pre-#1104 'LocationInstancesDTOv2'.
--   This is what @world-pages@ v4 (#1101) encoded. Never edited; a
--   further gen-params change freezes the CURRENT type as a v5 instead
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
    , gp3LocationInstances       ∷ !LocationInstancesDTOv2
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
    , gp3LocationInstances       = toLocationInstancesDTOv2
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
    , wgpLocationInstances       = fromLocationInstancesDTOv2
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
--   instances needs each definition's bounds / label (since #1230 there
--   is no margin to resolve), and no
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
