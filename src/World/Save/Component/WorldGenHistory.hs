{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | HISTORICAL, decode-only worldgen-parameter wire shapes (issue #2098
--   owner split of the #760 frozen worldgen graph).
--
--   Every @WorldGenParamsDTOv1@ … @WorldGenParamsDTOv6@ shape the
--   @"world-pages"@ component still ACCEPTS lives here, newest first,
--   each with the version it was the current shape for:
--
--   - 'WorldGenParamsDTOv6' — @world-pages@ v7, pre-#916 (no encounter).
--   - 'WorldGenParamsDTOv5' — @world-pages@ v6, pre-#1230 (a discovery
--     margin on every instance).
--   - 'WorldGenParamsDTOv4' — @world-pages@ v5, pre-#1104 (no etymology).
--   - 'WorldGenParamsDTOv3' — @world-pages@ v4, pre-#1102 (no river
--     names).
--   - 'WorldGenParamsDTOv2' — @world-pages@ v2 and v3, pre-#1101 (no
--     gloss).
--   - 'WorldGenParamsDTOv1' — @world-pages@ v1, pre-#911 (three
--     chunk-keyed location sets instead of an instance table).
--
--   These declarations are IMMUTABLE. Nothing here may be renamed,
--   reordered, tidied or otherwise rewritten: each one is the literal
--   byte layout of a shipped save, and its @from…@ conversion is the
--   migration into today's live record. The deliberate absences are part
--   of that contract — a field a version never stored stays absent rather
--   than being inferred, and v1's location flags reconstruct through
--   'Location.Instance.pendingLegacyFlags'.
--
--   The @to…@ encoders beside them are exercised by the compatibility
--   suite (through "World.Save.Component.Page", which re-exports them);
--   they are part of the public surface, not module-private helpers.
--
--   This is the TOP of the worldgen owner order: it consumes the leaves
--   in "World.Save.Component.WorldGenClimate" and the location and
--   river-name shapes in "World.Save.Component.WorldGenNaming", and it
--   never refers to the current shape in
--   "World.Save.Component.WorldGenCurrent".
--
--   The frozen-DTO boundary rule is stated ONCE, in
--   "World.Save.Component.Types".
module World.Save.Component.WorldGenHistory
    ( WorldGenParamsDTOv6(..)
    , WorldGenParamsDTOv5(..)
    , WorldGenParamsDTOv4(..)
    , WorldGenParamsDTOv3(..)
    , WorldGenParamsDTOv2(..)
    , WorldGenParamsDTOv1(..)
    , toWorldGenParamsDTOv6
    , fromWorldGenParamsDTOv6
    , toWorldGenParamsDTOv5
    , fromWorldGenParamsDTOv5
    , toWorldGenParamsDTOv4
    , fromWorldGenParamsDTOv4
    , toWorldGenParamsDTOv3
    , fromWorldGenParamsDTOv3
    , toWorldGenParamsDTOv2
    , fromWorldGenParamsDTOv2
    , fromWorldGenParamsDTOv1
    , toWorldGenParamsDTOv1
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Generate.Types (WorldGenParams(..), withVolcanoCtx)
import World.Magma.Types (emptyVolcanoCtx)
import World.Geology.Timeline.Types (GeoTimeline)
import World.Ocean.Types (OceanMap, OceanDistMap)
import Location.Overlay.Types (LocationOverlay)
import World.River.Naming (emptyRiverNames)
import Location.Instance
    ( LocationInstance(..), instancesToList, isDiscoveredLifecycle
    , pendingLegacyFlags )
import World.Chunk.Types (ChunkCoord)
import World.Save.Component.WorldGenClimate
import World.Save.Component.WorldGenNaming

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
