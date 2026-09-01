{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The CURRENT worldgen-parameter wire shape (issue #2098 owner split of
--   the #760 frozen worldgen graph).
--
--   This owner holds exactly one declaration family: 'WorldGenParamsDTO',
--   the shape the @"world-pages"@ component WRITES today, and its
--   exhaustive field-by-field conversions to and from the live
--   'World.Generate.Types.WorldGenParams' record. Keeping it alone here
--   is the point — a field added to the live record surfaces as a
--   compile error in one small module rather than somewhere inside a
--   file that also carries six decode-only historical shapes.
--
--   It sits above the leaf owners and below the historical one:
--   "World.Save.Component.WorldGenClimate" supplies the generation and
--   climate mirrors it embeds, "World.Save.Component.WorldGenNaming" the
--   location and river-name tables, and nothing in
--   "World.Save.Component.WorldGenHistory" refers to this shape.
--
--   The frozen-DTO boundary rule is stated ONCE, in
--   "World.Save.Component.Types".
module World.Save.Component.WorldGenCurrent
    ( WorldGenParamsDTO(..)
    , toWorldGenParamsDTO
    , fromWorldGenParamsDTO
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
import World.Chunk.Types (ChunkCoord)
import World.Save.Component.WorldGenClimate
import World.Save.Component.WorldGenNaming

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
