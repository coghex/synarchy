{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Generate.Freshwater
    ( extractFreshwaterSources
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import World.Weather.Types
import World.Chunk.Types (chunkSize)
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , RiverSegment(..), LakeParams(..))
import World.Geology.Timeline.Types (PersistentFeature(..)
                                    , FeatureShape(..), FeatureActivity(..))
import World.Base (GeoCoord(..))

-- * Freshwater Moisture Sources

-- | Convert tile-space coordinates to a ClimateCoord using the (u,v)
--   convention: u = gx - gy (east-west), v = gx + gy (north-south).
--   Wraps u and clamps rv to match regionGridCoords in Lookup.hs.
geoToClimateCoord ∷ Int → Int → Int → ClimateCoord
geoToClimateCoord worldSize gx gy =
    let w = worldSize * chunkSize
        halfW = w `div` 2
        regionsPerSide = climateRegionCount worldSize
        regionSizeTiles = climateRegionSize * chunkSize
        u = gx - gy
        v = gx + gy
        -- Wrap u to [-halfW, halfW), matching regionGridCoords
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
        ru = (wrappedU + halfW) `div` regionSizeTiles
        -- Clamp rv to valid range (v-axis doesn't wrap)
        rvRaw = (v + halfW) `div` regionSizeTiles
        rv = max 0 (min (regionsPerSide - 1) rvRaw)
    in ClimateCoord ru rv

-- | Extract freshwater moisture sources from persistent features.
--   Lakes contribute based on area, rivers based on flow rate.
--   Returns a map of climate regions to moisture weights (0-1).
extractFreshwaterSources ∷ Int → [PersistentFeature]
                         → HM.HashMap ClimateCoord Float
extractFreshwaterSources worldSize features =
    let entries = concatMap (featureToMoisture worldSize) features
    in HM.fromListWith max entries

featureToMoisture ∷ Int → PersistentFeature → [(ClimateCoord, Float)]
featureToMoisture worldSize pf
    | pfActivity pf ≢ FActive = []
    | otherwise = case pfFeature pf of
        HydroShape (RiverFeature rp) → riverMoisture worldSize rp
        HydroShape (LakeFeature lp)  → lakeMoisture worldSize lp
        _                            → []

riverMoisture ∷ Int → RiverParams → [(ClimateCoord, Float)]
riverMoisture worldSize rp =
    map (\seg →
        let GeoCoord sx sy = rsStart seg
            GeoCoord ex ey = rsEnd seg
            mx = (sx + ex) `div` 2
            my = (sy + ey) `div` 2
            coord = geoToClimateCoord worldSize mx my
            weight = min 1.0 (rsFlowRate seg * 2.0)
        in (coord, weight)
        ) (V.toList (rpSegments rp))

lakeMoisture ∷ Int → LakeParams → [(ClimateCoord, Float)]
lakeMoisture worldSize lp =
    let GeoCoord cx cy = lkCenter lp
        coord = geoToClimateCoord worldSize cx cy
        area = fromIntegral (lkRadius lp * lkRadius lp) ∷ Float
        weight = min 1.0 (area / 2500.0)
    in [(coord, weight)]
