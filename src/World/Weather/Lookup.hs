{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Lookup
    ( -- * Climate interpolation
      lookupLocalClimate
      -- * Shared coordinate math for custom interpolation
    , RegionGridCoords(..)
    , regionGridCoords
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , ClimateCoord(..), climateRegionSize)

-- | Pre-computed bilinear interpolation grid coordinates.
--   Shared between climate and erosion lookups.
data RegionGridCoords = RegionGridCoords
    { rgcRU0 ∷ !Int
    , rgcRU1 ∷ !Int
    , rgcRV0 ∷ !Int
    , rgcRV1 ∷ !Int
    , rgcTU  ∷ !Float
    , rgcTV  ∷ !Float
    } deriving (Show)

-- | Convert tile coordinates (gx, gy) to region grid interpolation
--   coordinates with proper u-axis wrapping and v-axis clamping.
--   @chunkSz@ is the chunk size in tiles (typically 16).
{-# INLINE regionGridCoords #-}
regionGridCoords ∷ Int → Int → Int → Int → RegionGridCoords
regionGridCoords chunkSz worldSize gx gy =
    let halfChunks = worldSize `div` 2
        w = worldSize * chunkSz
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW

        regionsPerSide = worldSize `div` climateRegionSize

        -- Continuous region coordinates
        fCS  = fromIntegral chunkSz ∷ Float
        fCRS = fromIntegral climateRegionSize ∷ Float
        hcF  = fromIntegral halfChunks ∷ Float
        ruF  = (fromIntegral wrappedU / fCS + hcF) / fCRS
        rvF  = (fromIntegral v / fCS + hcF) / fCRS

        -- Center-based interpolation
        ruC = ruF - 0.5
        rvC = rvF - 0.5
        ru0raw = floor ruC ∷ Int
        rv0raw = floor rvC ∷ Int
        ru0 = ((ru0raw `mod` regionsPerSide) + regionsPerSide) `mod` regionsPerSide
        ru1 = (ru0 + 1) `mod` regionsPerSide
        rv0 = max 0 (min (regionsPerSide - 1) rv0raw)
        rv1 = max 0 (min (regionsPerSide - 1) (rv0raw + 1))
        tu = ruC - fromIntegral ru0raw
        tv = rvC - fromIntegral rv0raw
    in RegionGridCoords ru0 ru1 rv0 rv1 tu tv

-- | Look up regional climate for a global tile coordinate.
--   Uses bilinear interpolation between the 4 nearest region centers
--   to eliminate hard grid boundaries.
--   Returns (temperature, precipitation, humidity, snowFraction).
{-# INLINE lookupLocalClimate #-}
lookupLocalClimate ∷ ClimateState → Int → Int → Int
                   → (Float, Float, Float, Float)
lookupLocalClimate climate worldSize gx gy =
    let regions = cgRegions (csClimate climate)
        RegionGridCoords ru0 ru1 rv0 rv1 tu tv =
            regionGridCoords chunkSz worldSize gx gy

        lookupRC ru rv =
            case HM.lookup (ClimateCoord ru rv) regions of
                Just rc →
                    let SeasonalClimate st wt = rcAirTemp rc
                        SeasonalClimate sp wp = rcPrecipitation rc
                    in ((st + wt) / 2.0, (sp + wp) / 2.0
                       , rcHumidity rc, rcPrecipType rc)
                Nothing →
                    (csGlobalTemp climate, 0.5, 0.5, 0.0)

        (t00, p00, h00, s00) = lookupRC ru0 rv0
        (t10, p10, h10, s10) = lookupRC ru1 rv0
        (t01, p01, h01, s01) = lookupRC ru0 rv1
        (t11, p11, h11, s11) = lookupRC ru1 rv1

        lerpF a b t = a + t * (b - a)
        temp   = lerpF (lerpF t00 t10 tu) (lerpF t01 t11 tu) tv
        precip = lerpF (lerpF p00 p10 tu) (lerpF p01 p11 tu) tv
        humid  = lerpF (lerpF h00 h10 tu) (lerpF h01 h11 tu) tv
        snow   = lerpF (lerpF s00 s10 tu) (lerpF s01 s11 tu) tv
    in (temp, precip, humid, snow)
  where
    chunkSz = 16
