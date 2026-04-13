{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Lookup
    ( -- * Climate interpolation
      lookupLocalClimate
    , lookupWaterTable
    , LocalClimate(..)
      -- * Shared coordinate math for custom interpolation
    , RegionGridCoords(..)
    , regionGridCoords
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , ClimateCoord(..), climateRegionSize)

-- | Interpolated climate data at a specific tile coordinate.
data LocalClimate = LocalClimate
    { lcTemp       ∷ !Float  -- ^ Annual mean temperature (°C)
    , lcSummerTemp ∷ !Float  -- ^ Summer mean temperature (°C)
    , lcWinterTemp ∷ !Float  -- ^ Winter mean temperature (°C)
    , lcPrecip     ∷ !Float  -- ^ Annual mean precipitation (0–1)
    , lcHumidity   ∷ !Float  -- ^ Relative humidity (0–1)
    , lcSnow       ∷ !Float  -- ^ Fraction of precipitation as snow (0–1)
    } deriving (Show)

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
{-# INLINE lookupLocalClimate #-}
lookupLocalClimate ∷ ClimateState → Int → Int → Int → LocalClimate
lookupLocalClimate climate worldSize gx gy =
    let regions = cgRegions (csClimate climate)
        RegionGridCoords ru0 ru1 rv0 rv1 tu tv =
            regionGridCoords chunkSz worldSize gx gy

        defTemp = csGlobalTemp climate

        lookupRC ru rv =
            case HM.lookup (ClimateCoord ru rv) regions of
                Just rc →
                    let SeasonalClimate st wt = rcAirTemp rc
                        SeasonalClimate sp wp = rcPrecipitation rc
                    in ((st + wt) / 2.0, st, wt
                       , (sp + wp) / 2.0
                       , rcHumidity rc, rcPrecipType rc)
                Nothing →
                    (defTemp, defTemp, defTemp, 0.5, 0.5, 0.0)

        (t00, st00, wt00, p00, h00, s00) = lookupRC ru0 rv0
        (t10, st10, wt10, p10, h10, s10) = lookupRC ru1 rv0
        (t01, st01, wt01, p01, h01, s01) = lookupRC ru0 rv1
        (t11, st11, wt11, p11, h11, s11) = lookupRC ru1 rv1

        lerpF a b t = a + t * (b - a)
        temp    = lerpF (lerpF t00  t10  tu) (lerpF t01  t11  tu) tv
        summer  = lerpF (lerpF st00 st10 tu) (lerpF st01 st11 tu) tv
        winter  = lerpF (lerpF wt00 wt10 tu) (lerpF wt01 wt11 tu) tv
        precip  = lerpF (lerpF p00  p10  tu) (lerpF p01  p11  tu) tv
        humid   = lerpF (lerpF h00  h10  tu) (lerpF h01  h11  tu) tv
        snow    = lerpF (lerpF s00  s10  tu) (lerpF s01  s11  tu) tv
    in LocalClimate temp summer winter precip humid snow
  where
    chunkSz = 16

-- | Look up the water table level at a global tile coordinate.
--   Returns (summerWT, winterWT) as Ints (z-levels).
--   Uses the same bilinear interpolation as lookupLocalClimate.
{-# INLINE lookupWaterTable #-}
lookupWaterTable ∷ ClimateState → Int → Int → Int → (Int, Int)
lookupWaterTable climate worldSize gx gy =
    let regions = cgRegions (csClimate climate)
        RegionGridCoords ru0 ru1 rv0 rv1 tu tv =
            regionGridCoords chunkSz worldSize gx gy

        lookupWT ru rv =
            case HM.lookup (ClimateCoord ru rv) regions of
                Just rc →
                    let SeasonalClimate ws ww = rcWaterTable rc
                    in (ws, ww)
                Nothing → (0.0, 0.0)

        (ws00, ww00) = lookupWT ru0 rv0
        (ws10, ww10) = lookupWT ru1 rv0
        (ws01, ww01) = lookupWT ru0 rv1
        (ws11, ww11) = lookupWT ru1 rv1

        lerpF a b t = a + t * (b - a)
        wtSummer = lerpF (lerpF ws00 ws10 tu) (lerpF ws01 ws11 tu) tv
        wtWinter = lerpF (lerpF ww00 ww10 tu) (lerpF ww01 ww11 tu) tv
    in (round wtSummer, round wtWinter)
  where
    chunkSz = 16
