{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Generate
    ( -- * Initialization (post-bombardment, early-Earth-like)
      initEarlyClimate
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Weather.Types
import World.Chunk.Types (ChunkCoord(..))
import World.Ocean.Types (OceanMap)

-- | Initialize climate state with early-Earth-like conditions.
--   Called after tectonic plates and primordial bombardment are done.
--
--   Climate regions are indexed in (u, v) space where:
--     u = cx - cy   (east-west, wraps cylindrically)
--     v = cx + cy   (north-south, bounded by glacier)
--
--   This ensures latitude bands run horizontally on screen
--   instead of diagonally along the cx or cy axis.
--
--   ClimateCoord (ru, rv) covers chunk-u in
--     [ru * climateRegionSize - halfChunks .. (ru+1)*climateRegionSize - 1 - halfChunks]
--   and chunk-v in the same range for rv.
initEarlyClimate ∷ Int          -- ^ worldSize (in chunks)
                 → OceanMap     -- ^ which chunks are ocean
                 → ClimateState
initEarlyClimate worldSize oceanMap =
    let regionsPerSide = worldSize `div` climateRegionSize
        halfRegions    = regionsPerSide `div` 2

        allCoords = [ ClimateCoord ru rv
                    | ru ← [0 .. regionsPerSide - 1]
                    , rv ← [0 .. regionsPerSide - 1]
                    ]

        -- Check if a climate region overlaps any ocean chunk.
        -- We check a sample of chunk coords that fall inside this
        -- region's (u, v) bounding box.
        halfChunks = worldSize `div` 2
        isOceanRegion (ClimateCoord ru rv) =
            let u0 = ru * climateRegionSize - halfChunks
                v0 = rv * climateRegionSize - halfChunks
            in any (\(du, dv) →
                    let u = u0 + du
                        v = v0 + dv
                    in if even (u + v)
                       then let cx = (u + v) `div` 2
                                cy = (v - u) `div` 2
                            in HS.member (ChunkCoord cx cy) oceanMap
                       else False
                   )
                   [ (du, dv) | du ← [0 .. climateRegionSize - 1]
                               , dv ← [0 .. climateRegionSize - 1] ]

        -- Build ocean cells for ocean regions
        oceanEntries = [ (coord, mkOceanCell coord)
                       | coord ← allCoords
                       , isOceanRegion coord
                       ]

        -- Build climate entries for all regions
        climateEntries = [ (coord, mkRegionClimate coord (isOceanRegion coord))
                         | coord ← allCoords
                         ]

        mkOceanCell ∷ ClimateCoord → OceanCell
        mkOceanCell (ClimateCoord _ru rv) =
            let -- Latitude: rv=0 is one pole, rv=regionsPerSide-1 is the other
                latRatio = abs (fromIntegral (rv - halfRegions))
                         / max 1 (fromIntegral halfRegions) ∷ Float
                baseSSTSummer = 18.0 + (1.0 - latRatio) * 15.0
                baseSSTWinter = baseSSTSummer - 5.0 * latRatio
                baseSalinity = 35.0
                iceThreshold = 0.75 ∷ Float
                iceCover = if latRatio > iceThreshold
                           then (latRatio - iceThreshold) / (1.0 - iceThreshold)
                           else 0.0
            in OceanCell
                { ocTemperature = SeasonalClimate baseSSTSummer baseSSTWinter
                , ocSalinity    = baseSalinity
                , ocDepth       = 200
                , ocCurrentDir  = 0.0
                , ocCurrentSpd  = 0.0
                , ocUpwelling   = 0.0
                , ocIceCover    = iceCover
                }

        mkRegionClimate ∷ ClimateCoord → Bool → RegionClimate
        mkRegionClimate (ClimateCoord _ru rv) isOcean =
            let latRatio = abs (fromIntegral (rv - halfRegions))
                         / max 1 (fromIntegral halfRegions) ∷ Float
                baseTempSummer = 20.0 + (1.0 - latRatio) * 15.0 - latRatio * 25.0
                baseTempWinter = baseTempSummer - 10.0 * latRatio
                baseHumidity = if isOcean then 0.8 else 0.4
                basePrecip = if latRatio < 0.2 then 0.7
                             else if latRatio < 0.5 then 0.4
                             else if latRatio < 0.8 then 0.5
                             else 0.2
                avgTemp = (baseTempSummer + baseTempWinter) / 2.0
                snowFrac = clamp01 ((5.0 - avgTemp) / 15.0)
            in RegionClimate
                { rcAirTemp        = SeasonalClimate baseTempSummer baseTempWinter
                , rcHumidity       = baseHumidity
                , rcPrecipitation  = SeasonalClimate basePrecip (basePrecip * 0.8)
                , rcPrecipType     = snowFrac
                , rcEvaporation    = if isOcean then 0.6 else 0.3
                , rcCloudCover     = baseHumidity * 0.6
                , rcPressure       = 1.0
                , rcWindDir        = 0.0
                , rcWindSpeed      = 0.0
                , rcOrographicLift = 0.0
                , rcContinentality = if isOcean then 0.0 else 0.5
                , rcAlbedo         = if isOcean then 0.06 else 0.25
                , rcElevAvg        = 0
                }

    in ClimateState
        { csClimate    = ClimateGrid
            { cgRegions = HM.fromList climateEntries
            , cgSize    = regionsPerSide
            }
        , csOcean      = OceanGrid
            { ogCells     = HM.fromList oceanEntries
            , ogDeepWater = HM.empty
            , ogCurrents  = []
            , ogThcCells  = []
            }
        , csAtmo       = emptyAtmoGrid
        , csSurface    = HM.empty
        , csGlobalCO2  = 1.0
        , csGlobalTemp = 0.0
        , csSolarConst = 1.0
        }
