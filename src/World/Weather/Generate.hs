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
--   Ocean cells: uniform 4°C, 35 ppt salinity, no currents.
--   Land regions: uniform 20°C, moderate humidity.
--   No wind/moisture/surface budgets yet — those come from simulation.
--
--   The OceanMap tells us which 4×4-chunk climate regions contain ocean.
--   We classify a climate region as ocean if ANY of its chunks are in the OceanMap.
initEarlyClimate ∷ Int          -- ^ worldSize (in chunks)
                 → OceanMap     -- ^ which chunks are ocean
                 → ClimateState
initEarlyClimate worldSize oceanMap =
    let regionsPerSide = worldSize `div` climateRegionSize
        halfRegions    = regionsPerSide `div` 2

        allCoords = [ ClimateCoord rx ry
                    | rx ← [0 .. regionsPerSide - 1]
                    , ry ← [0 .. regionsPerSide - 1]
                    ]

        -- Check if a climate region overlaps any ocean chunk.
        -- A climate region at (rx, ry) covers chunks:
        --   cx ∈ [rx * climateRegionSize - halfChunks .. (rx+1) * climateRegionSize - 1 - halfChunks]
        --   cy ∈ [ry * climateRegionSize - halfChunks .. (ry+1) * climateRegionSize - 1 - halfChunks]
        halfChunks = worldSize `div` 2
        isOceanRegion (ClimateCoord rx ry) =
            let cx0 = rx * climateRegionSize - halfChunks
                cy0 = ry * climateRegionSize - halfChunks
            in any (\(dx, dy) → HS.member (ChunkCoord (cx0 + dx) (cy0 + dy)) oceanMap)
                   [ (dx, dy) | dx ← [0 .. climateRegionSize - 1]
                               , dy ← [0 .. climateRegionSize - 1] ]

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
        mkOceanCell (ClimateCoord _rx ry) =
            let -- Latitude: ry=0 is one pole, ry=regionsPerSide-1 is the other
                latRatio = abs (fromIntegral (ry - halfRegions))
                         / max 1 (fromIntegral halfRegions) ∷ Float
                -- Polar oceans are colder, tropical are slightly warmer
                -- But early Earth: uniform ~4°C deep, surface varies slightly
                baseSSTSummer = 4.0 + (1.0 - latRatio) * 2.0   -- 4-6°C
                baseSSTWinter = 4.0 + (1.0 - latRatio) * 1.0   -- 4-5°C
                -- Higher salinity in subtropics (evaporation), lower at poles (ice melt)
                baseSalinity = 35.0
                -- Ice cover at high latitudes
                iceThreshold = 0.75 ∷ Float
                iceCover = if latRatio > iceThreshold
                           then (latRatio - iceThreshold) / (1.0 - iceThreshold)
                           else 0.0
            in OceanCell
                { ocTemperature = SeasonalClimate baseSSTSummer baseSSTWinter
                , ocSalinity    = baseSalinity
                , ocDepth       = 200          -- generic deep ocean
                , ocCurrentDir  = 0.0          -- no currents yet
                , ocCurrentSpd  = 0.0
                , ocUpwelling   = 0.0
                , ocIceCover    = iceCover
                }

        mkRegionClimate ∷ ClimateCoord → Bool → RegionClimate
        mkRegionClimate (ClimateCoord _rx ry) isOcean =
            let latRatio = abs (fromIntegral (ry - halfRegions))
                         / max 1 (fromIntegral halfRegions) ∷ Float
                -- Simple latitude-based temperature: 20°C baseline
                -- Hot at equator (~30°C), cold at poles (~-10°C)
                baseTempSummer = 20.0 + (1.0 - latRatio) * 15.0 - latRatio * 25.0
                baseTempWinter = baseTempSummer - 10.0 * latRatio  -- bigger swing at poles
                -- Ocean regions are more humid
                baseHumidity = if isOcean then 0.8 else 0.4
                -- Precipitation: higher in tropics and mid-latitudes
                basePrecip = if latRatio < 0.2 then 0.7       -- tropical
                             else if latRatio < 0.5 then 0.4  -- temperate
                             else if latRatio < 0.8 then 0.5  -- mid-lat storm belt
                             else 0.2                          -- polar desert
                -- Snow fraction: based on temperature
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
                , rcWindDir        = 0.0   -- no wind yet
                , rcWindSpeed      = 0.0
                , rcOrographicLift = 0.0   -- needs terrain data
                , rcContinentality = if isOcean then 0.0 else 0.5
                , rcAlbedo         = if isOcean then 0.06 else 0.25
                , rcElevAvg        = 0     -- needs terrain data
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
