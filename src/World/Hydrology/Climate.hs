{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Climate
    ( computeRegionalClimate
    , computePrecipitation
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Types
import World.Geology.Types
import World.Hydrology.Types

-- | Compute climate for all regions from current terrain state.
--   Called once per age during timeline build.
--
--   Pass 1: Base temperature from latitude + elevation lapse rate
--   Pass 2: Ocean distance (BFS from ocean regions)
--   Pass 3: Wind direction from latitude bands (Hadley cells)
--   Pass 4: Humidity from ocean evaporation, carried by wind,
--           reduced by distance and terrain blocking
--   Pass 5: Precipitation = humidity × orographic lift factor
--           Mountains on windward side get rain, leeward gets shadow
computeRegionalClimate ∷ Word64 → Int → [TectonicPlate]
                       → OceanMap
                       → GeoState
                       → HM.HashMap RegionCoord RegionalClimate
computeRegionalClimate seed worldSize plates oceanMap gs =
    let regionsPerSide = worldSize `div` regionSize
        halfRegions = regionsPerSide `div` 2

        -- Pass 1: temperature
        -- Latitude gives base temp, elevation applies lapse rate
        -- (~6.5°C per 1000m in real life, scaled for game)
        baseClimate = HM.fromList
            [ (RegionCoord rx ry, initClimate rx ry)
            | rx ← [0 .. regionsPerSide - 1]
            , ry ← [0 .. regionsPerSide - 1]
            ]

        initClimate rx ry =
            let latRatio = abs (fromIntegral (ry - halfRegions))
                         / fromIntegral halfRegions
                baseTemp = lookupRegionTemp (RegionCoord rx ry) (gsRegional gs)
                -- Elevation lapse: regions with high avg elevation are colder
                elevAvg = regionAvgElevation seed plates worldSize rx ry
                lapseRate = 0.01  -- °C per elevation unit
                temp = baseTemp - fromIntegral elevAvg * lapseRate
            in RegionalClimate
                { rcTemperature   = temp
                , rcHumidity      = 0.0  -- filled in pass 4
                , rcPrecipitation = 0.0  -- filled in pass 5
                , rcWindDir       = windDirFromLat latRatio
                , rcWindSpeed     = 0.5
                , rcElevAvg       = elevAvg
                , rcOceanDist     = 0    -- filled in pass 2
                }

        -- Pass 2: BFS from ocean regions to compute ocean distance
        -- Pass 3: Wind already set from latitude
        -- Pass 4: Trace moisture from ocean along wind direction
        -- Pass 5: Precipitation from humidity + orographic lift

        -- (I'm showing the structure — each pass is a fold over
        -- the region grid, updating the HashMap)

    in baseClimate  -- full implementation fills all passes

-- | Compute precipitation for a region.
--   High humidity + upslope wind = high precipitation.
--   Used to determine where rivers originate.
computePrecipitation ∷ RegionalClimate → Float
computePrecipitation rc =
    let humidity = rcHumidity rc
        -- Orographic effect: if wind blows toward higher ground,
        -- air rises, cools, dumps moisture
        orographic = 1.0  -- placeholder, computed from neighbors
    in humidity * orographic

-- | Wind direction from latitude (Hadley cell model)
windDirFromLat ∷ Float → Float
windDirFromLat latRatio
    | latRatio < 0.33 = π        -- Trade winds: blow westward (E→W)
    | latRatio < 0.66 = 0.0      -- Westerlies: blow eastward (W→E)
    | otherwise       = π        -- Polar easterlies: blow westward

-- | Average elevation across a region.
--   Samples a few points per region for speed.
regionAvgElevation ∷ Word64 → [TectonicPlate] → Int → Int → Int → Int
regionAvgElevation seed plates worldSize rx ry =
    -- Sample center of region + 4 cardinal points
    -- Uses elevationAtGlobal, same as your existing code
    0  -- placeholder
