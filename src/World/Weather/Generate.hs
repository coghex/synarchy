{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Generate
    ( -- * Initialization (post-bombardment, early-Earth-like)
      initEarlyClimate
      -- * Timeline-internal climate update from ElevGrid
    , updateClimateFromGrid
    , oceanRegionsFromGrid
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import World.Weather.Types
import World.Chunk.Types (ChunkCoord(..))
import World.Ocean.Types (OceanMap)
import World.Hydrology.Simulation (ElevGrid(..))
import World.Constants (seaLevel)

-----------------------------------------------------------
-- Public: init from chunk-resolution OceanMap (used in Init.hs)
-----------------------------------------------------------

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
        halfChunks = worldSize `div` 2

        allCoords = [ ClimateCoord ru rv
                    | ru ← [0 .. regionsPerSide - 1]
                    , rv ← [0 .. regionsPerSide - 1]
                    ]

        -- Check if a climate region overlaps any ocean chunk.
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

        oceanSet = HS.fromList [ coord | coord ← allCoords, isOceanRegion coord ]

    in buildClimateFromOceanSet worldSize oceanSet 1.0 0.0 1.0

-----------------------------------------------------------
-- Public: derive coarse ocean regions from ElevGrid
-----------------------------------------------------------

-- | Derive a set of ocean ClimateCoords from the coarse ElevGrid.
--   Used during timeline construction to approximate ocean coverage
--   without running the expensive chunk-level BFS flood fill.
--
--   Each grid sample below seaLevel and not flagged as land maps
--   to a ClimateCoord via (u, v) → (ru, rv) conversion.
oceanRegionsFromGrid ∷ ElevGrid → Int → HS.HashSet ClimateCoord
oceanRegionsFromGrid grid worldSize =
    let gridW   = egGridW grid
        spacing = egSpacing grid
        halfGrid = gridW `div` 2
        regionsPerSide = worldSize `div` climateRegionSize
        halfChunks = worldSize `div` 2
    in HS.fromList
        [ ClimateCoord ru rv
        | ix ← [0 .. gridW - 1]
        , iy ← [0 .. gridW - 1]
        , let idx = iy * gridW + ix
        , not (egLand grid VU.! idx)
          -- Map grid sample → (u,v) → ClimateCoord
        , let u = (ix - halfGrid) * spacing
              v = (iy - halfGrid) * spacing
              -- u-axis wraps, v-axis is bounded
              ru = (u + halfChunks) `div` climateRegionSize
              rv = (v + halfChunks) `div` climateRegionSize
        , ru ≥ 0, ru < regionsPerSide
        , rv ≥ 0, rv < regionsPerSide
        ]

-----------------------------------------------------------
-- Public: lightweight climate update during timeline
-----------------------------------------------------------

-- | Update climate from the current ElevGrid's ocean distribution.
--   Called at each Era/Period boundary inside buildTimeline.
--   
--   Takes the previous ClimateState to carry forward CO2, solar
--   constant, and global temperature offset. Recomputes maritime
--   index, temperatures, ocean cells, and regional climate from
--   the new ocean set.
updateClimateFromGrid ∷ Int                        -- ^ worldSize
                      → HS.HashSet ClimateCoord    -- ^ coarse ocean regions
                      → ClimateState               -- ^ previous climate state
                      → ClimateState
updateClimateFromGrid worldSize coarseOcean prevClimate =
    buildClimateFromOceanSet worldSize coarseOcean
        (csGlobalCO2  prevClimate)
        (csGlobalTemp prevClimate)
        (csSolarConst prevClimate)

-----------------------------------------------------------
-- Internal: shared climate builder
-----------------------------------------------------------

-- | Build a complete ClimateState from a set of ocean ClimateCoords.
--   This is the core climate model, parameterized by global state
--   so it can be called both from initEarlyClimate (with defaults)
--   and from updateClimateFromGrid (with carried-forward values).
buildClimateFromOceanSet ∷ Int                       -- ^ worldSize
                         → HS.HashSet ClimateCoord   -- ^ which regions are ocean
                         → Float                     -- ^ global CO2
                         → Float                     -- ^ global temp offset
                         → Float                     -- ^ solar constant
                         → ClimateState
buildClimateFromOceanSet worldSize oceanSet globalCO2 globalTempOffset solarConst =
    let regionsPerSide = worldSize `div` climateRegionSize
        halfRegions    = regionsPerSide `div` 2

        allCoords = [ ClimateCoord ru rv
                    | ru ← [0 .. regionsPerSide - 1]
                    , rv ← [0 .. regionsPerSide - 1]
                    ]

        isOceanRegion coord = HS.member coord oceanSet

        -------------------------------------------------------
        -- Maritime index via BFS distance to ocean
        -------------------------------------------------------

        neighbors (ClimateCoord ru rv) =
            let east  = ClimateCoord ((ru + 1) `mod` regionsPerSide) rv
                west  = ClimateCoord ((ru - 1 + regionsPerSide) `mod` regionsPerSide) rv
                north = ClimateCoord ru (rv + 1)
                south = ClimateCoord ru (rv - 1)
                inBounds (ClimateCoord _ rvy) = rvy ≥ 0 ∧ rvy < regionsPerSide
            in filter inBounds [east, west, north, south]

        bfsDistances ∷ HM.HashMap ClimateCoord Int
        bfsDistances =
            let initQueue = Seq.fromList (HS.toList oceanSet)
                initMap   = HM.fromList [ (c, 0) | c ← HS.toList oceanSet ]

                go queue distMap =
                    case Seq.viewl queue of
                        Seq.EmptyL → distMap
                        coord Seq.:< rest →
                            let d = HM.lookupDefault 0 coord distMap
                                step (m, q) n =
                                    if HM.member n m
                                       then (m, q)
                                       else (HM.insert n (d + 1) m, q Seq.|> n)
                                (distMap', queue') = foldl' step (distMap, rest) (neighbors coord)
                            in go queue' distMap'
            in if HS.null oceanSet
                  then HM.empty
                  else go initQueue initMap

        coastScale ∷ Float
        coastScale = 4.0

        maritimeIndex ∷ ClimateCoord → Float
        maritimeIndex coord
            | HS.null oceanSet     = 0.0
            | isOceanRegion coord  = 1.0
            | otherwise =
                let d = fromIntegral (HM.lookupDefault (regionsPerSide * 2) coord bfsDistances)
                in exp (-(d / coastScale))

        -------------------------------------------------------
        -- Climate model constants
        -- CO2 modulates base temperature: higher CO2 = warmer
        -------------------------------------------------------

        co2TempBoost = (globalCO2 - 1.0) * 6.0  -- +6°C per doubling above baseline
        tEquator    = 34.0 + co2TempBoost + globalTempOffset ∷ Float
        tPoleDrop   = 52.0  ∷ Float
        tGamma      = 1.25  ∷ Float
        seasonBase  = 3.0   ∷ Float
        seasonLat   = 14.0  ∷ Float
        seasonLand  = 12.0  ∷ Float

        -- Pressure belt magnitudes
        pHighPole   = 0.07  ∷ Float
        pHighSubt   = 0.05  ∷ Float
        pLowEqu     = 0.06  ∷ Float
        pLowSubpol  = 0.05  ∷ Float
        pTempScale  = 0.003 ∷ Float

        gauss center width x =
            exp (-( (x - center) * (x - center) ) / (width * width))

        clamp ∷ (Ord a) ⇒ a → a → a → a
        clamp lo hi = max lo . min hi

        -------------------------------------------------------
        -- Precompute mean temperature per region to get global mean
        -------------------------------------------------------

        latInfo (ClimateCoord _ru rv) =
            let latSigned = (fromIntegral (rv - halfRegions))
                          / max 1 (fromIntegral halfRegions)  ∷ Float
                latRatio  = abs latSigned
                latDeg    = latRatio * 90.0
            in (latSigned, latRatio, latDeg)

        tempMeanFor coord =
            let (_latSigned, latRatio, _latDeg) = latInfo coord
                m = maritimeIndex coord
                tMeanLat = tEquator - tPoleDrop * (latRatio ** tGamma)
                tMean = tMeanLat + 2.0 * m
            in tMean

        tempMeanMap = HM.fromList [ (coord, tempMeanFor coord) | coord ← allCoords ]

        globalMeanTemp =
            let temps = HM.elems tempMeanMap
            in if null temps then 15.0 else sum temps / fromIntegral (length temps)

        -------------------------------------------------------
        -- Ocean cells for ocean regions
        -------------------------------------------------------

        mkOceanCell ∷ ClimateCoord → OceanCell
        mkOceanCell coord@(ClimateCoord _ru _rv) =
            let (_latSigned, latRatio, _latDeg) = latInfo coord
                m = 1.0
                tMean = HM.lookupDefault 15.0 coord tempMeanMap
                seasonal = seasonBase + seasonLat * (latRatio ** 1.2)
                           + seasonLand * (1.0 - m)
                oceanSeasonal = seasonal * 0.4
                sstSummer = tMean + oceanSeasonal
                sstWinter = tMean - oceanSeasonal
                baseSalinity = 35.0
                iceCover = clamp 0.0 1.0 ((2.0 - sstWinter) / 6.0)
                windDir = windDirFor coord
                windSpd = windSpdFor coord
            in OceanCell
                { ocTemperature = SeasonalClimate sstSummer sstWinter
                , ocSalinity    = baseSalinity
                , ocDepth       = 200
                , ocCurrentDir  = windDir
                , ocCurrentSpd  = 0.35 * windSpd
                , ocUpwelling   = 0.1 * (1.0 - latRatio)
                , ocIceCover    = iceCover
                }

        -------------------------------------------------------
        -- Wind belt model (0 = +u/east, π/2 = +v/north)
        -------------------------------------------------------

        windDirFor coord =
            let (latSigned, _latRatio, latDeg) = latInfo coord
            in if latDeg < 10.0
                  then 0.0
               else if latDeg < 30.0
                  then if latSigned ≥ 0
                          then (5.0 * pi / 4.0) -- NH trades: NE → SW
                          else (3.0 * pi / 4.0) -- SH trades: SE → NW
               else if latDeg < 60.0
                  then 0.0                    -- westerlies: W → E
                  else pi                     -- polar easterlies: E → W

        windSpdFor coord =
            let (_latSigned, _latRatio, latDeg) = latInfo coord
            in if latDeg < 10.0 then 0.12
               else if latDeg < 30.0 then 0.45
               else if latDeg < 60.0 then 0.40
               else 0.32

        -------------------------------------------------------
        -- Regional climate (full initialization)
        -------------------------------------------------------

        mkRegionClimate ∷ ClimateCoord → Bool → RegionClimate
        mkRegionClimate coord@(ClimateCoord _ru _rv) isOcean =
            let (latSigned, latRatio, latDeg) = latInfo coord
                m = maritimeIndex coord

                tMean = HM.lookupDefault 15.0 coord tempMeanMap

                seasonal =
                    seasonBase
                    + seasonLat * (latRatio ** 1.2)
                    + seasonLand * (1.0 - m)

                tSummer = tMean + seasonal
                tWinter = tMean - seasonal

                pLat =
                    1.0
                    + pHighPole  * gauss 90.0 12.0 latDeg
                    + pHighSubt  * gauss 30.0 10.0 latDeg
                    - pLowEqu    * gauss  0.0 14.0 latDeg
                    - pLowSubpol * gauss 60.0 10.0 latDeg

                pTemp = -pTempScale * (tMean - globalMeanTemp)
                pressure = clamp 0.8 1.2 (pLat + pTemp)

                pressureDev = pressure - 1.0
                highFactor = clamp01 (pressureDev / 0.08)
                lowFactor  = clamp01 ((-pressureDev) / 0.08)

                humidity = clamp01 (0.30 + 0.50 * m + 0.25 * lowFactor - 0.25 * highFactor)

                seasonNorm = clamp01 (seasonal / 25.0)
                precipBase = max 0.0 (0.15 + 0.60 * lowFactor + 0.30 * m - 0.30 * highFactor)
                precipSummer = precipBase * (1.0 + 0.20 * seasonNorm)
                precipWinter = precipBase * (1.0 - 0.20 * seasonNorm)

                evap = 0.15 + 0.02 * max 0.0 (tMean - 5.0) + 0.35 * m

                cloudCover = clamp01 (0.20 + 0.70 * humidity - 0.30 * highFactor)

                snowFrac = clamp01 ((5.0 - tMean) / 20.0 + 0.40 * seasonNorm)

                albedoBase = if isOcean then 0.06 else 0.20
                albedo = clamp01 (albedoBase + 0.40 * snowFrac)

                continentality = clamp01 ((1.0 - m) * (0.60 + 0.40 * highFactor))

            in RegionClimate
                { rcAirTemp        = SeasonalClimate tSummer tWinter
                , rcHumidity       = humidity
                , rcPrecipitation  = SeasonalClimate precipSummer precipWinter
                , rcPrecipType     = snowFrac
                , rcEvaporation    = evap
                , rcCloudCover     = cloudCover
                , rcPressure       = pressure
                , rcWindDir        = windDirFor coord
                , rcWindSpeed      = windSpdFor coord
                , rcOrographicLift = 0.0
                , rcContinentality = continentality
                , rcAlbedo         = albedo
                , rcElevAvg        = 0
                }

        -- Build ocean cells for ocean regions
        oceanEntries = [ (coord, mkOceanCell coord)
                       | coord ← allCoords
                       , isOceanRegion coord
                       ]

        -- Build climate entries for all regions
        climateEntries = [ (coord, mkRegionClimate coord (isOceanRegion coord))
                         | coord ← allCoords
                         ]

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
        , csGlobalCO2  = globalCO2
        , csGlobalTemp = globalMeanTemp
        , csSolarConst = solarConst
        }
