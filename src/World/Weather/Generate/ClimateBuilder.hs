{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Generate.ClimateBuilder
    ( buildClimateFromOceanSet
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import World.Weather.Types
import World.Constants (seaLevel)

-- * Internal: shared climate builder

-- | Build a complete ClimateState from a set of ocean ClimateCoords.
--   This is the core climate model, parameterized by global state
--   so it can be called both from initEarlyClimate (with the
--   timeline's final CO2/solar forcing) and from updateClimateFromGrid
--   (with carried-forward values).
buildClimateFromOceanSet ∷ Int                       -- ^ worldSize
                         → HS.HashSet ClimateCoord   -- ^ which regions are ocean
                         → HM.HashMap ClimateCoord Float -- ^ freshwater moisture weights
                         → Float                     -- ^ global CO2
                         → Float                     -- ^ global temp offset
                         → Float                     -- ^ solar constant
                         → ClimateState
buildClimateFromOceanSet worldSize oceanSet freshwaterSources globalCO2 globalTempOffset solarConst =
    let regionsPerSide = climateRegionCount worldSize
        _halfRegions    = regionsPerSide `div` 2

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

        -- Maritime influence decays exponentially over this many
        -- climate regions. Scales with world size so larger worlds
        -- have proportionally wider coastal moisture belts.
        coastScale ∷ Float
        coastScale = fromIntegral regionsPerSide / 20.0

        maritimeIndex ∷ ClimateCoord → Float
        maritimeIndex coord
            | HS.null oceanSet     = 0.0
            | isOceanRegion coord  = 1.0
            | otherwise =
                let d = fromIntegral (HM.lookupDefault (regionsPerSide * 2) coord bfsDistances)
                in exp (-(d / coastScale))

        -------------------------------------------------------
        -- Freshwater moisture index via BFS from lakes/rivers
        -------------------------------------------------------

        freshwaterScale ∷ Float
        freshwaterScale = 2.0

        freshwaterSeeds = HM.keysSet freshwaterSources

        freshwaterMaxDist ∷ Int
        freshwaterMaxDist = 10

        freshwaterDistances ∷ HM.HashMap ClimateCoord Int
        freshwaterDistances =
            let initQueue = Seq.fromList (HS.toList freshwaterSeeds)
                initMap   = HM.fromList [ (c, 0) | c ← HS.toList freshwaterSeeds ]
                go queue distMap =
                    case Seq.viewl queue of
                        Seq.EmptyL → distMap
                        coord Seq.:< rest →
                            let d = HM.lookupDefault 0 coord distMap
                                step (m', q) n =
                                    if HM.member n m'
                                       then (m', q)
                                       else let d' = d + 1
                                            in if d' > freshwaterMaxDist
                                               then (m', q)
                                               else (HM.insert n d' m', q Seq.|> n)
                                (distMap', queue') = foldl' step (distMap, rest) (neighbors coord)
                            in go queue' distMap'
            in if HS.null freshwaterSeeds
                  then HM.empty
                  else go initQueue initMap

        freshwaterIndex ∷ ClimateCoord → Float
        freshwaterIndex coord
            | HM.null freshwaterSources = 0.0
            | HM.member coord freshwaterSources =
                HM.lookupDefault 0.0 coord freshwaterSources
            | otherwise =
                let d = fromIntegral (HM.lookupDefault (regionsPerSide * 2) coord freshwaterDistances)
                in 0.5 * exp (-(d / freshwaterScale))

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
            -- Map rv ∈ [0, regionsPerSide-1] to latSigned ∈ [-1, 1]
            -- symmetrically so both poles reach exactly ±1.0.
            let latSigned = if regionsPerSide ≤ 1 then 0.0
                            else (fromIntegral rv / fromIntegral (regionsPerSide - 1))
                                 * 2.0 - 1.0 ∷ Float
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
            let (_latSigned, latRatio, latDeg) = latInfo coord
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

                -- Freshwater moisture from lakes/rivers
                fw = freshwaterIndex coord
                totalMoisture = clamp01 (m + 0.4 * fw)

                -- Low pressure amplifies available moisture rather
                -- than creating precipitation independently. This
                -- makes continental interiors drier while coasts and
                -- waterways stay wet.
                moisturePrecip = 0.10 + 0.50 * totalMoisture
                convectiveBase = 0.08 * lowFactor
                upliftMultiplier = 1.0 + 1.2 * lowFactor - 0.6 * highFactor

                humidity = clamp01 (0.15 + 0.55 * totalMoisture
                                   + 0.15 * lowFactor - 0.20 * highFactor)

                seasonNorm = clamp01 (seasonal / 25.0)
                precipBase = max 0.0 (moisturePrecip * upliftMultiplier
                                     + convectiveBase)
                precipSummer = precipBase * (1.0 + 0.20 * seasonNorm)
                precipWinter = precipBase * (1.0 - 0.20 * seasonNorm)

                evap = 0.15 + 0.02 * max 0.0 (tMean - 5.0)
                     + 0.35 * m + 0.15 * fw

                cloudCover = clamp01 (0.20 + 0.70 * humidity - 0.30 * highFactor)

                snowFrac = clamp01 ((5.0 - tMean) / 20.0 + 0.40 * seasonNorm)

                albedoBase = if isOcean then 0.06 else 0.20
                albedo = clamp01 (albedoBase + 0.40 * snowFrac)

                continentality = clamp01 ((1.0 - m - 0.3 * fw) * (0.60 + 0.40 * highFactor))

                -- Water table: how deep below estimated terrain
                -- groundwater sits. Ocean regions = seaLevel.
                -- Inland: base offset of 20, raised by effective
                -- precipitation and lowered by elevation.
                --
                -- Summer: rain + snowmelt from winter accumulation.
                -- Winter: rain only (fallen snow stays frozen).
                winterRain    = precipWinter * (1.0 - snowFrac)
                snowAccum     = precipWinter * snowFrac
                -- Snowmelt in summer depends on summer temperature
                -- being above freezing. Cold summers = less melt.
                meltFraction  = clamp01 ((tSummer - 0.0) / 15.0)
                summerMelt    = snowAccum * meltFraction
                summerEffective = precipSummer + summerMelt
                winterEffective = winterRain

                -- Estimate terrain elevation from ocean distance.
                -- Actual per-tile elevation isn't available during
                -- climate generation, but regions further from the
                -- ocean tend to be higher (continental interiors).
                bfsDist = fromIntegral
                            (HM.lookupDefault 0 coord bfsDistances) ∷ Float
                estimatedElev = bfsDist * 5.0  -- ~5 z per region step inland
                elevPenalty   = estimatedElev / 15.0

                -- Base offset below terrain
                baseOffset  = 20.0 ∷ Float
                precipScale = 40.0 ∷ Float

                summerOffset = max 1.0 (baseOffset
                             - summerEffective * precipScale
                             + elevPenalty)
                winterOffset = max 1.0 (baseOffset
                             - winterEffective * precipScale
                             + elevPenalty)

                -- Water table = estimated terrain - offset.
                -- For ocean regions, always sea level.
                estTerrZ = fromIntegral seaLevel + estimatedElev
                wtSummer = if isOcean
                           then fromIntegral seaLevel
                           else estTerrZ - summerOffset
                wtWinter = if isOcean
                           then fromIntegral seaLevel
                           else estTerrZ - winterOffset

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
                , rcWaterTable     = SeasonalClimate wtSummer wtWinter
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
