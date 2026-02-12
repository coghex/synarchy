{-# LANGUAGE Strict #-}
module World.Geology.Generate
    ( -- * Feature generation and registration
      generateShieldVolcano
    , generateCinderCone
    , generateLavaDome
    , generateCaldera
    , generateFissure
    , generateLavaTube
    , generateSuperVolcano
    , generateHydrothermalVent
    , generateAndRegister
    , generateAndRegisterN
    ) where

import UPrelude
import Data.Bits (xor)
import Data.Word (Word64)
import World.Types
import World.Plate (isBeyondGlacier, elevationAtGlobal)
import World.Geology.Types
import World.Geology.Hash (hashGeo, hashToFloatGeo, hashToRangeGeo, scaleCount)

-----------------------------------------------------------
-- Feature Generation Helpers
-----------------------------------------------------------

-- | Generate and register a batch of volcanic features.
--   Returns the list of new PersistentFeatures and updated state.
generateAndRegister :: Word64 -> Int -> [TectonicPlate]
                    -> VolcanoEra
                    -> (Word64 -> Int -> [TectonicPlate] -> Int -> Int
                        -> Maybe VolcanicFeature)
                    -> Int  -- ^ period index
                    -> TimelineBuildState
                    -> ([PersistentFeature], TimelineBuildState)
generateAndRegister seed worldSize plates _era mkFeature periodIdx tbs0 =
    let halfTiles = (worldSize * 16) `div` 2
        maxFeatures = scaleCount worldSize 4
        maxAttempts = maxFeatures * 5

        go attemptIdx count tbs acc
            | attemptIdx >= maxAttempts = (acc, tbs)
            | count >= maxFeatures     = (acc, tbs)
            | otherwise =
                let h1 = hashGeo seed attemptIdx 70
                    h2 = hashGeo seed attemptIdx 71
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case mkFeature seed worldSize plates gx gy of
                    Nothing -> go (attemptIdx + 1) count tbs acc
                    Just feature ->
                        let (fid, tbs') = allocFeatureId tbs
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = feature
                                , pfActivity         = Active
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs'' = registerFeature pf tbs'
                        in go (attemptIdx + 1) (count + 1) tbs'' (pf : acc)

    in go 0 0 tbs0 []

-- | Like generateAndRegister but with explicit max attempts and max features.
generateAndRegisterN :: Int -> Int -> Word64 -> Int -> [TectonicPlate]
                     -> VolcanoEra
                     -> (Word64 -> Int -> [TectonicPlate] -> Int -> Int
                         -> Maybe VolcanicFeature)
                     -> Int -> TimelineBuildState
                     -> ([PersistentFeature], TimelineBuildState)
generateAndRegisterN baseMaxAttempts baseMaxFeatures seed worldSize plates
                     _era mkFeature periodIdx tbs0 =
    let halfTiles = (worldSize * 16) `div` 2
        maxFeatures = scaleCount worldSize baseMaxFeatures
        maxAttempts = scaleCount worldSize baseMaxAttempts

        go attemptIdx count tbs acc
            | attemptIdx >= maxAttempts = (acc, tbs)
            | count >= maxFeatures     = (acc, tbs)
            | otherwise =
                let h1 = hashGeo seed attemptIdx 70
                    h2 = hashGeo seed attemptIdx 71
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case mkFeature seed worldSize plates gx gy of
                    Nothing -> go (attemptIdx + 1) count tbs acc
                    Just feature ->
                        let (fid, tbs') = allocFeatureId tbs
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = feature
                                , pfActivity         = Active
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs'' = registerFeature pf tbs'
                        in go (attemptIdx + 1) (count + 1) tbs'' (pf : acc)

    in go 0 0 tbs0 []

-----------------------------------------------------------
-- Feature Constructors (called by generateAndRegister)
-----------------------------------------------------------

-- | Try to place a shield volcano. Requires land.
generateShieldVolcano :: Word64 -> Int -> [TectonicPlate]
                      -> Int -> Int -> Maybe VolcanicFeature
generateShieldVolcano seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 80
                h2 = hashGeo seed gy 81
                h3 = hashGeo seed (gx + gy) 82
                baseR  = hashToRangeGeo h1 60 120
                peakH  = hashToRangeGeo h2 100 400
                hasPit = hashToFloatGeo h3 > 0.5
                pitR   = if hasPit then hashToRangeGeo (hashGeo seed gx 83) 3 8 else 0
                pitD   = if hasPit then hashToRangeGeo (hashGeo seed gy 84) 20 60 else 0
            in Just $ ShieldVolcano ShieldParams
                { shCenter     = GeoCoord gx gy
                , shBaseRadius = baseR
                , shPeakHeight = peakH
                , shSummitPit  = hasPit
                , shPitRadius  = pitR
                , shPitDepth   = pitD
                }

-- | Try to place a cinder cone. Requires land.
generateCinderCone :: Word64 -> Int -> [TectonicPlate]
                   -> Int -> Int -> Maybe VolcanicFeature
generateCinderCone seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 90
                h2 = hashGeo seed gy 91
                h3 = hashGeo seed (gx + gy) 92
                h4 = hashGeo seed (gx * gy) 93
                baseR   = hashToRangeGeo h1 5 15
                peakH   = hashToRangeGeo h2 50 200
                craterR = hashToRangeGeo h3 2 (max 3 (baseR `div` 3))
                craterD = hashToRangeGeo h4 10 (max 15 (peakH `div` 3))
            in Just $ CinderCone CinderConeParams
                { ccCenter      = GeoCoord gx gy
                , ccBaseRadius  = baseR
                , ccPeakHeight  = peakH
                , ccCraterRadius = craterR
                , ccCraterDepth  = craterD
                }

-- | Try to place a lava dome. Requires land, prefers convergent boundaries.
generateLavaDome :: Word64 -> Int -> [TectonicPlate]
                 -> Int -> Int -> Maybe VolcanicFeature
generateLavaDome seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 100
                h2 = hashGeo seed gy 101
                baseR = hashToRangeGeo h1 10 25
                height = hashToRangeGeo h2 50 150
            in Just $ LavaDome LavaDomeParams
                { ldCenter     = GeoCoord gx gy
                , ldBaseRadius = baseR
                , ldHeight     = height
                }

-- | Try to place a caldera. Requires land.
generateCaldera :: Word64 -> Int -> [TectonicPlate]
                -> Int -> Int -> Maybe VolcanicFeature
generateCaldera seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 110
                h2 = hashGeo seed gy 111
                h3 = hashGeo seed (gx + gy) 112
                h4 = hashGeo seed (gx * gy) 113
                h5 = hashGeo seed (abs gx + abs gy) 114
                outerR  = hashToRangeGeo h1 30 80
                innerR  = hashToRangeGeo h2 (outerR `div` 2) (outerR * 3 `div` 4)
                rimH    = hashToRangeGeo h3 30 120
                floorD  = hashToRangeGeo h4 40 150
                hasLake = hashToFloatGeo h5 > 0.6
            in Just $ Caldera CalderaParams
                { caCenter      = GeoCoord gx gy
                , caOuterRadius = outerR
                , caInnerRadius = innerR
                , caRimHeight   = rimH
                , caFloorDepth  = floorD
                , caHasLake     = hasLake
                }

-- | Try to place a fissure. Works on land or ocean divergent boundaries.
generateFissure :: Word64 -> Int -> [TectonicPlate]
                -> Int -> Int -> Maybe VolcanicFeature
generateFissure seed worldSize plates gx gy =
    if isBeyondGlacier worldSize gx gy
    then Nothing
    else let h1 = hashGeo seed gx 120
             h2 = hashGeo seed gy 121
             h3 = hashGeo seed (gx + gy) 122
             h4 = hashGeo seed (gx * gy) 123
             h5 = hashGeo seed (abs gx) 124
             -- Fissure direction: random angle
             angle = hashToFloatGeo h1 * 2.0 * pi
             fissureLen = hashToRangeGeo h2 80 200
             halfLen = fromIntegral fissureLen / 2.0 :: Float
             ex = gx + round (halfLen * cos angle)
             ey = gy + round (halfLen * sin angle)
             sxCoord = gx - round (halfLen * cos angle)
             syCoord = gy - round (halfLen * sin angle)
             width   = hashToRangeGeo h3 5 10
             ridgeH  = hashToRangeGeo h4 20 80
             hasMagma = hashToFloatGeo h5 > 0.5
         in Just $ FissureVolcano FissureParams
             { fpStart       = GeoCoord sxCoord syCoord
             , fpEnd         = GeoCoord ex ey
             , fpWidth       = width
             , fpRidgeHeight = ridgeH
             , fpHasMagma    = hasMagma
             }

-- | Try to place a lava tube. Should be on the flank of an existing
--   shield volcano, but can also be standalone on basaltic terrain.
generateLavaTube :: Word64 -> Int -> [TectonicPlate]
                 -> Int -> Int -> Maybe VolcanicFeature
generateLavaTube seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 130
                h2 = hashGeo seed gy 131
                h3 = hashGeo seed (gx + gy) 132
                h4 = hashGeo seed (gx * gy) 133
                h5 = hashGeo seed (abs gx) 134
                angle = hashToFloatGeo h1 * 2.0 * pi
                tubeLen = hashToRangeGeo h2 40 120
                halfLen = fromIntegral tubeLen / 2.0 :: Float
                ex = gx + round (halfLen * cos angle)
                ey = gy + round (halfLen * sin angle)
                sxCoord = gx - round (halfLen * cos angle)
                syCoord = gy - round (halfLen * sin angle)
                width = hashToRangeGeo h3 3 6
                ridgeH = hashToRangeGeo h4 5 15
                collapses = hashToRangeGeo h5 2 6
            in Just $ LavaTube LavaTubeParams
                { ltStart        = GeoCoord sxCoord syCoord
                , ltEnd          = GeoCoord ex ey
                , ltWidth        = width
                , ltRidgeHeight  = ridgeH
                , ltCollapses    = collapses
                , ltCollapseSeed = fromIntegral (gx * 31 + gy * 17) `xor` seed
                }

-- | Try to place a supervolcano. Very rare. Requires land.
generateSuperVolcano :: Word64 -> Int -> [TectonicPlate]
                     -> Int -> Int -> Maybe VolcanicFeature
generateSuperVolcano seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 140
                h2 = hashGeo seed gy 141
                h3 = hashGeo seed (gx + gy) 142
                h4 = hashGeo seed (gx * gy) 143
                h5 = hashGeo seed (abs gx) 144
                calderaR = hashToRangeGeo h1 100 200
                rimH     = hashToRangeGeo h2 20 60
                floorD   = hashToRangeGeo h3 80 250
                ejectaR  = hashToRangeGeo h4 250 500
                ejectaD  = hashToRangeGeo h5 5 30
            in Just $ SuperVolcano SuperVolcanoParams
                { svCenter        = GeoCoord gx gy
                , svCalderaRadius = calderaR
                , svRimHeight     = rimH
                , svFloorDepth    = floorD
                , svEjectaRadius  = ejectaR
                , svEjectaDepth   = ejectaD
                }

-- | Try to place a hydrothermal vent. Requires ocean floor.
generateHydrothermalVent :: Word64 -> Int -> [TectonicPlate]
                         -> Int -> Int -> Maybe VolcanicFeature
generateHydrothermalVent seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev >= -100 || isBeyondGlacier worldSize gx gy
       then Nothing  -- Must be in deep ocean
       else let h1 = hashGeo seed gx 150
                h2 = hashGeo seed gy 151
                radius   = hashToRangeGeo h1 3 8
                chimneyH = hashToRangeGeo h2 10 30
            in Just $ HydrothermalVent HydrothermalParams
                { htCenter        = GeoCoord gx gy
                , htRadius        = radius
                , htChimneyHeight = chimneyH
                }

-----------------------------------------------------------
-- TimelineBuildState Helpers (needed for generateAndRegister)
-----------------------------------------------------------

-- | State threaded through timeline construction.
data TimelineBuildState = TimelineBuildState
    { tbsFeatures   :: ![PersistentFeature]
    , tbsNextId     :: !Int
    , tbsPeriods    :: ![GeoPeriod]       -- ^ Accumulated in reverse
    , tbsPeriodIdx  :: !Int
    }

-- | Helper to allocate a new feature ID.
allocFeatureId :: TimelineBuildState -> (GeoFeatureId, TimelineBuildState)
allocFeatureId tbs =
    let fid = GeoFeatureId (tbsNextId tbs)
    in (fid, tbs { tbsNextId = tbsNextId tbs + 1 })

-- | Helper to register a new persistent feature.
registerFeature :: PersistentFeature -> TimelineBuildState -> TimelineBuildState
registerFeature pf tbs = tbs
    { tbsFeatures = pf : tbsFeatures tbs
    }
