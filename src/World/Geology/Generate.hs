{-# LANGUAGE Strict, UnicodeSyntax #-}
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
import World.Base (GeoCoord(..))
import World.Types
import World.Plate (isBeyondGlacier, elevationAtGlobal, TectonicPlate)
import World.Geology.Types
import World.Geology.Hash (hashGeo, hashToFloatGeo, hashToRangeGeo, scaleCount)

-----------------------------------------------------------
-- Feature Generation Helpers
-----------------------------------------------------------

-- | Generate and register a batch of volcanic features.
--   Returns the list of new PersistentFeatures and updated state.
generateAndRegister ∷ Word64 → Int → [TectonicPlate]
                    → VolcanoEra
                    → (Word64 → Int → [TectonicPlate] → Int → Int
                        → Maybe FeatureShape)
                    → Int  -- ^ period index
                    → TimelineBuildState
                    → ([PersistentFeature], TimelineBuildState)
generateAndRegister seed worldSize plates _era mkFeature periodIdx tbs0 =
    let halfTiles = (worldSize * 16) `div` 2
        maxFeatures = scaleCount worldSize 4
        maxAttempts = maxFeatures * 5

        go attemptIdx count tbs acc
            | attemptIdx ≥ maxAttempts = (acc, tbs)
            | count ≥ maxFeatures     = (acc, tbs)
            | otherwise =
                let h1 = hashGeo seed attemptIdx 70
                    h2 = hashGeo seed attemptIdx 71
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case mkFeature seed worldSize plates gx gy of
                    Nothing → go (attemptIdx + 1) count tbs acc
                    Just feature →
                        let (fid, tbs') = allocFeatureId tbs
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = feature
                                , pfActivity         = FActive
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs'' = registerFeature pf tbs'
                        in go (attemptIdx + 1) (count + 1) tbs'' (pf : acc)

    in go 0 0 tbs0 []

-- | Like generateAndRegister but with explicit max attempts and max features.
generateAndRegisterN ∷ Int → Int → Word64 → Int → [TectonicPlate]
                     → VolcanoEra
                     → (Word64 → Int → [TectonicPlate] → Int → Int
                         → Maybe FeatureShape)
                     → Int → TimelineBuildState
                     → ([PersistentFeature], TimelineBuildState)
generateAndRegisterN baseMaxAttempts baseMaxFeatures seed worldSize plates
                     _era mkFeature periodIdx tbs0 =
    let halfTiles = (worldSize * 16) `div` 2
        maxFeatures = scaleCount worldSize baseMaxFeatures
        maxAttempts = scaleCount worldSize baseMaxAttempts

        go attemptIdx count tbs acc
            | attemptIdx ≥ maxAttempts = (acc, tbs)
            | count ≥ maxFeatures     = (acc, tbs)
            | otherwise =
                let h1 = hashGeo seed attemptIdx 70
                    h2 = hashGeo seed attemptIdx 71
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case mkFeature seed worldSize plates gx gy of
                    Nothing → go (attemptIdx + 1) count tbs acc
                    Just feature →
                        let (fid, tbs') = allocFeatureId tbs
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = feature
                                , pfActivity         = FActive
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

-- | Shield volcanos should be large but not world-dominating.
--   At worldSize=128 (2048 tiles), baseRadius 30-60 means
--   a shield covers at most ~6% of world width.
generateShieldVolcano ∷ Word64 → Int → [TectonicPlate]
                      → Int → Int → Maybe FeatureShape
generateShieldVolcano seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 80
                h2 = hashGeo seed gy 81
                h3 = hashGeo seed (gx + gy) 82
                baseR  = hashToRangeGeo h1 30 60
                peakH  = hashToRangeGeo h2 80 300
                hasPit = hashToFloatGeo h3 > 0.5
                pitR   = if hasPit then hashToRangeGeo (hashGeo seed gx 83) 2 5 else 0
                pitD   = if hasPit then hashToRangeGeo (hashGeo seed gy 84) 15 40 else 0
            in Just $ VolcanicShape $ ShieldVolcano ShieldParams
                { shCenter     = GeoCoord gx gy
                , shBaseRadius = baseR
                , shPeakHeight = peakH
                , shSummitPit  = hasPit
                , shPitRadius  = pitR
                , shPitDepth   = pitD
                }

-- | Cinder cones are small. Keep these tight.
generateCinderCone ∷ Word64 → Int → [TectonicPlate]
                   → Int → Int → Maybe FeatureShape
generateCinderCone seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 90
                h2 = hashGeo seed gy 91
                h3 = hashGeo seed (gx + gy) 92
                h4 = hashGeo seed (gx * gy) 93
                baseR   = hashToRangeGeo h1 4 10
                peakH   = hashToRangeGeo h2 30 120
                craterR = hashToRangeGeo h3 1 (max 2 (baseR `div` 3))
                craterD = hashToRangeGeo h4 8 (max 12 (peakH `div` 3))
            in Just $ VolcanicShape $ CinderCone CinderConeParams
                { ccCenter      = GeoCoord gx gy
                , ccBaseRadius  = baseR
                , ccPeakHeight  = peakH
                , ccCraterRadius = craterR
                , ccCraterDepth  = craterD
                }

-- | Lava domes: small-medium, steep.
generateLavaDome ∷ Word64 → Int → [TectonicPlate]
                 → Int → Int → Maybe FeatureShape
generateLavaDome seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 100
                h2 = hashGeo seed gy 101
                baseR = hashToRangeGeo h1 6 15
                height = hashToRangeGeo h2 30 100
            in Just $ VolcanicShape $ LavaDome LavaDomeParams
                { ldCenter     = GeoCoord gx gy
                , ldBaseRadius = baseR
                , ldHeight     = height
                }

-- | Calderas: medium features.
generateCaldera ∷ Word64 → Int → [TectonicPlate]
                → Int → Int → Maybe VolcanicFeature
generateCaldera seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 110
                h2 = hashGeo seed gy 111
                h3 = hashGeo seed (gx + gy) 112
                h4 = hashGeo seed (gx * gy) 113
                h5 = hashGeo seed (abs gx + abs gy) 114
                outerR  = hashToRangeGeo h1 15 40
                innerR  = hashToRangeGeo h2 (outerR `div` 2) (outerR * 3 `div` 4)
                rimH    = hashToRangeGeo h3 20 80
                floorD  = hashToRangeGeo h4 30 100
                hasLake = hashToFloatGeo h5 > 0.6
            in Just $ Caldera CalderaParams
                { caCenter      = GeoCoord gx gy
                , caOuterRadius = outerR
                , caInnerRadius = innerR
                , caRimHeight   = rimH
                , caFloorDepth  = floorD
                , caHasLake     = hasLake
                }

-- | Fissures: shorter.
generateFissure ∷ Word64 → Int → [TectonicPlate]
                → Int → Int → Maybe FeatureShape
generateFissure seed worldSize plates gx gy =
    if isBeyondGlacier worldSize gx gy
    then Nothing
    else let h1 = hashGeo seed gx 120
             h2 = hashGeo seed gy 121
             h3 = hashGeo seed (gx + gy) 122
             h4 = hashGeo seed (gx * gy) 123
             h5 = hashGeo seed (abs gx) 124
             angle = hashToFloatGeo h1 * 2.0 * π
             fissureLen = hashToRangeGeo h2 40 100
             halfLen = fromIntegral fissureLen / 2.0 ∷ Float
             ex = gx + round (halfLen * cos angle)
             ey = gy + round (halfLen * sin angle)
             sxCoord = gx - round (halfLen * cos angle)
             syCoord = gy - round (halfLen * sin angle)
             width   = hashToRangeGeo h3 3 6
             ridgeH  = hashToRangeGeo h4 15 50
             hasMagma = hashToFloatGeo h5 > 0.5
         in Just $ VolcanicShape $ FissureVolcano FissureParams
             { fpStart       = GeoCoord sxCoord syCoord
             , fpEnd         = GeoCoord ex ey
             , fpWidth       = width
             , fpRidgeHeight = ridgeH
             , fpHasMagma    = hasMagma
             }

-- | Lava tubes: shorter, subtler.
generateLavaTube ∷ Word64 → Int → [TectonicPlate]
                 → Int → Int → Maybe VolcanicFeature
generateLavaTube seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 130
                h2 = hashGeo seed gy 131
                h3 = hashGeo seed (gx + gy) 132
                h4 = hashGeo seed (gx * gy) 133
                h5 = hashGeo seed (abs gx) 134
                angle = hashToFloatGeo h1 * 2.0 * π
                tubeLen = hashToRangeGeo h2 20 60
                halfLen = fromIntegral tubeLen / 2.0 ∷ Float
                ex = gx + round (halfLen * cos angle)
                ey = gy + round (halfLen * sin angle)
                sxCoord = gx - round (halfLen * cos angle)
                syCoord = gy - round (halfLen * sin angle)
                width = hashToRangeGeo h3 2 4
                ridgeH = hashToRangeGeo h4 3 10
                collapses = hashToRangeGeo h5 1 4
            in Just $ LavaTube LavaTubeParams
                { ltStart        = GeoCoord sxCoord syCoord
                , ltEnd          = GeoCoord ex ey
                , ltWidth        = width
                , ltRidgeHeight  = ridgeH
                , ltCollapses    = collapses
                , ltCollapseSeed = fromIntegral (gx * 31 + gy * 17) `xor` seed
                }

-- | Super volcano: large but not absurd.
--   Caldera 50-100, ejecta 120-250.
generateSuperVolcano ∷ Word64 → Int → [TectonicPlate]
                     → Int → Int → Maybe FeatureShape
generateSuperVolcano seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 140
                h2 = hashGeo seed gy 141
                h3 = hashGeo seed (gx + gy) 142
                h4 = hashGeo seed (gx * gy) 143
                h5 = hashGeo seed (abs gx) 144
                calderaR = hashToRangeGeo h1 40 80
                rimH     = hashToRangeGeo h2 10 39
                floorD   = hashToRangeGeo h3 40 120
                ejectaR  = hashToRangeGeo h4 80 150
                ejectaD  = hashToRangeGeo h5 2 12
            in Just $ VolcanicShape $ SuperVolcano SuperVolcanoParams
                { svCenter        = GeoCoord gx gy
                , svCalderaRadius = calderaR
                , svRimHeight     = rimH
                , svFloorDepth    = floorD
                , svEjectaRadius  = ejectaR
                , svEjectaDepth   = ejectaD
                }

-- | Hydrothermal vents: unchanged, already tiny.
generateHydrothermalVent ∷ Word64 → Int → [TectonicPlate]
                         → Int → Int → Maybe FeatureShape
generateHydrothermalVent seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev ≥ -100 ∨ isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 150
                h2 = hashGeo seed gy 151
                radius   = hashToRangeGeo h1 3 8
                chimneyH = hashToRangeGeo h2 10 30
            in Just $ VolcanicShape $ HydrothermalVent HydrothermalParams
                { htCenter        = GeoCoord gx gy
                , htRadius        = radius
                , htChimneyHeight = chimneyH
                }
