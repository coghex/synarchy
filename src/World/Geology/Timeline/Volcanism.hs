{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.Volcanism
    ( applyPeriodVolcanism
    , applyVolcanicEvolution
    , generateEruption
    ) where
import UPrelude
import Data.Bits (xor)
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.Text as T
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Plate (TectonicPlate, elevationAtGlobal)
import World.Geology.Types
import World.Geology.Hash
import World.Geology.Generate
    ( generateShieldVolcano
    , generateCinderCone
    , generateFissure
    , generateSuperVolcano
    , generateHydrothermalVent
    , generateAndRegisterN
    )
import World.Geology.Evolution (evolveOneFeature)
import World.Hydrology.Simulation (ElevGrid(..), updateElevGrid)
import World.Geology.Timeline.Helpers
    ( mkGeoPeriod, featureCenter, isSuperVolcano )

-----------------------------------------------------------
-- Period-level volcanism
-----------------------------------------------------------

applyPeriodVolcanism ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState → ElevGrid
                     → (TimelineBuildState, ElevGrid)
applyPeriodVolcanism seed worldSize plates periodIdx tbs grid =
    let gs = tbsGeoState tbs
        volcSeed = seed `xor` 0xB45A1F1C
        pIdx = tbsPeriodIdx tbs
        activityLevel = gsCO2 gs
        currentDate = gdMillionYears (gsDate gs)

        (shields, tbs1) = generateAndRegisterN 12 3 volcSeed worldSize plates
                              VolcanoEra_Hotspot generateShieldVolcano pIdx tbs

        (fissures, tbs2) = generateAndRegisterN 10 3 (volcSeed + 1) worldSize plates
                               VolcanoEra_Boundary generateFissure pIdx tbs1

        (cinders, tbs3) = generateAndRegisterN 16 5 (volcSeed + 2) worldSize plates
                              VolcanoEra_Boundary generateCinderCone pIdx tbs2

        (vents, tbs4) = generateAndRegisterN 10 3 (volcSeed + 3) worldSize plates
                            VolcanoEra_Boundary generateHydrothermalVent pIdx tbs3

        hasSuperVolcano = any isSuperVolcano (tbsFeatures tbs4)
        (supers, tbs5) = if periodIdx ≡ 0 ∨ (periodIdx ≡ 1 ∧ not hasSuperVolcano)
            then let (s, t) = generateAndRegisterN 12 1 (volcSeed + 4) worldSize plates
                                  VolcanoEra_Hotspot generateSuperVolcano pIdx tbs4
                 in if null s ∧ not hasSuperVolcano
                    then forceOneSuperVolcano (volcSeed + 5) worldSize plates pIdx t
                    else (s, t)
            else ([], tbs4)

        allNew = shields <> fissures <> cinders <> vents <> supers
        events = map (\pf → VolcanicEvent (pfFeature pf)) allNew

        gs' = (tbsGeoState tbs5)
            { gsCO2 = gsCO2 (tbsGeoState tbs5) + fromIntegral (length allNew) * 0.01
            }

        period = mkGeoPeriod worldSize
            ("Volcanism " <> T.pack (show periodIdx))
            Period 50 currentDate
            events
            (ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 4000))
        tbs6 = addPeriod period (tbs5 { tbsGeoState = gs' })
        grid' = updateElevGrid worldSize grid period
    in (tbs6, grid')

forceOneSuperVolcano ∷ Word64 → Int → [TectonicPlate] → Int
                     → TimelineBuildState
                     → ([PersistentFeature], TimelineBuildState)
forceOneSuperVolcano seed worldSize plates periodIdx tbs =
    let halfTiles = (worldSize * 16) `div` 2
        go attempt
            | attempt ≥ 100 = ([], tbs)
            | otherwise =
                let h1 = hashGeo seed attempt 170
                    h2 = hashGeo seed attempt 171
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case generateSuperVolcano seed worldSize plates gx gy of
                    Nothing → go (attempt + 1)
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
                        in ([pf], tbs'')
    in go 0

-----------------------------------------------------------
-- Volcanic evolution + eruption generation
-----------------------------------------------------------

applyVolcanicEvolution ∷ Word64 → Int → [TectonicPlate]
                       → TimelineBuildState → ElevGrid
                       → (TimelineBuildState, ElevGrid)
applyVolcanicEvolution seed worldSize plates tbs grid =
    let periodIdx = tbsPeriodIdx tbs
        evolSeed = seed `xor` 0xEF01F100
        currentDate = gdMillionYears (gsDate (tbsGeoState tbs))

        featureIds = map pfId (tbsFeatures tbs)

        (events, tbs1) = foldl' (\(evts, st) fid →
            case lookupFeature fid st of
                Nothing → (evts, st)
                Just pf → evolveOneFeature evolSeed periodIdx (evts, st) pf
            ) ([], tbs) featureIds

        eruptSeed = seed `xor` 0x5E7A
        periodEruptions = catMaybes
            [ generateEruption eruptSeed worldSize periodIdx plates pf
            | pf ← tbsFeatures tbs1
            , case eruptionProfile (pfFeature pf) of
                Just ep → epTimelineScale ep ≡ Period
                Nothing → False
            ]

        allEvents = events <> periodEruptions

        period = mkGeoPeriod worldSize
            "Volcanic Evolution"
            Period 30 currentDate
            allEvents
            (ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 5000))
    in if null allEvents then (tbs1, grid)
       else let tbs2 = addPeriod period tbs1
                grid' = updateElevGrid worldSize grid period
            in (tbs2, grid')

generateEruption ∷ Word64 → Int → Int → [TectonicPlate]
                 → PersistentFeature → Maybe GeoEvent
generateEruption seed worldSize ageIdx plates pf =
    case pfActivity pf of
        FActive → case eruptionProfile (pfFeature pf) of
            Nothing → Nothing
            Just profile →
                let GeoFeatureId fidInt = pfId pf
                    h1 = hashGeo seed fidInt (700 + ageIdx)
                    roll = hashToFloatGeo h1
                in if roll < epEruptChance profile
                   then Just (buildEruptionEvent seed worldSize ageIdx plates pf profile)
                   else Nothing
        _ → Nothing

buildEruptionEvent ∷ Word64 → Int → Int → [TectonicPlate]
                   → PersistentFeature → EruptionProfile → GeoEvent
buildEruptionEvent seed worldSize ageIdx plates pf profile =
    let GeoFeatureId fidInt = pfId pf
        h2 = hashGeo seed fidInt (710 + ageIdx)
        h3 = hashGeo seed fidInt (711 + ageIdx)

        radius = hashToRangeGeo h2 (epMinRadius profile) (epMaxRadius profile)
        volume = hashToRangeGeo h3 (epMinVolume profile) (epMaxVolume profile)

        (sx, sy) = featureCenter (pfFeature pf)

        (srcElev, _) = elevationAtGlobal seed plates worldSize sx sy

        eruptionBoost = pfEruptionCount pf * 5
        lavaElev = srcElev + eruptionBoost + volume `div` 4

        flow = LavaFlow
            { lfSourceX   = sx
            , lfSourceY   = sy
            , lfRadius    = radius
            , lfElevation = lavaElev
            , lfVolume    = volume
            , lfMaterial  = epMaterial profile
            , lfViscosity = epViscosity profile
            }
    in EruptionEvent (pfId pf) flow
