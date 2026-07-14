{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Hydrology.Simulation.calderaHazardsFor' /
--   'isCalderaHazardAt' — the caldera/supervolcano hazard footprint
--   that 'World.Hydrology.Simulation.Flow.walkToDivide' consults so
--   the inland-origin river-source extension (issue #221) can't route
--   a small world's densely-packed volcanism through a caldera and
--   breach it (issue #811). No engine/world needed: these are plain
--   geometric functions over hand-built 'PersistentFeature' values.
module Test.Headless.River.CalderaHazard (spec) where

import UPrelude
import Test.Hspec
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Geology.Timeline.Types
    ( PersistentFeature(..), FeatureShape(..), FeatureActivity(..)
    , VolcanicFeature(..), CalderaParams(..), SuperVolcanoParams(..)
    , ShieldParams(..)
    )
import World.Hydrology.Simulation (calderaHazardsFor, isCalderaHazardAt)

worldSize ∷ Int
worldSize = 32

superVolcanoAt ∷ GeoCoord → Int → FeatureActivity → PersistentFeature
superVolcanoAt centre calderaR activity = PersistentFeature
    { pfId = GeoFeatureId 1
    , pfFeature = VolcanicShape $ SuperVolcano SuperVolcanoParams
        { svCenter = centre
        , svCalderaRadius = calderaR
        , svRimHeight = 30
        , svFloorDepth = 80
        , svEjectaRadius = calderaR + 40
        , svEjectaDepth = 5
        , svCenterElev = 100
        }
    , pfActivity = activity
    , pfFormationPeriod = 0
    , pfLastActivePeriod = 0
    , pfEruptionCount = 1
    , pfParentId = Nothing
    }

calderaAt ∷ GeoCoord → Int → FeatureActivity → PersistentFeature
calderaAt centre outerR activity = PersistentFeature
    { pfId = GeoFeatureId 2
    , pfFeature = VolcanicShape $ Caldera CalderaParams
        { caCenter = centre
        , caOuterRadius = outerR
        , caInnerRadius = outerR `div` 2
        , caRimHeight = 40
        , caFloorDepth = 60
        , caHasLake = False
        , caCenterElev = 100
        }
    , pfActivity = activity
    , pfFormationPeriod = 0
    , pfLastActivePeriod = 0
    , pfEruptionCount = 1
    , pfParentId = Nothing
    }

shieldAt ∷ GeoCoord → PersistentFeature
shieldAt centre = PersistentFeature
    { pfId = GeoFeatureId 3
    , pfFeature = VolcanicShape $ ShieldVolcano ShieldParams
        { shCenter = centre
        , shBaseRadius = 8
        , shPeakHeight = 100
        , shSummitPit = False
        , shPitRadius = 0
        , shPitDepth = 0
        , shCenterElev = 100
        }
    , pfActivity = FActive
    , pfFormationPeriod = 0
    , pfLastActivePeriod = 0
    , pfEruptionCount = 1
    , pfParentId = Nothing
    }

spec ∷ Spec
spec = do
    describe "calderaHazardsFor" $ do
        it "includes an active supervolcano, radius scaled to its rim's outer edge" $ do
            let hazards = calderaHazardsFor [superVolcanoAt (GeoCoord 0 0) 40 FActive]
            hazards `shouldBe` [(0, 0, 46)] -- round (40 * 1.15)

        it "includes an active caldera at its outer radius verbatim" $ do
            let hazards = calderaHazardsFor [calderaAt (GeoCoord 10 (-5)) 25 FActive]
            hazards `shouldBe` [(10, -5, 25)]

        it "includes a dormant volcano (still a live magma source)" $ do
            let hazards = calderaHazardsFor [superVolcanoAt (GeoCoord 0 0) 40 FDormant]
            length hazards `shouldBe` 1

        it "excludes an extinct volcano (World.Magma.Init gives it no magma source)" $ do
            calderaHazardsFor [superVolcanoAt (GeoCoord 0 0) 40 FExtinct] `shouldBe` []

        it "excludes a collapsed volcano" $ do
            calderaHazardsFor [superVolcanoAt (GeoCoord 0 0) 40 FCollapsed] `shouldBe` []

        it "excludes ordinary narrow-vent volcanoes (shield/cinder/dome/fissure/vent)" $ do
            calderaHazardsFor [shieldAt (GeoCoord 0 0)] `shouldBe` []

        it "collects hazards from multiple features, ignoring non-hazard ones" $ do
            let features =
                    [ shieldAt (GeoCoord 1 1)
                    , superVolcanoAt (GeoCoord 0 0) 40 FActive
                    , calderaAt (GeoCoord 100 100) 20 FActive
                    , superVolcanoAt (GeoCoord 5 5) 40 FExtinct
                    ]
            length (calderaHazardsFor features) `shouldBe` 2

    describe "isCalderaHazardAt" $ do
        it "is True for a point at the hazard centre" $ do
            isCalderaHazardAt worldSize [(0, 0, 40)] 0 0 `shouldBe` True

        it "is True for a point inside the hazard radius" $ do
            isCalderaHazardAt worldSize [(0, 0, 40)] 30 0 `shouldBe` True

        it "is False for a point clearly outside the hazard radius" $ do
            isCalderaHazardAt worldSize [(0, 0, 40)] 100 100 `shouldBe` False

        it "is True exactly on the radius boundary" $ do
            isCalderaHazardAt worldSize [(0, 0, 40)] 40 0 `shouldBe` True

        it "is False just past the radius boundary" $ do
            isCalderaHazardAt worldSize [(0, 0, 40)] 41 0 `shouldBe` False

        it "is False with no hazards at all" $ do
            isCalderaHazardAt worldSize [] 0 0 `shouldBe` False

        it "checks every hazard, not just the first" $ do
            let hazards = [(0, 0, 5), (200, 200, 40)]
            isCalderaHazardAt worldSize hazards 190 190 `shouldBe` True

        it "is wrap-aware across the u-axis seam" $ do
            -- (126,-126) and (-128,128) are ~359 tiles apart in raw x/y
            -- (nowhere near each other on a naive, unwrapped measure),
            -- but the world wraps along the diagonal u = gx - gy axis
            -- (period 512 for worldSize 32), and these two points sit
            -- almost exactly opposite each other across that seam —
            -- wrappedDeltaUV folds them to (-2, 2), just 3 tiles apart.
            isCalderaHazardAt worldSize [(-128, 128, 10)] 126 (-126)
                `shouldBe` True
