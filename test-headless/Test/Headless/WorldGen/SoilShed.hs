{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the last-age soil-shedding gate in
--   'World.Geology.Erosion' (#225). Real mountains shed soil off steep
--   faces, leaving bare rock high up while the eroded material settles
--   in the flats. The final-age erosion pass produces the soil veneer;
--   the gate decides, per tile, whether a soil cap survives.
--
--   The key signal is the tile's TRUE local relief — the steepest
--   single-neighbour drop (NOT the deviation from the neighbour average,
--   which a uniformly steep mountainside reads as ~0 and so used to keep
--   its soil). Above 'soilShedRelief' (3 tiles) the column exposes rock;
--   flat / gentle ground keeps soil.
--
--   We exercise 'applyErosion' directly with synthetic neighbour
--   elevations — the gate is pure and this pins it without a full world
--   generation.
module Test.Headless.WorldGen.SoilShed (spec) where

import UPrelude
import Test.Hspec
import World.Geology (GeoModification(..))
import World.Geology.Erosion (applyErosion)
import World.Geology.Timeline.Types (ErosionParams(..), defaultErosionParams)

-- | Temperate last-age params: 'erosionSediment' will pick a soil
--   (material id ≥ 50) for a kept cap.
lastAgeParams ∷ ErosionParams
lastAgeParams = defaultErosionParams { epIsLastAge = True }

-- granite source rock, mid hardness, a single geological "age" period.
granite ∷ Word8
granite = 1

hardness ∷ Float
hardness = 0.5

-- | Run the final-age erosion at @elev@ with the four cardinal
--   neighbours at the given elevations.
run ∷ Int → (Int, Int, Int, Int) → GeoModification
run elev nbrs =
    applyErosion lastAgeParams 128 1 1.0 granite hardness elev nbrs

isSoil ∷ Word8 → Bool
isSoil m = (m ≥ 50 ∧ m ≤ 67) ∨ (m ≥ 110 ∧ m ≤ 113)

spec ∷ Spec
spec = do
    describe "last-age soil veneer (flat / gentle ground keeps soil)" $ do
        it "caps flat ground with a soil veneer (≥1 tile)" $ do
            let gm = run 50 (50, 50, 50, 50)
            gmMaterialOverride gm `shouldSatisfy` maybe False isSoil
            gmIntrusionDepth gm `shouldSatisfy` (≥ 1)

        it "still caps a gentle 2-tile slope with soil" $ do
            -- maxRelief 2 < soilShedRelief 3 → soil survives, just thinner.
            let gm = run 50 (48, 50, 50, 50)
            gmMaterialOverride gm `shouldSatisfy` maybe False isSoil
            gmIntrusionDepth gm `shouldSatisfy` (≥ 1)

    describe "last-age soil shedding (steep faces expose rock)" $ do
        it "sheds soil on a steep face (one neighbour 4 lower)" $ do
            let gm = run 50 (46, 50, 50, 50)
            gmMaterialOverride gm `shouldBe` Nothing
            gmIntrusionDepth gm `shouldBe` 0

        it "sheds soil on a sheer cliff (one neighbour far lower)" $ do
            let gm = run 50 (20, 50, 50, 50)
            gmMaterialOverride gm `shouldBe` Nothing
            gmIntrusionDepth gm `shouldBe` 0

    describe "soil shedding is final-age only (geological ages still deposit strata)" $
        it "a non-last-age steep face still overrides material (rock strata)" $ do
            -- A long age (duration 15) over a sheer 100-tile drop forces a
            -- clearly-negative erosion delta, so the override is present.
            let gm = applyErosion defaultErosionParams 128 15 1.0 granite hardness
                                  100 (0, 100, 100, 100)
            -- Mid-timeline erosion produces sedimentary rock, never bare
            -- exposure, so the override is present (a rock id, not a soil).
            gmMaterialOverride gm `shouldSatisfy` maybe False (not . isSoil)
