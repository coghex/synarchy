{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests proving final-age mountain soil shed (#225 / PR #279) is
--   REDISTRIBUTED to the adjacent lower/gentler receiving terrain instead
--   of simply deleted (#812). 'World.Geology.Erosion.Math' computes purely
--   per-tile from a 1-ring neighbour stencil; a receiver recognises a
--   shedding donor neighbour from its OWN stencil alone (see the
--   'shedCredit' comment in Math.hs), so this spec exercises 'applyErosion'
--   directly with synthetic neighbour elevations, same as
--   'Test.Headless.WorldGen.SoilShed'.
module Test.Headless.WorldGen.SoilRedistribution (spec) where

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
run elev nbrs = applyErosion lastAgeParams 128 1 1.0 granite hardness elev nbrs

spec ∷ Spec
spec = describe "final-age soil shed redistribution (#812)" $ do
    it "a steep mountain face still exposes bare rock (donor, #225 unchanged)" $ do
        -- One coherent terrain profile: a donor at 54 dropping 4 tiles
        -- to a receiver at 50 on its S side (mirrored by the receiver
        -- test below, whose N neighbour is this same donor).
        let donor = run 54 (54, 50, 54, 54)
        gmMaterialOverride donor `shouldBe` Nothing
        gmIntrusionDepth donor `shouldBe` 0

    it "the adjacent lower receiver gains soil the donor no longer caps" $ do
        -- Same terrain profile as above: receiver at 50 with the steep
        -- donor (elev 54, a downhill drop of 4 ≥ soilShedRelief 3 from
        -- the donor's own perspective) immediately to its N.
        let receiverWithDonor = run 50 (54, 50, 50, 50)
            -- Otherwise-IDENTICAL fixture where the uphill neighbour is
            -- only 2 tiles up — below the shed threshold, so it is NOT
            -- a donor. Under the pre-#812 implementation these two
            -- fixtures produce IDENTICAL soil depth: 'maxDrop'/'reliefNorm'
            -- only read DOWNHILL neighbours, and an uphill neighbour
            -- never raises them regardless of how high it stands. This
            -- comparison is exactly the case the old per-cell
            -- implementation cannot distinguish, and must fail.
            receiverNoDonor = run 50 (52, 50, 50, 50)
        gmIntrusionDepth receiverWithDonor `shouldSatisfy`
            (> gmIntrusionDepth receiverNoDonor)
        gmMaterialOverride receiverWithDonor `shouldSatisfy` isJust

    it "an unaffected flat receiver's soil is unchanged (no phantom credit)" $ do
        -- All neighbours flat / below the shed threshold: no donor
        -- anywhere nearby, so behaviour must match pre-#812 exactly.
        let flat = run 50 (50, 50, 50, 50)
        gmIntrusionDepth flat `shouldBe` 2  -- max 1 (round (4*0.5*(1-0)))

    it "redistribution stays bounded when boxed in by donors on every side" $ do
        let single = run 50 (54, 50, 50, 50)
            boxed  = run 50 (54, 54, 54, 54)
        gmIntrusionDepth boxed `shouldSatisfy` (> gmIntrusionDepth single)
        -- Bounded: capped credit (soilShedRelief = 3), not one tile per
        -- donor neighbour, so 4 donor neighbours don't tower unbounded.
        gmIntrusionDepth boxed `shouldSatisfy` (≤ gmIntrusionDepth single + 2)

    it "a donor itself never receives credit even beside a taller neighbour" $ do
        -- This tile (elev 54) itself sheds toward its S neighbour (50,
        -- drop 4) AND has an even taller neighbour to its N (60, drop
        -- from ITS perspective would be negative — 54 is downhill of
        -- 60) — but exposeRock zeroes soil regardless of any upslope
        -- donor, so #225's donor-side result is untouched by #812.
        let donorBesideTallerPeak = run 54 (60, 50, 54, 54)
        gmMaterialOverride donorBesideTallerPeak `shouldBe` Nothing
        gmIntrusionDepth donorBesideTallerPeak `shouldBe` 0
