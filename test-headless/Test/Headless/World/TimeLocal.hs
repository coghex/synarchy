{-# LANGUAGE UnicodeSyntax #-}
-- | Pure tests for 'localSunAngle' (#483): longitude-local solar time.
--   Pins the properties the rendering/gameplay hookup depends on —
--   seam continuity, noon at the meridian, monotonic phase along u,
--   and sane behaviour for a degenerate world size — without needing
--   a GPU or a generated world.
module Test.Headless.World.TimeLocal (spec) where

import UPrelude
import Test.Hspec
import World.Chunk.Types (chunkSize)
import World.Time.Local (localSunAngle)

-- w64: worldSize=64 chunks → circumference = 64 * chunkSize tiles.
circumference64 ∷ Int
circumference64 = 64 * chunkSize

spec ∷ Spec
spec = describe "localSunAngle" $ do
    it "agrees with the global clock at the meridian (u = gx - gy = 0)" $
        localSunAngle 64 0 0 0.5 `shouldBe` 0.5

    it "agrees with the global clock anywhere on the u=0 line" $
        localSunAngle 64 10 10 0.5 `shouldBe` 0.5

    it "the seam agrees: u and u + circumference give the same phase" $
        let gx = 10 ∷ Int
            gy = 0
            here = localSunAngle 64 gx gy 0.25
            wrapped = localSunAngle 64 (gx + circumference64) gy 0.25
        in wrapped `shouldBe` here

    it "the seam agrees the other direction too (u - circumference)" $
        let gx = 10 ∷ Int
            gy = 0
            here = localSunAngle 64 gx gy 0.25
            wrapped = localSunAngle 64 (gx - circumference64) gy 0.25
        in wrapped `shouldBe` here

    it "a full trip around the cylinder returns to the same phase" $
        -- 0.5 (not an arbitrary decimal like 0.1) so the +1.0/-1.0 carry
        -- round-trips exactly in Float and this can assert bit-equality.
        localSunAngle 64 circumference64 0 0.5
            `shouldBe` localSunAngle 64 0 0 0.5

    it "stays in [0, 1)" $ do
        localSunAngle 64 500 (-500) 0.9 `shouldSatisfy` (\a → a ≥ 0 ∧ a < 1)
        localSunAngle 64 (-500) 500 0.05 `shouldSatisfy` (\a → a ≥ 0 ∧ a < 1)

    it "phase increases monotonically with u across a quarter of the cylinder" $
        let angleAt u = localSunAngle 64 u 0 0.0
            step = circumference64 `div` 8
        in mapM_ (\u → angleAt u `shouldSatisfy` (< angleAt (u + step)))
                 [0, step .. circumference64 `div` 4 - step]

    it "opposite sides of a small planet are twelve hours apart" $
        let here  = localSunAngle 64 0 0 0.0
            there = localSunAngle 64 (circumference64 `div` 2) 0 0.0
        in abs (there - here) `shouldBe` 0.5

    it "degenerate world size (0, e.g. an unset arena) doesn't blow up" $
        localSunAngle 0 5 0 0.5 `shouldSatisfy` (\a → a ≥ 0 ∧ a < 1)
