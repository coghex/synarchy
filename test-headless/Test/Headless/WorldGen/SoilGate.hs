{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the post-classification soil gates in
--   'World.Generate.Chunk' (`surfaceDemotion` and friends). The climate
--   classifier paints soils with no physical context; these gates demote
--   a soil that can't survive the final terrain (wetland soils need flat
--   + wet ground; salt flat needs a flat basin floor).
--
--   We exercise them directly on synthetic terrain/water-table vectors —
--   the gating is the entire logic, and worldgen rarely produces the
--   cold hyper-arid climate that yields salt flat at all, so a unit test
--   is the reliable way to verify the salt-flat arm.
module Test.Headless.WorldGen.SoilGate (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (chunkSize)
import World.Generate.Chunk
    (surfaceDemotion, demoteWetland, wetlandKeep, saltFlatKeep, nearFlat)

saltFlat, lightGravel, muck, clay ∷ Word8
saltFlat    = 67
lightGravel = 66
muck        = 64
clay        = 50

idxOf ∷ Int → Int → Int
idxOf lx ly = ly * chunkSize + lx

-- | A flat terrain field at a uniform z, optionally raising one tile to
--   create a slope next to the test tile (8,8).
flatField ∷ Int → VU.Vector Int
flatField z = VU.replicate (chunkSize * chunkSize) z

-- | Flat field with a single raised neighbour east of (8,8), so (8,8)
--   has a 4-neighbour Δ of @bump@.
slopeField ∷ Int → Int → VU.Vector Int
slopeField z bump = flatField z VU.// [(idxOf 9 8, z + bump)]

-- | No out-of-chunk neighbours needed — the test tile is interior.
noOut ∷ Int → Int → Maybe Int
noOut _ _ = Nothing

ti ∷ Int
ti = idxOf 8 8

spec ∷ Spec
spec = do
    describe "nearFlat" $ do
        it "is flat when all 4-neighbours are within Δ2" $
            nearFlat noOut (flatField 10) ti `shouldBe` True
        it "is not flat when a neighbour differs by more than 2" $
            nearFlat noOut (slopeField 10 5) ti `shouldBe` False

    describe "saltFlatKeep" $ do
        it "keeps salt flat on a flat basin floor" $
            saltFlatKeep noOut (flatField 10) ti `shouldBe` True
        it "rejects salt flat on a slope" $
            saltFlatKeep noOut (slopeField 10 5) ti `shouldBe` False
        it "keeps salt flat sub-sea (left to the seabed pass)" $
            -- tz ≤ seaLevel even though steeply sloped.
            saltFlatKeep noOut (slopeField (-3) 5) ti `shouldBe` True

    describe "surfaceDemotion — salt flat" $ do
        let wt = flatField 100   -- irrelevant to the salt-flat arm
        it "leaves a flat salt flat unchanged" $
            surfaceDemotion noOut (flatField 10) wt ti saltFlat
              `shouldBe` Nothing
        it "demotes a sloped salt flat to light gravel" $
            surfaceDemotion noOut (slopeField 10 5) wt ti saltFlat
              `shouldBe` Just lightGravel

    describe "surfaceDemotion — wetland (regression)" $ do
        let wetWt = flatField 10   -- wt ≥ tz-1 at z=10
            dryWt = flatField 0    -- wt well below tz
        it "keeps muck on flat, wet ground" $
            surfaceDemotion noOut (flatField 10) wetWt ti muck
              `shouldBe` Nothing
        it "demotes muck on a slope to clay" $
            surfaceDemotion noOut (slopeField 10 5) wetWt ti muck
              `shouldBe` Just clay
        it "demotes muck on flat but dry ground to clay" $
            surfaceDemotion noOut (flatField 10) dryWt ti muck
              `shouldBe` Just clay

    describe "surfaceDemotion — non-gated material" $
        it "leaves ordinary loam alone" $
            surfaceDemotion noOut (slopeField 10 5) (flatField 0) ti 56
              `shouldBe` Nothing

    describe "demoteWetland table" $
        it "maps the three wetland soils to their next rung" $
            map demoteWetland [62, 63, 64, 56]
              `shouldBe` [Just 57, Just 58, Just 50, Nothing]
