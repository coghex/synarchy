{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Unit contract for 'breachSealedBasins' (#220): sealed deep
--   sub-sea basins near the shore get a narrow lower-only channel to
--   the open ocean; shallow or small ponds are left alone; glacier
--   lips are never carved; nothing is ever raised.
--
--   Synthetic grids only — no worldgen. worldSize 2 (32×32 tiles)
--   keeps the canonical-space math honest (the pass wraps per
--   'wrapGlobalU', so tiles outside the canonical diamond are
--   projection targets, not decision makers).
module Test.Headless.WorldGen.CoastBreach (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector.Unboxed as VU
import World.Constants (seaLevel)
import World.Chunk.Types (chunkSize)
import World.Material (MaterialId(..), matGlacier)
import World.Geology.Coastal (breachSealedBasins)
import World.Plate (wrapGlobalU)

-- | Build a grid from a per-tile elevation function over global
--   coords, worldSize 2 → worldTiles 32.
worldSizeT ∷ Int
worldSizeT = 2

worldTilesT ∷ Int
worldTilesT = worldSizeT * chunkSize

mkGrid ∷ (Int → Int → Int) → VU.Vector Int
mkGrid f = VU.generate (worldTilesT * worldTilesT) $ \i →
    let gx = i `mod` worldTilesT - worldTilesT `div` 2
        gy = i `div` worldTilesT - worldTilesT `div` 2
    in f gx gy

atG ∷ VU.Vector Int → Int → Int → Int
atG v gx gy =
    v VU.! ((gy + worldTilesT `div` 2) * worldTilesT
            + (gx + worldTilesT `div` 2))

plainMats ∷ VU.Vector MaterialId
plainMats = VU.replicate (worldTilesT * worldTilesT) (MaterialId 1)

-- | Ocean on the low-v side, a 2-tile ridge, then a deep basin.
--   Uses v = gx+gy bands so the geometry lives inside the canonical
--   diamond regardless of the u-wrap.
ridgeWorld ∷ Int → Int → Int
ridgeWorld gx gy
    | v ≤ -6            = -8   -- open ocean (largest water body)
    | v ≡ -5 ∨ v ≡ -4   = 9    -- sealing ridge (land)
    | v ≥ -3 ∧ v ≤ 2    = -10  -- deep sealed basin (qualifies)
    | otherwise         = 6    -- inland
  where v = gx + gy

spec ∷ Spec
spec = describe "breachSealedBasins" $ do
    it "carves a lower-only channel from a sealed deep basin to the ocean" $ do
        let elev = mkGrid ridgeWorld
            out  = breachSealedBasins worldSizeT plainMats elev
        -- something on the ridge went to sea-1
        VU.any (≡ seaLevel - 1) out `shouldBe` True
        -- lower-only: no tile ever rises
        VU.and (VU.zipWith (≥) elev out) `shouldBe` True

    it "leaves shallow sealed ponds alone" $ do
        let shallow gx gy
                | v ≤ -6          = -8
                | v ≡ -5 ∨ v ≡ -4 = 9
                | v ≥ -3 ∧ v ≤ 2  = -2   -- too shallow to qualify
                | otherwise       = 6
              where v = gx + gy
            elev = mkGrid shallow
            out  = breachSealedBasins worldSizeT plainMats elev
        out `shouldBe` elev

    it "never carves glacier tiles" $ do
        let elev = mkGrid ridgeWorld
            mats = VU.generate (worldTilesT * worldTilesT) $ \i →
                let gx = i `mod` worldTilesT - worldTilesT `div` 2
                    gy = i `div` worldTilesT - worldTilesT `div` 2
                in if ridgeWorld gx gy ≡ 9
                   then matGlacier
                   else MaterialId 1
            out = breachSealedBasins worldSizeT mats elev
        -- the only route is through glacier: no carve at all
        out `shouldBe` elev

    it "projects carves consistently onto wrap-alias copies" $ do
        let elev = mkGrid ridgeWorld
            out  = breachSealedBasins worldSizeT plainMats elev
            half = worldTilesT `div` 2
            pairs =
                [ (i, (gx', gy'))
                | i ← [0 .. worldTilesT * worldTilesT - 1]
                , let gx = i `mod` worldTilesT - half
                      gy = i `div` worldTilesT - half
                      (gx', gy') = wrapGlobalU worldSizeT gx gy
                , (gx', gy') ≢ (gx, gy)
                ]
        -- every alias tile equals its canonical image
        forM_ pairs $ \(i, (gx', gy')) →
            out VU.! i `shouldBe` atG out gx' gy'
