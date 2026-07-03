{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Unit contracts for the #223 bed-depth work: the inland rift
--   field ('World.Plate.riftFieldMemo'), the river channel bed model
--   ('World.Fluid.River.Identify.computeBedDepth'), and the graben
--   lake carve ('World.Fluid.Lake.Graben.grabenCarveIndex').
--
--   Synthetic grids only — no worldgen.
module Test.Headless.WorldGen.BedDepth (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Graben
    ( grabenCarveIndex, grabenRiftThreshold, grabenMinArea )
import World.Fluid.Lake.Types
    ( Lake(..), LakeChunkEntry(..), WorldLakes(..) )
import World.Fluid.River.Identify
    ( computeBedDepth, depthFromRadius, maxBedDepth )
import World.Plate
    ( generatePlates, riftFieldMemo, riftTectonicIntensity, wrapGlobalU )

worldSizeT ∷ Int
worldSizeT = 2

worldTilesT ∷ Int
worldTilesT = worldSizeT * chunkSize

nTilesT ∷ Int
nTilesT = worldTilesT * worldTilesT

halfT ∷ Int
halfT = worldTilesT `div` 2

idxOf ∷ Int → Int → Int
idxOf gx gy = (gy + halfT) * worldTilesT + (gx + halfT)

mkGrid ∷ (Int → Int → Int) → VU.Vector Int
mkGrid f = VU.generate nTilesT $ \i →
    let gx = i `mod` worldTilesT - halfT
        gy = i `div` worldTilesT - halfT
    in f gx gy

-- | A straight west-east river along gy ≡ 0: centre tiles with
--   width radius 0, perp 0, surface at the given z.
riverRow ∷ Int → (VU.Vector Bool, VU.Vector Int, VU.Vector Int, VU.Vector Int)
riverRow surf =
    let isR  = VU.generate nTilesT $ \i →
            i `div` worldTilesT ≡ halfT     -- gy ≡ 0 row
        wr   = VU.map (\r → if r then 0 else (-1)) isR
        perp = VU.replicate nTilesT 0
        sz   = VU.map (\r → if r then surf else minBound) isR
    in (isR, wr, perp, sz)

noRift ∷ Int → Int → Float
noRift _ _ = 0.0

fullRift ∷ Int → Int → Float
fullRift _ _ = 1.0

spec ∷ Spec
spec = describe "bed depth (#223)" $ do
    describe "riftFieldMemo" $ do
        let plates = generatePlates 42 worldSizeT 4
            direct = riftTectonicIntensity 42 worldSizeT plates
            memo   = riftFieldMemo 42 worldSizeT plates
            coords = [ (gx, gy)
                     | gx ← [-halfT, -halfT + 5 .. halfT - 1]
                     , gy ← [-halfT, -halfT + 5 .. halfT - 1] ]

        it "stays in [0,1]" $
            forM_ coords $ \(gx, gy) → do
                let v = memo gx gy
                v `shouldSatisfy` (≥ 0.0)
                v `shouldSatisfy` (≤ 1.0)

        it "memoized field equals the direct query" $
            forM_ coords $ \(gx, gy) →
                memo gx gy `shouldBe` direct gx gy

        it "is wrap-exact (tile ≡ its u-alias)" $ do
            let canonical = [ (gx, gy) | (gx, gy) ← coords
                            , gx - gy ≥ negate halfT, gx - gy < halfT ]
            forM_ canonical $ \(gx, gy) → do
                let u = gx - gy
                    v = gx + gy
                    -- shift one full wrap in u
                    gx' = (u + worldTilesT + v) `div` 2
                    gy' = (v - u - worldTilesT) `div` 2
                    (cgx, cgy) = wrapGlobalU worldSizeT gx' gy'
                -- the alias projects back onto the canonical tile...
                (cgx, cgy) `shouldBe` (gx, gy)
                -- ...and the field agrees at the alias coordinates
                memo gx' gy' `shouldBe` memo gx gy

    describe "computeBedDepth" $ do
        it "keeps ordinary flat reaches at exactly depthFromRadius" $ do
            let terrain = mkGrid (\_ _ → 5)
                (isR, wr, perp, sz) = riverRow 5
                bd = computeBedDepth 42 worldSizeT terrain isR wr perp
                                     sz noRift
            forM_ [-halfT .. halfT - 1] $ \gx →
                bd VU.! idxOf gx 0 `shouldBe` depthFromRadius 0

        it "deepens confined reaches but never past maxBedDepth" $ do
            -- sheer 40-z walls one tile to each side of the channel
            let terrain = mkGrid $ \_ gy →
                    if abs gy ≤ 1 then 20 else 60
                (isR, wr, perp, sz) = riverRow 20
                bd = computeBedDepth 42 worldSizeT terrain isR wr perp
                                     sz noRift
                depths = [ bd VU.! idxOf gx 0
                         | gx ← [-halfT .. halfT - 1] ]
            maximum depths `shouldSatisfy` (> depthFromRadius 0)
            forM_ depths $ \d → do
                d `shouldSatisfy` (≥ 1)
                d `shouldSatisfy` (≤ maxBedDepth)

        it "rift intensity alone deepens the bed" $ do
            let terrain = mkGrid (\_ _ → 5)
                (isR, wr, perp, sz) = riverRow 5
                bd = computeBedDepth 42 worldSizeT terrain isR wr perp
                                     sz fullRift
                depths = [ bd VU.! idxOf gx 0
                         | gx ← [-halfT .. halfT - 1] ]
            maximum depths `shouldSatisfy` (> depthFromRadius 0)

        it "never deepens a near-sea reach below the unboosted fit" $ do
            -- max confinement AND max rift at a surface of seaLevel+1:
            -- the boost cannot afford any depth, base fit wins.
            let terrain = mkGrid $ \_ gy →
                    if abs gy ≤ 1 then seaLevel + 1 else 60
                (isR, wr, perp, sz) = riverRow (seaLevel + 1)
                bd = computeBedDepth 42 worldSizeT terrain isR wr perp
                                     sz fullRift
            forM_ [-halfT .. halfT - 1] $ \gx →
                bd VU.! idxOf gx 0 `shouldBe` depthFromRadius 0

    describe "grabenCarveIndex" $ do
        -- one square inland lake, surface 10, natural floor 9
        -- (1-deep pan), big enough to qualify
        let side = 12   -- 144 tiles ≥ grabenMinArea
            inLake gx gy = gx ≥ 0 ∧ gx < side ∧ gy ≥ 0 ∧ gy < side
            terrain = mkGrid $ \gx gy → if inLake gx gy then 9 else 14
            lake = Lake { lkSurface = 10, lkFloor = 9
                        , lkArea = side * side
                        , lkBBoxMinX = 0, lkBBoxMinY = 0
                        , lkBBoxMaxX = side - 1, lkBBoxMaxY = side - 1 }
            entryFor cx cy = LakeChunkEntry
                { lceLakeId = 0
                , lceBitmask = VU.generate (chunkSize * chunkSize) $ \li →
                    let lx = li `mod` chunkSize
                        ly = li `div` chunkSize
                    in inLake (cx * chunkSize + lx) (cy * chunkSize + ly)
                }
            byChunk = HM.fromList
                [ (ChunkCoord cx cy, V.singleton (entryFor cx cy))
                | cx ← [0 .. (side - 1) `div` chunkSize]
                , cy ← [0 .. (side - 1) `div` chunkSize] ]
            lakes = WorldLakes { wlLakes = V.singleton lake
                               , wlByChunk = byChunk
                               , wlCarveDelta = HM.empty }

        it "grabens a shallow rifted lake, shore shelf untouched" $ do
            let out = grabenCarveIndex 42 worldSizeT terrain fullRift lakes
            HM.null out `shouldBe` False
            let deltaAt gx gy =
                    case HM.lookup (ChunkCoord (gx `div` chunkSize)
                                               (gy `div` chunkSize)) out of
                        Nothing → 0
                        Just dv →
                            let lx = ((gx `mod` chunkSize) + chunkSize)
                                     `mod` chunkSize
                                ly = ((gy `mod` chunkSize) + chunkSize)
                                     `mod` chunkSize
                            in dv VU.! (ly * chunkSize + lx)
            -- interior deepens
            deltaAt (side `div` 2) (side `div` 2) `shouldSatisfy` (> 0)
            -- ring-1 shore shelf untouched
            forM_ [0 .. side - 1] $ \gx →
                deltaAt gx 0 `shouldBe` 0
            -- deltas are bounded by the graben depth cap (24) plus
            -- the shallow pan's existing 1 z of depth
            forM_ [ (gx, gy) | gx ← [0 .. side - 1], gy ← [0 .. side - 1] ] $
                \(gx, gy) → deltaAt gx gy `shouldSatisfy` (≤ 24)

        it "ignores lakes below the rift threshold" $ do
            let weak _ _ = grabenRiftThreshold - 0.05
                out = grabenCarveIndex 42 worldSizeT terrain weak lakes
            out `shouldBe` HM.empty

        it "ignores small lakes regardless of rift" $ do
            let tiny = lakes { wlLakes = V.singleton
                                 lake { lkArea = grabenMinArea - 1 } }
                out = grabenCarveIndex 42 worldSizeT terrain fullRift tiny
            out `shouldBe` HM.empty

        it "ignores sea-level (coastal-clamped) lakes" $ do
            let clamped = lakes { wlLakes = V.singleton
                                    lake { lkSurface = seaLevel } }
                out = grabenCarveIndex 42 worldSizeT terrain fullRift clamped
            out `shouldBe` HM.empty
