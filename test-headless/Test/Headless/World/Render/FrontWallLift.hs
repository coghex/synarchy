{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for 'World.Render.Quads.structureFrontWallClear' — the
--   #418 flora/vegetation lift over structure front walls.
--
--   The regression under test (issue #423): loaded chunks are keyed by
--   canonical (u-wrapped) coords, and a chunk's structures by tile
--   coords in that canonical frame. The lift used to probe neighbours
--   with a raw 'globalToChunk' lookup, so a wall just across the
--   cylindrical U seam was silently missed and the sprite kept its
--   unlifted key (the #418 straddle came back at the seam). The fix
--   canonicalises the probed chunk coord AND shifts the structure tile
--   key by the same wrap delta.
--
--   No engine needed: the function is pure. Structure lookups are fed
--   from a hand-built @ChunkCoord → ChunkStructures@ map, mirroring how
--   'renderWorldQuads' backs it with the loaded-chunk map.
module Test.Headless.World.Render.FrontWallLift (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Types (StructureSlot(..), StructurePieceData(..), ChunkStructures)
import World.Chunk.Types (ChunkCoord(..), chunkSeamChebyshev)
import World.Render.Quads (structureFrontWallClear)

seTag ∷ Word8
seTag = fromIntegral (fromEnum SWallSE)

-- | Lookup backed by a plain map: one 'ChunkStructures' per chunk coord.
lookupFrom ∷ [(ChunkCoord, [((Int, Int, Word8), StructurePieceData)])]
           → (ChunkCoord → Maybe ChunkStructures)
lookupFrom chunks cc = HM.lookup cc (HM.fromList [ (c, HM.fromList ps) | (c, ps) ← chunks ])

-- | A wall piece at world z 5 (palette ids irrelevant to sort keys).
wallZ5 ∷ StructurePieceData
wallZ5 = StructurePieceData 0 0 5

-- | worldSize 8 chunks → 128 tiles around the cylinder, canonical chunk
--   u ∈ [-4, 4). All scenarios use zSlice 5 = the wall z, so the key's
--   z-term is zero and the expected key is depth + tieBreak alone.
ws, zSlice ∷ Int
ws = 8
zSlice = 5

-- | Interior reference layout: SE wall at (-1,2) in chunk (-1,0), sprite
--   at (1,2) — two tiles east in grid space, spatially in front of the
--   wall's south-vertex anchor (0,3) at FaceSouth (depth 3 vs 3).
interiorLookup ∷ ChunkCoord → Maybe ChunkStructures
interiorLookup = lookupFrom [ (ChunkCoord (-1) 0, [((-1, 2, seTag), wallZ5)]) ]

-- | Seam layout with the SAME relative geometry: sprite at (-31,34)
--   (chunk (-2,2), tile u = -65 in its chunk's frame), wall on the
--   physically adjacent tile two steps east ACROSS the U seam. That
--   tile's raw coords (-33,34) live in raw chunk (-3,2), u = -5 —
--   non-canonical; the chunk is loaded as (1,-2) and the wall stored
--   under the wrapped tile key (31,-30).
seamLookup ∷ ChunkCoord → Maybe ChunkStructures
seamLookup = lookupFrom [ (ChunkCoord 1 (-2), [((31, -30, seTag), wallZ5)]) ]

spec ∷ Spec
spec = do
    it "lifts a sprite in front of an interior front wall (#418 baseline)" $ do
        let k = structureFrontWallClear FaceSouth ws zSlice interiorLookup 1 2
        k `shouldSatisfy` isJust
        -- Pin the strip-key formula: anchor depth 3, z-term 0 (wall z ≡
        -- zSlice), SE tieBreak 0.0006.
        k `shouldSatisfy` maybe False (\v → abs (v - 3.0006) < 1.0e-4)

    it "does not lift a sprite behind the wall" $ do
        -- depth 1+0 = 1 < wall anchor depth 3 → spatially behind.
        structureFrontWallClear FaceSouth ws zSlice interiorLookup 1 0
            `shouldBe` Nothing

    it "does not lift when no wall is near" $ do
        structureFrontWallClear FaceSouth ws zSlice (const Nothing) 1 2
            `shouldBe` Nothing

    it "lifts across the U seam exactly like the interior case (#423)" $ do
        let seamK     = structureFrontWallClear FaceSouth ws zSlice seamLookup (-31) 34
            interiorK = structureFrontWallClear FaceSouth ws zSlice interiorLookup 1 2
        seamK `shouldSatisfy` isJust
        -- The u-wrap preserves v = gx+gy, so the seam pair must produce
        -- the SAME strip key as the interior pair with identical relative
        -- geometry: depth 3 + z-term 0 + SE tieBreak.
        seamK `shouldBe` interiorK

    it "skips the cross-seam wall at east/west facings" $ do
        -- Depth follows u at FaceEast; the wall's stored frame sits a
        -- whole world width away, so the lift must not fire.
        structureFrontWallClear FaceEast ws zSlice seamLookup (-31) 34
            `shouldBe` Nothing

    it "still lifts at east/west facings away from the seam" $ do
        -- FaceEast depth = gx - gy: sprite (1,0) depth 1, wall anchor
        -- (0,3) depth -3 → sprite in front; frames agree (no wrap).
        structureFrontWallClear FaceEast ws zSlice interiorLookup 1 0
            `shouldSatisfy` isJust

    it "chunk gate: the seam sprite/wall chunks are wrap-adjacent" $ do
        -- The per-chunk gate in renderWorldQuads admits sprite chunks
        -- within seam-aware Chebyshev 1 of a structure-bearing chunk;
        -- this is the exact pair from the seam scenario above (raw
        -- distance 4, physical distance 1).
        chunkSeamChebyshev ws (ChunkCoord 1 (-2)) (ChunkCoord (-2) 2)
            `shouldBe` 1
