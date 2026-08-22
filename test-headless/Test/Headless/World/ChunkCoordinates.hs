{-# LANGUAGE Strict #-}
-- | Pure tests for the shared chunk-coordinate derivation (#1113).
--
--   Five modules used to hand-roll floor division by 'chunkSize', each
--   pasting a @if r < 0 then q - 1@ correction onto @divMod@ — a
--   correction 'divMod' already makes, so every copy's guard was dead.
--   They now all call 'globalToChunk'.
--
--   The greps in #1113 prove the copies are gone; these tests prove the
--   survivor computes what they computed. Each case is stated as the
--   independent arithmetic identity the removed helpers relied on, not
--   as a transcript of 'globalToChunk', so a future edit to the shared
--   helper fails here rather than silently moving worldgen output.
module Test.Headless.World.ChunkCoordinates (spec) where

import UPrelude
import Test.Hspec
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Generate.Coordinates (globalToChunk, chunkToGlobal)

-- | The chunk coord and local index the five removed helpers built:
--   floor-divide each axis, then take the offset back from the chunk's
--   own origin. Written with @quot@ plus an explicit negative
--   correction — the formulation those copies THOUGHT they needed — so
--   it is arithmetically independent of 'div'.
expected ∷ Int → Int → (ChunkCoord, (Int, Int))
expected gx gy =
    let fd a = let (q, r) = a `quotRem` chunkSize
               in if r < 0 then q - 1 else q
        cx = fd gx
        cy = fd gy
    in (ChunkCoord cx cy, (gx - cx * chunkSize, gy - cy * chunkSize))

-- | Coords spanning several chunks either side of the origin, hitting
--   both chunk boundaries and interiors on each axis.
coords ∷ [(Int, Int)]
coords = [ (gx, gy) | gx ← axis, gy ← axis ]
  where
    axis = [ -33, -32, -31, -17, -16, -15, -1, 0, 1
           , 15, 16, 17, 31, 32, 33 ]

spec ∷ Spec
spec = do
    it "matches the floor-division the five removed helpers computed" $
        [ globalToChunk gx gy | (gx, gy) ← coords ]
            `shouldBe` [ expected gx gy | (gx, gy) ← coords ]

    it "puts every local offset inside the chunk" $
        [ (lx, ly)
        | (gx, gy) ← coords
        , let (_, (lx, ly)) = globalToChunk gx gy
        , lx < 0 ∨ lx ≥ chunkSize ∨ ly < 0 ∨ ly ≥ chunkSize
        ] `shouldBe` []

    it "round-trips back to the global coord it was given" $
        [ chunkToGlobal cc lx ly
        | (gx, gy) ← coords
        , let (cc, (lx, ly)) = globalToChunk gx gy
        ] `shouldBe` coords

    it "keeps negative coords in the chunk on their own side of zero" $
        map (fst ∘ uncurry globalToChunk)
            [ (-1, -1), (-16, -16), (-17, -17), (0, 0) ]
            `shouldBe` [ ChunkCoord (-1) (-1)
                       , ChunkCoord (-1) (-1)
                       , ChunkCoord (-2) (-2)
                       , ChunkCoord 0 0
                       ]
