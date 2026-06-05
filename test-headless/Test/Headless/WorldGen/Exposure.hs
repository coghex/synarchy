-- | Column-exposure invariant: any tile face visible from open air
--   must be backed by a stored, non-air material.
--
--   The renderer draws a blank placeholder quad whenever a visible
--   column position has no stored material ('World.Render.Quads'
--   @blankTileToQuad@: index out of range or @matAir@). At gen time
--   the column build ('World.Generate.Chunk', @exposeFrom@) promises
--   to store solid strata from the lowest 4-cardinal neighbour
--   surface up to the column's own surface — this spec pins that
--   promise:
--
--   For every column @c@ with terrain surface @tz@ and cardinal
--   neighbour surfaces @ns@ (in-chunk, plus cross-chunk when the
--   neighbour chunk is loaded):
--
--   > ctStartZ c ≤ min (tz : ns)            -- coverage reaches down
--   > ctMats c ! (z - ctStartZ) ≠ matAir    -- ∀ z ∈ [min ns .. tz]
--
--   Violations render as blank/void tiles on cliff faces. The
--   volcano case (w128 seed 42, chunk (-5,6)) exercises the
--   basalt-cap + lava paths where the user reported blanks.
module Test.Headless.WorldGen.Exposure (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Headless.Harness
import World.Types

-- | Sentinel guard: beyond-glacier columns store minBound terrain and
--   empty mats; both directions of the invariant skip them.
realZ ∷ Int → Bool
realZ z = z > minBound + 1 ∧ z > -100000

-- | One violation: (chunk, lx, ly, z, reason).
data Violation = Violation ChunkCoord Int Int Int String
    deriving Show

-- | Check every column of one chunk against the exposure invariant.
--   @lookupNbrZ@ resolves a neighbour tile's terrain surface across
--   chunk borders; returns Nothing when the neighbour chunk isn't
--   loaded (skipped — can't claim exposure against unknown terrain).
chunkViolations
    ∷ HM.HashMap ChunkCoord LoadedChunk → ChunkCoord → LoadedChunk
    → [Violation]
chunkViolations world coord lc =
    [ v
    | ly ← [0 .. chunkSize - 1]
    , lx ← [0 .. chunkSize - 1]
    , let idx = columnIndex lx ly
          tz  = lcTerrainSurfaceMap lc VU.! idx
          col = lcTiles lc V.! idx
    , realZ tz
    , let nbrZs = [ z
                  | (dx, dy) ← [(1,0), (-1,0), (0,1), (0,-1)]
                  , Just z ← [lookupNbrZ (lx + dx) (ly + dy)]
                  , realZ z
                  ]
    , let exposedLow = minimum (tz : nbrZs)
    , v ← violationsFor lx ly tz exposedLow (ctStartZ col) (ctMats col)
    ]
  where
    ChunkCoord cx cy = coord
    lookupNbrZ nx ny
        | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
            Just (lcTerrainSurfaceMap lc VU.! columnIndex nx ny)
        | otherwise =
            let ccx = cx + ((nx - (nx `mod` chunkSize)) `div` chunkSize)
                ccy = cy + ((ny - (ny `mod` chunkSize)) `div` chunkSize)
                nx' = nx `mod` chunkSize
                ny' = ny `mod` chunkSize
            in do
                nlc ← HM.lookup (ChunkCoord ccx ccy) world
                let z = lcTerrainSurfaceMap nlc VU.! columnIndex nx' ny'
                if VU.null (ctMats (lcTiles nlc V.! columnIndex nx' ny'))
                    then Nothing   -- empty (beyond-glacier) neighbour
                    else Just z
    violationsFor lx ly tz exposedLow startZ mats
        -- A column with a real surface must store SOMETHING — empty
        -- mats with real terrain renders as a void.
        | VU.null mats =
            [ Violation coord lx ly tz "real surface but empty column" ]
        | startZ > exposedLow =
            [ Violation coord lx ly exposedLow
                ("column starts at " ⧺ show startZ
                 ⧺ " but exposed down to " ⧺ show exposedLow) ]
        | otherwise =
            [ Violation coord lx ly z "matAir in exposed band"
            | z ← [exposedLow .. tz]
            , let i = z - startZ
            , i ≥ 0 ∧ i < VU.length mats
            , mats VU.! i ≡ 0
            ]

worldViolations ∷ HM.HashMap ChunkCoord LoadedChunk → [Violation]
worldViolations world =
    [ v
    | (coord, lc) ← HM.toList world
    , v ← chunkViolations world coord lc
    ]

reportViolations ∷ [Violation] → Expectation
reportViolations [] = pure ()
reportViolations vs = expectationFailure $
    show (length vs) ⧺ " exposed positions lack a solid tile; first 10: "
    ⧺ show (take 10 vs)

spec ∷ Spec
spec = aroundAll withHeadlessEngine $ do

    describe "Column exposure invariant" $ do

        it "holds across a w64 world (seed 42)" $ \env → do
            sendWorldCommand env
                (WorldInit (WorldPageId "expo64") 42 64 3)
            ws ← waitForWorldInit env (WorldPageId "expo64") 120
            tiles ← getWorldTileData ws
            reportViolations (worldViolations (wtdChunks tiles))

        it "holds around a volcano with lava + basalt caps (w128 seed 42)" $ \env → do
            sendWorldCommand env
                (WorldInit (WorldPageId "expo128") 42 128 3)
            ws ← waitForWorldInit env (WorldPageId "expo128") 300
            -- The seed-42 w128 volcano breaching a mountain lake sits
            -- near tile (-78, 102) → chunk (-5, 6). Queue it plus a
            -- ring so cross-chunk neighbours resolve.
            queueChunks ws [ ChunkCoord cx cy
                           | cx ← [-7 .. -3], cy ← [4 .. 8] ]
            ok ← waitForChunksAt ws (ChunkCoord (-5) 6) 120
            ok `shouldBe` True
            -- Last queued coord — when it's in, the whole ring is.
            okLast ← waitForChunksAt ws (ChunkCoord (-3) 8) 120
            okLast `shouldBe` True
            tiles ← getWorldTileData ws
            reportViolations (worldViolations (wtdChunks tiles))
