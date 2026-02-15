{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluids
    ( -- * Types
      FluidType(..)
    , FluidCell(..)
    , OceanMap
      -- * Constants
    , seaLevel
    , regionSize
      -- * Region
    , RegionCoord(..)
    , Region(..)
    , chunkToRegion
      -- * Ocean flood fill
    , computeOceanMap
      -- * Chunk-level fluid
    , computeChunkFluid
      -- * Query
    , isOceanChunk
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalU)

-- | Compute which chunks are ocean by flood-filling from
--   ocean plate centers. A chunk is ocean if:
--   1. Its center elevation (from plates) is below sea level
--   2. It's reachable from an ocean plate center without
--      crossing chunks above sea level
--
--   This correctly leaves inland basins unflooded.
computeOceanMap ∷ Word64 → Int → Int → [TectonicPlate]
               → (Int → Int → (Int, MaterialId) → (Int, MaterialId))
               → OceanMap
computeOceanMap seed worldSize plateCount plates applyTL =
    let halfSize = worldSize `div` 2

        chunkElev ∷ ChunkCoord → Int
        chunkElev (ChunkCoord cx cy) =
            let midGX = cx * chunkSize + chunkSize `div` 2
                midGY = cy * chunkSize + chunkSize `div` 2
                (gx', gy') = wrapGlobalU worldSize midGX midGY
            in if isBeyondGlacier worldSize gx' midGY
               then seaLevel + 100
               else let (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' midGY
                    in if baseMat ≡ matGlacier
                       then seaLevel + 100
                       else fst (applyTL gx' gy' (baseElev, baseMat))

        -- Find seed chunks: for each ocean plate, find the chunk
        -- containing its center, verify it's below sea level
        oceanSeeds = concatMap (\plate →
            if plateIsLand plate
            then []
            else let cx = floorDiv' (plateCenterX plate) chunkSize
                     cy = floorDiv' (plateCenterY plate) chunkSize
                     coord = ChunkCoord cx cy
                 in if cx ≥ -halfSize ∧ cx < halfSize
                     ∧ cy ≥ -halfSize ∧ cy < halfSize
                     ∧ chunkElev coord < seaLevel
                    then [coord]
                    else []
            ) plates

        -- BFS flood fill from seeds
        -- Wraps X, clamps Y to world bounds
        wrapChunkX cx =
            let wrapped = ((cx + halfSize) `mod` (halfSize * 2) + (halfSize * 2))
                          `mod` (halfSize * 2) - halfSize
            in wrapped

        neighbors (ChunkCoord cx cy) =
            [ ChunkCoord (wrapChunkX (cx + dx)) (cy + dy)
            | (dx, dy) ← [(-1,0), (1,0), (0,-1), (0,1)]
            , let ny = cy + dy
            , ny ≥ -halfSize ∧ ny < halfSize
            ]

        bfs ∷ Seq ChunkCoord → HS.HashSet ChunkCoord → HS.HashSet ChunkCoord
        bfs Empty visited = visited
        bfs (current :<| queue) visited =
            let nextNeighbors = filter (\n →
                    not (HS.member n visited)
                    ∧ chunkElev n < seaLevel
                    ) (neighbors current)
                visited' = foldl' (flip HS.insert) visited nextNeighbors
                queue' = foldl' (:|>) queue nextNeighbors
            in bfs queue' visited'

        initialVisited = HS.fromList oceanSeeds
        initialQueue = Seq.fromList oceanSeeds

    in bfs initialQueue initialVisited

-- | Check if a chunk is in the ocean map.
isOceanChunk ∷ OceanMap → ChunkCoord → Bool
isOceanChunk = flip HS.member

-----------------------------------------------------------
-- Chunk-Level Fluid Computation
-----------------------------------------------------------

-- | Compute the fluid map for a loaded chunk.
--   
--   Three cases produce ocean tiles:
--   1. The chunk itself is in the ocean map → any column below sea level gets water
--   2. A neighbor chunk is ocean → edge tiles below sea level on that side get water,
--      then BFS inward to fill connected below-sea-level tiles
--   3. Neither → no ocean (inland basin, correctly unflooded)
--
--   This gives seamless coastlines: land chunks whose terrain slopes
--   below sea level at the ocean boundary get water up to the cliff face.
computeChunkFluid ∷ OceanMap → ChunkCoord
                  → HM.HashMap (Int, Int) Int   -- ^ surfaceMap (terrain elevation)
                  → HM.HashMap (Int, Int) FluidCell
computeChunkFluid oceanMap coord surfaceMap
    -- Case 1: full ocean chunk — every below-sea-level column gets water
    | isOceanChunk oceanMap coord = HM.foldlWithKey' (\acc (lx, ly) surfZ →
        if surfZ < seaLevel
        then HM.insert (lx, ly) (FluidCell Ocean seaLevel) acc
        else acc
        ) HM.empty surfaceMap

    -- Case 2: land chunk — check if any neighbor is ocean, flood from edges
    | otherwise =
        let ChunkCoord cx cy = coord
            -- Which edges border an ocean chunk?
            oceanN = isOceanChunk oceanMap (ChunkCoord cx (cy - 1))
            oceanS = isOceanChunk oceanMap (ChunkCoord cx (cy + 1))
            oceanE = isOceanChunk oceanMap (ChunkCoord (cx + 1) cy)
            oceanW = isOceanChunk oceanMap (ChunkCoord (cx - 1) cy)
            -- Also check diagonal neighbors for corner coverage
            oceanNE = isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy - 1))
            oceanNW = isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy - 1))
            oceanSE = isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy + 1))
            oceanSW = isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy + 1))
        in if not (oceanN ∨ oceanS ∨ oceanE ∨ oceanW
                 ∨ oceanNE ∨ oceanNW ∨ oceanSE ∨ oceanSW)
           -- No ocean neighbors at all → definitely no coastal water
           then HM.empty
           -- Seed BFS from edge tiles that face an ocean neighbor
           else let maxIdx = chunkSize - 1
                    -- Seed tiles: edge tiles adjacent to an ocean chunk
                    -- AND below sea level
                    seeds = filter belowSea $
                        (if oceanN then [(lx, 0)  | lx ← [0..maxIdx]] else [])
                     ++ (if oceanS then [(lx, maxIdx) | lx ← [0..maxIdx]] else [])
                     ++ (if oceanE then [(maxIdx, ly) | ly ← [0..maxIdx]] else [])
                     ++ (if oceanW then [(0, ly)  | ly ← [0..maxIdx]] else [])
                     -- Corner seeds for diagonal ocean neighbors
                     ++ (if oceanNE then [(maxIdx, 0)] else [])
                     ++ (if oceanNW then [(0, 0)] else [])
                     ++ (if oceanSE then [(maxIdx, maxIdx)] else [])
                     ++ (if oceanSW then [(0, maxIdx)] else [])

                    belowSea (lx, ly) = case HM.lookup (lx, ly) surfaceMap of
                        Just z  → z < seaLevel
                        Nothing → False

                    -- BFS inward: flood connected below-sea-level tiles
                    bfs ∷ Seq (Int, Int) → HS.HashSet (Int, Int)
                        → HM.HashMap (Int, Int) FluidCell
                        → HM.HashMap (Int, Int) FluidCell
                    bfs Empty _ acc = acc
                    bfs (cur :<| queue) visited acc =
                        let (lx, ly) = cur
                            nbrs = filter (\(nx, ny) →
                                       nx ≥ 0 ∧ nx ≤ maxIdx
                                     ∧ ny ≥ 0 ∧ ny ≤ maxIdx
                                     ∧ not (HS.member (nx, ny) visited)
                                     ∧ belowSea (nx, ny)
                                   )
                                   [(lx-1,ly), (lx+1,ly), (lx,ly-1), (lx,ly+1)]
                            visited' = foldl' (flip HS.insert) visited nbrs
                            queue'   = foldl' (:|>) queue nbrs
                            acc'     = foldl' (\a (nx,ny) →
                                           HM.insert (nx,ny) (FluidCell Ocean seaLevel) a
                                       ) acc nbrs
                        in bfs queue' visited' acc'

                    seedSet = HS.fromList seeds
                    seedAcc = HM.fromList
                        [(s, FluidCell Ocean seaLevel) | s ← seeds]
                    seedQueue = Seq.fromList seeds

                in bfs seedQueue seedSet seedAcc

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)
