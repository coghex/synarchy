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
            in if isBeyondGlacier worldSize gx' gy'
               then seaLevel + 100
               else let (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
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

    -- Case 2: land chunk bordering ocean — flood ALL below-sea-level columns.
    -- The chunk-level ocean map already prevents inland basins from being
    -- adjacent to ocean chunks, so any below-sea-level tile in a chunk
    -- bordering the ocean should have water.
    | hasOceanNeighbor = HM.foldlWithKey' (\acc (lx, ly) surfZ →
        if surfZ < seaLevel
        then HM.insert (lx, ly) (FluidCell Ocean seaLevel) acc
        else acc
        ) HM.empty surfaceMap

    -- Case 3: no ocean neighbors — no ocean water
    | otherwise = HM.empty
  where
    ChunkCoord cx cy = coord
    hasOceanNeighbor =
        isOceanChunk oceanMap (ChunkCoord cx (cy - 1))
      ∨ isOceanChunk oceanMap (ChunkCoord cx (cy + 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) cy)
      ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) cy)
      ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy - 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy - 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy + 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy + 1))

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)
