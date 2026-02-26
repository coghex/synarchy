{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Ocean
    ( computeOceanMap
    , isOceanChunk
    , hasAnyOceanFluid
    , computeChunkFluid
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as MV
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Control.Monad (when)
import World.Base
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalU)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal

-----------------------------------------------------------
-- Ocean Flood Fill
-----------------------------------------------------------

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

        -- Wrap chunk coords in u-space (consistent with the isometric world)
        wrapChunkU (ccx, ccy) =
            let w = halfSize * 2
                u = ccx - ccy
                v = ccx + ccy
                halfW = w `div` 2
                wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
                cx' = (wrappedU + v) `div` 2
                cy' = (v - wrappedU) `div` 2
            in (cx', cy')

        neighbors (ChunkCoord cx cy) =
            [ let (cx', cy') = wrapChunkU (cx + dx, cy + dy)
              in ChunkCoord cx' cy'
            | (dx, dy) ← [(-1,0), (1,0), (0,-1), (0,1)]
            , let (_, cy') = wrapChunkU (cx + dx, cy + dy)
            , cy' ≥ -halfSize ∧ cy' < halfSize
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

isOceanChunk ∷ OceanMap → ChunkCoord → Bool
isOceanChunk = flip HS.member

-----------------------------------------------------------
-- Chunk-Level Ocean Fluid
-----------------------------------------------------------

computeChunkFluid ∷ Int → OceanMap → ChunkCoord
                  → VU.Vector Int
                  → FluidMap
computeChunkFluid worldSize oceanMap coord surfaceMap
    | isOceanChunk oceanMap coord =
        buildOceanSurface surfaceMap
    | hasOceanNeighbor =
        buildOceanSurface surfaceMap
    | otherwise = emptyFluidMap
  where
    ChunkCoord cx cy = coord
    wrap = wrapChunkCoordU worldSize
    hasOceanNeighbor =
        isOceanChunk oceanMap (wrap (ChunkCoord cx (cy - 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord cx (cy + 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) cy))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) cy))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) (cy - 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) (cy - 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) (cy + 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) (cy + 1)))

    buildOceanSurface surf =
        withFluidMap $ \mv →
            forEachSurface surf $ \idx _lx _ly surfZ →
                when (surfZ < seaLevel) $
                    MV.write mv idx (Just (FluidCell Ocean seaLevel))

-- | Quick boolean check: does this chunk have any ocean fluid?
--   Just checks the ocean map and neighbors — no vector allocation.
hasAnyOceanFluid ∷ OceanMap → ChunkCoord → Bool
hasAnyOceanFluid oceanMap coord =
    let ChunkCoord cx cy = coord
    in isOceanChunk oceanMap coord
     ∨ isOceanChunk oceanMap (ChunkCoord cx (cy - 1))
     ∨ isOceanChunk oceanMap (ChunkCoord cx (cy + 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) cy)
     ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) cy)
     ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy - 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy - 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy + 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy + 1))
