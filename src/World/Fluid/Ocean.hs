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
import Control.Monad (when, forM_)
import Control.Monad.ST (ST)
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

        -- Sample a single tile's post-timeline elevation
        sampleElev gx gy =
            let (gx', gy') = wrapGlobalU worldSize gx gy
            in if isBeyondGlacier worldSize gx' gy'
               then seaLevel + 100
               else let (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
                    in if baseMat ≡ matGlacier
                       then seaLevel + 100
                       else fst (applyTL gx' gy' (baseElev, baseMat))

        -- Use MINIMUM elevation across 5 sample points (center + corners).
        -- This ensures the BFS can traverse chunks where part of the
        -- chunk is below sea level (e.g., river valleys, coastal areas)
        -- even if the center point is above sea level.
        chunkElev ∷ ChunkCoord → Int
        chunkElev (ChunkCoord cx cy) =
            let baseGX = cx * chunkSize
                baseGY = cy * chunkSize
                samples = [ sampleElev (baseGX + chunkSize `div` 2)
                                       (baseGY + chunkSize `div` 2)
                          , sampleElev baseGX baseGY
                          , sampleElev (baseGX + chunkSize - 1) baseGY
                          , sampleElev baseGX (baseGY + chunkSize - 1)
                          , sampleElev (baseGX + chunkSize - 1)
                                       (baseGY + chunkSize - 1)
                          ]
            in minimum samples

        oceanSeeds = concatMap (\plate →
            if plateIsLand plate
            then []
            else let cx = floorDiv' (plateCenterX plate) chunkSize
                     cy = floorDiv' (plateCenterY plate) chunkSize
                     coord = ChunkCoord cx cy
                 in if cx ≥ -halfSize ∧ cx < halfSize
                     ∧ cy ≥ -halfSize ∧ cy < halfSize
                     ∧ chunkElev coord ≤ seaLevel
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
                    ∧ chunkElev n ≤ seaLevel
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

    -- Two-pass ocean fill:
    --   Pass 1: place ocean where terrain ≤ sea level
    --   Pass 2: propagate ocean one tile outward to cover coastline
    --           gaps (terrain side faces at the water's edge)
    buildOceanSurface surf =
        withFluidMap $ \mv → do
            -- Pass 1: seed ocean at/below sea level
            forEachSurface surf $ \idx _lx _ly surfZ →
                when (surfZ ≤ seaLevel ∧ surfZ > minBound) $
                    MV.write mv idx (Just (FluidCell Ocean seaLevel))
            -- Pass 2: extend one tile into coast so the ocean
            -- surface covers the terrain side-face at the waterline
            forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
                val ← MV.read mv idx
                when (isNothing val) $ do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                    adj ← adjacentHasOcean mv lx ly
                    when adj $
                        MV.write mv idx (Just (FluidCell Ocean seaLevel))

    adjacentHasOcean ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                     → ST s Bool
    adjacentHasOcean mv lx ly = do
        let check x y
              | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = return False
              | otherwise = isJust ⊚ MV.read mv (y * chunkSize + x)
        n ← check lx (ly - 1)
        s ← check lx (ly + 1)
        e ← check (lx + 1) ly
        w ← check (lx - 1) ly
        return (n ∨ s ∨ e ∨ w)

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
