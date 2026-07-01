{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Ocean
    ( computeOceanMap
    , isOceanChunk
    , hasAnyOceanFluid
    ) where

import UPrelude
import Data.List (sort)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Fluid.Internal

-- * Ocean Flood Fill

computeOceanMap ∷ Word64 → Int → Int → [TectonicPlate]
               → (Int → Int → (Int, MaterialId) → (Int, MaterialId))
               → (OceanMap, OceanDistMap)
computeOceanMap seed worldSize _plateCount plates applyTL =
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

        -- Use MEDIAN elevation across 5 sample points (center + corners).
        -- This requires a majority (3+) of the chunk to be at/below sea
        -- level for the BFS to propagate, preventing ocean from bleeding
        -- inland through single low-lying corners on flat continents.
        -- Coastal-fluid placement happens in `composeFluidMap` (water-
        -- table-driven), which reads `oceanDist` for its classification.
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
                sorted5 = sort samples
            in sorted5 !! 2  -- median

        -- For each ocean plate, seed the BFS from a below-sea-level
        -- chunk near its center. If the center chunk itself is above
        -- sea level (a volcanic island, say), spiral outward until we
        -- find one. Without this fallback, an ocean plate with an
        -- island center contributes no seed and its territory stays
        -- unflooded.
        seedRingRadius = 4 ∷ Int  -- up to 4 chunks (~64 tiles) out
        inBounds (ChunkCoord cx cy) =
              cx ≥ -halfSize ∧ cx < halfSize
            ∧ cy ≥ -halfSize ∧ cy < halfSize
        findOceanSeed cx0 cy0 =
            let ring r = [ ChunkCoord (cx0 + dx) (cy0 + dy)
                         | dx ← [-r .. r], dy ← [-r .. r]
                         , max (abs dx) (abs dy) ≡ r
                         ]
                candidates r
                  | r > seedRingRadius = []
                  | otherwise =
                      let here = filter (\c → inBounds c ∧ chunkElev c ≤ seaLevel)
                                        (ring r)
                      in case here of
                           (c:_) → [c]
                           []    → candidates (r + 1)
            in candidates 0
        -- Canonicalize seeds via the shared seam wrap. Seeds come from
        -- plate centers whose chunk coords can have u (= cx-cy) outside
        -- the canonical [-halfSize, halfSize) range — they're in the
        -- playable square but not on the canonical side of the wrap.
        -- Post-wrap entries from `neighbors` are always canonical, so
        -- without this step the visited set would mix canonical and
        -- non-canonical keys for the same physical chunk and
        -- canonical lookups (after #10) would miss seam-adjacent
        -- ocean (audit #11). Uses the same wrapChunkCoordU as the
        -- lookup side (hasAnyOceanFluid) so insert and lookup can't
        -- drift (issue #316).
        canonChunk = wrapChunkCoordU worldSize
        oceanSeeds = concatMap (\plate →
            if plateIsLand plate
            then []
            else let cx = floorDiv' (plateCenterX plate) chunkSize
                     cy = floorDiv' (plateCenterY plate) chunkSize
                 in map canonChunk (findOceanSeed cx cy)
            ) plates

        -- Wrap chunk coords in u-space (consistent with the isometric
        -- world) via the shared canonical wrap (issue #316).
        neighbors (ChunkCoord cx cy) =
            [ nb
            | (dx, dy) ← [(-1,0), (1,0), (0,-1), (0,1)]
            , let nb@(ChunkCoord _ cy') =
                    wrapChunkCoordU worldSize (ChunkCoord (cx + dx) (cy + dy))
            , cy' ≥ -halfSize ∧ cy' < halfSize
            ]

        -- BFS with distance tracking. Two phases:
        -- Phase 1: standard ocean BFS (only ocean chunks, seaLevel check)
        -- Phase 2: extend into land chunks for water table gradient
        maxLandDist = 30 ∷ Int

        -- Phase 1: ocean BFS (same as before)
        oceanBfs ∷ Seq ChunkCoord → HS.HashSet ChunkCoord → HS.HashSet ChunkCoord
        oceanBfs Empty visited = visited
        oceanBfs (current :<| queue) visited =
            let nextNbrs = filter (\n →
                    not (HS.member n visited)
                    ∧ chunkElev n ≤ seaLevel
                    ) (neighbors current)
                visited' = foldl' (flip HS.insert) visited nextNbrs
                queue' = foldl' (:|>) queue nextNbrs
            in oceanBfs queue' visited'

        initialVisited = HS.fromList oceanSeeds
        initialQueue = Seq.fromList oceanSeeds
        oceanSet = oceanBfs initialQueue initialVisited

        -- Phase 2: distance BFS from ocean boundary into land.
        -- Start from all ocean chunks at dist=0, propagate into ALL
        -- neighbors (including land) tracking distance.
        distBfs ∷ Seq (ChunkCoord, Int) → HM.HashMap ChunkCoord Int
                → HM.HashMap ChunkCoord Int
        distBfs Empty dm = dm
        distBfs ((current, dist) :<| queue) dm =
            if dist ≥ maxLandDist
            then distBfs queue dm
            else
                let nextNbrs = filter (\n → not (HM.member n dm))
                                      (neighbors current)
                    dm' = foldl' (\m n → HM.insert n (dist + 1) m) dm nextNbrs
                    queue' = foldl' (\q n → q :|> (n, dist + 1)) queue nextNbrs
                in distBfs queue' dm'

        distSeeds = [(c, 0) | c ← HS.toList oceanSet]
        distInitial = HM.fromList distSeeds
        distQueue = Seq.fromList distSeeds
        distMap = distBfs distQueue distInitial

    in (oceanSet, distMap)

isOceanChunk ∷ OceanMap → ChunkCoord → Bool
isOceanChunk = flip HS.member

-- | Quick boolean check: does this chunk have any ocean fluid?
--   Checks all chunks within Chebyshev distance 2 (a 5×5 grid).
--   With chunkBorder=10 and chunkSize=16, beach processing can
--   extend through a coastal chunk (not itself in the ocean map)
--   into the next chunk. The 2-hop radius ensures that chunk
--   still runs coastal erosion.
hasAnyOceanFluid ∷ Int → OceanMap → ChunkCoord → Bool
hasAnyOceanFluid worldSize oceanMap (ChunkCoord cx cy) =
    let wrap = wrapChunkCoordU worldSize
    in any (\(dx, dy) → isOceanChunk oceanMap (wrap (ChunkCoord (cx + dx) (cy + dy))))
           [ (dx, dy) | dx ← [-2 .. 2], dy ← [-2 .. 2] ]
