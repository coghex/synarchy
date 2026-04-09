{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Ocean
    ( computeOceanMap
    , isOceanChunk
    , hasAnyOceanFluid
    , computeChunkFluid
    ) where

import UPrelude
import Data.List (sort)
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

-- * Ocean Flood Fill

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

        -- Use MEDIAN elevation across 5 sample points (center + corners).
        -- This requires a majority (3+) of the chunk to be at/below sea
        -- level for the BFS to propagate, preventing ocean from bleeding
        -- inland through single low-lying corners on flat continents.
        -- Coastal coverage is maintained by computeChunkFluid's
        -- hasOceanNeighbor + per-tile surfZ checks.
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
            [ ChunkCoord cx' cy'
            | (dx, dy) ← [(-1,0), (1,0), (0,-1), (0,1)]
            , let (cx', cy') = wrapChunkU (cx + dx, cy + dy)
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

-- * Chunk-Level Ocean Fluid

computeChunkFluid ∷ Int → OceanMap → ChunkCoord
                  → VU.Vector Int
                  → FluidMap
computeChunkFluid worldSize oceanMap coord surfaceMap =
    let nearOcean = isOceanChunk oceanMap coord
                  ∨ hasAnyOceanFluid worldSize oceanMap coord

        ChunkCoord cx cy = coord
        wrap = wrapChunkCoordU worldSize

        -- Which cardinal neighbor chunks are ocean?
        -- Used to extend ocean fluid across chunk boundaries.
        oceanN = isOceanChunk oceanMap (wrap (ChunkCoord cx (cy - 1)))
        oceanS = isOceanChunk oceanMap (wrap (ChunkCoord cx (cy + 1)))
        oceanE = isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) cy))
        oceanW = isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) cy))

        adjacentHasOcean ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                         → ST s Bool
        adjacentHasOcean mv lx ly = do
            let check x y
                  | x < 0         = return oceanW
                  | x ≥ chunkSize = return oceanE
                  | y < 0         = return oceanN
                  | y ≥ chunkSize = return oceanS
                  | otherwise     = isJust ⊚ MV.read mv (y * chunkSize + x)
            n ← check lx (ly - 1)
            s ← check lx (ly + 1)
            e ← check (lx + 1) ly
            w ← check (lx - 1) ly
            return (n ∨ s ∨ e ∨ w)

        -- Does the mutable fluid map have any ocean fluid set?
        -- Used after Pass 1 to gate Pass 2 on actual content.
        anyOceanInMap ∷ MV.MVector s (Maybe FluidCell) → ST s Bool
        anyOceanInMap mv = go 0
          where
            n = chunkSize * chunkSize
            go i
              | i ≥ n = return False
              | otherwise = do
                  v ← MV.read mv i
                  case v of
                      Just _  → return True
                      Nothing → go (i + 1)

    -- Pass 1 always runs: any below-seaLevel tile gets ocean fluid.
    -- This prevents chunk-boundary artifacts in the world view where
    -- the hasAnyOceanFluid gate would exclude coastal chunks just
    -- beyond its distance-2 range. The world view's eroded terrain
    -- has very few below-seaLevel tiles on continents, so this
    -- doesn't create noise pools.
    --
    -- The zoom map is unaffected: its pixel rendering uses tileColor
    -- (which checks the per-chunk chunkOcean flag), NOT the fluid map,
    -- for ocean rendering. Ocean fluid falls through to baseColor.
    --
    -- Pass 2 (boundary extension) extends water one tile into coast
    -- for side-face rendering. It runs whenever the chunk has any
    -- in-chunk ocean tiles (filled by Pass 1) OR has an ocean neighbor
    -- chunk. The chunk-level `nearOcean` gate based on the median
    -- elevation map misses chunks where Pass 1 produced ocean tiles
    -- but the median elevation is > seaLevel (e.g., a chunk that's
    -- 60% ocean but has a few high tiles raising the median). Without
    -- this fix, terr=1 tiles inside such chunks remain dry even when
    -- surrounded by ocean — visible as 1-tile islands in the audit.
    in withFluidMap $ \mv → do
        forEachSurface surfaceMap $ \idx _lx _ly surfZ →
            when (surfZ ≤ seaLevel ∧ surfZ > minBound) $
                MV.write mv idx (Just (FluidCell Ocean seaLevel))
        -- Determine if Pass 2 should run: any ocean fluid in this
        -- chunk (after Pass 1), or any cardinal neighbor chunk is
        -- ocean per the chunk-level map.
        hasInChunkOcean ← anyOceanInMap mv
        let runPass2 = hasInChunkOcean ∨ oceanN ∨ oceanS ∨ oceanE ∨ oceanW
        when runPass2 $
            forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
                val ← MV.read mv idx
                when (isNothing val) $ do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        surfZ = surfaceMap VU.! idx
                    when (surfZ ≤ seaLevel + 2 ∧ surfZ > minBound) $ do
                        adj ← adjacentHasOcean mv lx ly
                        when adj $
                            MV.write mv idx (Just (FluidCell Ocean seaLevel))

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
