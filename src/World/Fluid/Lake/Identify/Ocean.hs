{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Ocean/coastal proximity helpers for the lake-identification
--   pipeline: the world-edge ocean BFS that seeds the priority flood
--   and excludes open ocean from basin labelling
--   ('computeWorldEdgeOcean'), the wider "renders as ocean anywhere"
--   flood consumed by 'World.Generate.Chunk.Fluid'
--   ('computeRenderedOcean'), and the coastal-basin detection used by
--   'World.Fluid.Lake.Identify.Components.buildLakes' to decide which
--   basins get their surface clamped to sea level
--   ('computeRenderedOceanSeed', 'nearOceanMask', 'dilateChunkSet').
--   Called once from 'World.Fluid.Lake.Identify.identifyWorldLakes'.
--   See that module's header comment for the full pipeline overview.
module World.Fluid.Lake.Identify.Ocean
    ( computeWorldEdgeOcean
    , computeRenderedOcean
    , computeRenderedOceanSeed
    , nearOceanMask
    , dilateChunkSet
    , coastalProximity
    , coastalChunkRadius
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import qualified Data.HashSet as HS
import World.Fluid.Internal (wrapChunkCoordU)
import World.Ocean.Types (OceanMap)

-- | Tile-distance from open ocean within which a sub-sea basin gets
--   its surface clamped to 'World.Constants.seaLevel'. Catches a
--   coastal basin that would otherwise render as a lake perched above
--   the adjacent sea when only a thin land bar separates the two.
--   Tuned wide enough to cross the chunk-level oceanic region that
--   surrounds the tile-level world-edge ocean BFS — many "rendered as
--   ocean" tiles aren't in the world-edge BFS because the BFS only
--   walks sub-sea tiles, so a wider dilation lets the coastal-lake
--   detection find basins separated from BFS-ocean by several land
--   tiles.
coastalProximity ∷ Int
coastalProximity = 25

-- | Chunk-level proximity used as a fallback when the tile dilation
--   can't bridge the land between a lake and ocean. A lake whose
--   chunk is within this many cardinal hops of an oceanic chunk is
--   treated as coastal.
coastalChunkRadius ∷ Int
coastalChunkRadius = 3

-- | Tile-resolution BFS: which tiles are open ocean reachable from
--   the world boundary? Seeds: tiles on the world-boundary ring
--   (outermost row/col of the grid) that are at or below sea level.
--   Propagates 4-cardinally to neighbors that are also at or below
--   sea level. Beyond-glacier sentinel cells (@minBound@) are walls.
--
--   The output is consumed by 'World.Fluid.Lake.Identify.Flood.priorityFlood'
--   (as drain seeds) and
--   'World.Fluid.Lake.Identify.Components.labelComponents' (to
--   exclude open ocean from basin labelling). Landlocked sub-sea-level
--   tiles are NOT in the result, so they join an inland basin
--   component and get a proper rim-elevation surface instead of the
--   ocean's sea level.
computeWorldEdgeOcean ∷ VU.Vector Int → Int → VU.Vector Bool
computeWorldEdgeOcean terrain worldTiles = runST $ do
    let nTiles = worldTiles * worldTiles
        isSubSea i =
            let t = terrain VU.! i
            in t ≠ minBound ∧ t ≤ seaLevel
    flag  ← VUM.replicate nTiles False
    queue ← newSTRef ([] ∷ [Int])
    -- Seed: boundary-ring tiles that are sub-sea.
    forM_ [0 .. nTiles - 1] $ \idx → do
        let bx = idx `mod` worldTiles
            by = idx `div` worldTiles
            onEdge = bx ≡ 0 ∨ bx ≡ worldTiles - 1
                   ∨ by ≡ 0 ∨ by ≡ worldTiles - 1
        when (onEdge ∧ isSubSea idx) $ do
            VUM.write flag idx True
            modifySTRef' queue (idx :)
    let bfs = do
            q ← readSTRef queue
            case q of
                []       → pure ()
                (i : rs) → do
                    writeSTRef queue rs
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                        tryN ok nIdx
                            | not ok = pure ()
                            | otherwise = do
                                f ← VUM.read flag nIdx
                                when (not f ∧ isSubSea nIdx) $ do
                                    VUM.write flag nIdx True
                                    modifySTRef' queue (nIdx :)
                    tryN (bx > 0)              (i - 1)
                    tryN (bx < worldTiles - 1) (i + 1)
                    tryN (by > 0)              (i - worldTiles)
                    tryN (by < worldTiles - 1) (i + worldTiles)
                    bfs
    bfs
    VU.freeze flag

-- | Tile-resolution "rendered ocean": every sub-sea tile in a
--   connected sub-sea component that renders as ocean ANYWHERE. Floods
--   (4-connected, through sub-sea) from two seed sets — world-edge
--   sub-sea tiles AND sub-sea tiles in oceanic chunks (oceanMap) — so
--   it fills not just the edge-connected open ocean but also enclosed
--   inland seas whose core the coarse chunk-flood rendered ocean.
--   'composeFluidMap' ORs this in, so a whole chunk the chunk-flood
--   couldn't reach no longer renders dry inside a sea. A truly-dry
--   inland basin (no oceanic chunk in its component) is not flooded
--   and stays a lake / dry, as before.
computeRenderedOcean ∷ Int → OceanMap → VU.Vector Int → VU.Vector Bool
computeRenderedOcean worldSize oceanMap terrain = runST $ do
    let worldTiles = worldSize * chunkSize
        nTiles = worldTiles * worldTiles
        half   = worldTiles `div` 2
        isSubSea i = let t = terrain VU.! i in t ≠ minBound ∧ t ≤ seaLevel
        chunkOf i =
            let gx = (i `mod` worldTiles) - half
                gy = (i `div` worldTiles) - half
            in wrapChunkCoordU worldSize
                   (ChunkCoord (gx `div` chunkSize) (gy `div` chunkSize))
    flag  ← VUM.replicate nTiles False
    queue ← newSTRef ([] ∷ [Int])
    forM_ [0 .. nTiles - 1] $ \idx → when (isSubSea idx) $ do
        let bx = idx `mod` worldTiles
            by = idx `div` worldTiles
            onEdge = bx ≡ 0 ∨ bx ≡ worldTiles - 1
                   ∨ by ≡ 0 ∨ by ≡ worldTiles - 1
        when (onEdge ∨ HS.member (chunkOf idx) oceanMap) $ do
            VUM.write flag idx True
            modifySTRef' queue (idx :)
    let bfs = do
            q ← readSTRef queue
            case q of
                []       → pure ()
                (i : rs) → do
                    writeSTRef queue rs
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                        tryN ok nIdx
                            | not ok = pure ()
                            | otherwise = do
                                f ← VUM.read flag nIdx
                                when (not f ∧ isSubSea nIdx) $ do
                                    VUM.write flag nIdx True
                                    modifySTRef' queue (nIdx :)
                    tryN (bx > 0)              (i - 1)
                    tryN (bx < worldTiles - 1) (i + 1)
                    tryN (by > 0)              (i - worldTiles)
                    tryN (by < worldTiles - 1) (i + worldTiles)
                    bfs
    bfs
    VU.freeze flag

-- | Combine the world-edge ocean BFS with the chunk-level oceanic
--   classifier. A tile is included iff it's in either source — so a
--   sub-sea tile that's BFS-isolated but lives in an oceanic chunk
--   (where chunk-gen will render it as Ocean) still counts as ocean
--   for the coastal-lake detection. Catches "renders as ocean" tiles
--   that the pure tile-level BFS misses.
computeRenderedOceanSeed
    ∷ Int                  -- ^ worldSize
    → OceanMap
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ worldOcean (BFS result)
    → VU.Vector Bool
computeRenderedOceanSeed worldSize oceanMap terrain worldOcean =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
    in VU.generate nTiles $ \i →
        if worldOcean VU.! i
        then True
        else
          let t = terrain VU.! i
          in if t ≡ minBound ∨ t > seaLevel
             then False
             else
               let gx = (i `mod` worldTiles) - half
                   gy = (i `div` worldTiles) - half
                   cx = gx `div` chunkSize
                   cy = gy `div` chunkSize
                   worldSize' = worldTiles `div` chunkSize
               in HS.member (wrapChunkCoordU worldSize' (ChunkCoord cx cy))
                            oceanMap

-- | Dilate the world-edge ocean mask outward by @maxDist@ tiles
--   (4-cardinal). Returns a mask of every land tile that's within
--   @maxDist@ steps of some open-ocean tile. Used by
--   'World.Fluid.Lake.Identify.Components.buildLakes' to detect
--   coastal basins whose surface should be clamped to 'seaLevel'.
nearOceanMask
    ∷ Int                  -- ^ maxDist
    → Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ worldOcean
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool
nearOceanMask maxDist worldTiles worldOcean terrain = runST $ do
    let nTiles = worldTiles * worldTiles
    near ← VUM.replicate nTiles False
    let seeds = [ i | i ← [0 .. nTiles - 1], worldOcean VU.! i ]
    forM_ seeds (\i → VUM.write near i True)
    frontierRef ← newSTRef seeds
    let expand 0 = pure ()
        expand d = do
            frontier ← readSTRef frontierRef
            nextRef  ← newSTRef ([] ∷ [Int])
            forM_ frontier $ \i → do
                let bx = i `mod` worldTiles
                    by = i `div` worldTiles
                    -- E/W wrap as an x-torus so coastlines at the
                    -- world edge still get dilated into. Without
                    -- this, lakes at the rightmost / leftmost columns
                    -- may not register as coastal when the wrapped
                    -- neighbour is ocean.
                    east = if bx < worldTiles - 1
                           then i + 1
                           else i + 1 - worldTiles
                    west = if bx > 0
                           then i - 1
                           else i - 1 + worldTiles
                    tryN ok nIdx = when ok $ do
                        let nT = terrain VU.! nIdx
                        cur ← VUM.read near nIdx
                        when (nT ≠ minBound ∧ not cur) $ do
                            VUM.write near nIdx True
                            modifySTRef' nextRef (nIdx :)
                tryN True                  west
                tryN True                  east
                tryN (by > 0)              (i - worldTiles)
                tryN (by < worldTiles - 1) (i + worldTiles)
            next ← readSTRef nextRef
            writeSTRef frontierRef next
            expand (d - 1)
    expand maxDist
    VU.unsafeFreeze near

-- | Dilate a set of chunks outward by @r@ cardinal hops. Used to
--   build a "near-oceanic" chunk set as a fallback for the tile-level
--   coastal dilation.
dilateChunkSet ∷ Int → HS.HashSet ChunkCoord → HS.HashSet ChunkCoord
dilateChunkSet 0 s = s
dilateChunkSet r s =
    let s' = HS.foldl' (\acc (ChunkCoord cx cy) →
                acc `HS.union` HS.fromList
                    [ ChunkCoord (cx - 1) cy
                    , ChunkCoord (cx + 1) cy
                    , ChunkCoord cx (cy - 1)
                    , ChunkCoord cx (cy + 1)
                    ])
                s s
    in dilateChunkSet (r - 1) s'
