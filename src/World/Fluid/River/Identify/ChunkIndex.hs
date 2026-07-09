{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-chunk indexing for the river-identification pipeline: buckets
--   the finalized per-tile river data (bitmask, surface z, width,
--   carve delta) into per-'ChunkCoord' vectors so chunk generation can
--   look up its slice in O(1). Called once from
--   'World.Fluid.River.Identify.traceRivers'. See that module's header
--   comment for the full pipeline overview.
module World.Fluid.River.Identify.ChunkIndex
    ( buildCarveDeltaIndex
    , buildRiverChunkIndex
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.River.Types (RiverChunkEntry(..))

-- | Bucket the per-tile carve delta into per-chunk vectors so chunk
--   gen can look up its slice in O(1). Only chunks that touch a river
--   appear in the output map.
buildCarveDeltaIndex
    ∷ Int                  -- ^ worldSize
    → Int                  -- ^ half
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ per-tile carve delta
    → HM.HashMap ChunkCoord (VU.Vector Int)
buildCarveDeltaIndex worldSize half isRiverTile carveDelta =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        chunkArea  = chunkSize * chunkSize
        -- Pass 1: bucket non-zero deltas by chunk.
        step acc i
            | not (isRiverTile VU.! i) = acc
            | otherwise =
                let d = carveDelta VU.! i
                in if d ≤ 0 then acc
                   else
                     let gx = (i `mod` worldTiles) - half
                         gy = (i `div` worldTiles) - half
                         cx = gx `div` chunkSize
                         cy = gy `div` chunkSize
                         lx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                         ly = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                         li = ly * chunkSize + lx
                     in HM.insertWith (++) (ChunkCoord cx cy)
                                      [(li, d)] acc
        accum ∷ HM.HashMap ChunkCoord [(Int, Int)]
        accum = foldl' step HM.empty [0 .. nTiles - 1]
        -- Pass 2: freeze each chunk's tile list into a chunkArea-long
        -- delta vector (zero for tiles without carving).
        freezeChunk tiles = VU.create $ do
            v ← VUM.replicate chunkArea (0 ∷ Int)
            forM_ tiles $ \(li, d) → VUM.write v li d
            pure v
    in HM.map freezeChunk accum

-- | Build the per-chunk index: one 'RiverChunkEntry' per (chunk, river)
--   pair that overlaps. Both the bitmask and the per-tile surface z are
--   chunkArea-long.
buildRiverChunkIndex
    ∷ Int                  -- ^ worldSize
    → Int                  -- ^ half
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ compId
    → VU.Vector Int        -- ^ surfZ
    → VU.Vector Int        -- ^ widthRadius
    → HM.HashMap ChunkCoord (V.Vector RiverChunkEntry)
buildRiverChunkIndex worldSize half isRiverTile compId surfZ widthRadius =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        chunkArea  = chunkSize * chunkSize
        -- Pass 1: bucket tiles into (chunk, riverId) → [(localIdx, surfZ, width)].
        step acc i
            | not (isRiverTile VU.! i) = acc
            | otherwise =
                let gx     = (i `mod` worldTiles) - half
                    gy     = (i `div` worldTiles) - half
                    cx     = gx `div` chunkSize
                    cy     = gy `div` chunkSize
                    lx     = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                    ly     = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                    li     = ly * chunkSize + lx
                    rid    = compId VU.! i
                    z      = surfZ VU.! i
                    w      = max 0 (widthRadius VU.! i)
                in HM.insertWith (++) (ChunkCoord cx cy, rid)
                                 [(li, z, w)] acc
        accumKey ∷ HM.HashMap (ChunkCoord, Int) [(Int, Int, Int)]
        accumKey = foldl' step HM.empty [0 .. nTiles - 1]
        -- Pass 2: per key, freeze into bitmask + per-tile surface + width.
        perKey ∷ HM.HashMap (ChunkCoord, Int) RiverChunkEntry
        perKey = HM.mapWithKey
            (\(_, rid) tiles →
                let (bm, surfs, widths) = runST $ do
                        b ← VUM.replicate chunkArea False
                        s ← VUM.replicate chunkArea minBound
                        w ← VUM.replicate chunkArea (0 ∷ Word8)
                        forM_ tiles $ \(li, z, wr) → do
                            VUM.write b li True
                            VUM.write s li z
                            VUM.write w li (fromIntegral wr)
                        bF ← VU.unsafeFreeze b
                        sF ← VU.unsafeFreeze s
                        wF ← VU.unsafeFreeze w
                        pure (bF, sF, wF)
                in RiverChunkEntry
                    { rceRiverId       = rid
                    , rceBitmask       = bm
                    , rcePerTileSurfZ  = surfs
                    , rceWidthRadius   = widths
                    })
            accumKey
        -- Pass 3: regroup by chunk.
        byChunk = HM.foldlWithKey'
            (\m (cc, _) e → HM.insertWith (++) cc [e] m)
            HM.empty perKey
    in HM.map V.fromList byChunk
