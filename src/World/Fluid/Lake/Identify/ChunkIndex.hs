{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-chunk indexing for the lake-identification pipeline: buckets
--   the finalized per-lake tile data into per-'ChunkCoord' bitmasks
--   ('buildChunkIndex') and derives the carve-delta vectors that pull
--   a clamped coastal basin's above-sea-level tiles down to sub-sea
--   ('buildLakeCarveIndex'). Called once from
--   'World.Fluid.Lake.Identify.identifyWorldLakes'. See that module's
--   header comment for the full pipeline overview.
module World.Fluid.Lake.Identify.ChunkIndex
    ( buildLakeCarveIndex
    , buildChunkIndex
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types (Lake(..), LakeChunkEntry(..))

-- | For each chunk holding tiles of a clamped lake (lkSurface ≤
--   seaLevel), build a per-tile carve delta vector that pulls each
--   above-sea-level lake tile down to @seaLevel − 1@. Without this,
--   a coastal basin with floor above sea level would clamp its
--   surface but render no water — composeFluidMap would see
--   @lkSurf < terrZ@ on every tile and skip them. The carve makes
--   the basin floor sub-sea so the clamped surface fills correctly.
--
--   Chunks with no clamped-lake tiles are absent from the output.
buildLakeCarveIndex
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ world terrain (pre-carve)
    → V.Vector Lake        -- ^ finalLakes (post-clamp surface values)
    → HM.HashMap ChunkCoord (V.Vector LakeChunkEntry)
    → HM.HashMap ChunkCoord (VU.Vector Int)
buildLakeCarveIndex worldTiles terrain finalLakes lakesByChunk =
    let half       = worldTiles `div` 2
        chunkArea  = chunkSize * chunkSize
        processChunk (ChunkCoord cx cy) entries = runST $ do
            v ← VUM.replicate chunkArea (0 ∷ Int)
            V.forM_ entries $ \entry → do
                let lid  = lceLakeId entry
                    bm   = lceBitmask entry
                    lake = finalLakes V.! lid
                when (lkSurface lake ≤ seaLevel) $
                    forM_ [0 .. chunkArea - 1] $ \li → when (bm VU.! li) $ do
                        let lx    = li `mod` chunkSize
                            ly    = li `div` chunkSize
                            gx    = cx * chunkSize + lx
                            gy    = cy * chunkSize + ly
                            gxOff = gx + half
                            gyOff = gy + half
                        when (gxOff ≥ 0 ∧ gxOff < worldTiles
                              ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $ do
                            let idx = gyOff * worldTiles + gxOff
                                t   = terrain VU.! idx
                                d   = max 0 (t - (seaLevel - 1))
                            cur ← VUM.read v li
                            when (d > cur) (VUM.write v li d)
            VU.unsafeFreeze v
        result = HM.mapWithKey processChunk lakesByChunk
        -- Drop chunks where every tile delta ended up at zero (a chunk
        -- with only non-clamped lakes).
    in HM.filter (\dv → VU.any (> 0) dv) result

-- | For every chunk that overlaps a kept lake, build a bitmask of
--   which chunk-local tiles belong to that lake. Chunks with no
--   lakes are omitted from the output map.
buildChunkIndex
    ∷ Int                -- ^ worldTiles
    → VU.Vector Int      -- ^ old label → new lake id (-1 = dropped)
    → VU.Vector Int      -- ^ labels
    → HM.HashMap ChunkCoord (V.Vector LakeChunkEntry)
buildChunkIndex worldTiles idMap labels =
    let half      = worldTiles `div` 2
        chunkArea = chunkSize * chunkSize
        nTiles    = worldTiles * worldTiles
        -- Pass 1: accumulate (chunk, lakeId) → [(lx, ly)] tile lists.
        accumKey ∷ HM.HashMap (ChunkCoord, Int) [(Int, Int)]
        accumKey = foldl' step HM.empty [0 .. nTiles - 1]
        step acc idx =
            let oldLbl = labels VU.! idx
            in if oldLbl < 0
               then acc
               else
                 let newId = idMap VU.! oldLbl
                 in if newId < 0
                    then acc
                    else
                      let gx = (idx `mod` worldTiles) - half
                          gy = (idx `div` worldTiles) - half
                          cx = gx `div` chunkSize
                          cy = gy `div` chunkSize
                          lx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                          ly = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                      in HM.insertWith (++) (ChunkCoord cx cy, newId)
                                       [(lx, ly)] acc
        -- Pass 2: freeze each tile list into a bitmask.
        perKey ∷ HM.HashMap (ChunkCoord, Int) LakeChunkEntry
        perKey = HM.mapWithKey
            (\(_, lid) tiles →
                let bm = VU.create $ do
                        v ← VUM.replicate chunkArea False
                        forM_ tiles $ \(lx, ly) →
                            VUM.write v (ly * chunkSize + lx) True
                        pure v
                in LakeChunkEntry { lceLakeId = lid, lceBitmask = bm })
            accumKey
        -- Pass 3: regroup by chunk, dropping the redundant lake-id key.
        byChunk = HM.foldlWithKey'
            (\m (cc, _) e → HM.insertWith (++) cc [e] m)
            HM.empty perKey
    in HM.map V.fromList byChunk
