{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The priority-flood stage of the lake-identification pipeline:
--   a bucket-queue variant of the classic priority-flood watershed
--   algorithm, seeded from world-edge-connected open ocean and the
--   world boundary. Called once from
--   'World.Fluid.Lake.Identify.identifyWorldLakes'. See that module's
--   header comment for the full pipeline overview.
module World.Fluid.Lake.Identify.Flood
    ( priorityFlood
    ) where

import UPrelude
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

-- | Bucket-queue priority flood over the world-tile terrain.
--   Output @filled[i]@ is the lowest elevation a drop of water at @i@
--   would have to rise to in order to escape to a seeded boundary
--   (world-edge-connected ocean or world edge). Closed depressions
--   get the rim elevation; slopes and peaks get @terrain[i]@.
--
--   Seeds: tiles flagged in 'worldOcean' (world-edge-connected open
--   ocean) plus tiles adjacent to a beyond-glacier sentinel (the
--   world boundary). Landlocked sub-sea tiles are NOT seeded — they
--   instead end up inside a basin component whose surface is the
--   real escape rim.
priorityFlood
    ∷ VU.Vector Int → VU.Vector Bool → Int → VU.Vector Int
priorityFlood terrain worldOcean worldTiles = runST $ do
    let nTiles     = worldTiles * worldTiles
        bucketBase = -10000 ∷ Int
        nBuckets   =  20000 ∷ Int
        toBucket z = max 0 (min (nBuckets - 1) (z - bucketBase))
    zM      ← VUM.replicate nTiles (maxBound ∷ Int)
    procM   ← VUM.replicate nTiles False
    buckets ← MV.replicate nBuckets ([] ∷ [Int])
    cursor  ← newSTRef 0
    let push idx z = do
            VUM.write zM idx z
            VUM.write procM idx True
            let b = toBucket z
            xs ← MV.read buckets b
            MV.write buckets b (idx : xs)
        seedTile idx = do
            let t = terrain VU.! idx
            push idx t
        -- A tile is a "boundary drain" if it has any adjacent
        -- beyond-glacier (= minBound) neighbor. Cheap edge check.
        adjacentToVoid idx =
            let bx = idx `mod` worldTiles
                by = idx `div` worldTiles
                check ok j =
                    ok ∧ terrain VU.! j ≡ minBound
            in check (bx > 0)              (idx - 1)
             ∨ check (bx < worldTiles - 1) (idx + 1)
             ∨ check (by > 0)              (idx - worldTiles)
             ∨ check (by < worldTiles - 1) (idx + worldTiles)
    -- Seed pass.
    forM_ [0 .. nTiles - 1] $ \idx → do
        let t = terrain VU.! idx
        when (t ≠ minBound) $ do
            when (worldOcean VU.! idx ∨ adjacentToVoid idx)
                 (seedTile idx)
    let tryPropagate srcZ nIdx = do
            let nT = terrain VU.! nIdx
            when (nT ≠ minBound) $ do
                done ← VUM.read procM nIdx
                when (not done) $ do
                    let nZ = max nT srcZ
                    push nIdx nZ
        drain = do
            i ← readSTRef cursor
            when (i < nBuckets) $ do
                xs ← MV.read buckets i
                case xs of
                    []           → writeSTRef cursor (i + 1) >> drain
                    (idx : rest) → do
                        MV.write buckets i rest
                        srcZ ← VUM.read zM idx
                        let bx = idx `mod` worldTiles
                            by = idx `div` worldTiles
                        when (bx > 0)              (tryPropagate srcZ (idx - 1))
                        when (bx < worldTiles - 1) (tryPropagate srcZ (idx + 1))
                        when (by > 0)              (tryPropagate srcZ (idx - worldTiles))
                        when (by < worldTiles - 1) (tryPropagate srcZ (idx + worldTiles))
                        drain
    drain
    VU.freeze zM
