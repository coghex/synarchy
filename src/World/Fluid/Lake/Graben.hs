{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Graben (rift-lake) bed carving — issue #223.
--
--   Lakes are natural-basin fills with no bed model of their own:
--   the floor is whatever terrain the priority flood happened to
--   cover, so tectonically-rifted basins read identically to any
--   shallow pan. This pass gives lakes that sit on a divergent
--   plate boundary (per 'World.Plate.riftFieldMemo') a graben
--   cross-section: an untouched shore shelf, steep walls dropping
--   ~3 z per tile from two tiles off the shore, and a deep floor at
--   @lkSurface − riftDepth@ with mild value-noise relief so it reads
--   as a rift floor rather than a flat box.
--
--   The carve is expressed through the same per-chunk delta plumbing
--   the coastal clamp used ('wlCarveDelta', applied at chunk gen via
--   @max@ with the river deltas), and is strictly lower-only: where
--   the natural basin is already deeper than the graben target, the
--   natural floor wins. Lake SURFACES are untouched — the fill level
--   comes from the pre-carve priority flood, so deepening a bed only
--   deepens the water column.
--
--   Scope guards:
--     * only inland lakes ('lkSurface' > 'seaLevel') — sub-sea and
--       coastal-clamped basins belong to the seabed pass, which
--       supersedes lake carve deltas there;
--     * only lakes of at least 'grabenMinArea' tiles — a graben
--       needs room for shelf + wall + floor;
--     * only where the footprint-mean rift intensity reaches
--       'grabenRiftThreshold'.
module World.Fluid.Lake.Graben
    ( grabenCarveIndex
    , grabenRiftThreshold
    , grabenMinArea
    , grabenWallSlope
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    ( Lake(..), WorldLakes(..), LakeChunkEntry(..) )
import World.Plate (wrappedValueNoise2D)

-- | Footprint-mean rift intensity a lake needs before it grabens.
grabenRiftThreshold ∷ Float
grabenRiftThreshold = 0.35

-- | Minimum lake area (tiles). Below this the ring profile barely
--   reaches its wall, let alone a floor.
grabenMinArea ∷ Int
grabenMinArea = 24

-- | Wall steepness: z dropped per ring step once past the shore
--   shelf (rings 0/1 untouched, the drop starts at ring 2).
grabenWallSlope ∷ Int
grabenWallSlope = 3

-- | Depth of the graben floor below the lake surface, scaled by the
--   footprint-mean rift intensity.
grabenDepthFor ∷ Float → Int
grabenDepthFor riftMean =
    max 8 (min 24 (round (8.0 + 20.0 * riftMean ∷ Float)))

-- | Wavelength (tiles) of the floor relief noise.
grabenFloorNoiseScale ∷ Int
grabenFloorNoiseScale = 6

-- | Build the per-chunk graben carve deltas for a final lake table.
--   Returns the map Timeline attaches as 'wlCarveDelta' (the clamp
--   deltas the identifier itself produced are superseded by the
--   seabed pass and dropped before this runs).
grabenCarveIndex
    ∷ Word64               -- ^ world seed (floor relief noise)
    → Int                  -- ^ worldSize (chunks per side)
    → VU.Vector Int        -- ^ world terrain (pre-carve, worldTiles²)
    → (Int → Int → Float)  -- ^ rift-intensity field
    → WorldLakes           -- ^ final lake table
    → HM.HashMap ChunkCoord (VU.Vector Int)
grabenCarveIndex seed worldSize terrain riftAt lakes =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2
        chunkArea  = chunkSize * chunkSize
        nLakes     = V.length (wlLakes lakes)

        -- Per-tile lake id (−1 = not a lake tile), rebuilt from the
        -- per-chunk bitmasks so this pass needs no access to the
        -- identifier's internal label grid.
        lakeIdAt ∷ VU.Vector Int
        lakeIdAt = VU.create $ do
            v ← VUM.replicate nTiles (-1)
            forM_ (HM.toList (wlByChunk lakes)) $ \(ChunkCoord cx cy, entries) →
                V.forM_ entries $ \entry → do
                    let bm = lceBitmask entry
                    forM_ [0 .. chunkArea - 1] $ \li →
                        when (bm VU.! li) $ do
                            let lx = li `mod` chunkSize
                                ly = li `div` chunkSize
                                gxOff = cx * chunkSize + lx + half
                                gyOff = cy * chunkSize + ly + half
                            when (gxOff ≥ 0 ∧ gxOff < worldTiles
                                  ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $
                                VUM.write v (gyOff * worldTiles + gxOff)
                                            (lceLakeId entry)
            pure v

        -- Footprint-mean rift intensity per lake.
        riftMeanOf ∷ V.Vector Float
        riftMeanOf = runST $ do
            sums ← VUM.replicate nLakes (0.0 ∷ Float)
            cnts ← VUM.replicate nLakes (0 ∷ Int)
            forM_ [0 .. nTiles - 1] $ \i → do
                let lid = lakeIdAt VU.! i
                when (lid ≥ 0) $ do
                    let gx = (i `mod` worldTiles) - half
                        gy = (i `div` worldTiles) - half
                    s ← VUM.read sums lid
                    VUM.write sums lid (s + riftAt gx gy)
                    c ← VUM.read cnts lid
                    VUM.write cnts lid (c + 1)
            V.generateM nLakes $ \lid → do
                s ← VUM.read sums lid
                c ← VUM.read cnts lid
                pure (if c ≡ 0 then 0.0 else s / fromIntegral c)

        -- Which lakes graben, and how deep.
        depthOf ∷ V.Vector Int      -- ^ 0 = not a graben lake
        depthOf = V.generate nLakes $ \lid →
            let lake = wlLakes lakes V.! lid
                rm   = riftMeanOf V.! lid
            in if lkSurface lake > seaLevel
                  ∧ lkArea lake ≥ grabenMinArea
                  ∧ rm ≥ grabenRiftThreshold
               then grabenDepthFor rm
               else 0

        anyGraben = V.any (> 0) depthOf

        -- Ring distance from shore for every lake tile: tiles whose
        -- 4-neighbourhood leaves their own lake are ring 1; BFS
        -- inward from there. Grid edges count as shore (the lake
        -- identifier treats them as walls the same way).
        ringOf ∷ VU.Vector Int
        ringOf = VU.create $ do
            ring ← VUM.replicate nTiles (0 ∷ Int)
            let neighbours i =
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                    in [ i - 1          | bx > 0 ]
                     ⧺ [ i + 1          | bx < worldTiles - 1 ]
                     ⧺ [ i - worldTiles | by > 0 ]
                     ⧺ [ i + worldTiles | by < worldTiles - 1 ]
                isShoreEdge i =
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                        lid = lakeIdAt VU.! i
                    in bx ≡ 0 ∨ bx ≡ worldTiles - 1
                     ∨ by ≡ 0 ∨ by ≡ worldTiles - 1
                     ∨ any (\j → lakeIdAt VU.! j ≠ lid) (neighbours i)
            frontier0Ref ← newSTRef []
            forM_ [0 .. nTiles - 1] $ \i →
                when (lakeIdAt VU.! i ≥ 0 ∧ isShoreEdge i) $ do
                    VUM.write ring i 1
                    fs ← readSTRef frontier0Ref
                    writeSTRef frontier0Ref (i : fs)
            let expand [] _ = pure ()
                expand frontier d = do
                    nextRef ← newSTRef []
                    forM_ frontier $ \i → do
                        let lid = lakeIdAt VU.! i
                        forM_ (neighbours i) $ \j →
                            when (lakeIdAt VU.! j ≡ lid) $ do
                                rj ← VUM.read ring j
                                when (rj ≡ 0) $ do
                                    VUM.write ring j d
                                    ns ← readSTRef nextRef
                                    writeSTRef nextRef (j : ns)
                    next ← readSTRef nextRef
                    expand next (d + 1)
            frontier0 ← readSTRef frontier0Ref
            expand frontier0 2
            pure ring

        -- Per-tile carve delta.
        deltaAt i =
            let lid = lakeIdAt VU.! i
            in if lid < 0 then 0
               else
                 let gDepth = depthOf V.! lid
                     ring   = ringOf VU.! i
                 in if gDepth ≡ 0 ∨ ring < 2
                    then 0
                    else
                      let prof0 = min gDepth (grabenWallSlope * (ring - 1))
                          gx = (i `mod` worldTiles) - half
                          gy = (i `div` worldTiles) - half
                          -- Floor relief: only the flat floor gets
                          -- the ±2 z noise, walls stay monotone.
                          var | prof0 < gDepth = 0
                              | otherwise =
                                  round (wrappedValueNoise2D
                                            (seed + 20623) worldSize
                                            gx gy grabenFloorNoiseScale
                                         * 4.0 ∷ Float) - 2
                          prof = max grabenWallSlope (prof0 + var)
                          surf = lkSurface (wlLakes lakes V.! lid)
                          target = surf - prof
                      in max 0 (terrain VU.! i - target)

        -- Bucket non-zero deltas per chunk (same output shape as the
        -- identifier's clamp-carve index).
        step acc i =
            let d = deltaAt i
            in if d ≤ 0 then acc
               else
                 let gx = (i `mod` worldTiles) - half
                     gy = (i `div` worldTiles) - half
                     cx = gx `div` chunkSize
                     cy = gy `div` chunkSize
                     lx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                     ly = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                     li = ly * chunkSize + lx
                 in HM.insertWith (⧺) (ChunkCoord cx cy) [(li, d)] acc

        buckets = foldl' step HM.empty [0 .. nTiles - 1]

        toVec assocs = VU.create $ do
            v ← VUM.replicate chunkArea 0
            forM_ assocs $ \(li, d) → do
                cur ← VUM.read v li
                when (d > cur) (VUM.write v li d)
            pure v

    in if not anyGraben
       then HM.empty
       else HM.map toVec buckets
