{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Connected-component labelling and per-component aggregation for
--   the lake-identification pipeline: which basin tiles belong
--   together ('labelComponents'), and summarizing each component's
--   floor/surface/area/bbox into a 'Lake' ('buildLakes'), including
--   the coastal sea-level clamp. Called once from
--   'World.Fluid.Lake.Identify.identifyWorldLakes'. See that module's
--   header comment for the full pipeline overview.
module World.Fluid.Lake.Identify.Components
    ( labelComponents
    , buildLakes
    , LakeWithId(..)
    , dropOldId
    , clampFloorTolerance
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import qualified Data.HashSet as HS
import World.Fluid.Internal (wrapChunkCoordU)
import World.Ocean.Types (OceanMap)
import World.Fluid.Lake.Types (Lake(..))

-- | A coastal basin is sea-level-clamped only when its FLOOR is at
--   or below @seaLevel + clampFloorTolerance@. The coastal tests in
--   'World.Fluid.Lake.Identify.Ocean' are deliberately wide
--   (chunk-dilated geography) — without this gate they reach perched
--   mountain pockets inside "coastal" chunks and the clamp + carve
--   digs them into 1-tile wells with sea-level water at the bottom.
--   Tolerance 2 keeps beach pans a tile or two above sea rendering
--   flush with the adjacent ocean.
clampFloorTolerance ∷ Int
clampFloorTolerance = 2

-- | Lake record extended with its raw label id (only used during
--   construction; the public 'Lake' from 'World.Fluid.Lake.Types'
--   does not carry it). We tag-and-renumber so the final lake vector
--   can be densely indexed from 0.
data LakeWithId = LakeWithId
    { lkOldId ∷ !Int
    , lkLake  ∷ !Lake
    } deriving Show

dropOldId ∷ LakeWithId → Lake
dropOldId = lkLake

-- | BFS-label every basin tile (where @filled > terrain@ AND the tile
--   isn't world-edge-connected ocean). Non-basin tiles get @-1@.
--   Components are 4-cardinally connected via basin tiles only.
--
--   Sub-sea LANDLOCKED tiles (terrain ≤ seaLevel but NOT in
--   'worldOcean') count as basin tiles — these are inland-sea floors
--   that get water at their basin's rim. World-edge ocean tiles are
--   excluded so the chunk-gen ocean classifier owns them.
labelComponents
    ∷ VU.Vector Int → VU.Vector Int → VU.Vector Bool → Int
    → VU.Vector Int
labelComponents terrain filled worldOcean worldTiles = runST $ do
    let nTiles = worldTiles * worldTiles
        isBasin i =
            let e = terrain VU.! i
                f = filled  VU.! i
            in e ≠ minBound
             ∧ not (worldOcean VU.! i)
             ∧ f ≠ maxBound
                   -- Unreached by the flood: typically tiles enclosed
                   -- by a beyond-glacier ring. Treat as not-a-basin
                   -- so they don't poison their component's surface
                   -- with 'maxBound'.
             ∧ f > e
    labels  ← VUM.replicate nTiles (-1 ∷ Int)
    nextLbl ← newSTRef 0
    forM_ [0 .. nTiles - 1] $ \start → do
        cur ← VUM.read labels start
        when (cur < 0 ∧ isBasin start) $ do
            lbl ← readSTRef nextLbl
            writeSTRef nextLbl (lbl + 1)
            VUM.write labels start lbl
            queue ← newSTRef [start]
            let loop = do
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
                                        nlbl ← VUM.read labels nIdx
                                        when (nlbl < 0 ∧ isBasin nIdx) $ do
                                            VUM.write labels nIdx lbl
                                            modifySTRef' queue (nIdx :)
                            tryN (bx > 0)              (i - 1)
                            tryN (bx < worldTiles - 1) (i + 1)
                            tryN (by > 0)              (i - worldTiles)
                            tryN (by < worldTiles - 1) (i + worldTiles)
                            loop
            loop
    VU.freeze labels

-- | Scan the label map once, summarizing each component into a 'Lake'.
buildLakes
    ∷ Int                -- ^ number of labels
    → VU.Vector Int      -- ^ terrain
    → VU.Vector Int      -- ^ filled
    → VU.Vector Int      -- ^ labels (-1 = no basin)
    → Int                -- ^ worldTiles
    → VU.Vector Bool     -- ^ coastalMask
    → OceanMap           -- ^ chunk-level oceanic set
    → V.Vector LakeWithId
buildLakes nLabels terrain filled labels worldTiles coastalMask oceanMap = runST $ do
    let half = worldTiles `div` 2
        worldSize = worldTiles `div` chunkSize
    floorsM ← VUM.replicate nLabels (maxBound ∷ Int)
    surfsM  ← VUM.replicate nLabels (minBound ∷ Int)
    areasM  ← VUM.replicate nLabels (0       ∷ Int)
    bxminM  ← VUM.replicate nLabels (maxBound ∷ Int)
    byminM  ← VUM.replicate nLabels (maxBound ∷ Int)
    bxmaxM  ← VUM.replicate nLabels (minBound ∷ Int)
    bymaxM  ← VUM.replicate nLabels (minBound ∷ Int)
    coastalM ← VUM.replicate nLabels False
    VU.iforM_ labels $ \idx lbl → when (lbl ≥ 0) $ do
        let e  = terrain VU.! idx
            f  = filled  VU.! idx
            gx = (idx `mod` worldTiles) - half
            gy = (idx `div` worldTiles) - half
            cx = gx `div` chunkSize
            cy = gy `div` chunkSize
            -- Coastal via the dilated tile mask:
            inMask = coastalMask VU.! idx
            -- Coastal via the dilated chunk set. Wrap the chunk coord
            -- through the u-axis torus so the lookup matches the
            -- canonical key 'computeOceanMap' uses (chunks at the wrap
            -- edge live at their wrapped coord).
            chunkNearOcean =
                HS.member (wrapChunkCoordU worldSize (ChunkCoord cx cy))
                          oceanMap
        VUM.modify floorsM (min e) lbl
        VUM.modify surfsM  (max f) lbl
        VUM.modify areasM  (+ 1)   lbl
        VUM.modify bxminM  (min gx) lbl
        VUM.modify byminM  (min gy) lbl
        VUM.modify bxmaxM  (max gx) lbl
        VUM.modify bymaxM  (max gy) lbl
        when (inMask ∨ chunkNearOcean) (VUM.write coastalM lbl True)
    floors ← VU.freeze floorsM
    surfs  ← VU.freeze surfsM
    areas  ← VU.freeze areasM
    bxmins ← VU.freeze bxminM
    bymins ← VU.freeze byminM
    bxmaxs ← VU.freeze bxmaxM
    bymaxs ← VU.freeze bymaxM
    coastal ← VU.freeze coastalM
    pure $ V.generate nLabels $ \i →
        let rawSurf  = surfs  VU.! i
            rawFloor = floors VU.! i
            -- Coastal basin: clamp surface to sea level so it
            -- renders flush with the adjacent open ocean instead of
            -- as a perched inland lake stepped above it. For
            -- above-sea floors the chunk-gen pipeline carves the
            -- basin tiles down to sub-sea via 'wlCarveDelta' so the
            -- water plane still finds tiles to render at @seaLevel@.
            --
            -- The clamp is gated on the basin FLOOR sitting at or
            -- barely above sea level ('clampFloorTolerance'). The
            -- coastal test alone is chunk-dilated geography — it
            -- reaches 3 chunks inland and through dry sub-sea
            -- basins, so without the floor gate a 1-tile mountain
            -- pocket at z 35 inside a "coastal" chunk would clamp
            -- 36→0 and then carve 35→−1: the infamous 1-tile wells
            -- with 30z walls and a film of sea-level water at the
            -- bottom (TERRAIN_PIT in world_audit, fixed 2026-06-05).
            -- Beach pans a tile or two above sea still clamp and
            -- render flush — the intent of dropping the original
            -- floor restriction — but perched basins keep their rim
            -- surface and render as ordinary ponds.
            adjSurf
              | coastal VU.! i
              ∧ rawSurf > seaLevel
              ∧ rawFloor ≤ seaLevel + clampFloorTolerance = seaLevel
              | otherwise                                  = rawSurf
        in LakeWithId
            { lkOldId = i
            , lkLake  = Lake
                { lkSurface  = adjSurf
                , lkFloor    = rawFloor
                , lkArea     = areas  VU.! i
                , lkBBoxMinX = bxmins VU.! i
                , lkBBoxMinY = bymins VU.! i
                , lkBBoxMaxX = bxmaxs VU.! i
                , lkBBoxMaxY = bymaxs VU.! i
                }
            }
