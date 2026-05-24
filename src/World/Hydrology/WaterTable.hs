{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-chunk water-table computation.
--
--   The water table is the elevation of the saturation horizon: below it
--   the ground is saturated (groundwater), above it dry. Rivers and lakes
--   are the tiles where the water table breaches the surface; subsurface
--   water (e.g. revealed by digging) is implied by `z ≤ wt[x, y]`.
--
--   See @src\/World\/Hydrology\/DESIGN.md@ for the full architectural
--   rationale. This module implements §5.4 of that document: a per-chunk
--   compute over a bordered region (chunkSize + 2 * chunkBorder), with
--   cross-chunk continuity achieved through the bordered region's
--   overlap with neighbor chunks.
--
--   Implements oceans, channel pinning (from `channelMask`), sink-fill,
--   ascending-terrain descent propagation, and a single-tile gap-fill
--   pass that connects rivers to adjacent lakes/oceans across flat
--   1-step terrain. The fluid composer in `World.Generate.Chunk` reads
--   the resulting wt and classifies tiles as river / lake / ocean / dry.
module World.Hydrology.WaterTable
    ( computeWaterTable
    , depthFromClimate
    , isSubsurfaceWet
    , waterTableAtTile
    ) where

import UPrelude
import Control.Monad.ST (runST)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Chunk.Types (LoadedChunk(..), chunkSize)
import World.Constants (seaLevel)
import World.Generate.Constants (chunkBorder)
import World.Plate (wrapGlobalU)
import World.Types (ChunkCoord(..))
import World.Weather.Lookup (LocalClimate(..), lookupLocalClimate)
import World.Weather.Types (ClimateState)

-- | Vertical distance below terrain that the water table sits at, in
--   undisturbed land. Climate-only — wet climates shallow, arid deep.
--
--   Material-drainage variation was attempted as Phase 2 (mpDrainage
--   pulling the wt deeper for sand/karst, shallower for clay/granite)
--   but produced widespread artifacts: in wet climates every
--   low-drainage tile collapsed to wt=terrain, flooding ~90% of the
--   map. Reverted to climate-only. `mpDrainage` is still on
--   `MaterialProps` and parsed from YAML, sitting idle for a future
--   more carefully calibrated attempt.
depthFromClimate ∷ LocalClimate → Int
depthFromClimate c =
    let p = lcPrecip c   -- 0..1
        t = lcTemp c     -- °C
        baseDepth
          | p > 0.40   = 2
          | p > 0.20   = 5
          | p > 0.10   = 15
          | otherwise = 40
        aridPenalty
          | t > 25.0 ∧ p < 0.15 = 40
          | t > 20.0 ∧ p < 0.25 = 15
          | otherwise           = 0
    in baseDepth + aridPenalty

-- | Compute the water-table z-value for each tile in a chunk's interior.
--
--   Inputs:
--     * @climate@:   global climate field, looked up per tile
--     * @worldSize@: needed for the global-coord lookup
--     * @coord@:     the chunk this compute is for
--     * @borderedTerrain@: terrain elevation over the bordered region
--                          (@borderSize × borderSize@ entries, indexed
--                          @by * borderSize + bx@). This is the same
--                          bordered terrain @generateChunk@ uses for
--                          erosion + coastal passes.
--
--   Output: a @VU.Vector Int@ of length @chunkSize * chunkSize@, indexed
--   @ly * chunkSize + lx@, giving the water-table z for each interior tile.
--
--   Algorithm (§5.4 of DESIGN.md):
--     1. Initial:     wt[t] = terrain[t] - depthFromClimate(climate[t])
--     2. Pin oceans:  wt[t] = seaLevel  when terrain[t] ≤ seaLevel
--        Pin channels: wt[t] = terrain[t] when channelMask[t]
--        Pin sinks:    wt[t] = spillway elevation when t has no
--                      strictly-lower neighbor but at least one higher
--                      one. The spillway = the lowest cardinal neighbor.
--                      This floods true depressions up to the rim and
--                      then the sweep step lifts the rest of the basin
--                      via inheritance.
--     3. Ascending-terrain sweep: for each tile in order of lowest
--        terrain first, lift wt[t] to at least its downstream neighbor's
--        wt. Downstream = steepest-descent cardinal neighbor.
--     4. No clip — wt may exceed terrain in basins; those tiles are
--        surface water (classification handled by the fluid composer).
computeWaterTable
    ∷ ClimateState
    → Int
    → ChunkCoord
    → VU.Vector Int    -- ^ bordered terrain
    → VU.Vector Int    -- ^ bordered channel-floor map (river tiles get
                       --   the channel-floor elevation, others get
                       --   @maxBound@; same value across cross-sections
                       --   yields flat river surfaces)
    → VU.Vector Int
computeWaterTable climate worldSize coord borderedTerrain channelMask =
    let bSize = chunkSize + 2 * chunkBorder
        area  = bSize * bSize
        ChunkCoord cx cy = coord

        -- Bordered-index → global (gx, gy) for climate lookups. The
        -- u-axis wraps; v doesn't. We wrap here so the bordered region
        -- straddling the cylindrical seam reads the right climate cells.
        bToGlobal idx =
            let bx = idx `mod` bSize
                by = idx `div` bSize
                lx = bx - chunkBorder
                ly = by - chunkBorder
                gx = cx * chunkSize + lx
                gy = cy * chunkSize + ly
            in wrapGlobalU worldSize gx gy

        -- Step 1 + 2 fused: produce the initial wt vector with ocean
        -- and channel tiles already pinned.
        --
        -- Priority: ocean > channel > climate default.
        -- Ocean tiles get seaLevel regardless of being in a channel
        -- (matters at river mouths).
        -- Channel tiles get pinned to their terrain (= channel floor =
        -- surface water of an actively flowing river); this then
        -- propagates upstream during the sweep so lakes naturally form
        -- where drainage paths cross terrain depressions.
        -- Cap on how far the water surface may sit above local terrain
        -- at any pinned tile. Without it, two failure modes produce
        -- isolated water columns:
        --   * Sink-fill: a 1-tile-wide deep pit on a plateau spillways
        --     up to its rim height (e.g. terr=70, rim=78 ⇒ 8-tile column).
        --   * Channel mask: natural terrain inside the mask zone dips
        --     below the segment's channelFloor (terr=34, cf=38), pinning
        --     wt 4 above terrain.
        -- Capping wt to t+columnCap keeps the tile classified as wet (the
        -- sweep can still propagate the lake/river surface across the
        -- basin via cardinal neighbors), but limits the visible water
        -- stack on any one tile to a natural-looking ~2 tiles.
        columnCap = 2 ∷ Int

        wt0 = VU.generate area $ \idx →
            let t = borderedTerrain VU.! idx
                cf = channelMask VU.! idx
            in if t ≤ seaLevel
               then seaLevel
               else if cf ≠ maxBound
               then min cf (t + columnCap)
                         -- channel tile: pin to segment's interpolated
                         -- channel-floor, capped to terrain+columnCap so
                         -- natural dips below cf show as shallow water
                         -- rather than tall columns.
               else
                 let spill = spillwayElev borderedTerrain bSize idx t
                 in if spill > t + columnCap
                    then min spill (t + columnCap)
                                 -- real sink (≥3-tile rim): flood to
                                 -- spillway, capped to columnCap so
                                 -- narrow deep pits don't render as
                                 -- water towers. Wider basins fill up
                                 -- via the sweep phase, which lifts
                                 -- neighbors to match — that respects
                                 -- the cap implicitly because each
                                 -- pinned tile only contributes its
                                 -- own capped value.
                    else
                      let (gx, gy) = bToGlobal idx
                          c = lookupLocalClimate climate worldSize gx gy
                      in t - depthFromClimate c

        -- Step 3: ascending-terrain sweep. Sort tile indices by terrain
        -- elevation, lowest first; for each, lift wt to its downstream
        -- neighbor's wt if that's higher. We sort once and walk linearly.
        --
        -- Downstream is the steepest-descent cardinal neighbor (lowest
        -- terrain among N/S/E/W, must be strictly lower than self).
        -- Sinks (no lower neighbor) propagate nothing — their wt is
        -- whatever step 1/2 produced.
        ordered =
            sortBy (comparing snd)
                [ (idx, borderedTerrain VU.! idx) | idx ← [0 .. area - 1] ]

        wtFinal = runST $ do
            wtM ← VU.thaw wt0
            -- Phase 1: ascending-terrain sweep with STRICT-downstream
            -- propagation. Lifts wt from descent paths.
            forM_ ordered $ \pair → do
                let idx = fst pair
                    myT = snd pair
                    dIdx = steepestDescent borderedTerrain bSize idx myT
                if dIdx < 0
                   then pure ()
                   else do
                       downWT ← VUM.read wtM dIdx
                       myWT ← VUM.read wtM idx
                       when (downWT > myWT) $ VUM.write wtM idx downWT
            -- Phase 2: single-tile gap fill. Connects dry tiles to
            -- adjacent surface water — but ONLY from sources that were
            -- already wet after Phase 1 (sink-filled lakes, channel-
            -- pinned rivers, ocean propagation). This avoids chain
            -- propagation across plateaus: a dry tile that gets lifted
            -- to wet here CAN'T then donate to its own neighbors,
            -- because we read donor wts from a snapshot taken before
            -- the pass.
            --
            -- Fixes the "1-tile gap between river and lake" case
            -- (gap tile inherits donor's wt; both ends connect).
            -- Doesn't help multi-tile gaps — those need river-tracing
            -- changes (separately tracked).
            wtSnapshot ← VU.freeze wtM
            forM_ [0 .. area - 1] $ \idx → do
                let myT = borderedTerrain VU.! idx
                    myWTinit = wtSnapshot VU.! idx
                when (myWTinit < myT) $ do
                    -- A neighbor donates iff BOTH:
                    --   1. Its terrain is close to ours (|Δ| ≤ 2) —
                    --      this restricts donation to truly flat
                    --      connectors. Without this, any low-lying
                    --      tile near a river gets flooded to the
                    --      river's water surface.
                    --   2. Its water surface is above our terrain —
                    --      i.e., water from it would physically flood
                    --      our tile.
                    -- Snapshot read prevents chain-propagation across
                    -- plateaus.
                    let bx = idx `mod` bSize
                        by = idx `div` bSize
                        donorIfFloods nIdx ok
                          | not ok = minBound
                          | abs (borderedTerrain VU.! nIdx - myT) > 2 = minBound
                          | wtSnapshot VU.! nIdx ≤ myT = minBound
                          | otherwise = wtSnapshot VU.! nIdx
                        n = donorIfFloods (idx - bSize) (by > 0)
                        s = donorIfFloods (idx + bSize) (by < bSize - 1)
                        w = donorIfFloods (idx - 1)     (bx > 0)
                        e = donorIfFloods (idx + 1)     (bx < bSize - 1)
                        rawDonor = max (max n s) (max w e)
                        -- Same column cap as the pinning step. Without
                        -- this, a flat-terrain connector adjacent to a
                        -- capped sink can still inherit a wt above its
                        -- own terrain+columnCap when the donor's terrain
                        -- happened to be 2 higher than ours.
                        donor = min rawDonor (myT + columnCap)
                    when (donor > myWTinit) $ VUM.write wtM idx donor
            VU.unsafeFreeze wtM

        -- Extract the chunkSize × chunkSize interior from the bordered
        -- result, dropping the chunkBorder ring on every side.
        interior = VU.generate (chunkSize * chunkSize) $ \i →
            let lx = i `mod` chunkSize
                ly = i `div` chunkSize
                bidx = (ly + chunkBorder) * bSize + (lx + chunkBorder)
            in wtFinal VU.! bidx

    in interior

-- | Spillway elevation: the lowest cardinal neighbor's terrain, for use
--   when a tile has no strictly-lower neighbor (i.e., is a sink). If
--   the lowest neighbor's terrain is still ≤ self.terrain, this tile
--   isn't a true depression (it's a flat plateau or a saddle); we
--   return self.terrain so the caller's @spill > t@ guard rejects it.
--
--   Top-level for the same reason as `steepestDescent` —
--   GHC 9.12 + Strict panic-dodge.
spillwayElev ∷ VU.Vector Int → Int → Int → Int → Int
spillwayElev terrain bSize idx myT =
    let bx = idx `mod` bSize
        by = idx `div` bSize
        nIdx = idx - bSize
        sIdx = idx + bSize
        wIdx = idx - 1
        eIdx = idx + 1
        nT = if by > 0          then terrain VU.! nIdx else myT
        sT = if by < bSize - 1  then terrain VU.! sIdx else myT
        wT = if bx > 0          then terrain VU.! wIdx else myT
        eT = if bx < bSize - 1  then terrain VU.! eIdx else myT
        m1 = min nT sT
        m2 = min wT eT
    in min m1 m2

-- | Steepest-descent cardinal neighbor of tile @idx@ over a bordered
--   region of side @bSize@. Returns @-1@ if no neighbor is strictly
--   lower than the tile's own terrain (i.e., the tile is a local sink).
--
--   Top-level rather than @let@-bound inside the sweep so the inner
--   loop is a plain @Int@-returning call — avoids a GHC 9.12 panic
--   with @Strict@ + nested @do@ + @Maybe@-returning binders.
steepestDescent ∷ VU.Vector Int → Int → Int → Int → Int
steepestDescent terrain bSize idx myT =
    let bx = idx `mod` bSize
        by = idx `div` bSize
        -- Candidate cardinal neighbors with their in-bounds flags.
        nIdx  = idx - bSize
        sIdx  = idx + bSize
        wIdx  = idx - 1
        eIdx  = idx + 1
        nOk   = by > 0
        sOk   = by < bSize - 1
        wOk   = bx > 0
        eOk   = bx < bSize - 1
        -- Sentinel terrain for invalid / not-lower neighbors. We
        -- pick anything ≥ myT so they never win the min comparison.
        sentinel = myT
        nT = if nOk then terrain VU.! nIdx else sentinel
        sT = if sOk then terrain VU.! sIdx else sentinel
        wT = if wOk then terrain VU.! wIdx else sentinel
        eT = if eOk then terrain VU.! eIdx else sentinel
        -- Find min among the candidates; if it's still ≥ myT, sink.
        bestIdx0 = if nT < myT then nIdx else (-1)
        bestT0   = if nT < myT then nT  else sentinel
        (bestIdx1, bestT1) =
            if sT < bestT0 then (sIdx, sT) else (bestIdx0, bestT0)
        (bestIdx2, bestT2) =
            if wT < bestT1 then (wIdx, wT) else (bestIdx1, bestT1)
        (bestIdx3, _) =
            if eT < bestT2 then (eIdx, eT) else (bestIdx2, bestT2)
    in bestIdx3

-- * Subsurface query

-- | Read the water-table elevation at a chunk-local (lx, ly) tile.
--   The water table is the z-value below which the column is saturated
--   with groundwater. @z ≤ waterTableAtTile lc lx ly@ means a buried
--   tile at z is wet; revealing it (by digging) should expose water.
waterTableAtTile ∷ LoadedChunk → Int → Int → Int
waterTableAtTile lc lx ly = lcWaterTableMap lc VU.! (ly * chunkSize + lx)

-- | Is the buried position @(lx, ly, z)@ wet? Below the water table,
--   the ground is saturated; removing a tile here should expose water.
--
--   This is the core API the design doc's Phase D promised — any
--   subsurface system (digging, cave generation, units flooding in
--   tunnels) can call this to ask "is this position underground but wet?"
isSubsurfaceWet ∷ LoadedChunk → Int → Int → Int → Bool
isSubsurfaceWet lc lx ly z = z ≤ waterTableAtTile lc lx ly
