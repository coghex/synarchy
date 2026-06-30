{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Timeline
    ( applyTimelineChunk
    , applyTimelineFast
    , removeElevationSpikes
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), matGlacier, MaterialRegistry(..)
                      , getMaterialProps, MaterialProps(..))
import World.Plate (wrapGlobalU, TectonicPlate, elevationAtGlobal)
import World.Geology.Types (GeoModification(..))
import World.Geology.Event (applyGeoEvent)
import World.Geology.Timeline.Types (isRiverCarveEvent)
import World.Geology.Erosion
    (applyErosion, applyErosionLerp4, lookupRegionalErosion
    , erosionCornerLookup)
import World.Weather.Lookup (RegionGridCoords(..), regionGridCoords)
import World.Scale (WorldScale(..), computeWorldScale)
import World.Constants (seaLevel)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline.Fast (applyTimelineFast)
import Control.Monad (forM_, when)

-- * Spike Removal
--
-- Single-tile elevation outliers can survive elevation generation
-- when geological features (mountain ridges from convergent plate
-- boundaries) align with the u-v isometric axes — they appear as
-- 1-tile-wide diagonal ridges in xy with cardinal neighbors at
-- much lower elevation. The audit catches these as TERRAIN_SPIKE.
-- This pass detects them and lowers them toward neighbor max +
-- (threshold - 1) so the result is just under the audit threshold.

removeElevationSpikes
    ∷ Int  -- ^ spike threshold (lower if delta exceeds this)
    → Int  -- ^ max iterations
    → Int  -- ^ borderSize
    → (VU.Vector Int, VU.Vector MaterialId)
    → (VU.Vector Int, VU.Vector MaterialId)
removeElevationSpikes threshold maxIters bSize (elevVec, matVec) =
    (go False spikeConvergeCap (go True maxIters elevVec), matVec)
  where
    area = bSize * bSize

    -- Two phases. Phase 1 (cliff on) is the existing combined pass:
    -- isolated-pillar spike removal AND the iteration-limited cliff
    -- ramp that tames sheer walls. Its `maxIters` budget is deliberately
    -- small so it doesn't over-flatten mountains (see project_timeline_
    -- depth). But that same budget can run out mid-ramp on a steep peak:
    -- the flank below the summit is still being lowered when the cap is
    -- hit, leaving the summit standing as an isolated pillar (#254: seed
    -- 1337 kept +16/+19 TERRAIN_SPIKE peaks because the cliff ramp never
    -- finished re-exposing them). Phase 2 (cliff off) runs spike removal
    -- ONLY, to convergence: it lowers such residual pillars to
    -- mxNbr+(threshold-1) but never touches a tile's neighbours, so it
    -- can't cascade or flatten slopes and settles in 1-2 passes.
    spikeConvergeCap = 32

    go _       0 ev = ev
    go cliffOn n ev =
        let (ev', changed) = pass cliffOn ev
        in if changed then go cliffOn (n - 1) ev' else ev'

    -- Double-buffered: read neighbors from the immutable input `ev`,
    -- write changes to a fresh mutable `em`. This makes each pass
    -- scan-order-independent — without it, north/west neighbors
    -- (already processed in row-major order) leak their post-update
    -- values into south/east tiles' decisions, biasing how adjacent
    -- spike clusters resolve (audit #20). The outer `go` loop still
    -- iterates until convergence; updates just don't cascade within
    -- a single pass.
    pass cliffOn ev = runST $ do
        em ← VUM.new area
        forM_ [0 .. area - 1] $ \i →
            VUM.write em i (ev VU.! i)
        changedRef ← VUM.new 1
        VUM.write changedRef 0 (0 ∷ Int)
        forM_ [0 .. area - 1] $ \idx → do
            let bx = idx `mod` bSize
                by = idx `div` bSize
            -- Skip the outermost ring so all 4 cardinal neighbors
            -- are in-bounds (matches the audit which requires 4
            -- valid neighbors before classifying as a spike).
            when (bx > 0 ∧ bx < bSize - 1
                 ∧ by > 0 ∧ by < bSize - 1) $ do
                let e  = ev VU.! idx
                    n' = ev VU.! (idx - bSize)
                    s' = ev VU.! (idx + bSize)
                    w' = ev VU.! (idx - 1)
                    eN = ev VU.! (idx + 1)
                    mxNbr = max n' (max s' (max w' eN))
                    mnNbr = min n' (min s' (min w' eN))
                    -- Two independent extreme-feature clamps; whichever
                    -- lowers the tile more wins.
                    --   • SPIKE: tile > `threshold` above ALL neighbours
                    --     (an isolated pillar) → lower to mxNbr+(threshold-1).
                    --   • CLIFF: tile > `cliffThreshold` above its LOWEST
                    --     neighbour (a sheer wall the spike test misses —
                    --     the plateau side keeps mxNbr high) → lower to
                    --     mnNbr+cliffThreshold, capping the step. Iterating
                    --     ramps the wall back into the slope. Coastlines and
                    --     river canyons are shaped by earlier passes (coastal
                    --     table, smoothCliffs), so this mainly tames the
                    --     plate-boundary / ridge walls left tall when fewer
                    --     Ages erode them (see project_timeline_depth).
                    cliffThreshold = 28
                    spikeTarget = if e - mxNbr > threshold
                                  then mxNbr + (threshold - 1) else e
                    cliffTarget = if cliffOn ∧ e - mnNbr > cliffThreshold
                                  then mnNbr + cliffThreshold else e
                    target = min spikeTarget cliffTarget
                when (target < e) $ do
                    VUM.write em idx target
                    VUM.write changedRef 0 (1 ∷ Int)
        ef ← VU.unsafeFreeze em
        c ← VUM.read changedRef 0
        pure (ef, c > 0)

-- * Per-Period Timeline Application (chunk-level with erosion)

applyTimelineChunk ∷ GeoTimeline → Int → MaterialRegistry → WorldScale → ChunkCoord
                   → (VU.Vector Int, VU.Vector MaterialId)
                   → (VU.Vector Int, VU.Vector MaterialId)
applyTimelineChunk timeline worldSize registry wsc coord (baseElevVec, baseMatVec) =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize - chunkBorder
        chunkMinGY = cy * chunkSize - chunkBorder
        chunkMaxGX = cx * chunkSize + chunkSize + chunkBorder - 1
        chunkMaxGY = cy * chunkSize + chunkSize + chunkBorder - 1
        -- Apply all geological periods (events + erosion)
        (postElev, postMat) =
            foldl' (applyOnePeriod chunkMinGX chunkMinGY chunkMaxGX chunkMaxGY)
                   (baseElevVec, baseMatVec) (gtPeriods timeline)
        -- Final pass: re-apply all river carving events from every period.
        -- Erosion in later periods fills carved channels. Re-applying
        -- after all periods ensures the channels survive. River carving
        -- is target-based so re-application is idempotent.
        allRiverEvents = concatMap (\period →
            let relevant = V.toList $ V.filter (\(_, bb) →
                    bboxOverlapsChunk worldSize bb chunkMinGX chunkMinGY
                                                   chunkMaxGX chunkMaxGY
                    ) (gpExplodedEvents period)
            in filter (\(evt, _) → isRiverCarveEvent evt) relevant
            ) (gtPeriods timeline)
        -- Post-carving smoothing: collapse steep cliffs and pillars
        -- left by river carving. Only affects tiles within a small
        -- radius of tiles actually modified by river carving. This
        -- prevents natural cliffs far from rivers from being flattened,
        -- which would cause strata/elevation mismatches and black holes.
        -- Iterates to propagate smoothing outward from carved tiles.
        smoothCliffs preCarve vecs = go (3 ∷ Int) vecs
          where
            -- Build a mask of tiles that were modified by river carving.
            -- Then dilate it by 2 tiles so smoothing affects immediate
            -- neighbors of carved tiles but not distant terrain.
            carveMask = buildCarveMask borderSize preCarve (fst vecs)

            go 0 v = v
            go n (ev, mv) =
                let (ev', changed) = smoothCliffsPass borderSize carveMask ev
                in if changed then go (n - 1) (ev', mv) else (ev', mv)

        -- | Boolean-ish mask: 0 = no smoothing, 1-3 = smooth (distance
        --   from a carved tile + 1). Pure pull-style dilation via
        --   VU.generate so the dilation is symmetric in all four
        --   cardinal directions — the previous in-place push-style
        --   cascaded forward in row-major order, producing a wider
        --   mask on the east/south sides than the west/north (audit
        --   #21).
        buildCarveMask ∷ Int → VU.Vector Int → VU.Vector Int → VU.Vector Int
        buildCarveMask bSize preElev postElev =
            let area = bSize * bSize
                initial = VU.generate area $ \idx →
                    if postElev VU.! idx < preElev VU.! idx then 3 else 0 ∷ Int
                dilateOnce src = VU.generate area $ \idx →
                    let bx = idx `mod` bSize
                        by = idx `div` bSize
                        self = src VU.! idx
                        n  = if by > 0           then src VU.! (idx - bSize) else 0
                        s  = if by < bSize - 1   then src VU.! (idx + bSize) else 0
                        w  = if bx > 0           then src VU.! (idx - 1)     else 0
                        e  = if bx < bSize - 1   then src VU.! (idx + 1)     else 0
                        maxNbr = max n (max s (max w e))
                        fromNeighbor = if maxNbr > 1 then maxNbr - 1 else 0
                    in max self fromNeighbor
            in dilateOnce (dilateOnce initial)

        -- | Double-buffered smoothing: read neighbors from the
        --   immutable input `ev`, write to mutable `em`. Updates
        --   no longer cascade within a single pass (audit #21).
        --   Outer `go` loop still iterates to convergence.
        smoothCliffsPass bSize mask ev = runST $ do
            em ← VUM.new (bSize * bSize)
            forM_ [0 .. bSize * bSize - 1] $ \i →
                VUM.write em i (ev VU.! i)
            changedRef ← VUM.new 1
            VUM.write changedRef 0 (0 ∷ Int)
            forM_ [0 .. bSize * bSize - 1] $ \idx → do
                let bx = idx `mod` bSize
                    by = idx `div` bSize
                -- Only smooth tiles near carved areas (mask > 0)
                -- and skip edge tiles of the border
                when (mask VU.! idx > 0
                     ∧ bx > 0 ∧ bx < bSize - 1
                     ∧ by > 0 ∧ by < bSize - 1) $ do
                    let e  = ev VU.! idx
                        n' = ev VU.! (idx - bSize)
                        s' = ev VU.! (idx + bSize)
                        w' = ev VU.! (idx - 1)
                        e' = ev VU.! (idx + 1)
                        mn = min n' (min s' (min w' e'))
                    -- If this tile is >3 above its lowest neighbor,
                    -- lower it toward the neighbor. This collapses
                    -- tall pillars and smooths cliff faces left by
                    -- river carving.
                    when (e - mn > 3) $ do
                        let target = mn + 3
                        VUM.write em idx target
                        VUM.write changedRef 0 (1 ∷ Int)
            ef ← VU.unsafeFreeze em
            c ← VUM.read changedRef 0
            pure (ef, c > 0)

    in if null allRiverEvents
       then (postElev, postMat)
       else let preCarveElev = postElev
                carved = applyRiverCarvePass coord allRiverEvents (postElev, postMat)
            in smoothCliffs preCarveElev carved
  where
    borderSize = chunkSize + 2 * chunkBorder

    {-# INLINE toIndex #-}
    toIndex lx ly =
        let bx = lx + chunkBorder
            by = ly + chunkBorder
        in by * borderSize + bx

    {-# INLINE fromIndex #-}
    fromIndex idx =
        let (by, bx) = idx `divMod` borderSize
        in (bx - chunkBorder, by - chunkBorder)

    {-# INLINE inBorder #-}
    inBorder lx ly =
        lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
        ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

    {-# INLINE lookupElev #-}
    lookupElev vec lx ly fallback =
        if inBorder lx ly
        then vec VU.! toIndex lx ly
        else fallback

    -- CHUNK PATH: uses gpExplodedEvents (per-segment bboxes)
    -- Filtered once per chunk, then per-row Y-band sweep + per-tile
    -- bbox-wrap check inside the ST loop. The Y-band sweep (audit
    -- #23) computes the active event subset once per row instead of
    -- once per tile. The row filter MUST be wrap-aware: gy = (v−u)/2,
    -- so wrapping u by ±w shifts a tile's effective Δy against the
    -- bbox centre by ±w/2 — 'tileInBBoxWrapped' re-aliases the tile
    -- relative to the bbox, and a prefilter on the pre-wrap gy alone
    -- dropped every event whose bbox sat across the seam from the
    -- chunk (half a crater simply missing at the wrap meridian, and
    -- chunk-vs-fast parity broken there since the fast path has no
    -- prefilter). 'rowMayHitBBox' therefore accepts a row if ANY
    -- u-alias of it (Δy shifted by k·w/2, k ∈ {−1,0,1}) lands in the
    -- bbox's Y band; tileInBBoxWrapped stays the exact per-tile
    -- authority. Regression test:
    -- Test.Headless.WorldGen.WrapSeam.
    applyOnePeriod cMinGX cMinGY cMaxGX cMaxGY (elevVec, matVec) period =
        let -- River-carve events fire per-period (along with
        -- everything else). Carving channels each period — instead
        -- of deferring all carving to the end — is the intended
        -- design: later periods' erosion sees the existing channel
        -- and shapes its banks, and rivers can shift / dry up
        -- across geological history so the per-period record
        -- matters. 'applyRiverCarvePass' at the end of
        -- 'applyTimelineChunk' then re-asserts the final channel
        -- floors against the last period's erosion deposition;
        -- 'smoothCliffs' cleans up any remaining sharp edges.
            relevantTagged = V.toList $ V.filter (\(_, bb) →
                bboxOverlapsChunk worldSize bb cMinGX cMinGY cMaxGX cMaxGY
                ) (gpExplodedEvents period)

            borderArea = borderSize * borderSize

            ChunkCoord _ cy = coord

            -- Wrap-aware Y-band test (see the block comment above).
            -- Mirrors tileInBBoxWrapped's centre/half-extent integer
            -- arithmetic, with +1 slack for the div-2 rounding, so it
            -- is a strict superset of what the per-tile check accepts.
            halfWTiles = (worldSize * chunkSize) `div` 2
            rowMayHitBBox gy (_, bb) =
                let bMidY  = (bbMinY bb + bbMaxY bb) `div` 2
                    bHalfY = (bbMaxY bb - bbMinY bb) `div` 2
                    d      = gy - bMidY
                in abs d ≤ bHalfY + 1
                 ∨ abs (d - halfWTiles) ≤ bHalfY + 1
                 ∨ abs (d + halfWTiles) ≤ bHalfY + 1

            (postElev, postMat) =
                if null relevantTagged
                then (elevVec, matVec)
                else runST $ do
                    elevM ← VUM.new borderArea
                    matM  ← VUM.new borderArea
                    forM_ [0 .. borderSize - 1] $ \by → do
                        let ly = by - chunkBorder
                            gy = cy * chunkSize + ly
                            -- Y-band sweep: events whose bbox vertical
                            -- extent could include this row under any
                            -- u-alias of the row's tiles.
                            activeAtRow = filter (rowMayHitBBox gy)
                                                 relevantTagged
                        forM_ [0 .. borderSize - 1] $ \bx → do
                            let idx  = by * borderSize + bx
                                lx   = bx - chunkBorder
                                elev = elevVec VU.! idx
                                mat  = matVec  VU.! idx
                            if mat ≡ matGlacier
                                then do
                                    VUM.write elevM idx elev
                                    VUM.write matM  idx mat
                                else do
                                    let (gx, _)     = chunkToGlobal coord lx ly
                                        (gx', gy')  = wrapGlobalU worldSize gx gy
                                        cellEvents  = filter (\(_, bb) →
                                            tileInBBoxWrapped worldSize gx' gy' bb
                                            ) activeAtRow
                                        (elevOut, matOut) =
                                            foldl' (\(e, m) (evt, _) →
                                                applyOneEvent worldSize gx' gy' (e, m) evt
                                            ) (elev, mat) cellEvents
                                    VUM.write elevM idx elevOut
                                    VUM.write matM  idx matOut
                    elevF ← VU.unsafeFreeze elevM
                    matF  ← VU.unsafeFreeze matM
                    pure (elevF, matF)

            -- Cache the regional-erosion table lookups per period.
            -- Inside applyTimelineChunk's bordered region (24 tiles
            -- per side), all tiles fall inside the same 2×2 climate-
            -- region cell — so the 4 HashMap lookups can be done
            -- once per chunk-period instead of once per tile
            -- (formerly the 'findWithDefault' + record-construction
            -- hotspot at 55% of total init time).  We precompute the
            -- four corner ErosionParams here and lerp per-tile below.
            --
            -- For the rare chunk that straddles a region boundary
            -- (so some tile's @ru0@ ≠ the chunk-centre's), the per-
            -- tile path falls back to 'lookupRegionalErosion' so the
            -- correctness boundary stays exactly where it used to.
            erosionFB    = gpErosion period
            regMap       = gpRegionalErosion period
            regMapEmpty  = HM.null regMap
            periodDur    = gpDuration period
            wsScaleC     = wsScale wsc
            ChunkCoord _ccx _ccy = coord
            centreGX     = cMinGX + chunkSize `div` 2
            centreGY     = cMinGY + chunkSize `div` 2
            RegionGridCoords ru0C ru1C rv0C rv1C _ _ =
                regionGridCoords chunkSize worldSize centreGX centreGY
            (ep00C, ep10C, ep01C, ep11C) =
                erosionCornerLookup erosionFB regMap ru0C ru1C rv0C rv1C
            (finalElev, finalMat) = runST $ do
                elevM ← VUM.new borderArea
                matM  ← VUM.new borderArea
                forM_ [0 .. borderArea - 1] $ \idx → do
                    let elev = postElev VU.! idx
                        mat  = postMat  VU.! idx
                    if mat ≡ matGlacier
                        then do
                            VUM.write elevM idx elev
                            VUM.write matM  idx mat
                        else do
                            let bx = idx `mod` borderSize
                                by = idx `div` borderSize
                                -- Direct neighbour reads avoid the
                                -- per-call lookupElev/inBorder/toIndex
                                -- overhead (formerly ~21% of total
                                -- time). Tiles at the border edge of
                                -- the bordered region use 'elev'
                                -- itself as the fallback.
                                nN = if by > 0
                                     then postElev VU.! (idx - borderSize)
                                     else elev
                                nS = if by < borderSize - 1
                                     then postElev VU.! (idx + borderSize)
                                     else elev
                                nE = if bx < borderSize - 1
                                     then postElev VU.! (idx + 1)
                                     else elev
                                nW = if bx > 0
                                     then postElev VU.! (idx - 1)
                                     else elev
                                neighbors = (nN, nS, nE, nW)
                                lx = bx - chunkBorder
                                ly = by - chunkBorder
                                gx = cMinGX + lx + chunkBorder
                                gy = cMinGY + ly + chunkBorder
                                hardness = mpHardness
                                    (getMaterialProps registry mat)
                                -- Fast path: when this tile lands in
                                -- the same region cell as the chunk
                                -- centre (the common case), call the
                                -- scalar variant 'applyErosionLerp4'
                                -- with the 4 cached corner EPs — it
                                -- inlines just the 5 \"hot\" Float
                                -- lerps the erosion math actually
                                -- consumes, and defers the 4 sediment-
                                -- only lerps into a closure that only
                                -- fires if a material override is
                                -- needed.  Avoids the per-tile
                                -- ErosionParams record allocation that
                                -- 'lerpErosionParams' used to do.
                                erosionMod
                                  | regMapEmpty =
                                      applyErosion erosionFB worldSize
                                          periodDur wsScaleC
                                          (unMaterialId mat) hardness
                                          elev neighbors
                                  | otherwise =
                                      let RegionGridCoords ru0 _ rv0 _ tu tv =
                                              regionGridCoords chunkSize
                                                  worldSize gx gy
                                      in if ru0 ≡ ru0C ∧ rv0 ≡ rv0C
                                         then applyErosionLerp4
                                                  ep00C ep10C ep01C ep11C
                                                  tu tv
                                                  worldSize periodDur
                                                  wsScaleC (unMaterialId mat)
                                                  hardness elev neighbors
                                         else applyErosion
                                                  (lookupRegionalErosion
                                                      erosionFB regMap
                                                      worldSize gx gy)
                                                  worldSize periodDur
                                                  wsScaleC (unMaterialId mat)
                                                  hardness elev neighbors
                            VUM.write elevM idx (elev + gmElevDelta erosionMod)
                            VUM.write matM  idx (case gmMaterialOverride erosionMod of
                                Just m  → MaterialId m
                                Nothing → mat)
                elevF ← VU.unsafeFreeze elevM
                matF  ← VU.unsafeFreeze matM
                pure (elevF, matF)

        in (finalElev, finalMat)

    applyOneEvent ws gx gy (elev, mat) event =
        let hardness = mpHardness (getMaterialProps registry mat)
            mod' = applyGeoEvent event ws gx gy elev hardness
            elev' = elev + gmElevDelta mod'
            mat'  = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → mat
        in (elev', mat')

    -- | Final pass: re-apply river carving events after all periods.
    --   Erosion in later periods fills carved channels. This pass
    --   ensures the channels are present at the correct depth.
    applyRiverCarvePass crd riverEvts (elevVec, matVec) =
        let borderArea = borderSize * borderSize
        in runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                VUM.write elevM idx (elevVec VU.! idx)
                VUM.write matM  idx (matVec  VU.! idx)
            let minFloor = seaLevel - 1
            forM_ [0 .. borderArea - 1] $ \idx → do
                elev ← VUM.read elevM idx
                mat  ← VUM.read matM  idx
                let (lx, ly) = fromIndex idx
                    (gx, gy) = chunkToGlobal crd lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                    cellEvents = filter (\(_, bb) →
                        tileInBBoxWrapped worldSize gx' gy' bb
                        ) riverEvts
                    (elevOut, matOut) =
                        foldl' (\(e, m) (evt, _) →
                            applyOneEvent worldSize gx' gy' (e, m) evt
                        ) (elev, mat) cellEvents
                -- If river events touch this tile, enforce a floor to
                -- prevent erosion-deepened terrain from creating deep
                -- sub-ocean channels at river mouths.
                let elevClamped = if not (null cellEvents) ∧ elevOut < minFloor
                                  then minFloor
                                  else elevOut
                VUM.write elevM idx elevClamped
                VUM.write matM  idx matOut
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

-- * Bbox-Filtered Timeline Application (for zoom cache + ocean flood-fill)
--
-- Per-tile approximation of the chunk path. Used at world-init time
-- before chunks exist (ocean flood-fill, zoom-cache build) and for
-- camera.goToTile from Lua.
--
-- Lockstep design: to give hydraulic erosion a non-zero slope to chew
-- on, the center tile is advanced through the timeline alongside its 4
-- cardinal neighbors. Each period applies events to all 5 tiles, then
-- erosion: the center uses its real neighbors' post-events elevs,
-- while the neighbors fall back to slope-zero (we don't track their
-- neighbors). Neighbors are seeded from plate base elevation.
--
-- This still diverges from the chunk path in two places:
--   1. No cliff smoothing / spike removal (chunk-scope only).
--   2. The 4 tracked neighbors themselves erode with slope-zero, so
--      after many periods they drift below the chunk-path state.
-- For the center tile that's queried, neither matters much — most
-- erosion the center experiences is hydraulic, which now responds to
-- real slope.

-- `applyTimelineFast` and its helpers moved to `World.Generate.Timeline.Fast`
-- so `Geology.Timeline.compactRiverEvents` can call them too. Re-exported
-- below for existing call sites.

