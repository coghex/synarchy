{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Timeline
    ( applyTimelineChunk
    , applyTimelineFast
    , removeElevationSpikes
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), matGlacier, MaterialRegistry(..)
                      , getMaterialProps, MaterialProps(..))
import World.Plate (wrapGlobalU, TectonicPlate, elevationAtGlobal)
import World.Geology.Types (GeoModification(..))
import World.Geology.Event (applyGeoEvent)
import World.Geology.Timeline.Types (GeoEvent(..))
import World.Hydrology.Types (HydroFeature(..))
import World.Geology.Erosion (applyErosion, lookupRegionalErosion)
import World.Scale (WorldScale(..), computeWorldScale)
import World.Constants (seaLevel)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
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
    (go maxIters elevVec, matVec)
  where
    area = bSize * bSize

    go 0 ev = ev
    go n ev =
        let (ev', changed) = pass ev
        in if changed then go (n - 1) ev' else ev'

    -- Double-buffered: read neighbors from the immutable input `ev`,
    -- write changes to a fresh mutable `em`. This makes each pass
    -- scan-order-independent — without it, north/west neighbors
    -- (already processed in row-major order) leak their post-update
    -- values into south/east tiles' decisions, biasing how adjacent
    -- spike clusters resolve (audit #20). The outer `go` loop still
    -- iterates until convergence; updates just don't cascade within
    -- a single pass.
    pass ev = runST $ do
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
                -- Spike: tile is more than `threshold` above all
                -- 4 cardinal neighbors. Lower it to mxNbr + (threshold - 1)
                -- so the resulting delta is below the audit threshold
                -- but the tile remains visibly above its neighbors.
                when (e - mxNbr > threshold) $ do
                    let target = mxNbr + (threshold - 1)
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

    toIndex lx ly =
        let bx = lx + chunkBorder
            by = ly + chunkBorder
        in by * borderSize + bx

    fromIndex idx =
        let (by, bx) = idx `divMod` borderSize
        in (bx - chunkBorder, by - chunkBorder)

    inBorder lx ly =
        lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
        ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

    lookupElev vec lx ly fallback =
        if inBorder lx ly
        then vec VU.! toIndex lx ly
        else fallback

    -- CHUNK PATH: uses gpExplodedEvents (per-segment bboxes)
    -- Filtered once per chunk, then per-row Y-band sweep + per-tile
    -- bbox-wrap check inside the ST loop. The Y-band sweep (audit
    -- #23) computes the active event subset once per row instead of
    -- once per tile, since bbox Y extent is unaffected by u-axis
    -- wrap. For chunks far from the cylindrical seam (the common
    -- case) pre-wrap gy uniquely identifies the row; tileInBBoxWrapped
    -- still does the final per-tile correctness check.
    applyOnePeriod cMinGX cMinGY cMaxGX cMaxGY (elevVec, matVec) period =
        let relevantTagged = V.toList $ V.filter (\(_, bb) →
                bboxOverlapsChunk worldSize bb cMinGX cMinGY cMaxGX cMaxGY
                ) (gpExplodedEvents period)

            borderArea = borderSize * borderSize

            ChunkCoord _ cy = coord

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
                            -- extent includes this row's pre-wrap gy.
                            activeAtRow = filter (\(_, bb) →
                                bbMinY bb ≤ gy ∧ bbMaxY bb ≥ gy
                                ) relevantTagged
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
                            let (lx, ly) = fromIndex idx
                                neighbors =
                                    ( lookupElev postElev lx (ly - 1) elev
                                    , lookupElev postElev lx (ly + 1) elev
                                    , lookupElev postElev (lx + 1) ly elev
                                    , lookupElev postElev (lx - 1) ly elev
                                    )
                                gx = cMinGX + lx + chunkBorder
                                gy = cMinGY + ly + chunkBorder
                                regionalParams = lookupRegionalErosion
                                    (gpErosion period) (gpRegionalErosion period)
                                    worldSize gx gy
                                hardness = mpHardness (getMaterialProps registry mat)
                                erosionMod = applyErosion
                                    regionalParams
                                    worldSize
                                    (gpDuration period)
                                    (wsScale wsc)
                                    (unMaterialId mat)
                                    hardness
                                    elev
                                    neighbors
                            VUM.write elevM idx (elev + gmElevDelta erosionMod)
                            VUM.write matM  idx (case gmMaterialOverride erosionMod of
                                Just m  → MaterialId m
                                Nothing → mat)
                elevF ← VU.unsafeFreeze elevM
                matF  ← VU.unsafeFreeze matM
                pure (elevF, matF)

        in (finalElev, finalMat)

    isRiverCarveEvent ∷ GeoEvent → Bool
    isRiverCarveEvent (HydroEvent (RiverFeature _)) = True
    isRiverCarveEvent (HydroEvent (GlacierFeature _)) = True
    isRiverCarveEvent (RiverSegmentEvent _) = True
    isRiverCarveEvent (RiverDeltaEvent _) = True
    isRiverCarveEvent _ = False

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

-- | Apply only the events for a single period to a tile (no erosion).
{-# INLINE applyPeriodEvents #-}
applyPeriodEvents ∷ Int → Int → Int → MaterialRegistry → GeoPeriod
  → (Int, MaterialId) → (Int, MaterialId)
applyPeriodEvents worldSize gx gy registry period (elev, mat) =
    let (gx', gy') = wrapGlobalU worldSize gx gy
        bb = gpPeriodBBox period
    in if not (tileInBBoxWrapped worldSize gx' gy' bb)
       then (elev, mat)
       else applyExplodedEvents worldSize gx' gy' elev mat registry
                                (gpExplodedEvents period)

-- | Apply erosion for a single period to a tile with explicit neighbor
--   elevations. Caller decides what neighbors to pass (real for the
--   tracked center, self-as-neighbors for the slope-zero fallback).
{-# INLINE applyPeriodErosionAt #-}
applyPeriodErosionAt ∷ Int → WorldScale → Int → Int → MaterialRegistry → GeoPeriod
  → (Int, MaterialId) → (Int, Int, Int, Int) → (Int, MaterialId)
applyPeriodErosionAt worldSize wsc gx gy registry period (elev, mat) nbrs =
    let hardness = mpHardness (getMaterialProps registry mat)
        regionalParams = lookupRegionalErosion
            (gpErosion period) (gpRegionalErosion period)
            worldSize gx gy
        erosionMod = applyErosion
            regionalParams
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId mat)
            hardness
            elev
            nbrs
        elev' = elev + gmElevDelta erosionMod
        mat' = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → mat
    in (elev', mat')

applyTimelineFast ∷ GeoTimeline → [TectonicPlate] → Int → Int → Int → MaterialRegistry
  → (Int, MaterialId) → (Int, MaterialId)
applyTimelineFast timeline plates worldSize gx gy registry (baseElev, baseMat) =
    let seed = gtSeed timeline
        wsc = computeWorldScale worldSize

        -- Seed the 4 cardinal neighbors from plate-base elevation.
        sampleBase ngx ngy =
            let (ngx', ngy') = wrapGlobalU worldSize ngx ngy
            in elevationAtGlobal seed plates worldSize ngx' ngy'
        nN0 = sampleBase gx       (gy - 1)
        nS0 = sampleBase gx       (gy + 1)
        nE0 = sampleBase (gx + 1) gy
        nW0 = sampleBase (gx - 1) gy

        -- One lockstep period: events on all 5 tiles, then erosion
        -- (center uses real neighbors, neighbors use slope-zero).
        step (c, nN, nS, nE, nW) period =
            let c1  = applyPeriodEvents worldSize gx       gy       registry period c
                nN1 = applyPeriodEvents worldSize gx       (gy - 1) registry period nN
                nS1 = applyPeriodEvents worldSize gx       (gy + 1) registry period nS
                nE1 = applyPeriodEvents worldSize (gx + 1) gy       registry period nE
                nW1 = applyPeriodEvents worldSize (gx - 1) gy       registry period nW
                c2  = applyPeriodErosionAt worldSize wsc gx gy registry period c1
                          (fst nN1, fst nS1, fst nE1, fst nW1)
                -- Erode neighbors with slope-zero so they accumulate
                -- some smoothing across periods (otherwise they'd
                -- only ever receive event deltas).
                eN = fst nN1; eS = fst nS1; eE = fst nE1; eW = fst nW1
                nN2 = applyPeriodErosionAt worldSize wsc gx       (gy - 1) registry period nN1
                          (eN, eN, eN, eN)
                nS2 = applyPeriodErosionAt worldSize wsc gx       (gy + 1) registry period nS1
                          (eS, eS, eS, eS)
                nE2 = applyPeriodErosionAt worldSize wsc (gx + 1) gy       registry period nE1
                          (eE, eE, eE, eE)
                nW2 = applyPeriodErosionAt worldSize wsc (gx - 1) gy       registry period nW1
                          (eW, eW, eW, eW)
            in (c2, nN2, nS2, nE2, nW2)

        (cFinal, _, _, _, _) =
            foldl' step ((baseElev, baseMat), nN0, nS0, nE0, nW0) (gtPeriods timeline)
        (elev, mat) = cFinal

        -- Re-apply cached river carving after all periods (bbox-filtered).
        (gx', gy') = wrapGlobalU worldSize gx gy
        (elev', mat') = applyExplodedEvents worldSize gx' gy' elev mat registry
                                            (gtRiverExplodedEvents timeline)
    in (elev', mat')

-- | Tight loop over the exploded events vector.
--   Recomputes hardness per event from the current material — matches
--   the chunk path's semantics (Generate/Chunk.hs applyOneEvent), so a
--   river event that overrides matId to e.g. riverbed correctly affects
--   the hardness seen by a subsequent lava event on the same tile.
{-# INLINE applyExplodedEvents #-}
applyExplodedEvents ∷ Int → Int → Int → Int → MaterialId → MaterialRegistry
                    → V.Vector (GeoEvent, EventBBox)
                    → (Int, MaterialId)
applyExplodedEvents worldSize gx gy e0 m0 registry vec = go 0 e0 m0
  where
    len = V.length vec
    go !i !e !m
        | i ≥ len   = (e, m)
        | otherwise =
            let (evt, evtBB) = V.unsafeIndex vec i
            in if not (tileInBBoxWrapped worldSize gx gy evtBB)
                    then go (i + 1) e m
                    else let hardness = mpHardness (getMaterialProps registry m)
                             mod' = applyGeoEvent evt worldSize gx gy e hardness
                             e' = e + gmElevDelta mod'
                             m' = case gmMaterialOverride mod' of
                                 Just mm → MaterialId mm
                                 Nothing → m
                         in go (i + 1) e' m'

