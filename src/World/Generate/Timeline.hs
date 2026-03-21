{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Timeline
    ( applyTimelineChunk
    , applyTimeline
    , applyTimelineFast
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
import World.Plate (wrapGlobalU)
import World.Geology.Types (GeoModification(..))
import World.Geology.Event (applyGeoEvent)
import World.Geology.Timeline.Types (GeoEvent(..))
import World.Hydrology.Types (HydroFeature(..))
import World.Geology.Erosion (applyErosion, lookupRegionalErosion)
import World.Scale (WorldScale(..), computeWorldScale)
import World.Constants (seaLevel)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

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

        -- | Build a boolean mask: True for tiles within 2 tiles of a
        --   carved tile. "Carved" = elevation decreased by the river
        --   carve pass.
        buildCarveMask bSize preElev postElev = runST $ do
            mask ← VUM.replicate (bSize * bSize) (0 ∷ Int)
            -- Mark carved tiles
            forM_ [0 .. bSize * bSize - 1] $ \idx → do
                let pre  = preElev VU.! idx
                    post = postElev VU.! idx
                when (post < pre) $
                    VUM.write mask idx 3  -- 3 = carved tile + 2 dilation budget
            -- Dilate: spread marks to neighbors, decrementing each step.
            -- Two dilation passes give a 2-tile radius around carved tiles.
            forM_ [1 ∷ Int, 2] $ \_ →
                forM_ [0 .. bSize * bSize - 1] $ \idx → do
                    v ← VUM.read mask idx
                    when (v > 1) $ do
                        let bx = idx `mod` bSize
                            by = idx `div` bSize
                            spread nIdx = do
                                nv ← VUM.read mask nIdx
                                when (nv < v - 1) $
                                    VUM.write mask nIdx (v - 1)
                        when (by > 0)          $ spread (idx - bSize)
                        when (by < bSize - 1)  $ spread (idx + bSize)
                        when (bx > 0)          $ spread (idx - 1)
                        when (bx < bSize - 1)  $ spread (idx + 1)
            VU.unsafeFreeze mask

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
                    e ← VUM.read em idx
                    n' ← VUM.read em (idx - bSize)
                    s' ← VUM.read em (idx + bSize)
                    w' ← VUM.read em (idx - 1)
                    e' ← VUM.read em (idx + 1)
                    let mn = min n' (min s' (min w' e'))
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
    -- Filtered once per chunk, then fine-filtered per tile inside the ST loop.
    applyOnePeriod cMinGX cMinGY cMaxGX cMaxGY (elevVec, matVec) period =
        let relevantTagged = V.toList $ V.filter (\(_, bb) →
                bboxOverlapsChunk worldSize bb cMinGX cMinGY cMaxGX cMaxGY
                ) (gpExplodedEvents period)

            borderArea = borderSize * borderSize

            (postElev, postMat) =
                if null relevantTagged
                then (elevVec, matVec)
                else runST $ do
                    elevM ← VUM.new borderArea
                    matM  ← VUM.new borderArea
                    forM_ [0 .. borderArea - 1] $ \idx → do
                        let (lx, ly) = fromIndex idx
                            elev = elevVec VU.! idx
                            mat  = matVec  VU.! idx
                        if mat ≡ matGlacier
                            then do
                                VUM.write elevM idx elev
                                VUM.write matM  idx mat
                            else do
                                let (gx, gy)   = chunkToGlobal coord lx ly
                                    (gx', gy') = wrapGlobalU worldSize gx gy
                                    cellEvents = filter (\(_, bb) →
                                        tileInBBoxWrapped worldSize gx' gy' bb
                                        ) relevantTagged
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
                                props = getMaterialProps registry mat
                                hardness = mpHardness props
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

-- * Legacy Timeline Application (single-column, for external callers)

applyTimeline ∷ GeoTimeline → Int → Int → Int → Float
  → (Int, MaterialId) → (Int, MaterialId)
applyTimeline timeline worldSize gx gy hardness (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize
        (elev, mat) = foldl' (applyPeriodSingle worldSize wsc gx gy hardness)
                             (baseElev, baseMat) (gtPeriods timeline)
        -- Re-apply cached river carving after all periods (bbox-filtered)
        (gx', gy') = wrapGlobalU worldSize gx gy
        (elev', mat') = applyExplodedEvents worldSize gx' gy' elev mat hardness
                                            (gtRiverExplodedEvents timeline)
    in (elev', mat')

applyOneEvtS ∷ Int → Int → Int → Float → (Int, MaterialId) → GeoEvent → (Int, MaterialId)
applyOneEvtS worldSize gx gy hardness (e, m) event =
    let mod' = applyGeoEvent event worldSize gx gy e hardness
        e' = e + gmElevDelta mod'
        m' = case gmMaterialOverride mod' of
            Just mm → MaterialId mm
            Nothing → m
    in (e', m')

applyPeriodSingle ∷ Int → WorldScale → Int → Int → Float
  → (Int, MaterialId) → GeoPeriod → (Int, MaterialId)
applyPeriodSingle worldSize wsc gx gy hardness (elev, mat) period =
    let (elev', mat') = foldl' (applyOneEvtS worldSize gx gy hardness) (elev, mat)
                               (gpEvents period)
        regionalParams = lookupRegionalErosion
            (gpErosion period) (gpRegionalErosion period)
            worldSize gx gy
        erosionMod = applyErosion
            regionalParams
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId mat')
            hardness
            elev'
            (elev', elev', elev', elev')
        elev'' = elev' + gmElevDelta erosionMod
        mat'' = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → mat'
    in (elev'', mat'')

-- * Bbox-Filtered Timeline Application (for zoom cache)

applyTimelineFast ∷ GeoTimeline → Int → Int → Int → Float
  → (Int, MaterialId) → (Int, MaterialId)
applyTimelineFast timeline worldSize gx gy hardness (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize
        (elev, mat) = foldl' (applyPeriodFiltered worldSize wsc gx gy hardness)
                             (baseElev, baseMat) (gtPeriods timeline)
        -- Re-apply cached river carving after all periods (bbox-filtered)
        (gx', gy') = wrapGlobalU worldSize gx gy
        (elev', mat') = applyExplodedEvents worldSize gx' gy' elev mat hardness
                                            (gtRiverExplodedEvents timeline)
    in (elev', mat')

-- ZOOM CACHE PATH: uses gpTaggedEvents (compact, ~10 events)
-- River events go through applyGeoEvent → applyRiverCarve which
-- handles per-segment iteration internally. This is fine because
-- most tiles miss the coarse bbox entirely (~10 cheap comparisons),
-- and the rare tile that hits a river does one segment fold.
applyPeriodFiltered ∷ Int → WorldScale → Int → Int → Float
  → (Int, MaterialId) → GeoPeriod → (Int, MaterialId)
applyPeriodFiltered worldSize wsc gx gy hardness (elev, mat) period =
    let (gx', gy') = wrapGlobalU worldSize gx gy
        bb = gpPeriodBBox period
        -- Early exit: tile outside all events in this period
        (elev', mat') =
            if not (tileInBBoxWrapped worldSize gx' gy' bb)
            then (elev, mat)
            else applyExplodedEvents worldSize gx' gy' elev mat hardness
                                     (gpExplodedEvents period)
        regionalParams = lookupRegionalErosion
            (gpErosion period) (gpRegionalErosion period)
            worldSize gx gy
        erosionMod = applyErosion
            regionalParams
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId mat')
            hardness
            elev'
            (elev', elev', elev', elev')
        elev'' = elev' + gmElevDelta erosionMod
        mat'' = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → mat'
    in (elev'', mat'')

-- | Tight loop over the exploded events vector.
--   Uses two separate accumulators instead of a tuple to avoid
--   boxing overhead. The INLINE lets GHC unbox the Int accumulator.
{-# INLINE applyExplodedEvents #-}
applyExplodedEvents ∷ Int → Int → Int → Int → MaterialId → Float
                    → V.Vector (GeoEvent, EventBBox)
                    → (Int, MaterialId)
applyExplodedEvents worldSize gx gy e0 m0 hardness vec = go 0 e0 m0
  where
    len = V.length vec
    go !i !e !m
        | i ≥ len   = (e, m)
        | otherwise =
            let (evt, evtBB) = V.unsafeIndex vec i
            in if not (tileInBBoxWrapped worldSize gx gy evtBB)
                    then go (i + 1) e m
                    else let mod' = applyGeoEvent evt worldSize gx gy e hardness
                             e' = e + gmElevDelta mod'
                             m' = case gmMaterialOverride mod' of
                                 Just mm → MaterialId mm
                                 Nothing → m
                         in go (i + 1) e' m'

