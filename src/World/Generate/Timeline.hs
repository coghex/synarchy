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
import World.Material (MaterialId(..), matGlacier)
import World.Plate (wrapGlobalU)
import World.Geology.Types (GeoModification(..))
import World.Geology.Event (applyGeoEvent)
import World.Geology.Erosion (applyErosion)
import World.Scale (WorldScale(..), computeWorldScale)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

-----------------------------------------------------------
-- Per-Period Timeline Application (chunk-level with erosion)
-----------------------------------------------------------

applyTimelineChunk ∷ GeoTimeline → Int → WorldScale → ChunkCoord
                   → (VU.Vector Int, VU.Vector MaterialId)
                   → (VU.Vector Int, VU.Vector MaterialId)
applyTimelineChunk timeline worldSize wsc coord (baseElevVec, baseMatVec) =
    let ChunkCoord cx cy = coord
        chunkMinGX0 = cx * chunkSize - chunkBorder
        chunkMinGY0 = cy * chunkSize - chunkBorder
        chunkMaxGX0 = cx * chunkSize + chunkSize + chunkBorder - 1
        chunkMaxGY0 = cy * chunkSize + chunkSize + chunkBorder - 1
        -- Wrap chunk bounds into canonical u-space
        (chunkMinGX, chunkMinGY) = wrapGlobalU worldSize chunkMinGX0 chunkMinGY0
        (chunkMaxGX, chunkMaxGY) = wrapGlobalU worldSize chunkMaxGX0 chunkMaxGY0
    in foldl' (applyOnePeriod chunkMinGX chunkMinGY chunkMaxGX chunkMaxGY)
              (baseElevVec, baseMatVec) (gtPeriods timeline)
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
                                erosionMod = applyErosion
                                    (gpErosion period)
                                    worldSize
                                    (gpDuration period)
                                    (wsScale wsc)
                                    (unMaterialId mat)
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

    applyOneEvent ws gx gy (elev, mat) event =
        let mod' = applyGeoEvent event ws gx gy elev
            elev' = elev + gmElevDelta mod'
            mat'  = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → mat
        in (elev', mat')

-----------------------------------------------------------
-- Legacy Timeline Application (single-column, for external callers)
-----------------------------------------------------------

applyTimeline ∷ GeoTimeline → Int → Int → Int → (Int, MaterialId) → (Int, MaterialId)
applyTimeline timeline worldSize gx gy (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize
    in foldl' (applyPeriodSingle worldSize wsc gx gy)
              (baseElev, baseMat) (gtPeriods timeline)

applyPeriodSingle ∷ Int → WorldScale → Int → Int
                  → (Int, MaterialId) → GeoPeriod → (Int, MaterialId)
applyPeriodSingle worldSize wsc gx gy (elev, mat) period =
    let (elev', mat') = foldl' applyOneEvent (elev, mat) (gpEvents period)
        erosionMod = applyErosion
            (gpErosion period)
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId mat')
            elev'
            (elev', elev', elev', elev')
        elev'' = elev' + gmElevDelta erosionMod
        mat'' = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → mat'
    in (elev'', mat'')
  where
    applyOneEvent (e, m) event =
        let mod' = applyGeoEvent event worldSize gx gy e
            e' = e + gmElevDelta mod'
            m' = case gmMaterialOverride mod' of
                Just mm → MaterialId mm
                Nothing → m
        in (e', m')

-----------------------------------------------------------
-- Bbox-Filtered Timeline Application (for zoom cache)
-----------------------------------------------------------

applyTimelineFast ∷ GeoTimeline → Int → Int → Int → (Int, MaterialId) → (Int, MaterialId)
applyTimelineFast timeline worldSize gx gy (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize
    in foldl' (applyPeriodFiltered worldSize wsc gx gy)
              (baseElev, baseMat) (gtPeriods timeline)

-- ZOOM CACHE PATH: uses gpTaggedEvents (compact, ~10 events)
-- River events go through applyGeoEvent → applyRiverCarve which
-- handles per-segment iteration internally. This is fine because
-- most tiles miss the coarse bbox entirely (~10 cheap comparisons),
-- and the rare tile that hits a river does one segment fold.
applyPeriodFiltered ∷ Int → WorldScale → Int → Int
                    → (Int, MaterialId) → GeoPeriod → (Int, MaterialId)
applyPeriodFiltered worldSize wsc gx gy (elev, mat) period =
    let (gx', gy') = wrapGlobalU worldSize gx gy  -- ADD THIS
        bb = gpPeriodBBox period
        -- Early exit: tile outside all events in this period
        (elev', mat') =
            if gx' < bbMinX bb ∨ gx' > bbMaxX bb ∨ gy' < bbMinY bb ∨ gy' > bbMaxY bb
            then (elev, mat)
            else applyExplodedEvents worldSize gx' gy' elev mat
                                     (gpExplodedEvents period)

        erosionMod = applyErosion
            (gpErosion period)
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId mat')
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
applyExplodedEvents ∷ Int → Int → Int → Int → MaterialId
                    → V.Vector (GeoEvent, EventBBox)
                    → (Int, MaterialId)
applyExplodedEvents worldSize gx gy e0 m0 vec = go startIdx e0 m0
  where
    len = V.length vec

    -- Binary search: find first index where bbMinY > gy - maxPossiblePad
    -- We want events whose bbMinY ≤ gy (since bbMaxY ≥ bbMinY, these could contain gy).
    -- Events are sorted by bbMinY ascending.
    -- Skip all events where bbMaxY < gy (they end before this tile's Y).
    -- Stop when bbMinY > gy (they start after this tile's Y).
    startIdx = lowerBound 0 len
    lowerBound !lo !hi
        | lo ≥ hi   = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
                (_, bb) = V.unsafeIndex vec mid
            in if bbMaxY bb < gy
               then lowerBound (mid + 1) hi
               else lowerBound lo mid
    go !i !e !m
        | i ≥ len   = (e, m)
        | otherwise =
            let (evt, evtBB) = V.unsafeIndex vec i
            in if bbMinY evtBB > gy
               then (e, m)
               -- Replace the simple check with wrapped version:
               else if not (tileInBBoxWrapped worldSize gx gy evtBB)
                    then go (i + 1) e m
                    else let mod' = applyGeoEvent evt worldSize gx gy e
                             e' = e + gmElevDelta mod'
                             m' = case gmMaterialOverride mod' of
                                 Just mm → MaterialId mm
                                 Nothing → m
                         in go (i + 1) e' m'
