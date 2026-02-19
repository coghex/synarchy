{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Timeline
    ( applyTimelineChunk
    , applyTimeline
    , applyTimelineFast
    ) where

import UPrelude
import Control.Monad.ST (runST)
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

-- | Walk the geological timeline period by period, applying events
--   and erosion across all columns simultaneously.
--   Events are per-tile (no neighbor dependency).
--   Erosion smooths each tile toward its neighbors' post-event elevations,
--   using the shared elevation map for neighbor lookups.
applyTimelineChunk ∷ GeoTimeline → Int → WorldScale → ChunkCoord
                   → (VU.Vector Int, VU.Vector MaterialId)
                   → (VU.Vector Int, VU.Vector MaterialId)
applyTimelineChunk timeline worldSize wsc coord (baseElevVec, baseMatVec) =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize - chunkBorder
        chunkMinGY = cy * chunkSize - chunkBorder
        chunkMaxGX = cx * chunkSize + chunkSize + chunkBorder - 1
        chunkMaxGY = cy * chunkSize + chunkSize + chunkBorder - 1
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

    applyOnePeriod cMinGX cMinGY cMaxGX cMaxGY (elevVec, matVec) period =
        let -- Use pre-computed tagged events, filter to chunk overlap
            relevantTagged = filter (\(_, bb) →
                bboxOverlapsChunk worldSize bb cMinGX cMinGY cMaxGX cMaxGY
                ) (gpTaggedEvents period)

            borderArea = borderSize * borderSize

            -- Phase 1: Apply geological events (single pass)
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
                                    -- Per-cell bbox filter using pre-computed bboxes
                                    cellEvents = filter (\(_, bb) →
                                        gx' ≥ bbMinX bb ∧ gx' ≤ bbMaxX bb ∧
                                        gy' ≥ bbMinY bb ∧ gy' ≤ bbMaxY bb
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

            -- Phase 2: Apply erosion (single pass)
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

-- | Like applyTimeline but with bounding-box pre-filtering.
--   Skips events whose bbox doesn't contain (gx, gy), avoiding
--   the expensive distance/sqrt computation inside applyGeoEvent.
--
--   For a typical world with ~20 periods × ~10 events, a tile
--   far from any feature evaluates ~0-5 events instead of ~200.
--   This is safe because applyGeoEvent already returns noModification
--   for out-of-range tiles — bbox filtering just short-circuits earlier.
--
--   Uses the same erosion logic as applyTimeline (single-column,
--   no neighbor data). The only difference is the event filter.
applyTimelineFast ∷ GeoTimeline → Int → Int → Int → (Int, MaterialId) → (Int, MaterialId)
applyTimelineFast timeline worldSize gx gy (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize
    in foldl' (applyPeriodFiltered worldSize wsc gx gy)
              (baseElev, baseMat) (gtPeriods timeline)

applyPeriodFiltered ∷ Int → WorldScale → Int → Int
                    → (Int, MaterialId) → GeoPeriod → (Int, MaterialId)
applyPeriodFiltered worldSize wsc gx gy (elev, mat) period =
    let -- Use pre-computed tagged events instead of calling eventBBox
        relevantEvents = filter (\(_, bb) →
            gx ≥ bbMinX bb ∧ gx ≤ bbMaxX bb
             ∧ gy ≥ bbMinY bb ∧ gy ≤ bbMaxY bb
            ) (gpTaggedEvents period)

        (elev', mat') = foldl' applyOneEvt (elev, mat) relevantEvents

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
    applyOneEvt (e, m) (event, _bb) =
        let mod' = applyGeoEvent event worldSize gx gy e
            e' = e + gmElevDelta mod'
            m' = case gmMaterialOverride mod' of
                Just mm → MaterialId mm
                Nothing → m
        in (e', m')
