{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | River-event compaction: the post-timeline pass that collapses
--   every active river's scattered per-Age
--   @HydroEvent (RiverFeature _)@ entries down to one, carrying its
--   final persistent state, landed in the most-recent Age period.
--   Called once from 'World.Geology.Timeline.buildTimeline'.
module World.Geology.Timeline.Compact
    ( compactRiverEvents
    ) where
import UPrelude
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import World.Types
import World.Plate (wrapGlobalU, elevationAtGlobal)
import World.Material (MaterialRegistry)
import World.Fluid.River (fixupSegmentContinuity)
import World.Generate.Timeline.Fast (applyTimelineFastFrom)
import World.Geology.Timeline.Helpers
    (mkGeoPeriod, isActiveRiver, getRiverParamsFromPf)

-- | Compact river events so each active river contributes exactly one
--   `HydroEvent (RiverFeature _)`, carrying its final persistent state,
--   landed in the most-recent Age period. All other periods have river
--   HydroEvents stripped. See buildTimeline for the motivation.
--
--   Each segment's `rsStartElev`/`rsEndElev` is re-sampled from the
--   timeline at end-of-Eon. The original values came from the river
--   trace's elev grid at BIRTH age and never updated. Erosion in later
--   ages typically lowered terrain along and around the river path —
--   if we don't re-sample, the carve target `cf = rsEndElev - rsDepth`
--   stays at the stale (high) value while neighbor terrain has dropped,
--   so the river ends up perched on whatever survived erosion, with
--   visible water cliffs against the lower adjacent valleys.
--
--   We sample via `applyTimelineFastFrom` on a stripped-rivers view —
--   pre-river terrain at the endpoint, so the carve doesn't try to
--   target itself. Sampled values may produce non-descending segments
--   (sub-grid noise, locally rising terrain); `fixupSegmentContinuity`
--   re-threads start = prev.end and clamps `rsEndElev ≤ rsStartElev`.
--
--   Input list is newest-first (matches `tbsPeriods` convention).
compactRiverEvents
    ∷ Word64
    → [TectonicPlate]
    → MaterialRegistry
    → Int
    → [PersistentFeature]
    → [GeoPeriod]
    → [GeoPeriod]
compactRiverEvents seed plates registry ws features finalPeriods =
    let -- All periods with river HydroEvents stripped — the view we
        -- sample against. Used only for sampling; the result we emit
        -- below also starts from this list (then injects fresh re-carves
        -- into the most-recent Age period).
        strippedAll = map stripRivers finalPeriods

        sampleOne gx gy =
            let (gx', gy') = wrapGlobalU ws gx gy
                base = elevationAtGlobal seed plates ws gx' gy'
                (e, _) = applyTimelineFastFrom seed plates ws gx' gy'
                                               registry strippedAll V.empty base
            in e

        -- "Drain perched rivers": sample not just the endpoint but a
        -- small star pattern around it (center + 4 cardinal offsets at
        -- `drainRadius`), and take the MIN. Without this, segments
        -- whose endpoints happen to land on a plateau next to a lower
        -- valley keep the plateau elev as refElev — the carve target
        -- stays above the valley floor and the river ends up perched
        -- on the plateau, rendering as a tall water cliff against the
        -- valley below (seed 666 had 18-tile water cliffs from this).
        -- Sampling the local minimum lets the carve reach down into
        -- the valley so the water surface sits at the valley floor.
        -- Sampling more than ~5 tiles out picks up unrelated valleys
        -- in steep terrain (seed 666 type cases), which makes things
        -- worse not better; ≤ 3 is too local to escape narrow plateaus.
        -- Tuned visually + on the cliff fuzz.
        drainRadius ∷ Int
        drainRadius = 5

        sampleAt (GeoCoord gx gy) =
            let c = sampleOne  gx               gy
                n = sampleOne  gx              (gy - drainRadius)
                s = sampleOne  gx              (gy + drainRadius)
                e = sampleOne (gx + drainRadius) gy
                w = sampleOne (gx - drainRadius) gy
            in minimum [c, n, s, e, w]

        resampleSeg seg =
            seg { rsStartElev = sampleAt (rsStart seg)
                , rsEndElev   = sampleAt (rsEnd seg)
                }

        resampleRiver pf =
            let river = getRiverParamsFromPf pf
                resampled = V.map resampleSeg (rpSegments river)
                threaded = fixupSegmentContinuity resampled
            in river { rpSegments = threaded }

        -- Map pfId → resampled RiverParams, so tributary re-alignment
        -- below can look up the parent's *resampled* segments (we need
        -- the post-sample elevations to compute the parent's
        -- channel-floor at the junction).
        resampledById = HM.fromList
            [ (pfId pf, resampleRiver pf)
            | pf ← features
            , isActiveRiver (pfFeature pf)
            , pfActivity pf ≡ FActive
            ]

        -- Tributary re-alignment.
        --
        -- After re-sampling, a tributary's last segment ends at the
        -- sampled natural elev at its mouth coord (= junction with
        -- parent). The parent's segment, also re-sampled, has the SAME
        -- elev at that coord. But channel-floors are
        -- `lerpedElev − rsDepth`, and trib/parent have different
        -- depths, so trib's cf and parent's cf at the junction differ
        -- by `mainDepth − tribDepth` — a 4–7 tile water cliff at the
        -- confluence in the worst cases. Force trib's last segment to
        -- end at `parentCf + tribDepth` so the cf's match (clamped to
        -- ≤ rsStartElev of the last seg so we never produce uphill).
        --
        -- For a branched trib, junction is at parent's rsStart (alongT
        -- = 0). For a merged trib, junction is somewhere along the
        -- parent's segment; the alongT projection handles both cases.
        alignTribToParent tribRp parentRp =
            let tribSegs = rpSegments tribRp
                lastIdx = V.length tribSegs - 1
            in if V.null tribSegs ∨ V.null (rpSegments parentRp)
               then tribRp
               else
                 let lastSeg = tribSegs V.! lastIdx
                     junctionCoord = rsEnd lastSeg
                     parentSeg = closestParentSeg
                                     (rpSegments parentRp) junctionCoord
                     parentLerpElev = lerpSegElev parentSeg junctionCoord
                     parentCf = parentLerpElev - rsDepth parentSeg
                     desiredEnd = parentCf + rsDepth lastSeg
                     clamped = min desiredEnd (rsStartElev lastSeg)
                     newLast = lastSeg { rsEndElev = clamped }
                 in tribRp { rpSegments = tribSegs V.// [(lastIdx, newLast)] }

        finalRiverParams pf =
            let resampled = HM.lookupDefault (getRiverParamsFromPf pf)
                                             (pfId pf) resampledById
            in case pfParentId pf of
                Just parentId →
                    case HM.lookup parentId resampledById of
                        Just parentRp → alignTribToParent resampled parentRp
                        Nothing       → resampled  -- parent gone (drained, etc)
                Nothing → resampled

        freshRiverEvts =
            [ HydroEvent (RiverFeature (finalRiverParams pf))
            | pf ← features
            , isActiveRiver (pfFeature pf)
            , pfActivity pf ≡ FActive
            ]

        -- Inject the re-carves into the most-recent Age period of the
        -- already-stripped list. `injectAt` walks until the first Age
        -- and appends `freshRiverEvts` there; later periods are
        -- unchanged.
        injectAt [] = []
        injectAt (p:ps)
            | gpScale p ≡ Age =
                let evts' = gpEvents p ⧺ freshRiverEvts
                in rebuild p evts' : ps
            | otherwise = p : injectAt ps

    in injectAt strippedAll
  where
    notStaleRiver (HydroEvent (RiverFeature _)) = False
    notStaleRiver _                             = True

    rebuild p evts = mkGeoPeriod ws (gpName p) (gpScale p) (gpDuration p)
                                 (gpDate p) evts
                                 (gpErosion p) (gpRegionalErosion p)

    -- Fast path: if a period has no river HydroEvents to strip, return
    -- the original — skips bbox re-tagging in `mkGeoPeriod`.
    stripRivers p =
        let cleaned = filter notStaleRiver (gpEvents p)
        in if length cleaned ≡ length (gpEvents p)
           then p
           else rebuild p cleaned

    -- Pick the parent segment whose midpoint is closest to the
    -- junction coord — best heuristic for finding which segment the
    -- tributary actually joins.
    closestParentSeg parentSegs (GeoCoord jx jy) =
        let dist seg =
                let GeoCoord sx sy = rsStart seg
                    GeoCoord ex ey = rsEnd seg
                    mx = (sx + ex) `div` 2
                    my = (sy + ey) `div` 2
                in abs (mx - jx) + abs (my - jy)
            bestIdx = V.minIndexBy
                (\a b → compare (dist a) (dist b)) parentSegs
        in parentSegs V.! bestIdx

    -- Lerp the parent segment's reference elev at the projected
    -- position of `target`. Project `target − start` onto `end − start`,
    -- clamp t ∈ [0, 1], lerp between rsStartElev/rsEndElev. Ignores
    -- u-axis wrap — junctions are local so wrap effects are negligible.
    lerpSegElev seg (GeoCoord tx ty) =
        let GeoCoord sx sy = rsStart seg
            GeoCoord ex ey = rsEnd seg
            dx = fromIntegral (ex - sx) ∷ Float
            dy = fromIntegral (ey - sy) ∷ Float
            denom = dx * dx + dy * dy
        in if denom < 1e-6
           then rsStartElev seg
           else
             let px = fromIntegral (tx - sx) ∷ Float
                 py = fromIntegral (ty - sy) ∷ Float
                 t = max 0 (min 1 ((px * dx + py * dy) / denom))
                 startE = fromIntegral (rsStartElev seg) ∷ Float
                 endE   = fromIntegral (rsEndElev seg) ∷ Float
             in floor (startE + t * (endE - startE))
