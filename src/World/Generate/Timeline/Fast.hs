{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-tile approximation of the chunk path's timeline application.
--   Used at world-init time before chunks exist (ocean flood-fill,
--   zoom-cache build) and for camera.goToTile from Lua. Also used by
--   `Geology.Timeline.compactRiverEvents` to sample current terrain
--   at river segment endpoints for re-elevation.
--
--   Lockstep design: to give hydraulic erosion a non-zero slope to chew
--   on, the center tile is advanced through the timeline alongside its
--   4 cardinal neighbors. Each period applies events to all 5 tiles,
--   then erosion: the center uses its real neighbors' post-events
--   elevs, while the neighbors fall back to slope-zero. Neighbors are
--   seeded from plate base elevation.
--
--   This diverges from the chunk path in two ways:
--     1. No cliff smoothing / spike removal (chunk-scope only).
--     2. The 4 tracked neighbors themselves erode with slope-zero, so
--        after many periods they drift below the chunk-path state.
--   For the center tile that's queried, neither matters much — most
--   erosion the center experiences is hydraulic, which now responds to
--   real slope.
--
--   Lives in its own module (not in `Generate.Timeline`) so that
--   `Geology.Timeline` can import it without pulling in chunk-scope
--   helpers that depend on `Geology.Timeline.Helpers` etc.
module World.Generate.Timeline.Fast
    ( applyTimelineFast
    , applyTimelineFastFrom
    , applyPeriodEvents
    , applyPeriodErosionAt
    , applyExplodedEvents
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Geology.Coastal.Types (CoastalTable(..))
import World.Fluid.Seabed.Types (SeabedTable(..))
import World.Material (MaterialId(..), MaterialRegistry
                      , getMaterialProps, MaterialProps(..), matGlacier)
import World.Plate (wrapGlobalU, elevationAtGlobal)
import World.Geology.Event (applyGeoEvent)
import World.Geology.Erosion (applyErosion, lookupRegionalErosion)
import World.Scale (WorldScale(..), computeWorldScale)

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

-- | The wrapping API that consumes a full `GeoTimeline`. Existing
--   callers (zoom cache, ocean flood-fill, Lua camera) use this form.
applyTimelineFast ∷ GeoTimeline → [TectonicPlate] → Int → Int → Int
                  → MaterialRegistry
                  → (Int, MaterialId) → (Int, MaterialId)
applyTimelineFast timeline plates worldSize gx gy registry base =
    let (e, m) = applyTimelineFastFrom (gtSeed timeline) plates worldSize
                     gx gy registry
                     (gtPeriods timeline) (gtRiverExplodedEvents timeline)
                     base
        -- Coastal: the global table (save v25) makes this a per-tile
        -- lookup the fast path can afford — the old windowed pass was
        -- why it skipped coastal entirely (a ~20z chunk-vs-fast
        -- divergence in coastal zones). An empty table (e.g. the
        -- init-time ocean-map probe, which runs before the table is
        -- built) is a no-op.
        cx = gx `div` chunkSize
        cy = gy `div` chunkSize
        li = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                 * chunkSize
           + ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
        d  = case HM.lookup (ChunkCoord cx cy)
                            (coElevDelta (gtCoastal timeline)) of
               Just dv → dv VU.! li
               Nothing → 0
        m' = case HM.lookup (ChunkCoord cx cy)
                            (coMatOverride (gtCoastal timeline)) of
               Just mv | mv VU.! li ≠ 0 ∧ m ≠ matGlacier
                       → MaterialId (mv VU.! li)
               _       → m
        -- Seabed: same guarded per-tile lookup (save v26). Applied
        -- on top of coastal. Empty table (ocean-map probe at init,
        -- which runs before the seabed table exists) is a no-op.
        sd = case HM.lookup (ChunkCoord cx cy)
                            (sbElevDelta (gtSeabed timeline)) of
               Just dv → dv VU.! li
               Nothing → 0
        m'' = case HM.lookup (ChunkCoord cx cy)
                             (sbMatOverride (gtSeabed timeline)) of
                Just mv | mv VU.! li ≠ 0 ∧ m' ≠ matGlacier
                        → MaterialId (mv VU.! li)
                _       → m'
    in if e ≡ minBound then (e, m) else (e + d + sd, m'')

-- | Lower-level form taking the periods + river-exploded vector directly.
--   This is what `Geology.Timeline.compactRiverEvents` calls with a
--   stripped-rivers period list + an empty exploded vector to sample
--   pre-river current terrain at segment endpoints (so the carve target
--   tracks erosion rather than stale trace-time elevation).
applyTimelineFastFrom
    ∷ Word64
    → [TectonicPlate]
    → Int → Int → Int
    → MaterialRegistry
    → [GeoPeriod]
    → V.Vector (GeoEvent, EventBBox)
    → (Int, MaterialId)
    → (Int, MaterialId)
applyTimelineFastFrom seed plates worldSize gx gy registry periods riverExploded
                      (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize

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
            foldl' step ((baseElev, baseMat), nN0, nS0, nE0, nW0) periods
        (elev, mat) = cFinal

        -- Re-apply cached river carving after all periods (bbox-filtered).
        (gx', gy') = wrapGlobalU worldSize gx gy
        (elev', mat') = applyExplodedEvents worldSize gx' gy' elev mat registry
                                            riverExploded
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
