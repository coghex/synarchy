{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Blood pooling (#883): the STATIONARY half of ongoing bleeding — a
--   unit that is bleeding externally but not covering ground (standing,
--   shuffling in place, or collapsed) grows a LOCAL pool instead of
--   stacking trail marks. Companion to "Blood.Trail" (the moving half);
--   both consume the SAME per-unit 'Unit.Types.Trail.TrailState'
--   accumulator "Combat.Wounds.Tick" feeds with conserved real external
--   blood loss, which is what makes walk-then-stop-then-walk seamless.
--
--   The pool is built from LAYERED BOUNDED SPAWNS (the epic's chosen
--   mechanism): small overlapping pool/drop marks placed through the
--   same 'Blood.Types.spawnDecal' entry point everything else uses, up
--   to a documented per-cluster bound. Decal records are NEVER mutated
--   — there is no "grow this decal" field and there deliberately will
--   not be one, which locks the persisted record shape ahead of #884.
--
--   Three parts, mirroring "Blood.Trail"'s split:
--
--   * 'classifyOngoing' — the pure per-tick arbitration between the two
--     halves, by DISPLACEMENT from the cluster anchor (not per-tick step
--     distance: a unit shuffling in place covers path distance without
--     going anywhere, and must keep feeding one pool rather than
--     starting a new cluster every tick).
--   * 'consumePoolLayers' — the pure cadence/volume/bound math that pops
--     zero or more layers, and 'poolLayerOffset'/'poolBloodForVolume'
--     which place and weight them.
--   * 'spawnPoolLayer' — the IO glue, via 'Blood.Types.spawnDecal'
--     (texture reuse, caps, FIFO eviction, aging, and teardown all keep
--     applying with no new decal lifecycle).
module Blood.Pool
    ( -- * Thresholds (documented named constants)
      PoolThresholds(..)
    , defaultPoolThresholds
    , poolStyleVolume
    , poolTravelSpeed
      -- * Per-tick arbitration
    , OngoingMode(..)
    , classifyOngoing
      -- * Pure layer accumulator
    , PoolLayerOut(..)
    , consumePoolLayers
    , poolAtBound
    , poolLayerOffset
      -- * Volume -> texture-request mapping
    , PoolBlood(..)
    , poolStyleFor
    , poolFootprintFor
    , poolBloodForVolume
      -- * IO glue
    , spawnPoolLayer
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import World.Page.Types (WorldPageId)
import World.State.Types (WorldManager(..), WorldState(..))
import Unit.Types (UnitId)
import Unit.Types.Trail (TrailState(..))
import Blood.Trail (trailSeverityBucket, trailOpacity, trailModerateVolume)
import Blood.Types

-- ----- Thresholds -----

-- | Every numeric bound the pool obeys, named so both the hspec math
--   tests and @tools/bleeding_trail_probe.py@ can assert against the
--   SAME documented contract instead of magic coordinates (issue #883
--   spec addition).
data PoolThresholds = PoolThresholds
    { ptClusterRadius ∷ !Float
      -- ^ World tiles. A unit staying within this of the cluster anchor
      --   is "in place" and keeps feeding the SAME cluster; crossing it
      --   ends the cluster outright (fresh anchor, fresh layer budget).
    , ptMaxLayers     ∷ !Int
      -- ^ The per-cluster layer bound (requirement 3): a unit bleeding
      --   in place indefinitely gets at most this many marks out of one
      --   cluster — a max-size pool. Never re-derived from the decal
      --   store, so global FIFO eviction cannot reopen the budget.
    , ptMinCadence    ∷ !Double
      -- ^ Real (unpaused @gameTimeRef@) seconds between layers — the
      --   same clock "Blood.Trail" uses, never the world calendar, so
      --   density is immune to @world.setTimeScale@ (requirement 7).
    , ptMinVolume     ∷ !Float
      -- ^ Litres of accumulated external loss a layer costs. Unlike the
      --   trail's gates this IS a volume floor: it is what makes a
      --   trickle pool slowly and an arterial bleed pool fast
      --   (requirement 4) — the two gates together mean the interval
      --   between layers is @max(cadence-limited, volume-limited)@.
    , ptJitterRadius  ∷ !Float
      -- ^ World tiles. Layers are placed on a deterministic spiral
      --   within this of the anchor, so the cluster reads as one
      --   growing pool of overlapping marks rather than a stack.
    } deriving (Show, Eq)

-- | Roughly: a pool grows a mark every 1.5 s (or every 15 mL, whichever
--   is slower), spreads over about a third of a tile, and tops out at a
--   dozen layers — a bleeding-out unit's pool finishes growing in
--   around 18 s, comfortably inside the time an untreated arterial
--   bleed takes to kill, while a light trickle never gets near the
--   bound before clotting.
defaultPoolThresholds ∷ PoolThresholds
defaultPoolThresholds = PoolThresholds
    { ptClusterRadius = 1.0
    , ptMaxLayers     = 12
    , ptMinCadence    = 1.5
    , ptMinVolume     = 0.015
    , ptJitterRadius  = 0.35
    }

-- | Litres at/above which a layer reads as an actual pool rather than
--   scattered drops. Shares "Blood.Trail"'s moderate band deliberately
--   — the two halves are the same blood, so the same amount should not
--   look "moderate" on a trail and "light" in a pool.
poolStyleVolume ∷ Float
poolStyleVolume = trailModerateVolume

-- | The documented speed (world tiles per real second) above which a
--   unit is always travelling and therefore never pools: covering
--   'ptClusterRadius' faster than 'ptMinCadence' means every cadence
--   window ends outside the previous anchor. Below it a unit counts as
--   effectively stationary (a crawl, a collapsed shuffle) and pools —
--   which is the intended behaviour, not an edge case.
poolTravelSpeed ∷ PoolThresholds → Float
poolTravelSpeed pt = ptClusterRadius pt / realToFrac (ptMinCadence pt)

-- ----- Per-tick arbitration -----

-- | Which half of ongoing bleeding owns THIS movement tick. Exactly one
--   runs per tick per unit, so the shared accumulator can never be
--   consumed twice.
data OngoingMode = ModeTravel | ModeDwell
    deriving (Show, Eq)

-- | Decide the mode for a unit now at @(px, py)@, and return the
--   accumulator with its cluster bookkeeping updated.
--
--   The test is DISPLACEMENT from the cluster anchor, never this tick's
--   step distance: a unit shuffling in place covers real path distance
--   every tick while going nowhere, and requirement 5 says that must
--   keep feeding one pool rather than restarting a cluster each tick.
--   Conversely a unit that has genuinely walked out of
--   'ptClusterRadius' has left its pool behind — the cluster ends, and
--   the new position becomes a fresh anchor with a FULL fresh layer
--   budget (issue #883 spec addition: the bound is per active cluster,
--   not per unit for all time).
--
--   A unit with no anchor yet (first tick after it started bleeding)
--   anchors where it stands and counts as dwelling; if it is in fact
--   walking, the very next crossing flips it to 'ModeTravel'.
classifyOngoing
    ∷ PoolThresholds → (Float, Float) → TrailState → (OngoingMode, TrailState)
classifyOngoing pt (px, py) ts = case tsClusterAnchor ts of
    Nothing → (ModeDwell, anchored)
    Just (ax, ay)
        | sqrt ((px - ax) * (px - ax) + (py - ay) * (py - ay))
            > ptClusterRadius pt → (ModeTravel, anchored)
        | otherwise              → (ModeDwell, ts)
  where
    anchored = ts { tsClusterAnchor = Just (px, py), tsClusterLayers = 0 }

-- ----- Pure layer accumulator -----

-- | One popped layer: @ploIndex@ is its 0-based index WITHIN the
--   current cluster (which drives 'poolLayerOffset', so the pool grows
--   outward deterministically), @ploVolume@ the litres it represents.
data PoolLayerOut = PoolLayerOut
    { ploIndex  ∷ !Int
    , ploVolume ∷ !Float
    } deriving (Show, Eq)

-- | Has this cluster already spent its whole layer budget? Exposed for
--   the debug query's at-bound flag (requirement 9).
poolAtBound ∷ PoolThresholds → TrailState → Bool
poolAtBound pt ts = tsClusterLayers ts ≥ ptMaxLayers pt

-- | Advance a dwelling unit's pool by one movement tick, popping zero
--   or more layers. @now@ is the absolute unpaused game-time seconds
--   (never the world calendar).
--
--   Cadence AND volume are both hard floors, and a window covering
--   several multiples of both pops that many layers at once, each
--   taking an EQUAL share of the pending volume — the same catch-up
--   shape 'Blood.Trail.consumeTrailMarks' uses, and for the same
--   reason: layer density must not depend on how the same stretch of
--   game time was chopped into ticks (requirement 7).
--
--   At the bound nothing is popped and NOTHING is discarded: the
--   pending volume stays banked in the shared accumulator, and
--   @tsLastMarkAt@/@tsDistSinceMark@ are left exactly as they were, so
--   when the unit walks off, the resumed trail still has to clear its
--   own full distance/cadence gates before its first mark — no
--   catch-up burst (issue #883 spec addition).
--
--   A popped layer resets BOTH gates (cadence via @tsLastMarkAt@,
--   distance to zero): a pool layer is an emission like a trail mark,
--   so the next trail mark is a full gate away from the pool rather
--   than stamped on top of it.
consumePoolLayers
    ∷ PoolThresholds → Double → TrailState → (TrailState, [PoolLayerOut])
consumePoolLayers pt now ts0
    | tsPendingVolume ts0 ≤ 0 = (ts0, [])
    | n ≤ 0                   = (ts0, [])
    | otherwise               = (ts', layers)
  where
    remaining = max 0 (ptMaxLayers pt - tsClusterLayers ts0)
    elapsed   = max 0 (now - tsLastMarkAt ts0)
    byCadence
        | ptMinCadence pt ≤ 0 = remaining
        | otherwise           = floor (elapsed / ptMinCadence pt)
    byVolume
        | ptMinVolume pt ≤ 0 = remaining
        | otherwise          = floor (tsPendingVolume ts0 / ptMinVolume pt)
    n         = minimum [remaining, byCadence, byVolume]
    -- Guarded because this module is {-# LANGUAGE Strict #-}: every
    -- where-binding is forced on entry, including on the @n <= 0@
    -- guards above where the quotient would otherwise be a NaN.
    share     = if n > 0 then tsPendingVolume ts0 / fromIntegral n else 0
    base      = tsClusterLayers ts0
    layers    = [ PoolLayerOut (base + i) share | i ← [0 .. n - 1] ]
    -- Only the cadence multiples actually SPENT are consumed; any
    -- remainder carries forward, so chopping the same dwell into
    -- different tick sizes converges on the same layer count.
    ts'       = ts0
        { tsPendingVolume = 0
        , tsDistSinceMark = 0
        , tsLastMarkAt    = now - (elapsed - fromIntegral n * ptMinCadence pt)
        , tsClusterLayers = base + n
        }

-- | Deterministic placement of layer @idx@ relative to the cluster
--   anchor: a golden-angle spiral whose radius grows with
--   @sqrt (idx / ptMaxLayers)@, so successive layers overlap near the
--   centre and spread outward as the pool fills in — always within
--   'ptJitterRadius' of the anchor (issue #883 acceptance (a)). @seed@
--   only rotates the whole spiral, so two adjacent bleeders' clusters
--   do not land in lockstep.
poolLayerOffset ∷ PoolThresholds → Int → Int → (Float, Float)
poolLayerOffset pt seed idx = (r * cos ang, r * sin ang)
  where
    n     = max 1 (ptMaxLayers pt)
    -- Clamp rather than wrap: an index past the bound can only come
    -- from a caller ignoring 'poolAtBound', and must still land inside
    -- the documented radius.
    frac  = fromIntegral (max 0 (min idx n)) / fromIntegral n
    r     = max 0 (ptJitterRadius pt) * sqrt frac
    -- 2.399963 rad — the golden angle, the standard even-spread
    -- phyllotaxis constant; successive indices never line up.
    ang   = fromIntegral idx * 2.399963
          + fromIntegral (seed `mod` 360) * (pi / 180)

-- ----- Volume -> texture-request mapping -----

-- | The resolved visual weight of one pool layer, before the caller
--   supplies world position/rotation/seed — same shape as
--   'Blood.Trail.TrailBlood'.
data PoolBlood = PoolBlood
    { pbStyle     ∷ !BloodStyle
    , pbSeverity  ∷ !SeverityBucket
    , pbFootprint ∷ !FootprintBucket
    , pbOpacity   ∷ !Float
    } deriving (Show, Eq)

-- | Requirement 1's vocabulary: the pool/drop family only — scattered
--   drops for a light layer, an actual pool once there is enough volume
--   in one layer to read as one. Never smear/spatter/streak (those read
--   as travel or impact, not a pool).
poolStyleFor ∷ Float → BloodStyle
poolStyleFor v
    | v ≥ poolStyleVolume = StylePool
    | otherwise           = StyleDrops

-- | Always SMALL. The pool grows by layering more marks, never by
--   making one mark bigger — that is exactly the "layered bounded
--   spawns" decision, and it is what keeps decal records immutable.
poolFootprintFor ∷ Float → FootprintBucket
poolFootprintFor _ = FootprintSmall

-- | Map one layer's external blood volume (litres) onto its full
--   texture-request weight. Severity and opacity reuse
--   "Blood.Trail"'s monotonic volume mapping so the same amount of
--   blood never reads heavier on a trail than in a pool.
poolBloodForVolume ∷ Float → PoolBlood
poolBloodForVolume v = PoolBlood
    { pbStyle     = poolStyleFor v
    , pbSeverity  = trailSeverityBucket v
    , pbFootprint = poolFootprintFor v
    , pbOpacity   = trailOpacity v
    }

-- ----- IO glue -----

-- | Place one pool-layer decal via the SAME 'Blood.Types.spawnDecal'
--   entry point 'Blood.Trail.spawnTrailMark' and
--   'Blood.Impact.spawnImpactBlood' use. Silently a no-op if @page@
--   names a world that isn't currently loaded (same policy as the other
--   two — issue #607 requirement 8's precedent).
spawnPoolLayer
    ∷ WorldSimCapability
    → WorldPageId
    → Float           -- ^ gx (anchor + layer offset, already applied)
    → Float           -- ^ gy
    → Int             -- ^ surfaceZ
    → Text            -- ^ representative wound kind (texture-request tag only)
    → Float           -- ^ this layer's volume (litres)
    → Float           -- ^ rotation, radians
    → Int             -- ^ seed
    → Maybe UnitId    -- ^ source unit
    → Double          -- ^ game time
    → IO ()
spawnPoolLayer wsc page gx gy z kind volume rotation seed mSrc now = do
    wm ← readIORef (wsWorldManagerRef wsc)
    case lookup page (wmWorlds wm) of
        Nothing → pure ()
        Just ws → do
            let pb = poolBloodForVolume volume
                req = BloodTextureRequest
                    { btrStyle      = pbStyle pb
                    , btrWoundKind  = kind
                    , btrSeverity   = pbSeverity pb
                    , btrFootprint  = pbFootprint pb
                    , btrAnisotropy = AnisotropyNone
                      -- Smooth edges: a settled pool has a clean
                      -- meniscus, unlike a thrown/dragged mark.
                    , btrEdge       = EdgeSmooth
                    , btrSeed       = seed
                    }
                mkSpec tid = BloodDecalSpec
                    { bspTexture        = tid
                    , bspPage           = page
                    , bspX              = gx
                    , bspY              = gy
                    , bspSurfaceZ       = z
                    , bspOffsetX        = 0
                    , bspOffsetY        = 0
                    , bspRotation       = rotation
                    , bspScale          = 1
                    , bspCreatedAt      = now
                    , bspInitialWetness = 1
                    , bspWoundKind      = kind
                    , bspSeverity       = pbSeverity pb
                    , bspSourceUnit     = mSrc
                    , bspOpacity        = pbOpacity pb
                    }
            atomicModifyIORef' (wsBloodStoreRef ws) $ \store →
                let (store', _, _, _) = spawnDecal req mkSpec store
                in (store', ())
