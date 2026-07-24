{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Bleeding trails (#882): the MOVING half of ongoing bleeding — a
--   unit with externally-bleeding wounds leaves a bounded trail of
--   marks along its travelled path. Companion to "Blood.Impact" (the
--   one-shot mark a FRESH wound stamps on landing); this module is the
--   ongoing, distance/time-gated emitter that consumes the per-unit
--   'Unit.Types.Trail.TrailState' accumulator "Combat.Wounds.Tick"
--   feeds with conserved real external blood loss.
--
--   Two halves, same split as 'Blood.Impact':
--
--   * The pure decision — 'consumeTrailMarks' — advances the
--     accumulator by one movement tick's step distance and pops zero
--     or more marks, and 'trailBloodForVolume' maps a mark's aggregated
--     volume onto a texture request (style/severity/footprint/opacity).
--   * 'spawnTrailMark' — the IO glue that places the resulting decal via
--     the SAME 'Blood.Types.spawnDecal' entry point 'Blood.Impact'
--     already goes through (texture reuse/caps/FIFO eviction/aging/
--     teardown all keep applying with no new decal lifecycle).
--
--   See docs/blood_decals.md's "Ongoing bleeding behavior" for the
--   design-record rationale, and "Combat.Wounds.Bleed" for the
--   external- vs internal-bleed kind classification this module reuses
--   throughout (imported, never redefined).
module Blood.Trail
    ( -- * Thresholds (documented named constants)
      TrailThresholds(..)
    , defaultTrailThresholds
    , trailModerateVolume
    , trailSevereVolume
    , trailCatastrophicVolume
      -- * Pure accumulator
    , TrailMarkOut(..)
    , consumeTrailMarks
      -- * Volume -> texture-request mapping
    , TrailBlood(..)
    , trailSeverityBucket
    , trailFootprint
    , trailStyleFor
    , trailOpacity
    , trailBloodForVolume
      -- * IO glue
    , spawnTrailMark
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import World.Page.Types (WorldPageId)
import World.State.Types (WorldManager(..), WorldState(..))
import Unit.Types (UnitId)
import Unit.Types.Trail (TrailState(..))
import Blood.Types

-- ----- Thresholds -----

-- | Both gates a mark must clear (issue #882 requirement 4): distance
--   travelled AND real elapsed seconds since the last mark, so marks
--   per tile of path stay independent of frame rate, unit-thread tick
--   rate, movement speed, or @world.setTimeScale@ (the cadence clock
--   here is 'Unit.Types.Trail.tsLastMarkAt' — the unpaused
--   @gameTimeRef@, never the world calendar). Neither gate is a volume
--   threshold — see 'consumeTrailMarks'.
data TrailThresholds = TrailThresholds
    { ttMinDistance ∷ !Float   -- ^ world tiles
    , ttMinCadence  ∷ !Double  -- ^ real seconds
    } deriving (Show, Eq)

-- | Roughly one tile and half a second apart — close enough that a
--   walking unit's trail reads as continuous, far enough that a sprint
--   or a fast-forwarded tick can't flood the bounded decal store.
defaultTrailThresholds ∷ TrailThresholds
defaultTrailThresholds = TrailThresholds
    { ttMinDistance = 1.0
    , ttMinCadence  = 0.5
    }

-- Named volume bands (litres a single mark represents) driving
-- 'trailSeverityBucket'/'trailFootprint'/'trailStyleFor'/'trailOpacity'.
-- Calibrated against "Combat.Wounds.Constants".bleedScale's own
-- calibration comment: an ordinary moderate wound (~0.1 L/s) crossing
-- the distance/cadence gates over roughly a second lands mid-"Moderate";
-- the bleedScale docstring's extreme case (~1.4 L/s, a severed-artery
-- neck wound) lands well past "Catastrophic".
trailModerateVolume, trailSevereVolume, trailCatastrophicVolume ∷ Float
trailModerateVolume     = 0.05
trailSevereVolume       = 0.15
trailCatastrophicVolume = 0.4

-- ----- Pure accumulator -----

-- | One popped mark: @tmoFraction@ locates it along THIS tick's step
--   (0 = the position at the start of the tick, 1 = the end — see
--   'consumeTrailMarks'), @tmoVolume@ is the litres it represents.
data TrailMarkOut = TrailMarkOut
    { tmoFraction ∷ !Float
    , tmoVolume   ∷ !Float
    } deriving (Show, Eq)

-- | Advance a unit's trail accumulator by one movement tick's step
--   distance, popping zero or more marks. @dt@ is THIS call's own real
--   elapsed seconds (paired with @stepDist@, its own distance); @now@
--   is the absolute unpaused game-time seconds (never the world
--   calendar) — @now - dt@ is when this call's window started.
--
--   Distance and cadence are BOTH hard floors (issue #882 requirement
--   4) — a mark needs at least 'ttMinDistance' path-tiles AND at least
--   'ttMinCadence' real seconds since the last one. Volume is
--   deliberately NOT a gating threshold (issue #882 requirement 3): once
--   the gates are satisfied, the mark's weight is the FULL amount
--   accumulated since the last mark (or an equal share of it, when a
--   large jump satisfies several multiples of the gates at once — see
--   below), so a slow trickle crossing the gates produces a light mark
--   while a heavy bleed over the same distance/time produces a heavy
--   one — the RATE of marks stays capped by distance/cadence
--   regardless of how fast blood is accumulating.
--
--   A jump crossing several multiples of BOTH gates at once (a big
--   catch-up dt, or a large single step in the hspec partition-
--   invariance tests) pops that many marks, each getting an EQUAL share
--   of the pending volume (so total emitted volume is exactly
--   conserved). Each mark's @tmoFraction@ within THIS call's
--   [start,end] step is whichever of the two gates is the SLOWER
--   (later-reached) one for that mark — @max@ of the distance-implied
--   and the time-implied fraction, assuming uniform motion across the
--   call — so a mark whose count is capped by cadence lands where
--   cadence actually cleared, not wherever distance alone would have
--   put it. This is what keeps positions (not just counts/volumes)
--   timestep-partition-invariant: using distance alone would place a
--   cadence-limited mark differently depending on how the same journey
--   was chunked into calls (a confirmed round-2 review regression).
consumeTrailMarks
    ∷ TrailThresholds → Float → Double → Double → TrailState
    → (TrailState, [TrailMarkOut])
consumeTrailMarks tp stepDist dt now ts0 =
    let d0           = tsDistSinceMark ts0
        d1           = d0 + max 0 stepDist
        elapsedEnd   = max 0 (now - tsLastMarkAt ts0)
        elapsedStart = max 0 (elapsedEnd - max 0 dt)
        nDist | ttMinDistance tp > 0 = floor (d1 / ttMinDistance tp)
              | otherwise            = 0 ∷ Int
        nTime | ttMinCadence tp > 0  = floor (elapsedEnd / ttMinCadence tp)
              | otherwise            = 0 ∷ Int
        -- Distance/cadence alone would happily "pop" a zero-volume mark
        -- once banked progress from an EARLIER bleed clears both gates
        -- on a tick where nothing has drained since (a wound that just
        -- clotted, or a unit that was briefly not bleeding at all) —
        -- issue #882 requirement 2/3 forbid a mark with no real blood
        -- behind it, so no pop happens at all while pendingVolume is
        -- empty; the banked distance/cadence simply keeps waiting
        -- (harmlessly — the caller clears the whole accumulator once
        -- there's neither pending volume nor any live external bleed
        -- left to wait for, see "Unit.Thread.Movement").
        --
        -- Likewise requiring stepDist > 0 (round-4 review): distance
        -- and cadence can BOTH already be banked from earlier movement
        -- while the unit has since stopped — cadence alone keeps
        -- advancing with real time regardless of motion. Popping in a
        -- call where nothing moved would place a mark at a stationary
        -- unit's resting position, which is stationary/collapsed
        -- POOLING — issue #882 requirement explicitly defers that to
        -- #883. A pop only ever happens in a call where the unit is
        -- ACTUALLY moving; the banked state is preserved untouched
        -- (nothing here resets it) for whenever movement resumes.
        n | tsPendingVolume ts0 > 0 ∧ stepDist > 0 = max 0 (min nDist nTime)
          | otherwise                              = 0
    in if n ≤ 0
       then (ts0 { tsDistSinceMark = d1 }, [])
       else
         let share = tsPendingVolume ts0 / fromIntegral n
             distFrac k
                 | stepDist > 0 =
                     clamp01 ((fromIntegral k * ttMinDistance tp - d0) / stepDist)
                 | otherwise = 0
             timeFrac k
                 | dt > 0 =
                     clamp01 (realToFrac
                        ((fromIntegral k * ttMinCadence tp - elapsedStart) / dt))
                 | otherwise = 0
             frac k = max (distFrac k) (timeFrac k)
             marks = [ TrailMarkOut (frac k) share | k ← [1 .. n] ]
             -- Residual distance/time must be measured from where the
             -- LAST mark actually landed (round-3 review) — not from
             -- "n whole minDistance multiples", which is only correct
             -- when distance is the dimension that governed every
             -- popped mark. When cadence governs instead (frac driven
             -- by timeFrac, e.g. 3 tiles in 1s: minDistance=1 would
             -- allow 3 marks but minCadence caps it to 2, landing the
             -- 2nd at the full 3-tile mark, not the 2-tile one), that
             -- formula fictionally banks leftover distance the unit
             -- never actually travelled PAST the real mark position —
             -- letting the very next tiny step pop another mark well
             -- under 'ttMinDistance' away. @frac n@ (monotonic in k) is
             -- the last mark's true fraction through THIS call's own
             -- [start,end] window; only the portion of the step AFTER
             -- that fraction is unconsumed distance/time.
             lastFrac = frac n
             ts' = ts0
                 { tsPendingVolume = 0
                 , tsDistSinceMark = (1 - lastFrac) * stepDist
                 , tsLastMarkAt    = now - realToFrac (1 - lastFrac) * dt
                 }
         in (ts', marks)

-- ----- Volume -> texture-request mapping -----

-- | The resolved visual weight of a trail mark, before the caller
--   supplies world position/rotation/seed — same shape as
--   'Blood.Impact.ImpactBlood', minus anisotropy (trail marks stay
--   within the "drops and/or short smears" vocabulary, never the
--   directional spatter/streak family impact marks use).
data TrailBlood = TrailBlood
    { tbStyle     ∷ !BloodStyle
    , tbSeverity  ∷ !SeverityBucket
    , tbFootprint ∷ !FootprintBucket
    , tbOpacity   ∷ !Float
    } deriving (Show, Eq)

-- | Severity bucket from a mark's aggregated volume (litres) — larger
--   accumulated loss never produces a lighter result (monotonic in
--   @v@), unlike 'Blood.Impact.impactSeverityBucket' which reads wound
--   SEVERITY (0..1), not accumulated blood volume.
trailSeverityBucket ∷ Float → SeverityBucket
trailSeverityBucket v
    | v ≥ trailCatastrophicVolume = SeverityCatastrophic
    | v ≥ trailSevereVolume       = SeveritySevere
    | v ≥ trailModerateVolume     = SeverityModerate
    | otherwise                   = SeverityMinor

-- | Footprint scales with volume too — a light trickle doesn't paint as
--   wide a mark as a heavy bleed.
trailFootprint ∷ Float → FootprintBucket
trailFootprint v
    | v ≥ trailSevereVolume   = FootprintLarge
    | v ≥ trailModerateVolume = FootprintMedium
    | otherwise               = FootprintSmall

-- | Requirement 1's vocabulary: drops for a light mark, a short smear
--   once there's enough volume to read as one — never pool/spatter/
--   streak (those read as impact marks, not an ongoing trail).
trailStyleFor ∷ Float → BloodStyle
trailStyleFor v
    | v ≥ trailModerateVolume = StyleSmear
    | otherwise               = StyleDrops

-- | Opacity climbs with volume (0.3 floor so even the lightest mark is
--   visible), capped at 1 once volume reaches/exceeds the catastrophic
--   band.
trailOpacity ∷ Float → Float
trailOpacity v = clamp01 (0.3 + 0.7 * (v / trailCatastrophicVolume))

-- | Map one mark's aggregated external blood volume (litres) onto its
--   full texture-request weight.
trailBloodForVolume ∷ Float → TrailBlood
trailBloodForVolume v = TrailBlood
    { tbStyle     = trailStyleFor v
    , tbSeverity  = trailSeverityBucket v
    , tbFootprint = trailFootprint v
    , tbOpacity   = trailOpacity v
    }

-- ----- IO glue -----

-- | Place one trail-mark decal via the SAME 'Blood.Types.spawnDecal'
--   entry point 'Blood.Impact.spawnImpactBlood' uses — texture reuse,
--   caps, FIFO eviction, aging, and teardown all keep applying with no
--   new decal lifecycle. Silently a no-op if @page@ names a world that
--   isn't currently loaded (same policy as impact blood — issue #607
--   requirement 8's precedent).
spawnTrailMark
    ∷ EngineEnv
    → WorldPageId
    → Float           -- ^ gx
    → Float           -- ^ gy
    → Int             -- ^ surfaceZ
    → Text            -- ^ representative wound kind (texture-request tag only)
    → Float           -- ^ aggregated volume (litres) this mark represents
    → Float           -- ^ rotation, radians
    → Int             -- ^ seed
    → Maybe UnitId    -- ^ source unit
    → Double          -- ^ game time
    → IO ()
spawnTrailMark env page gx gy z kind volume rotation seed mSrc now = do
    wm ← readIORef (worldManagerRef env)
    case lookup page (wmWorlds wm) of
        Nothing → pure ()
        Just ws → do
            let tb = trailBloodForVolume volume
                req = BloodTextureRequest
                    { btrStyle      = tbStyle tb
                    , btrWoundKind  = kind
                    , btrSeverity   = tbSeverity tb
                    , btrFootprint  = tbFootprint tb
                    , btrAnisotropy = AnisotropyNone
                    , btrEdge       = EdgeModerate
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
                    , bspSeverity       = tbSeverity tb
                    , bspSourceUnit     = mSrc
                    , bspOpacity        = tbOpacity tb
                    }
            atomicModifyIORef' (wsBloodStoreRef ws) $ \store →
                let (store', _, _, _) = spawnDecal req mkSpec store
                in (store', ())
