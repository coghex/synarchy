{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Climb mechanics: wall-climb + pullup Z/xy interpolation, per-unit
--   climb speed from stats, slip-chance rolls that can convert an
--   in-progress climb into a fall, climbing-skill XP drain, and
--   initiating a climb off a cliff.
module Unit.Thread.Movement.Climb
    ( tickClimbZ
    , tickPullup
    , startClimb
    , rollClimbSlips
    , convertSlippedClimb
    , applyClimbXP
    , clearPendingClimbXP
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (atomicModifyIORef')
import qualified System.Random as Random
import Engine.Core.State (EngineEnv(..))
import Unit.Types (UnitId(..), UnitInstance(..))
import Unit.Sim.Types
import Unit.Thread.Movement.Types
    (UnitMoveStats(..), baselineUnitHeight, vectorToDirection)
import Unit.Thread.Movement.Fall (fallDuration)
import Unit.Thread.Movement.Timers (chainStepDuration)

-- | Metres of body height needed to mantle one full z-level. A unit's
--   climb "reach" (z-levels it can pull up onto without wall-climbing) is
--   height / this. EQUAL to 'baselineUnitHeight' by design — a baseline
--   acolyte (1.8 m) reaches exactly 1 z, i.e. a 1-z cliff is handled
--   entirely by the pullup, with no vertical wall-climb, matching
--   "acolytes are tall enough to climb a 1-z cliff and should go almost
--   straight to the pullup".
heightPerClimbZ ∷ Float
heightPerClimbZ = baselineUnitHeight

-- | How many z-levels the unit can mantle (pull up onto) given its height.
climbReachZ ∷ UnitMoveStats → Float
climbReachZ s = max 0.1 (umsHeight s / heightPerClimbZ)

-- | Per-z-level slip chance. Probability rises sharply with body
--   weight relative to strength × dexterity, and drops sharply with
--   climbing skill (squared so high skill effectively eliminates
--   slips). Clamped so even a perfect climber has a non-zero risk
--   and a terrible one isn't guaranteed to fall every z.
--
--     base        = 0.05
--     skill_mod   = (1 - skill/100)²
--     control_mod = clamp(1.5 / (dex × str), 0.1, 5)
--     weight_mod  = max(1, mass/70)
--     per_z       = base × skill_mod × control_mod × weight_mod
--     clamp [0.001, 0.5]
slipChancePerZ ∷ UnitMoveStats → Float
slipChancePerZ s =
    let base       = 0.05
        skillMod   = (1 - umsClimbing s / 100.0) ** 2
        dxs        = max 0.05 (umsDexterity s * umsStrength s)
        controlMod = max 0.1 (min 5.0 (1.5 / dxs))
        weightMod  = max 1.0 (umsBodyMass s / 70.0)
        raw        = base * skillMod * controlMod * weightMod
    in max 0.001 (min 0.5 raw)

-- | For each unit that just started a climb this tick (usClimbStartTime
--   == Just now, usClimbSlipAt still Nothing), roll the slip dice. If
--   any z-level fails, schedule the slip at a uniformly random time
--   between the climb's start and end. Uses the engine's stat RNG so
--   the rolls are reproducible per seed and don't race with combat.
rollClimbSlips
    ∷ EngineEnv → Double
    → (UnitId → UnitMoveStats)
    → HM.HashMap UnitId UnitSimState
    → IO (HM.HashMap UnitId UnitSimState)
rollClimbSlips env now statsFor sim = do
    -- Snapshot newly-started climbs that haven't been rolled yet.
    let newClimbers =
            [ (uid, ss)
            | (uid, ss) ← HM.toList sim
            , usClimbStartTime ss ≡ Just now
            , usClimbSlipAt ss ≡ Nothing
            ]
    if null newClimbers
        then return sim
        else atomicModifyIORef' (statRNGRef env) $ \rng0 →
            let (rng', rolled) = foldl (rollOne now statsFor) (rng0, sim) newClimbers
            in (rng', rolled)
  where
    rollOne nowT sf (rng, accMap) (uid, ss) =
        let stats   = sf uid
            fromZ   = case usClimbFromTile ss of
                        Just (_, _, z) → z
                        Nothing        → 0
            toZ     = case usClimbToTile ss of
                        Just (_, _, z) → z
                        Nothing        → 0
            dz      = abs (toZ - fromZ)
            untilT  = case usTransitionUntil ss of
                        Just t  → t
                        Nothing → nowT
            perZ    = slipChancePerZ stats
            -- Pr(no slip over the whole climb) = (1 - perZ)^dz.
            -- So Pr(at least one slip) = 1 - (1 - perZ)^dz.
            failProb = 1 - (1 - perZ) ** fromIntegral dz
            (roll, rng1) = Random.uniformR (0.0 ∷ Float, 1.0) rng
        in if roll ≥ failProb
            then (rng1, accMap)
            else
                let (frac, rng2) = Random.uniformR (0.0 ∷ Double, 1.0) rng1
                    slipAt       = nowT + frac * (untilT - nowT)
                    ss'          = ss { usClimbSlipAt = Just slipAt }
                in (rng2, HM.insert uid ss' accMap)

-- | If a unit's slip time has been reached while it's still mid-climb,
--   transform the active Climbing transition into a Falling transition
--   from its current usRealZ down to the climb's origin tile.
convertSlippedClimb
    ∷ Double → UnitId → UnitSimState → UnitSimState
convertSlippedClimb now _uid ss =
    case (usState ss, usClimbSlipAt ss, usClimbFromTile ss) of
        (TransitioningTo Climbing, Just slipAt,
         Just (fromX, fromY, fromZ)) | now ≥ slipAt →
            let curRealZ  = usRealZ ss
                currentZ  = floor curRealZ ∷ Int
                dropZ     = currentZ - fromZ
                duration  = fallDuration dropZ
                dstX      = fromX
                dstY      = fromY
            in ss { usState            = TransitioningTo Falling
                  , usTransitionUntil  = Just (now + duration)
                  , usTransitionStride = 1
                  , usFallFromTile     = Just (fromX, fromY, currentZ)
                  , usFallToTile       = Just (dstX, dstY, fromZ)
                  , usJumpApex         = Nothing   -- a fall, not a leap
                  -- Cancel the climb-chain follow-ups; the unit isn't
                  -- going to crawl + stand after a fall.
                  , usPostTransition   = []
                  -- Clear climb state — the climb is over.
                  , usClimbFromTile    = Nothing
                  , usClimbToTile      = Nothing
                  , usClimbStartTime   = Nothing
                  , usClimbSlipAt      = Nothing
                  , usPendingClimbXP   = 0
                  }
        _ → ss

-- | Apply a climb-XP bump to one unit's "climbing" skill using
--   diminishing returns: delta = base × (1 - skill/100)² × dz.
--   New skill clamped to [0, 100].
applyClimbXP
    ∷ (UnitId, Float)
    → HM.HashMap UnitId UnitInstance
    → HM.HashMap UnitId UnitInstance
applyClimbXP (uid, dz) insts = case HM.lookup uid insts of
    Nothing → insts
    Just inst →
        let cur      = HM.lookupDefault 0.0 "climbing" (uiSkills inst)
            growthMul = (1 - cur / 100.0) ** 2
            delta    = climbXPBase * dz * growthMul
            new      = min 100.0 (max 0.0 (cur + delta))
            inst'    = inst { uiSkills = HM.insert "climbing" new
                                                   (uiSkills inst) }
        in HM.insert uid inst' insts

-- | Per-z-level base gain (before the diminishing-returns multiplier).
--   At skill 0: +0.5 per z. At skill 50: +0.125 per z. At skill 90:
--   +0.005 per z. Climbing becomes a meaningful long-term investment.
climbXPBase ∷ Float
climbXPBase = 0.5

clearPendingClimbXP ∷ UnitSimState → UnitSimState
clearPendingClimbXP ss = ss { usPendingClimbXP = 0 }

-- | While the unit is in TransitioningTo Climbing, lerp usRealZ up the
--   WALL portion of the cliff only — from the base z to the climb-top
--   (dstZ − pullup reach), with xy frozen at the base. The remaining
--   `pullupZ` is covered by the pullup (Crawling) phase via tickPullup,
--   which also slides the unit forward onto the ledge. A unit whose
--   reach ≥ the cliff height has a zero-length wall climb and goes
--   straight to the pullup. usGridZ stays at fromZ until the climb
--   commits the top in handleTransitionExpiry.
tickClimbZ ∷ Double → UnitMoveStats → UnitSimState → UnitSimState
tickClimbZ now stats us =
    case (usState us, usClimbFromTile us, usClimbToTile us,
          usTransitionUntil us, usClimbStartTime us) of
        (TransitioningTo Climbing, Just (_, _, fromZ),
         Just (_, _, toZ), Just untilT, Just startT) →
            let dz        = fromIntegral (toZ - fromZ) ∷ Float
                pullupZ   = min dz (climbReachZ stats)
                climbTopZ = fromIntegral toZ - pullupZ
                dur       = max 0.001 (untilT - startT)
                remain    = max 0 (untilT - now)
                progress  = clamp01 (1 - realToFrac remain / dur)
                fromZF    = fromIntegral fromZ ∷ Float
                lerp      = fromZF + realToFrac progress * (climbTopZ - fromZF)
            in us { usRealZ = if remain ≤ 0 then climbTopZ else lerp }
        _ → us
  where
    clamp01 ∷ Double → Double
    clamp01 x = max 0 (min 1 x)

-- | The pullup. During TransitioningTo Crawling on an active climb (climb
--   fields still set), slide the unit UP and FORWARD: usRealZ from the
--   climb-top to the ledge z, and usRealX/usRealY from the frozen base
--   to the ledge anchor (usClimbToTile's xy). This is the mantle motion
--   — without it the unit teleported onto the ledge and the pullup anim
--   played in place. Standing (stand-up) plays in place afterward, so
--   only the Crawling step interpolates.
tickPullup ∷ Double → UnitMoveStats → UnitSimState → UnitSimState
tickPullup now stats us =
    case (usState us, usClimbFromTile us, usClimbToTile us,
          usTransitionUntil us) of
        (TransitioningTo Crawling, Just (fx, fy, fromZ),
         Just (tx, ty, toZ), Just untilT) →
            let dz        = fromIntegral (toZ - fromZ) ∷ Float
                pullupZ   = min dz (climbReachZ stats)
                climbTopZ = fromIntegral toZ - pullupZ
                remain    = max 0 (untilT - now)
                progress  = clamp01 (1 - realToFrac remain / chainStepDuration)
                p         = realToFrac progress ∷ Float
                toZF      = fromIntegral toZ ∷ Float
            in us { usRealX = fx + p * (tx - fx)
                  , usRealY = fy + p * (ty - fy)
                  , usRealZ = climbTopZ + p * (toZF - climbTopZ)
                  }
        _ → us
  where
    clamp01 ∷ Double → Double
    clamp01 x = max 0 (min 1 x)

-- | Climb ascent rate in tiles-of-Z per real-second. 0.3 → 3.33 s
--   per Z level. Tuned for "climbing is a real commitment" pacing.
climbSpeedZ ∷ Float
climbSpeedZ = 0.3

-- | Per-unit climb speed in z-tiles per second. Replaces the bare
--   climbSpeedZ constant. Formula:
--
--     speed = climbSpeedZ
--           × (1 + skill/200)            -- trained climbers up to 1.5×
--           × sqrt(strength / (mass/70)) -- strength-to-weight ratio
--           × dexterity                  -- fine-motor control
--
--   Then clamped to [climbSpeedZ × 0.3, climbSpeedZ × 2.5] so extreme
--   stat combinations don't trivialise or break the climb.
--
--   At baseline acolyte stats (skill 10, dex 1.0, str 1.0, mass 70):
--   multiplier ≈ 1.05. At baseline bear (skill 0, dex 0.5, str 9.0,
--   mass 143): multiplier ≈ 1.05 (strength offsets weight; dex drags).
--   So a healthy bear and acolyte climb at about the same rate, while
--   high-skill / high-dex specialists pull ahead substantially.
computeClimbSpeed ∷ UnitMoveStats → Float
computeClimbSpeed s =
    let skillBonus  = 1.0 + umsClimbing s / 200.0
        massRel     = max 0.1 (umsBodyMass s / 70.0)
        strRatio    = sqrt (max 0.05 (umsStrength s / massRel))
        dexFactor   = max 0.2 (umsDexterity s)
        raw         = climbSpeedZ * skillBonus * strRatio * dexFactor
        lo          = climbSpeedZ * 0.3
        hi          = climbSpeedZ * 2.5
    in max lo (min hi raw)

-- | Initiate a climb sequence: stop horizontal movement, face the
--   cliff, snap into a Climbing transition whose duration scales
--   with the Z delta. Queue Crawling and Standing as the post-climb
--   chain so handleTransitionExpiry rolls the unit through the
--   pullup and stand-up animations without AI orchestration.
startClimb
    ∷ Double              -- now
    → UnitMoveStats
    → UnitSimState
    → ((Int, Int), Int)   -- dst tile (gx, gy) + dst Z
    → Int                 -- src Z (where the climb starts)
    → (Float, Float)      -- step direction (for facing)
    → UnitSimState
startClimb now stats us ((dgx, dgy), dstZ) srcZ (nx, ny) =
    let dz       = dstZ - srcZ
        -- Only the WALL portion (cliff height minus the unit's mantle
        -- reach) is a vertical wall-climb; the rest is the pullup. A
        -- unit whose reach ≥ the cliff height has a zero-length climb
        -- and goes straight to the pullup next tick.
        pullupZ  = min (fromIntegral dz) (climbReachZ stats)
        climbDz  = max 0 (fromIntegral dz - pullupZ)
        speed    = computeClimbSpeed stats
        duration = realToFrac (climbDz / speed) ∷ Double
        srcX     = usRealX us
        srcY     = usRealY us
        (sx, sy) = (floor srcX ∷ Int, floor srcY ∷ Int)
        -- Pullup anchor = just PAST the climbed edge, by `ledgeInset`,
        -- on the destination tile. The pullup (Crawling) phase
        -- interpolates the unit from its frozen base xy to here, so the
        -- inset controls how far forward the mantle hauls — small enough
        -- to read as "pulled up onto the lip", not "leapt half a tile
        -- inward". Measured from the NEAR (climbed) edge so east- and
        -- west-facing cliffs are symmetric. floor() resolves to the
        -- destination tile either way, so the logical tile/z stay
        -- consistent (no climb loop).
        ledgeInset = 0.125
        edgeX
            | dgx > sx  = fromIntegral dgx + ledgeInset        -- climb east
            | dgx < sx  = fromIntegral (dgx + 1) - ledgeInset  -- climb west
            | otherwise = srcX                                 -- cardinal y-only
        edgeY
            | dgy > sy  = fromIntegral dgy + ledgeInset        -- climb south
            | dgy < sy  = fromIntegral (dgy + 1) - ledgeInset  -- climb north
            | otherwise = srcY                                 -- cardinal x-only
    in us { usState            = TransitioningTo Climbing
          , usTransitionUntil  = Just (now + duration)
          , usTransitionStride = 1
          , usFacing           = vectorToDirection nx ny
          , usClimbFromTile    = Just (srcX, srcY, srcZ)
          , usClimbToTile      = Just (edgeX, edgeY, dstZ)
          , usClimbStartTime   = Just now
          -- After Climbing transition completes, auto-chain to
          -- Crawling (pullup over the edge) then Standing (stand
          -- up from crawling). handleTransitionExpiry consumes
          -- this queue.
          , usPostTransition   = [Crawling, Standing]
          }
