{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-tick movement for units.
--
-- Continuous motion in ℝ². Each tick:
--   * If `usLocalPath` is non-empty, head toward its first waypoint.
--     Pop the waypoint on arrival.
--   * Otherwise (greedy mode), head toward `usTarget`.
--   * Before stepping into a new tile, ask `stepCost`:
--       Nothing                → replan
--       cost > replan threshold (greedy mode only) → replan
--       cost ≤ threshold       → step
--
-- Replan = `localAStar` from current tile to the final target's
-- tile, storing the result as continuous tile-center waypoints in
-- `usLocalPath`. If A* finds no path at all, the unit stays idle this
-- tick and tries again next tick — the "never gives up" rule.
module Unit.Thread.Movement
    ( tickAllMovement
    , UnitMoveStats(..)
    , startJump
    , jumpMaxTiles
    , maxJumpHeight
    , strikeReach
    , metresPerTile
    , lungeImpactSpeed
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Control.Monad (when, forM_)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Combat.Types (pushInjuryEvent)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import qualified System.Random as Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData(..))
import Unit.Sim.Types
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..), UnitDef(..)
                  , Wound(..))
import Unit.Pathing.Cost (stepCost, lookupTerrainZ, isCliffStep
                         , PathingConfig(..))
import Unit.Pathing.AStar (localAStar, defaultMaxRadius)
import Unit.Fall (FallInjury(..), fallInjuries, fallStunFor, gravity, metresPerZ)

-- | Per-unit movement stats relevant to climb/fall mechanics.
--   Snapshotted once per tick at the top of tickAllMovement so the
--   pure inner functions don't have to round-trip through the unit
--   manager.
data UnitMoveStats = UnitMoveStats
    { umsBodyMass  ∷ !Float   -- ^ kg
    , umsToughness ∷ !Float   -- ^ stat (1.0 = baseline)
    , umsClimbing  ∷ !Float   -- ^ skill 0..100
    , umsDexterity ∷ !Float   -- ^ stat (1.0 = baseline)
    , umsStrength  ∷ !Float   -- ^ stat (1.0 = baseline)
    , umsRunThreshold ∷ !Float -- ^ absolute speed (tiles/s) above which the
                               --   unit renders the Running anim instead of
                               --   Walking = def.run_threshold × def.max_speed.
    , umsHeight    ∷ !Float   -- ^ body height (metres). Drives climb reach:
                               --   how much of a cliff the unit mantles in the
                               --   pullup vs has to wall-climb first.
    }

defaultMoveStats ∷ UnitMoveStats
defaultMoveStats = UnitMoveStats
    { umsBodyMass  = 70.0
    , umsToughness = 1.0
    , umsClimbing  = 0.0
    , umsDexterity = 1.0
    , umsStrength  = 1.0
    , umsRunThreshold = 1.0e9  -- no def → never auto-run (sentinel high)
    , umsHeight    = 1.8       -- baseline human height
    }

-- | Metres of body height needed to mantle one full z-level. A unit's
--   climb "reach" (z-levels it can pull up onto without wall-climbing) is
--   height / this. Tuned so a baseline acolyte (1.8 m) reaches exactly
--   1 z — i.e. a 1-z cliff is handled entirely by the pullup, with no
--   vertical wall-climb, matching "acolytes are tall enough to climb a
--   1-z cliff and should go almost straight to the pullup".
heightPerClimbZ ∷ Float
heightPerClimbZ = 1.8

-- | How many z-levels the unit can mantle (pull up onto) given its height.
climbReachZ ∷ UnitMoveStats → Float
climbReachZ s = max 0.1 (umsHeight s / heightPerClimbZ)

-- | Distance below which the unit is considered arrived at a
--   waypoint or target. Larger than one tick of motion (≈ 0.066) so
--   the unit can't tick past a sub-goal and start oscillating.
arrivalEpsilon ∷ Float
arrivalEpsilon = 0.1

-- | Advance all units with active move targets, using cost-aware
--   greedy movement + local A* replan. World tile data is snapshotted
--   once per tick so all units in this batch see the same world.
tickAllMovement ∷ Double → EngineEnv → IORef UnitThreadState → IO ()
tickAllMovement dt env utsRef = do
    mWtd ← snapshotVisibleWorldTiles env
    now  ← readIORef (gameTimeRef env)
    -- Snapshot per-unit body_mass + toughness so the pure movement
    -- tick can compute fall impact without reading the unit manager.
    -- Defaults match human baseline (70 kg, toughness 1.0) — picked
    -- so a unit with no stats declared behaves like an unmodified
    -- acolyte against the impact thresholds.
    um ← readIORef (unitManagerRef env)
    -- Pathing cost tunables (climb/ramp/fall/river/lake penalties +
    -- replan threshold), loaded once from config/pathing.yaml. Read per
    -- tick so a future settings UI that mutates the ref takes effect
    -- live; the read is a single IORef deref.
    pc ← readIORef (pathingConfigRef env)
    let statsFor uid = case HM.lookup uid (umInstances um) of
            Just inst →
                -- Gait threshold (absolute tiles/s) from the unit's def:
                -- run_threshold × max_speed. No def → leave it effectively
                -- infinite so the unit never auto-runs.
                let (maxSp, runFrac) = case HM.lookup (uiDefName inst) (umDefs um) of
                        Just d  → (udMaxSpeed d, udRunThreshold d)
                        Nothing → (1.0e9, 0.6)
                in UnitMoveStats
                { umsBodyMass  = HM.lookupDefault 70.0 "body_mass"  (uiStats  inst)
                , umsToughness = HM.lookupDefault 1.0  "toughness"  (uiStats  inst)
                , umsClimbing  = HM.lookupDefault 0.0  "climbing"   (uiSkills inst)
                , umsDexterity = HM.lookupDefault 1.0  "dexterity"  (uiStats  inst)
                , umsStrength  = HM.lookupDefault 1.0  "strength"   (uiStats  inst)
                , umsRunThreshold = runFrac * maxSp
                , umsHeight    = HM.lookupDefault 1.8  "height"     (uiStats  inst)
                }
            Nothing → defaultMoveStats
    uts ← readIORef utsRef
    let simStates  = utsSimStates uts
        simStates' = HM.mapWithKey
                        (\uid ss → tickUnit pc now dt mWtd (statsFor uid) ss)
                        simStates

    -- Climb slip rolls + climb→fall conversions. Two passes that
    -- both need RNG / per-unit stats, so they live up here (tickUnit
    -- is pure and has neither).
    simStates'' ← rollClimbSlips env now statsFor simStates'
    let simStates''' = HM.mapWithKey (convertSlippedClimb now statsFor)
                                     simStates''

    -- Drain climbing XP from any unit that finished a climb this
    -- tick. Diminishing-returns curve: per z-level gain is
    -- base × (1 - skill/100)² so progression slows sharply as the
    -- skill matures. Skill caps at 100.
    let climbers = [ (uid, xp)
                   | (uid, ss) ← HM.toList simStates'''
                   , let xp = usPendingClimbXP ss
                   , xp > 0
                   ]
    when (not (null climbers)) $
        atomicModifyIORef' (unitManagerRef env) $ \um' →
            let bumped = foldr applyClimbXP (umInstances um') climbers
            in (um' { umInstances = bumped }, ())

    -- Apply physics-based fall injuries. A unit that just landed stamped
    -- its drop magnitude onto usPendingFallDrop (the pure tick has no
    -- body-part or substance data); here — where both, plus the unit
    -- manager, are in hand — run the Unit.Fall model to turn the drop into
    -- a SET of fracture/concussion wounds (energy from mass × g × h,
    -- per-part bone resistance vs delivered load), apply them, and size
    -- the knockdown stun from the worst injury. The combat wound tick then
    -- heals/bleeds them; the Lua injury tick reads them for impairment +
    -- death. Mirrors the climb-XP drain above.
    sm ← readIORef (substanceManagerRef env)
    let fallResults =
            [ (uid, injs, foldr (max . fiSeverity) 0 injs)
            | (uid, ss) ← HM.toList simStates'''
            , Just drop ← [usPendingFallDrop ss]
            , Just inst ← [HM.lookup uid (umInstances um)]
            , Just def  ← [HM.lookup (uiDefName inst) (umDefs um)]
            , let mass  = HM.lookupDefault 70.0 "body_mass" (uiStats inst)
                  tough = HM.lookupDefault 1.0  "toughness" (uiStats inst)
                  injs  = fallInjuries sm def mass tough drop
            ]
    when (not (null fallResults)) $
        atomicModifyIORef' (unitManagerRef env) $ \um' →
            let applyOne (uid, injs, _) m = case HM.lookup uid m of
                    Nothing → m
                    Just inst →
                        let ws = [ Wound { woundPart     = fiPart i
                                         , woundKind     = fiKind i
                                         , woundSeverity = fiSeverity i
                                         , woundAt       = now
                                         , woundBandage  = 1.0
                                         , woundClot     = 0.0
                                         , woundHeal     = 0.0
                                         , woundDressing = ""
                                         , woundInfection = 0.0
                                         , woundClean    = False
                                         , woundInfectionType = ""
                                         , woundNecrosis = 0.0 }
                                 | i ← injs ]
                        in HM.insert uid (inst { uiWounds = ws <> uiWounds inst }) m
            in (um' { umInstances = foldr applyOne (umInstances um') fallResults }, ())

    -- Feed the injury log: one event per fall that actually hurt someone,
    -- carrying a "detail" string (part:woundKind:sevPct|…) the injury-log
    -- prose turns into a clause list, plus the worst severity + count.
    forM_ fallResults $ \(uid, injs, worst) → when (not (null injs)) $
        let detail = T.intercalate "|"
                [ T.intercalate ":"
                    [ fiPart i, fiKind i
                    , T.pack (show (round (fiSeverity i * 100) ∷ Int)) ]
                | i ← injs ]
        in pushInjuryEvent (injuryEventsRef env) now (unUnitId uid) "fall"
             [ ("detail",   detail)
             , ("count",    T.pack (show (length injs)))
             , ("severity", T.pack (show worst)) ]

    -- Knockdown stun per landed unit, keyed for the sim writeback.
    let stunMap = HM.fromList [ (uid, fallStunFor worst)
                              | (uid, _, worst) ← fallResults ]
        setFall uid ss = case HM.lookup uid stunMap of
            Just stun → ss { usGetUpAt = Just (now + stun)
                           , usPendingFallDrop = Nothing }
            Nothing
              | usPendingFallDrop ss ≡ Nothing → ss
              | otherwise → ss { usPendingFallDrop = Nothing }  -- defensive

    -- Clear the pending fields on the snapshot we just wrote back.
    let simStates'''' = HM.mapWithKey
                          (\uid ss → setFall uid (clearPendingClimbXP ss))
                          simStates'''
    writeIORef utsRef (uts { utsSimStates = simStates'''' })

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
    ∷ Double → (UnitId → UnitMoveStats)
    → UnitId → UnitSimState → UnitSimState
convertSlippedClimb now statsFor uid ss =
    case (usState ss, usClimbSlipAt ss, usClimbFromTile ss) of
        (TransitioningTo Climbing, Just slipAt,
         Just (fromX, fromY, fromZ)) | now ≥ slipAt →
            let stats     = statsFor uid
                curRealZ  = usRealZ ss
                currentZ  = floor curRealZ ∷ Int
                dropZ     = currentZ - fromZ
                duration  = fallDuration dropZ
                dstX      = fromX
                dstY      = fromY
                impact    = computeFallImpact dropZ stats
            in ss { usState            = TransitioningTo Falling
                  , usTransitionUntil  = Just (now + duration)
                  , usTransitionStride = 1
                  , usFallFromTile     = Just (fromX, fromY, currentZ)
                  , usFallToTile       = Just (dstX, dstY, fromZ)
                  , usFallImpact       = Just impact
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

snapshotVisibleWorldTiles ∷ EngineEnv → IO (Maybe WorldTileData)
snapshotVisibleWorldTiles env = do
    wm ← readIORef (worldManagerRef env)
    case wmVisible wm of
        []          → pure Nothing
        (pageId:_)  → case lookup pageId (wmWorlds wm) of
            Nothing → pure Nothing
            Just ws → Just <$> readIORef (wsTilesRef ws)

tickUnit ∷ PathingConfig → Double → Double → Maybe WorldTileData
         → UnitMoveStats
         → UnitSimState → UnitSimState
tickUnit pc now dt mWtd stats us =
    let us1 = handleGetUp now
            $ handleTransitionExpiry now
            $ handlePickupExpiry now
            $ handleEatExpiry now
            $ handleDrinkExpiry now us
        -- Climb-Z interpolation (Phase 1, Standing→Climbing) +
        -- Fall-Z interpolation (Standing→Falling, descent path).
        -- Climb lerps usRealZ upward; fall lerps it downward. Both
        -- pin xy at the start position for the whole transition.
        -- handleTransitionExpiry handles the landing snaps + the
        -- post-fall outcome routing (walk / collapse / kill).
        us2 = tickPullup now stats (tickFallZ now (tickClimbZ now stats us1))
    in case usState us2 of
        -- Stationary anim states block movement.
        Drinking            → us2
        Eating              → us2
        Picking             → us2
        TransitioningTo _   → us2
        _ → case usTarget us2 of
            Nothing → us2
            Just mt →
                let subGoal = case usLocalPath us2 of
                        (p : _) → p
                        []      → (mtTargetX mt, mtTargetY mt)
                in stepTowardSubGoal pc now dt mWtd stats us2 mt subGoal

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

-- | Mirror of tickClimbZ for falling: usRealZ lerps DOWN from the
--   top of the cliff to the landing tile's Z while the unit is in
--   TransitioningTo Falling. usGridZ stays at the top until the
--   landing snap so game-logic tile lookups treat the unit as still
--   on the upper tile mid-fall.
tickFallZ ∷ Double → UnitSimState → UnitSimState
tickFallZ now us =
    case (usState us, usFallFromTile us, usFallToTile us,
          usTransitionUntil us) of
        (TransitioningTo Falling, Just (fx, fy, fromZ), Just (tx, ty, toZ), Just untilT) →
            let remain = max 0 (untilT - now)
                fromZF = fromIntegral fromZ ∷ Float
                toZF   = fromIntegral toZ   ∷ Float
            in case usJumpApex us of
                Just apex →
                    -- LEAP: a gravity arc (rise then fall) plus horizontal
                    -- interpolation across the gap. realZ peaks `apex` above
                    -- the chord midpoint (4·p·(1−p) is 1 at p=0.5), so it
                    -- launches up, sails over, and comes down onto the target.
                    let dur  = max 0.001 (jumpFlightTime apex)
                        p    = realToFrac (clamp01 (1 - realToFrac remain / dur)) ∷ Float
                        arcZ = fromZF + (toZF - fromZF) * p + apex * 4 * p * (1 - p)
                    in us { usRealX = fx + p * (tx - fx)
                          , usRealY = fy + p * (ty - fy)
                          , usRealZ = if remain ≤ 0 then toZF else arcZ }
                Nothing →
                    -- FALL: parabolic, gravity-accelerated descent (realZ
                    -- falls as ½·g·t² → progress² in normalised time), xy
                    -- pinned at the launch and snapped at landing.
                    let dur      = max 0.001
                                       (untilT - fallStartFromUntil untilT fromZ toZ)
                        progress = clamp01 (1 - realToFrac remain / dur)
                        accel    = realToFrac progress * realToFrac progress ∷ Float
                        lerp     = fromZF + accel * (toZF - fromZF)
                    in us { usRealZ = if remain ≤ 0 then toZF else lerp }
        _ → us
  where
    clamp01 ∷ Double → Double
    clamp01 x = max 0 (min 1 x)

fallStartFromUntil ∷ Double → Int → Int → Double
fallStartFromUntil untilT fromZ toZ =
    untilT - fallDuration (abs (toZ - fromZ))

-- | Climb ascent rate in tiles-of-Z per real-second. 0.3 → 3.33 s
--   per Z level. Tuned for "climbing is a real commitment" pacing.
climbSpeedZ ∷ Float
climbSpeedZ = 0.3

-- | Free-fall gravity in z-levels/s². Derived from the injury model's
--   constants (Unit.Fall.gravity m/s² ÷ metresPerZ) so the motion that
--   brings a unit down stays consistent with the impact energy the fall
--   injuries assume — one source of truth for "how hard gravity pulls."
fallGravityZ ∷ Float
fallGravityZ = gravity / metresPerZ

-- | Time (s) to free-fall `dropZ` z-levels from rest under gravity:
--   dropZ = ½·g·t² ⇒ t = √(2·dropZ/g). Replaces the old constant-rate
--   dropZ/fallSpeedZ — short falls start gently, tall falls build real
--   speed, and the descent matches the energy the injury model uses.
fallDuration ∷ Int → Double
fallDuration dropZ =
    realToFrac (sqrt (2 * fromIntegral (max 0 dropZ) / fallGravityZ))

-- | Metres per tile at UNIT scale (combat/leaps). Not the worldgen
--   tectonic 10 m/tile — at the sprite scale a tile reads as ~2-3 m, so
--   a 1-tile-gap leap looks athletic-but-possible. Lets the leap's
--   horizontal reach (tiles) be related to strike heights (metres).
metresPerTile ∷ Float
metresPerTile = 2.5

-- | Max horizontal leap distance (tiles) from the leap skill + stats,
--   PENALISED by body-fat fraction (power-to-weight: a fat unit launches
--   the same legs against more mass). The skill/stat split: LEARNED
--   `jumping` sets the base, agility/strength extend it, fat cuts it.
--   Tuned so only an athletic acolyte clears a 1-tile gap (~2-tile leap)
--   and a fat one falls short; a squirrel leaps well past it.
jumpMaxTiles ∷ Float → Float → Float → Float → Float
             -- jumping skill, agility, strength, fat fraction
jumpMaxTiles jumpSkill agility strength fatFrac =
    (1.8 + 1.2 * (jumpSkill / 100)
         + 1.0 * max 0 (agility - 1)
         + 0.4 * max 0 (strength - 1))
    * (1 - 0.6 * clamp01F fatFrac)

-- | Max vertical strike height (metres above foot) a unit can reach by
--   leaping STRAIGHT UP — the hard cap that stops a squirrel headshotting
--   a giraffe. Same skill/stat/fat blend as the distance; squirrel ~1.5 m,
--   athletic acolyte ~1.0 m, bear ~1.8 m.
maxJumpHeight ∷ Float → Float → Float → Float → Float
              -- jumping skill, agility, strength, fat fraction
maxJumpHeight jumpSkill agility strength fatFrac =
    (0.6 + 1.0 * (jumpSkill / 100)
         + 0.5 * max 0 (agility - 1)
         + 0.2 * max 0 (strength - 1))
    * (1 - 0.6 * clamp01F fatFrac)

-- | Highest a unit can land a strike at horizontal leap distance d
--   (tiles), given its max distance and max vertical reach: the reach
--   ENVELOPE. Full height at d=0 (a near, steep-pitched leap), tapering
--   to 0 at max distance (a long, flat leap stays low). Capped ≥ 0.
strikeReach ∷ Float → Float → Float → Float
            -- d (tiles), maxDist (tiles), maxHeight (m)
strikeReach d maxDist maxHeight
    | maxDist ≤ 0 = maxHeight
    | otherwise   = max 0 (maxHeight * (1 - (d / maxDist) * (d / maxDist)))

clamp01F ∷ Float → Float
clamp01F x = max 0 (min 1 x)

-- | Arc apex (z above launch) for a leap of horizontal distance d —
--   taller for longer leaps, clamped so the arc stays readable.
jumpApexFor ∷ Float → Float
jumpApexFor d = max 0.5 (min 2.0 (0.5 + 0.3 * d))

-- | Time of flight (s) for a symmetric leap reaching apex `a`: up then
--   down, each leg √(2a/g). Shares fallGravityZ so a leap and a fall
--   obey the exact same gravity.
jumpFlightTime ∷ Float → Double
jumpFlightTime a = realToFrac (2 * sqrt (2 * max 0.01 a / fallGravityZ))

-- | Impact speed (m/s) of a lunge — how fast the body is travelling when it
--   hits, used by combat to size the full-body momentum a lunge adds to its
--   strike. A real horizontal leap (d > 0): the horizontal travel speed
--   (distance ÷ flight time). An in-place vertical pounce (d ≈ 0): the
--   downward speed gained falling from the unit's max jump height
--   (√(2·g·h)). Both rise with how committed the leap is.
lungeImpactSpeed ∷ Float → Float → Float    -- distTiles, maxHeight (m)
lungeImpactSpeed distTiles maxHeight
    | distTiles ≤ 0.01 = sqrt (2 * gravity * max 0 maxHeight)
    | otherwise        =
        let t = max 0.05 (realToFrac (jumpFlightTime (jumpApexFor distTiles)))
        in distTiles * metresPerTile / t

-- | Set up a LEAP: a Standing→Falling arc transition from the unit's
--   current position to (tgx,tgy) at the same z (slice 1 = flat leaps).
--   usJumpApex is what makes tickFallZ arc up-then-down + lerp xy, and
--   what makes handleTransitionExpiry land the unit STANDING (a leap is
--   a controlled jump, not a fall). Reuses the falling/landing anims.
startJump ∷ Double → UnitSimState → Int → Int → UnitSimState
startJump now ss tgx tgy =
    let fromX = usRealX ss
        fromY = usRealY ss
        z     = usGridZ ss
        dstX  = fromIntegral tgx + 0.5
        dstY  = fromIntegral tgy + 0.5
        dx    = dstX - fromX
        dy    = dstY - fromY
        d     = sqrt (dx * dx + dy * dy)
        apex  = jumpApexFor d
        dur   = jumpFlightTime apex
    in ss { usState            = TransitioningTo Falling
          , usTransitionUntil  = Just (now + dur)
          , usTransitionStride = 1
          , usFacing           = vectorToDirection dx dy
          , usFallFromTile     = Just (fromX, fromY, z)
          , usFallToTile       = Just (dstX, dstY, z)
          , usFallImpact       = Nothing
          , usJumpApex         = Just apex
          , usPostTransition   = []
          , usTarget           = Nothing
          , usLocalPath        = []
          }

-- The Z drop magnitude at which a descent step stops being a "walk off
-- the edge" and becomes a real fall is now the configurable
-- `pcFallTriggerDrop` (config/pathing.yaml) — the same knob the cost
-- function uses, so the planner and the mover agree on what counts as a
-- fall. dz of -1 still plays the regular walking animation; dz of
-- -pcFallTriggerDrop or worse triggers the Standing→Falling transition.

-- | Legacy scalar fall impact (dz × mass/50 / toughness). The landing
--   OUTCOME no longer uses this — falls now go through the `Unit.Fall`
--   physics injury model (see tickAllMovement). Retained only because a
--   slip-converted climb still stamps `usFallImpact` for any future
--   consumer; the value is otherwise vestigial.
computeFallImpact ∷ Int → UnitMoveStats → Float
computeFallImpact dz stats =
    let absDz   = abs (fromIntegral dz) ∷ Float
        massN   = umsBodyMass stats / 50.0
        toughN  = max 0.1 (umsToughness stats)
    in absDz * massN / toughN

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

-- | Fall knockdown recovery. A non-lethal fall lands the unit in the
--   Collapsed pose with a self-timed `usGetUpAt`. Once the down/landing
--   transition has finished (usState back to Idle) and the timer is
--   reached, stand the unit up and clear the timer (one-shot).
--
--   This is INTENTIONALLY independent of the resource revive gate in
--   unit_resources.lua: a fall knockdown recovers on its own clock, not
--   when the unit happens to be ≥50% blood/stamina/hydration. If the
--   unit is also genuinely exhausted/thirsty/bleeding, the Lua survival
--   tick or the combat wound tick will re-collapse it next tick — but as
--   a SURVIVAL collapse (no getup timer), correctly resource-gated and
--   now legible in the status panel.
handleGetUp ∷ Double → UnitSimState → UnitSimState
handleGetUp now us = case (usPose us, usGetUpAt us, usState us) of
    (Collapsed, Just t, Idle) | now ≥ t →
        us { usPose = Standing, usState = Idle, usGetUpAt = Nothing }
    _ → us

-- | When a Drinking timer expires, snap back to Idle.
handleDrinkExpiry ∷ Double → UnitSimState → UnitSimState
handleDrinkExpiry now us = case usDrinkUntil us of
    Just t | usState us ≡ Drinking ∧ now ≥ t →
        us { usState = Idle, usDrinkUntil = Nothing }
    _ → us

-- | When an Eating timer expires, snap back to Idle.
handleEatExpiry ∷ Double → UnitSimState → UnitSimState
handleEatExpiry now us = case usEatUntil us of
    Just t | usState us ≡ Eating ∧ now ≥ t →
        us { usState = Idle, usEatUntil = Nothing }
    _ → us

-- | When a Picking timer expires, snap back to Idle.
handlePickupExpiry ∷ Double → UnitSimState → UnitSimState
handlePickupExpiry now us = case usPickupUntil us of
    Just t | usState us ≡ Picking ∧ now ≥ t →
        us { usState = Idle, usPickupUntil = Nothing }
    _ → us

-- | When a pose transition timer expires, commit the target pose.
--
--   If the unit has a chained transition queued in usPostTransition,
--   start the next one immediately — used by the climb sequence to
--   roll Climbing → Crawling → Standing in one shot without the AI
--   having to orchestrate it. AI-side logic remains responsible for
--   one-off chains (e.g. source-drinking unit transitions to
--   Crouching, drinks, then issues TransitionTo Standing).
--
--   Note: this is a pure state transition — UnitTransitionTo is what
--   reads the next anim's duration. We can't read anim data from a
--   pure tick. Solution: leave usTransitionUntil empty for the
--   chained step and let the AI / a follow-up command set it. For
--   the climb chain specifically, we set the duration here based on
--   the climb anim asset names: the durations are short enough
--   (~0.5–1 s for pull-up / stand-up) that we can pick a conservative
--   default and have UnitTransitionTo overwrite if called again.
handleTransitionExpiry ∷ Double → UnitSimState → UnitSimState
handleTransitionExpiry now us = case (usState us, usTransitionUntil us) of
    (TransitioningTo target, Just t) | now ≥ t →
        let -- Climb chain handoff:
            --
            --   * End of Climbing transition (target == Climbing):
            --     snap xy to the cliff edge (usClimbToTile's xy)
            --     and Z to the top. Tiny snap since edgeX/edgeY are
            --     within ~0.05 tiles of the climb's frozen base xy.
            --     Pullup + standup then play at this fixed anchor.
            --
            --   * End of Crawling transition (post-pullup): nothing
            --     to snap — xy is already at the cliff edge from
            --     the previous step. Clear climb fields here so the
            --     standup phase doesn't keep climbToTile around.
            wasClimbing = usPose us ≡ Climbing
            snapped = case (target, usClimbToTile us, usFallToTile us) of
                (Climbing, Just (_, _, tz), _) →
                    -- Wall-climb phase done. The unit is hanging at the
                    -- climb-top over the BASE xy; the pullup (Crawling)
                    -- phase slides it up + forward onto the ledge
                    -- (tickPullup). So DON'T snap xy/realZ to the ledge
                    -- here — leave them where the climb left them and
                    -- only commit the logical tile-z to the top. Stamp
                    -- the dz climbed onto the XP field tickAllMovement
                    -- drains into the "climbing" skill.
                    let dz = case usClimbFromTile us of
                            Just (_, _, fromZ) →
                                fromIntegral (abs (tz - fromZ)) ∷ Float
                            Nothing → 0
                    in us { usGridZ = tz
                          , usPendingClimbXP = usPendingClimbXP us + dz
                          }
                (Falling, _, Just (tx, ty, tz)) →
                    us { usRealX = tx, usRealY = ty
                       , usGridZ = tz, usRealZ = fromIntegral tz }
                _ → us
            -- Clear climb fields after the pullup (Crawling
            -- transition while wasClimbing). The Climbing transition
            -- end keeps them set in case any future tick needs them.
            clearClimb = case target of
                Crawling | wasClimbing → True
                _                      → False
            -- Fall landing. With the physics injury model, EVERY fall (a
            -- drop ≥ fallTriggerDz — a 1-z step never enters the fall
            -- transition) becomes a knockdown: play the Collapsed landing
            -- anim, then sit knocked down until usGetUpAt. The drop
            -- magnitude is stamped onto usPendingFallDrop for
            -- tickAllMovement, which runs Unit.Fall to turn it into a SET
            -- of fracture/concussion wounds and sizes the getup stun from
            -- the worst of them. Death is NOT decided here — a tall fall
            -- kills by inflicting a lethal (vital severity ≥1) injury that
            -- the Lua injury tick acts on (death emerges from injuries).
            -- A LEAP (usJumpApex set) lands on its feet: it shares the
            -- Falling transition + snap, but skips the fall's knockdown +
            -- injury and stands the unit straight back up.
            isLeap        = isJust (usJumpApex us)
            fellAndLanded = target ≡ Falling ∧ not isLeap
            leapLanded    = target ≡ Falling ∧ isLeap
            fallDrop = case (usFallFromTile us, usFallToTile us) of
                (Just (_, _, fz), Just (_, _, tz)) → fz - tz
                _                                  → 0
            (chainAfter, clearFall, pendingDrop)
              | fellAndLanded = ([Collapsed], True, Just fallDrop)
              -- A leap touches down then recovers to standing: hold the
              -- landing frame and chain Falling→Standing, which plays the
              -- `falling-to-standing` (landing) anim — same shape as a
              -- fall's Falling→Collapsed, but no knockdown/injury.
              | leapLanded    = ([Standing], True, Nothing)
              | otherwise     = ( usPostTransition snapped, False
                                , usPendingFallDrop snapped )
            landedPose
              | fellAndLanded = Falling   -- held on the collapse landing frame
              | leapLanded    = Falling   -- held on the landing frame; chain stands it up
              | otherwise     = target
            usPosed = snapped { usPose            = landedPose
                              , usState           = Idle
                              , usTransitionUntil = Nothing
                              , usClimbFromTile   = if clearClimb
                                                    then Nothing
                                                    else usClimbFromTile snapped
                              , usClimbToTile     = if clearClimb
                                                    then Nothing
                                                    else usClimbToTile snapped
                              , usClimbStartTime  = if clearClimb
                                                    then Nothing
                                                    else usClimbStartTime snapped
                              , usFallFromTile    = if clearFall
                                                    then Nothing
                                                    else usFallFromTile snapped
                              , usFallToTile      = if clearFall
                                                    then Nothing
                                                    else usFallToTile snapped
                              , usFallImpact      = if clearFall
                                                    then Nothing
                                                    else usFallImpact snapped
                              , usPostTransition  = chainAfter
                              -- Drop magnitude for tickAllMovement's injury
                              -- pass; usGetUpAt is set there once the worst
                              -- injury (and thus the stun) is known.
                              , usPendingFallDrop = pendingDrop
                              -- Leap is over once it lands (cleared whether
                              -- it was a leap or a fall).
                              , usJumpApex        = Nothing
                              }
        in case usPostTransition usPosed of
            (next : rest) →
                -- Start chained transition with a placeholder duration.
                -- A future enhancement is to look up anim duration via
                -- a snapshot — for the climb's pullup + stand-up steps,
                -- 0.8s each is a reasonable default until that lands.
                usPosed
                    { usState            = TransitioningTo next
                    , usTransitionUntil  = Just (now + chainStepDuration)
                    , usPostTransition   = rest
                    }
            [] → usPosed
    _ → us

-- | Default duration for a chained pose transition fired from
--   handleTransitionExpiry. The pure tick path doesn't have anim
--   data on hand, so we pick a value that matches the typical
--   pullup / stand-up pace. Acolyte's climb_pullup and the reversed
--   standing_to_crawling are both ~0.8 s, so 0.8 s is on the nose.
chainStepDuration ∷ Double
chainStepDuration = 0.8

-- | Try to advance toward `subGoal`. If we arrive, pop the waypoint
--   (or clear the final target). Otherwise, take one step.
stepTowardSubGoal
    ∷ PathingConfig
    → Double
    → Double
    → Maybe WorldTileData
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → UnitSimState
stepTowardSubGoal pc now dt mWtd stats us mt (gx, gy) =
    let dx   = gx - usRealX us
        dy   = gy - usRealY us
        dist = sqrt (dx * dx + dy * dy)
        -- A crawling unit (legs maimed) is capped to a crawl regardless of
        -- the commanded speed — it drags itself along the ground.
        effSpeed = if usPose us ≡ Crawling
                   then min (mtSpeed mt) crawlSpeed
                   else mtSpeed mt
        step = effSpeed * realToFrac dt
    in if dist ≤ max step arrivalEpsilon
       then arriveAtSubGoal stats us mt (gx, gy) mWtd
       else moveToward pc now stats us mt mWtd dx dy dist step

-- | Top speed (tiles/sec) of a unit dragging itself along on a maimed
--   body. Slow enough to read as a crawl; the injury speed-multiplier
--   already applied at command time stacks on top.
crawlSpeed ∷ Float
crawlSpeed = 0.7

-- | Snap to the sub-goal. If we arrived at the final target (no more
--   waypoints, sub-goal is the target), clear the target. Otherwise
--   pop the first waypoint and continue next tick.
arriveAtSubGoal
    ∷ UnitMoveStats
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → Maybe WorldTileData
    → UnitSimState
arriveAtSubGoal stats us mt (gx, gy) mWtd =
    let z   = lookupZ mWtd (floor gx) (floor gy) (usGridZ us)
        us' = us { usRealX = gx, usRealY = gy
                 , usGridZ = z, usRealZ = fromIntegral z }
    in case usLocalPath us' of
        (_ : rest) →
            -- Popped a waypoint. If there are more, continue along the
            -- path; otherwise resume greedy heading toward the final
            -- target (unless we're already there).
            let arrivedAtFinal =
                    abs (gx - mtTargetX mt) < arrivalEpsilon
                    ∧ abs (gy - mtTargetY mt) < arrivalEpsilon
            in if null rest ∧ arrivedAtFinal
               then us' { usLocalPath = []
                        , usTarget    = Nothing
                        , usState     = Idle
                        }
               else us' { usLocalPath = rest
                        , usState     = gaitForPose (usPose us') stats mt
                        }
        [] →
            -- Greedy mode: subGoal was the final target, so we've arrived.
            us' { usTarget = Nothing, usState = Idle }

-- | Step one tick toward the sub-goal. Cost-check first; on block or
--   high-cost (greedy mode only) trigger replan. If the next tile
--   crossing is a cliff (Z step that has no slope ramp), initiate a
--   climb transition instead of taking the step.
moveToward
    ∷ PathingConfig
    → Double             -- now (game time, for transition expiry)
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → Float    -- dx
    → Float    -- dy
    → Float    -- distance to sub-goal
    → Float    -- step length this tick
    → UnitSimState
moveToward pc now stats us mt mWtd dx dy dist step =
    let nx   = dx / dist
        ny   = dy / dist
        newX = usRealX us + nx * step
        newY = usRealY us + ny * step
        srcTile = (floor (usRealX us), floor (usRealY us))
        dstTile = (floor newX, floor newY)
        -- stepCost enforces the no-corner-cutting rule itself (a
        -- diagonal step grazing an impassable axis-neighbour returns
        -- Nothing), so the greedy stepper and A* agree by construction.
        mCost
            | srcTile ≡ dstTile = Just 0  -- sub-tile motion, no boundary cross
            | otherwise = case mWtd of
                Just wtd → stepCost pc wtd srcTile dstTile
                Nothing  → Just 0  -- no world snapshot: don't block
        followingPath = not (null (usLocalPath us))
        -- Cliff and fall detection: only meaningful when actually
        -- crossing a tile boundary. The pathfinder already rejects
        -- most cliffs via replanCostThreshold, but when the unit
        -- must climb (or drop into a fall) there's no alternative.
        --
        --   * mCliff: dz > 0 + no walkable slope ⇒ start climb.
        --   * mFall:  dz ≤ -fallTriggerDz       ⇒ start fall. Smaller
        --             drops (dz = -1) walk off normally; the engine's
        --             usual Z-snap path handles them silently.
        mCliff = case mWtd of
            Just wtd | srcTile ≢ dstTile →
                case (lookupTerrainZ wtd (fst srcTile) (snd srcTile),
                      lookupTerrainZ wtd (fst dstTile) (snd dstTile)) of
                    (Just sz, Just dz)
                        | isCliffStep wtd srcTile dstTile sz dz →
                            Just (sz, dz)
                    _ → Nothing
            _ → Nothing
        mFall = case mWtd of
            Just wtd | srcTile ≢ dstTile →
                case (lookupTerrainZ wtd (fst srcTile) (snd srcTile),
                      lookupTerrainZ wtd (fst dstTile) (snd dstTile)) of
                    (Just sz, Just dz)
                        | sz - dz ≥ pcFallTriggerDrop pc → Just (sz, dz)
                    _ → Nothing
            _ → Nothing
    in case mCost of
        Nothing →
            replan pc us mt mWtd srcTile
        Just c | not followingPath ∧ c > pcReplanCostThreshold pc →
            replan pc us mt mWtd srcTile
        Just _ → case (mCliff, mFall) of
            (Just (srcZ, dstZ), _) →
                -- Face the CLIFF, not the unit's walking sub-step.
                -- A unit angling into the cliff (e.g. mostly east,
                -- a bit south) would otherwise face DirS while
                -- climbing an east-facing cliff — the climb anim
                -- would render perpendicular. The cliff direction
                -- is the tile-grid delta from source to dest tile.
                let (sx, sy)   = srcTile
                    (dgx, dgy) = dstTile
                    cliffDx    = fromIntegral (dgx - sx) ∷ Float
                    cliffDy    = fromIntegral (dgy - sy) ∷ Float
                in startClimb now stats us (dstTile, dstZ) srcZ
                              (cliffDx, cliffDy)
            (_, Just (srcZ, dstZ)) →
                -- Fall: same facing logic as climb, but the unit
                -- launches into the air rather than grabbing rock.
                let (sx, sy)   = srcTile
                    (dgx, dgy) = dstTile
                    fallDx     = fromIntegral (dgx - sx) ∷ Float
                    fallDy     = fromIntegral (dgy - sy) ∷ Float
                in startFall now stats us (dstTile, dstZ) srcZ
                             (fallDx, fallDy)
            _ →
                let (dgx, dgy) = dstTile
                    newZ       = lookupZ mWtd dgx dgy (usGridZ us)
                in us { usRealX  = newX
                      , usRealY  = newY
                      , usGridZ  = newZ
                      , usRealZ  = fromIntegral newZ
                      , usFacing = vectorToDirection nx ny
                      , usState  = gaitForPose (usPose us) stats mt
                      }

-- | Walking vs Running gait, by whether the commanded speed crosses the
--   unit's run-anim threshold (def.run_threshold × def.max_speed). This
--   is the fix for "units never run": the per-tick movement update used
--   to hard-code Walking, clobbering the Running activity set at command
--   time. Now the gait is re-derived from speed every tick so it sticks.
gaitFor ∷ UnitMoveStats → MoveTarget → UnitActivity
gaitFor stats mt
    | mtSpeed mt > umsRunThreshold stats = Running
    | otherwise                          = Walking

-- | Gait that respects the pose: a Crawling unit is always Walking-gait
--   (there is only a crawling-walk anim, no crawling-run), so a unit told
--   to move fast still renders crawling rather than falling back.
gaitForPose ∷ Pose → UnitMoveStats → MoveTarget → UnitActivity
gaitForPose Crawling _ _ = Walking
gaitForPose _ stats mt   = gaitFor stats mt

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

-- | Initiate a fall sequence. Stops xy movement, faces the cliff
--   the unit's about to drop off, and starts a Standing→Falling
--   transition whose duration scales with the drop distance.
--   handleTransitionExpiry consumes usFallImpact to pick the
--   landing outcome (walk, collapse, or kill).
startFall
    ∷ Double              -- now
    → UnitMoveStats
    → UnitSimState
    → ((Int, Int), Int)   -- dst tile (gx, gy) + dst Z (bottom)
    → Int                 -- src Z (top of the fall)
    → (Float, Float)      -- step direction (for facing)
    → UnitSimState
startFall now stats us ((dgx, dgy), dstZ) srcZ (nx, ny) =
    let dz       = srcZ - dstZ                 -- positive: drop magnitude
        duration = fallDuration dz
        srcX     = usRealX us
        srcY     = usRealY us
        -- Land on the destination tile center. Falls don't have the
        -- "land on the edge" semantic that climbs do — by the time
        -- you've hit the ground, you've cleared the cliff face.
        dstX     = fromIntegral dgx + 0.5
        dstY     = fromIntegral dgy + 0.5
        impact   = computeFallImpact dz stats
    in us { usState            = TransitioningTo Falling
          , usTransitionUntil  = Just (now + duration)
          , usTransitionStride = 1
          , usFacing           = vectorToDirection nx ny
          , usFallFromTile     = Just (srcX, srcY, srcZ)
          , usFallToTile       = Just (dstX, dstY, dstZ)
          , usFallImpact       = Just impact
          , usJumpApex         = Nothing   -- a fall, not a leap
          -- usPostTransition stays empty here; the landing outcome
          -- (set in handleTransitionExpiry based on impact) decides
          -- whether to chain into Collapsed/Standing/Dead.
          , usPostTransition   = []
          -- Clear any movement target the fall interrupted. The AI
          -- can re-issue after recovery. Without this, a unit that
          -- survives a fall would immediately try to walk back to
          -- its old goal mid-collapse anim.
          , usTarget           = Nothing
          , usLocalPath        = []
          }

-- | Run local A* from `srcTile` to the final target's tile. Store
--   the resulting waypoints (in tile-center continuous coords) in
--   `usLocalPath`. If A* makes no progress (empty path), the unit
--   sits this tick — `usTarget` is preserved so the next tick can
--   try again ("never gives up").
replan
    ∷ PathingConfig
    → UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → (Int, Int)
    → UnitSimState
replan pc us mt mWtd srcTile =
    let finalTile = (floor (mtTargetX mt), floor (mtTargetY mt))
        tilePath = case mWtd of
            Just wtd → localAStar pc wtd srcTile finalTile defaultMaxRadius
            Nothing  → []
        wps = map tileCenter tilePath
    in if null wps
       then us { usLocalPath = [], usState = Idle }
       else us { usLocalPath = wps, usState  = Walking }

tileCenter ∷ (Int, Int) → (Float, Float)
tileCenter (gx, gy) = (fromIntegral gx + 0.5, fromIntegral gy + 0.5)

lookupZ ∷ Maybe WorldTileData → Int → Int → Int → Int
lookupZ mWtd gx gy fallback = case mWtd of
    Just wtd → case lookupTerrainZ wtd gx gy of
        Just z  → z
        Nothing → fallback
    Nothing → fallback

-- | Map a tile-grid velocity vector to one of 8 compass directions.
--
-- The sprite labels are aligned with the iso projection's compass, not
-- the raw grid axes:
--   DirS  = tile-grid (+x, +y)  — projects to screen-down
--   DirSE = tile-grid (+x,  0)  — projects to screen-lower-right
--   DirE  = tile-grid (+x, -y)  — projects to screen-right
--   DirNE = tile-grid ( 0, -y)
--   DirN  = tile-grid (-x, -y)
--   DirNW = tile-grid (-x,  0)
--   DirW  = tile-grid (-x, +y)
--   DirSW = tile-grid ( 0, +y)
-- That makes the result in tile-grid frame, which is what the renderer
-- expects — `Unit.Render.resolveTexture` then applies `cameraRotSteps`
-- to keep the on-screen direction consistent through camera rotations.
vectorToDirection ∷ Float → Float → Direction
vectorToDirection nx ny
    | norm < 22.5   = DirSE
    | norm < 67.5   = DirS
    | norm < 112.5  = DirSW
    | norm < 157.5  = DirW
    | norm < 202.5  = DirNW
    | norm < 247.5  = DirN
    | norm < 292.5  = DirNE
    | norm < 337.5  = DirE
    | otherwise     = DirSE
  where
    deg  = atan2 ny nx * (180.0 / pi)
    norm = if deg < 0 then deg + 360 else deg
