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
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import qualified System.Random as Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData(..))
import Unit.Sim.Types
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..))
import Unit.Pathing.Cost (stepCost, lookupTerrainZ, isCliffStep, replanCostThreshold)
import Unit.Pathing.AStar (localAStar, defaultMaxRadius)

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
    }

defaultMoveStats ∷ UnitMoveStats
defaultMoveStats = UnitMoveStats
    { umsBodyMass  = 70.0
    , umsToughness = 1.0
    , umsClimbing  = 0.0
    , umsDexterity = 1.0
    , umsStrength  = 1.0
    }

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
    let statsFor uid = case HM.lookup uid (umInstances um) of
            Just inst → UnitMoveStats
                { umsBodyMass  = HM.lookupDefault 70.0 "body_mass"  (uiStats  inst)
                , umsToughness = HM.lookupDefault 1.0  "toughness"  (uiStats  inst)
                , umsClimbing  = HM.lookupDefault 0.0  "climbing"   (uiSkills inst)
                , umsDexterity = HM.lookupDefault 1.0  "dexterity"  (uiStats  inst)
                , umsStrength  = HM.lookupDefault 1.0  "strength"   (uiStats  inst)
                }
            Nothing → defaultMoveStats
    uts ← readIORef utsRef
    let simStates  = utsSimStates uts
        simStates' = HM.mapWithKey
                        (\uid ss → tickUnit now dt mWtd (statsFor uid) ss)
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
    -- Clear the pending field on the snapshot we just wrote back.
    let simStates'''' = HM.map clearPendingClimbXP simStates'''
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
                duration  = realToFrac (fromIntegral dropZ / fallSpeedZ)
                            ∷ Double
                dstX      = fromX
                dstY      = fromY
                impact    = computeFallImpact dropZ stats
            in ss { usState            = TransitioningTo Falling
                  , usTransitionUntil  = Just (now + duration)
                  , usTransitionStride = 1
                  , usFallFromTile     = Just (fromX, fromY, currentZ)
                  , usFallToTile       = Just (dstX, dstY, fromZ)
                  , usFallImpact       = Just impact
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

tickUnit ∷ Double → Double → Maybe WorldTileData
         → UnitMoveStats
         → UnitSimState → UnitSimState
tickUnit now dt mWtd stats us =
    let us1 = handleTransitionExpiry now
            $ handlePickupExpiry now
            $ handleEatExpiry now
            $ handleDrinkExpiry now us
        -- Climb-Z interpolation (Phase 1, Standing→Climbing) +
        -- Fall-Z interpolation (Standing→Falling, descent path).
        -- Climb lerps usRealZ upward; fall lerps it downward. Both
        -- pin xy at the start position for the whole transition.
        -- handleTransitionExpiry handles the landing snaps + the
        -- post-fall outcome routing (walk / collapse / kill).
        us2 = tickFallZ now (tickClimbZ now us1)
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
                in stepTowardSubGoal now dt mWtd stats us2 mt subGoal

-- | While the unit is in TransitioningTo Climbing and has a climb
--   destination set, lerp usRealZ smoothly from usClimbFromTile's z
--   toward the target z based on how far we are through the
--   transition. usGridZ (the integer tile-z used by game logic)
--   stays at fromZ for the duration of the climb — the renderer
--   consumes usRealZ for the visual position, but tile lookups
--   treat the unit as logically pinned at the cliff base until
--   handleTransitionExpiry commits the snap at the top.
tickClimbZ ∷ Double → UnitSimState → UnitSimState
tickClimbZ now us =
    case (usState us, usClimbFromTile us, usClimbToTile us,
          usTransitionUntil us, usClimbStartTime us) of
        (TransitioningTo Climbing, Just (_, _, fromZ),
         Just (_, _, toZ), Just untilT, Just startT) →
            let dur      = max 0.001 (untilT - startT)
                remain   = max 0 (untilT - now)
                progress = clamp01 (1 - realToFrac remain / dur)
                fromZF   = fromIntegral fromZ ∷ Float
                toZF     = fromIntegral toZ   ∷ Float
                lerp     = fromZF + realToFrac progress * (toZF - fromZF)
            in us { usRealZ = if remain ≤ 0 then toZF else lerp }
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
        (TransitioningTo Falling, Just (_, _, fromZ), Just (_, _, toZ), Just untilT) →
            let dur = max 0.001
                          (untilT - fallStartFromUntil untilT fromZ toZ)
                remain   = max 0 (untilT - now)
                progress = clamp01 (1 - realToFrac remain / dur)
                fromZF   = fromIntegral fromZ ∷ Float
                toZF     = fromIntegral toZ   ∷ Float
                lerp     = fromZF + realToFrac progress * (toZF - fromZF)
            in us { usRealZ = if remain ≤ 0 then toZF else lerp }
        _ → us
  where
    clamp01 ∷ Double → Double
    clamp01 x = max 0 (min 1 x)

fallStartFromUntil ∷ Double → Int → Int → Double
fallStartFromUntil untilT fromZ toZ =
    let dz = abs (toZ - fromZ)
        secondsPerZ = realToFrac (1 / fallSpeedZ) ∷ Double
        duration = secondsPerZ * fromIntegral dz
    in untilT - duration

-- | Climb ascent rate in tiles-of-Z per real-second. 0.3 → 3.33 s
--   per Z level. Tuned for "climbing is a real commitment" pacing.
climbSpeedZ ∷ Float
climbSpeedZ = 0.3

-- | Fall descent rate in tiles-of-Z per real-second. Faster than
--   climbing — falling is hardly a controlled motion. ~6 z/sec is
--   a 2-tile drop in 0.33 s, which keeps the falling animation
--   snappy without making it inscrutable.
fallSpeedZ ∷ Float
fallSpeedZ = 6.0

-- | The Z drop magnitude at which a descent step stops being a
--   "walk off the edge" and becomes a real fall. dz of -1 still
--   plays the regular walking animation; dz of -2 or worse triggers
--   the Standing→Falling transition + impact calc.
fallTriggerDz ∷ Int
fallTriggerDz = 2

-- | Impact thresholds. impact = dz × (body_mass / 50) / toughness.
--   At these values the unit walks off (no impact event), collapses
--   into the landing anim, or dies. Tuned so:
--     * Acolyte (70 kg, toughness 1.0): walks dz=1 (1.4), collapses
--       dz=2-3 (2.8/4.2), dies dz=4+ (5.6+).
--     * Bear   (~143 kg, toughness 3.0): walks dz=1 (0.95), collapses
--       dz=2-5 (1.9/2.9/3.8/4.8), dies dz=6+ (5.7+).
impactWalkThreshold ∷ Float
impactWalkThreshold = 1.5

impactKillThreshold ∷ Float
impactKillThreshold = 5.0

-- | Compute fall impact. impact = dz × (mass/50) / toughness.
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
                (Climbing, Just (tx, ty, tz), _) →
                    -- Climb done. Stamp the dz climbed onto a
                    -- transient XP field that tickAllMovement
                    -- drains into the unit's "climbing" skill.
                    let dz = case usClimbFromTile us of
                            Just (_, _, fromZ) →
                                fromIntegral (abs (tz - fromZ)) ∷ Float
                            Nothing → 0
                    in us { usRealX = tx, usRealY = ty
                          , usGridZ = tz, usRealZ = fromIntegral tz
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
            -- Fall landing: decide what happens on impact. The fall
            -- transition's expiry routes to walk (low impact),
            -- collapse (medium), or death (high) based on the
            -- impact score stored at fall start.
            fellAndLanded = target ≡ Falling
            (poseAfter, chainAfter, clearFall) = case usFallImpact us of
                Just imp
                  | not fellAndLanded         → (target, usPostTransition snapped, False)
                  | imp ≤ impactWalkThreshold → (Standing, [], True)
                       -- Light landing: keep pose Standing, no chain.
                  | imp ≤ impactKillThreshold →
                       -- Medium landing: TransitioningTo Collapsed
                       -- (the landing anim plays as that transition).
                       -- After it, leave the unit Collapsed — the
                       -- existing collapsed→standing path handles
                       -- recovery.
                       (Falling, [Collapsed], True)
                  | otherwise                 → (Dead, [], True)
                       -- Lethal: snap to Dead pose. UnitKill-style
                       -- in-flight clears below.
                Nothing                       → (target, usPostTransition snapped, False)
            killedByFall = poseAfter ≡ Dead ∧ fellAndLanded
            usPosed = snapped { usPose            = poseAfter
                              , usState           = if killedByFall
                                                    then Idle
                                                    else Idle
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
                              -- Lethal fall clears the same in-flight
                              -- fields UnitKill clears so AI / drink /
                              -- transition timers can't tick on a
                              -- corpse.
                              , usTarget          = if killedByFall
                                                    then Nothing
                                                    else usTarget snapped
                              , usLocalPath       = if killedByFall
                                                    then []
                                                    else usLocalPath snapped
                              , usDrinkUntil      = if killedByFall
                                                    then Nothing
                                                    else usDrinkUntil snapped
                              , usEatUntil        = if killedByFall
                                                    then Nothing
                                                    else usEatUntil snapped
                              , usPickupUntil     = if killedByFall
                                                    then Nothing
                                                    else usPickupUntil snapped
                              , usPostTransition  = chainAfter
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
    ∷ Double
    → Double
    → Maybe WorldTileData
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → UnitSimState
stepTowardSubGoal now dt mWtd stats us mt (gx, gy) =
    let dx   = gx - usRealX us
        dy   = gy - usRealY us
        dist = sqrt (dx * dx + dy * dy)
        step = mtSpeed mt * realToFrac dt
    in if dist ≤ max step arrivalEpsilon
       then arriveAtSubGoal us mt (gx, gy) mWtd
       else moveToward now stats us mt mWtd dx dy dist step

-- | Snap to the sub-goal. If we arrived at the final target (no more
--   waypoints, sub-goal is the target), clear the target. Otherwise
--   pop the first waypoint and continue next tick.
arriveAtSubGoal
    ∷ UnitSimState
    → MoveTarget
    → (Float, Float)
    → Maybe WorldTileData
    → UnitSimState
arriveAtSubGoal us mt (gx, gy) mWtd =
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
                        , usState     = Walking
                        }
        [] →
            -- Greedy mode: subGoal was the final target, so we've arrived.
            us' { usTarget = Nothing, usState = Idle }

-- | Step one tick toward the sub-goal. Cost-check first; on block or
--   high-cost (greedy mode only) trigger replan. If the next tile
--   crossing is a cliff (Z step that has no slope ramp), initiate a
--   climb transition instead of taking the step.
moveToward
    ∷ Double             -- now (game time, for transition expiry)
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → Float    -- dx
    → Float    -- dy
    → Float    -- distance to sub-goal
    → Float    -- step length this tick
    → UnitSimState
moveToward now stats us mt mWtd dx dy dist step =
    let nx   = dx / dist
        ny   = dy / dist
        newX = usRealX us + nx * step
        newY = usRealY us + ny * step
        srcTile = (floor (usRealX us), floor (usRealY us))
        dstTile = (floor newX, floor newY)
        mCost
            | srcTile ≡ dstTile = Just 0  -- sub-tile motion, no boundary cross
            | otherwise = case mWtd of
                Just wtd → case stepCost wtd srcTile dstTile of
                    Nothing → Nothing
                    Just c  →
                        -- No-corner-cutting: a diagonal step grazes the
                        -- two axis-aligned neighbors of srcTile. Reject
                        -- the step if either neighbor is impassable
                        -- (Ocean/Lava/unloaded chunk), so units can't
                        -- slip diagonally past a wall. Endpoint cost is
                        -- preserved; only the passability gate widens.
                        let (sx, sy)   = srcTile
                            (dgx, dgy) = dstTile
                            isDiagonal = sx ≢ dgx ∧ sy ≢ dgy
                            cornerOk =
                                case ( stepCost wtd srcTile (dgx, sy)
                                     , stepCost wtd srcTile (sx, dgy) ) of
                                    (Just _, Just _) → True
                                    _                → False
                        in if isDiagonal ∧ not cornerOk
                           then Nothing
                           else Just c
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
                        | sz - dz ≥ fallTriggerDz → Just (sz, dz)
                    _ → Nothing
            _ → Nothing
    in case mCost of
        Nothing →
            replan us mt mWtd srcTile
        Just c | not followingPath ∧ c > replanCostThreshold →
            replan us mt mWtd srcTile
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
                      , usState  = Walking
                      }

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
        speed    = computeClimbSpeed stats
        duration = realToFrac (fromIntegral dz / speed) ∷ Double
        srcX     = usRealX us
        srcY     = usRealY us
        (sx, sy) = (floor srcX ∷ Int, floor srcY ∷ Int)
        -- Pullup anchor = the cliff edge of the destination tile
        -- facing the source, nudged inward by `edgeMargin` so that
        -- `floor edgeX` reliably returns `dgx` (the destination
        -- tile), not `dgx+1`. Without this nudge, the unit lands
        -- exactly on the tile-boundary line — `floor` rounds up
        -- and the unit is logically on the SOURCE tile with the
        -- destination tile's Z. Next tick the engine re-syncs Z
        -- down to the source tile's terrain and the cliff trigger
        -- fires again. Climb loop.
        --
        -- Anchoring slightly INSIDE the destination tile keeps the
        -- visual at the cliff edge (margin is too small to see)
        -- and the game-logic tile assignment correct.
        edgeMargin = 0.05
        edgeX
            | dgx > sx  = fromIntegral dgx + edgeMargin
                -- east cliff: just inside west edge of dst tile
            | dgx < sx  = fromIntegral (dgx + 1) - edgeMargin
                -- west cliff: just inside east edge of dst tile
            | otherwise = srcX                       -- cardinal y-only
        edgeY
            | dgy > sy  = fromIntegral dgy + edgeMargin
                -- south cliff: just inside north edge of dst tile
            | dgy < sy  = fromIntegral (dgy + 1) - edgeMargin
                -- north cliff: just inside south edge of dst tile
            | otherwise = srcY                       -- cardinal x-only
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
        duration = realToFrac (fromIntegral dz / fallSpeedZ) ∷ Double
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
    ∷ UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → (Int, Int)
    → UnitSimState
replan us mt mWtd srcTile =
    let finalTile = (floor (mtTargetX mt), floor (mtTargetY mt))
        tilePath = case mWtd of
            Just wtd → localAStar wtd srcTile finalTile defaultMaxRadius
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
