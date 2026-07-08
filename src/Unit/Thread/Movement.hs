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
--
-- The per-unit tick logic (timers, climb/fall Z-interpolation,
-- cost-aware stepping) lives in the sibling "Unit.Thread.Movement.*"
-- modules; this module is the batch-level orchestrator: it snapshots
-- world/unit state once per tick, drives every unit's pure tick, then
-- writes back the IO-only side effects (climb slip rolls, climb XP,
-- fall injuries, the injury log) that need the unit/substance managers.
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
import qualified Data.Text as T
import Combat.Types (pushInjuryEvent)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Sim.Types
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..), UnitDef(..)
                  , Wound(..))
import Unit.Fall (FallInjury(..), fallInjuries, fallStunFor)
import Unit.Thread.Movement.Types
    (UnitMoveStats(..), defaultMoveStats, baselineUnitHeight)
import Unit.Thread.Movement.Leap
    (metresPerTile, jumpMaxTiles, maxJumpHeight, strikeReach
    , lungeImpactSpeed, startJump)
import Unit.Thread.Movement.Climb
    (rollClimbSlips, convertSlippedClimb, applyClimbXP, clearPendingClimbXP)
import Unit.Thread.Movement.PathAdvance (tickUnit, snapshotVisibleWorldTiles)

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
    -- Surface-material registry for the per-tile movement factor (#312):
    -- soft/loose ground slows the unit and biases A* toward firm routes.
    -- Read once per tick (single IORef deref), like the pathing config.
    reg ← readIORef (materialRegistryRef env)
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
                , umsHeight    = HM.lookupDefault baselineUnitHeight
                                                       "height"     (uiStats  inst)
                }
            Nothing → defaultMoveStats
    uts ← readIORef utsRef
    let simStates  = utsSimStates uts
        simStates' = HM.mapWithKey
                        (\uid ss → tickUnit pc reg now dt mWtd (statsFor uid) ss)
                        simStates

    -- Climb slip rolls + climb→fall conversions. Two passes that
    -- both need RNG / per-unit stats, so they live up here (tickUnit
    -- is pure and has neither).
    simStates'' ← rollClimbSlips env now statsFor simStates'
    let simStates''' = HM.mapWithKey (convertSlippedClimb now)
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
