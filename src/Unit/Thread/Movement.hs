{-# LANGUAGE Strict #-}
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
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Combat.Types (pushInjuryEvent)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Unit.Sim.Types
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..), UnitDef(..)
                  , Wound(..), TrailState(..))
import Unit.Fall (FallInjury(..), fallInjuries, fallStunFor)
import Blood.Impact (spawnImpactBlood, impactFallbackAngle, pickImpactWound)
import Blood.Trail
    ( defaultTrailThresholds, consumeTrailMarks, TrailMarkOut(..)
    , spawnTrailMark
    )
import Blood.Pool
    ( defaultPoolThresholds, classifyOngoing, OngoingMode(..)
    , consumePoolLayers, PoolLayerOut(..), poolLayerOffset, spawnPoolLayer
    )
import Combat.Wounds (externalBleedRateFor, dominantExternalBleedWound)
import Unit.Thread.Movement.Types
    (UnitMoveStats(..), defaultMoveStats, baselineUnitHeight)
import Unit.Thread.Movement.Leap
    (metresPerTile, jumpMaxTiles, maxJumpHeight, strikeReach
    , lungeImpactSpeed, startJump)
import Unit.Thread.Movement.Climb
    (rollClimbSlips, convertSlippedClimb, applyClimbXP, clearPendingClimbXP)
import Unit.Thread.Movement.PathAdvance
    (tickUnit, snapshotOwnedWorldTiles, moveWorldFor)

-- | Advance all units with active move targets, using cost-aware
--   greedy movement + local A* replan. Each OWNING page's tile data is
--   snapshotted once per tick, so all units in this batch see one
--   consistent view of their own world (#1593).
tickAllMovement ∷ Double → EngineEnv → IORef UnitThreadState → IO ()
tickAllMovement dt env utsRef = do
    now  ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    -- Snapshot per-unit body_mass + toughness so the pure movement
    -- tick can compute fall impact without reading the unit manager.
    -- Defaults match human baseline (70 kg, toughness 1.0) — picked
    -- so a unit with no stats declared behaves like an unmodified
    -- acolyte against the impact thresholds.
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    -- Pathing cost tunables (climb/ramp/fall/river/lake penalties +
    -- replan threshold), loaded once from config/pathing.yaml. Read per
    -- tick so a future settings UI that mutates the ref takes effect
    -- live; the read is a single IORef deref.
    pc ← readIORef (ucPathingConfigRef (toUnitCombatCapability env))
    -- Surface-material registry for the per-tile movement factor (#312):
    -- soft/loose ground slows the unit and biases A* toward firm routes.
    -- Read once per tick (single IORef deref), like the pathing config.
    reg ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    -- Per-unit view of the batch's snapshots: this mover's OWN page's
    -- tiles, flagged as verified. #1217 made a hazard-protected request
    -- fail closed on anything else; #1593 makes that the only terrain any
    -- mover ever sees, so nobody paths against another page's heightmap.
    let worldFor mInst snaps = moveWorldFor snaps (uiPage <$> mInst)
        statsFor uid   = statsForInst (HM.lookup uid (umInstances um))
        statsForInst mInst = case mInst of
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
    -- Terrain comes from each mover's OWN page (#1593), so the batch
    -- snapshots exactly the pages this tick's movers stand on — each read
    -- once, so every mover on one page sees one consistent view of it. A
    -- sim state whose unit is gone contributes no page and gets no
    -- terrain, rather than borrowing whichever page happens to be active.
    mSnaps ← snapshotOwnedWorldTiles (toWorldSimCapability env) $ HS.fromList
        [ uiPage inst
        | uid ← HM.keys simStates
        , Just inst ← [HM.lookup uid (umInstances um)] ]
    let -- The stats and the terrain view both derive from the mover's
        -- instance, so they share ONE lookup per moving unit per tick.
        simStates' = HM.mapWithKey
                        (\uid ss →
                            let mInst = HM.lookup uid (umInstances um)
                            in tickUnit pc reg now dt (worldFor mInst mSnaps)
                                        (statsForInst mInst) ss)
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
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um' →
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
    sm ← readReadOnlyRef
        (crvSubstanceManagerRef (toContentRegistriesViewCapability env))
    let fallResults =
            [ (uid, injs, foldr (max . fiSeverity) 0 injs, usRealX ss, usRealY ss, usGridZ ss)
            | (uid, ss) ← HM.toList simStates'''
            , Just drop ← [usPendingFallDrop ss]
            , Just inst ← [HM.lookup uid (umInstances um)]
            , Just def  ← [HM.lookup (uiDefName inst) (umDefs um)]
            , let mass  = HM.lookupDefault 70.0 "body_mass" (uiStats inst)
                  tough = HM.lookupDefault 1.0  "toughness" (uiStats inst)
                  injs  = fallInjuries sm def mass tough drop
            ]
    when (not (null fallResults)) $
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um' →
            let applyOne (uid, injs, _, _, _, _) m = case HM.lookup uid m of
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
    forM_ fallResults $ \(uid, injs, worst, _, _, _) → when (not (null injs)) $
        let detail = T.intercalate "|"
                [ T.intercalate ":"
                    [ fiPart i, fiKind i
                    , tshow (round (fiSeverity i * 100) ∷ Int) ]
                | i ← injs ]
        in pushInjuryEvent (ucInjuryEventsRef (toUnitCombatCapability env)) now (unUnitId uid) "fall"
             [ ("detail",   detail)
             , ("count",    tshow (length injs))
             , ("severity", tshow worst) ]

    -- Impact blood (#607): ONE mark per fall (never per fractured part,
    -- so a bad fall that breaks several bones at once stays bounded to
    -- a single decal — requirement 9). pickImpactWound resolves which
    -- single injury represents the fall — NOT just the raw-severity
    -- worst one, since the worst-by-severity injury might not itself
    -- clear its own catastrophic threshold while a lower-severity one
    -- in the same fall does. A fall has no "attacker", so direction
    -- always falls back to a deterministic seeded angle (requirement 7).
    -- Position comes from THIS tick's fresh sim state (gx, gy, gz), not
    -- the unit-manager mirror -- that mirror is only refreshed by a
    -- separate publish-to-render pass (Unit.Thread), so at this point in
    -- the tick it can still be one tick behind the landing this very
    -- fall just computed (a floating mid-air decal bug).
    forM_ fallResults $ \(uid, injs, _, gx, gy, gz) → when (not (null injs)) $
        case HM.lookup uid (umInstances um) of
            Nothing   → pure ()
            Just inst →
                case pickImpactWound [ (fiKind i, fiSeverity i) | i ← injs ] of
                    Nothing → pure ()
                    Just (kind, sev, _) →
                        let seed = round (now * 1000.0) + fromIntegral (unUnitId uid)
                            angle = impactFallbackAngle seed
                        in spawnImpactBlood (toWorldSimCapability env) (uiPage inst) gx gy gz
                             kind sev angle seed (Just uid) now

    -- Ongoing bleeding (#882 trails + #883 pooling): both halves are
    -- driven from here, from ONE per-unit accumulator, with exactly one
    -- of them consuming it per tick (see Blood.Pool.classifyOngoing).
    -- Only units carrying an active trail accumulator
    -- (uiTrailState — populated by Combat.Wounds.Tick's conserved
    -- external-loss accounting; Nothing for a unit that has never bled
    -- externally) do any work here. `um`'s uiGridX/Y still mirrors this
    -- unit's position as of the END of the PREVIOUS tick — Unit.Thread's
    -- publishToRender (which writes it) hasn't run yet for THIS tick —
    -- so pairing it with this tick's fresh simStates''' position gives
    -- exactly this tick's step distance, the same "old vs new" pairing
    -- the fall-injury/impact-blood code above uses.
    -- `ts` itself is NOT read here — only used as a stale existence
    -- filter (does this unit have SOME trail state worth checking).
    -- The atomic block below re-reads uiTrailState from the FRESH
    -- instance it receives, so a concurrent wound-tick write (the
    -- combat thread merges into the very same unitManagerRef
    -- independently) is never lost to this stale snapshot — worst
    -- case a brand-new bleed that started after this snapshot is
    -- simply picked up next tick instead of this one.
    let trailSteps =
            [ (uid, ox, oy, nx, ny, nz, def)
            | (uid, ss) ← HM.toList simStates'''
            , Just inst ← [HM.lookup uid (umInstances um)]
            , Just _    ← [uiTrailState inst]
            , Just def  ← [HM.lookup (uiDefName inst) (umDefs um)]
            , let ox = uiGridX inst; oy = uiGridY inst
                  nx = usRealX ss;   ny = usRealY ss
                  nz = usGridZ ss
            ]
    trailMarks ← if null trailSteps then pure [] else
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um' →
            let processOne (accMap, accMarks) (uid, ox, oy, nx, ny, nz, def) =
                    case HM.lookup uid accMap of
                        Nothing → (accMap, accMarks)
                        Just liveInst → case uiTrailState liveInst of
                            -- Cleared concurrently (e.g. the wound tick
                            -- just marked this unit DiedNow) — nothing
                            -- to consume.
                            Nothing → (accMap, accMarks)
                            Just ts →
                                let extRate = externalBleedRateFor def liveInst
                                in if uiPose liveInst ≡ "dead" ∨ extRate ≤ 0
                                   -- Check death / zero-external-bleed
                                   -- BEFORE consuming anything
                                   -- (#882): a synchronous
                                   -- treatment (unit.treatBleeding
                                   -- zeroing woundBandage) or a wound
                                   -- healing out can drop external bleed
                                   -- to zero between wound ticks, while
                                   -- ts still holds volume banked from
                                   -- BEFORE that moment — if this tick's
                                   -- distance/cadence also happen to
                                   -- clear, calling consumeTrailMarks
                                   -- first would stamp that now-stale
                                   -- volume. Clearing outright here,
                                   -- with NO marks popped, is the only
                                   -- way "external rate reached zero ⇒
                                   -- no further marks" holds exactly.
                                   then ( HM.insert uid
                                            (liveInst { uiTrailState = Nothing }) accMap
                                        , accMarks )
                                   else
                                     let stepDist = sqrt ((nx - ox) * (nx - ox)
                                                         + (ny - oy) * (ny - oy))
                                         repKind = maybe T.empty woundKind
                                             (dominantExternalBleedWound def liveInst)
                                         seedFor i = round (now * 1000.0)
                                             + fromIntegral (unUnitId uid) + i
                                         -- #883: which half owns this
                                         -- tick, by DISPLACEMENT from the
                                         -- cluster anchor (never this
                                         -- tick's step — a unit shuffling
                                         -- in place must keep feeding one
                                         -- pool, not restart a cluster
                                         -- every tick). Uses the END-of-
                                         -- tick position, the same fresh
                                         -- sim state the step is measured
                                         -- against.
                                         (mode, tsC) =
                                             classifyOngoing defaultPoolThresholds
                                                 (nx, ny) ts
                                         (ts', marks) = case mode of
                                           ModeTravel →
                                             let (tsT, popped) =
                                                   consumeTrailMarks
                                                     defaultTrailThresholds
                                                     stepDist dt now tsC
                                                 mkMark i m =
                                                     ( uiPage liveInst
                                                     , ox + tmoFraction m * (nx - ox)
                                                     , oy + tmoFraction m * (ny - oy)
                                                     , nz
                                                     , repKind
                                                     , tmoVolume m
                                                     , seedFor i
                                                     , Just uid
                                                     )
                                             in (tsT, zipWith mkMark [0 ∷ Int ..] popped)
                                           ModeDwell →
                                             -- Bank this tick's step so a
                                             -- resumed trail still has to
                                             -- clear its own distance gate,
                                             -- then grow the local pool.
                                             let tsB = tsC { tsDistSinceMark =
                                                     tsDistSinceMark tsC
                                                         + max 0 stepDist }
                                                 -- (nx, ny) is where the
                                                 -- unit ACTUALLY is; a
                                                 -- cluster's first layer
                                                 -- re-anchors there, so a
                                                 -- walk-then-stop pools
                                                 -- under the stopped unit
                                                 -- rather than back at the
                                                 -- last radius crossing.
                                                 (tsP, layers) =
                                                   consumePoolLayers
                                                     defaultPoolThresholds
                                                     (nx, ny) now tsB
                                                 (ax, ay) = fromMaybe (nx, ny)
                                                     (tsClusterAnchor tsP)
                                                 mkLayer i l =
                                                     let sd = seedFor i
                                                         (dx, dy) = poolLayerOffset
                                                             defaultPoolThresholds
                                                             sd (ploIndex l)
                                                     in ( uiPage liveInst
                                                        , ax + dx, ay + dy, nz
                                                        , repKind
                                                        , ploVolume l
                                                        , sd
                                                        , Just uid
                                                        )
                                             in (tsP, zipWith mkLayer [0 ∷ Int ..] layers)
                                         tagged = [ (mode, m) | m ← marks ]
                                     in ( HM.insert uid
                                            (liveInst { uiTrailState = Just ts' }) accMap
                                        , accMarks ++ tagged )
                (finalMap, allMarks) = foldl' processOne (umInstances um', []) trailSteps
            in (um' { umInstances = finalMap }, allMarks)
    forM_ trailMarks $ \(mode, (page, gx, gy, gz, kind, vol, seed, mSrc)) →
        (case mode of ModeTravel → spawnTrailMark; ModeDwell → spawnPoolLayer)
            (toWorldSimCapability env) page gx gy gz kind vol
            (impactFallbackAngle seed) seed mSrc now

    -- Knockdown stun per landed unit, keyed for the sim writeback.
    let stunMap = HM.fromList [ (uid, fallStunFor worst)
                              | (uid, _, worst, _, _, _) ← fallResults ]
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
