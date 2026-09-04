{-# LANGUAGE Strict #-}
module Unit.Thread.Command.Motion
    ( handleUnitMoveToCommand
    , handleUnitSetMoveSpeedCommand
    , handleUnitJumpCommand
    , handleUnitStopCommand
    , stopUnitSimState
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Unit.Types
import Unit.Sim.Types
import Unit.Thread.Command.Body (injurySpeedMult)
import Unit.Thread.Command.MotionGuard (motionPayloadOk)
import Unit.Thread.Command.Pose (isTransitioning)
import Unit.Thread.Movement (startJump, jumpMaxTiles)

handleUnitMoveToCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                        → Float → Float → Float → MoveHazardPolicy → IO ()
handleUnitMoveToCommand env utsRef uid tx ty speed hazard = do
  ok ← motionPayloadOk env "UnitMoveTo" [("targetX", tx), ("targetY", ty)]
                                        [("speed", speed)]
  when ok $ do
    -- Apply the injury speed multiplier on receipt so EVERY move
    -- command — commanded, wander, attack-pursuit, retreat — gets
    -- scaled the same way without the AI caller having to know.
    --
    -- Note: the umRef read below is NOT atomic with the utsRef
    -- modify. If the wound subsystem (10 Hz) lands a new wound
    -- between the two, this move commits with the pre-wound
    -- multiplier and the unit travels its current path segment at
    -- the stale speed. The next move command picks up the fresh
    -- state. Effect is bounded (one segment of slightly-too-fast
    -- movement, ≲1% per-command hit rate) and not worth merging the
    -- two refs to close — kept here so the next reader doesn't
    -- mistake the separation for an oversight.
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    let (effSpeed, isRunning) = case HM.lookup uid (umInstances um) of
            Nothing   → (speed, False)
            Just inst →
                let (bodyParts, maxSp, runFrac) = case HM.lookup (uiDefName inst) (umDefs um) of
                        Just d  → (udBodyParts d, udMaxSpeed d, udRunThreshold d)
                        Nothing → ([], 3.0, 0.6)
                    sp     = speed * injurySpeedMult bodyParts inst
                    runCut = maxSp * runFrac   -- per-unit run-anim threshold
                in (sp, sp > runCut)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                -- Standing AND Crawling units can move (a crawling unit
                -- crawls slowly toward the goal — the mover caps its
                -- speed). Crouching / Collapsed refuse moves until they
                -- transition back up. In-progress transitions also ignore
                -- moves so a right-click can't yank a unit out of a
                -- mid-transition.
                | usPose ss ≢ Standing ∧ usPose ss ≢ Crawling → (uts, ())
                | isTransitioning (usState ss) → (uts, ())
                | otherwise →
                    -- A crawling unit is always Walking-gait (there's no
                    -- crawling-run anim, and the mover caps its speed); only
                    -- a standing unit can break into a Running activity.
                    let activity = if isRunning ∧ usPose ss ≡ Standing
                                   then Running else Walking
                        -- A new request REPLACES the previous one
                        -- wholesale — destination, speed, local path
                        -- AND hazard policy (#1217). A protected wander
                        -- can therefore never make a later player
                        -- command refuse a cliff, nor vice versa.
                        ss' = ss { usTarget    = Just (MoveTarget tx ty effSpeed hazard)
                                 , usState     = activity
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

-- | Retarget the speed of an ALREADY in-flight move, leaving
--   `usTarget`'s x/y, `usLocalPath`, and `usState` untouched — unlike
--   'handleUnitMoveToCommand', which always wipes `usLocalPath` to
--   force a fresh greedy/replan cycle. A caller re-running `moveTo`
--   every tick just to nudge the commanded speed stalls pathing (the
--   unit keeps losing its computed route); this command exists so
--   continuous feedback (#999's stamina-adaptive pacing) can retarget
--   speed every tick without that cost. No-op if the unit has no
--   in-flight target (nothing to retarget) — never creates one.
--
--   The target's hazard policy (#1217) is deliberately RETAINED: this
--   command changes the pace of the route the caller already asked for,
--   so re-permitting a fall it refused would be a silent policy change
--   nobody requested.
handleUnitSetMoveSpeedCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                              → Float → IO ()
handleUnitSetMoveSpeedCommand env utsRef uid speed = do
  ok ← motionPayloadOk env "UnitSetMoveSpeed" [] [("speed", speed)]
  when ok $ do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    let effSpeed = case HM.lookup uid (umInstances um) of
            Nothing   → speed
            Just inst →
                let bodyParts = case HM.lookup (uiDefName inst) (umDefs um) of
                        Just d  → udBodyParts d
                        Nothing → []
                in speed * injurySpeedMult bodyParts inst
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss → case usTarget ss of
                Nothing → (uts, ())
                Just mt →
                    let ss' = ss { usTarget = Just (mt { mtSpeed = effSpeed }) }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitJumpCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                      → Int → Int → IO ()
handleUnitJumpCommand env utsRef uid tgx tgy = do
    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    -- Reach = learned jumping skill blended with agility/strength stats
    -- (the skill/stat split). Unknown unit → 0 reach (can't leap).
    let maxTiles = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst →
                let bm = HM.lookupDefault 1.0 "body_mass" (uiStats inst)
                    fm = HM.lookupDefault 0.0 "fat_mass"  (uiStats inst)
                    fatFrac = if bm > 0 then fm / bm else 0
                in jumpMaxTiles (HM.lookupDefault 0.0 "jumping"  (uiSkills inst))
                                (HM.lookupDefault 1.0 "agility"  (uiStats  inst))
                                (HM.lookupDefault 1.0 "strength" (uiStats  inst))
                                fatFrac
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                -- Only a standing, non-transitioning unit can leap.
                | usPose ss ≢ Standing         → (uts, ())
                | isTransitioning (usState ss)  → (uts, ())
                | otherwise →
                    let dstX = fromIntegral tgx + 0.5
                        dstY = fromIntegral tgy + 0.5
                        dx   = dstX - usRealX ss
                        dy   = dstY - usRealY ss
                        d    = sqrt (dx * dx + dy * dy)
                    -- Refuse a leap beyond reach (or a no-op onto self); the
                    -- unit just stays put — slice 1 has no "fall short" yet.
                    in if d < 0.001 ∨ d > maxTiles
                       then (uts, ())
                       else let ss' = startJump now ss tgx tgy
                            in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

-- | The pure core of 'handleUnitStopCommand': drop whatever the unit is
--   doing and return it to Idle — EXCEPT when it is mid-pose-transition,
--   in which case the transition is left to finish on its own.
--
--   Why the exception (#1709). A pose transition — a leap, a fall, a
--   climb — interpolates the CONTINUOUS position
--   (@usRealX@/@usRealY@/@usRealZ@) between two grid anchors, and only
--   'Unit.Thread.Movement.Timers.handleTransitionExpiry' reconciles it
--   back to the grid (the landing snap that sets
--   @usRealZ = fromIntegral usGridZ@). Clearing @usState@ /
--   @usTransitionUntil@ partway through that arc strands the unit at the
--   interpolated position permanently: both
--   'Unit.Thread.Movement.Fall.tickFallZ' and @handleTransitionExpiry@
--   match on the very state the stop erased, so nothing finishes the arc
--   and nothing snaps the position. The unit renders above its grid
--   layer for good ('Unit.Render' derives the sprite offset from the
--   continuous @uiRealZ@ that 'Unit.Thread' mirrors from @usRealZ@), and
--   the @unit-sim@ save component round-trips that inconsistent pair
--   verbatim. It reached this handler because a mental break
--   (@scripts/mental_state.lua@'s @enterBreak@) calls @unit.stop@ from
--   the physiology tick, which — unlike @unit_ai.lua@'s @tickOne@ — has
--   no mid-transition guard of its own.
--
--   So a stop that lands mid-transition keeps exactly the fields the
--   in-flight arc owns — @usState@, @usTransitionUntil@,
--   @usPostTransition@ (already untouched here, so the landing's
--   stand-up / collapse chain still runs), @usGetUpAt@ (the knockdown
--   timer a landed fall stamps on during its chained Collapsed step),
--   and the leap/fall/climb endpoints this handler never touched — and
--   clears only what the caller actually wants dropped: the move target,
--   the local path, and the drink/eat/pickup timers. The arc lands on
--   its own tick with position snapped to the grid and its normal
--   outcome intact (a fall's @usPendingFallDrop@ + knockdown + injury
--   pass, a leap's stand-up chain), leaving the unit Idle and stationary
--   — no target to walk out, because the stop already took it.
--
--   Nothing else changes. A walking, running, working, drinking, eating,
--   or fighting unit is preempted on the same tick it always was, which
--   is what every other @unit.stop@ caller observes: they all reach it
--   through @tickOne@, which refuses to dispatch mid-transition, so this
--   branch is unreachable for them.
stopUnitSimState ∷ UnitSimState → UnitSimState
stopUnitSimState ss
    -- The guard reads the AUTHORITATIVE unit-thread state, in the same
    -- transaction that applies the stop. It deliberately does NOT trust
    -- an activity the caller observed earlier: `unit.stop` only ENQUEUES
    -- `UnitStop`, and `unit.getActivity` reads the render mirror, so the
    -- unit could have entered (or left) a transition between the two.
    | isTransitioning (usState ss) =
        ss { usTarget      = Nothing
           , usLocalPath   = []
           , usDrinkUntil  = Nothing
           , usEatUntil    = Nothing
           , usPickupUntil = Nothing
           }
    | otherwise =
        ss { usTarget    = Nothing
           , usState     = Idle
           , usLocalPath = []
           , usDrinkUntil      = Nothing
           , usEatUntil        = Nothing
           , usPickupUntil     = Nothing
           , usTransitionUntil = Nothing
           , usGetUpAt         = Nothing
           }

handleUnitStopCommand ∷ IORef UnitThreadState → UnitId → IO ()
handleUnitStopCommand utsRef uid = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                (uts { utsSimStates =
                         HM.insert uid (stopUnitSimState ss) simStates }, ())
