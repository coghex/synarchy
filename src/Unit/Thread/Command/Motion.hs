{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command.Motion
    ( handleUnitMoveToCommand
    , handleUnitJumpCommand
    , handleUnitStopCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Types
import Unit.Sim.Types
import Unit.Thread.Command.Body (injurySpeedMult)
import Unit.Thread.Command.Pose (isTransitioning)
import Unit.Thread.Movement (startJump, jumpMaxTiles)

handleUnitMoveToCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                        → Float → Float → Float → IO ()
handleUnitMoveToCommand env utsRef uid tx ty speed = do
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
    um ← readIORef (unitManagerRef env)
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
                        ss' = ss { usTarget    = Just (MoveTarget tx ty effSpeed)
                                 , usState     = activity
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitJumpCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                      → Int → Int → IO ()
handleUnitJumpCommand env utsRef uid tgx tgy = do
    now ← readIORef (gameTimeRef env)
    um  ← readIORef (unitManagerRef env)
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

handleUnitStopCommand ∷ IORef UnitThreadState → UnitId → IO ()
handleUnitStopCommand utsRef uid = do
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             , usGetUpAt         = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())
