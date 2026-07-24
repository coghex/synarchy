{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command.Pose
    ( handleUnitCollapseCommand
    , handleUnitCrawlCommand
    , handleUnitKillCommand
    , handleUnitReviveCommand
    , handleUnitDrinkCommand
    , handleUnitEatCommand
    , handleUnitPickupCommand
    , handleUnitTransitionToCommand
    , isTransitioning
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Anim (stateKey)
import Unit.Types
import Unit.Sim.Types

isTransitioning ∷ UnitActivity → Bool
isTransitioning (TransitioningTo _) = True
isTransitioning _                   = False

handleUnitCollapseCommand ∷ IORef UnitThreadState → UnitId → IO ()
handleUnitCollapseCommand utsRef uid = do
    -- Snap to Collapsed pose. Fall animation is deferred — when the
    -- standing→collapsed composite is authored, this handler will
    -- instead queue a TransitioningTo Collapsed.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usTarget    = Nothing
                             , usPose      = Collapsed
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             -- A survival/explicit collapse is NOT a fall
                             -- knockdown: clear any getup timer so this
                             -- collapse stays resource-gated (recovers via
                             -- checkRevive), not auto-stood by the movement
                             -- tick.
                             , usGetUpAt         = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitCrawlCommand ∷ IORef UnitThreadState → UnitId → IO ()
handleUnitCrawlCommand utsRef uid = do
    -- Drop to a sustained Crawling pose. Unlike Collapsed this KEEPS the
    -- in-flight move target + walking state, so a unit maimed mid-stride
    -- keeps crawling toward its goal (the mover caps its speed to a
    -- crawl). Only clears mid-transition/getup timers so it can't be both
    -- crawling and mid-climb. No-op if already crawling/collapsed/dead.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≡ Crawling ∨ usPose ss ≡ Dead → (uts, ())
                | otherwise →
                    -- Preserve a Walking/Idle move state (so it crawls on
                    -- toward the goal); drop a stranded transition to Idle.
                    let st  = if isTransitioning (usState ss)
                              then Idle else usState ss
                        ss' = ss { usPose            = Crawling
                                 , usState           = st
                                 , usTransitionUntil = Nothing
                                 , usGetUpAt         = Nothing
                                 , usDrinkUntil      = Nothing
                                 , usEatUntil        = Nothing
                                 , usPickupUntil     = Nothing
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitKillCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
handleUnitKillCommand env utsRef uid = do
    -- Terminal: snap to Dead pose and clear all in-flight state.
    -- No animation chain — just an instant transition. Dead units
    -- are filtered out by AI / movement / drink / pickup via the
    -- non-Standing guards and the Lua-side dead-pose short-circuit.
    --
    -- Also clear the bleeding-trail accumulator (#882 requirement 5)
    -- and stamp uiPose="dead" HERE, synchronously with the kill — a
    -- queued UnitKill (e.g. the unit.kill debug surface, or an
    -- AI-driven death) bypasses Combat.Wounds.Tick's own DiedNow
    -- clearing entirely, and waiting for the NEXT tick's
    -- publishToRender to mirror uiPose="dead" from sim state would
    -- leave BOTH Unit.Thread.Movement's dead-pose check AND
    -- Combat.Wounds.Tick's own "uiPose inst == dead" early-exit guard
    -- reading a stale (pre-kill) pose for up to one more wound tick —
    -- long enough for a still-externally-bleeding corpse to recreate
    -- uiTrailState from Nothing before the mirror ever lands (round-5
    -- review). Stamping uiPose here directly closes that race; the
    -- next publishToRender idempotently re-derives the SAME "dead"
    -- string from usPose, so this never fights it.
    atomicModifyIORef' (unitManagerRef env) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing   → (um, ())
            Just inst → (um { umInstances = HM.insert uid
                                (inst { uiTrailState = Nothing
                                      , uiPose = "dead" }) (umInstances um) }, ())
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usPose             = Dead
                             , usState            = Idle
                             , usTarget           = Nothing
                             , usLocalPath        = []
                             , usDrinkUntil       = Nothing
                             , usEatUntil         = Nothing
                             , usPickupUntil      = Nothing
                             , usTransitionUntil  = Nothing
                             , usTransitionStride = 1
                             , usPostTransition   = []
                             , usClimbFromTile    = Nothing
                             , usClimbToTile      = Nothing
                             , usClimbStartTime   = Nothing
                             , usClimbSlipAt      = Nothing
                             , usFallFromTile     = Nothing
                             , usFallToTile       = Nothing
                             , usPendingClimbXP   = 0
                             , usGetUpAt          = Nothing
                             , usPendingFallDrop = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitReviveCommand ∷ IORef UnitThreadState → UnitId → IO ()
handleUnitReviveCommand utsRef uid = do
    -- Snap to Standing pose. Per the orthogonal-pose plan, a real
    -- revive eventually chains Collapsed → Crawling → Crouching →
    -- Standing reverse transitions; for now (no transition assets yet)
    -- it just snaps. Acts on Collapsed (waking) AND Crawling (legs healed,
    -- standing back up) units.
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≢ Collapsed ∧ usPose ss ≢ Crawling → (uts, ())
                | otherwise →
                    let ss' = ss { usPose            = Standing
                                 , usState           = Idle
                                 , usDrinkUntil      = Nothing
                                 , usEatUntil        = Nothing
                                 , usPickupUntil     = Nothing
                                 , usTransitionUntil = Nothing
                                 , usGetUpAt         = Nothing
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitDrinkCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
handleUnitDrinkCommand env utsRef uid = do
    -- Only acts on Idle units. Drinking blocks movement; an explicit
    -- precondition stops us from interrupting another animated state
    -- (Walking moves are easy to interrupt, Collapsed/Reviving are
    -- not, so we just require Idle for simplicity).
    um ← readIORef (unitManagerRef env)
    let duration = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = "drinking" ∷ Text
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Idle → (uts, ())
                | duration ≤ 0 → (uts, ())  -- no anim → no-op
                | otherwise →
                    let ss' = ss { usState      = Drinking
                                 , usTarget     = Nothing
                                 , usLocalPath  = []
                                 , usDrinkUntil = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitEatCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
handleUnitEatCommand env utsRef uid = do
    -- Same shape as UnitDrink. Plays the "eat" state animation and
    -- auto-reverts to Idle when the timer expires. Nutrition/inventory
    -- changes are applied Lua-side before this command is issued.
    um ← readIORef (unitManagerRef env)
    uts0 ← readIORef utsRef
    let mPose = usPose <$> HM.lookup uid (utsSimStates uts0)
        duration = case (HM.lookup uid (umInstances um), mPose) of
            (Just inst, Just curPose) → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = stateKey curPose Eating
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
            _ → 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Idle → (uts, ())
                | duration ≤ 0 → (uts, ())
                | otherwise →
                    let ss' = ss { usState      = Eating
                                 , usTarget     = Nothing
                                 , usLocalPath  = []
                                 , usEatUntil   = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitPickupCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
handleUnitPickupCommand env utsRef uid = do
    -- Same shape as UnitDrink. Plays the "pickup" state animation
    -- briefly, then auto-reverts to Idle.
    um ← readIORef (unitManagerRef env)
    let duration = case HM.lookup uid (umInstances um) of
            Nothing   → 0
            Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                Nothing  → 0
                Just def →
                    let key      = "pickup" ∷ Text
                        animName = HM.lookupDefault key key (udStateAnims def)
                    in case HM.lookup animName (udAnimations def) of
                        Nothing → 0
                        Just a  →
                            let counts = V.length <$> Map.elems (aFrames a)
                                maxN   = if null counts then 0 else maximum counts
                                fps    = aFps a
                            in if fps > 0 ∧ maxN > 0
                               then fromIntegral maxN / realToFrac fps ∷ Double
                               else 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usState ss ≢ Idle → (uts, ())
                | duration ≤ 0 → (uts, ())
                | otherwise →
                    let ss' = ss { usState       = Picking
                                 , usTarget      = Nothing
                                 , usLocalPath   = []
                                 , usPickupUntil = Just (now + duration)
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

handleUnitTransitionToCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                              → Pose → Int → IO ()
handleUnitTransitionToCommand env utsRef uid target stride = do
    -- Initiate a pose transition. Stride ≥ 2 skips frames at render
    -- time and proportionally shortens the duration — used by the AI
    -- when chaining multiple transitions back-to-back.
    let s = max 1 stride
    um  ← readIORef (unitManagerRef env)
    uts0 ← readIORef utsRef
    let mCurrentPose = usPose <$> HM.lookup uid (utsSimStates uts0)
        duration = case (HM.lookup uid (umInstances um), mCurrentPose) of
            (Just inst, Just curPose) →
                case HM.lookup (uiDefName inst) (umDefs um) of
                    Nothing  → 0
                    Just def →
                        let key      = stateKey curPose (TransitioningTo target)
                            animName = HM.lookupDefault key key (udStateAnims def)
                        in case HM.lookup animName (udAnimations def) of
                            Nothing → 0
                            Just a  →
                                let counts = V.length <$> Map.elems (aFrames a)
                                    maxN   = if null counts then 0 else maximum counts
                                    fps    = aFps a
                                    -- The renderer (Unit.Render.pickFrame)
                                    -- plays strided frames 0, s, 2s, … and
                                    -- clamps the last step to the destination
                                    -- frame (maxN-1). It first reaches that
                                    -- frame at raw step ceil((maxN-1)/s), and
                                    -- the transition must last ONE interval
                                    -- longer so that final frame is actually
                                    -- shown — expiry runs before the render
                                    -- publish, so a duration that ends exactly
                                    -- on that step publishes the target pose
                                    -- over it (truncating non-divisor strides,
                                    -- e.g. maxN=9, s=3 → 0,3,6,8). A stride
                                    -- larger than the whole animation has no
                                    -- in-between frames to show and collapses
                                    -- to an instant (zero-duration) transition.
                                    visible
                                      | s > maxN  = 0
                                      | otherwise = ((maxN - 1 + s - 1) `div` s) + 1
                                in if fps > 0 ∧ maxN > 0
                                   then fromIntegral visible / realToFrac fps ∷ Double
                                   else 0
            _ → 0
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss
                | usPose ss ≡ target → (uts, ())  -- already there
                | isTransitioning (usState ss) → (uts, ())  -- already mid-transition
                | duration ≤ 0 →
                    -- No frames to play (stride skipped past the whole
                    -- animation, or no transition anim exists): resolve
                    -- immediately to the target pose rather than forcing a
                    -- one-frame TransitioningTo state. Clear the move target
                    -- and local path just like the normal branch — otherwise
                    -- the unit keeps moving in the same tick after the
                    -- "instant" pose switch (commands run before
                    -- tickAllMovement), which can leave e.g. a crouching unit
                    -- walking.
                    let ss' = ss { usPose      = target
                                 , usState     = Idle
                                 , usTarget    = Nothing
                                 , usLocalPath = []
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())
                | otherwise →
                    let ss' = ss { usState             = TransitioningTo target
                                 , usTarget            = Nothing
                                 , usLocalPath         = []
                                 , usTransitionUntil   = Just (now + duration)
                                 , usTransitionStride  = s
                                 }
                    in (uts { utsSimStates = HM.insert uid ss' simStates }, ())
