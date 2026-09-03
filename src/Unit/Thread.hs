{-# LANGUAGE Strict #-}
module Unit.Thread
    ( startUnitThread
    , unitTickRate
    , UnitTickSeams(..)
    , productionUnitTickSeams
    , unitTickWith
    ) where

import UPrelude
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView (toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef, newIORef, atomicModifyIORef'
                  , modifyIORef')
import Control.Concurrent (threadDelay)
import Engine.Core.Clock (monotonicSeconds, sampleElapsed, sanitiseElapsed)
import Engine.Core.Thread
    (ThreadState, WorkerFailLevel(..), WorkerSpec(..), noRefusal
    , startWorkerThread)
import Engine.Core.State
    (EngineEnv, EngineLifecycle(..), lifecycleRef, loggerRef, saveBarrierRef)
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, ownersGated)
import Engine.Core.Log (logDebug, logError, LogCategory(..))
import Unit.Types
import Unit.Sim.Types
import Unit.Anim (stateKey, resolveStateAnim, poseTag, chooseAnim)
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Movement (tickAllMovement)
import Building.Thread.Command (processAllBuildingCommands)

-- | Unit thread tick rate in seconds (30 Hz).
unitTickRate ∷ Double
unitTickRate = 1.0 / 30.0

-- | The unit tick's injectable seams (#2204): the clock it samples, the
--   movement integrator it hands the sanitised @dt@ to, and the sleep it
--   paces itself with. Production uses 'productionUnitTickSeams'; the
--   headless gate substitutes a scripted clock and recording movement /
--   sleep so the bound on each can be asserted directly rather than
--   inferred from a unit's displacement.
data UnitTickSeams = UnitTickSeams
    { tickClock    ∷ IO Double
      -- ^ Elapsed-time source, seconds. 'monotonicSeconds' in production.
    , tickMovement ∷ Double → EngineEnv → IORef UnitThreadState → IO ()
      -- ^ 'tickAllMovement' in production; receives the SANITISED @dt@.
    , tickSleep    ∷ Int → IO ()
      -- ^ 'threadDelay' in production, microseconds.
    }

productionUnitTickSeams ∷ UnitTickSeams
productionUnitTickSeams = UnitTickSeams
    { tickClock    = monotonicSeconds
    , tickMovement = tickAllMovement
    , tickSleep    = threadDelay
    }

startUnitThread ∷ EngineEnv → IO ThreadState
startUnitThread env = startWorkerThread WorkerSpec
    { wsName        = "Unit"
    , wsLoggerRef   = loggerRef env
    , wsCategory    = CatThread
    , wsStartingMsg = "Starting unit thread..."
    , wsStartedMsg  = Just "Unit thread started"
    , wsFailMsg     = "Failed starting unit thread: "
    , wsFailLevel   = WorkerFailError
    , wsFailFatal   = "Unit thread start failure."
    , wsStartup     = \_ → noRefusal $ do
        lastTimeRef ← monotonicSeconds ⌦ newIORef
        -- utsRef now lives on EngineEnv (Phase 4 of save/load v2) so
        -- the world thread can read+write sim state at save/load.
        let uts = ucUtsRef (toUnitCombatCapability env)
        pure (lastTimeRef, uts)
    , wsTick        = uncurry (unitTickWith productionUnitTickSeams env)
    , wsOnStop      = \_ → do
        logger ← readIORef (loggerRef env)
        logDebug logger CatThread "Unit thread stopping..."
    , wsOnCrash     = \_ e → do
        logger ← readIORef (loggerRef env)
        logError logger CatThread $ "Unit thread crashed: " <> tshow e
        writeIORef (lifecycleRef env) CleaningUp
    }

-- | One unit tick against the given seams (#2204). The simulation @dt@
--   is the SANITISED elapsed since the previous raw sample (which
--   'sampleElapsed' replaces unconditionally, dropping any excess above
--   the shared cap), so 'wsGameTimeRef' and movement each advance by at
--   most 'Engine.Core.Clock.maxElapsedStep' per tick. The execution
--   measurement that paces the sleep at the end goes through the same
--   sanitiser: a negative or NaN difference would otherwise turn into an
--   over-long or arbitrary 'threadDelay'.
unitTickWith ∷ UnitTickSeams → EngineEnv → IORef Double → IORef UnitThreadState
             → IO (Maybe (IORef Double, IORef UnitThreadState))
unitTickWith seams env lastTimeRef utsRef = do
    dt ← sampleElapsed (tickClock seams) lastTimeRef
    tickStart ← readIORef lastTimeRef

    -- #2221: the per-OWNER gate rather than the global capture lock,
    -- so this loop's gated work stops at its OWN final-pass
    -- acknowledgement instead of waiting for the boundary the initiator
    -- only declares once every other owner has acknowledged too.
    -- Asked for BOTH owners this loop answers for, in one reading of
    -- the barrier: they acknowledge together below, and the pair must
    -- never disagree about whether this tick's work may run. The
    -- acknowledgements themselves stay unconditional — parking is a
    -- gate on WORK, never a block before an acknowledgement, so
    -- 'SaveBuilding' can still ack after 'SaveUnit' has parked the loop.
    locked ← ownersGated (saveBarrierRef env) [SaveUnit, SaveBuilding]
    unless locked $ processAllUnitCommands env utsRef
    paused ← readIORef (wsEnginePausedRef (toWorldSimCapability env))
    unless paused $ do
        modifyIORef' (wsGameTimeRef (toWorldSimCapability env)) (+ dt)
        tickMovement seams dt env utsRef
    -- Issue #763: a load publish
    -- (World.Load.Publish.publishStagedSession) swaps
    -- unitManagerRef and utsRef itself while THIS thread is
    -- meant to be fully quiesced (SaveUnit) — but
    -- publishToRender was never gated on 'locked' the way
    -- every other write below is, so it could previously
    -- copy STALE utsRef sim state onto the freshly-swapped
    -- unitManagerRef mid-publish (or the old unitManagerRef
    -- with freshly-swapped utsRef), corrupting a reused unit
    -- id's render-facing pose/anim/position with data from
    -- the session being replaced. A save never writes either
    -- ref, so gating this costs nothing there beyond a
    -- render-state update pausing for the same brief window
    -- 'processAllUnitCommands' already skips.
    unless locked $ publishToRender env utsRef
    -- Buildings have no thread of their own (§2.2 of the
    -- capability inventory), so their queue is drained
    -- here — still outside the pause-only movement block,
    -- still inside the save barrier's `unless locked`
    -- gate, and still before BOTH acknowledgements below.
    -- Since #896 the drain takes the narrow building
    -- capability plus the logger and world/sim view rather
    -- than this thread's whole environment.
    unless locked $ processAllBuildingCommands
        (loggerRef env)
        (toWorldSimCapability env)
        (toContentRegistriesViewCapability env)
        (toBuildingCapability env)
    acknowledgeCurrent (saveBarrierRef env) SaveUnit
    acknowledgeCurrent (saveBarrierRef env) SaveBuilding

    tickEnd ← tickClock seams
    let elapsed = sanitiseElapsed (tickEnd - tickStart)
        sleepTime = max 0 (unitTickRate - elapsed)
    tickSleep seams (floor (sleepTime * 1000000))
    pure (Just (lastTimeRef, utsRef))

-- | Copy sim-thread positions/facing into the render-visible UnitManager.
--   Also drives unit animations: the resolved anim for (usPose, usState)
--   is stamped onto the instance. uiAnimStart resets only when the
--   anim name OR the reverse flag changes (so refacing or re-entering
--   the same activity doesn't restart frame 0).
publishToRender ∷ EngineEnv → IORef UnitThreadState → IO ()
publishToRender env utsRef = do
    uts ← readIORef utsRef
    let simStates = utsSimStates uts
    if HM.null simStates
        then return ()
        else do
            now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
            atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                let defs = umDefs um
                    updated = HM.mapWithKey (\uid inst →
                        case HM.lookup uid simStates of
                            Nothing → inst
                            Just ss
                              -- Debug freeze: skip the sim-derived
                              -- update so Lua's setAnim / setFacing /
                              -- setPos aren't stomped. Used by the
                              -- debug anim panel's preview-cycle.
                              | uiFrozen inst → inst
                              | otherwise →
                                let -- Lua-driven anim override (combat
                                    -- swings, posture changes, etc.)
                                    -- wins over the state-driven map.
                                    -- Empty string = no override.
                                    override = uiAnimOverride inst
                                    -- Cumulative EFFECTIVE wound severity
                                    -- (heal eases it, necrosis floors it).
                                    -- The injured-anim swap fires above the
                                    -- same threshold the Lua-side
                                    -- combatAnimName helper uses (1.0), and
                                    -- on the same per-wound value the Lua
                                    -- `unit.getWounds` severity reports, so
                                    -- the engine and Lua sides stay in
                                    -- lockstep as a wound heals.
                                    woundSev = sum (map woundEffSeverity
                                                        (uiWounds inst))
                                    injured = woundSev > 1.0
                                    baseKey = stateKey (usPose ss) (usState ss)
                                    -- Resolve via two-tier lookup: an
                                    -- injured-prefixed key first if the
                                    -- unit qualifies, falling back to the
                                    -- plain state key. The yaml may or
                                    -- may not have an injured- variant
                                    -- registered for any given state.
                                    resolveAnim def =
                                        let injK = "injured-" <> baseKey
                                            injR = resolveStateAnim def injK
                                        in if injured ∧ injR ≢ injK
                                              then injR
                                              else resolveStateAnim def baseKey
                                    stateAnim =
                                          case HM.lookup (uiDefName inst) defs of
                                              Just def → resolveAnim def
                                              Nothing  → uiCurrentAnim inst
                                    -- Precedence (see 'chooseAnim'): a
                                    -- Dead unit always shows its death
                                    -- animation, even if a Lua combat
                                    -- override was still set when it was
                                    -- killed; otherwise the override wins.
                                    targetAnim =
                                        chooseAnim (usPose ss) override stateAnim
                                    -- Transition assets are shared between
                                    -- forward and reverse: standing-to-crouching
                                    -- plays normally for Standing→Crouching, and
                                    -- the same asset reversed for Crouching→
                                    -- Standing. Reverse is detected by depth:
                                    -- moving toward lower depth = reverse.
                                    newReverse = case usState ss of
                                        TransitioningTo target →
                                            poseDepth (usPose ss) > poseDepth target
                                        _ → False
                                    newStride = case usState ss of
                                        TransitioningTo _ → usTransitionStride ss
                                        _                 → 1
                                    samePlayback = targetAnim ≡ uiCurrentAnim inst
                                                 ∧ newReverse ≡ uiAnimReverse inst
                                                 ∧ newStride  ≡ uiAnimStride  inst
                                in inst { uiGridX       = usRealX ss
                                        , uiGridY       = usRealY ss
                                        , uiGridZ       = usGridZ ss
                                        , uiRealZ       = usRealZ ss
                                        , uiFacing      = usFacing ss
                                        , uiCurrentAnim = targetAnim
                                        , uiAnimStart   = if samePlayback
                                                          then uiAnimStart inst
                                                          else now
                                        , uiAnimReverse = newReverse
                                        , uiActivity    = activityLabel (usState ss)
                                        , uiPose        = poseTag (usPose ss)
                                        , uiAnimStride  = newStride
                                        -- Cliff column being climbed (for
                                        -- far-face occlusion in the renderer);
                                        -- set only while the climb fields are
                                        -- live (Climbing→pullup), else Nothing.
                                        , uiClimbDest   = case usClimbToTile ss of
                                            Just (tx, ty, _) →
                                                Just (floor tx, floor ty)
                                            Nothing → Nothing
                                        }
                      ) (umInstances um)
                in (um { umInstances = updated }, ())

-- | Stable string labels for UnitActivity. Lua reads these via
--   `unit.getActivity`. Pose is exposed separately via `unit.getPose`.
activityLabel ∷ UnitActivity → Text
activityLabel Idle              = "idle"
activityLabel Walking           = "walking"
activityLabel Running           = "running"
activityLabel Drinking          = "drinking"
activityLabel Eating            = "eating"
activityLabel Picking           = "pickup"
activityLabel (TransitioningTo _) = "transitioning"
