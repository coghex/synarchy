{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread
    ( startUnitThread
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef, newIORef, atomicModifyIORef'
                  , modifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, captureLocked)
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..))
import Unit.Types
import Unit.Sim.Types
import Unit.Anim (stateKey, resolveStateAnim, poseTag, chooseAnim)
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Movement (tickAllMovement)
import Building.Thread.Command (processAllBuildingCommands)

-- | Unit thread tick rate in seconds (30 Hz).
unitTickRate ∷ Double
unitTickRate = 1.0 / 30.0

startUnitThread ∷ EngineEnv → IO ThreadState
startUnitThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do
            logInfo logger CatThread "Starting unit thread..."
            lastTimeRef ← getPOSIXTime ⌦ newIORef . realToFrac
            -- utsRef now lives on EngineEnv (Phase 4 of save/load v2) so
            -- the world thread can read+write sim state at save/load.
            tid ← forkIO $ unitLoop env stateRef lastTimeRef (utsRef env) `finally` putMVar doneVar ()
            logInfo logger CatThread "Unit thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatThread $ "Failed starting unit thread: "
                <> T.pack (show e)
            error "Unit thread start failure."
        )
    return $ ThreadState stateRef threadId doneVar

unitLoop ∷ EngineEnv → IORef ThreadControl → IORef Double
         → IORef UnitThreadState → IO ()
unitLoop env stateRef lastTimeRef utsRef = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatThread "Unit thread stopping..."
            pure ()
        ThreadPaused → do
            threadDelay 100000
            unitLoop env stateRef lastTimeRef utsRef
        ThreadRunning → do
            -- One guarded tick per iteration; the recursive call lives
            -- OUTSIDE the catch — inside it, each tick pushes a catch
            -- frame that never pops (unbounded stack growth).
            ok ← catch
              (do
                tickStart ← realToFrac ⊚ getPOSIXTime
                lastTime ← readIORef lastTimeRef
                let dt = tickStart - lastTime
                writeIORef lastTimeRef tickStart

                locked ← captureLocked (saveBarrierRef env)
                unless locked $ processAllUnitCommands env utsRef
                paused ← readIORef (enginePausedRef env)
                unless paused $ do
                    modifyIORef' (gameTimeRef env) (+ dt)
                    tickAllMovement dt env utsRef
                -- Round 3 review (issue #763): a load publish
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
                unless locked $ processAllBuildingCommands env
                acknowledgeCurrent (saveBarrierRef env) SaveUnit
                acknowledgeCurrent (saveBarrierRef env) SaveBuilding

                tickEnd ← realToFrac ⊚ getPOSIXTime
                let elapsed = tickEnd - tickStart ∷ Double
                    sleepTime = max 0 (unitTickRate - elapsed)
                threadDelay (floor (sleepTime * 1000000))
                pure True
              )
              (\(e ∷ SomeException) → do
                logger ← readIORef (loggerRef env)
                logError logger CatThread $ "Unit thread crashed: " <> T.pack (show e)
                writeIORef (lifecycleRef env) CleaningUp
                pure False
              )
            when ok $ unitLoop env stateRef lastTimeRef utsRef

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
            now ← readIORef (gameTimeRef env)
            atomicModifyIORef' (unitManagerRef env) $ \um →
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
                                        in if injured ∧ injR ≠ injK
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
