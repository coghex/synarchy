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
import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Anim (stateKey, resolveStateAnim, poseTag)
import Unit.Command.Types (UnitCommand(..))
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
    threadId ← catch
        (do
            logInfo logger CatThread "Starting unit thread..."
            lastTimeRef ← getPOSIXTime ⌦ newIORef . realToFrac
            -- utsRef now lives on EngineEnv (Phase 4 of save/load v2) so
            -- the world thread can read+write sim state at save/load.
            tid ← forkIO $ unitLoop env stateRef lastTimeRef (utsRef env)
            logInfo logger CatThread "Unit thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatThread $ "Failed starting unit thread: "
                <> T.pack (show e)
            error "Unit thread start failure."
        )
    return $ ThreadState stateRef threadId

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
            catch
              (do
                tickStart ← realToFrac ⊚ getPOSIXTime
                lastTime ← readIORef lastTimeRef
                let dt = tickStart - lastTime
                writeIORef lastTimeRef tickStart

                processAllUnitCommands env utsRef
                paused ← readIORef (enginePausedRef env)
                unless paused $ do
                    modifyIORef' (gameTimeRef env) (+ dt)
                    tickAllMovement dt env utsRef
                publishToRender env utsRef
                processAllBuildingCommands env

                tickEnd ← realToFrac ⊚ getPOSIXTime
                let elapsed = tickEnd - tickStart ∷ Double
                    sleepTime = max 0 (unitTickRate - elapsed)
                threadDelay (floor (sleepTime * 1000000))
                unitLoop env stateRef lastTimeRef utsRef
              )
              (\(e ∷ SomeException) → do
                logger ← readIORef (loggerRef env)
                logError logger CatThread $ "Unit thread crashed: " <> T.pack (show e)
                writeIORef (lifecycleRef env) CleaningUp
              )

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
                                let targetAnim = case HM.lookup (uiDefName inst) defs of
                                        Just def → resolveStateAnim def
                                                       (stateKey (usPose ss) (usState ss))
                                        Nothing  → uiCurrentAnim inst
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
                                        , uiFacing      = usFacing ss
                                        , uiCurrentAnim = targetAnim
                                        , uiAnimStart   = if samePlayback
                                                          then uiAnimStart inst
                                                          else now
                                        , uiAnimReverse = newReverse
                                        , uiActivity    = activityLabel (usState ss)
                                        , uiPose        = poseTag (usPose ss)
                                        , uiAnimStride  = newStride
                                        }
                      ) (umInstances um)
                in (um { umInstances = updated }, ())

-- | Stable string labels for UnitActivity. Lua reads these via
--   `unit.getActivity`. Pose is exposed separately via `unit.getPose`.
activityLabel ∷ UnitActivity → Text
activityLabel Idle              = "idle"
activityLabel Walking           = "walking"
activityLabel Drinking          = "drinking"
activityLabel Eating            = "eating"
activityLabel Picking           = "pickup"
activityLabel (TransitioningTo _) = "transitioning"
