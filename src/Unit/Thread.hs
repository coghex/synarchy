{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread
    ( startUnitThread
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef, newIORef, atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Command.Types (UnitCommand(..))
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Movement (tickAllMovement)

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
            utsRef ← newIORef emptyUnitThreadState
            tid ← forkIO $ unitLoop env stateRef lastTimeRef utsRef
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
                tickAllMovement dt utsRef
                publishToRender env utsRef

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
publishToRender ∷ EngineEnv → IORef UnitThreadState → IO ()
publishToRender env utsRef = do
    uts ← readIORef utsRef
    let simStates = utsSimStates uts
    if HM.null simStates
        then return ()
        else atomicModifyIORef' (unitManagerRef env) $ \um →
            let updated = HM.mapWithKey (\uid inst →
                    case HM.lookup uid simStates of
                        Nothing → inst
                        Just ss → inst { uiGridX  = usRealX ss
                                       , uiGridY  = usRealY ss
                                       , uiGridZ  = usGridZ ss
                                       , uiFacing = usFacing ss
                                       }
                  ) (umInstances um)
            in (um { umInstances = updated }, ())
