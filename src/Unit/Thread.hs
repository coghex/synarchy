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
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Sim.Types
import Unit.Command.Types (UnitCommand(..))
import Unit.Thread.Command (processAllUnitCommands)

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

-- | Unit thread tick rate in seconds. 30 Hz.
unitTickRate ∷ Double
unitTickRate = 1.0 / 30.0

-----------------------------------------------------------
-- Start Unit Thread
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Unit Loop
-----------------------------------------------------------

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
            tickStart ← realToFrac ⊚ getPOSIXTime
            lastTime ← readIORef lastTimeRef
            let _dt = tickStart - lastTime ∷ Double
            writeIORef lastTimeRef tickStart

            -- 1. Process commands (spawn, destroy, teleport, moveTo, stop)
            processAllUnitCommands env utsRef

            -- 2. [Future] Tick movement interpolation
            --    tickAllMovement env dt utsRef

            -- 3. Publish changed positions to unitManagerRef
            publishToRender env utsRef

            -- Sleep to maintain tick rate
            tickEnd ← realToFrac ⊚ getPOSIXTime
            let elapsed = tickEnd - tickStart ∷ Double
                sleepTime = max 0 (unitTickRate - elapsed)
            threadDelay (floor (sleepTime * 1000000))
            unitLoop env stateRef lastTimeRef utsRef

-----------------------------------------------------------
-- Publish sim state to render-visible UnitManager
-----------------------------------------------------------

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
                        Just ss → inst { uiGridX = usRealX ss
                                       , uiGridY = usRealY ss
                                       , uiGridZ = usGridZ ss
                                       }
                  ) (umInstances um)
            in (um { umInstances = updated }, ())
