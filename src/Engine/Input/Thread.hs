{-# LANGUAGE Strict #-}
module Engine.Input.Thread where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay, ThreadId, killThread, forkIO)
import Control.Exception (SomeException, catch)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Engine.Core.Types (EngineEnv(..))
import Engine.Input.Types
import Engine.Input.Callback
import Engine.Core.Queue as Q

-- | Thread state with safe shutdown
data InputThreadState = InputThreadState
    { itsRunning :: IORef ThreadState
    , itsThreadId :: ThreadId }

-- | Thread state for control flow
data ThreadState = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

-- | Start the input processing thread
startInputThread ∷ EngineEnv → IO InputThreadState
startInputThread env = do
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            Q.writeQueue (logQueue env) "Starting input thread..."
            tid ← forkIO $ runInputLoop env defaultInputState ThreadRunning
            return tid
        ) 
        (\(e :: SomeException) → do
            Q.writeQueue (logQueue env) $ T.pack $
                "Failed to start input thread: " ⧺ show e
            error "Input thread failed to start"
        )
    return $ InputThreadState stateRef threadId

-- | Main input processing loop with timing control
runInputLoop ∷ EngineEnv → InputState → ThreadState → IO ()
runInputLoop _   _     ThreadStopped = return ()
runInputLoop env inpSt ThreadPaused  = do
    threadDelay 100000  -- 100ms pause check
    runInputLoop env inpSt ThreadRunning
runInputLoop env inpSt ThreadRunning = do
    -- Start frame timing
    frameStart ← getCurrentTime
    
    -- Process all pending inputs
    newInpSt ← processInputs env inpSt
    
    -- Calculate frame time and delay to maintain consistent rate
    frameEnd ← getCurrentTime
    let diff  = diffUTCTime frameEnd frameStart
        usecs = floor (toRational diff * 1000000) ∷ Int
        targetFrameTime = 1000  -- 1ms target frame time
        delay = targetFrameTime - usecs
    
    -- Only delay if we're running faster than target
    when (delay > 0) $ threadDelay delay
    
    -- Continue loop
    runInputLoop env newInpSt ThreadRunning

-- | Process all queued inputs
processInputs ∷ EngineEnv → InputState → IO InputState
processInputs env inpSt = do
    mEvent ← tryReadQueue (inputQueue env)
    case mEvent of
        Just event → do
            newState ← processInput env inpSt event
            processInputs env newState
        Nothing → return inpSt

-- | Process individual input events
processInput ∷ EngineEnv → InputState → InputEvent → IO InputState
processInput env inpSt event = case event of
    InputKeyEvent key keyState mods →
        return $ updateKeyState inpSt key keyState mods
    InputMouseEvent btn pos state →
        return $ updateMouseState inpSt btn pos state
    InputWindowEvent winEv →
        return $ updateWindowState inpSt winEv
    InputScrollEvent x y →
        return $ updateScrollState inpSt x y

-- | Helper state update functions
updateKeyState ∷ InputState → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → InputState
updateKeyState state key keyState mods = state
    { inpKeyStates = Map.insert key newKeyState (inpKeyStates state) }
    where
        newKeyState = KeyState
            { keyPressed = keyState ≡ GLFW.KeyState'Pressed
            , keyMods = mods
            , keyTime = 0.0
            }

updateMouseState ∷ InputState → GLFW.MouseButton → (Double, Double) → GLFW.MouseButtonState → InputState
updateMouseState state btn pos btnState = state
    { inpMousePos = pos
    , inpMouseBtns = Map.insert btn (btnState ≡ GLFW.MouseButtonState'Pressed) (inpMouseBtns state)
    }

updateWindowState ∷ InputState → WindowEvent → InputState
updateWindowState state (WindowResize w h) = state { inpWindowSize = (w, h) }
updateWindowState state (WindowFocus focused) = state { inpWindowFocused = focused }
updateWindowState state _ = state

updateScrollState ∷ InputState → Double → Double → InputState
updateScrollState state x y = state { inpScrollDelta = (x, y) }

-- | Safely shutdown input thread
shutdownInputThread ∷ EngineEnv → InputThreadState → IO ()
shutdownInputThread env its = catch (do
    -- Signal thread to stop
    writeIORef (itsRunning its) ThreadStopped
    Q.writeQueue (logQueue env) "Input thread signaled to stop..."
    
    -- Give thread time to cleanup and exit gracefully
    threadDelay 50000  -- 50ms should be enough for cleanup
    
    -- Check if thread is still alive and force kill if necessary
    writeIORef (itsRunning its) ThreadStopped
    killThread (itsThreadId its)
    Q.writeQueue (logQueue env) "Input thread shutdown complete."
    ) $ \(e :: SomeException) → do
        -- Log any errors during shutdown
        Q.writeQueue (logQueue env) $ T.pack $
            "Error shutting down input thread: " ⧺ show e
        -- Force kill thread if graceful shutdown failed
        killThread (itsThreadId its)
