module Engine.Core.Thread where

import UPrelude
import Control.Concurrent (threadDelay, ThreadId, killThread)
import Data.IORef (IORef, readIORef, writeIORef)

-- | Thread state with safe shutdown
data ThreadState = ThreadState
    { tsRunning  ∷ IORef ThreadControl
    , tsThreadId ∷ ThreadId }

-- | Thread state for control flow
data ThreadControl = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

-- | shutdown a thread gracefully
shutdownThread ∷ ThreadState → IO ()
shutdownThread ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure () -- Thread has already stopped
        _ → do
            writeIORef (tsRunning ts) ThreadStopped
            waitThreadComplete ts

-- | Wait for a thread to complete, ensuring graceful shutdown
waitThreadComplete ∷ ThreadState → IO ()
waitThreadComplete ts = waitThreadComplete' 0 ts
waitThreadComplete' ∷ Int → ThreadState → IO ()
waitThreadComplete' i ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure () -- Thread has already stopped
        _ → do
            threadDelay 10000 -- Check every 10ms
            if i ≥ 1000 -- Timeout after 10 seconds
                then killThread (tsThreadId ts) -- Force kill
                else waitThreadComplete' (i + 1) ts
