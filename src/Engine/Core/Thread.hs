module Engine.Core.Thread where

import UPrelude
import Control.Concurrent (threadDelay, ThreadId, killThread)
import Data.IORef (IORef, readIORef)
import Data.Foldable (forM_)

-- | Thread state with safe shutdown
data ThreadState = ThreadState
    { tsRunning  ∷ IORef ThreadControl
    , tsThreadId ∷ ThreadId }

-- | Thread state for control flow
data ThreadControl = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

-- | Wait for a thread to complete, ensuring graceful shutdown
waitThreadComplete ∷ ThreadState → IO ()
waitThreadComplete ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure () -- Thread has already stopped
        _ → do
            threadDelay 10000 -- Check every 10ms
            waitThreadComplete ts
