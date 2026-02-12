module Engine.Core.Thread where

import UPrelude
import Control.Concurrent (threadDelay, ThreadId, killThread)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Foldable (forM_)

-----------------------------------------------------------
-- Thread Control
-----------------------------------------------------------

data ThreadState = ThreadState
    { tsRunning  ∷ IORef ThreadControl
    , tsThreadId ∷ ThreadId }

data ThreadControl = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

shutdownThread ∷ ThreadState → IO ()
shutdownThread ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure ()
        _ → do
            writeIORef (tsRunning ts) ThreadStopped
            waitThreadComplete ts

waitThreadComplete ∷ ThreadState → IO ()
waitThreadComplete ts = waitThreadComplete' 0 ts

waitThreadComplete' ∷ Int → ThreadState → IO ()
waitThreadComplete' i ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure ()
        _ → do
            threadDelay 10000
            if i ≥ 1000
                then killThread (tsThreadId ts)
                else waitThreadComplete' (i + 1) ts
