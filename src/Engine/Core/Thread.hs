module Engine.Core.Thread
  ( waitThreadComplete -- Export threading utility functions
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.IORef (IORef, readIORef)
import Engine.Input.Thread (InputThreadState(..), ThreadState(..))
import Data.Foldable (forM_)

-- | Wait for a thread to complete, ensuring graceful shutdown
waitThreadComplete ∷ InputThreadState → IO ()
waitThreadComplete its = do
    tstate ← readIORef (itsRunning its)
    case tstate of
        ThreadStopped → pure () -- Thread has already stopped
        _ → do
            threadDelay 10000 -- Check every 10ms
            waitThreadComplete its
