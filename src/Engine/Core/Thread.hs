module Engine.Core.Thread where

import UPrelude
import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.IORef (IORef, readIORef, writeIORef)
import System.Timeout (timeout)

data ThreadState = ThreadState
    { tsRunning  ∷ IORef ThreadControl
    , tsThreadId ∷ ThreadId
    , tsDone     ∷ MVar ()
      -- ^ Filled exactly once when the thread's loop actually exits —
      --   via 'finally' at the fork site, so it covers a clean stop, a
      --   self-crash, and an async 'killThread' alike. Distinct from
      --   'tsRunning' (the stop *request*) so 'shutdownThread' genuinely
      --   waits instead of reading back the flag it just set.
    }

data ThreadControl = ThreadRunning | ThreadPaused | ThreadStopped
    deriving (Show, Eq)

-- | Signal stop and block until the thread's loop actually exits, up to
--   a 10 s timeout, then force-kill. Idempotent: a second call once the
--   thread is already stopped returns immediately (no re-wait).
shutdownThread ∷ ThreadState → IO ()
shutdownThread ts = do
    tstate ← readIORef (tsRunning ts)
    case tstate of
        ThreadStopped → pure ()
        _ → do
            writeIORef (tsRunning ts) ThreadStopped
            result ← timeout (10 * 1000000) (takeMVar (tsDone ts))
            case result of
                Just () → pure ()
                Nothing → killThread (tsThreadId ts)
