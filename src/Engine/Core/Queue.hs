{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Core.Queue where

import UPrelude
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue

-----------------------------------------------------------
-- Queue
-----------------------------------------------------------

data Queue α = Queue
    { queueTQueue ∷ TQueue α    -- ^ Underlying STM TQueue
    }

newQueue ∷ IO (Queue α)
newQueue = Queue ⊚ STM.atomically newTQueue

writeQueue ∷ Queue α → α → IO ()
writeQueue q val = STM.atomically $ writeTQueue (queueTQueue q) val

readQueue ∷ Queue α → IO α
readQueue q = STM.atomically $ readTQueue (queueTQueue q)

tryReadQueue ∷ Queue α → IO (Maybe α)
tryReadQueue q = STM.atomically $ tryReadTQueue (queueTQueue q)

flushQueue ∷ Queue α → IO [α]
flushQueue q = STM.atomically $ flushTQueue (queueTQueue q)

-----------------------------------------------------------
-- Channel
-----------------------------------------------------------

type Channel α = STM.TChan α

newChannel ∷ IO (Channel α)
newChannel = STM.atomically STM.newTChan

readChannel ∷ Channel α → STM.STM α
readChannel = STM.readTChan

writeChannel ∷ Channel α → α → STM.STM ()
writeChannel = STM.writeTChan

tryReadChannel ∷ Channel α → STM.STM (Maybe α)
tryReadChannel = STM.tryReadTChan
