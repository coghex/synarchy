{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Core.Queue where

import UPrelude
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue

-- | Queue type for thread communication
data Queue α = Queue
    { queueTQueue ∷ TQueue α    -- ^ Underlying STM TQueue
    } deriving (Eq)

-- | Create a new empty queue
newQueue ∷ IO (Queue α)
newQueue = Queue ⊚ STM.atomically newTQueue

-- | Write a value to a queue
writeQueue ∷ Queue α → α → IO ()
writeQueue q val = STM.atomically $ writeTQueue (queueTQueue q) val

-- | Read a value from a queue, blocking if empty
readQueue ∷ Queue α → IO α
readQueue q = STM.atomically $ readTQueue (queueTQueue q)

-- | Try to read from a queue, returning Nothing if empty
tryReadQueue ∷ Queue α → IO (Maybe α)
tryReadQueue q = STM.atomically $ tryReadTQueue (queueTQueue q)

-- | Read all available items from a queue
flushQueue ∷ Queue α → IO [α]
flushQueue q = STM.atomically $ flushTQueue (queueTQueue q)

type Channel α = STM.TChan α

newChannel ∷ IO (Channel α)
newChannel = STM.atomically STM.newTChan

readChannel ∷ Channel α → STM.STM α
readChannel = STM.readTChan

writeChannel ∷ Channel α → α → STM.STM ()
writeChannel = STM.writeTChan

tryReadChannel ∷ Channel α → STM.STM (Maybe α)
tryReadChannel = STM.tryReadTChan
