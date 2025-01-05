{-# LANGUAGE Strict #-}
module Engine.Core.Queue where

import UPrelude
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)

-- | Queue type for thread communication
data Queue α = Queue
    { queueTQueue ∷ TQueue α    -- ^ Underlying STM TQueue
    }

-- | Create a new empty queue
newQueue ∷ IO (Queue α)
newQueue = Queue <$> atomically newTQueue

-- | Write a value to a queue
writeQueue ∷ Queue α → α → IO ()
writeQueue q val = atomically $ writeTQueue (queueTQueue q) val

-- | Try to read from a queue, returning Nothing if empty
tryReadQueue ∷ Queue α → IO (Maybe α)
tryReadQueue q = atomically $ tryReadTQueue (queueTQueue q)

-- | Read all available items from a queue
flushQueue ∷ Queue α → IO [α]
flushQueue q = atomically $ flushTQueue (queueTQueue q)
