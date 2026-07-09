{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Core.Queue where

import UPrelude
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue

-- | Thin wrapper around 'TQueue' for inter-thread communication
data Queue α = Queue
    { queueTQueue ∷ TQueue α
    } deriving (Eq)

newQueue ∷ IO (Queue α)
newQueue = Queue ⊚ STM.atomically newTQueue

writeQueue ∷ Queue α → α → IO ()
writeQueue q val = STM.atomically $ writeTQueue (queueTQueue q) val

readQueue ∷ Queue α → IO α
readQueue q = STM.atomically $ readTQueue (queueTQueue q)

tryReadQueue ∷ Queue α → IO (Maybe α)
tryReadQueue q = STM.atomically $ tryReadTQueue (queueTQueue q)

-- | Read with a timeout (microseconds). Don't wrap 'readQueue' in
--   'System.Timeout.timeout' instead: its exception can arrive after
--   the STM dequeue commits, silently dropping the message. Here the
--   dequeue and the timeout race inside a single transaction.
readQueueTimeout ∷ Int → Queue α → IO (Maybe α)
readQueueTimeout micros q = do
    delayVar ← STM.registerDelay micros
    STM.atomically $ STM.orElse
        (Just ⊚ readTQueue (queueTQueue q))
        (do timedOut ← STM.readTVar delayVar
            STM.check timedOut
            return Nothing)

flushQueue ∷ Queue α → IO [α]
flushQueue q = STM.atomically $ flushTQueue (queueTQueue q)

-- | Block until the queue is empty (its consumer has drained it) or
--   the timeout (microseconds) elapses; True = drained. Same
--   single-transaction race-free shape as 'readQueueTimeout'. Used by
--   the input.* injection verbs (#644) to ack that the input thread
--   has consumed the synthesized events.
waitQueueEmpty ∷ Int → Queue α → IO Bool
waitQueueEmpty micros q = do
    delayVar ← STM.registerDelay micros
    STM.atomically $ STM.orElse
        (do empty ← isEmptyTQueue (queueTQueue q)
            STM.check empty
            return True)
        (do timedOut ← STM.readTVar delayVar
            STM.check timedOut
            return False)
