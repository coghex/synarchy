-- Engine/Concurrent/Queue.hs
module Engine.Concurrent.Queue
  ( Queue
  , Channel
  , newQueue
  , writeQueue
  , tryReadQueue
  , newChannel
  , readChannel
  , writeChannel
  , tryReadChannel
  ) where

import UPrelude
import qualified Control.Concurrent.STM as STM

type Queue α = STM.TQueue α
type Channel α = STM.TChan α

newQueue ∷ IO (Queue α)
newQueue = STM.newTQueueIO

writeQueue ∷ Queue α → α → STM.STM ()
writeQueue = STM.writeTQueue

tryReadQueue ∷ Queue α → STM.STM (Maybe α)
tryReadQueue = STM.tryReadTQueue

newChannel ∷ IO (Channel α)
newChannel = STM.atomically STM.newTChan

readChannel ∷ Channel α → STM.STM α
readChannel = STM.readTChan

writeChannel ∷ Channel α → α → STM.STM ()
writeChannel = STM.writeTChan

tryReadChannel ∷ Channel α → STM.STM (Maybe α)
tryReadChannel = STM.tryReadTChan
