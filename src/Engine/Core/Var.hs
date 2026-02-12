-- Engine/Concurrent/Var.hs
module Engine.Core.Var
  ( Var
  , newVar
  , readVar
  , writeVar
  , modifyVar
  , modifyVar'
  , atomically
  , dupVar
  ) where

import UPrelude
import qualified Control.Concurrent.STM as STM

type Var α = STM.TVar α

newVar ∷ α → STM.STM (Var α)
newVar = STM.newTVar

readVar ∷ Var α → STM.STM α
readVar = STM.readTVar

writeVar ∷ Var α → α → STM.STM ()
writeVar = STM.writeTVar

modifyVar ∷ Var α → (α → (α, β)) → STM.STM β
modifyVar ref f = do
  a ← readVar ref
  let (!a', !b) = f a
  writeVar ref a'
  pure b

modifyVar' ∷ Var α → (α → α) → STM.STM ()
modifyVar' = STM.modifyTVar'

atomically ∷ STM.STM α → IO α
atomically = STM.atomically

dupVar ∷ Var a → IO (Var a)
dupVar v = atomically $ readVar v ⌦ newVar ⌦ return
