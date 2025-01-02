{-# LANGUAGE StrictData #-}

module Engine.Core.Resource
  ( allocResource
  , allocResource'
  , locally
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Engine.Core.Monad (EngineM(..), EngineM')

-- | allocates a resource and ensures cleanup
allocResource ∷ (α → EngineM' ε ()) → EngineM ε σ α → EngineM ε σ α
allocResource free alloc = EngineM $ \e s c → unEngineM alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right a) ⌦ \r → r ⚟ unEngineM (free a) e s pure
{-# INLINE allocResource #-}
-- | common case where we dont prepend
--   the release acton for finer control
allocResource' ∷ (α → EngineM' ε ()) → EngineM ε σ α
  → EngineM ε σ (α, EngineM ε σ ())
allocResource' free alloc = EngineM $ \e s c → unEngineM alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right (a, EngineM $ \e' s' c'
    → c' (Right ()) ⌦ \r → r ⚟ unEngineM (free a) e' s' pure))
{-# INLINE allocResource' #-}
-- | run nested continuations locally frees
--   all resources, only for side effects 
locally ∷ EngineM' ε α → EngineM ε σ α
locally p = EngineM $ \e s c → unEngineM p e s pure ⌦ c
{-# INLINE locally #-}
