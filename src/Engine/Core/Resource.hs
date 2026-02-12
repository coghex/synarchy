{-# LANGUAGE StrictData, UnicodeSyntax #-}
module Engine.Core.Resource
  ( allocResource
  , allocResource'
  , allocResource'IO
  , locally
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Engine.Core.Monad (EngineM(..), EngineM')

-----------------------------------------------------------
-- Resource Management
-----------------------------------------------------------

allocResource ∷ (α → EngineM' ε ()) → EngineM ε σ α → EngineM ε σ α
allocResource free alloc = EngineM $ \e s c → unEngineM alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right a) ⌦ \r → r ⚟ unEngineM (free a) e s pure
{-# INLINE allocResource #-}

allocResource' ∷ (α → EngineM' ε ()) → EngineM ε σ α
  → EngineM ε σ (α, EngineM ε σ ())
allocResource' free alloc = EngineM $ \e s c → unEngineM alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right (a, EngineM $ \e' s' c'
    → c' (Right ()) ⌦ \r → r ⚟ unEngineM (free a) e' s' pure))
{-# INLINE allocResource' #-}

allocResource'IO ∷ (α → EngineM' ε ()) → EngineM ε σ α
                 → EngineM ε σ (α, IO ())
allocResource'IO free alloc = EngineM $ \e s c → unEngineM alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c $ Right (a, unEngineM (locally (free a)) e s $ \case
    Right _ → pure ()
    Left _ → pure ())
{-# INLINE allocResource'IO #-}

locally ∷ EngineM' ε α → EngineM ε σ α
locally p = EngineM $ \e s c → unEngineM p e s pure ⌦ c
{-# INLINE locally #-}
