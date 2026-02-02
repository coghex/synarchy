{-# LANGUAGE StrictData #-}

module Engine.Core.Resource
  ( allocResource
  , allocResource'
  , allocResource'IO
  , locally
  ) where

import UPrelude
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Engine.Core.Monad (EngineM(..), EngineM')
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logWarnM)

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
 -- | Version of allocResource' that returns an IO cleanup action
allocResource'IO :: (α → EngineM' ε ()) → EngineM ε σ α 
                → EngineM ε σ (α, IO ())
allocResource'IO free alloc = EngineM $ \e s c → 
  unEngineM alloc e s $ \case
    Left ex → c (Left ex)
    Right a → 
      unEngineM (logDebugM CatResource "Resource allocated with IO cleanup action") e s $ \_ →
        c $ Right (a, unEngineM (locally (free a)) e s $ \case
          Right _ → unEngineM (logDebugM CatResource "Resource cleanup completed successfully") e s pure
          Left ex → unEngineM (logWarnM CatResource $ "Resource cleanup failed (ignored): " <> T.pack (show ex)) e s pure)
{-# INLINE allocResource'IO #-}
-- | run nested continuations locally frees
--   all resources, only for side effects 
locally ∷ EngineM' ε α → EngineM ε σ α
locally p = EngineM $ \e s c → 
  unEngineM (logDebugM CatResource "Executing locally scoped resource cleanup") e s $ \_ →
    unEngineM p e s pure ⌦ c
{-# INLINE locally #-}
