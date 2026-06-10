{-# LANGUAGE StrictData, UnicodeSyntax #-}

module Engine.Core.Resource
  ( allocResource
  , allocResource'
  , allocResource'IO
  , locally
  ) where

import UPrelude
import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Data.IORef (readIORef)
import qualified Data.Text as T
import Engine.Core.Log (logError, LogCategory(..))
import Engine.Core.Monad (EngineM(..), EngineM')
import Engine.Core.State (loggerRef)

-- | Allocate a resource; the cleanup runs when the continuation exits —
--   on normal completion OR a (native IO) exception, via 'finally', so a
--   throw in the continuation can no longer leak the resource. Cleanup
--   runs exactly once and is masked against async exceptions. The
--   success path is unchanged (run continuation, then cleanup, return
--   the continuation's result).
allocResource ∷ (α → EngineM' ε ()) → EngineM ε σ α → EngineM ε σ α
allocResource free alloc = EngineM $ \e c → unEngineM alloc e $ \case
  Left ex → c (Left ex)
  Right a → c (Right a) `finally` unEngineM (free a) e pure
{-# INLINE allocResource #-}
-- | Like 'allocResource' but returns the cleanup action for manual
--   control. Invoking the action does NOT free immediately — it
--   positions the free in the unwind order at the invocation point:
--   it runs after the rest of the enclosing scope completes (on
--   success, CPS error, or native exception alike, via 'finally').
--   BufferUtils.createVulkanBuffer relies on this ordering to destroy
--   a buffer before the memory it is bound to is freed.
allocResource' ∷ (α → EngineM' ε ()) → EngineM ε σ α
  → EngineM ε σ (α, EngineM ε σ ())
allocResource' free alloc = EngineM $ \e c → unEngineM alloc e $ \case
  Left ex → c (Left ex)
  Right a → c (Right (a, EngineM $ \e' c'
    → c' (Right ()) `finally` unEngineM (free a) e' pure))
{-# INLINE allocResource' #-}
-- | Like 'allocResource'' but yields a plain @IO ()@ cleanup action
allocResource'IO ∷ (α → EngineM' ε ()) → EngineM ε σ α
                → EngineM ε σ (α, IO ())
allocResource'IO free alloc = EngineM $ \e c → unEngineM alloc e $ \case
  Left ex → c (Left ex)
  Right a → c $ Right (a, unEngineM (locally (free a)) e $ \case
    Right _ → pure ()
    Left err → do
      logger ← readIORef (loggerRef e)
      logError logger CatSystem $
        "resource cleanup failed: " <> T.pack (show err))
{-# INLINE allocResource'IO #-}
-- | Run a nested continuation locally, discarding its result type
locally ∷ EngineM' ε α → EngineM ε σ α
locally p = EngineM $ \e c → unEngineM p e pure ⌦ c
{-# INLINE locally #-}
