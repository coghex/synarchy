module Engine.Core.Monad
  ( EngineM(..)
  , EngineM'
  , runEngineM
  , MonadIO(..)
  , MonadError(..)
  , modifyGraphicsState
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Error.Exception (EngineException(..))
import Engine.Core.State

-- | CPS monad with Reader ('EngineEnv'), State ('EngineState'), IO, and
--   error handling. Type params: σ = continuation result, α = value.
--   The Reader environment and the state are not parameters — they are
--   concretely 'EngineEnv' and 'EngineState' (see the 'MonadReader' and
--   'MonadState' instances below). Capability narrowing happens through
--   the explicit projected records in @Engine.Core.Capability.*@
--   (issues #537\/#889), never by varying this monad's environment.
--
--   The env is an immutable value — its IORef\/queue handles never
--   change — so 'ask' returns it directly with no STM. 'EngineState'
--   lives behind 'engineStateRef' inside the env; only the main thread
--   runs this monad, so the IORef-backed state needs no synchronisation.
newtype EngineM σ α = EngineM
  { unEngineM ∷ EngineEnv
              → (Either EngineException α → IO σ)
              → IO σ
  }

-- | Specialised alias: continuation returns @Either EngineException α@
type EngineM' α = EngineM (Either EngineException α) α

runEngineM ∷ EngineM σ α
         → EngineEnv
         → (Either EngineException α → IO σ)
         → IO σ
runEngineM action env cont = unEngineM action env cont

instance Functor (EngineM σ) where
  fmap f m = EngineM $ \e c → unEngineM m e (c ∘ fmap f)

instance Applicative (EngineM σ) where
  pure x = EngineM $ \_ → ($ Right x)
  -- (<*>) must agree with ap: when mf has failed, mx's effects must
  -- not run — otherwise forM/mapM/traverse keep executing effects
  -- past a throwError (results discarded, side effects not).
  mf <*> mx = EngineM $ \e c → unEngineM mf e $ \case
    Left ex → c (Left ex)
    Right g → unEngineM mx e (c ∘ fmap g)

instance Monad (EngineM σ) where
  return = pure
  mx >>= k = EngineM $ \e c → unEngineM mx e $ \case
    Right x → unEngineM (k x) e c
    Left ex → c (Left ex)

instance MonadIO (EngineM σ) where
  liftIO m = EngineM $ \_ c → m ⌦ (c ∘ Right)

instance MonadError EngineException (EngineM σ) where
  throwError e = EngineM $ \_ c → c (Left e)
  catchError action handler = EngineM $ \e c →
    unEngineM action e $ \case
      Left err → unEngineM (handler err) e c
      Right r  → c (Right r)

instance MonadReader EngineEnv (EngineM σ) where
  ask = EngineM $ \e c → c (Right e)
  -- Pure Reader-local: run the sub-computation under @f e@; the
  -- original @e@ is unaffected for everything outside this call, so
  -- it is automatically restored and exception-safe (unlike the old
  -- TVar-mutating version). Currently unused but now correct.
  local f action = EngineM $ \e c → unEngineM action (f e) c

instance MonadState EngineState (EngineM σ) where
  get = EngineM $ \e c →
    readIORef (engineStateRef e) ⌦ \st → c (Right st)
  put newSt = EngineM $ \e c →
    writeIORef (engineStateRef e) newSt ⌦ \_ → c (Right ())

-- | Apply a pure update to the 'GraphicsState' nested inside 'EngineState'.
modifyGraphicsState ∷ (GraphicsState → GraphicsState) → EngineM σ ()
modifyGraphicsState f = modify $ \s → s { graphicsState = f (graphicsState s) }
