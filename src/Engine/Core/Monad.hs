module Engine.Core.Monad 
  ( EngineM(..)
  , EngineM'
  , runEngineM
  , MonadIO(..)
  , MonadError(..)
  , getEngineVars
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..), gets)
import qualified Control.Monad.Logger.CallStack as Logger
import Engine.Core.Error.Exception (EngineException(..))
import Engine.Core.Base
import Engine.Core.State
import Engine.Core.Var 

-- | Main engine monad transformer stack
-- ε = environment
-- σ = state
-- α = result
newtype EngineM ε σ α = EngineM 
  { unEngineM ∷ Var EngineEnv 
              → Var EngineState 
              → (Either EngineException α → IO σ) 
              → IO σ 
  }

-- | Common case where σ is either an action or an error
type EngineM' ε α = EngineM ε (Either EngineException α) α

-- | Run the engine monad
runEngineM ∷ EngineM ε σ α 
         → Var EngineEnv 
         → Var EngineState 
         → (Either EngineException α → IO σ) 
         → IO σ
runEngineM action env st cont = unEngineM action env st cont

instance Functor (EngineM ε σ) where
  fmap f m = EngineM $ \e s c → unEngineM m e s (c ∘ fmap f)

instance Applicative (EngineM ε σ) where
  pure x = EngineM $ \_ _ → ($ Right x)
  mf <*> mx = EngineM $ \e s c → unEngineM mf e s 
    $ \g → unEngineM mx e s (c ∘ (g <*>))

instance Monad (EngineM ε σ) where
  return = pure
  mx >>= k = EngineM $ \e s c → unEngineM mx e s $ \case
    Right x → unEngineM (k x) e s c
    Left ex → c (Left ex)

instance MonadIO (EngineM ε σ) where
  liftIO m = EngineM $ \_ _ c → m >>= (c ∘ Right)

instance MonadError EngineException (EngineM ε σ) where
  throwError e = EngineM $ \_ _ c → c (Left e)
  catchError action handler = EngineM $ \e s c → 
    unEngineM action e s $ \case
      Left err → unEngineM (handler err) e s c
      Right r  → c (Right r)

instance MonadReader EngineEnv (EngineM ε σ) where
  ask = EngineM $ \e _ c →
    atomically (readVar e) >>= \env → c (Right env)

instance MonadState EngineState (EngineM ε σ) where
  get = EngineM $ \_ s c →
    atomically (readVar s) >>= \st → c (Right st)
  put newSt = EngineM $ \_ s c →
    atomically (writeVar s newSt) >>= \_ → c (Right ())

getEngineVars ∷ EngineM ε σ (Var EngineEnv, Var EngineState)
getEngineVars = EngineM $ \e s c → c (Right (e, s))
