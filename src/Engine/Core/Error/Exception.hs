{-# LANGUAGE DeriveDataTypeable #-}
module Engine.Core.Error.Exception
  ( EngineException(..)
  , ExceptionType(..)
  , Exceptable
  , throwEngineException
  ) where

import UPrelude
import Control.Exception (Exception, displayException)
import Control.Monad.Error.Class (MonadError, throwError)
import qualified Data.Text as T
import Type.Reflection

type Exceptable ς = (Typeable ς, Eq ς, Show ς)

data ExceptionType 
  = ExSystem    -- ^ System-level exceptions
  | ExGraphics  -- ^ Graphics/Vulkan exceptions
  | ExResource  -- ^ Resource management exceptions
  deriving (Show, Eq, Typeable)

data EngineException = EngineException
  { errorType ∷ ExceptionType
  , errorMsg  ∷ T.Text 
  } deriving (Eq, Typeable)

instance Show EngineException where
  show (EngineException etype msg) = T.unpack msg ⧺ show etype

instance Exception EngineException where
  displayException (EngineException etype msg) = unlines
    [ " exception:"
    , "*** " ⧺ T.unpack msg
    ]

-- | Throw an EngineException in any MonadError context
throwEngineException ∷ MonadError EngineException m ⇒ EngineException → m α
throwEngineException = throwError
