-- | Boot-time native-exception guard shared by every 'Engine.Core.Monad.runEngineM'
--   call site (graphical, headless, dump).
module App.Exception
  ( guardNativeExceptions
  ) where

import UPrelude
import Control.Exception (displayException, fromException, throwIO, catch
                         , SomeException)
import System.Exit (ExitCode)
import qualified Data.Text as T
import Engine.Core.Error.Exception (EngineException(..), ExceptionType(..)
                                   , SystemError(..), mkErrorContext)

-- | Native (IO) exceptions — e.g. a VulkanException thrown straight
--   from the bindings — bypass the CPS error channel ('throwError')
--   and would escape 'runEngineM' uncaught, skipping the Left branch
--   that shuts worker threads down and flushes buffered logs. Route
--   them into that branch; explicit ExitCode throws still propagate.
guardNativeExceptions ∷ IO (Either EngineException ())
                      → IO (Either EngineException ())
guardNativeExceptions act = act `catch` \(e ∷ SomeException) →
    case fromException e of
        Just (ec ∷ ExitCode) → throwIO ec
        Nothing → pure $ Left $ EngineException
            (ExSystem (IOError (T.pack (displayException e))))
            "uncaught native exception"
            mkErrorContext
