{-# LANGUAGE FlexibleContexts, UnicodeSyntax #-}
module Engine.Core.Log.Monad
  ( -- * Monad integration
    getLogger
  , logDebugM
  , logInfoM
  , logWarnM
  , logErrorM
  , logDebugSM
  , logInfoSM
  , logWarnSM
  , logErrorSM
  , withLogContextM
  , logAndThrowM
  , withTiming
  ) where

import UPrelude
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Data.IORef (readIORef)
import Engine.Core.Log
import Engine.Core.Error.Exception (EngineException, ExceptionType)
import Engine.Core.State (EngineEnv(..))

-----------------------------------------------------------
-- Logger Access
-----------------------------------------------------------

getLogger ∷ (MonadIO m, MonadReader EngineEnv m) ⇒ m LoggerState
getLogger = do
  ref ← asks loggerRef
  liftIO $ readIORef ref

-----------------------------------------------------------
-- Basic Logging Functions
-----------------------------------------------------------

logDebugM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
          ⇒ LogCategory → Text → m ()
logDebugM cat msg = do
  logger ← getLogger
  logDebug logger cat msg

logInfoM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
         ⇒ LogCategory → Text → m ()
logInfoM cat msg = do
  logger ← getLogger
  logInfo logger cat msg

logWarnM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
         ⇒ LogCategory → Text → m ()
logWarnM cat msg = do
  logger ← getLogger
  logWarn logger cat msg

logErrorM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
          ⇒ LogCategory → Text → m ()
logErrorM cat msg = do
  logger ← getLogger
  logError logger cat msg

-----------------------------------------------------------
-- Structured Logging Functions
-----------------------------------------------------------

logDebugSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
           ⇒ LogCategory → Text → [(Text, Text)] → m ()
logDebugSM cat msg fields = do
  logger ← getLogger
  logDebugS logger cat msg fields

logInfoSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
          ⇒ LogCategory → Text → [(Text, Text)] → m ()
logInfoSM cat msg fields = do
  logger ← getLogger
  logInfoS logger cat msg fields

logWarnSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
          ⇒ LogCategory → Text → [(Text, Text)] → m ()
logWarnSM cat msg fields = do
  logger ← getLogger
  logWarnS logger cat msg fields

logErrorSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m) 
           ⇒ LogCategory → Text → [(Text, Text)] → m ()
logErrorSM cat msg fields = do
  logger ← getLogger
  logErrorS logger cat msg fields

-----------------------------------------------------------
-- Context and Utilities
-----------------------------------------------------------

withLogContextM ∷ (MonadIO m, MonadReader EngineEnv m) 
                ⇒ Text → m a → m a
withLogContextM breadcrumb action = do
  logger ← getLogger
  withLogContext logger breadcrumb action

-- Log and throw
logAndThrowM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m, MonadError EngineException m)
             ⇒ LogCategory
             → ExceptionType
             → Text
             → m a
logAndThrowM cat exType msg = do
  logger ← getLogger
  logAndThrow logger cat exType msg

{-# INLINE withTiming #-}
withTiming ∷ (MonadIO m, MonadReader EngineEnv m) 
           ⇒ LogCategory → Text → m a → m a
withTiming cat label action = do
  start ← liftIO getCurrentTime
  result ← action
  end ← liftIO getCurrentTime
  let durationMs = realToFrac (diffUTCTime end start * 1000) ∷ Double
  when (durationMs > 1.0) $
    logDebugSM cat label [("duration_ms", T.pack $ show durationMs)]
  return result
