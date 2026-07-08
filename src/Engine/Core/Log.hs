{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, UnicodeSyntax #-}
module Engine.Core.Log
  ( -- * Logger initialization
    LogConfig(..)
  , LogBackend(..)
  , LogCategory(..)
  , defaultLogConfig
  , initLogger
  , shutdownLogger

  -- * Category management
  , enableCategory
  , disableCategory
  , setCategoryLevel
  , isEnabled
  , getEnabledCategories
  , parseCategory

  -- * Core logging functions
  , logDebug
  , logThreadDebug
  , logInfo
  , logThreadInfo
  , logWarn
  , logThreadWarn
  , logError
  , logThreadError

  -- * Structured logging
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS

  -- * Exception integration
  , logAndThrow
  , logException
  , traceLog

  -- * Types
  , LogLevel(..)
  , LogEntry(..)
  , LogContext(..)
  , LoggerState(..)
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Time.Clock as Clock
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Control.Concurrent (myThreadId)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import GHC.Stack (HasCallStack, CallStack, callStack, getCallStack, SrcLoc)
import System.IO (hFlush)
import System.Environment (lookupEnv)
import Engine.Core.Error.Exception (EngineException(..), ExceptionType, mkErrorContext)
import Engine.Core.Log.Types
import Engine.Core.Log.Env (parseLogLevel, loadCategoryLevelsFromEnv, loadDebugCategoriesFromEnv)
import Engine.Core.Log.Format (writeLogEntry, writeThreadLogEntry)

-- | Applies env-var overrides (@ENGINE_LOG_LEVEL@, @ENGINE_DEBUG@, etc.)
--   on top of the supplied 'LogConfig'
initLogger ∷ LogConfig → IO LoggerState
initLogger LogConfig{..} = do
  envLevel ← lookupEnv "ENGINE_LOG_LEVEL"
  let minLevel = maybe lcMinLevel parseLogLevel envLevel
  categoryLevels ← loadCategoryLevelsFromEnv lcCategoryLevels
  debugCategories ← loadDebugCategoriesFromEnv lcDebugCategories

  minLevelRef ← newIORef minLevel
  categoryLevelsRef ← newIORef categoryLevels
  debugEnabledRef ← newIORef debugCategories
  contextRef ← newIORef mempty
  enabledRef ← newIORef lcEnableByDefault

  return LoggerState
    { lsBackend = lcBackend
    , lsMinLevel = minLevelRef
    , lsCategoryLevels = categoryLevelsRef
    , lsDebugEnabled = debugEnabledRef
    , lsContext = contextRef
    , lsEnabled = enabledRef
    , lsShowLocation = lcShowLocation
    }

shutdownLogger ∷ LoggerState → IO ()
shutdownLogger LoggerState{..} =
  case lsBackend of
    LogToHandle h → hFlush h
    LogToFile _ → return ()  -- File handles managed separately
    LogToCallback _ → return ()
    LogMulti backends → mapM_ shutdownOne backends
  where
    shutdownOne (LogToHandle h) = hFlush h
    shutdownOne _ = return ()

setCategoryLevel ∷ LoggerState → LogCategory → LogLevel → IO ()
setCategoryLevel LoggerState{..} cat level =
  atomicModifyIORef' lsCategoryLevels $ \cats →
    (Map.insert cat level cats, ())

enableCategory ∷ LoggerState → LogCategory → IO ()
enableCategory LoggerState{..} cat =
  atomicModifyIORef' lsDebugEnabled $ \cats →
    (Map.insert cat True cats, ())

disableCategory ∷ LoggerState → LogCategory → IO ()
disableCategory LoggerState{..} cat =
  atomicModifyIORef' lsDebugEnabled $ \cats →
    (Map.delete cat cats, ())

isEnabled ∷ LoggerState → LogCategory → LogLevel → IO Bool
isEnabled LoggerState{..} cat level = do
  enabled ← readIORef lsEnabled
  if not enabled then return False else do
    -- Debug level checks per-category debug flags, not the global minimum
    if level ≡ LevelDebug then do
      debugFlags ← readIORef lsDebugEnabled
      return $ Map.findWithDefault False cat debugFlags
    else do
      globalMin ← readIORef lsMinLevel
      categoryLevels ← readIORef lsCategoryLevels
      let effectiveMin = Map.findWithDefault globalMin cat categoryLevels
      return $ level ≥ effectiveMin

getEnabledCategories ∷ LoggerState → IO [(LogCategory, LogLevel)]
getEnabledCategories LoggerState{..} = do
  cats ← readIORef lsCategoryLevels
  return $ Map.toList cats

-- | Walk the 'CallStack' bottom-up, skipping known logging frames
extractCallSite ∷ CallStack → Maybe SrcLoc
extractCallSite cs =
  let frames = getCallStack cs
      reversed = reverse frames
  in case reversed of
    [] → Nothing
    ((_, fallback):_) → case dropWhile isInternalCall reversed of
      ((_, loc):_) → Just loc
      _            → Just fallback  -- All internal: use most recent frame
  where
    isInternalCall (name, _) = name `elem`
      [ "logMessage"
      , "logDebug", "logDebugS"
      , "logInfo", "logInfoS"
      , "logWarn", "logWarnS"
      , "logError", "logErrorS"
      , "logDebugM", "logDebugSM"
      , "logInfoM", "logInfoSM"
      , "logWarnM", "logWarnSM"
      , "logErrorM", "logErrorSM"
      , "getLogger"  -- Also skip the logger fetcher
      ]

logMessage ∷ (HasCallStack, MonadIO m)
           ⇒ LoggerState
           → LogLevel
           → LogCategory
           → Text
           → Map.Map Text Text
           → m ()
logMessage ls@LoggerState{..} level cat msg fields = liftIO $ do
  shouldLog ← isEnabled ls cat level
  when shouldLog $ do
    now ← Clock.getCurrentTime
    tid ← myThreadId
    ctx ← readIORef lsContext

    let srcLoc = if lsShowLocation then extractCallSite callStack else Nothing
        entry = LogEntry
          { leLevel = level
          , leCategory = cat
          , leMessage = msg
          , leFields = Map.union fields (lcFields ctx)
          , leTimestamp = now
          , leThreadId = tid
          , leSrcLoc = srcLoc
          , leContext = lcBreadcrumbs ctx
          }

    writeLogEntry lsBackend entry

logThreadMessage ∷ (HasCallStack, MonadIO m)
                 ⇒ LoggerState
                 → LogLevel
                 → LogCategory
                 → Text
                 → Map.Map Text Text
                 → m ()
logThreadMessage ls@LoggerState{..} level cat msg fields = liftIO $ do
  shouldLog ← isEnabled ls cat level
  when shouldLog $ do
    now ← Clock.getCurrentTime
    tid ← myThreadId
    ctx ← readIORef lsContext

    let srcLoc = if lsShowLocation then extractCallSite callStack else Nothing
        entry = LogEntry
          { leLevel = level
          , leCategory = cat
          , leMessage = msg
          , leFields = Map.union fields (lcFields ctx)
          , leTimestamp = now
          , leThreadId = tid
          , leSrcLoc = srcLoc
          , leContext = lcBreadcrumbs ctx
          }

    writeThreadLogEntry lsBackend entry

logDebug ∷ (HasCallStack, MonadIO m)
         ⇒ LoggerState → LogCategory → Text → m ()
logDebug ls cat msg = logMessage ls LevelDebug cat msg Map.empty

logThreadDebug ∷ (HasCallStack, MonadIO m)
         ⇒ LoggerState → LogCategory → Text → m ()
logThreadDebug ls cat msg = logThreadMessage ls LevelDebug cat msg Map.empty

logInfo ∷ (HasCallStack, MonadIO m)
        ⇒ LoggerState → LogCategory → Text → m ()
logInfo ls cat msg = logMessage ls LevelInfo cat msg Map.empty

logThreadInfo ∷ (HasCallStack, MonadIO m)
        ⇒ LoggerState → LogCategory → Text → m ()
logThreadInfo ls cat msg = logThreadMessage ls LevelInfo cat msg Map.empty

logWarn ∷ (HasCallStack, MonadIO m)
        ⇒ LoggerState → LogCategory → Text → m ()
logWarn ls cat msg = logMessage ls LevelWarn cat msg Map.empty

logThreadWarn ∷ (HasCallStack, MonadIO m)
        ⇒ LoggerState → LogCategory → Text → m ()
logThreadWarn ls cat msg = logThreadMessage ls LevelWarn cat msg Map.empty

logError ∷ (HasCallStack, MonadIO m)
         ⇒ LoggerState → LogCategory → Text → m ()
logError ls cat msg = logMessage ls LevelError cat msg Map.empty

logThreadError ∷ (HasCallStack, MonadIO m)
         ⇒ LoggerState → LogCategory → Text → m ()
logThreadError ls cat msg = logThreadMessage ls LevelError cat msg Map.empty

logDebugS ∷ (HasCallStack, MonadIO m)
          ⇒ LoggerState → LogCategory → Text → [(Text, Text)] → m ()
logDebugS ls cat msg fields = logMessage ls LevelDebug cat msg (Map.fromList fields)

logInfoS ∷ (HasCallStack, MonadIO m)
         ⇒ LoggerState → LogCategory → Text → [(Text, Text)] → m ()
logInfoS ls cat msg fields = logMessage ls LevelInfo cat msg (Map.fromList fields)

logWarnS ∷ (HasCallStack, MonadIO m)
         ⇒ LoggerState → LogCategory → Text → [(Text, Text)] → m ()
logWarnS ls cat msg fields = logMessage ls LevelWarn cat msg (Map.fromList fields)

logErrorS ∷ (HasCallStack, MonadIO m)
          ⇒ LoggerState → LogCategory → Text → [(Text, Text)] → m ()
logErrorS ls cat msg fields = logMessage ls LevelError cat msg (Map.fromList fields)

logAndThrow ∷ (HasCallStack, MonadIO m, MonadError EngineException m)
            ⇒ LoggerState
            → LogCategory
            → ExceptionType
            → Text
            → m a
logAndThrow ls cat exType msg = do
  logError ls cat msg
  throwError $ EngineException exType msg mkErrorContext

logException ∷ (HasCallStack, MonadIO m)
             ⇒ LoggerState
             → LogCategory
             → EngineException
             → m ()
logException ls cat ex =
  logErrorS ls cat ("Exception caught: " <> T.pack (show ex)) []

-- | Log entry and exit of a function, including the return value
traceLog ∷ (HasCallStack, MonadIO m, Show a)
         ⇒ LoggerState
         → LogCategory
         → Text  -- ^ Function name
         → m a
         → m a
traceLog ls cat funcName action = do
  logDebug ls cat $ "→ " <> funcName
  result ← action
  logDebug ls cat $ "← " <> funcName <> " = " <> T.pack (show result)
  return result
