module Engine.Core.Log
  ( -- * Logger initialization
    LogConfig(..)
  , LogBackend(..)
  , LogCategory(..)
  , defaultLogConfig
  , initLogger
  , shutdownLogger

  -- * Category management
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
  , logErrorS

  -- * Exception integration
  , logAndThrow

  -- * Types
  , LogLevel(..)
  , LogEntry(..)
  , LogContext(..)
  , LoggerState(..)
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Time.Clock as Clock
import Data.IORef (newIORef, readIORef)
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
    LogToCallback _ → return ()

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

-- | The external source location a log entry should be attributed to.
--
--   'getCallStack' returns frames most-recent-first, each pairing a
--   function name with the location that function was CALLED from. The
--   chain runs unbroken from here out through the logging wrappers to
--   the first caller without a 'HasCallStack' constraint, so the LAST
--   frame is the public logging entry point paired with its external
--   call site — exactly what we want to report.
--
--   Internal helpers below that entry point can only ever push newer
--   (inner) frames, so adding, renaming, removing, or sharing one
--   cannot change the answer. This deliberately replaces the old list
--   of internal function-name strings (#945), which had to be kept in
--   sync by hand and silently misattributed entries whenever it drifted
--   -- as it did when #889 introduced the @*For@ layer.
--
--   The one standing requirement is that every PUBLIC logging entry
--   point carries 'HasCallStack'; without it the chain starts inside
--   this module and the outermost frame is an internal one. Nothing
--   here can check that, so a NEW public logging family must add a case
--   to the @logging source-location attribution@ describe block in
--   @test-headless\/Test\/Headless\/Core\/LogMonad.hs@.
extractCallSite ∷ CallStack → Maybe SrcLoc
extractCallSite cs = case reverse (getCallStack cs) of
  ((_, outermost):_) → Just outermost
  []                 → Nothing

-- | Shared entry-construction assembly for the normal and thread logging
--   paths (CH-8, #944): @write@ selects the backend dispatch
--   ('writeLogEntry' vs 'writeThreadLogEntry').
--
--   @srcLoc@ is computed by the caller, while it is still inside the
--   'HasCallStack' chain, rather than here — so this helper needs no such
--   constraint of its own, and the standing requirement stays where it
--   belongs, on the public logging entry points that do carry one. How a
--   location is chosen from that chain is 'extractCallSite''s contract
--   above; this helper only carries the result through.
logEntryWith ∷ MonadIO m
             ⇒ (LogBackend → LogEntry → IO ())
             → Maybe SrcLoc
             → LoggerState
             → LogLevel
             → LogCategory
             → Text
             → Map.Map Text Text
             → m ()
logEntryWith write srcLoc ls@LoggerState{..} level cat msg fields = liftIO $ do
  shouldLog ← isEnabled ls cat level
  when shouldLog $ do
    now ← Clock.getCurrentTime
    tid ← myThreadId
    ctx ← readIORef lsContext

    write lsBackend LogEntry
      { leLevel = level
      , leCategory = cat
      , leMessage = msg
      , leFields = Map.union fields (lcFields ctx)
      , leTimestamp = now
      , leThreadId = tid
      , leSrcLoc = srcLoc
      , leContext = lcBreadcrumbs ctx
      }

logMessage ∷ (HasCallStack, MonadIO m)
           ⇒ LoggerState
           → LogLevel
           → LogCategory
           → Text
           → Map.Map Text Text
           → m ()
logMessage ls@LoggerState{..} level cat msg fields =
  logEntryWith writeLogEntry
    (if lsShowLocation then extractCallSite callStack else Nothing)
    ls level cat msg fields

logThreadMessage ∷ (HasCallStack, MonadIO m)
                 ⇒ LoggerState
                 → LogLevel
                 → LogCategory
                 → Text
                 → Map.Map Text Text
                 → m ()
logThreadMessage ls@LoggerState{..} level cat msg fields =
  logEntryWith writeThreadLogEntry
    (if lsShowLocation then extractCallSite callStack else Nothing)
    ls level cat msg fields

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
