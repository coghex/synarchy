{-# LANGUAGE FlexibleContexts, UnicodeSyntax #-}
-- | Monadic logging, narrowed to the @core-init@ capability (issue
--   #889, epic #537): every function here reaches the logger only
--   through 'CoreCapability', never by dereferencing an 'EngineEnv'
--   field directly — this module imports the bare 'EngineEnv' type
--   (for the convenience wrappers' 'MonadReader' constraint) and
--   nothing else from "Engine.Core.State".
--
--   Two layers:
--
--   * The @*For@ primitives take a 'CoreCapability' explicitly — the
--     actual field access.
--   * The original @*M@\/@*SM@ names are thin 'MonadReader' @EngineEnv@
--     wrappers over them, so the ~440 existing production call sites
--     (39 modules) keep compiling unchanged. Per
--     'docs/engineenv_capability_inventory.md' SS7.1's own roadmap,
--     most call sites will keep reaching the logger through the
--     broader @EngineEnv@ carrier for a while yet — this migration is
--     about establishing the capability-record boundary, not about
--     rewriting every caller.
module Engine.Core.Log.Monad
  ( -- * Capability-scoped primitives
    getLoggerFor
  , logDebugFor
  , logInfoFor
  , logWarnFor
  , logErrorFor
  , logDebugSFor
  , logInfoSFor
  , logWarnSFor
  , logErrorSFor
  , logAndThrowFor
  , withTimingFor
    -- * Monad integration
  , getLogger
  , logDebugM
  , logInfoM
  , logWarnM
  , logErrorM
  , logDebugSM
  , logInfoSM
  , logWarnSM
  , logErrorSM
  , logAndThrowM
  , withTiming
  ) where

import UPrelude
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Data.IORef (readIORef)
import Engine.Core.Log
import Engine.Core.Error.Exception (EngineException, ExceptionType)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)

-- Capability-scoped primitives -------------------------------------------

getLoggerFor ∷ MonadIO m ⇒ CoreCapability → m LoggerState
getLoggerFor cap = liftIO $ readIORef (ccLoggerRef cap)

logDebugFor ∷ (HasCallStack, MonadIO m) ⇒ CoreCapability → LogCategory → Text → m ()
logDebugFor cap cat msg = do
  logger ← getLoggerFor cap
  logDebug logger cat msg

logInfoFor ∷ (HasCallStack, MonadIO m) ⇒ CoreCapability → LogCategory → Text → m ()
logInfoFor cap cat msg = do
  logger ← getLoggerFor cap
  logInfo logger cat msg

logWarnFor ∷ (HasCallStack, MonadIO m) ⇒ CoreCapability → LogCategory → Text → m ()
logWarnFor cap cat msg = do
  logger ← getLoggerFor cap
  logWarn logger cat msg

logErrorFor ∷ (HasCallStack, MonadIO m) ⇒ CoreCapability → LogCategory → Text → m ()
logErrorFor cap cat msg = do
  logger ← getLoggerFor cap
  logError logger cat msg

logDebugSFor ∷ (HasCallStack, MonadIO m)
             ⇒ CoreCapability → LogCategory → Text → [(Text, Text)] → m ()
logDebugSFor cap cat msg fields = do
  logger ← getLoggerFor cap
  logDebugS logger cat msg fields

logInfoSFor ∷ (HasCallStack, MonadIO m)
            ⇒ CoreCapability → LogCategory → Text → [(Text, Text)] → m ()
logInfoSFor cap cat msg fields = do
  logger ← getLoggerFor cap
  logInfoS logger cat msg fields

logWarnSFor ∷ (HasCallStack, MonadIO m)
            ⇒ CoreCapability → LogCategory → Text → [(Text, Text)] → m ()
logWarnSFor cap cat msg fields = do
  logger ← getLoggerFor cap
  logWarnS logger cat msg fields

logErrorSFor ∷ (HasCallStack, MonadIO m)
             ⇒ CoreCapability → LogCategory → Text → [(Text, Text)] → m ()
logErrorSFor cap cat msg fields = do
  logger ← getLoggerFor cap
  logErrorS logger cat msg fields

logAndThrowFor ∷ (HasCallStack, MonadIO m, MonadError EngineException m)
               ⇒ CoreCapability → LogCategory → ExceptionType → Text → m a
logAndThrowFor cap cat exType msg = do
  logger ← getLoggerFor cap
  logAndThrow logger cat exType msg

{-# INLINE withTimingFor #-}
withTimingFor ∷ MonadIO m ⇒ CoreCapability → LogCategory → Text → m a → m a
withTimingFor cap cat label action = do
  start ← liftIO getCurrentTime
  result ← action
  end ← liftIO getCurrentTime
  let durationMs = realToFrac (diffUTCTime end start * 1000) ∷ Double
  when (durationMs > 1.0) $  -- Only log if > 1ms
    logDebugSFor cap cat label [("duration_ms", T.pack $ show durationMs)]
  return result

-- Monad integration (EngineEnv-carried convenience wrappers) -------------

getLogger ∷ (MonadIO m, MonadReader EngineEnv m) ⇒ m LoggerState
getLogger = asks toCoreCapability ⌦ getLoggerFor

logDebugM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
          ⇒ LogCategory → Text → m ()
logDebugM cat msg = asks toCoreCapability ⌦ \cap → logDebugFor cap cat msg

logInfoM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
         ⇒ LogCategory → Text → m ()
logInfoM cat msg = asks toCoreCapability ⌦ \cap → logInfoFor cap cat msg

logWarnM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
         ⇒ LogCategory → Text → m ()
logWarnM cat msg = asks toCoreCapability ⌦ \cap → logWarnFor cap cat msg

logErrorM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
          ⇒ LogCategory → Text → m ()
logErrorM cat msg = asks toCoreCapability ⌦ \cap → logErrorFor cap cat msg

logDebugSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
           ⇒ LogCategory → Text → [(Text, Text)] → m ()
logDebugSM cat msg fields = asks toCoreCapability ⌦ \cap → logDebugSFor cap cat msg fields

logInfoSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
          ⇒ LogCategory → Text → [(Text, Text)] → m ()
logInfoSM cat msg fields = asks toCoreCapability ⌦ \cap → logInfoSFor cap cat msg fields

logWarnSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
          ⇒ LogCategory → Text → [(Text, Text)] → m ()
logWarnSM cat msg fields = asks toCoreCapability ⌦ \cap → logWarnSFor cap cat msg fields

logErrorSM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m)
           ⇒ LogCategory → Text → [(Text, Text)] → m ()
logErrorSM cat msg fields = asks toCoreCapability ⌦ \cap → logErrorSFor cap cat msg fields

logAndThrowM ∷ (HasCallStack, MonadIO m, MonadReader EngineEnv m, MonadError EngineException m)
             ⇒ LogCategory
             → ExceptionType
             → Text
             → m a
logAndThrowM cat exType msg = asks toCoreCapability ⌦ \cap → logAndThrowFor cap cat exType msg

{-# INLINE withTiming #-}
withTiming ∷ (MonadIO m, MonadReader EngineEnv m)
           ⇒ LogCategory → Text → m a → m a
withTiming cat label action = asks toCoreCapability ⌦ \cap → withTimingFor cap cat label action
