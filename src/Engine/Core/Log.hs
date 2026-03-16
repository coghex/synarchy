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
import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import Data.Maybe (mapMaybe)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))
import GHC.Stack (HasCallStack, CallStack, callStack, getCallStack, SrcLoc(..))
import System.IO (Handle, stdout, stderr, hPutStrLn, hFlush)
import System.Environment (lookupEnv)
import Engine.Core.Error.Exception (EngineException(..), ExceptionType, mkErrorContext)

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Show, Eq, Ord, Enum, Bounded)

data LogCategory
  = CatVulkan
  | CatGraphics
  | CatRender
  | CatShader
  | CatDescriptor
  | CatSwapchain
  | CatTexture
  | CatFont
  | CatAsset
  | CatResource
  | CatLua
  | CatScript
  | CatInput
  | CatScene
  | CatUI
  | CatWorld
  | CatUnit
  | CatThread
  | CatSystem
  | CatInit
  | CatState
  | CatGeneral
  | CatTest
  deriving (Show, Eq, Ord, Enum, Bounded)

parseCategory ∷ Text → Maybe LogCategory
parseCategory t = case T.toLower t of
  "vulkan"      → Just CatVulkan
  "graphics"    → Just CatGraphics
  "render"      → Just CatRender
  "shader"      → Just CatShader
  "descriptor"  → Just CatDescriptor
  "swapchain"   → Just CatSwapchain
  "texture"     → Just CatTexture
  "font"        → Just CatFont
  "asset"       → Just CatAsset
  "resource"    → Just CatResource
  "lua"         → Just CatLua
  "script"      → Just CatScript
  "input"       → Just CatInput
  "scene"       → Just CatScene
  "ui"          → Just CatUI
  "thread"      → Just CatThread
  "system"      → Just CatSystem
  "init"        → Just CatInit
  "state"       → Just CatState
  "general"     → Just CatGeneral
  "test"        → Just CatTest
  _             → Nothing

data LogBackend
  = LogToHandle Handle            -- ^ Write to file handle (stdout, stderr, file)
  | LogToFile FilePath            -- ^ Write to file (auto-opens)
  | LogToCallback (LogEntry → IO ())  -- ^ Custom callback
  | LogMulti [LogBackend]         -- ^ Multiple backends

instance Show LogBackend where
  show (LogToHandle _) = "LogToHandle"
  show (LogToFile path) = "LogToFile " ++ path
  show (LogToCallback _) = "LogToCallback"
  show (LogMulti backends) = "LogMulti " ++ show backends

data LogEntry = LogEntry
  { leLevel      ∷ LogLevel
  , leCategory   ∷ LogCategory
  , leMessage    ∷ Text
  , leFields     ∷ Map.Map Text Text  -- ^ Structured key-value fields
  , leTimestamp  ∷ Clock.UTCTime
  , leThreadId   ∷ ThreadId
  , leSrcLoc     ∷ Maybe SrcLoc  -- ^ Source location (single, not full stack)
  , leContext    ∷ [Text]  -- ^ Contextual breadcrumbs (e.g., ["Initialization", "Vulkan", "Swapchain"])
  } deriving (Show)

data LogContext = LogContext
  { lcFields  ∷ Map.Map Text Text  -- ^ Extra fields to add to all logs
  , lcBreadcrumbs ∷ [Text]         -- ^ Contextual path (e.g., function names)
  } deriving (Show)

instance Semigroup LogContext where
  (LogContext f1 b1) <> (LogContext f2 b2) = 
    LogContext (Map.union f2 f1) (b1 <> b2)

instance Monoid LogContext where
  mempty = LogContext Map.empty []

data LoggerState = LoggerState
  { lsBackend         ∷ LogBackend
  , lsMinLevel        ∷ IORef LogLevel           -- ^ Global minimum level
  , lsCategoryLevels  ∷ IORef (Map.Map LogCategory LogLevel)  -- ^ Per-category levels
  , lsDebugEnabled    ∷ IORef (Map.Map LogCategory Bool)      -- ^ Debug-only category flags
  , lsContext         ∷ IORef LogContext         -- ^ Thread-local would be better, but IORef works
  , lsEnabled         ∷ IORef Bool               -- ^ Master enable/disable
  , lsShowLocation    ∷ Bool                     -- ^ Show source location?
  }

data LogConfig = LogConfig
  { lcBackend          ∷ LogBackend
  , lcMinLevel         ∷ LogLevel
  , lcCategoryLevels   ∷ Map.Map LogCategory LogLevel
  , lcDebugCategories  ∷ [LogCategory]  -- ^ Categories enabled for debug by default
  , lcEnableByDefault  ∷ Bool
  , lcShowThreadId     ∷ Bool
  , lcShowTimestamp    ∷ Bool
  , lcShowLocation     ∷ Bool  -- ^ Show source location (file:line)?
  } deriving (Show)

defaultLogConfig ∷ LogConfig
defaultLogConfig = LogConfig
  { lcBackend = LogToHandle stdout
  , lcMinLevel = LevelInfo
  , lcCategoryLevels = Map.empty  -- Use global minimum by default
  , lcDebugCategories = []  -- No debug categories enabled by default
  , lcEnableByDefault = True
  , lcShowThreadId = True
  , lcShowTimestamp = True
  , lcShowLocation = True
  }

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

parseLogLevel ∷ String → LogLevel
parseLogLevel s = case map toLower s of
  "debug" → LevelDebug
  "info"  → LevelInfo
  "warn"  → LevelWarn
  "error" → LevelError
  _       → LevelInfo

-- | Check @ENGINE_LOG_\<CATEGORY\>=\<level\>@ env vars
loadCategoryLevelsFromEnv ∷ Map.Map LogCategory LogLevel → IO (Map.Map LogCategory LogLevel)
loadCategoryLevelsFromEnv initial = do
  let categories = [minBound .. maxBound] ∷ [LogCategory]
  foldM loadOne initial categories
  where
    loadOne acc cat = do
      let envVar = "ENGINE_LOG_" <> map toUpper (show cat)
      mLevel ← lookupEnv envVar
      case mLevel of
        Just lvl → return $ Map.insert cat (parseLogLevel lvl) acc
        Nothing  → return acc

-- | Parse @ENGINE_DEBUG=Vulkan,Lua,Graphics@ (or @all@)
loadDebugCategoriesFromEnv ∷ [LogCategory] → IO (Map.Map LogCategory Bool)
loadDebugCategoriesFromEnv defaults = do
  mDebugStr ← lookupEnv "ENGINE_DEBUG"
  let defaultMap = Map.fromList [(cat, True) | cat ← defaults]
  case mDebugStr of
    Nothing → return defaultMap
    Just str → case str of
                    "all" → return $ Map.fromList $ [(CatVulkan, True), (CatGraphics, True), (CatShader, True),
                                      (CatDescriptor, True), (CatSwapchain, True), (CatTexture, True),
                                      (CatFont, True), (CatAsset, True), (CatResource, True),
                                      (CatLua, True), (CatScript, True), (CatInput, True),
                                      (CatScene, True), (CatUI, True), (CatThread, True),
                                      (CatSystem, True), (CatInit, True), (CatState, True),
                                      (CatGeneral, True), (CatTest, True)]
                    _      → do
                                let catNames = map T.strip $ T.splitOn "," (T.pack str)
                                    cats = mapMaybe parseCategory catNames
                                return $ Map.fromList [(cat, True) | cat ← cats]

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


writeLogEntry ∷ LogBackend → LogEntry → IO ()
writeLogEntry backend entry = case backend of
  LogToHandle h → TIO.hPutStrLn h (formatLogEntry entry) >> hFlush h
  LogToFile path → appendFile path (T.unpack $ formatLogEntry entry <> "\n")
  LogToCallback cb → cb entry
  LogMulti backends → mapM_ (`writeLogEntry` entry) backends

writeThreadLogEntry ∷ LogBackend → LogEntry → IO ()
writeThreadLogEntry backend entry = case backend of
  LogToHandle h → TIO.hPutStrLn h (formatThreadLogEntry entry) >> hFlush h
  LogToFile path → appendFile path (T.unpack $ formatThreadLogEntry entry <> "\n")
  LogToCallback cb → cb entry
  LogMulti backends → mapM_ (`writeLogEntry` entry) backends

formatLogEntry ∷ LogEntry → Text
formatLogEntry LogEntry{..} = 
  T.intercalate " " $ filter (not . T.null)
    [ formatTimestamp leTimestamp
    , formatLevel leLevel
    , formatCategory leCategory
    , formatThread leThreadId
    , formatContext leContext
    , formatLocation leSrcLoc
    , leMessage
    , formatFields leFields
    ]

formatThreadLogEntry ∷ LogEntry → Text
formatThreadLogEntry LogEntry{..} = 
  T.intercalate " " $ filter (not . T.null)
    [ formatTimestamp leTimestamp
    , formatLevel leLevel
    , formatCategory leCategory
    , ""
    , ""
    , ""
    , leMessage
    , formatFields leFields
    ]

formatTimestamp ∷ Clock.UTCTime → Text
formatTimestamp t = T.pack $ TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

formatLevel ∷ LogLevel → Text
formatLevel LevelDebug = "[DEBUG]"
formatLevel LevelInfo  = "[INFO]"
formatLevel LevelWarn  = "[WARN]"
formatLevel LevelError = "[ERROR]"

formatCategory ∷ LogCategory → Text
formatCategory cat = "[" <> T.pack (drop 3 (show cat)) <> "]"

formatThread ∷ ThreadId → Text
formatThread tid = "[Θ:" <> T.pack (drop 9 (show tid)) <> "]"

formatContext ∷ [Text] → Text
formatContext [] = ""
formatContext ctx = "[" <> T.intercalate " > " ctx <> "]"

formatLocation ∷ Maybe SrcLoc → Text
formatLocation Nothing = ""
formatLocation (Just loc) = 
  let modName = T.pack $ srcLocModule loc
      fileName = T.takeWhileEnd (/= '.') modName  -- Get last component
  in "[" <> fileName <> ":" <> T.pack (show (srcLocStartLine loc)) <> "]"

formatFields ∷ Map.Map Text Text → Text
formatFields fields 
  | Map.null fields = ""
  | otherwise = "{" <> T.intercalate ", " (map formatField $ Map.toList fields) <> "}"
  where
    formatField (k, v) = k <> "=" <> v

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
