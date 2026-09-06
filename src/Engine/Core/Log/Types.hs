module Engine.Core.Log.Types
  ( -- * Levels & categories
    LogLevel(..)
  , LogCategory(..)
  , allLogCategories
  , categoryEnvName
  , parseCategory

  -- * Backend & entries
  , LogBackend(..)
  , LogEntry(..)
  , LogContext(..)

  -- * Logger state & config
  , LoggerState(..)
  , LogConfig(..)
  , defaultLogConfig
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Time.Clock as Clock
import Data.IORef (IORef)
import Control.Concurrent (ThreadId)
import GHC.Stack (SrcLoc)
import System.IO (Handle, stdout)

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
  | CatEvent  -- ^ Player Events subsystem (registry load, unknown
              --   category warnings from 'Engine.PlayerEvent.emitEvent').
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every 'LogCategory', in constructor order. The single enumeration
--   both @ENGINE_DEBUG@ surfaces are derived from, so adding a
--   constructor extends them without editing a table (#1915).
allLogCategories ∷ [LogCategory]
allLogCategories = [minBound .. maxBound]

-- | A category's @ENGINE_DEBUG@ spelling, derived from the constructor
--   name rather than a hand-maintained list: drop the @Cat@ prefix and
--   lowercase the rest. This reproduces byte-for-byte all 22 spellings
--   the former hand-written table accepted — @CatUI@'s @ui@ included —
--   and is the lowercased form of what 'Engine.Core.Log.Format' already
--   displays, so a category can never be displayable but unnameable.
categoryEnvName ∷ LogCategory → Text
categoryEnvName = T.toLower ∘ T.pack ∘ drop 3 ∘ show

-- | Case-insensitive lookup over every derived name. Unrecognized names
--   stay 'Nothing' (dropped silently by the caller), which is the
--   pre-existing contract.
categoryEnvNames ∷ Map.Map Text LogCategory
categoryEnvNames = Map.fromList [(categoryEnvName c, c) | c ← allLogCategories]

parseCategory ∷ Text → Maybe LogCategory
parseCategory t = Map.lookup (T.toLower t) categoryEnvNames

data LogBackend
  = LogToHandle Handle            -- ^ Write to file handle (stdout, stderr, file)
  | LogToCallback (LogEntry → IO ())  -- ^ Custom callback

instance Show LogBackend where
  show (LogToHandle _) = "LogToHandle"
  show (LogToCallback _) = "LogToCallback"

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
  , lcShowLocation     ∷ Bool  -- ^ Show source location (file:line)?
  } deriving (Show)

defaultLogConfig ∷ LogConfig
defaultLogConfig = LogConfig
  { lcBackend = LogToHandle stdout
  , lcMinLevel = LevelInfo
  , lcCategoryLevels = Map.empty  -- Use global minimum by default
  , lcDebugCategories = []  -- No debug categories enabled by default
  , lcEnableByDefault = True
  , lcShowLocation = True
  }
