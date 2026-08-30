module Engine.Core.Log.Env
  ( parseLogLevel
  , loadCategoryLevelsFromEnv
  , loadDebugCategoriesFromEnv
  ) where

import UPrelude
import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)
import System.Environment (lookupEnv)
import Engine.Core.Log.Types
  (LogLevel(..), LogCategory, allLogCategories, parseCategory)

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
  foldM loadOne initial allLogCategories
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
                    -- Derived from the category type, not a hand-written
                    -- list: the former 21-entry literal had drifted past
                    -- CatRender, CatWorld and CatUnit (#1915).
                    "all" → return $ Map.fromList
                              [(cat, True) | cat ← allLogCategories]
                    _      → do
                                let catNames = map T.strip $ T.splitOn "," (T.pack str)
                                    cats = mapMaybe parseCategory catNames
                                return $ Map.fromList [(cat, True) | cat ← cats]
