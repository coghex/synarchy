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
  ( LogLevel(..), LogCategory, allLogCategories, categoryEnvName
  , parseCategory )

parseLogLevel ∷ String → LogLevel
parseLogLevel s = case map toLower s of
  "debug" → LevelDebug
  "info"  → LevelInfo
  "warn"  → LevelWarn
  "error" → LevelError
  _       → LevelInfo

-- | The @ENGINE_LOG_\<CATEGORY\>@ variable that sets one category's
--   threshold. Derived from 'categoryEnvName' — the same @Cat@-stripped
--   spelling 'parseCategory' accepts and 'Engine.Core.Log.Format'
--   displays — so @CatVulkan@ is reached by @ENGINE_LOG_VULKAN@ and a
--   category can never be displayable but unreachable.
--
--   It used to uppercase @show cat@ directly, which leaked the internal
--   constructor prefix into the only advertised way to reach this
--   surface: the documented @ENGINE_LOG_VULKAN@ did nothing and only the
--   undocumented @ENGINE_LOG_CATVULKAN@ had any effect (#1918). No
--   derived name is @ENGINE_LOG_LEVEL@, which
--   'Engine.Core.Log.initLogger' reads separately as the global minimum.
categoryLevelEnvVar ∷ LogCategory → String
categoryLevelEnvVar =
  ("ENGINE_LOG_" <>) ∘ map toUpper ∘ T.unpack ∘ categoryEnvName

-- | Check @ENGINE_LOG_\<CATEGORY\>=\<level\>@ env vars, one per
--   'LogCategory', spelled as 'categoryLevelEnvVar' derives them.
--   A category with no variable set keeps whatever the supplied map
--   already had for it, and so falls through to the global minimum.
loadCategoryLevelsFromEnv ∷ Map.Map LogCategory LogLevel → IO (Map.Map LogCategory LogLevel)
loadCategoryLevelsFromEnv initial = do
  foldM loadOne initial allLogCategories
  where
    loadOne acc cat = do
      mLevel ← lookupEnv (categoryLevelEnvVar cat)
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
