{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
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
import Engine.Core.Log.Types (LogLevel(..), LogCategory(..), parseCategory)

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
                                      (CatGeneral, True), (CatTest, True), (CatEvent, True)]
                    _      → do
                                let catNames = map T.strip $ T.splitOn "," (T.pack str)
                                    cats = mapMaybe parseCategory catNames
                                return $ Map.fromList [(cat, True) | cat ← cats]
