module Engine.Scripting.Lua.API.Log
  ( registerLogAPI
  , setDebugCategoryFn
  , getDebugCategoriesFn
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Engine.Core.Log
import Data.IORef (IORef, readIORef)

-- | Register Lua logging API
registerLogAPI :: Lua.State -> IORef LoggerState -> IO ()
registerLogAPI lst loggerRef = Lua.runWith lst $ do
  -- synarchy.setDebug("Vulkan", true)
  Lua.pushHaskellFunction (setDebugCategoryFn loggerRef)
  Lua.setfield (-2) (Lua.Name "setDebug")
  
  -- synarchy.getDebugCategories() -> {"Vulkan", "Lua", ...}
  Lua.pushHaskellFunction (getDebugCategoriesFn loggerRef)
  Lua.setfield (-2) (Lua.Name "getDebugCategories")
  
  -- synarchy.enableLog(true/false)
  Lua.pushHaskellFunction (enableLogFn loggerRef)
  Lua.setfield (-2) (Lua.Name "enableLog")

-- | Lua function: setDebug(category: string, enabled: boolean)
setDebugCategoryFn :: IORef LoggerState -> Lua.LuaE Lua.Exception Lua.NumResults
setDebugCategoryFn loggerRef = do
  categoryStr <- Lua.tostring 1 >>= \case
    Nothing -> Lua.failLua "Expected category string"
    Just s -> return s
  
  enabled <- Lua.toboolean 2
  
  logger <- Lua.liftIO $ readIORef loggerRef
  
  case parseCategory (T.pack $ show categoryStr) of
    Nothing -> do
      Lua.pushstring $ "Unknown category: " <> categoryStr
      Lua.pushboolean False
      return 2
    Just cat -> do
      Lua.liftIO $ if enabled
        then enableCategory logger cat
        else disableCategory logger cat
      Lua.pushboolean True
      return 1

-- | Lua function: getDebugCategories() -> table of enabled categories
getDebugCategoriesFn :: IORef LoggerState -> Lua.LuaE Lua.Exception Lua.NumResults
getDebugCategoriesFn loggerRef = do
  logger <- Lua.liftIO $ readIORef loggerRef
  debugFlags <- Lua.liftIO $ readIORef (lsDebugEnabled logger)
  
  Lua.newtable
  let enabledCats = [cat | (cat, True) <- Map.toList debugFlags]
  forM_ (zip [1..] enabledCats) $ \(i :: Int, cat) -> do
    Lua.pushstring (show cat)
    Lua.rawseti (-2) (fromIntegral i)
  
  return 1

-- | Lua function: enableLog(enabled: boolean)
enableLogFn :: IORef LoggerState -> Lua.LuaE Lua.Exception Lua.NumResults
enableLogFn loggerRef = do
  enabled <- Lua.toboolean 1
  logger <- Lua.liftIO $ readIORef loggerRef
  Lua.liftIO $ atomicModifyIORef' (lsEnabled logger) $ \_ -> (enabled, ())
  return 0
