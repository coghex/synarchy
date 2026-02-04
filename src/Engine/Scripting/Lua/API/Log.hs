module Engine.Scripting.Lua.API.Log
  ( registerLogAPI
  , setDebugCategoryFn
  , getDebugCategoriesFn
  , logInfoFn
  , logWarnFn
  , logDebugFn
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Engine.Core.State
import Engine.Core.Log
import Data.IORef (IORef, readIORef, atomicModifyIORef')

import Engine.Scripting.Lua.Debug (getSourceInfo, SourceInfo(..))

logInfoFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
logInfoFn env = do
    msg <- Lua.tostring 1
    -- Level 2: 0=C function, 1=logInfoFn wrapper, 2=Lua caller
    mInfo <- getSourceInfo 2
    let (srcFile, srcLine) = case mInfo of
            Just info -> (siSource info, siCurrentLine info)
            Nothing   -> ("<unknown>", 0)
        srcFileStripped = dropDir srcFile
        dropDir ('.':'/':ss) = dropDir ss
        dropDir ('/':ss)     = ss
        dropDir (_:ss)       = dropDir ss
        dropDir _            = ""
    case msg of
        Just msgBS -> Lua.liftIO $ do
            logger <- readIORef (loggerRef env)
            let msgText = TE.decodeUtf8 msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":"
                              <> T.pack (show srcLine) <> "] " <> msgText
            logThreadInfo logger CatLua fullMsg
        Nothing -> pure ()
    return 0

logWarnFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
logWarnFn env = do
    msg <- Lua.tostring 1
    -- Level 2: 0=C function, 1=logWarnFn wrapper, 2=Lua caller
    mInfo <- getSourceInfo 2
    let (srcFile, srcLine) = case mInfo of
            Just info -> (siSource info, siCurrentLine info)
            Nothing   -> ("<unknown>", 0)
        srcFileStripped = dropDir srcFile
        dropDir ('.':'/':ss) = dropDir ss
        dropDir ('/':ss)     = ss
        dropDir (_:ss)       = dropDir ss
        dropDir _            = ""
    case msg of
        Just msgBS -> Lua.liftIO $ do
            logger <- readIORef (loggerRef env)
            let msgText = TE.decodeUtf8 msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":"
                              <> T.pack (show srcLine) <> "] " <> msgText
            logThreadWarn logger CatLua fullMsg
        Nothing -> pure ()
    return 0

logDebugFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
logDebugFn env = do
    msg <- Lua.tostring 1
    
    -- Level 2: 0=C function, 1=logInfoFn wrapper, 2=Lua caller
    mInfo <- getSourceInfo 2
    
    let (srcFile, srcLine) = case mInfo of
            Just info -> (siSource info, siCurrentLine info)
            Nothing   -> ("<unknown>", 0)
    
    case msg of
        Just msgBS -> Lua.liftIO $ do
            logger <- readIORef (loggerRef env)
            let msgText = TE.decodeUtf8 msgBS
                fullMsg = "[" <> T.pack srcFile <> ":" <> T.pack (show srcLine) <> "] " <> msgText
            logThreadDebug logger CatLua fullMsg
        Nothing -> pure ()
    
    return 0

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
    Lua.pushstring (BS.pack (show cat))
    Lua.rawseti (-2) (fromIntegral i)
  
  return 1

-- | Lua function: enableLog(enabled: boolean)
enableLogFn :: IORef LoggerState -> Lua.LuaE Lua.Exception Lua.NumResults
enableLogFn loggerRef = do
  enabled <- Lua.toboolean 1
  logger <- Lua.liftIO $ readIORef loggerRef
  Lua.liftIO $ atomicModifyIORef' (lsEnabled logger) $ \_ -> (enabled, ())
  return 0
