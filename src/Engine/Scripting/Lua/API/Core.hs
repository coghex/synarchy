module Engine.Scripting.Lua.API.Core
  ( logInfoFn
  , loadScriptFn
  , killScriptFn
  , setTickIntervalFn
  , pauseScriptFn
  , resumeScriptFn
  ) where

import UPrelude
import Engine.Scripting.Types (ScriptValue(..))
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.Script (callModuleFunction)
import Engine.Scripting.Lua.Util (isValidRef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, LogCategory(..))
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.IORef (atomicModifyIORef', readIORef)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel(..), toLogStr, defaultLoc)
import Data.Time.Clock (getCurrentTime, utctDayTime)

logInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
logInfoFn env = do
  msg ← Lua.tostring 1
  logger ← liftIO $ readIORef (loggerRef env)
  case msg of
    Just bs → logInfo logger CatLua (TE.decodeUtf8 bs)
    Nothing → return ()
  return 0

setTickIntervalFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setTickIntervalFn env backendState = do
   scriptIdNum ← Lua.tointeger 1
   interval ← Lua.tonumber 2
   case (scriptIdNum, interval) of
       (Just sid, Just (Lua.Number seconds)) → Lua.liftIO $ do
           currentTime ← getCurrentTime
           let currentSecs = realToFrac $ utctDayTime currentTime
           atomically $ modifyTVar (lbsScripts backendState) $
               Map.adjust (\s → s { scriptTickRate = seconds
                                  , scriptNextTick = currentSecs + seconds
                                  }) (fromIntegral sid)
           logger ← readIORef (loggerRef env)
           logInfo logger CatLua $ T.pack $
               "Tick interval for script " ⧺ show sid ⧺ " set to " ⧺ show seconds ⧺ " seconds."
       _ → Lua.liftIO $ do
           logger ← readIORef (loggerRef env)
           logInfo logger CatLua
               "setTickInterval requires 2 arguments: scriptId, seconds"
   return 0

loadScriptFn ∷ EngineEnv → LuaBackendState → Lua.State 
             → Lua.LuaE Lua.Exception Lua.NumResults
loadScriptFn _env backendState lst = do
    path ← Lua.tostring 1
    tickRate ← Lua.tonumber 2
    case (path, tickRate) of
        (Just pathBS, Just rate) → do
            scriptId ← Lua.liftIO $ do
                let pathStr = TE.decodeUtf8 pathBS
                
                sid ← atomicModifyIORef' (lbsNextScriptId backendState)
                    (\n → (n + 1, n))
                
                status ← Lua.runWith lst $ Lua.dofileTrace (Just $ T.unpack pathStr)
                case status of
                    Lua.OK → do
                        modRef ← Lua.runWith lst $ do
                            isTable ← Lua.istable (-1)
                            if isTable
                                then Lua.ref Lua.registryindex
                                else do
                                    Lua.pop 1
                                    return (Lua.Reference (fromIntegral Lua.refnil))
                        
                        currentTime ← getCurrentTime
                        let currentSecs = realToFrac $ utctDayTime currentTime
                            script = LuaScript
                                { scriptId        = sid
                                , scriptPath      = T.unpack pathStr
                                , scriptTickRate  = realToFrac rate
                                , scriptNextTick  = currentSecs + realToFrac rate
                                , scriptModuleRef = modRef
                                , scriptPaused    = False
                                }
                        
                        atomically $ modifyTVar (lbsScripts backendState) $
                            Map.insert sid script
                        
                        when (isValidRef modRef) $
                            void $ callModuleFunction lst modRef "init" []
                        
                        return (Just sid)
                    _ → return Nothing
            
            case scriptId of
                Just sid → Lua.pushinteger (Lua.Integer $ fromIntegral sid)
                Nothing  → Lua.pushnil
        _ → Lua.pushnil
    return 1

killScriptFn ∷ LuaBackendState → Lua.State → Lua.LuaE Lua.Exception Lua.NumResults
killScriptFn backendState lst = do
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ do
            scriptsMap ← readTVarIO (lbsScripts backendState)
            case Map.lookup (fromIntegral sid) scriptsMap of
                Just script → do
                    when (isValidRef (scriptModuleRef script)) $ do
                        _ ← callModuleFunction lst (scriptModuleRef script) "shutdown" []
                        Lua.runWith lst $ Lua.unref Lua.registryindex (scriptModuleRef script)
                    atomically $ modifyTVar (lbsScripts backendState) $
                        Map.delete (fromIntegral sid)
                Nothing → return ()
        _ → return ()
    return 0

pauseScriptFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
pauseScriptFn backendState = do
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ atomically $ modifyTVar (lbsScripts backendState) $
            Map.adjust (\s → s { scriptPaused = True }) (fromIntegral sid)
        _ → return ()
    return 0

resumeScriptFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
resumeScriptFn backendState = do
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ do
            currentTime ← getCurrentTime
            let currentSecs = realToFrac $ utctDayTime currentTime
            atomically $ modifyTVar (lbsScripts backendState) $
                Map.adjust (\s → s { scriptPaused = False, scriptNextTick = currentSecs }) 
                           (fromIntegral sid)
        _ → return ()
    return 0
