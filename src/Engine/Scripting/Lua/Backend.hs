module Engine.Scripting.Lua.Backend
  ( LuaBackend(..)
  , createLuaBackend
  , startLuaThread
  ) where

import UPrelude
import Engine.Scripting.Backend
import Engine.Scripting.Types
import Engine.Scripting.Lua.Types
import Engine.Core.Thread
import Engine.Core.State
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Dynamic (toDyn, fromDynamic)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text.Encoding as TE
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad.Logger (Loc(..), LogLevel(..), toLogStr, defaultLoc)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Lua scripting backend
data LuaBackend = LuaBackend

instance ScriptBackend LuaBackend where
  initBackend _ = do
    lst <- Lua.newstate
    _ <- Lua.runWith lst Lua.openlibs
    return $ ScriptContext (toDyn lst)
  
  closeBackend _ (ScriptContext dyn) = 
    case fromDynamic dyn of
      Just lst -> Lua.close lst
      Nothing  -> error "Invalid Lua context"
  
  loadScript _ (ScriptContext dyn) path =
    case fromDynamic dyn of
      Just lst -> do
        result <- Lua.runWith lst $ Lua.dofileTrace $ Just path
        case result of
          Lua.OK -> return $ ScriptSuccess []
          _      -> do
            err <- Lua.runWith lst $ Lua.tostring (-1)
            return $ ScriptError (T.pack $ show err)
      Nothing -> return $ ScriptError "Invalid Lua context"
  
  reloadScript backend ctx path = loadScript backend ctx path
  
  callFunction _ (ScriptContext dyn) funcName args =
    case fromDynamic dyn of
      Just lst -> do
        result <- Lua.runWith lst $ callLuaFunction funcName args
        case result of
          Lua.OK -> return $ ScriptSuccess []
          _      -> return $ ScriptError "Function call failed"
      Nothing -> return $ ScriptError "Invalid Lua context"
  
  registerFunction _ _ _ _ = 
    -- TODO: Implement function registration
    return ()
  
  backendName _    = "Lua"
  backendVersion _ = "5.5"

registerLuaAPI :: Lua.State -> EngineEnv -> IO ()
registerLuaAPI lst env = Lua.runWith lst $ do
  Lua.newtable 
  -- engine.logInfo
  Lua.pushHaskellFunction (logInfoFn :: Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "logInfo")
  Lua.setglobal (Lua.Name "engine")
  where logInfoFn = do
          msg <- Lua.tostring 1
          let lf = logFunc env
          case msg of
            Just bs → Lua.liftIO $ lf defaultLoc "lua" LevelInfo (toLogStr $ TE.decodeUtf8 bs)
            Nothing → return ()
          return 0


-- | Helper to call Lua function with explicit type
callLuaFunction :: T.Text -> [ScriptValue] -> Lua.LuaE Lua.Exception Lua.Status
callLuaFunction funcName _args = do
  let name = Lua.Name (TE.encodeUtf8 funcName)
  _ <- Lua.getglobal name
  -- TODO: Push arguments
  Lua.call 0 Lua.multret
  return Lua.OK

-- | Create Lua backend
createLuaBackend :: IO LuaBackend
createLuaBackend = return LuaBackend

-- | Start Lua thread (existing thread management)
startLuaThread ∷ EngineEnv → IO ThreadState
startLuaThread env = do
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            let lf = logFunc env
            lf defaultLoc "lua" LevelInfo "Starting lua thread..."
            let lteq = luaToEngineQueue env
                etlq = engineToLuaQueue env
            backendState ← createLuaBackendState lteq etlq
            tid ← forkIO $ runLuaLoop env backendState stateRef
            return tid
        ) 
        (\(e :: SomeException) → do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError $
                "Failed to start lua thread: " <> (toLogStr (show e))
            error "Lua thread failed to start"
        )
    return $ ThreadState stateRef threadId

-- | Create Lua backend state
createLuaBackendState ::  Q.Queue LuaToEngineMsg -> Q.Queue EngineToLuaMsg
  -> IO LuaBackendState
createLuaBackendState ltem etlm = do
  lState ← Lua.newstate
  _ ← Lua.runWith lState $ Lua.openlibs
  scriptsVar ← newTVarIO Map.empty
  return LuaBackendState
    { lbsLuaState     = lState
    , lbsScripts      = scriptsVar
    , lbsMsgQueues    = (ltem, etlm)
    }

-- | Lua event loop (existing code)
runLuaLoop ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
runLuaLoop env ls stateRef = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            Lua.close (lbsLuaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000
            runLuaLoop env ls stateRef
        ThreadRunning → do
            frameStart ← getCurrentTime
            let newLuaSt = ls
            frameEnd ← getCurrentTime
            let diff  = diffUTCTime frameEnd frameStart
                usecs = floor (toRational diff * 1000000) ∷ Int
                targetFrameTime = 1000
                delay = targetFrameTime - usecs
            when (delay > 0) $ threadDelay delay
            runLuaLoop env newLuaSt stateRef
