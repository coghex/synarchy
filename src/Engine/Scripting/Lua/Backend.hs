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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text.Encoding as TE
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when, unless)
import Control.Monad.Logger (Loc(..), LogLevel(..), toLogStr, defaultLoc)
import Data.Time.Clock (getCurrentTime, diffUTCTime, utctDayTime)

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

registerLuaAPI ∷ Lua.State → EngineEnv → LuaBackendState → IO ()
registerLuaAPI lst env backendState = Lua.runWith lst $ do
  Lua.newtable 
  -- engine.logInfo
  Lua.pushHaskellFunction (logInfoFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "logInfo")
  -- engine.setTickInterval
  Lua.pushHaskellFunction (setTickIntervalFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "setTickInterval")
  Lua.setglobal (Lua.Name "engine")
  where
    logInfoFn = do
      msg ← Lua.tostring 1
      let lf = logFunc env
      case msg of
        Just bs → Lua.liftIO $ lf defaultLoc "lua" LevelInfo (toLogStr $ TE.decodeUtf8 bs)
        Nothing → return ()
      return 0
    setTickIntervalFn = do
      interval ← Lua.tonumber 1
      case interval of
        Just (Lua.Number seconds) → Lua.liftIO $ do
          scriptPathMaybe ← Lua.runWith lst $ do
            _ ← Lua.getfield Lua.registryindex (Lua.Name "_CURRENT_SCRIPT") ∷  Lua.LuaE Lua.Exception Lua.Type
            Lua.tostring (-1)
          case scriptPathMaybe of
            Just pathBS → do
              let scriptPath = TE.decodeUtf8 pathBS
                  lf = logFunc env
              currentTime ← getCurrentTime
              let currentSecs = realToFrac $ utctDayTime currentTime
              atomically $ modifyTVar (lbsScripts backendState) $ Map.adjust
                (\s -> s { scriptTickRate = seconds
                         , scriptNextTick = currentSecs + seconds
                         }) (T.unpack scriptPath)
              lf defaultLoc "lua" LevelInfo $ toLogStr $
                "Tick interval for script " ⧺ (show scriptPath) ⧺ " set to " ⧺ (show seconds) ⧺ " seconds."
            Nothing → do
              let lf = logFunc env
              lf defaultLoc "lua" LevelError "setTickInterval called outside of script context."
        Nothing → return ()
      return 0

-- | Helper to call Lua function with explicit type
callLuaFunction :: T.Text -> [ScriptValue] -> Lua.LuaE Lua.Exception Lua.Status
callLuaFunction funcName args = do
  let name = Lua.Name (TE.encodeUtf8 funcName)
  _ <- Lua.getglobal name
  -- push arguments onto the lua stack
  forM_ args $ \arg → case arg of
    ScriptNumber n → Lua.pushnumber (Lua.Number n)
    ScriptString s → Lua.pushstring (TE.encodeUtf8 s)
    ScriptBool b   → Lua.pushboolean b
    ScriptNil      → Lua.pushnil
    _              → Lua.pushnil -- unsupported types push nil for now
  let numArgs = fromIntegral (length args)
  Lua.call numArgs Lua.multret
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
            -- register lua api
            registerLuaAPI (lbsLuaState backendState) env backendState
            lf defaultLoc "lua" LevelInfo "Lua API registered."
            let scriptPath = "scripts/init.lua"
            Lua.runWith (lbsLuaState backendState) $ do
              Lua.pushstring (TE.encodeUtf8 $ T.pack scriptPath)
              Lua.setfield Lua.registryindex (Lua.Name "_CURRENT_SCRIPT") ∷  Lua.LuaE Lua.Exception ()
            currentTime ← getCurrentTime 
            let currentSecs = realToFrac $ utctDayTime currentTime
                defaultScript = LuaScript
                  { scriptPath     = scriptPath
                  , scriptTickRate = 1.0 -- default 1 second tick rate
                  , scriptNextTick = currentSecs + 1.0 }
            atomically $ modifyTVar (lbsScripts backendState) $
              Map.insert scriptPath defaultScript
            backend ← createLuaBackend
            let scriptCtx = ScriptContext (toDyn (lbsLuaState backendState))
            loadResult ← loadScript backend scriptCtx scriptPath
            case loadResult of
              ScriptSuccess _ → lf defaultLoc "lua" LevelInfo $ toLogStr $
                                  (T.pack $ (show scriptPath) ⧺ " loaded successfully.")
              ScriptError err → lf defaultLoc "lua" LevelError $ toLogStr $
                                  T.pack $ "Failed to load " ⧺ scriptPath ⧺ ": " ⧺ (show err)
            initResult ← callFunction backend scriptCtx "init" []
            case initResult of
              ScriptSuccess _ → lf defaultLoc "lua" LevelInfo
                                  "init() function executed successfully."
              ScriptError err → lf defaultLoc "lua" LevelError $ toLogStr $
                                  "Failed to execute init(): " ⧺ (show err)
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
            let lf = logFunc env
            lf defaultLoc "lua" LevelInfo "Stopping lua thread..."
            Lua.close (lbsLuaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000
            runLuaLoop env ls stateRef
        ThreadRunning → do
            frameStart ← getCurrentTime
            let currentSecs = realToFrac $ utctDayTime frameStart
            -- get scripts
            scriptsMap ← readTVarIO (lbsScripts ls)
            let scripts = Map.elems scriptsMap
            -- find next wake time
            let nextWakeTimes = map scriptNextTick scripts
                nextWakeTime = if null nextWakeTimes
                               then currentSecs + 1.0
                               else minimum nextWakeTimes
                sleepTime = nextWakeTime - currentSecs
            when (sleepTime > 0) $ do
                let sleepMicros = min (floor (sleepTime * 1000000)) 1000000 -- max 100ms sleep
                threadDelay sleepMicros
            -- check control again
            control' ← readIORef stateRef
            unless (control' ≡ ThreadStopped) $ do
              currentTime ← getCurrentTime
              let currentSecs' = realToFrac $ utctDayTime currentTime
              -- execture all scripts whose time has come
              scriptsMap' ← readTVarIO (lbsScripts ls)
              forM_ (Map.toList scriptsMap') $ \(path, script) → do
                when (currentSecs' ≥ scriptNextTick script) $ do
                    -- set current script in registry
                    Lua.runWith (lbsLuaState ls) $ do
                      Lua.pushstring (TE.encodeUtf8 $ T.pack path)
                      Lua.setfield Lua.registryindex (Lua.Name "_CURRENT_SCRIPT") ∷  Lua.LuaE Lua.Exception ()
                    -- execute update function
                    backend ← createLuaBackend
                    let scriptCtx = ScriptContext (toDyn (lbsLuaState ls))
                        dt        = scriptTickRate script
                    _ ← callFunction backend scriptCtx "update" [ScriptNumber dt]
                    -- update next tick time (preserve overshoot for determinism)
                    atomically $ modifyTVar (lbsScripts ls) $ Map.adjust
                      (\s -> s { scriptNextTick = scriptNextTick s + scriptTickRate s }) path

              -- check for messages from engine
              let (_, etlq) = lbsMsgQueues ls
              msg ← Q.tryReadQueue etlq
              case msg of
                  Just LuaThreadKill → writeIORef stateRef ThreadStopped
                  _                  → return ()
              -- continue loop
              runLuaLoop env ls stateRef
