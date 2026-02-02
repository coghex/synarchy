module Engine.Scripting.Lua.Thread
  ( startLuaThread
  , runLuaLoop
  , createLuaBackendState
  , processLuaMsg
  , processLuaMsgs
  ) where

import UPrelude
import Engine.Scripting.Types
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Shell (setupShellSandbox)
import Engine.Scripting.Lua.Script (callModuleFunction)
import Engine.Scripting.Lua.Util (isValidRef, broadcastToModules)
import Engine.Asset.Types (AssetPool)
import Engine.Core.Log (logWarn, logDebug, LogCategory(..))
import Engine.Core.Thread
import Engine.Core.State (EngineEnv(..))
import Engine.Event.Types (Event(..))
import Engine.Input.Types (InputState, keyToText)
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import Control.Monad.Logger (LogLevel(..), toLogStr, defaultLoc)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Timeout (timeout)

-- | Start the Lua scripting thread
startLuaThread ∷ EngineEnv → IO ThreadState
startLuaThread env = do
    let apRef     = assetPoolRef env
        objIdRef  = nextObjectIdRef env
        inputSRef = inputStateRef env
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua "Starting lua thread..."
            let lteq = luaToEngineQueue env
                etlq = luaQueue env
            backendState ← createLuaBackendState lteq etlq apRef objIdRef inputSRef
            registerLuaAPI (lbsLuaState backendState) env backendState
            logDebug logger CatLua "Lua API registered."
            setupShellSandbox (lbsLuaState backendState)
            logDebug logger CatLua "Shell sandbox set up."
            
            -- Load init.lua as a module
            let scriptPath = "scripts/init.lua"
            currentTime ← getCurrentTime
            let currentSecs = realToFrac $ utctDayTime currentTime
            
            initScriptId ← atomicModifyIORef' (lbsNextScriptId backendState)
                (\n → (n + 1, n))
            
            status ← Lua.runWith (lbsLuaState backendState) $ 
                Lua.dofileTrace (Just scriptPath)
            
            case status of
                Lua.OK → do
                    modRef ← Lua.runWith (lbsLuaState backendState) $ do
                        isTable ← Lua.istable (-1)
                        if isTable
                            then Lua.ref Lua.registryindex
                            else do
                                Lua.pop 1
                                return (Lua.Reference (fromIntegral Lua.refnil))
                    
                    let initScript = LuaScript
                          { scriptId        = initScriptId
                          , scriptPath      = scriptPath
                          , scriptTickRate  = 1.0
                          , scriptNextTick  = currentSecs + 1.0
                          , scriptModuleRef = modRef
                          , scriptPaused    = False
                          }
                    
                    atomically $ modifyTVar (lbsScripts backendState) $
                        Map.insert initScriptId initScript
                    
                    logDebug logger CatLua $ T.pack $ scriptPath
                                   ⧺ "init.lua loaded successfully as module."
                    
                    when (isValidRef modRef) $ do
                        _ ← callModuleFunction (lbsLuaState backendState) modRef "init" 
                            [ScriptNumber (fromIntegral initScriptId)]
                        return ()
                    
                    logDebug logger CatLua "init() called on game module."
                    
                _ → do
                    errMsg ← Lua.runWith (lbsLuaState backendState) $ do
                        err ← Lua.tostring (-1)
                        Lua.pop 1
                        return $ maybe "Unknown error" TE.decodeUtf8 err
                    logWarn logger CatLua $
                        T.pack $ "Error loading " ⧺ scriptPath ⧺ ": " ⧺ T.unpack errMsg
            
            tid ← forkIO $ runLuaLoop env backendState stateRef
            return tid
        ) 
        (\(e ∷ SomeException) → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatLua $ "Lua thread failed to start."
                                  <> T.pack (show e)
            Q.writeQueue (eventQueue env) (EventError "LuaThread" (T.pack (show e)))
            error "Lua thread failed to start."
        )
    return $ ThreadState stateRef threadId

-- | Create Lua backend state
createLuaBackendState ∷ Q.Queue LuaToEngineMsg → Q.Queue LuaMsg
                      → IORef AssetPool → IORef Word32
                      → IORef InputState → IO LuaBackendState
createLuaBackendState ltem etlm apRef objIdRef inputSRef = do
  lState ← Lua.newstate
  _ ← Lua.runWith lState $ Lua.openlibs
  scriptsVar ← newTVarIO Map.empty
  scriptIdRef ← newIORef 1
  return LuaBackendState
    { lbsLuaState     = lState
    , lbsScripts      = scriptsVar
    , lbsNextScriptId = scriptIdRef
    , lbsMsgQueues    = (ltem, etlm)
    , lbsAssetPool    = apRef
    , lbsNextObjectId = objIdRef
    , lbsInputState   = inputSRef
    }

-- | Lua event loop
runLuaLoop ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
runLuaLoop env ls stateRef = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua "Lua thread stopping..."
            Lua.close (lbsLuaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000
            runLuaLoop env ls stateRef
        ThreadRunning → do
            currentTime ← getCurrentTime
            let currentSecs = realToFrac $ utctDayTime currentTime
            scriptsMap ← readTVarIO (lbsScripts ls)
            let scripts = Map.elems scriptsMap
                nextWakeTimes = map scriptNextTick scripts
                nextWakeTime = if null nextWakeTimes
                               then currentSecs + 1.0
                               else minimum nextWakeTimes
                sleeptime = max 0.001 (nextWakeTime - currentSecs)
            let maxSleepMicros = min 16666 (floor (sleeptime * 1000000))
                (_, etlq) = lbsMsgQueues ls
            mMsg ← timeout maxSleepMicros (Q.readQueue etlq)
            case mMsg of
              Just msg → do
                  processLuaMsg env ls stateRef msg
                  processLuaMsgs env ls stateRef
                  runLuaLoop env ls stateRef
              Nothing → do
                  currentTime' ← getCurrentTime
                  let currentSecs' = realToFrac $ utctDayTime currentTime'
                  scriptsMap' ← readTVarIO (lbsScripts ls)
                  forM_ (Map.toList scriptsMap') $ \(sid, script) → do
                    when (not (scriptPaused script) && currentSecs' ≥ scriptNextTick script) $ do
                      when (isValidRef (scriptModuleRef script)) $ do
                        let dt = scriptTickRate script
                        _ ← callModuleFunction (lbsLuaState ls) (scriptModuleRef script) "update" [ScriptNumber dt]
                        return ()
                      atomically $ modifyTVar (lbsScripts ls) $
                        Map.adjust (\s → s { scriptNextTick = scriptNextTick s + scriptTickRate s }) sid
                  runLuaLoop env ls stateRef

-- | Process messages from anywhere to lua
processLuaMsgs ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
processLuaMsgs env ls stateRef = do
    let (_, etlq) = lbsMsgQueues ls
    mMsg ← Q.tryReadQueue etlq
    case mMsg of
        Just msg → do
            currentTime ← getCurrentTime
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua $ 
                "lua recv msg: " <> T.pack (show msg)
            processLuaMsg env ls stateRef msg
            processLuaMsgs env ls stateRef
        Nothing → return ()

-- | Process a single lua message
processLuaMsg ∷ EngineEnv → LuaBackendState → IORef ThreadControl → LuaMsg → IO ()
processLuaMsg env ls stateRef msg = case msg of
  LuaTextureLoaded handle assetId → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $ 
        "Texture loaded: " <> T.pack (show handle) <> " -> " <> T.pack (show assetId)
  LuaFontLoaded handle → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $ 
        "Font loaded: " <> T.pack (show handle)
  LuaFontLoadFailed err → do
    logger ← readIORef (loggerRef env)
    logWarn logger CatLua $ 
        "Font load failed: " <> T.pack (show err)
  LuaThreadKill → writeIORef stateRef ThreadStopped
  LuaMouseDownEvent button x y → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    broadcastToModules ls "onMouseDown"
      [ScriptNumber (fromIntegral buttonNum), ScriptNumber x, ScriptNumber y]
  LuaMouseUpEvent button x y → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    broadcastToModules ls "onMouseUp"
      [ScriptNumber (fromIntegral buttonNum), ScriptNumber x, ScriptNumber y]
  LuaKeyDownEvent key → 
    broadcastToModules ls "onKeyDown" [ScriptString (keyToText key)]
  LuaKeyUpEvent key → 
    broadcastToModules ls "onKeyUp" [ScriptString (keyToText key)]
  LuaShellToggle → 
    broadcastToModules ls "onShellToggle" []
  LuaCharInput fid c → 
    broadcastToModules ls "onCharInput"
      [ScriptNumber (fromIntegral fid), ScriptString (T.singleton c)]
  LuaTextBackspace fid → 
    broadcastToModules ls "onTextBackspace" [ScriptNumber (fromIntegral fid)]
  LuaTextDelete fid → 
    broadcastToModules ls "onTextDelete" [ScriptNumber (fromIntegral fid)]
  LuaTabPressed fid → 
    broadcastToModules ls "onTabPressed" [ScriptNumber (fromIntegral fid)]
  LuaTextSubmit fid → 
    broadcastToModules ls "onTextSubmit" [ScriptNumber (fromIntegral fid)]
  LuaFocusLost fid → 
    broadcastToModules ls "onFocusLost" [ScriptNumber (fromIntegral fid)]
  LuaCursorUp fid → 
    broadcastToModules ls "onCursorUp" [ScriptNumber (fromIntegral fid)]
  LuaCursorDown fid → 
    broadcastToModules ls "onCursorDown" [ScriptNumber (fromIntegral fid)]
  LuaCursorLeft fid → 
    broadcastToModules ls "onCursorLeft" [ScriptNumber (fromIntegral fid)]
  LuaCursorRight fid → 
    broadcastToModules ls "onCursorRight" [ScriptNumber (fromIntegral fid)]
  LuaCursorHome fid → 
    broadcastToModules ls "onCursorHome" [ScriptNumber (fromIntegral fid)]
  LuaCursorEnd fid → 
    broadcastToModules ls "onCursorEnd" [ScriptNumber (fromIntegral fid)]
  LuaInterrupt fid → 
    broadcastToModules ls "onInterrupt" [ScriptNumber (fromIntegral fid)]
  _ → return ()
