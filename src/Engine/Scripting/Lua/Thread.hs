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
import Engine.Scripting.Lua.DebugServer (DebugCommand(..), startDebugServer, pollDebugCommand)
import Engine.Asset.Types (AssetPool)
import Engine.Core.Log (logWarn, logDebug, logInfo, LogCategory(..))
import Engine.Core.Thread
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (EngineConfig(..))
import Engine.Event.Types (Event(..))
import Engine.Input.Types (InputState, keyToText)
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.List (find, sortBy)
import qualified Data.Text.Read as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (putMVar, tryPutMVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import Control.Monad.Logger (LogLevel(..), toLogStr, defaultLoc)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import System.Timeout (timeout)

startLuaThread ∷ EngineEnv → IO ThreadState
startLuaThread env = do
    let apRef     = assetPoolRef env
        objIdRef  = nextObjectIdRef env
        inputSRef = inputStateRef env
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            logger ← readIORef (loggerRef env)
            logInfo logger CatLua "Starting Lua scripting thread..."
            let lteq = luaToEngineQueue env
                etlq = luaQueue env
            backendState ← createLuaBackendState lteq etlq apRef objIdRef inputSRef
            registerLuaAPI (lbsLuaState backendState) env backendState
            logDebug logger CatLua "Lua API registered."
            setupShellSandbox (lbsLuaState backendState)
            logDebug logger CatLua "Shell sandbox set up."
            
            let scriptPath = "scripts/init.lua"
            currentTime ← getCurrentTime
            let currentSecs = realToFrac $ utctDayTime currentTime
            
            initScriptId ← atomicModifyIORef' (lbsNextScriptId backendState)
                (\n → (n + 1, n))
            
            status ← Lua.runWith (lbsLuaState backendState) $ 
                Lua.dofileTrace (Just scriptPath)
            
            case status of
                Lua.OK → do
                    logDebug logger CatLua $ "Lua script loaded: " <> T.pack scriptPath
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
                    
                    logDebug logger CatLua $ "Lua script module loaded with ID: " 
                                   <> T.pack (show initScriptId)
                    
                    when (isValidRef modRef) $ do
                        logDebug logger CatLua "Calling init() on Lua module"
                        _ ← callModuleFunction (lbsLuaState backendState) modRef "init" 
                            [ScriptNumber (fromIntegral initScriptId)]
                        return ()
                    
                    logDebug logger CatLua "Lua module initialized"

                _ → do
                    errMsg ← Lua.runWith (lbsLuaState backendState) $ do
                        err ← Lua.tostring (-1)
                        Lua.pop 1
                        return $ maybe "Unknown error" TE.decodeUtf8 err
                    logWarn logger CatLua $
                        "Failed to load Lua script: " <> T.pack scriptPath 
                        <> " - " <> errMsg
            
            let port = ecDebugPort (engineConfig env)
            debugQueue ← startDebugServer port
            logInfo logger CatLua $ "Debug server listening on port " <> T.pack (show port)
            tid ← forkIO $ runLuaLoop env backendState stateRef debugQueue
            return tid
        ) 
        (\(e ∷ SomeException) → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatLua $ "Lua thread failed to start: " <> T.pack (show e)
            Q.writeQueue (eventQueue env) (EventError "LuaThread" (T.pack (show e)))
            error "Lua thread failed to start."
        )
    return $ ThreadState stateRef threadId

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

runLuaLoop ∷ EngineEnv → LuaBackendState → IORef ThreadControl
           → TQueue DebugCommand → IO ()
runLuaLoop env ls stateRef debugQueue = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua "Lua thread stopped"
            Lua.close (lbsLuaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000
            runLuaLoop env ls stateRef debugQueue
        ThreadRunning → do
            catch
              (do
                processDebugCommands (lbsLuaState ls) debugQueue

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
                      runLuaLoop env ls stateRef debugQueue
                  Nothing → do
                      currentTime' ← getCurrentTime
                      let currentSecs' = realToFrac $ utctDayTime currentTime'
                      scriptsMap' ← readTVarIO (lbsScripts ls)
                      forM_ (Map.toList scriptsMap') $ \(sid, script) → do
                        when (not (scriptPaused script) ∧ currentSecs' ≥ scriptNextTick script) $ do
                          when (isValidRef (scriptModuleRef script)) $ do
                            let dt = scriptTickRate script
                            _ ← callModuleFunction (lbsLuaState ls) (scriptModuleRef script) "update" [ScriptNumber dt]
                            return ()
                          atomically $ modifyTVar (lbsScripts ls) $
                            Map.adjust (\s → s { scriptNextTick = scriptNextTick s + scriptTickRate s }) sid
                      runLuaLoop env ls stateRef debugQueue
              )
              (\(e ∷ SomeException) → do
                logger ← readIORef (loggerRef env)
                logWarn logger CatLua $ "Lua thread crashed: " <> T.pack (show e)
                -- Drain pending debug commands so clients don't hang
                let drainDebug = do
                        mCmd ← pollDebugCommand debugQueue
                        case mCmd of
                            Nothing → pure ()
                            Just (DebugCommand _ mvar) → do
                                _ ← tryPutMVar mvar ("ERROR: Lua thread crashed: " <> T.pack (show e))
                                drainDebug
                drainDebug
                Lua.close (lbsLuaState ls)
                writeIORef (lifecycleRef env) CleaningUp
              )

-- | Process all pending debug commands from the TCP server.
--   Each command is a line of Lua code. We execute it via
--   loadstring + pcall and send the result back through the MVar.
processDebugCommands ∷ Lua.State → TQueue DebugCommand → IO ()
processDebugCommands lst debugQueue = do
    mCmd ← pollDebugCommand debugQueue
    case mCmd of
        Nothing → return ()
        Just (DebugCommand cmdText responseMVar) → do
            result ← executeDebugLua lst cmdText
            putMVar responseMVar result
            processDebugCommands lst debugQueue

-- | Execute a Lua string and return the result as text.
--   Uses loadstring to compile, then pcall to run safely.
--   Captures return values and any errors.
--   Tables are automatically serialized to JSON format.
executeDebugLua ∷ Lua.State → Text → IO Text
executeDebugLua lst cmdText = Lua.runWith lst $ do
    let code = TE.encodeUtf8 cmdText
        chunkName = Lua.Name ("=" <> code)
    -- Try wrapping in "return ..." first for expressions
    let returnWrapped = "return " <> code
    status ← Lua.loadbuffer returnWrapped chunkName
    status' ← if status ≡ Lua.OK
        then return Lua.OK
        else do
            Lua.pop 1  -- pop error from failed load
            Lua.loadbuffer code chunkName
    case status' of
        Lua.OK → do
            -- Run the loaded chunk with pcall
            callStatus ← Lua.pcall 0 Lua.multret Nothing
            case callStatus of
                Lua.OK → do
                    -- Collect all return values
                    top ← Lua.gettop
                    if top ≡ 0
                        then return "ok"
                        else do
                            parts ← forM [1..top] $ \i →
                                luaValueToText 0 i
                            Lua.settop 0
                            return (T.intercalate "\t" parts)
                _ → do
                    err ← Lua.tostring (-1)
                    Lua.pop 1
                    return $ "error: " <> maybe "unknown" TE.decodeUtf8 err
        _ → do
            err ← Lua.tostring (-1)
            Lua.pop 1
            return $ "syntax error: " <> maybe "unknown" TE.decodeUtf8 err

-- | Convert a Lua value at the given stack index to a Text representation.
--   Tables are recursively serialized to JSON format.
--   Depth limit prevents infinite recursion on circular references.
luaValueToText ∷ Int → Lua.StackIndex → Lua.LuaE Lua.Exception Text
luaValueToText depth idx
    | depth > 8 = return "\"<max depth>\""
    | otherwise = do
        ty ← Lua.ltype idx
        case ty of
            Lua.TypeNil     → return "null"
            Lua.TypeBoolean → do
                b ← Lua.toboolean idx
                return $ if b then "true" else "false"
            Lua.TypeNumber  → do
                Lua.pushvalue idx
                mStr ← Lua.tostring (-1)
                Lua.pop 1
                return $ maybe "0" TE.decodeUtf8 mStr
            Lua.TypeString  → do
                mStr ← Lua.tostring idx
                return $ case mStr of
                    Just bs → "\"" <> escapeJsonText (TE.decodeUtf8 bs) <> "\""
                    Nothing → "\"\""
            Lua.TypeTable   → luaTableToJson depth idx
            _               → do
                -- Function, userdata, thread, etc.
                Lua.pushvalue idx
                mStr ← Lua.tostring (-1)
                Lua.pop 1
                return $ case mStr of
                    Just bs → TE.decodeUtf8 bs
                    Nothing → "\"<" <> T.pack (show ty) <> ">\""

-- | Serialize a Lua table to JSON. Detects arrays vs objects:
--   if all keys are consecutive integers starting at 1, emit [...],
--   otherwise emit {...}.
luaTableToJson ∷ Int → Lua.StackIndex → Lua.LuaE Lua.Exception Text
luaTableToJson depth idx = do
    -- First pass: check if it's an array (consecutive integer keys 1..n)
    let absIdx = if idx < 0 then idx - 1 else idx
    Lua.pushnil  -- first key
    pairs ← collectTablePairs (depth + 1) absIdx []
    let isArray = not (null pairs)
                ∧ all (\(k, _) → case T.decimal k of
                        Right (n, rest) → T.null rest ∧ n > (0 ∷ Int)
                        _ → False) pairs
    if isArray
        then do
            -- Sort by integer key and emit as array
            let readInt t = case T.decimal t of
                    Right (n, _) → n ∷ Int
                    _            → 0
                sorted = sortBy (\(a,_) (b,_) → compare (readInt a) (readInt b)) pairs
            return $ "[" <> T.intercalate "," (map snd sorted) <> "]"
        else do
            let entries = map (\(k, v) → "\"" <> escapeJsonText k <> "\":" <> v) pairs
            return $ "{" <> T.intercalate "," entries <> "}"

-- | Collect all key-value pairs from a table. Leaves stack clean.
collectTablePairs ∷ Int → Lua.StackIndex → [(Text, Text)]
                  → Lua.LuaE Lua.Exception [(Text, Text)]
collectTablePairs depth tableIdx acc = do
    hasNext ← Lua.next tableIdx
    if not hasNext
        then return (reverse acc)
        else do
            -- Stack: ... table ... key value
            valText ← luaValueToText depth (-1)
            -- Get key as text (careful: tostring on key would break next())
            keyText ← do
                keyTy ← Lua.ltype (-2)
                case keyTy of
                    Lua.TypeNumber → do
                        Lua.pushvalue (-2)
                        mStr ← Lua.tostring (-1)
                        Lua.pop 1
                        return $ maybe "0" TE.decodeUtf8 mStr
                    Lua.TypeString → do
                        mStr ← Lua.tostring (-2)
                        return $ maybe "" TE.decodeUtf8 mStr
                    _ → return "<key>"
            Lua.pop 1  -- pop value, keep key for next iteration
            collectTablePairs depth tableIdx ((keyText, valText) : acc)

-- | Escape special characters for JSON string values.
escapeJsonText ∷ Text → Text
escapeJsonText = T.concatMap $ \c → case c of
    '"'  → "\\\""
    '\\' → "\\\\"
    '\n' → "\\n"
    '\r' → "\\r"
    '\t' → "\\t"
    _    → T.singleton c

processLuaMsgs ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
processLuaMsgs env ls stateRef = do
    let (_, etlq) = lbsMsgQueues ls
    mMsg ← Q.tryReadQueue etlq
    case mMsg of
        Just msg → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua $ "Engine-to-Lua message: " <> T.pack (show msg)
            processLuaMsg env ls stateRef msg
            processLuaMsgs env ls stateRef
        Nothing → return ()

processLuaMsg ∷ EngineEnv → LuaBackendState → IORef ThreadControl → LuaMsg → IO ()
processLuaMsg env ls stateRef msg = case msg of
  LuaTextureLoaded handle assetId → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $ 
        "Texture loaded with handle " <> T.pack (show handle) 
        <> " as asset " <> T.pack (show assetId)
  LuaFontLoaded handle path → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $ 
        "Font " <> T.pack (show path)
        <> " loaded with handle " <> T.pack (show handle)
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
  LuaScrollEvent dx dy → do
    broadcastToModules ls "onScroll"
      [ ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
      ]
  LuaZSliceScroll dx dy → do
    broadcastToModules ls "onZSliceScroll"
      [ ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
      ]
  LuaUIClickEvent elemHandle callbackName → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIRightClickEvent elemHandle callbackName → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIScrollEvent elemHandle dx dy → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls "onUIScroll"
      [ ScriptNumber (fromIntegral h)
      , ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
      ]
  LuaUICharInput c → 
    broadcastToModules ls "onUICharInput" [ScriptString (T.singleton c)]
  LuaUIBackspace → 
    broadcastToModules ls "onUIBackspace" []
  LuaUIDelete → 
    broadcastToModules ls "onUIDelete" []
  LuaUISubmit → 
    broadcastToModules ls "onUISubmit" []
  LuaUIEscape → 
    broadcastToModules ls "onUIEscape" []
  LuaUICursorLeft → 
    broadcastToModules ls "onUICursorLeft" []
  LuaUICursorRight → 
    broadcastToModules ls "onUICursorRight" []
  LuaUIHome → 
    broadcastToModules ls "onUIHome" []
  LuaUIEnd → 
    broadcastToModules ls "onUIEnd" []
  LuaUIFocusLost → 
    broadcastToModules ls "onUIFocusLost" []
  LuaKeyDownEvent key → 
    broadcastToModules ls "onKeyDown" [ScriptString (keyToText key)]
  LuaKeyUpEvent key → 
    broadcastToModules ls "onKeyUp" [ScriptString (keyToText key)]
  LuaShellToggle → 
    broadcastToModules ls "onShellToggle" []
  LuaArenaReady pageId →
    broadcastToModules ls "onArenaReady" [ScriptString pageId]
  LuaOpenArena →
    broadcastToModules ls "onOpenArena" []
  LuaDebugToggle → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua "Debug overlay toggle requested"
    scriptsMap ← readTVarIO (lbsScripts ls)
    let mDebugScript = find (\s → scriptPath s ≡ "scripts/debug.lua")
                            (Map.elems scriptsMap)
    case mDebugScript of
      Just debugScript → do
        when (isValidRef (scriptModuleRef debugScript)) $ do
          _ ← callModuleFunction (lbsLuaState ls)
                                 (scriptModuleRef debugScript) "toggle" []
          return ()
      Nothing → 
        logWarn logger CatLua "Debug script not found"
  LuaDebugShow → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua "Debug overlay show requested"
    scriptsMap ← readTVarIO (lbsScripts ls)
    let mDebugScript = find (\s → scriptPath s ≡ "scripts/debug.lua")
                            (Map.elems scriptsMap)
    case mDebugScript of
      Just debugScript → do
        when (isValidRef (scriptModuleRef debugScript)) $ do
          _ ← callModuleFunction (lbsLuaState ls)
                                 (scriptModuleRef debugScript) "show" []
          return ()
      Nothing → 
        logWarn logger CatLua "Debug script not found"
  LuaDebugHide → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua "Debug overlay hide requested"
    scriptsMap ← readTVarIO (lbsScripts ls)
    let mDebugScript = find (\s → scriptPath s ≡ "scripts/debug.lua")
                            (Map.elems scriptsMap)
    case mDebugScript of
      Just debugScript → do
        when (isValidRef (scriptModuleRef debugScript)) $ do
          _ ← callModuleFunction (lbsLuaState ls)
                                 (scriptModuleRef debugScript) "hide" []
          return ()
      Nothing → 
        logWarn logger CatLua "Debug script not found"
  LuaWindowResize w h → do
    broadcastToModules ls "onWindowResize"
      [ScriptNumber (fromIntegral w), ScriptNumber (fromIntegral h)]
  LuaFramebufferResize w h → do
    broadcastToModules ls "onFramebufferResize"
      [ScriptNumber (fromIntegral w), ScriptNumber (fromIntegral h)]
  LuaAssetLoaded assetType handle path → do
    broadcastToModules ls "onAssetLoaded"
      [ ScriptString assetType
      , ScriptNumber (fromIntegral handle)
      , ScriptString path
      ]
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
  LuaWorldGenLog text →
    broadcastToModules ls "onWorldGenLog" [ScriptString text]
  LuaHudLogInfo text1 text2 →
    broadcastToModules ls "onSetInfoText" [ScriptString text1, ScriptString text2]
  LuaHudLogWeatherInfo text →
    broadcastToModules ls "onSetWeatherInfo" [ScriptString text]
  LuaWorldPreviewReady handleInt →
    broadcastToModules ls "onWorldPreviewReady"
      [ScriptNumber (fromIntegral handleInt)]
  _ → return ()
