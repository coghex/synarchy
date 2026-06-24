-- | Lua scripting thread.
--
--   Threading model: a single dedicated OS thread owns the Lua.State.
--   All other threads (input, world, debug console) communicate via
--   STM queues (luaQueue for LuaMsg, TQueue for DebugCommand).
--   The Lua.State is NEVER accessed from another thread.
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
import Engine.Scripting.Lua.API.Shell (setupShellSandbox, luaValueToText)
import Engine.Scripting.Lua.Script (callModuleFunction, loadModuleRef)
import Engine.Scripting.Lua.Util (isValidRef, broadcastToModules, nowSeconds)
import Engine.Scripting.Lua.DebugServer (DebugCommand(..), startDebugServer, pollDebugCommand)
import Engine.Asset.Types (AssetPool)
import Engine.Core.Log (logWarn, logDebug, logInfo, LogCategory(..), LoggerState)
import Engine.Core.Thread
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import World.State.Types (wmWorlds, wsLoadPhaseRef, wsInitQueueRef, LoadPhase(..))
import Engine.Core.Types (EngineConfig(..))
import Engine.Event.Types (Event(..))
import Engine.Input.Types (InputState, keyToText, clickRouteText)
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.List (find, sortBy, sort)
import qualified Data.Text.Read as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryPutMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forM_, when)
import Control.Monad.Logger (LogLevel(..), toLogStr, defaultLoc)

startLuaThread ∷ EngineEnv → IO ThreadState
startLuaThread env = do
    let apRef     = assetPoolRef env
        objIdRef  = nextObjectIdRef env
        inputSRef = inputStateRef env
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do
            logger ← readIORef (loggerRef env)
            logInfo logger CatLua "Starting Lua scripting thread..."
            let lteq = luaToEngineQueue env
                etlq = luaQueue env
            backendState ← createLuaBackendState lteq etlq apRef objIdRef inputSRef (loggerRef env)
            registerLuaAPI (lbsLuaState backendState) env backendState
            logDebug logger CatLua "Lua API registered."
            setupShellSandbox (lbsLuaState backendState)
            logDebug logger CatLua "Shell sandbox set up."
            
            let scriptPath = "scripts/init.lua"
            currentSecs ← nowSeconds
            
            initScriptId ← atomicModifyIORef' (lbsNextScriptId backendState)
                (\n → (n + 1, n))
            
            result ← Lua.runWith (lbsLuaState backendState) $
                loadModuleRef scriptPath

            case result of
                Right modRef → do
                    logDebug logger CatLua $ "Lua script loaded: " <> T.pack scriptPath
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
                        _ ← callModuleFunction backendState modRef "init" 
                            [ScriptNumber (fromIntegral initScriptId)]
                        return ()
                    
                    logDebug logger CatLua "Lua module initialized"

                Left errMsg →
                    logWarn logger CatLua $
                        "Failed to load Lua script: " <> T.pack scriptPath
                        <> " - " <> errMsg
            
            let port = ecDebugPort (engineConfig env)
            eDebugQueue ← startDebugServer port (debugBuiltin env)
            debugQueue ← case eDebugQueue of
                Right q → do
                    logInfo logger CatLua $
                        "Debug server listening on port " <> T.pack (show port)
                    return q
                Left err → do
                    -- Engine keeps running without a console; the queue
                    -- is inert (nothing ever feeds it).
                    logWarn logger CatLua $
                        "Debug server failed to start on port "
                        <> T.pack (show port) <> ": " <> err
                    atomically newTQueue
            tid ← forkIO $ runLuaLoop env backendState stateRef debugQueue `finally` putMVar doneVar ()
            return tid
        ) 
        (\(e ∷ SomeException) → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatLua $ "Lua thread failed to start: " <> T.pack (show e)
            Q.writeQueue (eventQueue env) (EventError "LuaThread" (T.pack (show e)))
            error "Lua thread failed to start."
        )
    return $ ThreadState stateRef threadId doneVar

createLuaBackendState ∷ Q.Queue LuaToEngineMsg → Q.Queue LuaMsg
                      → IORef AssetPool → IORef Word32
                      → IORef InputState → IORef LoggerState → IO LuaBackendState
createLuaBackendState ltem etlm apRef objIdRef inputSRef loggerR = do
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
    , lbsLoggerRef    = loggerR
    }

runLuaLoop ∷ EngineEnv → LuaBackendState → IORef ThreadControl
           → TQueue DebugCommand → IO ()
runLuaLoop env ls stateRef debugQueue = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua "Lua thread stopped"
            -- Answer any debug commands still queued at teardown so their
            -- client threads (and netcat connections) don't sit out the full
            -- 30 s response timeout while the engine shuts down. Mirrors the
            -- crash handler's drain.
            let drainDebug = do
                    mCmd ← pollDebugCommand debugQueue
                    case mCmd of
                        Nothing → pure ()
                        Just (DebugCommand _ mvar) → do
                            _ ← tryPutMVar mvar "engine shutting down"
                            drainDebug
            drainDebug
            Lua.close (lbsLuaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000
            runLuaLoop env ls stateRef debugQueue
        ThreadRunning → do
            -- One guarded tick per iteration; the recursive call lives
            -- OUTSIDE the catch — inside it, each tick pushes a catch
            -- frame that never pops (unbounded stack growth).
            ok ← catch
              (do
                processDebugCommands (lbsLuaState ls) debugQueue

                currentSecs ← nowSeconds
                scriptsMap ← readTVarIO (lbsScripts ls)
                let scripts = Map.elems scriptsMap
                    -- Paused scripts never advance their nextTick;
                    -- including them would pin sleeptime at the floor
                    -- and busy-spin the loop at ~1 kHz.
                    nextWakeTimes = map scriptNextTick
                                        (filter (not ∘ scriptPaused) scripts)
                    nextWakeTime = if null nextWakeTimes
                                   then currentSecs + 1.0
                                   else minimum nextWakeTimes
                    sleeptime = max 0.001 (nextWakeTime - currentSecs)
                let maxSleepMicros = min 16666 (floor (sleeptime * 1000000))
                    (_, etlq) = lbsMsgQueues ls
                -- readQueueTimeout, NOT System.Timeout around readQueue:
                -- the timeout exception can land after the STM dequeue
                -- commits, silently dropping the message.
                mMsg ← Q.readQueueTimeout maxSleepMicros etlq
                case mMsg of
                  Just msg → do
                      processLuaMsg env ls stateRef msg
                      processLuaMsgs env ls stateRef
                      pure True
                  Nothing → do
                      currentSecs' ← nowSeconds
                      scriptsMap' ← readTVarIO (lbsScripts ls)
                      forM_ (Map.toList scriptsMap') $ \(sid, script) → do
                        when (not (scriptPaused script) ∧ currentSecs' ≥ scriptNextTick script) $ do
                          when (isValidRef (scriptModuleRef script)) $ do
                            let dt = scriptTickRate script
                            _ ← callModuleFunction ls (scriptModuleRef script) "update" [ScriptNumber dt]
                            return ()
                          atomically $ modifyTVar (lbsScripts ls) $
                            Map.adjust (\s → s { scriptNextTick = scriptNextTick s + scriptTickRate s }) sid
                      pure True
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
                pure False
              )
            when ok $ runLuaLoop env ls stateRef debugQueue

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

-- | Debug-console BUILT-INS, run on the per-connection client thread
--   (see 'startDebugServer') so they never block the single Lua thread.
--
--   'world.waitForInit' / 'world.waitForChunks' only poll world-state
--   IORefs — no Lua state — so handling them here means: (a) the Lua
--   thread stays free (a second connection can poll progress mid-wait,
--   ticks keep running); (b) there's no debug-server response cap on the
--   wait (the old 30 s 'takeMVar' timeout used to make 'waitForInit(300)'
--   over netcat spuriously report a timeout on any world taking >30 s to
--   generate). Returns 'Nothing' for anything it doesn't recognise, so
--   the command falls through to the Lua thread unchanged.
debugBuiltin ∷ EngineEnv → Text → IO (Maybe Text)
debugBuiltin env cmd =
    let t0 = T.strip cmd
        t1 = maybe t0 T.strip (T.stripPrefix "return " t0)
        t2 = T.strip (fromMaybe t1 (T.stripSuffix ";" t1))
        isQuit = t2 ≡ "engine.quit"
               ∨ case matchCall "engine.quit" t2 of Just _ → True; Nothing → False
    in if isQuit
       -- Handle quit HERE, on the client thread, so the ack is sent before
       -- the Lua thread (which would otherwise answer this command) is torn
       -- down. Round-tripping quit through the thread it's about to kill is
       -- the shutdown race that left the client blocked on the full 30 s
       -- response timeout. Same effect as quitFn: just flip the lifecycle
       -- flag; the main/headless loop drives the actual teardown.
       then writeIORef (lifecycleRef env) CleaningUp ≫ return (Just "shutting down")
       else case matchCall "world.waitForInit" t2 of
           Just arg → Just <$> runWaitForInit env (fromMaybe 600 arg)
           Nothing  → case matchCall "world.waitForChunks" t2 of
               Just arg → Just <$> runWaitForChunks env (fromMaybe 120 arg)
               Nothing  → return Nothing

-- | Match @<fn>(<int?>)@ exactly. @Just Nothing@ = no/empty arg (use the
--   caller's default); @Just (Just n)@ = explicit timeout; @Nothing@ =
--   not this call, or a non-integer arg → fall through to Lua.
matchCall ∷ Text → Text → Maybe (Maybe Int)
matchCall fn t = case T.stripPrefix fn t of
    Nothing → Nothing
    Just rest →
        let r = T.strip rest
        in if not (T.null r) ∧ T.head r ≡ '(' ∧ T.last r ≡ ')'
           then let inner = T.strip (T.init (T.drop 1 r))
                in if T.null inner
                   then Just Nothing
                   else case T.decimal inner of
                          Right (n, rm) | T.null (T.strip rm) → Just (Just n)
                          _                                   → Nothing
           else Nothing

-- | Poll the active world's load phase until done (or timeout), then
--   return the same tab-joined progress 'world.getInitProgress' yields.
runWaitForInit ∷ EngineEnv → Int → IO Text
runWaitForInit env timeoutSec = loop (timeoutSec * 4) ⌦ \_ → fmtInitProgress env
  where
    loop ∷ Int → IO ()
    loop 0 = return ()
    loop n = do
        mgr ← readIORef (worldManagerRef env)
        case wmWorlds mgr of
            ((_, ws):_) → do
                phase ← readIORef (wsLoadPhaseRef ws)
                case phase of
                    LoadDone → return ()
                    _        → threadDelay 250000 ≫ loop (n - 1)
            [] → threadDelay 250000 ≫ loop (n - 1)

-- | Poll the active world's init queue until empty (or timeout); return
--   the remaining chunk count (matches 'world.waitForChunks').
runWaitForChunks ∷ EngineEnv → Int → IO Text
runWaitForChunks env timeoutSec = T.pack ∘ show ⊚ loop (timeoutSec * 4)
  where
    remaining ∷ IO Int
    remaining = do
        mgr ← readIORef (worldManagerRef env)
        case wmWorlds mgr of
            ((_, ws):_) → length ⊚ readIORef (wsInitQueueRef ws)
            []          → return 0
    loop ∷ Int → IO Int
    loop 0 = remaining
    loop n = do
        r ← remaining
        if r ≡ 0 then return 0 else threadDelay 250000 ≫ loop (n - 1)

-- | Format the active world's load phase as the four tab-separated
--   values 'world.getInitProgress' returns: phase, current, total, stage.
fmtInitProgress ∷ EngineEnv → IO Text
fmtInitProgress env = do
    mgr ← readIORef (worldManagerRef env)
    case wmWorlds mgr of
        ((_, ws):_) → do
            phase ← readIORef (wsLoadPhaseRef ws)
            return $ case phase of
                LoadIdle           → fmt 0 0 0 "idle"
                LoadPhase1 c t     → fmt 1 c t "setup"
                LoadPhase2 rm t    → fmt 2 (t - rm) t "chunks"
                LoadDone           → fmt 3 1 1 "done"
        [] → return (fmt 0 0 0 "idle")
  where
    -- Match 'world.getInitProgress' over the console exactly: the stage
    -- string is rendered quoted by 'luaValueToText', so quote it here.
    fmt ∷ Int → Int → Int → Text → Text
    fmt a b c s = T.intercalate "\t" [tshow a, tshow b, tshow c, "\"" <> s <> "\""]
    tshow = T.pack ∘ show

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
  LuaMouseUpEvent button x y downRoute → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    -- onMouseUp fires on every physical release (UI widget drags
    -- depend on it); the 4th arg says where the matching press was
    -- routed ("game"/"ui"/"swallowed") so handlers can pair with
    -- onMouseDown by filtering on "game".
    broadcastToModules ls "onMouseUp"
      [ ScriptNumber (fromIntegral buttonNum), ScriptNumber x, ScriptNumber y
      , ScriptString (clickRouteText downRoute) ]
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
          _ ← callModuleFunction ls
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
          _ ← callModuleFunction ls
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
          _ ← callModuleFunction ls
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
  LuaHudLogResourcesInfo text →
    broadcastToModules ls "onSetResourcesInfo" [ScriptString text]
  LuaWorldPreviewReady handleInt →
    broadcastToModules ls "onWorldPreviewReady"
      [ScriptNumber (fromIntegral handleInt)]
  LuaShowPopup category msg r g b a buttons mCoords →
    broadcastToModules ls "onShowPopup"
      [ ScriptString category
      , ScriptString msg
      , ScriptNumber (realToFrac r)
      , ScriptNumber (realToFrac g)
      , ScriptNumber (realToFrac b)
      , ScriptNumber (realToFrac a)
      , buttonsToScriptValue buttons
      , coordsToScriptValue mCoords
      ]

-- | Build a Lua array @{ {label=..., action=...}, ... }@ from the
--   button list carried by 'LuaShowPopup'. Lua-side popup module
--   iterates with @ipairs@ and reads @b.label@ / @b.action@.
buttonsToScriptValue ∷ [(Text, Text)] → ScriptValue
buttonsToScriptValue btns = ScriptTable $
    zipWith (\i (lbl, act) →
        (ScriptNumber (fromIntegral (i ∷ Int))
        , ScriptTable
            [ (ScriptString "label",  ScriptString lbl)
            , (ScriptString "action", ScriptString act)
            ]))
        [1..] btns

-- | Encode the optional payload as either @{x=gx, y=gy}@ or 'nil'.
--   Lua-side popup module checks for nil before allowing 'go_to'.
coordsToScriptValue ∷ Maybe (Int, Int) → ScriptValue
coordsToScriptValue Nothing = ScriptNil
coordsToScriptValue (Just (gx, gy)) = ScriptTable
    [ (ScriptString "x", ScriptNumber (fromIntegral gx))
    , (ScriptString "y", ScriptNumber (fromIntegral gy))
    ]
