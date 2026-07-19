-- | Lua scripting thread.
--
--   Threading model: a single dedicated OS thread owns the Lua.State.
--   All other threads (input, world, debug console) communicate via
--   STM queues (luaQueue for LuaMsg, TQueue for DebugCommand).
--   The Lua.State is NEVER accessed from another thread.
--
--   Debug-console command handling lives in 'Engine.Scripting.Lua.Thread.Console';
--   engine-to-Lua message dispatch lives in 'Engine.Scripting.Lua.Thread.Dispatch'.
--   Both are re-exported here so the public API is unchanged.
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
import Engine.Scripting.Lua.Script (callModuleFunction, loadModuleRef)
import Engine.Scripting.Lua.Util (isValidRef, nowSeconds)
import Engine.Scripting.Lua.DebugServer (DebugCommand(..), startDebugServer, pollDebugCommand)
import Engine.Scripting.Lua.Thread.Console (processDebugCommands, debugBuiltin)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg, processLuaMsgs)
import Engine.Asset.Types (AssetPool)
import Engine.Core.Log (logWarn, logDebug, logInfo, LogCategory(..), LoggerState)
import Engine.Core.Thread
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (EngineConfig(..))
import Engine.Save.Barrier (captureLocked)
import Engine.Input.Types (InputState)
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryPutMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch, finally)

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
            registerLuaAPI (lbsLuaState backendState) env backendState stateRef
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

                    atomically $ modifyTVar' (lbsScripts backendState) $
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
                -- Round 5 review (issue #763): the Lua thread is the
                -- one thread the save barrier never actually gated --
                -- SaveLua's own self-ack (in saveWorldFn/handleLoadStaged)
                -- persists across every later quiescence pass by
                -- design (Engine.Save.Barrier.acknowledgeSave's special
                -- casing), so this loop never needed a per-tick
                -- acknowledgeCurrent the way Unit/Combat/Simulation/
                -- Input do -- but that also meant nothing stopped THIS
                -- loop's own NEXT tick from processing debug commands,
                -- queued Lua messages, or script updates while
                -- captureLocked was still True. Concretely: once
                -- handleLoadStaged (dispatched from a PRIOR tick's
                -- message processing, below) applies the required Lua
                -- components and queues WorldLoadPublish, THIS tick
                -- completes and the loop recurses -- and the very next
                -- tick would resume normal processing even though the
                -- world thread hasn't swapped the Haskell-side session
                -- yet, letting a debug command or script observe the
                -- new Lua singletons against the still-old Haskell
                -- state. Checked fresh every tick (never cached), so
                -- the SAME tick that dispatches LuaLoadStaged always
                -- starts unlocked (the transaction only reaches
                -- SaveSnapshotBoundary partway through
                -- handleLoadStaged, by which point this tick has
                -- already passed the gate) and normal processing
                -- resumes on the first tick after the world thread
                -- calls releaseCaptureLock.
                locked ← captureLocked (saveBarrierRef env)
                if locked
                  then threadDelay 1000 >> pure True
                  else do
                    -- Round 8 review: releaseCaptureLock (world thread,
                    -- right after publishStagedSession) flips
                    -- captureLocked False the INSTANT publish
                    -- completes -- but LuaSaveLoaded was already queued
                    -- onto luaQueue by publishStagedSession itself,
                    -- strictly BEFORE that release. Processing debug
                    -- commands first (as this branch used to, unconditionally)
                    -- let an ALREADY-queued debug command run against
                    -- the freshly-published session before the required
                    -- onSaveLoaded reconciliation (off-page-survivor
                    -- pruning, stale nested-reference scrub, UI reset)
                    -- ever got a chance to. Draining whatever's already
                    -- in luaQueue first closes that ordering gap without
                    -- disturbing the blocking-wait-based sleep below,
                    -- which only ever blocks on genuinely NEW messages;
                    -- nothing here double-processes since each queue
                    -- read removes what it reads.
                    processLuaMsgs env ls stateRef
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
                              atomically $ modifyTVar' (lbsScripts ls) $
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
