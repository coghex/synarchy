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
  , createLuaBackendState
  , processLuaMsg
  , processLuaMsgs
  , runDueScripts
  ) where

import UPrelude
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Shell (setupShellSandbox)
import Engine.Scripting.Lua.Script (callModuleFunction, loadModuleRef)
import Engine.Scripting.Lua.Util (isValidRef, nowSeconds)
import Engine.Scripting.Lua.TickPolicy
    (schedulerSleepMicros, scriptIsDue, advanceTick)
import Engine.Scripting.Lua.DebugServer
    ( DebugCommand(..), DebugConsole(..), DebugServerConfig(..)
    , defaultDebugServerConfig, startDebugServer, stopDebugConsole
    , inertDebugConsole, pollDebugCommand
    , DebugListenerFailure(..), ListenerAction(..), listenerAction
    , reportDebugListenerFailure, handleDebugListenerLoss
    , reportBootCleanup )
import Engine.Scripting.Lua.Thread.Console (processDebugCommands, debugBuiltin)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg, processLuaMsgs)
import Engine.Asset.Types (AssetPool)
import Engine.Core.Log (logWarn, logDebug, logInfo, LogCategory(..), LoggerState)
import Engine.Core.Thread
import Engine.Core.State
    (EngineEnv(..), EngineLifecycle(..), requestEngineCleanup)
import Engine.Core.Types (EngineConfig(..))
import Engine.Save.Barrier (captureLocked)
import Engine.Input.Types (InputState)
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)

-- | Start the Lua scripting thread, or refuse to when the boot mode
--   requires a debug console it cannot have (#1190).
--
--   'Left' is reachable only for a 'ConsoleRequired' mode
--   (@--headless@, @--offscreen@); the other three tolerate a dead
--   listener exactly as before and always get a 'Right'. By the time a
--   'Left' is returned the cause has been reported on stderr and the
--   Lua state allocated for the thread has been closed — no scripting
--   thread was forked, so there is nothing for the caller to stop. What
--   the caller still owns is whatever workers it had started BEFORE
--   this call; @App.Boot.luaThreadOrAbort@ is the shared tail that
--   stops those and exits non-zero.
startLuaThread ∷ EngineEnv → IO (Either DebugListenerFailure ThreadState)
startLuaThread env = startWorkerThreadEither WorkerSpec
    { wsName        = "Lua"
    , wsLoggerRef   = loggerRef env
    , wsCategory    = CatLua
    , wsStartingMsg = "Starting Lua scripting thread..."
      -- This worker has never logged a post-fork line.
    , wsStartedMsg  = Nothing
    , wsFailMsg     = "Lua thread failed to start: "
      -- The one worker whose start failure has always been a warning.
    , wsFailLevel   = WorkerFailWarn
    , wsFailFatal   = "Lua thread failed to start."
    , wsStartup     = luaStartup env
    , wsTick        = luaTick env
    , wsOnStop      = \lls → do
        logger ← readIORef (loggerRef env)
        logDebug logger CatLua "Lua thread stopped"
        -- #2170: stop the console FIRST, so no accept thread and no
        -- client handler outlives this worker. Every admitted client is
        -- closed, killed and joined here, before the Lua state below is
        -- freed out from under a thread that might still be holding a
        -- command against it.
        stopDebugConsole (llsConsole lls)
        -- Answer any debug commands still queued at teardown so their
        -- client threads (and netcat connections) don't sit out the full
        -- 30 s response timeout while the engine shuts down. Mirrors the
        -- crash handler's drain.
        drainDebugQueue (llsDebugQueue lls) "engine shutting down"
        Lua.close (lbsLuaState (llsBackend lls))
    , wsOnCrash     = \lls e → do
        logger ← readIORef (loggerRef env)
        logWarn logger CatLua $ "Lua thread crashed: " <> tshow e
        -- Same ordering as the clean stop above (#2170).
        stopDebugConsole (llsConsole lls)
        -- Drain pending debug commands so clients don't hang
        drainDebugQueue (llsDebugQueue lls) $
            "ERROR: Lua thread crashed: " <> tshow e
        Lua.close (lbsLuaState (llsBackend lls))
        writeIORef (lifecycleRef env) CleaningUp
    }

-- | What the Lua startup hands to every later tick: the backend state
--   with the REAL debug queue spliced in, the control ref
--   'processLuaMsg'/'processLuaMsgs' need, and the debug queue itself.
data LuaLoopState = LuaLoopState
    { llsBackend    ∷ LuaBackendState
    , llsControlRef ∷ IORef ThreadControl
    , llsDebugQueue ∷ TQueue DebugCommand
    , llsConsole    ∷ DebugConsole
      -- ^ The console this worker owns (#2170), so the teardown paths
      --   above can stop its accept loop and its clients. Inert — no
      --   listener, a queue nothing feeds — for the port-0 sentinel and
      --   for a console-optional mode whose bind failed.
    }

-- | Answer and discard every queued debug command with one reply.
drainDebugQueue ∷ TQueue DebugCommand → T.Text → IO ()
drainDebugQueue debugQueue reply = go
  where
    go = do
        mCmd ← pollDebugCommand debugQueue
        case mCmd of
            Nothing → pure ()
            Just (DebugCommand _ mvar) → do
                _ ← tryPutMVar mvar reply
                go

-- | The Lua thread's startup: create the backend state, register the
--   API, sandbox the shell, load and run @scripts\/init.lua@, and bind
--   the debug listener. 'Left' means the boot mode REQUIRED a console it
--   could not have (#1190) — the cause is already reported and the Lua
--   state already closed, and the shared lifecycle forks nothing.
luaStartup ∷ EngineEnv → IORef ThreadControl
           → IO (Either DebugListenerFailure LuaLoopState)
luaStartup env stateRef = do
    let apRef     = assetPoolRef env
        objIdRef  = nextObjectIdRef env
        inputSRef = inputStateRef env
    logger ← readIORef (loggerRef env)
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
                           <> tshow initScriptId

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

    let mode = ecBootMode (engineConfig env)
        port = ecDebugPort (engineConfig env)
        -- Production defaults for every bound, with only the terminal
        -- loss hook overridden: #2170 requires the loss to be reported
        -- on stderr in every mode and to STOP the engine in a
        -- console-required one, and the mode is knowledge this layer
        -- has and 'startDebugServer' deliberately does not.
        serverConfig = (defaultDebugServerConfig port (debugBuiltin env))
            { dscOnLoss = handleDebugListenerLoss mode port $
                void (requestEngineCleanup (lifecycleRef env))
            }
        -- Shared by both branches that actually touch a socket.
        attemptBind = startDebugServer serverConfig
        listening c = do
            logInfo logger CatLua $
                "Debug server listening on port " <> tshow port
            return (Right c)
    -- #1190: whether a dead listener is survivable is a per-MODE
    -- decision, made here (with the mode in hand) rather than
    -- inside 'startDebugServer', which sees only a number and so
    -- cannot tell --dump's deliberate port-0 sentinel from the
    -- same 0 reaching a mode whose only control surface it is.
    eDebugConsole ← case listenerAction mode port of
        TolerateListener → attemptBind ⌦ \case
            Right c  → listening c
            Left err → do
                -- Engine keeps running without a console; the
                -- queue is inert (nothing ever feeds it).
                logWarn logger CatLua $
                    "Debug server failed to start on port "
                    <> tshow port <> ": " <> err
                Right ⊚ inertDebugConsole
        RequireListener → attemptBind ⌦ \case
            Right c  → listening c
            Left err → return (Left (ListenerBindFailed err))
        -- No socket is touched at all, so no READY marker is
        -- emitted on either handle.
        RejectPortZero → return (Left ListenerPortZero)
    case eDebugConsole of
        Left failure → do
            reportDebugListenerFailure mode port failure
            -- The Lua state was allocated (and the API
            -- registered, and scripts/init.lua run) before the
            -- listener was ever attempted, so it is live here
            -- and nothing else will ever close it: no loop was
            -- forked, so @luaTick@'s teardown close can
            -- never run.
            Lua.close (lbsLuaState backendState)
            reportBootCleanup
                "closed the Lua state (no scripting thread was started)"
            return (Left failure)
        Right console →
            -- Issue #763: the real debug queue only
            -- exists once 'startDebugServer' above returns, but
            -- 'backendState' was constructed earlier (so registerLuaAPI/
            -- script init could run against it) with the throwaway
            -- placeholder 'createLuaBackendState' makes internally.
            -- Splice the real queue in now via record update so
            -- 'Engine.Scripting.Lua.Thread.Dispatch's 'LuaSaveLoaded'
            -- handler can reach it as 'lbsDebugQueue' — cheaper than
            -- threading 'debugQueue' through 'createLuaBackendState'
            -- and its dozen-plus test call sites, none of which
            -- exercise real debug-command handling.
            return $ Right LuaLoopState
                { llsBackend    = backendState
                    { lbsDebugQueue = consoleQueue console }
                , llsControlRef = stateRef
                , llsDebugQueue = consoleQueue console
                , llsConsole    = console
                }

createLuaBackendState ∷ Q.Queue LuaToEngineMsg → Q.Queue LuaMsg
                      → IORef AssetPool → IORef Word32
                      → IORef InputState → IORef LoggerState → IO LuaBackendState
createLuaBackendState ltem etlm apRef objIdRef inputSRef loggerR = do
  lState ← Lua.newstate
  -- This is where gameplay's random stream gets its entropy (#1330).
  -- 'openlibs' runs 'luaopen_math', which seeds the state's one
  -- 'math.random' stream from the clock AND the state's own address, and
  -- it happens here — before 'scripts/init.lua' is ever loaded, so every
  -- consumer sees an already-seeded stream. Nothing in 'scripts/' may
  -- call 'math.randomseed': doing so replaces per-state entropy with
  -- whatever that caller chose, and two engines launched in the same
  -- second then share one simulation. A UI widget wanting its own
  -- draws keeps its own stream ('scripts/ui/random.lua').
  _ ← Lua.runWith lState $ Lua.openlibs
  scriptsVar ← newTVarIO Map.empty
  scriptIdRef ← newIORef 1
  -- Placeholder — 'startLuaThread' splices in the REAL debug queue via
  -- record update once one exists (issue #763); every
  -- other caller (headless tests exercising unrelated Lua API surface)
  -- never touches 'lbsDebugQueue' at all, so an inert, never-fed queue
  -- here keeps their call sites unchanged.
  placeholderDebugQueue ← atomically newTQueue
  languageCacheRef ← newIORef Nothing
  return LuaBackendState
    { lbsLuaState     = lState
    , lbsScripts      = scriptsVar
    , lbsNextScriptId = scriptIdRef
    , lbsMsgQueues    = (ltem, etlm)
    , lbsAssetPool    = apRef
    , lbsNextObjectId = objIdRef
    , lbsInputState   = inputSRef
    , lbsLoggerRef    = loggerR
    , lbsLanguageCache = languageCacheRef
    , lbsDebugQueue   = placeholderDebugQueue
    }

-- | One running tick of the Lua thread. The shared lifecycle
--   ('Engine.Core.Thread.workerLoop') owns the control-state dispatch,
--   the paused poll, and the per-tick catch boundary around this.
luaTick ∷ EngineEnv → LuaLoopState → IO (Maybe LuaLoopState)
luaTick env lls = do
    let ls         = llsBackend lls
        stateRef   = llsControlRef lls
        debugQueue = llsDebugQueue lls
    -- Issue #763: the Lua thread is the
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
      then threadDelay 1000 >> pure (Just lls)
      else do
        -- releaseCaptureLock (world thread,
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

        -- Issue #763: 'processLuaMsgs' just
        -- above can itself dispatch 'LuaLoadStaged' —
        -- 'handleLoadStaged' applies the prepared Lua state
        -- (unit_ai/building_spawn singletons overwritten with
        -- the NEW session's data) and enters the capture lock
        -- (beginSave) SYNCHRONOUSLY, all inside this same call,
        -- before 'WorldLoadPublish' is ever queued for the
        -- world thread. The 'locked' value read at the top of
        -- this tick is now stale: continuing on to
        -- 'processDebugCommands'/script updates/the blocking
        -- queue read below with THAT stale value would let an
        -- already-queued debug command or a script's own
        -- "update" callback run against the freshly-applied
        -- Lua singletons while Haskell still exposes the OLD
        -- session (WorldLoadPublish hasn't been processed by
        -- the world thread yet) — exactly the mixed-state
        -- window the ORIGINAL 'locked' check exists to keep
        -- shut. Re-checking here and skipping the rest of
        -- THIS tick's unlocked work the instant it flips
        -- closes that window without needing to wait for the
        -- next tick's own (correctly gated) iteration.
        lockedAfterMsgs ← captureLocked (saveBarrierRef env)
        if lockedAfterMsgs
          then pure (Just lls)
          else do
            processDebugCommands (lbsLuaState ls) debugQueue

            currentSecs ← nowSeconds
            scriptsMap ← readTVarIO (lbsScripts ls)
            -- Sleep only as long as the next TIMED script allows.
            -- Paused scripts never advance their nextTick and
            -- event-only scripts (interval 0, #1695) never tick at
            -- all; including either would pin the sleep at the floor
            -- and busy-spin the loop at ~1 kHz. Both exclusions, the
            -- floor, the ~60 Hz cap and the overflow-safe microsecond
            -- conversion live in "Engine.Scripting.Lua.TickPolicy".
            let sleepMicros = schedulerSleepMicros currentSecs
                                                   (Map.elems scriptsMap)
                (_, etlq) = lbsMsgQueues ls
            -- readQueueTimeout, NOT System.Timeout around readQueue:
            -- the timeout exception can land after the STM dequeue
            -- commits, silently dropping the message.
            mMsg ← Q.readQueueTimeout sleepMicros etlq
            case mMsg of
              Just msg → do
                  processLuaMsg env ls stateRef msg
                  processLuaMsgs env ls stateRef
                  pure (Just lls)
              Nothing → do
                  currentSecs' ← nowSeconds
                  runDueScripts ls currentSecs'
                  pure (Just lls)

-- | One scheduler pass over the loaded scripts: reschedule every script
--   that is DUE, then call @update@ on each of them.
--
--   Which scripts those are is 'scriptIsDue''s answer and nothing else,
--   so a paused or event-only script (interval @0@, #1695) is skipped
--   here for exactly the reason it was skipped when the sleep above was
--   computed — the two can't drift. The @dt@ handed to @update@ is the
--   script's own accepted interval, unchanged.
--
--   __The reentrancy rule (#2205).__ The rescheduling happens FIRST, in
--   one transaction, BEFORE any callback of the pass runs — not after
--   each @update@ returns, which is what used to make a callback's own
--   scheduling decision get advanced a second time on top. Because the
--   scheduler never writes a deadline again once a callback has started,
--   a callback that reschedules — @engine.setTickInterval@,
--   @engine.pauseScript@ or @engine.resumeScript@, on ITSELF or on ANY
--   OTHER script, whether or not that other script has already had its
--   turn this pass — is always the last writer, and the scheduler
--   neither overwrites its decision nor adds an interval to it. When
--   several successful calls target one script, the last one wins.
--
--   So after a pass every script's stored schedule is exactly one of:
--
--   * 'advanceTick' applied to the deadline and rate the pass found it
--     with, when no callback touched it — which is also what a REFUSED
--     'Engine.Scripting.Lua.API.Core.setTickIntervalFn' leaves standing,
--     since #1695 makes a refusal store nothing at all; or
--   * whatever the last successful scheduling call of the pass stored —
--     including @engine.pauseScript@ deliberately leaving the rate and
--     the deadline exactly where they were and flipping only the pause
--     flag.
--
--   The pass SNAPSHOT is unchanged by any of this: eligibility,
--   iteration order and each @dt@ all come from the map as it was read,
--   so rescheduling a script mid-pass never adds, cancels or retimes a
--   callback this pass was already going to make — it only decides the
--   target's stored schedule afterwards. @engine.killScript@ deletes the
--   entry and no later write puts it back; @engine.loadScript@ inserts
--   one that cannot be in the already-captured snapshot, so it first
--   becomes due on a later pass.
--
--   Exported so "Test.Headless.Lua.TickInterval" can drive the real
--   pass against a bare backend rather than reproducing it.
runDueScripts ∷ LuaBackendState → Double → IO ()
runDueScripts ls now = do
    scriptsMap ← readTVarIO (lbsScripts ls)
    let due = filter (scriptIsDue now ∘ snd) (Map.toList scriptsMap)
    unless (null due) $
      atomically $ modifyTVar' (lbsScripts ls) $ \m →
        foldr (Map.adjust (advanceTick now) ∘ fst) m due
    forM_ due $ \(_, script) →
      when (isValidRef (scriptModuleRef script)) $
        void $ callModuleFunction ls (scriptModuleRef script) "update"
                   [ScriptNumber (scriptTickRate script)]
