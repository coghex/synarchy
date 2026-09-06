-- | The Lua worker's scheduling fairness contract (#2415).
--
--   The worker used to drain engine messages to emptiness, then console
--   commands to emptiness, and reach 'runDueScripts' only from the
--   TIMEOUT branch of the blocking queue read that followed. Traffic
--   that never let that read time out could therefore deny a due
--   @update@ callback service indefinitely.
--
--   Every example here drives the PRODUCTION round —
--   'Engine.Scripting.Lua.Thread.Scheduler.schedulerRound', the whole of
--   what @luaTick@ now does — against a real 'LuaBackendState' with the
--   real Lua API registered, real queues, the real save barrier and the
--   real load-status ref. The only things replaced are the three things
--   a spec cannot drive deterministically in place: the monotonic clock,
--   the idle wait, and the park delay, all injected through
--   'SchedulerSeam'. Nothing here reimplements the loop.
--
--   Time is advanced by hand and replenishment is finite, so no example
--   depends on how fast the machine runs.
module Test.Headless.Lua.SchedulerFairness (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (tryTakeMVar)
import Control.Exception (finally)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Concurrent.STM.TQueue
    (TQueue, newTQueue, tryReadTQueue, writeTQueue)
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Load.Status
    ( LoadOutcome(..), LoadPhase(..), LoadStatus(..)
    , advanceLoad, beginLoad, failLoad, readLoadStatus )
import Engine.Save.Barrier
    ( SaveOwner(..), SaveStatus(..), acknowledgeSave, beginSave, failSave
    , ownerGated, readSaveStatus, saveInProgress )
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.DebugServer
    ( DebugCommand(..), DebugCommandState(..), cancelDebugCommand
    , newDebugCommand, readDebugCommandState )
import Engine.Scripting.Lua.Thread (createLuaBackendState, processLuaMsgs)
import Engine.Scripting.Lua.Thread.Console
    (processDebugCommands, processDebugCommandsBounded)
import Engine.Scripting.Lua.Thread.Scheduler
    ( RoundResult(..), SchedulerContext(..), SchedulerLimits(..)
    , SchedulerSeam(..), consoleCommandBudget, defaultSchedulerLimits
    , engineMessageBudget, processLuaMsgsBounded, schedulerRound )
import Engine.Scripting.Lua.TickPolicy (maxSleepMicros)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..), LuaScript(..))
import World.Save.Payload (emptyLoadReconcileContext)

-- | The scheduler under test plus everything a spec needs to steer and
--   observe it. The backend, its queues and the round are production;
--   the clock, the wait and the park are the injected seam.
data Rig = Rig
    { rigBackend   ∷ LuaBackendState
    , rigCtx       ∷ SchedulerContext
    , rigClock     ∷ IORef Double
      -- ^ Read by every 'seamNow'. Only 'advanceClock' moves it.
    , rigWaits     ∷ IORef [Int]
      -- ^ Every microsecond bound the round handed the idle wait, in
      --   order — so an example can assert the round took the bound
      --   'schedulerSleepMicros' computed rather than one it invented.
    , rigWaitImpl  ∷ IORef (Int → IO (Maybe LuaMsg))
      -- ^ What the wait then does. The default returns immediately with
      --   no message, which is a timeout with the sleep removed; the
      --   idle example swaps in the REAL 'Q.readQueueTimeout'.
    , rigParks     ∷ IORef Int
      -- ^ How many times the round took the parked path's delay.
    , rigControl   ∷ IORef ThreadControl
    }

-- | A rig on its own private queues: its own engine-to-Lua queue rather
--   than the shared 'EngineEnv' one, and its own debug queue, so no
--   example can leak a message or a command into another spec's engine.
newRig ∷ EngineEnv → IO Rig
newRig env = do
    lteq  ← Q.newQueue
    etlq  ← Q.newQueue
    ls0   ← createLuaBackendState lteq etlq (assetPoolRef env)
                                  (nextObjectIdRef env) (inputStateRef env)
                                  (loggerRef env)
    debugQueue ← atomically newTQueue
    let ls = ls0 { lbsDebugQueue = debugQueue }
    control ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls control
    clock    ← newIORef 0
    waits    ← newIORef []
    waitImpl ← newIORef (\_ → pure Nothing)
    parks    ← newIORef 0
    carry    ← newIORef Nothing
    let seam = SchedulerSeam
          { seamNow  = readIORef clock
          , seamWait = \micros → do
                atomicModifyIORef' waits (\ws → (ws ⧺ [micros], ()))
                impl ← readIORef waitImpl
                impl micros
          , seamPark = atomicModifyIORef' parks (\n → (n + 1, ()))
          }
    pure Rig
        { rigBackend  = ls
        , rigCtx      = SchedulerContext
            { scEnv        = env
            , scBackend    = ls
            , scControlRef = control
            , scDebugQueue = debugQueue
            , scSeam       = seam
            , scLimits     = defaultSchedulerLimits
            , scCarryRef   = carry
            }
        , rigClock    = clock
        , rigWaits    = waits
        , rigWaitImpl = waitImpl
        , rigParks    = parks
        , rigControl  = control
        }

round1 ∷ Rig → IO RoundResult
round1 rig = schedulerRound (rigCtx rig)

advanceClock ∷ Rig → Double → IO ()
advanceClock rig dt = atomicModifyIORef' (rigClock rig) (\t → (t + dt, ()))

engineQueue ∷ Rig → Q.Queue LuaMsg
engineQueue rig = snd (lbsMsgQueues (rigBackend rig))

queueMsg ∷ Rig → Text → IO ()
queueMsg rig text = Q.writeQueue (engineQueue rig) (LuaWorldGenLog text)

queueMsgs ∷ Rig → [Text] → IO ()
queueMsgs rig = mapM_ (queueMsg rig)

queuedMessages ∷ Rig → IO Int
queuedMessages rig = Q.qsDepth ⊚ Q.queueStats (engineQueue rig)

debugQueueOf ∷ Rig → TQueue DebugCommand
debugQueueOf = lbsDebugQueue ∘ rigBackend

queueCommand ∷ Rig → Text → IO DebugCommand
queueCommand rig cmdText = do
    cmd ← newDebugCommand cmdText
    atomically $ writeTQueue (debugQueueOf rig) cmd
    pure cmd

-- | Everything still queued on the console side, removed so the count
--   is unambiguous. Only ever called at the end of an example.
drainQueuedCommands ∷ Rig → IO [DebugCommand]
drainQueuedCommands rig = go []
  where
    go acc = do
        mCmd ← atomically (tryReadTQueue (debugQueueOf rig))
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | Register one real Lua module against the rig, its module table
--   produced by evaluating @chunk@ exactly as a loaded script file's
--   would be. @nextTick@ decides whether the scheduler finds it due.
registerModule ∷ Rig → Word32 → FilePath → Double → Double → BS.ByteString
               → IO ()
registerModule rig sid path rate nextTick chunk = do
    ref ← Lua.runWith (lbsLuaState (rigBackend rig)) $ do
        status ← Lua.dostring chunk ∷ Lua.LuaE Lua.Exception Lua.Status
        case status of
            Lua.OK → Lua.ref Lua.registryindex
            _      → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                error $ "fixture chunk failed to load: " ⧺ path
                      ⧺ " -- " ⧺ maybe "?" BS.unpack err
    atomically $ modifyTVar' (lbsScripts (rigBackend rig)) $ Map.insert sid
        LuaScript
            { scriptId        = sid
            , scriptPath      = path
            , scriptTickRate  = rate
            , scriptNextTick  = nextTick
            , scriptModuleRef = ref
            , scriptPaused    = False
            }

-- | The witness module every fairness example uses: it records the
--   order in which the dispatcher broadcast messages to it and the
--   order in which the scheduler called its @update@, and calls
--   @__hook()@ once per message when the test installed one.
witnessChunk ∷ BS.ByteString
witnessChunk =
    "__msgs = {} __ticks = {} __cmds = {} \
    \return { onWorldGenLog = function(text) \
    \           __msgs[#__msgs+1] = text \
    \           if __hook then __hook() end \
    \         end, \
    \         update = function(dt) __ticks[#__ticks+1] = dt end }"

-- | A Haskell action reachable from Lua as a global, so a real handler
--   or a real console command can be what changes the world mid-round.
installHook ∷ Rig → BS.ByteString → IO () → IO ()
installHook rig name act = Lua.runWith (lbsLuaState (rigBackend rig)) $ do
    Lua.pushHaskellFunction
        (Lua.liftIO act ≫ pure 0 ∷ Lua.LuaE Lua.Exception Lua.NumResults)
    Lua.setglobal (Lua.Name name)

-- | Evaluate a Lua expression and read its value back as text.
evalLua ∷ Rig → BS.ByteString → IO Text
evalLua rig expr = Lua.runWith (lbsLuaState (rigBackend rig)) $ do
    status ← Lua.dostring ("return " <> expr) ∷ Lua.LuaE Lua.Exception Lua.Status
    value ← Lua.tostring (-1)
    Lua.pop 1
    pure $ case status of
        Lua.OK → maybe "<nil>" TE.decodeUtf8Lenient value
        _      → "<error: " <> maybe "?" TE.decodeUtf8Lenient value <> ">"

-- | The comma-joined contents of a Lua array global, @\"\"@ when empty.
joined ∷ Rig → BS.ByteString → IO Text
joined rig name = evalLua rig ("table.concat(" <> name <> ", ',')")

-- | How many entries a Lua array global holds.
counted ∷ Rig → BS.ByteString → IO Text
counted rig name = evalLua rig ("tostring(#" <> name <> ")")

-- | Close the 'SaveLua' owner gate through the real barrier and answer
--   with the request id that reopens it. Asserts the gate really shut,
--   so a setup that stopped working cannot silently pass an example
--   about parking.
--
--   The barrier belongs to the shared @aroundAll@ engine, so an earlier
--   example anywhere in that group can have left a transaction standing.
--   That is cleared first rather than reported: what these examples are
--   about is what the ROUND does with a shut gate, and refusing to shut
--   one would only turn somebody else's leak into a failure here.
closeOwnerGate ∷ EngineEnv → IO Int
closeOwnerGate env = do
    stale ← saveInProgress (saveBarrierRef env)
    when stale $ do
        current ← readSaveStatus (saveBarrierRef env)
        forM_ current $ \s →
            failSave (saveBarrierRef env) (ssRequestId s) "spec setup"
    started ← beginSave (saveBarrierRef env) (Set.fromList [SaveLua])
    case started of
        Left err → do
            expectationFailure $ "could not begin a save: " ⧺ T.unpack err
            pure 0
        Right n → do
            -- The Lua owner's own post-final-acknowledgement park
            -- (#2221) — the thing 'ownerGated' actually answers for this
            -- worker. 'requiredQuiescencePasses' is 3, so a sole owner
            -- reaches the final pass on its second acknowledgement.
            acknowledgeSave (saveBarrierRef env) n SaveLua
            acknowledgeSave (saveBarrierRef env) n SaveLua
            ownerGated (saveBarrierRef env) SaveLua `shouldReturn` True
            pure n

reopenOwnerGate ∷ EngineEnv → Int → IO ()
reopenOwnerGate env n = do
    failSave (saveBarrierRef env) n "spec teardown"
    ownerGated (saveBarrierRef env) SaveLua `shouldReturn` False

-- | Put the real load status into the published-but-unreconciled state
--   the hold is defined over, and answer with the request id.
enterWaitingPublish ∷ EngineEnv → IO Int
enterWaitingPublish env = do
    clearStaleLoad env
    started ← beginLoad (loadStatusRef env) "spec-save"
    case started of
        Left err → do
            expectationFailure $ "could not begin a load: " ⧺ T.unpack err
            pure 0
        Right n → do
            advanceLoad (loadStatusRef env) n LoadWaitingPublish
            pure n

-- | The load status ref is shared by the whole @aroundAll@ engine too,
--   so an earlier example anywhere in that group can leave a
--   non-terminal load standing. Same reasoning as 'closeOwnerGate'.
clearStaleLoad ∷ EngineEnv → IO ()
clearStaleLoad env = do
    current ← readLoadStatus (loadStatusRef env)
    forM_ current $ \s →
        when (isNothing (lsOutcome s)) $
            failLoad (loadStatusRef env) (lsRequestId s) "spec setup"

-- | Terminalise whatever load an example started, so the shared status
--   ref is clean for the next one.
clearLoad ∷ EngineEnv → Int → IO ()
clearLoad env n = do
    failLoad (loadStatusRef env) n "spec teardown"
    mStatus ← readLoadStatus (loadStatusRef env)
    (isJust ∘ (lsOutcome =<<)) mStatus `shouldBe` True

spec ∷ SpecWith EngineEnv
spec = describe "Lua scheduler fairness (#2415)" $ do
    timerServiceSpec
    engineBatchSpec
    consoleBatchSpec
    idleWaitSpec
    parkSpec
    reconciliationSpec
    preservedContractsSpec

timerServiceSpec ∷ SpecWith EngineEnv
timerServiceSpec = describe "timers are served independently of queue emptiness" $ do
    it "runs a due script after repeated message wakeups that never let \
       \the queue read time out" $ \env → do
        rig ← newRig env
        -- Due at t=5, so several rounds pass before the deadline and the
        -- example cannot pass by running the script immediately.
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 5.0 witnessChunk
        -- The wait ALWAYS answers with a message: this is the traffic
        -- pattern the old loop could not survive, since only its
        -- timeout branch ever reached 'runDueScripts'.
        writeIORef (rigWaitImpl rig) (\_ → pure (Just (LuaWorldGenLog "wake")))

        results ← forM [1 .. 8 ∷ Int] $ \_ → do
            advanceClock rig 1.0
            round1 rig

        -- Not one round timed out, so the OLD scheduler would have made
        -- no pass at all here.
        map rrWoke results `shouldBe` replicate 8 True
        all (isJust ∘ rrWaited) results `shouldBe` True
        sum (map rrDuePasses results) `shouldSatisfy` (≥ 1)
        counted rig "__ticks" `shouldNotReturn` "0"

    it "makes at most one due pass per round, so continuously due work \
       \cannot deny the message and console opportunities" $ \env → do
        rig ← newRig env
        -- A 1 ms interval against a clock that jumps a whole second per
        -- round: this script is due on every single round.
        registerModule rig 1 "scripts/fairness_witness.lua" 0.001 0.0 witnessChunk

        -- Both ordinary queues are topped up past their budgets before
        -- every round, so the timer competes with work that never runs
        -- out — the overload shape, with the timing removed.
        results ← forM [1 .. 3 ∷ Int] $ \k → do
            queueMsgs rig [T.pack ("r" ⧺ show k ⧺ "m" ⧺ show i)
                          | i ← [1 .. 40 ∷ Int]]
            forM_ [1 .. 12 ∷ Int] $ \i →
                void $ queueCommand rig
                    (T.pack ("__cmds[#__cmds+1] = 'r" ⧺ show k
                             ⧺ "c" ⧺ show i ⧺ "'"))
            advanceClock rig 1.0
            round1 rig

        -- One pass per round: never a burst of passes ahead of the other
        -- classes, and never a round that skipped the due script.
        map rrDuePasses results `shouldBe` [1, 1, 1]
        counted rig "__ticks" `shouldReturn` "3"
        -- And every round spent its full message and console budget on
        -- the backlog rather than the timer eating the round.
        map rrMessages results `shouldBe` replicate 3 engineMessageBudget
        map rrConsole results `shouldBe` replicate 3 consoleCommandBudget
        -- Neither queue ran dry, so none of that was for want of work.
        queuedMessages rig `shouldReturn` (3 * 40 - 3 * engineMessageBudget)
        (length ⊚ drainQueuedCommands rig)
            `shouldReturn` (3 * 12 - 3 * consoleCommandBudget)

engineBatchSpec ∷ SpecWith EngineEnv
engineBatchSpec = describe "the engine-message batch is bounded" $ do
    it "ships the named initial limits the contract asks for" $ \_ → do
        -- Every other example is written against these names rather than
        -- against 32 and 8, so this is the one place a silent change to
        -- the shipped defaults becomes visible.
        engineMessageBudget `shouldBe` 32
        consoleCommandBudget `shouldBe` 8
        slEngineMessages defaultSchedulerLimits `shouldBe` engineMessageBudget
        slConsoleEntries defaultSchedulerLimits `shouldBe` consoleCommandBudget

    it "dispatches exactly the budget, in order, once each, and leaves \
       \the rest queued while timers and console work still get a turn" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        let sent = [T.pack ('m' : show i) | i ← [1 .. 40 ∷ Int]]
        queueMsgs rig sent
        cmd ← queueCommand rig "__cmds[#__cmds+1] = 'console'"

        r ← round1 rig

        rrMessages r `shouldBe` engineMessageBudget
        queuedMessages rig `shouldReturn` (40 - engineMessageBudget)
        -- FIFO and exactly once: the first 32 sent, in order, no repeats.
        joined rig "__msgs"
            `shouldReturn` T.intercalate "," (take engineMessageBudget sent)
        -- The point of the bound: a due script and a console command are
        -- served in the SAME round the queue stayed nonempty through.
        rrDuePasses r `shouldBe` 1
        counted rig "__ticks" `shouldReturn` "1"
        rrConsole r `shouldBe` 1
        joined rig "__cmds" `shouldReturn` "console"
        readDebugCommandState cmd `shouldReturn` DebugCommandClaimed
        -- Queued work remains, so the round repeats rather than sleeping.
        rrWaited r `shouldBe` Nothing

    it "still bounds the batch when the handlers themselves keep \
       \replenishing the queue" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        -- A real handler-driven refill: every dispatched message runs
        -- the module's callback, which puts another message back. The
        -- budget is finite so the example terminates.
        refills ← newIORef (20 ∷ Int)
        installHook rig "__hook" $ do
            left ← atomicModifyIORef' refills (\n → (max 0 (n - 1), n))
            when (left > 0) $ Q.writeQueue (engineQueue rig)
                                           (LuaWorldGenLog "refill")
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 40 ∷ Int]]

        r ← round1 rig

        rrMessages r `shouldBe` engineMessageBudget
        -- 40 queued, 32 taken, 20 put back by the handlers.
        queuedMessages rig `shouldReturn` (40 - engineMessageBudget + 20)
        rrDuePasses r `shouldBe` 1
        counted rig "__ticks" `shouldReturn` "1"

    it "drains the leftovers on the next rounds without re-dispatching \
       \anything" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        let sent = [T.pack ('m' : show i) | i ← [1 .. 40 ∷ Int]]
        queueMsgs rig sent

        r1 ← round1 rig
        r2 ← round1 rig

        rrMessages r1 `shouldBe` engineMessageBudget
        rrMessages r2 `shouldBe` (40 - engineMessageBudget)
        queuedMessages rig `shouldReturn` 0
        joined rig "__msgs" `shouldReturn` T.intercalate "," sent

consoleBatchSpec ∷ SpecWith EngineEnv
consoleBatchSpec = describe "the console batch is bounded" $ do
    it "charges cancelled entries against the budget, keeps FIFO among \
       \the rest, and leaves the remainder queued" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 40 ∷ Int]]
        cmds ← forM [1 .. 12 ∷ Int] $ \i →
            queueCommand rig (T.pack ("__cmds[#__cmds+1] = 'c" ⧺ show i ⧺ "'"))
        -- Two entries the drain will dequeue but must not run.
        void $ cancelDebugCommand (cmds !! 1) "CANCELLED: for the test"
        void $ cancelDebugCommand (cmds !! 4) "CANCELLED: for the test"

        r ← round1 rig

        -- Eight LOOKS, not eight executions: the two cancelled entries
        -- cost their slots exactly as the executed ones do.
        rrConsole r `shouldBe` consoleCommandBudget
        joined rig "__cmds" `shouldReturn` "c1,c3,c4,c6,c7,c8"
        readDebugCommandState (cmds !! 1)
            `shouldReturn` DebugCommandCancelled "CANCELLED: for the test"
        readDebugCommandState (cmds !! 0) `shouldReturn` DebugCommandClaimed
        -- The other four are untouched and still queued, in order.
        leftovers ← drainQueuedCommands rig
        map dcCommand leftovers `shouldBe` ["__cmds[#__cmds+1] = 'c9'"
                                           ,"__cmds[#__cmds+1] = 'c10'"
                                           ,"__cmds[#__cmds+1] = 'c11'"
                                           ,"__cmds[#__cmds+1] = 'c12'"]
        -- Meanwhile the other two classes got their turns.
        rrMessages r `shouldBe` engineMessageBudget
        queuedMessages rig `shouldReturn` (40 - engineMessageBudget)
        rrDuePasses r `shouldBe` 1

    it "keeps making progress across rounds while the console queue is \
       \replenished past the budget" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        forM_ [1 .. 8 ∷ Int] $ \i →
            void $ queueCommand rig (T.pack ("__cmds[#__cmds+1] = 'a" ⧺ show i ⧺ "'"))

        r1 ← round1 rig
        forM_ [1 .. 6 ∷ Int] $ \i →
            void $ queueCommand rig (T.pack ("__cmds[#__cmds+1] = 'b" ⧺ show i ⧺ "'"))
        r2 ← round1 rig

        rrConsole r1 `shouldBe` 8
        rrConsole r2 `shouldBe` 6
        joined rig "__cmds"
            `shouldReturn` "a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6"

idleWaitSpec ∷ SpecWith EngineEnv
idleWaitSpec = describe "idle waiting and wakeups" $ do
    it "blocks through the real readQueueTimeout and charges the message \
       \it woke on to the NEXT round's budget" $ \env → do
        rig ← newRig env
        -- The real primitive, on the rig's real queue. A message lands
        -- just before the read, so the round wakes rather than sleeping
        -- the machine-dependent bound out.
        writeIORef (rigWaitImpl rig) $ \micros → do
            queueMsg rig "woke"
            Q.readQueueTimeout micros (engineQueue rig)
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk

        r1 ← round1 rig

        -- Nothing was runnable, so the round waited; and with no timed
        -- script the bound is the idle cap, not the 1 ms floor.
        rrMessages r1 `shouldBe` 0
        rrWaited r1 `shouldBe` Just maxSleepMicros
        rrWoke r1 `shouldBe` True
        queuedMessages rig `shouldReturn` 0
        joined rig "__msgs" `shouldReturn` ""

        -- The carried message is dispatched FIRST next round and costs
        -- one budget slot, so only 31 more come off the queue.
        writeIORef (rigWaitImpl rig) (\_ → pure Nothing)
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 40 ∷ Int]]
        r2 ← round1 rig

        rrMessages r2 `shouldBe` engineMessageBudget
        queuedMessages rig `shouldReturn` (40 - (engineMessageBudget - 1))
        joined rig "__msgs" `shouldReturn`
            T.intercalate "," ("woke" : [T.pack ('m' : show i)
                                        | i ← [1 .. engineMessageBudget - 1]])

    it "does not busy-loop on paused or event-only scripts" $ \env → do
        rig ← newRig env
        -- Paused, and event-only (interval 0, #1695): neither is on the
        -- timer, so neither may pin the wait at the sleep floor.
        registerModule rig 1 "scripts/fairness_event.lua" 0.0 0.0 witnessChunk
        registerModule rig 2 "scripts/fairness_paused.lua" 0.001 0.0 witnessChunk
        atomically $ modifyTVar' (lbsScripts (rigBackend rig)) $
            Map.adjust (\s → s { scriptPaused = True }) 2

        r ← round1 rig

        rrDuePasses r `shouldBe` 0
        counted rig "__ticks" `shouldReturn` "0"
        rrWaited r `shouldBe` Just maxSleepMicros

    it "takes no idle wait at all while admissible work remains" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 40 ∷ Int]]

        r ← round1 rig

        rrWaited r `shouldBe` Nothing
        readIORef (rigWaits rig) `shouldReturn` []

parkSpec ∷ SpecWith EngineEnv
parkSpec = describe "the save/load owner gate and worker control" $ do
    it "parks the whole round, doing no ordinary work, while the gate is \
       \shut at entry" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        queueMsgs rig ["m1", "m2"]
        cmd ← queueCommand rig "__cmds[#__cmds+1] = 'console'"
        n ← closeOwnerGate env

        r ← round1 rig `finallyReopen` reopenOwnerGate env n

        r `shouldBe` RoundResult
            { rrMessages = 0, rrConsole = 0, rrDuePasses = 0
            , rrParked = True, rrHalted = False
            , rrWaited = Nothing, rrWoke = False }
        -- A parked round is not a busy round: it still took its delay.
        readIORef (rigParks rig) `shouldReturn` 1
        queuedMessages rig `shouldReturn` 2
        counted rig "__ticks" `shouldReturn` "0"
        readDebugCommandState cmd `shouldReturn` DebugCommandQueued

    it "abandons the rest of the round when a MESSAGE handler shuts the \
       \gate, leaving the batch's leftovers queued" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        gateRef ← newIORef Nothing
        seen ← newIORef (0 ∷ Int)
        -- The third dispatched message starts a transaction, exactly as
        -- a real 'LuaLoadStaged' handler does mid-batch.
        installHook rig "__hook" $ do
            k ← atomicModifyIORef' seen (\n → (n + 1, n + 1))
            when (k ≡ 3) $ do
                n ← closeOwnerGate env
                writeIORef gateRef (Just n)
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 12 ∷ Int]]
        void $ queueCommand rig "__cmds[#__cmds+1] = 'console'"

        r ← round1 rig
        readIORef gateRef ⌦ mapM_ (reopenOwnerGate env)

        rrMessages r `shouldBe` 3
        rrParked r `shouldBe` True
        rrDuePasses r `shouldBe` 0
        rrConsole r `shouldBe` 0
        readIORef (rigParks rig) `shouldReturn` 1
        queuedMessages rig `shouldReturn` 9
        joined rig "__msgs" `shouldReturn` "m1,m2,m3"
        counted rig "__ticks" `shouldReturn` "0"
        joined rig "__cmds" `shouldReturn` ""

    it "abandons the rest of the round when a CONSOLE command shuts the \
       \gate" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        gateRef ← newIORef Nothing
        installHook rig "__park" $ do
            n ← closeOwnerGate env
            writeIORef gateRef (Just n)
        void $ queueCommand rig "__cmds[#__cmds+1] = 'first'"
        void $ queueCommand rig "__park()"
        void $ queueCommand rig "__cmds[#__cmds+1] = 'third'"

        r ← round1 rig
        readIORef gateRef ⌦ mapM_ (reopenOwnerGate env)

        rrConsole r `shouldBe` 2
        rrParked r `shouldBe` True
        joined rig "__cmds" `shouldReturn` "first"
        -- The console batch is the round's LAST work class, so what the
        -- recheck after it prevents is the idle wait and the round's own
        -- continuation: neither may run with the gate shut.
        rrWaited r `shouldBe` Nothing
        readIORef (rigWaits rig) `shouldReturn` []
        readIORef (rigParks rig) `shouldReturn` 1
        leftovers ← drainQueuedCommands rig
        map dcCommand leftovers `shouldBe` ["__cmds[#__cmds+1] = 'third'"]

    it "stops the round the moment the worker leaves ThreadRunning, and \
       \does nothing at all once it has" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        seen ← newIORef (0 ∷ Int)
        installHook rig "__hook" $ do
            k ← atomicModifyIORef' seen (\n → (n + 1, n + 1))
            when (k ≡ 2) $ writeIORef (rigControl rig) ThreadStopped
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 10 ∷ Int]]
        void $ queueCommand rig "__cmds[#__cmds+1] = 'console'"

        r1 ← round1 rig

        rrMessages r1 `shouldBe` 2
        rrHalted r1 `shouldBe` True
        rrParked r1 `shouldBe` False
        rrDuePasses r1 `shouldBe` 0
        rrConsole r1 `shouldBe` 0
        queuedMessages rig `shouldReturn` 8

        -- A paused worker is likewise given no ordinary work; the shared
        -- worker loop owns the polling, so the round must not spin here.
        writeIORef (rigControl rig) ThreadPaused
        r2 ← round1 rig
        r2 `shouldBe` RoundResult
            { rrMessages = 0, rrConsole = 0, rrDuePasses = 0
            , rrParked = False, rrHalted = True
            , rrWaited = Nothing, rrWoke = False }
        readIORef (rigParks rig) `shouldReturn` 0
        queuedMessages rig `shouldReturn` 8

        -- And resuming lets the leftovers through, unreordered.
        writeIORef (rigControl rig) ThreadRunning
        r3 ← round1 rig
        rrMessages r3 `shouldBe` 8
        joined rig "__msgs"
            `shouldReturn` T.intercalate "," [T.pack ('m' : show i)
                                             | i ← [1 .. 10 ∷ Int]]

    it "keeps nothing buffered outside the queue for a load cutover to \
       \miss" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk

        -- First put a message in the carry slot — the one place this
        -- scheduler holds a message that is no longer on the queue.
        writeIORef (rigWaitImpl rig)
            (\_ → pure (Just (LuaWorldGenLog "carried")))
        r1 ← round1 rig
        rrWoke r1 `shouldBe` True
        writeIORef (rigWaitImpl rig) (\_ → pure Nothing)

        -- Then a handler that flushes the engine queue mid-batch, which
        -- is what 'handleLoadStaged' does on this same thread once a
        -- load's Lua apply succeeds, so that work queued for the
        -- REPLACED session cannot fire against its replacement. The
        -- flush is the real 'Q.flushQueue' call that handler makes; only
        -- the handler making it is stood in for, since standing up a
        -- whole load transaction is not what this example is about.
        dispatches ← newIORef (0 ∷ Int)
        discarded ← newIORef (0 ∷ Int)
        installHook rig "__hook" $ do
            -- The fourth dispatch is 'm3': carried, m1, m2, m3.
            k ← atomicModifyIORef' dispatches (\n → (n + 1, n + 1))
            when (k ≡ 4) $ do
                stale ← Q.flushQueue (engineQueue rig)
                writeIORef discarded (length stale)
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 10 ∷ Int]]

        r2 ← round1 rig

        -- The carried message was dispatched FIRST, ahead of anything
        -- taken off the queue, so the slot was already empty when the
        -- flush ran and nothing outlived the cutover.
        joined rig "__msgs" `shouldReturn` "carried,m1,m2,m3"
        readIORef discarded `shouldReturn` 7
        rrMessages r2 `shouldBe` 4
        queuedMessages rig `shouldReturn` 0

        -- And no later round resurrects the discarded work.
        r3 ← round1 rig
        rrMessages r3 `shouldBe` 0
        joined rig "__msgs" `shouldReturn` "carried,m1,m2,m3"

    it "re-checks the gate between individual dispatches inside the \
       \bounded drains themselves" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 10 ∷ Int]]
        calls ← newIORef (0 ∷ Int)
        gateRef ← newIORef Nothing
        -- The real predicate, with a real transaction opening under it
        -- after the fourth look.
        let admitted = do
                k ← atomicModifyIORef' calls (\n → (n + 1, n + 1))
                when (k ≡ 5) $ do
                    n ← closeOwnerGate env
                    writeIORef gateRef (Just n)
                not ⊚ ownerGated (saveBarrierRef env) SaveLua
        n ← processLuaMsgsBounded env (rigBackend rig) (rigControl rig)
                                  engineMessageBudget admitted
        readIORef gateRef ⌦ mapM_ (reopenOwnerGate env)

        n `shouldBe` 4
        queuedMessages rig `shouldReturn` 6
        joined rig "__msgs" `shouldReturn` "m1,m2,m3,m4"

reconciliationSpec ∷ SpecWith EngineEnv
reconciliationSpec = describe "the post-publication reconciliation hold" $ do
    it "keeps consuming engine messages in FIFO batches but runs no \
       \timer and no console command until LuaSaveLoaded is reached" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        requestId ← enterWaitingPublish env
        -- The reconciliation message sits BEYOND the first batch, which
        -- is the whole point: queue emptiness cannot stand in for the
        -- hold's predicate.
        queueMsgs rig [T.pack ('m' : show i) | i ← [1 .. 35 ∷ Int]]
        Q.writeQueue (engineQueue rig)
            (LuaSaveLoaded requestId [] [] emptyLoadReconcileContext)
        stale ← forM [1 .. 3 ∷ Int] $ \i →
            queueCommand rig (T.pack ("__cmds[#__cmds+1] = 's" ⧺ show i ⧺ "'"))

        r1 ← round1 rig

        rrMessages r1 `shouldBe` engineMessageBudget
        rrDuePasses r1 `shouldBe` 0
        rrConsole r1 `shouldBe` 0
        counted rig "__ticks" `shouldReturn` "0"
        joined rig "__cmds" `shouldReturn` ""
        mapM_ (\c → readDebugCommandState c `shouldReturn` DebugCommandQueued)
              stale

        r2 ← round1 rig

        -- Round two reaches LuaSaveLoaded. Its handler cancels every
        -- stale command, and only THEN do timers and the console resume.
        rrMessages r2 `shouldBe` 4
        rrDuePasses r2 `shouldBe` 1
        counted rig "__ticks" `shouldReturn` "1"
        joined rig "__cmds" `shouldReturn` ""
        -- Zero, not three: the reconciliation handler cancels the stale
        -- commands by REMOVING them from the queue itself, so the
        -- console batch that follows finds nothing left to look at. What
        -- matters is that none of them ever ran.
        rrConsole r2 `shouldBe` 0
        (map dcCommand ⊚ drainQueuedCommands rig) `shouldReturn` []
        forM_ stale $ \c → do
            state ← readDebugCommandState c
            case state of
                DebugCommandCancelled reply →
                    reply `shouldBe` "REJECTED: a load transaction \
                                     \replaced the session while this \
                                     \command was queued"
                other → expectationFailure $
                    "stale command was not cancelled: " ⧺ show other
            resp ← tryTakeMVar (dcResponse c)
            resp `shouldSatisfy` isJust
        status ← readLoadStatus (loadStatusRef env)
        (lsPhase ⊚ status) `shouldBe` Just LoadPublished
        (lsOutcome =<< status) `shouldBe` Just LoadSucceeded

    it "gives a FAILED reconciliation its own terminal disposition and \
       \then resumes ordinary work" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        registerModule rig 2 "scripts/fairness_reconcile.lua" 1.0 1e9
            "return { onSaveLoaded = function() \
            \  error('reconcile blew up') \
            \end }"
        requestId ← enterWaitingPublish env
        queueMsgs rig ["m1", "m2"]
        Q.writeQueue (engineQueue rig)
            (LuaSaveLoaded requestId [] [] emptyLoadReconcileContext)

        r ← round1 rig

        rrMessages r `shouldBe` 3
        -- The hold cleared on the terminal outcome, failed or not.
        rrDuePasses r `shouldBe` 1
        status ← readLoadStatus (loadStatusRef env)
        (lsPhase ⊚ status) `shouldBe` Just LoadReconciliationFailed
        case lsOutcome =<< status of
            Just (LoadReconciliationIncomplete detail) →
                detail `shouldSatisfy`
                    T.isInfixOf "scripts/fairness_reconcile.lua"
            other → expectationFailure $
                "expected a reconciliation-incomplete outcome, got "
                ⧺ show other
        -- Diagnostics survive: the failing module is named, not flattened
        -- away, and the console is usable again straight afterwards.
        (length ∘ maybe [] lsReconciliationFailures) status `shouldBe` 1
        void $ queueCommand rig "__cmds[#__cmds+1] = 'after'"
        r2 ← round1 rig
        rrConsole r2 `shouldBe` 1
        joined rig "__cmds" `shouldReturn` "after"

    it "falls through to the idle wait, rather than spinning, when the \
       \only work left is a timer the hold is withholding" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        requestId ← enterWaitingPublish env
        -- Nothing queued at all, and a script that IS due: exactly the
        -- state in which treating a withheld timer as "work remains"
        -- would repeat the round forever with nothing runnable.
        advanceClock rig 10.0

        r ← round1 rig `finallyReopen` clearLoad env requestId

        rrMessages r `shouldBe` 0
        rrConsole r `shouldBe` 0
        rrDuePasses r `shouldBe` 0
        counted rig "__ticks" `shouldReturn` "0"
        -- It slept on the queue. A round that came straight back here is
        -- what a 100%-CPU hold looks like; the overdue script pins the
        -- bound at the sleep floor, which is the existing policy.
        rrWaited r `shouldBe` Just 1000

    it "does not gate diagnostics across asynchronous load STAGING, \
       \which touches no live session state" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 0.0 witnessChunk
        clearStaleLoad env
        started ← beginLoad (loadStatusRef env) "spec-staging"
        requestId ← case started of
            Left err → do
                expectationFailure $ "could not begin a load: " ⧺ T.unpack err
                pure 0
            Right n → pure n
        -- A load IS in progress, but staging is not the published-and-
        -- unreconciled window, so nothing is withheld.
        advanceLoad (loadStatusRef env) requestId LoadStaged
        void $ queueCommand rig "__cmds[#__cmds+1] = 'staging'"

        r ← round1 rig `finallyReopen` clearLoad env requestId

        rrDuePasses r `shouldBe` 1
        counted rig "__ticks" `shouldReturn` "1"
        rrConsole r `shouldBe` 1
        joined rig "__cmds" `shouldReturn` "staging"

preservedContractsSpec ∷ SpecWith EngineEnv
preservedContractsSpec = describe "the unbounded drains its other callers need" $ do
    it "leaves processLuaMsgs draining to completion across more than \
       \one scheduler batch, which input settlement depends on" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        let sent = [T.pack ('m' : show i) | i ← [1 .. 100 ∷ Int]]
        queueMsgs rig sent

        processLuaMsgs env (rigBackend rig) (rigControl rig)

        queuedMessages rig `shouldReturn` 0
        joined rig "__msgs" `shouldReturn` T.intercalate "," sent

    it "keeps processLuaMsgs draining past a mid-drain worker stop, \
       \which the bounded path deliberately does not" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        -- 'LuaThreadKill' is the in-thread writer of ThreadStopped, and
        -- the exhaustive helper has never read the control ref: messages
        -- queued behind a kill are still dispatched in the same drain,
        -- and synchronous input settlement depends on that whole-queue
        -- contract. The bounded path stopping there (above) must not
        -- leak into this one.
        seen ← newIORef (0 ∷ Int)
        installHook rig "__hook" $ do
            k ← atomicModifyIORef' seen (\n → (n + 1, n + 1))
            when (k ≡ 2) $ writeIORef (rigControl rig) ThreadStopped
        let sent = [T.pack ('m' : show i) | i ← [1 .. 10 ∷ Int]]
        queueMsgs rig sent

        processLuaMsgs env (rigBackend rig) (rigControl rig)

        queuedMessages rig `shouldReturn` 0
        joined rig "__msgs" `shouldReturn` T.intercalate "," sent
        readIORef (rigControl rig) `shouldReturn` ThreadStopped

    it "leaves processDebugCommands draining the whole console queue" $ \env → do
        rig ← newRig env
        registerModule rig 1 "scripts/fairness_witness.lua" 1.0 1e9 witnessChunk
        forM_ [1 .. 12 ∷ Int] $ \i →
            void $ queueCommand rig (T.pack ("__cmds[#__cmds+1] = 'c" ⧺ show i ⧺ "'"))

        processDebugCommands (lbsLuaState (rigBackend rig)) (debugQueueOf rig)

        (map dcCommand ⊚ drainQueuedCommands rig) `shouldReturn` []
        counted rig "__cmds" `shouldReturn` "12"

    it "bounds the console drain to its limit, cancelled entries \
       \included, when the scheduler is the caller" $ \env → do
        rig ← newRig env
        cmds ← forM [1 .. 12 ∷ Int] $ \i →
            queueCommand rig (T.pack ("__cmds[#__cmds+1] = 'c" ⧺ show i ⧺ "'"))
        void $ Lua.runWith (lbsLuaState (rigBackend rig))
            (Lua.dostring "__cmds = {}" ∷ Lua.LuaE Lua.Exception Lua.Status)
        void $ cancelDebugCommand (cmds !! 0) "CANCELLED: for the test"

        n ← processDebugCommandsBounded (lbsLuaState (rigBackend rig))
                (debugQueueOf rig) consoleCommandBudget (pure True)

        n `shouldBe` consoleCommandBudget
        joined rig "__cmds" `shouldReturn` "c2,c3,c4,c5,c6,c7,c8"
        leftovers ← drainQueuedCommands rig
        length leftovers `shouldBe` 4

-- | Run an action and then a cleanup, whatever the action did. Used for
--   the examples that mutate the SHARED save barrier or load status, so
--   a failing assertion cannot leave either engaged for the next
--   example.
finallyReopen ∷ IO α → IO () → IO α
finallyReopen = finally
