-- | The Lua worker's scheduling round (#2415).
--
--   == What this replaces
--
--   The loop this module owns used to be written inline in
--   "Engine.Scripting.Lua.Thread"'s tick, and it gave ordinary queue
--   work unbounded priority: engine messages were drained to emptiness,
--   then console commands were, and only the TIMEOUT branch of the
--   subsequent blocking queue read ever reached 'runDueScripts'.
--   Continuously arriving traffic therefore never had to time out, so a
--   due @update@ callback — the AI and physiology cadence among them —
--   could be denied service for as long as the traffic lasted while
--   other workers kept running.
--
--   A round now processes a BOUNDED batch of each ordinary work class
--   and checks the timers in between, so every class gets a turn at a
--   bounded distance from every other. That is a service-OPPORTUNITY
--   guarantee and deliberately nothing stronger: it is not a
--   millisecond deadline, it does not make simulation speed constant
--   under overload, and it cannot preempt a single long-running Lua
--   callback, which still holds this thread for as long as it runs.
--
--   == The round
--
--   1. The owner gate ('Engine.Save.Barrier.ownerGated' for 'SaveLua')
--      is checked at entry. A closed gate parks the round — no ordinary
--      work at all — after the same bounded delay the inline loop took.
--   2. Up to 'slEngineMessages' engine messages are dispatched, oldest
--      first.
--   3. One 'runDueScripts' pass runs, if any script is due and timed
--      work is admitted.
--   4. Up to 'slConsoleEntries' console entries are DEQUEUED — a
--      cancelled entry costs its slot exactly as an executed one does,
--      because dequeuing is the work being budgeted.
--   5. If any admissible work is left the round returns immediately and
--      the worker loop re-enters it with no wait. Otherwise it takes the
--      existing 'Q.readQueueTimeout' idle wait, bounded by
--      'schedulerSleepMicros', and carries any message that wait returns
--      into the NEXT round's engine-message budget.
--
--   Between every one of those steps, and between each individual
--   dispatch inside steps 2 and 4, the gate and the worker's own
--   control state are re-read. A gate that closes mid-round abandons
--   the rest of the round and routes to the park, so a handler that
--   starts a save or load transaction cannot leave the remainder of its
--   own batch running through the transaction's boundary.
--
--   == Why the repeat condition ranges over ADMITTED work only
--
--   Step 5 repeats without an idle wait only while work the round is
--   actually PERMITTED to do remains. A timer that is due but withheld
--   — during the post-publication reconciliation hold below — is
--   deliberately NOT a reason to repeat: it would spin this thread at
--   100% with nothing runnable for the whole hold. Such a round falls
--   through to the ordinary idle wait instead, which is also what makes
--   the narrow window in "World.Thread.Command.Save"'s mismatched
--   @requestId@ branch (gate open, phase still 'LoadWaitingPublish', no
--   @LuaSaveLoaded@ ever coming) cost a sleep rather than a spin.
--
--   == The reconciliation hold
--
--   'World.Load.Publish.publishStagedSession' can queue arbitrarily
--   many messages AHEAD of the @LuaSaveLoaded@ that reconciles Lua with
--   the freshly published session, and it releases the owner gate
--   immediately afterwards. Between those two instants the gate is open
--   and the session is published, but Lua has not reconciled: running a
--   script @update@ or a console command there is exactly the
--   mixed-state window the gate used to keep shut.
--
--   So engine messages keep flowing in FIFO batches — the message must
--   be REACHED, and skipping ahead to it would reorder the queue — while
--   timed passes and ordinary console execution are withheld until
--   'reconciliationHeld' goes false. Queue emptiness is not a usable
--   substitute for that predicate: the hold has to survive more than one
--   batch.
--
--   The hold always terminates. @publishStagedSession@ enqueues
--   @LuaSaveLoaded@ BEFORE @releaseCaptureLock'@ runs, so on the success
--   path the message is already queued the instant the gate opens; the
--   failure path releases the gate and then terminalises the load, which
--   clears the predicate through 'lsOutcome'.
--
--   == What is NOT gated
--
--   'Engine.Load.Status.loadInProgress' is deliberately not the
--   predicate. Load STAGING touches no live engine state and is the one
--   long, observable phase of a load, so gating diagnostics across all
--   of it would blind the debug console for the whole of a load's
--   duration. Only 'LoadWaitingPublish' with no outcome yet holds.
module Engine.Scripting.Lua.Thread.Scheduler
  ( -- * Batch limits
    SchedulerLimits(..)
  , defaultSchedulerLimits
  , engineMessageBudget
  , consoleCommandBudget
  , parkDelayMicros
    -- * The clock \/ wait \/ park seam
  , SchedulerSeam(..)
  , productionSeam
    -- * A worker's scheduling context
  , SchedulerContext(..)
  , newSchedulerContext
    -- * One round
  , RoundResult(..)
  , emptyRoundResult
  , schedulerRound
    -- * The pieces a round is made of
  , processLuaMsgsBounded
  , ordinaryWorkAdmitted
  , workerRunning
  , reconciliationHeld
  , anyScriptDue
  , moreAdmissibleWork
  , runDueScripts
  ) where

import UPrelude
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.Script (callModuleFunction)
import Engine.Scripting.Lua.Util (isValidRef, nowSeconds)
import Engine.Scripting.Lua.TickPolicy
    (schedulerSleepMicros, scriptIsDue, advanceTick)
import Engine.Scripting.Lua.DebugServer (DebugCommand)
import Engine.Scripting.Lua.Thread.Console (processDebugCommandsBounded)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Core.Log (logDebug, LogCategory(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Load.Status
    (LoadPhase(..), LoadStatus(..), readLoadStatus)
import Engine.Save.Barrier (SaveOwner(..), ownerGated)
import qualified Engine.Core.Queue as Q
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Concurrent.STM.TQueue (TQueue, isEmptyTQueue)

-- | How much ordinary work of each class one round may do before the
--   other classes get their turn.
data SchedulerLimits = SchedulerLimits
    { slEngineMessages ∷ !Int
      -- ^ Engine messages DISPATCHED per round. A message the idle wait
      --   returned is carried into the next round and charged here.
    , slConsoleEntries ∷ !Int
      -- ^ Console entries DEQUEUED per round, cancelled ones included:
      --   an unclaimable entry still costs the round a look, and a
      --   client that cancels faster than the drain runs would otherwise
      --   be an unbounded source of free work.
    } deriving (Eq, Show)

-- | The initial engine-message limit. A proposed starting constant, not
--   a measured optimum: it is large enough that ordinary bursts still
--   clear in one round and small enough that a flood cannot hide a
--   script deadline behind it.
engineMessageBudget ∷ Int
engineMessageBudget = 32

-- | The initial console limit. Much smaller than the message budget
--   because each entry can run arbitrary Lua, so eight is already a
--   round of unbounded cost.
consoleCommandBudget ∷ Int
consoleCommandBudget = 8

-- | The bounded delay a parked round takes before returning, unchanged
--   from the inline loop. A parked round is not a busy round: without
--   this the thread would spin for a whole save or load transaction.
parkDelayMicros ∷ Int
parkDelayMicros = 1000

defaultSchedulerLimits ∷ SchedulerLimits
defaultSchedulerLimits = SchedulerLimits
    { slEngineMessages = engineMessageBudget
    , slConsoleEntries = consoleCommandBudget
    }

-- | Everything a round does that a test cannot drive deterministically
--   in place: reading the clock, taking the idle wait, and sleeping the
--   park. 'productionSeam' is the only implementation the engine uses;
--   the record exists so a spec can advance time by hand and observe the
--   wait, and drive the REAL round rather than an imitation of it.
data SchedulerSeam = SchedulerSeam
    { seamNow  ∷ IO Double
      -- ^ The monotonic scheduler clock ('nowSeconds').
    , seamWait ∷ Int → IO (Maybe LuaMsg)
      -- ^ The idle wait, in microseconds. Production is
      --   'Q.readQueueTimeout' on the engine-to-Lua queue — NOT
      --   'System.Timeout.timeout' around a committed read, whose
      --   exception can land after the dequeue commits and drop the
      --   message.
    , seamPark ∷ IO ()
      -- ^ The bounded delay a gate-closed round takes.
    }

productionSeam ∷ LuaBackendState → SchedulerSeam
productionSeam ls = SchedulerSeam
    { seamNow  = nowSeconds
    , seamWait = \micros → Q.readQueueTimeout micros (snd (lbsMsgQueues ls))
    , seamPark = threadDelay parkDelayMicros
    }

-- | One Lua worker's scheduling state. Built ONCE per worker: the carry
--   slot below is the only thing here that must survive between rounds.
data SchedulerContext = SchedulerContext
    { scEnv        ∷ EngineEnv
    , scBackend    ∷ LuaBackendState
    , scControlRef ∷ IORef ThreadControl
    , scDebugQueue ∷ TQueue DebugCommand
    , scSeam       ∷ SchedulerSeam
    , scLimits     ∷ SchedulerLimits
    , scCarryRef   ∷ IORef (Maybe LuaMsg)
      -- ^ A message the idle wait already removed from the queue but
      --   which no round has dispatched yet.
      --
      --   'Q.readQueueTimeout' removes the value atomically, so there is
      --   no putting it back: re-enqueuing would move it BEHIND everything
      --   that arrived since, and dropping it would lose it outright.
      --   Holding it here keeps it ahead of every later entry and charges
      --   it to the engine budget exactly once, whenever service actually
      --   resumes — including across any number of parked rounds in
      --   between.
    }

newSchedulerContext ∷ EngineEnv → LuaBackendState → IORef ThreadControl
                    → TQueue DebugCommand → IO SchedulerContext
newSchedulerContext env ls stateRef debugQueue = do
    carryRef ← newIORef Nothing
    pure SchedulerContext
        { scEnv        = env
        , scBackend    = ls
        , scControlRef = stateRef
        , scDebugQueue = debugQueue
        , scSeam       = productionSeam ls
        , scLimits     = defaultSchedulerLimits
        , scCarryRef   = carryRef
        }

-- | What one round did. Every field is an observable of the round's own
--   decisions, so a spec asserts on work COUNTS and ordering rather than
--   on a wall-clock latency it would have had to invent.
data RoundResult = RoundResult
    { rrMessages  ∷ !Int
      -- ^ Engine messages dispatched, the carried one included.
    , rrConsole   ∷ !Int
      -- ^ Console entries dequeued, cancelled ones included.
    , rrDuePasses ∷ !Int
      -- ^ 'runDueScripts' passes made: never more than one per round.
    , rrParked    ∷ !Bool
      -- ^ The round ended on the owner-park path, at entry or mid-round.
    , rrHalted    ∷ !Bool
      -- ^ The worker left 'ThreadRunning', so the round stopped without
      --   parking. The shared worker loop owns what happens next.
    , rrWaited    ∷ !(Maybe Int)
      -- ^ The idle wait this round took, in microseconds.
    , rrWoke      ∷ !Bool
      -- ^ That wait returned a message, now in the carry slot.
    } deriving (Eq, Show)

emptyRoundResult ∷ RoundResult
emptyRoundResult = RoundResult
    { rrMessages  = 0
    , rrConsole   = 0
    , rrDuePasses = 0
    , rrParked    = False
    , rrHalted    = False
    , rrWaited    = Nothing
    , rrWoke      = False
    }

-- | Run one scheduling round. See the module header for the order and
--   the reasoning; this is the whole of the Lua worker's running tick.
schedulerRound ∷ SchedulerContext → IO RoundResult
schedulerRound ctx = continue emptyRoundResult $ \r0 → do
    r1 ← engineBatch r0
    continue r1 $ \r2 → do
        now ← seamNow seam
        r3 ← timedPass now r2
        continue r3 $ \r4 → do
            r5 ← consoleBatch r4
            continue r5 idleOrRepeat
  where
    seam    = scSeam ctx
    ls      = scBackend ctx
    limits  = scLimits ctx

    -- The fresh lifecycle/transaction check that guards EVERY work-class
    -- switch, including the round's own entry. A closed gate routes to
    -- the park (with its delay) whether it was closed on arrival or shut
    -- by a handler this round already ran; a worker that is no longer
    -- running stops the round and leaves the decision to
    -- 'Engine.Core.Thread.workerLoop', which owns pause polling and stop.
    continue ∷ RoundResult → (RoundResult → IO RoundResult) → IO RoundResult
    continue r k = do
        gated ← ownerGated (saveBarrierRef (scEnv ctx)) SaveLua
        if gated
          then seamPark seam ≫ pure r { rrParked = True }
          else do
            running ← workerRunning ctx
            if running then k r else pure r { rrHalted = True }

    engineBatch r = do
        carried ← readIORef (scCarryRef ctx)
        nCarry ← case carried of
            Nothing  → pure 0
            Just msg → do
                writeIORef (scCarryRef ctx) Nothing
                dispatchOne msg
                pure 1
        n ← processLuaMsgsBounded (scEnv ctx) ls (scControlRef ctx)
                (slEngineMessages limits - nCarry) (ordinaryWorkAdmitted ctx)
        pure r { rrMessages = nCarry + n }

    dispatchOne msg = do
        logger ← readIORef (loggerRef (scEnv ctx))
        logDebug logger CatLua $ "Engine-to-Lua message: " <> tshow msg
        processLuaMsg (scEnv ctx) ls (scControlRef ctx) msg

    timedPass now r = do
        held ← reconciliationHeld ctx
        due  ← if held then pure False else anyScriptDue ls now
        if not due
          then pure r
          else runDueScripts ls now ≫ pure r { rrDuePasses = 1 }

    consoleBatch r = do
        held ← reconciliationHeld ctx
        if held
          then pure r
          else do
            n ← processDebugCommandsBounded (lbsLuaState ls) (scDebugQueue ctx)
                    (slConsoleEntries limits) (ordinaryWorkAdmitted ctx)
            pure r { rrConsole = n }

    idleOrRepeat r = do
        now  ← seamNow seam
        more ← moreAdmissibleWork ctx now
        if more
          then pure r
          else do
            scripts ← readTVarIO (lbsScripts ls)
            let micros = schedulerSleepMicros now (Map.elems scripts)
            mMsg ← seamWait seam micros
            case mMsg of
                Nothing  → pure r { rrWaited = Just micros }
                Just msg → do
                    writeIORef (scCarryRef ctx) (Just msg)
                    pure r { rrWaited = Just micros, rrWoke = True }

-- | May the round do ordinary (message \/ timer \/ console) work right
--   now? Both halves are read fresh: a handler dispatched moments ago
--   may have entered a save or load transaction, and a stop or pause
--   may have been requested from another thread.
ordinaryWorkAdmitted ∷ SchedulerContext → IO Bool
ordinaryWorkAdmitted ctx = do
    gated ← ownerGated (saveBarrierRef (scEnv ctx)) SaveLua
    if gated then pure False else workerRunning ctx

workerRunning ∷ SchedulerContext → IO Bool
workerRunning ctx = (≡ ThreadRunning) ⊚ readIORef (scControlRef ctx)

-- | Is a published-but-unreconciled load holding timed passes and
--   ordinary console execution back? See the module header for why this
--   exact predicate, and why it always clears.
reconciliationHeld ∷ SchedulerContext → IO Bool
reconciliationHeld ctx = do
    mStatus ← readLoadStatus (loadStatusRef (scEnv ctx))
    pure $ case mStatus of
        Nothing     → False
        Just status → lsPhase status ≡ LoadWaitingPublish
                    ∧ isNothing (lsOutcome status)

-- | Would a 'runDueScripts' pass at @now@ call anything? Exactly
--   'scriptIsDue''s answer, so eligibility here and selection inside the
--   pass cannot disagree.
anyScriptDue ∷ LuaBackendState → Double → IO Bool
anyScriptDue ls now =
    any (scriptIsDue now) ∘ Map.elems ⊚ readTVarIO (lbsScripts ls)

-- | Is there work the NEXT round would be permitted to do? The repeat
--   condition, and deliberately not "is anything queued": a due timer
--   that the reconciliation hold withholds is not a reason to skip the
--   idle wait.
moreAdmissibleWork ∷ SchedulerContext → Double → IO Bool
moreAdmissibleWork ctx now = do
    ok ← ordinaryWorkAdmitted ctx
    if not ok then pure False else do
        stats ← Q.queueStats (snd (lbsMsgQueues (scBackend ctx)))
        if Q.qsDepth stats > 0 then pure True else do
            held ← reconciliationHeld ctx
            if held then pure False else do
                idle ← atomically (isEmptyTQueue (scDebugQueue ctx))
                if not idle
                  then pure True
                  else anyScriptDue (scBackend ctx) now

-- | 'Engine.Scripting.Lua.Thread.Dispatch.processLuaMsgs', bounded.
--
--   Dispatches at most @limit@ messages and leaves the rest queued in
--   order, so the queue's own telemetry stays true: each removed message
--   is counted dequeued exactly once by 'Q.tryReadQueue', and every
--   leftover is still a live element of the queue.
--
--   @admitted@ is re-run BEFORE each dequeue, never after: a message
--   this drain has taken off the queue is always dispatched, so a gate
--   that closes mid-batch stops the batch without stranding an
--   already-removed message. The exhaustive drain is untouched and stays
--   the one synchronous input settlement uses.
processLuaMsgsBounded ∷ EngineEnv → LuaBackendState → IORef ThreadControl
                      → Int → IO Bool → IO Int
processLuaMsgsBounded env ls stateRef limit admitted = go 0
  where
    (_, etlq) = lbsMsgQueues ls
    go n
      | n ≥ limit = pure n
      | otherwise = do
          ok ← admitted
          if not ok then pure n else do
            mMsg ← Q.tryReadQueue etlq
            case mMsg of
                Nothing  → pure n
                Just msg → do
                    logger ← readIORef (loggerRef env)
                    logDebug logger CatLua $
                        "Engine-to-Lua message: " <> tshow msg
                    processLuaMsg env ls stateRef msg
                    go (n + 1)

-- | One scheduler pass over the loaded scripts: reschedule every script
--   that is DUE, then call @update@ on each of them.
--
--   Which scripts those are is 'scriptIsDue''s answer and nothing else,
--   so a paused or event-only script (interval @0@, #1695) is skipped
--   here for exactly the reason it was skipped when the sleep was
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
--   A round makes at most ONE of these passes (#2415), so overdue work
--   can never consume repeated passes ahead of the message and console
--   opportunities the same round owes them.
--
--   Re-exported from "Engine.Scripting.Lua.Thread" so
--   "Test.Headless.Lua.TickInterval" can drive the real pass against a
--   bare backend rather than reproducing it.
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
