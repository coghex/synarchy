-- | The Lua tick-interval policy (#1695).
--
--   @engine.loadScript@ and @engine.setTickInterval@ used to write any
--   Lua number straight into 'scriptTickRate'/'scriptNextTick' and
--   report success, while the scheduler assumed a usable finite
--   interval. Zero and negative rates left a script permanently overdue
--   at the 1 ms sleep floor (~1 kHz — and two shipped gameplay modules
--   really were driven that way by the @0.0@ tool call sites); @NaN@
--   left it never due while pinning the loop at that floor;
--   @+Infinity@ overflowed the microsecond conversion to a zero
--   timeout and spun the loop with no sleep at all.
--
--   These specs pin the whole policy from
--   "Engine.Scripting.Lua.TickPolicy": what each entry point stores,
--   what it refuses (and that it refuses IDENTICALLY, leaving the
--   previous interval alone and naming the value in its log line), and
--   the scheduling that follows. The scheduling half drives the real
--   functions the Lua thread's loop calls — 'schedulerSleepMicros' for
--   the wait it takes and 'runDueScripts' for the pass it makes — so a
--   change to the loop cannot pass here by leaving a copy behind.
--
--   The second group pins the scheduler's REENTRANCY rule (#2205): what
--   'runDueScripts' stores for a script whose schedule was changed from
--   inside a callback of the same pass. The scheduler used to reschedule
--   each script AFTER its @update@ returned, on top of whatever that
--   callback had just stored, so a script that set its own interval next
--   ran at about @now + 2·rate@ — and so did any other script whose
--   interval a callback changed before its own turn came round.
--
--   Bare Lua backend, no world, no script pre-loaded — same technique
--   as "Test.Headless.Lua.ScriptState", whose sibling this is.
module Test.Headless.Lua.TickInterval (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..)
    , LogCategory(..), defaultLogConfig, initLogger )
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState, runDueScripts)
import Engine.Scripting.Lua.TickPolicy
    ( TickInterval(..), TickIntervalRefusal(..), classifyTickInterval
    , describeTickRefusal, tickIntervalSeconds, maxSleepMicros
    , minTickInterval, nextTimerWake, schedulerSleepMicros, scriptIsDue
    , scriptIsTimed )
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..), LuaScript(..))
import Engine.Scripting.Lua.Util (broadcastToModulesReportingErrors, nowSeconds)

-- | The fixture path, exactly as a script would name it: relative to
--   the resource root, which the test suite already runs from.
fixturePath ∷ BS.ByteString
fixturePath = "scripts/lua_tick_interval_fixture.lua"

-- | The #2205 fixture: a module whose @update@ runs whatever engine
--   calls the example installed on it, so a reschedule can be driven
--   from INSIDE a real 'runDueScripts' pass rather than around one.
reentrantPath ∷ BS.ByteString
reentrantPath = "scripts/lua_tick_reentrancy_fixture.lua"

-- | A bare Lua backend with the full API registered, no script loaded,
--   and its OWN message queues rather than the shared 'EngineEnv' ones,
--   so the queue-responsiveness example below can write to the real
--   @lbsMsgQueues@ without leaking a message into another spec's engine.
newBareBackend ∷ EngineEnv → IO LuaBackendState
newBareBackend env = do
    lteq ← Q.newQueue
    etlq ← Q.newQueue
    ls ← createLuaBackendState lteq etlq (assetPoolRef env)
                               (nextObjectIdRef env) (inputStateRef env)
                               (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run a chunk on the backend's canonical state, failing the example
--   outright on a Lua error — every chunk below is expected to run
--   clean, refusals included (the policy logs, it does not raise).
runLua_ ∷ LuaBackendState → BS.ByteString → IO ()
runLua_ ls src = do
    result ← Lua.runWith (lbsLuaState ls) $ do
        status ← Lua.dostring src
        case status of
            Lua.OK → pure (Right ())
            _ → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                pure (Left (maybe "unknown error" TE.decodeUtf8Lenient err))
    case result of
        Right () → pure ()
        Left err → expectationFailure $ "Lua chunk failed: " ⧺ T.unpack err

-- | Read one field off a global table, with the caller deciding how to
--   convert whatever type it holds.
withTableField ∷ LuaBackendState → BS.ByteString → BS.ByteString
               → (Lua.Type → Lua.LuaE Lua.Exception (Maybe α))
               → IO (Maybe α)
withTableField ls tbl field convert = Lua.runWith (lbsLuaState ls) $ do
    tyTbl ← Lua.getglobal (Lua.Name tbl)
    result ← if tyTbl ≡ Lua.TypeTable
        then do
            tyField ← Lua.getfield (-1) (Lua.Name field)
            v ← convert tyField
            Lua.pop 1
            pure v
        else pure Nothing
    Lua.pop 1
    pure result

readInt ∷ LuaBackendState → BS.ByteString → BS.ByteString → IO (Maybe Lua.Integer)
readInt ls tbl field = withTableField ls tbl field $ \ty →
    if ty ≡ Lua.TypeNumber then Lua.tointeger (-1) else pure Nothing

readNumber ∷ LuaBackendState → BS.ByteString → BS.ByteString → IO (Maybe Double)
readNumber ls tbl field = withTableField ls tbl field $ \ty →
    if ty ≡ Lua.TypeNumber
        then fmap (\(Lua.Number d) → d) ⊚ Lua.tonumber (-1)
        else pure Nothing

-- | Is this global absent? The load-refusal examples use it to show the
--   fixture's chunk never ran, which a "returned nil" assertion alone
--   would not.
globalIsNil ∷ LuaBackendState → BS.ByteString → IO Bool
globalIsNil ls name = Lua.runWith (lbsLuaState ls) go
  where
    go ∷ Lua.LuaE Lua.Exception Bool
    go = do
        ty ← Lua.getglobal (Lua.Name name)
        Lua.pop 1
        pure (ty ≡ Lua.TypeNil)

-- | Swap the engine's logger for a capturing one while an action runs,
--   restoring it afterwards. Every API handler reads the logger back
--   out of 'loggerRef' per call, so this catches the refusal
--   diagnostics at the level they were actually emitted.
withCapturedLog ∷ EngineEnv → IO α → IO (α, [LogEntry])
withCapturedLog env act = do
    capturedRef ← newIORef []
    original ← readIORef (loggerRef env)
    capturing ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :)) }
    result ← (writeIORef (loggerRef env) capturing ≫ act)
                 `finally` writeIORef (loggerRef env) original
    entries ← readIORef capturedRef
    pure (result, reverse entries)

-- | The Lua-source spelling of a value, including the ones with no
--   literal syntax.
luaLiteral ∷ Double → BS.ByteString
luaLiteral v
  | isNaN v            = "(0/0)"
  | isInfinite v ∧ v>0 = "(1/0)"
  | isInfinite v       = "(-1/0)"
  | otherwise          = BS.pack (show v)

-- | Every value the policy refuses, one per reason, plus the two
--   boundary neighbours below the minimum.
refusedValues ∷ [(Double, TickIntervalRefusal)]
refusedValues =
    [ (-1.0,       RefusedNegative)
    , (-0.5,       RefusedNegative)
    , (0/0,        RefusedNaN)
    , (1/0,        RefusedInfinite)
    , (-1/0,       RefusedInfinite)
    , (0.0009,     RefusedTooSmall)
    , (0.0005,     RefusedTooSmall)
    ]

-- | A stand-in script record for the pure scheduling examples, which
--   need particular interval/pause/next-tick combinations without
--   loading anything.
scriptAt ∷ Word32 → Double → Double → Bool → LuaScript
scriptAt sid rate next paused = LuaScript
    { scriptId        = sid
    , scriptPath      = "scripts/synthetic-" ⧺ show sid ⧺ ".lua"
    , scriptTickRate  = rate
    , scriptNextTick  = next
    , scriptModuleRef = Lua.RefNil
    , scriptPaused    = paused
    }

-- | The one script the API examples load, read back out of the backend.
loadedScript ∷ LuaBackendState → IO LuaScript
loadedScript ls = do
    scripts ← readTVarIO (lbsScripts ls)
    case Map.elems scripts of
        [s] → pure s
        other → do
            expectationFailure $ "expected exactly one loaded script, got "
                               ⧺ show (length other)
            pure (scriptAt 0 0 0 False)

-- | The warning text of every captured refusal, on the one channel the
--   policy is permitted to report through.
warnings ∷ [LogEntry] → [Text]
warnings = map leMessage
         ∘ filter (\e → leLevel e ≡ LevelWarn ∧ leCategory e ≡ CatLua)

------------------------------------------------------------------------
-- #2205 helpers: driving a reschedule from inside a real pass
------------------------------------------------------------------------

-- | Read one script back by id, failing the example when it is gone.
scriptById ∷ LuaBackendState → Word32 → IO LuaScript
scriptById ls sid = do
    scripts ← readTVarIO (lbsScripts ls)
    case Map.lookup sid scripts of
        Just s  → pure s
        Nothing → do
            expectationFailure $ "script " ⧺ show sid ⧺ " is no longer loaded"
            pure (scriptAt sid 0 0 False)

-- | The id a @engine.loadScript@ chunk stashed on the shared probe
--   table, as the 'Word32' the backend keys scripts by.
probeId ∷ LuaBackendState → BS.ByteString → IO Word32
probeId ls field = do
    mSid ← readInt ls "reentrantProbe" field
    case mSid of
        Just sid → pure (fromIntegral sid)
        Nothing  → do
            expectationFailure $ "reentrantProbe." ⧺ BS.unpack field
                               ⧺ " is not a script id — the load failed"
            pure 0

-- | Load the reentrancy fixture at @rate@ and tell it its own id: the
--   module is never handed one, and every reentrant body below needs it
--   to name itself to @engine.setTickInterval@ and friends.
loadReentrant ∷ LuaBackendState → BS.ByteString → IO Word32
loadReentrant ls rate = do
    runLua_ ls $ "reentrantProbe = reentrantProbe or {}\n\
                 \reentrantProbe.sid = engine.loadScript('" <> reentrantPath
                 <> "', " <> rate <> ")\n\
                 \luaTickReentrancyFixture.sid = reentrantProbe.sid"
    probeId ls "sid"

-- | Load the plain #1695 fixture alongside it as the cross-script
--   TARGET: it counts its own updates and dt and reschedules nothing,
--   so it shows what a pass did TO a script rather than what one did.
loadPeer ∷ LuaBackendState → BS.ByteString → IO Word32
loadPeer ls rate = do
    runLua_ ls $ "reentrantProbe = reentrantProbe or {}\n\
                 \reentrantProbe.peer = engine.loadScript('" <> fixturePath
                 <> "', " <> rate <> ")"
    probeId ls "peer"

-- | Install the reentrant body the fixture's @update@ will run, as a
--   Lua function of @(sid, dt)@ where @sid@ is the fixture's own id.
installAction ∷ LuaBackendState → BS.ByteString → IO ()
installAction ls body = runLua_ ls $
    "luaTickReentrancyFixture.action = function(sid, dt) " <> body <> " end"

-- | Take the body back off, so a follow-up pass is an ordinary tick.
clearAction ∷ LuaBackendState → IO ()
clearAction ls = runLua_ ls "luaTickReentrancyFixture.action = nil"

-- | Run a pass and report the real-clock window it ran in.
--
--   @engine.setTickInterval@ and @engine.resumeScript@ each sample the
--   clock INSIDE the call ("Engine.Scripting.Lua.API.Core"), not from
--   the @now@ the scheduler was handed, so a deadline one of them stored
--   can be bounded but never predicted — and the tests hand
--   'runDueScripts' a synthetic @now@ that is deliberately not the real
--   clock at all.
passWindow ∷ IO α → IO (α, Double, Double)
passWindow act = do
    t0 ← nowSeconds
    r  ← act
    t1 ← nowSeconds
    pure (r, t0, t1)

-- | This deadline is a callback's own decision and nothing more: it is
--   one @rate@ past a clock sample taken during the pass. The extra
--   interval the pre-#2205 scheduler added on top lands outside the
--   window and fails here.
storedByCallback ∷ Double → Double → Double → Double → Expectation
storedByCallback t0 t1 rate deadline
    | sample ≥ t0 ∧ sample ≤ t1 = pure ()
    | otherwise = expectationFailure $
        "stored deadline " ⧺ show deadline ⧺ " is one interval ("
        ⧺ show rate ⧺ ") past " ⧺ show sample ⧺ ", which is outside the \
          \pass's own clock window [" ⧺ show t0 ⧺ ", " ⧺ show t1
        ⧺ "]: the scheduler wrote over the callback's decision instead \
          \of leaving it alone"
  where
    sample = deadline - rate

-- | The deadline stored after a pass IS the boundary: a pass just
--   before it must not update, and a pass at it must update exactly
--   once more. Comparing against the stored deadline rather than a
--   predicted one keeps the API's own clock sample out of the
--   arithmetic while still catching an unwanted extra interval.
deadlineIsBoundary ∷ LuaBackendState → BS.ByteString → Double → Lua.Integer → IO ()
deadlineIsBoundary ls table deadline updatesSoFar = do
    runDueScripts ls (deadline - 1e-6)
    readInt ls table "updates" ⌦ (`shouldBe` Just updatesSoFar)
    runDueScripts ls deadline
    readInt ls table "updates" ⌦ (`shouldBe` Just (updatesSoFar + 1))

-- | One self-@setTickInterval@ example: the fixture changes its OWN
--   interval from inside @update@, and the pass must store exactly what
--   the callback asked for — one interval past the callback's own clock
--   sample, not two.
selfSetInterval ∷ EngineEnv → BS.ByteString → Double → IO ()
selfSetInterval env newRateSrc newRate = do
    ls ← newBareBackend env
    sid ← loadReentrant ls "0.25"
    before ← scriptById ls sid
    scriptTickRate before `shouldBe` 0.25
    installAction ls $ "engine.setTickInterval(sid, " <> newRateSrc <> ")"

    ((), t0, t1) ← passWindow $ runDueScripts ls (scriptNextTick before + 0.01)

    -- The pass still made exactly the callback it was going to make,
    -- with the PRE-PASS interval as its dt.
    readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)
    readNumber ls "luaTickReentrancyFixture" "lastDt" ⌦ (`shouldBe` Just 0.25)

    after ← scriptById ls sid
    scriptTickRate after `shouldBe` newRate
    storedByCallback t0 t1 newRate (scriptNextTick after)

    -- … and the NEXT update really does arrive one interval later.
    clearAction ls
    deadlineIsBoundary ls "luaTickReentrancyFixture" (scriptNextTick after) 1

spec ∷ SpecWith EngineEnv
spec = do
    policySpec
    reentrancySpec

-- | #1695's interval policy: what each entry point stores, what it
--   refuses, and the scheduling that follows. Kept a group of its own so
--   @--match "Lua tick-interval policy"@ still selects exactly these
--   examples, unchanged, after #2205 added its own beside them.
policySpec ∷ SpecWith EngineEnv
policySpec = describe "Lua tick-interval policy (#1695)" $ do

    ------------------------------------------------------------------
    -- Classification
    ------------------------------------------------------------------
    it "accepts zero as event-only and any finite interval at or above \
       \the minimum, and refuses everything the scheduler cannot honour" $ \_ → do
        classifyTickInterval 0            `shouldBe` Right TickEventOnly
        -- -0.0 compares equal to zero, so it is event-only rather than
        -- negative; the stored rate is normalised back to a plain zero.
        classifyTickInterval (-0.0)       `shouldBe` Right TickEventOnly
        tickIntervalSeconds TickEventOnly `shouldBe` 0
        (1 / tickIntervalSeconds TickEventOnly) `shouldBe` (1/0 ∷ Double)

        classifyTickInterval minTickInterval `shouldBe` Right (TickEvery 0.001)
        classifyTickInterval 0.016           `shouldBe` Right (TickEvery 0.016)
        classifyTickInterval 1.0             `shouldBe` Right (TickEvery 1.0)
        -- Finite, so accepted — and the scheduler still has to bound it.
        classifyTickInterval 1e308           `shouldBe` Right (TickEvery 1e308)

        mapM_ (\(v, reason) → classifyTickInterval v `shouldBe` Left reason)
              refusedValues

    it "names the offending value in every refusal diagnostic, and \
       \states the policy that rejected it" $ \_ →
        mapM_ (\(v, reason) → do
                  let msg = describeTickRefusal reason v
                  msg `shouldSatisfy` (tshow v `T.isInfixOf`)
                  msg `shouldSatisfy` ("event-only" `T.isInfixOf`)
                  msg `shouldSatisfy` ("left unchanged" `T.isInfixOf`))
              refusedValues

    ------------------------------------------------------------------
    -- Scheduling arithmetic — the loop's own functions
    ------------------------------------------------------------------
    it "keeps event-only and paused scripts out of the wake time, so a \
       \set holding only those idles instead of pinning the 1 ms floor" $ \_ → do
        let now       = 1000.0
            eventOnly = scriptAt 1 0     now   False
            paused    = scriptAt 2 0.016 now   True
            timed     = scriptAt 3 0.5   (now + 0.5) False

        nextTimerWake [eventOnly]                 `shouldBe` Nothing
        nextTimerWake [paused]                    `shouldBe` Nothing
        nextTimerWake [eventOnly, paused]         `shouldBe` Nothing
        nextTimerWake [eventOnly, paused, timed]  `shouldBe` Just (now + 0.5)

        -- The idle budget (capped at ~60 Hz), NOT the 1 ms floor of
        -- 1000 microseconds a permanently-overdue script produced.
        schedulerSleepMicros now []                        `shouldBe` maxSleepMicros
        schedulerSleepMicros now [eventOnly]               `shouldBe` maxSleepMicros
        schedulerSleepMicros now [eventOnly, paused]       `shouldBe` maxSleepMicros

        scriptIsTimed eventOnly `shouldBe` False
        scriptIsTimed paused    `shouldBe` False
        scriptIsTimed timed     `shouldBe` True
        scriptIsDue now eventOnly `shouldBe` False
        scriptIsDue (now + 3600) eventOnly `shouldBe` False

    it "bounds every accepted interval's timeout into [1, 16666] \
       \microseconds, including one large enough to overflow the \
       \microsecond conversion" $ \_ → do
        -- Zero, not an arbitrary epoch: the sub-cap case below measures
        -- an exact microsecond count, and (t + 0.005) - t is not
        -- exactly 0.005 for a large t.
        let now = 0.0
        -- 1e308 is finite and therefore accepted, but 1e308 * 1e6 is
        -- Infinity: the old min-after-floor expression produced 0 here
        -- and made registerDelay fire instantly.
        schedulerSleepMicros now [scriptAt 1 1e308 (now + 1e308) False]
            `shouldBe` maxSleepMicros
        schedulerSleepMicros now [scriptAt 1 3600 (now + 3600) False]
            `shouldBe` maxSleepMicros
        -- Ordinary sub-cap waits still measure their real distance.
        schedulerSleepMicros now [scriptAt 1 0.005 (now + 0.005) False]
            `shouldBe` 5000
        -- A due or overdue script gets the floor, which is legitimate:
        -- it is about to run, not being starved.
        schedulerSleepMicros now [scriptAt 1 0.5 now False] `shouldBe` 1000
        schedulerSleepMicros now [scriptAt 1 0.5 (now - 10) False] `shouldBe` 1000
        -- A script sitting exactly at the minimum interval legitimately
        -- schedules at that same floor.
        schedulerSleepMicros now [scriptAt 1 minTickInterval (now + minTickInterval) False]
            `shouldBe` 1000

    ------------------------------------------------------------------
    -- engine.setTickInterval
    ------------------------------------------------------------------
    it "engine.setTickInterval stores an accepted interval, accepts \
       \zero as event-only, and leaves a refused one untouched while \
       \logging the value it refused" $ \env → do
        ls ← newBareBackend env
        runLua_ ls $ "probe = { sid = engine.loadScript('" <> fixturePath
                     <> "', 0.25) }"
        before ← loadedScript ls
        scriptTickRate before `shouldBe` 0.25

        -- Every refusal: neither field moves, and a warning names it.
        mapM_ (\(v, _) → do
                  (_, entries) ← withCapturedLog env $ runLua_ ls $
                      "engine.setTickInterval(probe.sid, " <> luaLiteral v <> ")"
                  after ← loadedScript ls
                  scriptTickRate after `shouldBe` scriptTickRate before
                  scriptNextTick after `shouldBe` scriptNextTick before
                  case warnings entries of
                      [msg] → do
                          msg `shouldSatisfy` ("setTickInterval refused" `T.isInfixOf`)
                          msg `shouldSatisfy` (tshow v `T.isInfixOf`)
                      other → expectationFailure $
                          "expected one refusal warning for " ⧺ show v
                          ⧺ ", got " ⧺ show other)
              refusedValues

        -- The minimum is accepted, and quietly: no warning at all.
        (_, okEntries) ← withCapturedLog env $ runLua_ ls
            "engine.setTickInterval(probe.sid, 0.001)"
        warnings okEntries `shouldBe` []
        atMinimum ← loadedScript ls
        scriptTickRate atMinimum `shouldBe` minTickInterval
        scriptIsTimed atMinimum `shouldBe` True

        -- Zero is accepted and turns the script event-only.
        (_, zeroEntries) ← withCapturedLog env $ runLua_ ls
            "engine.setTickInterval(probe.sid, 0)"
        warnings zeroEntries `shouldBe` []
        eventOnly ← loadedScript ls
        scriptTickRate eventOnly `shouldBe` 0
        scriptIsTimed eventOnly `shouldBe` False
        now ← nowSeconds
        scriptIsDue (now + 3600) eventOnly `shouldBe` False
        nextTimerWake [eventOnly] `shouldBe` Nothing

    ------------------------------------------------------------------
    -- engine.loadScript
    ------------------------------------------------------------------
    it "engine.loadScript refuses a bad interval before the chunk runs, \
       \allocating no id and leaving nothing loaded" $ \env →
        mapM_ (\(v, _) → do
                  ls ← newBareBackend env
                  (_, entries) ← withCapturedLog env $ runLua_ ls $
                      "probe = { sid = engine.loadScript('" <> fixturePath
                      <> "', " <> luaLiteral v <> ") }"

                  -- nil back to Lua …
                  readInt ls "probe" "sid" ⌦ (`shouldBe` Nothing)
                  -- … nothing tracked …
                  scripts ← readTVarIO (lbsScripts ls)
                  Map.null scripts `shouldBe` True
                  -- … and the chunk itself never ran, so not even its
                  -- marker table exists.
                  globalIsNil ls "luaTickIntervalFixture" ⌦ (`shouldBe` True)

                  case warnings entries of
                      [msg] → do
                          msg `shouldSatisfy` ("loadScript refused" `T.isInfixOf`)
                          msg `shouldSatisfy` (tshow v `T.isInfixOf`)
                          msg `shouldSatisfy` ("not loaded" `T.isInfixOf`)
                      other → expectationFailure $
                          "expected one refusal warning for " ⧺ show v
                          ⧺ ", got " ⧺ show other)
              refusedValues

    it "engine.loadScript's refusal never disturbs an already-loaded \
       \script, and a valid duplicate still keeps its own interval" $ \env → do
        ls ← newBareBackend env
        runLua_ ls $ "probe = { sid = engine.loadScript('" <> fixturePath
                     <> "', 0.25) }"
        Just sid ← readInt ls "probe" "sid"
        before ← loadedScript ls

        (_, entries) ← withCapturedLog env $ runLua_ ls $
            "probe.bad = engine.loadScript('" <> fixturePath <> "', -1.0)"
        readInt ls "probe" "bad" ⌦ (`shouldBe` Nothing)
        length (warnings entries) `shouldBe` 1
        afterRefusal ← loadedScript ls
        scriptId       afterRefusal `shouldBe` scriptId before
        scriptTickRate afterRefusal `shouldBe` 0.25
        scriptNextTick afterRefusal `shouldBe` scriptNextTick before
        readInt ls "luaTickIntervalFixture" "loads" ⌦ (`shouldBe` Just 1)

        -- A VALID duplicate is the pre-existing dedup path: same id
        -- back, chunk not re-run, interval deliberately NOT re-set.
        runLua_ ls $ "probe.dup = engine.loadScript('" <> fixturePath
                     <> "', 0.5)"
        readInt ls "probe" "dup" ⌦ (`shouldBe` Just sid)
        afterDup ← loadedScript ls
        scriptTickRate afterDup `shouldBe` 0.25
        readInt ls "luaTickIntervalFixture" "loads" ⌦ (`shouldBe` Just 1)

    it "engine.loadScript accepts zero as event-only: the module loads \
       \and inits, is never updated on a timer however far the clock \
       \moves, and still receives broadcasts" $ \env → do
        ls ← newBareBackend env
        (_, entries) ← withCapturedLog env $ runLua_ ls $
            "probe = { sid = engine.loadScript('" <> fixturePath
            <> "', 0.0) }"
        warnings entries `shouldBe` []
        readInt ls "probe" "sid" ⌦ (`shouldNotBe` Nothing)

        -- Loaded and initialised like any other module.
        readInt ls "luaTickIntervalFixture" "loads" ⌦ (`shouldBe` Just 1)
        readInt ls "luaTickIntervalFixture" "inits" ⌦ (`shouldBe` Just 1)

        script ← loadedScript ls
        scriptTickRate script `shouldBe` 0
        scriptIsTimed script  `shouldBe` False

        -- The real scheduler pass, an hour of clock later: still not due.
        now ← nowSeconds
        runDueScripts ls (now + 3600)
        readInt ls "luaTickIntervalFixture" "updates" ⌦ (`shouldBe` Just 0)
        unchanged ← loadedScript ls
        scriptNextTick unchanged `shouldBe` scriptNextTick script

        -- Event delivery is independent of the timer, and stays so.
        errs ← broadcastToModulesReportingErrors ls "onTickIntervalProbe" []
        errs `shouldBe` []
        readInt ls "luaTickIntervalFixture" "broadcasts" ⌦ (`shouldBe` Just 1)

    it "an ordinary positive interval still ticks, receives that \
       \interval as its dt, and advances its next tick by it" $ \env → do
        ls ← newBareBackend env
        runLua_ ls $ "probe = { sid = engine.loadScript('" <> fixturePath
                     <> "', 0.25) }"
        script ← loadedScript ls
        scriptTickRate script `shouldBe` 0.25

        -- Not yet due …
        runDueScripts ls (scriptNextTick script - 0.01)
        readInt ls "luaTickIntervalFixture" "updates" ⌦ (`shouldBe` Just 0)

        -- … then due: update runs once, with the interval as dt, and
        -- the next tick advances by exactly one interval.
        runDueScripts ls (scriptNextTick script + 0.01)
        readInt ls "luaTickIntervalFixture" "updates" ⌦ (`shouldBe` Just 1)
        readNumber ls "luaTickIntervalFixture" "lastDt" ⌦ (`shouldBe` Just 0.25)
        advanced ← loadedScript ls
        scriptNextTick advanced `shouldBe` scriptNextTick script + 0.25

    it "with only paused and event-only scripts loaded the loop waits \
       \its idle budget, and a queued message is still consumed at once" $ \env → do
        ls ← newBareBackend env
        runLua_ ls $ "probe = { sid = engine.loadScript('" <> fixturePath
                     <> "', 0.0) }"
        -- A paused script alongside it, inserted directly: pausing is
        -- out of scope here, the point is the pair's effect on the wait.
        atomically $ modifyTVar' (lbsScripts ls) $
            Map.insert 9999 (scriptAt 9999 0.016 0 True)

        now ← nowSeconds
        scripts ← readTVarIO (lbsScripts ls)
        let sleepMicros = schedulerSleepMicros now (Map.elems scripts)
        sleepMicros `shouldBe` maxSleepMicros

        -- The loop's own queue, read with the timeout it just computed.
        let (_, etlq) = lbsMsgQueues ls
        Q.writeQueue etlq (LuaFontLoadFailed "tick-interval-probe")
        started ← nowSeconds
        received ← Q.readQueueTimeout sleepMicros etlq
        elapsed ← subtract started ⊚ nowSeconds
        case received of
            Just (LuaFontLoadFailed t) → t `shouldBe` "tick-interval-probe"
            other → expectationFailure $
                "expected the queued message back, got " ⧺ show (() ⚟ other)
        -- Far below the 16.6 ms budget: the wait returns on the message,
        -- not on the timeout.
        elapsed `shouldSatisfy` (< 0.25)

-- | The scheduler's reentrancy rule (#2205): a callback's own
--   scheduling decision is the LAST word on the script it targeted, and
--   'runDueScripts' never overwrites it or adds an interval on top.
--
--   The pass used to reschedule each script after its @update@
--   returned, applied to whatever was stored under that id by then — so
--   a callback's accepted 'engine.setTickInterval' write was advanced a
--   second time and the script next ran at about @now + 2·rate@. Every
--   example here drives the REAL 'runDueScripts'.
reentrancySpec ∷ SpecWith EngineEnv
reentrancySpec = describe "Lua scheduler reentrancy (#2205)" $ do

    ------------------------------------------------------------------
    -- A script rescheduling ITSELF
    ------------------------------------------------------------------
    it "a script that raises its own interval from inside update next \
       \runs one interval later, not two" $ \env →
        selfSetInterval env "1.0" 1.0

    it "a script that lowers its own interval from inside update next \
       \runs one interval later, not two" $ \env →
        selfSetInterval env "0.01" 0.01

    it "the last successful setTickInterval of a pass is the one that \
       \stands" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        before ← scriptById ls sid
        installAction ls "engine.setTickInterval(sid, 1.0) \
                         \engine.setTickInterval(sid, 0.5)"

        ((), t0, t1) ← passWindow $ runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

        after ← scriptById ls sid
        scriptTickRate after `shouldBe` 0.5
        storedByCallback t0 t1 0.5 (scriptNextTick after)

    it "a REFUSED setTickInterval stores nothing and therefore leaves \
       \the ordinary advance standing" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        before ← scriptById ls sid
        installAction ls "engine.setTickInterval(sid, -1.0)"

        (_, entries) ← withCapturedLog env $
            runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

        -- #1695: a refusal leaves BOTH fields alone, so this script's
        -- schedule was never touched by a callback and gets exactly the
        -- advance it would have got with no callback at all.
        after ← scriptById ls sid
        scriptTickRate after `shouldBe` 0.25
        scriptNextTick after `shouldBe` scriptNextTick before + 0.25
        case warnings entries of
            [msg] → do
                msg `shouldSatisfy` ("setTickInterval refused" `T.isInfixOf`)
                msg `shouldSatisfy` (tshow (-1.0 ∷ Double) `T.isInfixOf`)
            other → expectationFailure $
                "expected one refusal warning, got " ⧺ show other

    it "a script that turns ITSELF event-only from inside update stores \
       \the zero and is never timed again" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        before ← scriptById ls sid
        installAction ls "engine.setTickInterval(sid, 0)"

        ((), t0, t1) ← passWindow $ runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)
        readNumber ls "luaTickReentrancyFixture" "lastDt" ⌦ (`shouldBe` Just 0.25)

        after ← scriptById ls sid
        scriptTickRate after `shouldBe` 0
        -- Event-only makes the deadline inert, but it is still the
        -- callback's own: the scheduler wrote nothing after it.
        storedByCallback t0 t1 0 (scriptNextTick after)
        scriptIsTimed after `shouldBe` False
        nextTimerWake [after] `shouldBe` Nothing

        clearAction ls
        runDueScripts ls (scriptNextTick after + 3600)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

    it "a script that pauses ITSELF from inside update keeps its rate \
       \and its ordinary next deadline, and never ticks again while \
       \paused" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        before ← scriptById ls sid
        installAction ls "engine.pauseScript(sid)"

        runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

        -- engine.pauseScript changes ONLY scriptPaused: it stores no
        -- rate and no deadline, so the ordinary advance is what stands.
        -- Either way the deadline is inert while paused — a resume
        -- overwrites it — and the pause flag is the load-bearing part.
        after ← scriptById ls sid
        scriptPaused   after `shouldBe` True
        scriptTickRate after `shouldBe` 0.25
        scriptNextTick after `shouldBe` scriptNextTick before + 0.25
        scriptIsTimed  after `shouldBe` False

        clearAction ls
        runDueScripts ls (scriptNextTick after + 3600)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

    it "a script that pauses and then resumes ITSELF from inside update \
       \is left due at the resume's own sample, with no interval added" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        before ← scriptById ls sid
        installAction ls "engine.pauseScript(sid) engine.resumeScript(sid)"

        ((), t0, t1) ← passWindow $ runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

        -- The resume is the last successful call, so its own write —
        -- scriptNextTick = its clock sample, no interval — stands.
        after ← scriptById ls sid
        scriptPaused   after `shouldBe` False
        scriptTickRate after `shouldBe` 0.25
        storedByCallback t0 t1 0 (scriptNextTick after)

        clearAction ls
        deadlineIsBoundary ls "luaTickReentrancyFixture" (scriptNextTick after) 1

    ------------------------------------------------------------------
    -- A script rescheduling ANOTHER script, in both pass orders
    ------------------------------------------------------------------
    it "an interval change aimed at a script that has not had its turn \
       \yet neither retimes that turn nor gets advanced on top of" $ \env → do
        ls ← newBareBackend env
        -- Scripts are keyed by an ascending id, so loading the mutator
        -- FIRST puts its callback before the target's in the pass.
        mutator ← loadReentrant ls "0.25"
        peer    ← loadPeer ls "0.25"
        mutator `shouldSatisfy` (< peer)

        mBefore ← scriptById ls mutator
        pBefore ← scriptById ls peer
        installAction ls "engine.setTickInterval(reentrantProbe.peer, 1.0)"

        let now = max (scriptNextTick mBefore) (scriptNextTick pBefore) + 0.01
        ((), t0, t1) ← passWindow $ runDueScripts ls now

        -- Exactly one callback each, and the target's dt is still the
        -- interval the pass snapshot selected it with.
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)
        readInt ls "luaTickIntervalFixture"   "updates" ⌦ (`shouldBe` Just 1)
        readNumber ls "luaTickIntervalFixture" "lastDt" ⌦ (`shouldBe` Just 0.25)

        pAfter ← scriptById ls peer
        scriptTickRate pAfter `shouldBe` 1.0
        storedByCallback t0 t1 1.0 (scriptNextTick pAfter)

        -- The mutator touched nobody's schedule but the peer's, so its
        -- own is the ordinary advance.
        mAfter ← scriptById ls mutator
        scriptTickRate mAfter `shouldBe` 0.25
        scriptNextTick mAfter `shouldBe` scriptNextTick mBefore + 0.25

    it "an interval change aimed at a script that has ALREADY had its \
       \turn replaces the deadline the pass gave it" $ \env → do
        ls ← newBareBackend env
        -- The other order: the target is loaded first, so it runs and is
        -- rescheduled before the mutator's callback reaches it.
        peer    ← loadPeer ls "0.25"
        mutator ← loadReentrant ls "0.25"
        peer `shouldSatisfy` (< mutator)

        mBefore ← scriptById ls mutator
        pBefore ← scriptById ls peer
        installAction ls "engine.setTickInterval(reentrantProbe.peer, 1.0)"

        let now = max (scriptNextTick mBefore) (scriptNextTick pBefore) + 0.01
        ((), t0, t1) ← passWindow $ runDueScripts ls now

        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)
        readInt ls "luaTickIntervalFixture"   "updates" ⌦ (`shouldBe` Just 1)
        readNumber ls "luaTickIntervalFixture" "lastDt" ⌦ (`shouldBe` Just 0.25)

        pAfter ← scriptById ls peer
        scriptTickRate pAfter `shouldBe` 1.0
        storedByCallback t0 t1 1.0 (scriptNextTick pAfter)

        mAfter ← scriptById ls mutator
        scriptTickRate mAfter `shouldBe` 0.25
        scriptNextTick mAfter `shouldBe` scriptNextTick mBefore + 0.25

    ------------------------------------------------------------------
    -- The two lifecycle verbs, which change the SET rather than a
    -- schedule
    ------------------------------------------------------------------
    it "a script that kills ITSELF from inside update is gone, quietly, \
       \and never ticks again" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        -- A survivor alongside it, so "removed" means this one and not
        -- simply an empty map.
        peer ← loadPeer ls "0.25"
        before ← scriptById ls sid
        installAction ls "engine.killScript(sid)"

        (_, entries) ← withCapturedLog env $
            runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)
        -- No Lua error and no refusal: a callback that raised would
        -- have logged one here.
        warnings entries `shouldBe` []

        scripts ← readTVarIO (lbsScripts ls)
        Map.member sid  scripts `shouldBe` False
        Map.member peer scripts `shouldBe` True

        runDueScripts ls (scriptNextTick before + 3600)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

    it "a script loaded from inside update is initialised at once but \
       \takes no part in the pass that loaded it" $ \env → do
        ls ← newBareBackend env
        sid ← loadReentrant ls "0.25"
        before ← scriptById ls sid
        installAction ls $ "reentrantProbe.peer = engine.loadScript('"
                           <> fixturePath <> "', 0.05)"

        runDueScripts ls (scriptNextTick before + 0.01)
        readInt ls "luaTickReentrancyFixture" "updates" ⌦ (`shouldBe` Just 1)

        -- Loaded and initialised …
        loaded ← probeId ls "peer"
        readInt ls "luaTickIntervalFixture" "inits" ⌦ (`shouldBe` Just 1)
        scripts ← readTVarIO (lbsScripts ls)
        Map.size scripts `shouldBe` 2
        -- … but it cannot be in the snapshot the pass was already
        -- iterating, so it was not updated by it.
        readInt ls "luaTickIntervalFixture" "updates" ⌦ (`shouldBe` Just 0)

        -- It first becomes due on a LATER pass, at its own deadline.
        clearAction ls
        newScript ← scriptById ls loaded
        scriptTickRate newScript `shouldBe` 0.05
        deadlineIsBoundary ls "luaTickIntervalFixture" (scriptNextTick newScript) 0
