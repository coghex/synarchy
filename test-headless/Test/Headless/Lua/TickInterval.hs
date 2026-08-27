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

spec ∷ SpecWith EngineEnv
spec = describe "Lua tick-interval policy (#1695)" $ do

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
