-- | The monotonic elapsed-time contract (#2204).
--
--   Four interval consumers — render frame timing, the world tick, the
--   unit tick and the Lua scheduler — used to derive elapsed time from
--   the wall clock, so a host sleep or clock correction became a huge
--   or negative elapsed value: epoch-sized first-frame deltas, a
--   calendar stepping backwards, unit game time and movement jumping by
--   the length of the sleep, and every missed Lua interval replayed as
--   a burst. "Engine.Core.Clock" is now the one boundary all four pass
--   through, and these examples drive the REAL consumer entry points
--   ('updateFrameTimingWith', 'worldTickWith', 'unitTickWith',
--   'runDueScripts') with an injected clock rather than a copy of their
--   arithmetic. The contract itself: @docs/engine_contracts.md@
--   §Monotonic elapsed time.
--
--   Each example boots its own no-world engine
--   ('withHeadlessEngineNoWorld'): the world example makes its page
--   VISIBLE, which a live world worker would also tick on its own clock
--   and race the exact-advancement assertion; the unit example calls
--   the tick directly, and the harness never starts a unit worker.
module Test.Headless.Core.MonotonicClock (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (throwIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import GHC.Clock (getMonotonicTime)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Clock (maxElapsedStep, sanitiseElapsed, sampleElapsed)
import Engine.Core.Monad (EngineM', runEngineM)
import qualified Engine.Core.Queue as Q
import Engine.Core.State
    ( EngineEnv, EngineLifecycle(..), EngineState(..), TimingState(..)
    , assetPoolRef, engineStateRef, inputStateRef, lifecycleRef, loggerRef
    , nextObjectIdRef, videoConfigRef )
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Loop.Headless (headlessMode)
import Engine.Loop.Mode (runStartupHandshake)
import Engine.Loop.Timing (FrameTimingSeams(..), updateFrameTimingWith)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState, runDueScripts)
import Engine.Scripting.Lua.TickPolicy (advanceTick)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaScript(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Unit.Thread
    ( UnitTickSeams(..), productionUnitTickSeams, unitTickRate, unitTickWith )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread (worldTickWith)
import World.Time.Types (WorldDate(..), WorldTime(..))

-- * Seams

-- | A clock that answers the given samples in order and fails the
--   example if asked for more: every example states exactly how many
--   times the path under test reads the clock.
scriptedClock ∷ [Double] → IO (IO Double)
scriptedClock samples = do
    ref ← newIORef samples
    pure $ do
        remaining ← readIORef ref
        case remaining of
            (s:rest) → writeIORef ref rest ≫ pure s
            []       → throwIO (userError "scripted clock exhausted")

-- | Record every value an injected effect is handed, in call order.
recorder ∷ IO (IORef [α], α → IO ())
recorder = do
    ref ← newIORef []
    pure (ref, \x → modifyIORef' ref (⧺ [x]))

-- | Floating-point closeness for the deltas below (a hundredth of a
--   millisecond is far finer than any assertion here needs).
near ∷ Double → Double → Bool
near expected actual = abs (actual - expected) < 1e-9

-- * Render timing

readTiming ∷ EngineEnv → IO TimingState
readTiming env = timingState ⊚ readIORef (engineStateRef env)

-- | Pre-seed the retained raw sample for the sanitisation examples,
--   which are about the differences and not about the first sample.
seedLastFrame ∷ EngineEnv → Double → IO ()
seedLastFrame env t = modifyIORef' (engineStateRef env) $ \s →
    s { timingState = (timingState s) { lastFrameTime = t } }

-- | The frameDt path only runs under a software cap: the shipped
--   defaults are VSync on and no limit, which skip it entirely.
setSoftwareCap ∷ EngineEnv → Int → IO ()
setSoftwareCap env fps = modifyIORef' (videoConfigRef env) $ \vc →
    vc { vcVSync = False, vcFrameLimit = Just fps }

-- | The sleep a zero frameDt produces under a cap: the compensated
--   target, exactly as 'updateFrameTimingWith' computes it.
compensatedSleepMicros ∷ Int → Int
compensatedSleepMicros fps =
    floor ((1.0 / fromIntegral fps - 0.0012) * 1000000 ∷ Double)

runFrameTiming ∷ EngineEnv → FrameTimingSeams → IO ()
runFrameTiming env seams = do
    let action ∷ EngineM' ()
        action = updateFrameTimingWith seams
    _ ← runEngineM action env pure
    pure ()

-- * World tick

clockPage ∷ WorldPageId
clockPage = WorldPageId "monotonic_clock_page"

-- | One visible page at the given time of day and date, no generation
--   parameters (so the tick's chunk loading and discovery skip it), at
--   the given time scale, unpaused.
installClockPage ∷ EngineEnv → WorldTime → WorldDate → Float → IO WorldState
installClockPage env time date scale = do
    ws ← emptyWorldState
    writeIORef (wsTimeRef ws) time
    writeIORef (wsDateRef ws) date
    writeIORef (wsTimeScaleRef ws) scale
    let sim = toWorldSimCapability env
    writeIORef (wsWorldManagerRef sim) emptyWorldManager
        { wmWorlds = [(clockPage, ws)], wmVisible = [clockPage] }
    writeIORef (wsEnginePausedRef sim) False
    pure ws

-- | 0.25 s × 2880 game-minutes per real second = 720 game-minutes, a
--   whole number the calendar stores exactly (it floors to the minute
--   with no remainder accumulator); from 23:30 that lands on 11:30 the
--   next day, exercising the time-of-day and the date carry at once.
fullDayScale ∷ Float
fullDayScale = 2880

-- * Lua

fixturePath ∷ BS.ByteString
fixturePath = "scripts/lua_tick_interval_fixture.lua"

-- | A bare Lua backend with the full API registered and its own queues
--   (same technique as "Test.Headless.Lua.TickInterval").
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

-- | The fixture's update counter, off its @_G@ marker table.
fixtureUpdates ∷ LuaBackendState → IO (Maybe Lua.Integer)
fixtureUpdates ls = Lua.runWith (lbsLuaState ls) go
  where
    go ∷ Lua.LuaE Lua.Exception (Maybe Lua.Integer)
    go = do
        tyTbl ← Lua.getglobal (Lua.Name "luaTickIntervalFixture")
        result ← if tyTbl ≡ Lua.TypeTable
            then do
                tyField ← Lua.getfield (-1) (Lua.Name "updates")
                v ← if tyField ≡ Lua.TypeNumber
                        then Lua.tointeger (-1)
                        else pure Nothing
                Lua.pop 1
                pure v
            else pure Nothing
        Lua.pop 1
        pure result

loadedScript ∷ LuaBackendState → IO LuaScript
loadedScript ls = do
    scripts ← readTVarIO (lbsScripts ls)
    case Map.elems scripts of
        [s] → pure s
        other → do
            expectationFailure $ "expected exactly one loaded script, got "
                               ⧺ show (length other)
            pure (syntheticScript 0 0)

syntheticScript ∷ Double → Double → LuaScript
syntheticScript rate next = LuaScript
    { scriptId        = 1
    , scriptPath      = "scripts/synthetic-monotonic.lua"
    , scriptTickRate  = rate
    , scriptNextTick  = next
    , scriptModuleRef = Lua.RefNil
    , scriptPaused    = False
    }

-- * Spec

spec ∷ Spec
spec = describe "monotonic elapsed-time contract" $ do

    ------------------------------------------------------------------
    -- The shared boundary
    ------------------------------------------------------------------
    describe "the shared sanitiser" $ do
        it "names a maximum step of exactly a quarter second" $
            maxElapsedStep `shouldBe` 0.25

        it "maps a negative, NaN, +Infinity and -Infinity difference to zero" $ do
            sanitiseElapsed (-1)     `shouldBe` 0
            sanitiseElapsed (-1e-9)  `shouldBe` 0
            sanitiseElapsed (0 / 0)  `shouldBe` 0
            sanitiseElapsed (1 / 0)  `shouldBe` 0
            sanitiseElapsed (-1 / 0) `shouldBe` 0

        it "passes zero, an in-range value and exactly the cap through unchanged" $ do
            sanitiseElapsed 0    `shouldBe` 0
            sanitiseElapsed 0.1  `shouldBe` 0.1
            sanitiseElapsed 0.25 `shouldBe` 0.25

        it "caps an over-cap difference at exactly the maximum step" $ do
            sanitiseElapsed 0.2500001 `shouldBe` 0.25
            sanitiseElapsed 3600      `shouldBe` 0.25
            sanitiseElapsed 1e300     `shouldBe` 0.25

    describe "sampleElapsed" $ do
        it "replaces the previous raw sample after an over-cap measurement, \
           \so the dropped excess never carries into the next one" $ do
            lastRef ← newIORef 100
            clock ← scriptedClock [3700, 3700.1]
            sampleElapsed clock lastRef `shouldReturn` 0.25
            readIORef lastRef `shouldReturn` 3700
            second ← sampleElapsed clock lastRef
            second `shouldSatisfy` near 0.1

        it "replaces the previous raw sample after a negative measurement" $ do
            lastRef ← newIORef 100
            clock ← scriptedClock [50, 50.125]
            sampleElapsed clock lastRef `shouldReturn` 0
            readIORef lastRef `shouldReturn` 50
            sampleElapsed clock lastRef `shouldReturn` 0.125

    ------------------------------------------------------------------
    -- Render timing
    ------------------------------------------------------------------
    describe "render frame timing" $ do
        it "takes its first sample in the startup handshake, so the first \
           \stored deltaTime is a real bounded frame and not a difference \
           \from zero" $ withHeadlessEngineNoWorld $ \env → do
            before ← readTiming env
            lastFrameTime before `shouldBe` 0

            -- The production path: the one handshake every mode runs
            -- before its first running tick.
            writeIORef (lifecycleRef env) EngineStarting
            let handshake ∷ EngineM' ()
                handshake = runStartupHandshake headlessMode env
            _ ← runEngineM handshake env pure

            primed ← lastFrameTime ⊚ readTiming env
            now ← getMonotonicTime
            primed `shouldSatisfy` (> 0)
            (now - primed) `shouldSatisfy` (\d → d ≥ 0 ∧ d < 5)

            -- The first frame, 20 ms after the primed sample: measured
            -- from it. (Measured from zero it would be capped at 0.25,
            -- which is why the exact small value is what is asserted.)
            setSoftwareCap env 60
            clock ← scriptedClock [primed + 0.01, primed + 0.02]
            (_, recordSleep) ← recorder
            runFrameTiming env (FrameTimingSeams clock recordSleep)
            after ← readTiming env
            deltaTime after `shouldSatisfy` near 0.02
            lastFrameTime after `shouldBe` primed + 0.02

        it "sanitises the pre-sleep frameDt on its own: a backward clock \
           \step sleeps at most the compensated target" $
            withHeadlessEngineNoWorld $ \env → do
            seedLastFrame env 100
            setSoftwareCap env 60
            -- An hour backwards before the sleep; 50 ms after it.
            clock ← scriptedClock [100 - 3600, 100.05]
            (sleeps, recordSleep) ← recorder
            runFrameTiming env (FrameTimingSeams clock recordSleep)
            readIORef sleeps `shouldReturn` [compensatedSleepMicros 60]
            after ← readTiming env
            deltaTime after `shouldSatisfy` near 0.05
            lastFrameTime after `shouldBe` 100.05

        it "sanitises the pre-sleep frameDt on its own: a NaN difference \
           \sleeps at most the compensated target" $
            withHeadlessEngineNoWorld $ \env → do
            seedLastFrame env 100
            setSoftwareCap env 60
            clock ← scriptedClock [0 / 0, 100.05]
            (sleeps, recordSleep) ← recorder
            runFrameTiming env (FrameTimingSeams clock recordSleep)
            readIORef sleeps `shouldReturn` [compensatedSleepMicros 60]
            after ← readTiming env
            deltaTime after `shouldSatisfy` near 0.05

        it "sanitises the post-sleep actualDt separately, stores the raw \
           \sample, and feeds the FPS window the sanitised value" $
            withHeadlessEngineNoWorld $ \env → do
            seedLastFrame env 100
            -- Defaults: VSync on, so no frameDt path and one clock read.
            clock ← scriptedClock [3700, 3700.1]
            (sleeps, recordSleep) ← recorder
            runFrameTiming env (FrameTimingSeams clock recordSleep)
            readIORef sleeps `shouldReturn` []
            capped ← readTiming env
            deltaTime capped        `shouldBe` 0.25
            fpsWindowElapsed capped `shouldBe` 0.25
            lastFrameTime capped    `shouldBe` 3700
            -- The next frame is measured from the replaced raw sample.
            runFrameTiming env (FrameTimingSeams clock recordSleep)
            next ← readTiming env
            deltaTime next `shouldSatisfy` near 0.1

    ------------------------------------------------------------------
    -- World tick
    ------------------------------------------------------------------
    describe "the world tick" $ do
        it "advances an unpaused calendar by exactly effectiveTimeScale \
           \× 0.25 game-minutes on an over-cap sample, carrying the date \
           \across midnight" $ withHeadlessEngineNoWorld $ \env → do
            ws ← installClockPage env (WorldTime 23 30) (WorldDate 1 1 1)
                                  fullDayScale
            lastRef ← newIORef 100
            clock ← scriptedClock [3700]
            _ ← worldTickWith clock env lastRef
            readIORef (wsTimeRef ws) `shouldReturn` WorldTime 11 30
            readIORef (wsDateRef ws) `shouldReturn` WorldDate 1 1 2
            readIORef lastRef `shouldReturn` 3700

        it "advances by sanitised seconds × effectiveTimeScale on an \
           \in-range sample" $ withHeadlessEngineNoWorld $ \env → do
            ws ← installClockPage env (WorldTime 23 30) (WorldDate 1 1 1)
                                  fullDayScale
            lastRef ← newIORef 100
            -- 0.125 s × 2880 = 360 game-minutes: six hours.
            clock ← scriptedClock [100.125]
            _ ← worldTickWith clock env lastRef
            readIORef (wsTimeRef ws) `shouldReturn` WorldTime 5 30
            readIORef (wsDateRef ws) `shouldReturn` WorldDate 1 1 2

        it "passes a backward clock step as zero and measures the next \
           \tick from the replaced raw sample" $
            withHeadlessEngineNoWorld $ \env → do
            ws ← installClockPage env (WorldTime 23 30) (WorldDate 1 1 1)
                                  fullDayScale
            lastRef ← newIORef 100
            -- 49.7 s backwards. Deliberately NOT a multiple of half a
            -- second: at 2880 game-minutes per second every half
            -- second is a whole day, and the raw path's wrap would
            -- then land back on 23:30 by coincidence. -49.7 s raw is
            -- -143136 game-minutes, which wraps to 13:54.
            clock ← scriptedClock [50.3, 50.425]
            _ ← worldTickWith clock env lastRef
            readIORef (wsTimeRef ws) `shouldReturn` WorldTime 23 30
            readIORef (wsDateRef ws) `shouldReturn` WorldDate 1 1 1
            readIORef lastRef `shouldReturn` 50.3
            _ ← worldTickWith clock env lastRef
            readIORef (wsTimeRef ws) `shouldReturn` WorldTime 5 30
            readIORef (wsDateRef ws) `shouldReturn` WorldDate 1 1 2

        it "leaves a paused calendar alone whatever the sample" $
            withHeadlessEngineNoWorld $ \env → do
            ws ← installClockPage env (WorldTime 23 30) (WorldDate 1 1 1)
                                  fullDayScale
            writeIORef (wsEnginePausedRef (toWorldSimCapability env)) True
            lastRef ← newIORef 100
            clock ← scriptedClock [3700]
            _ ← worldTickWith clock env lastRef
            readIORef (wsTimeRef ws) `shouldReturn` WorldTime 23 30
            readIORef (wsDateRef ws) `shouldReturn` WorldDate 1 1 1

    ------------------------------------------------------------------
    -- Unit tick
    ------------------------------------------------------------------
    describe "the unit tick" $ do
        let unitSeams clock recordMove recordSleep = productionUnitTickSeams
                { tickClock    = clock
                , tickMovement = \dt _ _ → recordMove dt
                , tickSleep    = recordSleep
                }
            fullTickSleep = floor (unitTickRate * 1000000) ∷ Int

        it "advances wsGameTimeRef and hands movement at most the maximum \
           \step on an over-cap sample, replacing the raw sample" $
            withHeadlessEngineNoWorld $ \env → do
            let sim = toWorldSimCapability env
            writeIORef (wsEnginePausedRef sim) False
            writeIORef (wsGameTimeRef sim) 0
            lastRef ← newIORef 100
            -- tickStart, then tickEnd: no time passes inside the tick.
            clock ← scriptedClock [3700, 3700]
            (moves, recordMove) ← recorder
            (sleeps, recordSleep) ← recorder
            _ ← unitTickWith (unitSeams clock recordMove recordSleep) env
                             lastRef (ucUtsRef (toUnitCombatCapability env))
            readIORef (wsGameTimeRef sim) `shouldReturn` 0.25
            readIORef moves `shouldReturn` [0.25]
            readIORef lastRef `shouldReturn` 3700
            readIORef sleeps `shouldReturn` [fullTickSleep]

        it "paces its sleep from a sanitised execution measurement: a \
           \backward step inside the tick sleeps one full tick, not the \
           \step" $ withHeadlessEngineNoWorld $ \env → do
            let sim = toWorldSimCapability env
            writeIORef (wsEnginePausedRef sim) False
            lastRef ← newIORef 100
            clock ← scriptedClock [100.01, 90]
            (_, recordMove) ← recorder
            (sleeps, recordSleep) ← recorder
            _ ← unitTickWith (unitSeams clock recordMove recordSleep) env
                             lastRef (ucUtsRef (toUnitCombatCapability env))
            readIORef sleeps `shouldReturn` [fullTickSleep]

        it "paces its sleep from a sanitised execution measurement: a NaN \
           \difference sleeps one full tick" $
            withHeadlessEngineNoWorld $ \env → do
            let sim = toWorldSimCapability env
            writeIORef (wsEnginePausedRef sim) False
            lastRef ← newIORef 100
            clock ← scriptedClock [100.01, 0 / 0]
            (_, recordMove) ← recorder
            (sleeps, recordSleep) ← recorder
            _ ← unitTickWith (unitSeams clock recordMove recordSleep) env
                             lastRef (ucUtsRef (toUnitCombatCapability env))
            readIORef sleeps `shouldReturn` [fullTickSleep]

    ------------------------------------------------------------------
    -- Lua scheduling
    ------------------------------------------------------------------
    describe "Lua timed-script scheduling" $ do
        it "advanceTick keeps the old-deadline cadence below one interval \
           \of lateness and resets from now at one interval or more" $ do
            let s = syntheticScript 0.25 10
            scriptNextTick (advanceTick 10.01   s) `shouldBe` 10.25
            scriptNextTick (advanceTick 10.2499 s) `shouldBe` 10.25
            -- 0.3 late: one complete interval and change → from now.
            scriptNextTick (advanceTick 10.3    s) `shouldBe` 10.55
            scriptNextTick (advanceTick 13      s) `shouldBe` 13.25

        it "a script whose clock jumps across several intervals runs once, \
           \leaves a deadline past the jumped clock, and is not due again \
           \at that same now" $ withHeadlessEngineNoWorld $ \env → do
            ls ← newBareBackend env
            runLua_ ls $ "probe = { sid = engine.loadScript('" <> fixturePath
                         <> "', 0.25) }"
            script ← loadedScript ls
            let deadline = scriptNextTick script

            -- Small lateness: #1695's cadence, unchanged.
            runDueScripts ls (deadline + 0.01)
            fixtureUpdates ls `shouldReturn` Just 1
            cadence ← loadedScript ls
            scriptNextTick cadence `shouldBe` deadline + 0.25

            -- Forty intervals late in one jump: once, then a deadline
            -- strictly past the jumped clock.
            let jumped = scriptNextTick cadence + 10
            runDueScripts ls jumped
            fixtureUpdates ls `shouldReturn` Just 2
            reset ← loadedScript ls
            scriptNextTick reset `shouldSatisfy` (> jumped)
            scriptNextTick reset `shouldBe` jumped + 0.25

            -- The same pass again at the same clock: nothing to replay.
            runDueScripts ls jumped
            fixtureUpdates ls `shouldReturn` Just 2
            again ← loadedScript ls
            scriptNextTick again `shouldBe` scriptNextTick reset
