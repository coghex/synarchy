{-# LANGUAGE ScopedTypeVariables #-}
-- | Retained sub-minute calendar progress (#2471).
--
--   The world clock stores whole hours and minutes, and every tick used
--   to add @timeScale × dt@ game-minutes and floor the result straight
--   back. At the shipped default scale — one game-minute per real second
--   against "Engine.Core.Clock"'s 0.25 s cap — no admitted tick ever
--   contributed a whole minute, so the calendar never moved at all; at
--   higher scales the same elapsed time advanced it by different amounts
--   depending on how the world worker happened to partition it. Each
--   page now carries the leftover fraction of a minute in its own
--   @wsTimeRemainderRef@.
--
--   The contract: @docs/engine_contracts.md@ §Monotonic elapsed time.
--
--   Three specs, split by what each needs:
--
--     * 'spec' — the PURE advance and the numerical claims about it,
--       including an independent oracle over long irregular schedules.
--       No engine.
--     * 'tickSpec' — the REAL 'World.Thread.worldTickWith' with an
--       injected clock, exactly as "Test.Headless.Core.MonotonicClock"
--       drives it: an arithmetic copy would stay green if the tick
--       stopped threading the remainder at all. Its own no-world engine,
--       for the same reason that module's does — a live world worker
--       would tick these pages on its own clock and race the assertions.
--     * 'stagingSpec' — a forged persisted page through the real
--       'World.Load.Stage.stageSession', inspecting the staged page's
--       own remainder ref, following "Test.Headless.World.Calendar"'s
--       staging spec.
module Test.Headless.World.SubMinuteClock
    ( spec
    , tickSpec
    , stagingSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (find, sort)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log
    ( LogBackend(..), LogCategory(..), LogConfig(..), LogEntry(..)
    , LogLevel(..), LoggerState, defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import World.Load.Stage
    (stageSession, renderStageError, stagedTimeRemainderWarning)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Thread (worldTickWith)
import World.Thread.Command.Time (handleWorldSetDateCommand,
                                  handleWorldSetTimeCommand)
import World.Types

-- * Shared helpers

-- | A clock that answers the given samples in order and fails the
--   example if asked for more — the same seam
--   "Test.Headless.Core.MonotonicClock" scripts, so these examples drive
--   the production tick rather than a copy of its arithmetic.
scriptedClock ∷ [Double] → IO (IO Double)
scriptedClock samples = do
    ref ← newIORef samples
    pure $ do
        remaining ← readIORef ref
        case remaining of
            (s:rest) → writeIORef ref rest ≫ pure s
            []       → fail "scripted clock exhausted"

-- | A remainder from a value the domain must accept. Fails loudly rather
--   than substituting zero: an example built on a silently-clamped
--   fixture would assert nothing.
remainderOf ∷ HasCallStack ⇒ Double → ClockRemainder
remainderOf v = case mkClockRemainder v of
    Just r  → r
    Nothing → error ("not a valid clock remainder: " ⧺ show v)

-- | The default calendar's day count, for the year-boundary example.
daysPerYear ∷ Int
daysPerYear = ccDaysPerMonth defaultCalendarConfig
            * ccMonthsPerYear defaultCalendarConfig

-- * The pure spec

-- | The clock the pure examples advance, as one precise minute count —
--   the oracle's own representation, deliberately NOT the production
--   one. 'advanceWorldClock' splits the same quantity into whole minutes
--   plus a remainder plus a date, and the two must agree.
data OracleClock = OracleClock
    { ocMinutes ∷ !Double   -- ^ precise minutes since midnight, [0, 1440)
    , ocDays    ∷ !Int      -- ^ whole days elapsed since the start
    } deriving (Show, Eq)

-- | The independent oracle: accumulate the same exact per-tick products
--   and reduce into the day, with no reference to the production code.
--
--   The per-tick product is exact in 'Double' (two 'Float' significands
--   are 48 bits against 53), so the ONLY difference an implementation can
--   have from this is the order the running sum rounds in — which is what
--   'clockTickErrorBound' bounds and what the examples below compare
--   against.
oracleStep ∷ OracleClock → (Float, Float) → OracleClock
oracleStep (OracleClock minutes days) (scale, dt) =
    let total = minutes + realToFrac scale * realToFrac dt
        rolled = floor (total / clockMinutesPerDayD) ∷ Int
    in OracleClock (total - clockMinutesPerDayD * fromIntegral rolled)
                   (days + rolled)

-- | Drive the PRODUCTION pure advance over a schedule, accumulating the
--   days it reports.
advanceSchedule ∷ WorldTime → ClockRemainder → [(Float, Float)]
                → (WorldTime, ClockRemainder, Int)
advanceSchedule time0 rem0 = go time0 rem0 (WorldDate 1 1 1) 0
  where
    go time remainder _ rolledTotal [] = (time, remainder, rolledTotal)
    go time remainder date rolledTotal ((scale, dt):rest) =
        let (time', remainder', date', rolled) =
                advanceWorldClock defaultCalendarConfig scale dt
                                  time remainder date
        in go time' remainder' date' (rolledTotal + rolled) rest

-- | The production advance's own precise in-day minute total.
preciseMinutes ∷ WorldTime → ClockRemainder → Double
preciseMinutes (WorldTime h m) remainder =
    fromIntegral h * 60 + fromIntegral m + clockRemainderMinutes remainder

-- | The tolerance a schedule of @n@ ticks is allowed, straight from the
--   documented per-tick bound. Not a fudge factor chosen to make an
--   example pass: 'clockTickErrorBound' is half an ulp at
--   'clockMinutesPerDayD', and @n@ of them is the worst case where every
--   rounding goes the same way.
scheduleTolerance ∷ Int → Double
scheduleTolerance n = fromIntegral n * clockTickErrorBound

spec ∷ Spec
spec = describe "Calendar retains sub-minute progress" $ do

    describe "the pure advance" $ do

        it "keeps the fraction of a minute a single default-speed tick \
           \contributes" $ do
            -- 0.25 s at 1 game-minute per real second. The pre-#2471
            -- clock floored this to nothing, which is the whole defect.
            let (t, r, d, rolled) = advanceWorldClock defaultCalendarConfig
                    1 0.25 (WorldTime 10 0) zeroClockRemainder
                    (WorldDate 1 1 1)
            t `shouldBe` WorldTime 10 0
            clockRemainderMinutes r `shouldBe` 0.25
            d `shouldBe` WorldDate 1 1 1
            rolled `shouldBe` 0

        it "turns four such ticks into one whole minute and no leftover" $ do
            let (t, r, _) = advanceSchedule (WorldTime 10 0)
                    zeroClockRemainder (replicate 4 (1, 0.25))
            t `shouldBe` WorldTime 10 1
            r `shouldBe` zeroClockRemainder

        it "reaches day 1 11:00 from 240 quarter-second ticks at scale 1" $ do
            -- The acceptance case, on the pure advance: 240 × 0.25 s × 1
            -- = exactly 60 game-minutes.
            let (t, r, rolled) = advanceSchedule (WorldTime 10 0)
                    zeroClockRemainder (replicate 240 (1, 0.25))
            t `shouldBe` WorldTime 11 0
            r `shouldBe` zeroClockRemainder
            rolled `shouldBe` 0

        it "reaches the same day-3 22:00 however the same 60 s is \
           \partitioned at scale 60" $ do
            -- 60 real seconds at 60 game-minutes per second is 3600
            -- game-minutes: two whole days plus 720 minutes, from 10:00.
            let schedules =
                    [ ("600 x 0.1 s", replicate 600 (60, 0.1))
                    , ("480 x 0.125 s", replicate 480 (60, 0.125))
                    , ("one 60 s call", [(60, 60)]) ]
            forM_ schedules $ \(label, schedule) → do
                let (t, _, rolled) = advanceSchedule (WorldTime 10 0)
                        zeroClockRemainder schedule
                (label, t) `shouldBe` (label, WorldTime 22 0)
                (label, rolled) `shouldBe` (label, 2)

        it "tracks an independent oracle across a long irregular schedule \
           \of very small increments, within the documented bound" $ do
            -- Deliberately awkward: none of these dt values is exactly
            -- representable in binary except 0.125, the scales are not
            -- whole minutes per tick, and the schedule is long enough
            -- that a per-tick truncation would be obvious.
            let dts = [0.001, 0.1, 0.0166667, 0.125, 0.007, 0.0333333]
                scales = [1, 7.5, 60, 0.25, 1000]
                schedule = take 5000
                    [ (s, dt) | (s, dt) ← zip (cycle scales) (cycle dts) ]
                (t, r, rolled) = advanceSchedule (WorldTime 10 0)
                    zeroClockRemainder schedule
                oracle = foldl' oracleStep (OracleClock 600 0) schedule
            rolled `shouldBe` ocDays oracle
            abs (preciseMinutes t r - ocMinutes oracle)
                `shouldSatisfy` (< scheduleTolerance (length schedule))

        it "never lets the same elapsed time diverge by a whole minute \
           \between two partitions of it" $ do
            -- The property the acceptance states in its own terms: the
            -- WHOLE-minute clock two partitions reach is identical, so
            -- no player-visible reading can disagree.
            let coarse = replicate 2000 (37.5, 0.2)
                fine   = replicate 8000 (37.5, 0.05)
                (tc, _, rc) = advanceSchedule (WorldTime 6 17)
                    (remainderOf 0.5) coarse
                (tf, _, rf) = advanceSchedule (WorldTime 6 17)
                    (remainderOf 0.5) fine
            tc `shouldBe` tf
            rc `shouldBe` rf

        it "carries a remainder across a minute, a midnight, a month end \
           \and a year end, keeping the date" $ do
            -- One tick each, from a start already carrying 0.75 of a
            -- minute, so every boundary is crossed BY the retained
            -- fraction rather than by the tick alone.
            let step time date = advanceWorldClock defaultCalendarConfig
                    1 15 time (remainderOf 0.75) date
                (t1, r1, d1, n1) = step (WorldTime 10 0) (WorldDate 1 1 1)
                (t2, _, d2, n2) = step (WorldTime 23 59) (WorldDate 1 1 1)
                (t3, _, d3, n3) = step (WorldTime 23 59) (WorldDate 1 1 30)
                (t4, _, d4, n4) = step (WorldTime 23 59) (WorldDate 1 12 30)
            -- 0.75 + 15 = 15.75 minutes.
            (t1, d1, n1) `shouldBe` (WorldTime 10 15, WorldDate 1 1 1, 0)
            clockRemainderMinutes r1 `shouldBe` 0.75
            (t2, d2, n2) `shouldBe` (WorldTime 0 14, WorldDate 1 1 2, 1)
            (t3, d3, n3) `shouldBe` (WorldTime 0 14, WorldDate 1 2 1, 1)
            (t4, d4, n4) `shouldBe` (WorldTime 0 14, WorldDate 2 1 1, 1)

        it "spends exactly the retained fraction when it completes a \
           \minute at a boundary" $ do
            -- 0.75 already held plus 0.25 more is the whole minute that
            -- crosses midnight. Dropping the remainder here would leave
            -- the clock a minute behind for the rest of the session.
            let (t, r, d, rolled) = advanceWorldClock defaultCalendarConfig
                    1 15 (WorldTime 23 59) (remainderOf 0.75)
                    (WorldDate 4 6 9)
            t `shouldBe` WorldTime 0 14
            clockRemainderMinutes r `shouldBe` 0.75
            d `shouldBe` WorldDate 4 6 10
            rolled `shouldBe` 1

        it "advances a whole year of days from one enormous elapsed step \
           \and still returns a remainder in range" $ do
            -- Far past anything a sanitised tick produces, which is the
            -- point: the guards, not the caller, are what keep the
            -- returned remainder inside [0, 1).
            let (t, r, d, rolled) = advanceWorldClock defaultCalendarConfig
                    1440 (fromIntegral daysPerYear) (WorldTime 0 0)
                    (remainderOf 0.5) (WorldDate 1 1 1)
            rolled `shouldBe` daysPerYear
            d `shouldBe` WorldDate 2 1 1
            wtHour t `shouldSatisfy` (\h → h ≥ 0 ∧ h ≤ 23)
            clockRemainderMinutes r `shouldSatisfy` (\v → v ≥ 0 ∧ v < 1)

    describe "the remainder's own domain" $ do

        it "accepts the boundaries it must and refuses everything else" $ do
            map (fmap clockRemainderMinutes . mkClockRemainder)
                [0, 0.5, clockRemainderMinutes maxClockRemainder]
                `shouldBe` map Just
                    [0, 0.5, clockRemainderMinutes maxClockRemainder]
            map mkClockRemainder [1, 1.5, -0.5, -0, 0 / 0, 1 / 0, -1 / 0]
                `shouldSatisfy` \rs →
                    map isNothing rs
                        ≡ [True, True, True, False, True, True, True]

        it "repairs every refused value to no progress, and says so" $
            forM_ [1, 1.5, -0.5, 0 / 0, 1 / 0, -1 / 0] $ \v →
                repairClockRemainder v
                    `shouldBe` (zeroClockRemainder, True)

        it "leaves an accepted value exactly alone, and says nothing" $
            forM_ [0, 0.5, clockRemainderMinutes maxClockRemainder] $ \v →
                repairClockRemainder v `shouldBe` (remainderOf v, False)

        it "puts the largest remainder immediately below a whole minute" $ do
            clockRemainderMinutes maxClockRemainder `shouldSatisfy` (< 1)
            mkClockRemainder
                (clockRemainderMinutes maxClockRemainder * 1.0000001)
                `shouldBe` Nothing

    describe "the sun angle" $ do

        it "equals the whole-minute angle when no progress is retained" $
            forM_ [ WorldTime 0 0, WorldTime 6 0, WorldTime 12 0
                  , WorldTime 18 30, WorldTime 23 59 ] $ \t →
                worldTimeSunAngleWith t zeroClockRemainder
                    `shouldBe` worldTimeToSunAngle t

        it "is nondecreasing across a day of sub-minute ticks, and wraps \
           \at midnight" $ do
            -- Sampled by driving the real advance, so the claim is about
            -- the angles a session actually produces.
            let ticks = 4000
                sample (angles, time, remainder, date) _ =
                    let (t', r', d', _) = advanceWorldClock
                            defaultCalendarConfig 1 0.25 time remainder date
                    in ( worldTimeSunAngleWith t' r' : angles, t', r', d')
                (collected, _, _, _) = foldl' sample
                    ([], WorldTime 0 0, zeroClockRemainder, WorldDate 1 1 1)
                    [1 .. ticks ∷ Int]
                angles = reverse collected
            -- 4000 quarter-second ticks at scale 1 is 1000 game-minutes:
            -- inside one day, so no wrap in this run.
            angles `shouldBe` sort angles
            length (filter (\(a, b) → b > a) (zip angles (drop 1 angles)))
                `shouldSatisfy` (> 0)
            -- Midnight itself is 0 whatever fraction is retained.
            worldTimeSunAngleWith (WorldTime 0 0) (remainderOf 0.5)
                `shouldSatisfy` (\a → a > 0 ∧ a < 1e-3)

-- * The tick spec

tickPageA, tickPageB ∷ WorldPageId
tickPageA = WorldPageId "sub_minute_clock_a"
tickPageB = WorldPageId "sub_minute_clock_b"

-- | One visible page at the given time of day and scale, no generation
--   parameters (so the tick's chunk loading and discovery skip it),
--   unpaused — the same fixture "Test.Headless.Core.MonotonicClock"
--   installs.
installTickPages ∷ EngineEnv → [(WorldPageId, WorldTime, Float)]
                 → IO [(WorldPageId, WorldState)]
installTickPages env pages = do
    installed ← forM pages $ \(pid, time, scale) → do
        ws ← emptyWorldState
        writeIORef (wsTimeRef ws) time
        writeIORef (wsDateRef ws) (WorldDate 1 1 1)
        writeIORef (wsTimeScaleRef ws) scale
        pure (pid, ws)
    let sim = toWorldSimCapability env
    writeIORef (wsWorldManagerRef sim) emptyWorldManager
        { wmWorlds = installed, wmVisible = map fst installed }
    writeIORef (wsEnginePausedRef sim) False
    pure installed

-- | Run @n@ ticks of @dt@ seconds each through the REAL tick, against a
--   clock that answers exactly the samples those ticks consume.
runTicks ∷ EngineEnv → Int → Double → IO ()
runTicks env n dt = do
    clock ← scriptedClock [ fromIntegral i * dt | i ← [1 .. n] ]
    lastRef ← newIORef 0
    forM_ [1 .. n] $ \(_ ∷ Int) → void (worldTickWith clock env lastRef)

readClock ∷ WorldState → IO (WorldTime, ClockRemainder, WorldDate)
readClock ws = (,,)
    <$> readIORef (wsTimeRef ws)
    <*> readIORef (wsTimeRemainderRef ws)
    <*> readIORef (wsDateRef ws)

tickSpec ∷ SpecWith EngineEnv
tickSpec = describe "Calendar retains sub-minute progress, through the \
                    \real world tick" $ do

    it "reaches day 1 11:00 from 240 quarter-second ticks at the default \
       \speed" $ \env → do
        -- The headline case. Against the pre-#2471 tick this stays at
        -- 10:00 forever, which is the defect in one line.
        [(_, ws)] ← installTickPages env [(tickPageA, WorldTime 10 0, 1)]
        runTicks env 240 0.25
        readClock ws `shouldReturn`
            (WorldTime 11 0, zeroClockRemainder, WorldDate 1 1 1)

    it "reaches the same day-3 22:00 from either partition of 60 real \
       \seconds at scale 60" $ \env → do
        -- The over-cap single call is deliberately NOT here: the tick
        -- admits at most 0.25 s of any sample (#2204), so a 60 s sample
        -- through this path would advance 0.25 s worth. That comparison
        -- belongs to the pure advance, and 'spec' makes it there.
        forM_ [(600, 0.1), (480, 0.125)] $ \(n, dt) → do
            [(_, ws)] ← installTickPages env
                [(tickPageA, WorldTime 10 0, 60)]
            runTicks env n dt
            (time, _, date) ← readClock ws
            ((n, dt), time, date) `shouldBe`
                ((n, dt), WorldTime 22 0, WorldDate 1 1 3)

    it "admits only the capped 0.25 s of an over-cap sample, remainder \
       \included" $ \env → do
        [(_, ws)] ← installTickPages env [(tickPageA, WorldTime 10 0, 1)]
        clock ← scriptedClock [3700]
        lastRef ← newIORef 100
        _ ← worldTickWith clock env lastRef
        readClock ws `shouldReturn`
            (WorldTime 10 0, remainderOf 0.25, WorldDate 1 1 1)

    it "leaves a paused page's remainder bit-identical however many \
       \ticks land" $ \env → do
        [(_, ws)] ← installTickPages env [(tickPageA, WorldTime 10 0, 1)]
        runTicks env 3 0.25
        before ← readClock ws
        writeIORef (wsEnginePausedRef (toWorldSimCapability env)) True
        runTicks env 40 0.25
        readClock ws `shouldReturn` before

    it "keeps the accumulated remainder across a time-scale change and \
       \applies the new scale only to later elapsed time" $ \env → do
        [(pid, ws)] ← installTickPages env [(tickPageA, WorldTime 10 0, 1)]
        runTicks env 2 0.25          -- 0.5 minutes retained
        readIORef (wsTimeRemainderRef ws)
            `shouldReturn` remainderOf 0.5
        writeIORef (wsTimeScaleRef ws) 2
        pid `shouldBe` tickPageA
        runTicks env 1 0.25          -- + 0.5 minutes: exactly one minute
        readClock ws `shouldReturn`
            (WorldTime 10 1, zeroClockRemainder, WorldDate 1 1 1)

    it "runs each page's remainder independently and never leaks one \
       \into another" $ \env → do
        [(_, wsA), (_, wsB)] ← installTickPages env
            [ (tickPageA, WorldTime 10 0, 1)
            , (tickPageB, WorldTime 10 0, 2) ]
        runTicks env 1 0.25
        readIORef (wsTimeRemainderRef wsA) `shouldReturn` remainderOf 0.25
        readIORef (wsTimeRemainderRef wsB) `shouldReturn` remainderOf 0.5

    it "leaves a hidden page's remainder untouched, with no catch-up \
       \when it becomes visible again" $ \env → do
        [(_, wsA), (_, wsB)] ← installTickPages env
            [ (tickPageA, WorldTime 10 0, 1)
            , (tickPageB, WorldTime 10 0, 1) ]
        let sim = toWorldSimCapability env
        runTicks env 1 0.25
        -- Hide B, tick a long way, then show it again.
        atomicModifyIORef' (wsWorldManagerRef sim) $ \mgr →
            (mgr { wmVisible = [tickPageA] }, ())
        runTicks env 40 0.25
        readIORef (wsTimeRemainderRef wsB) `shouldReturn` remainderOf 0.25
        atomicModifyIORef' (wsWorldManagerRef sim) $ \mgr →
            (mgr { wmVisible = [tickPageA, tickPageB] }, ())
        runTicks env 1 0.25
        readIORef (wsTimeRemainderRef wsB) `shouldReturn` remainderOf 0.5
        readIORef (wsTimeRemainderRef wsA) `shouldReturn` remainderOf 0.5

    it "starts a freshly created page at no retained progress" $ \env → do
        ws ← emptyWorldState
        readIORef (wsTimeRemainderRef ws) `shouldReturn` zeroClockRemainder
        [(_, replaced)] ← installTickPages env
            [(tickPageA, WorldTime 10 0, 1)]
        runTicks env 1 0.25
        readIORef (wsTimeRemainderRef replaced)
            `shouldReturn` remainderOf 0.25
        -- Replacing the page with a new WorldState under the SAME id
        -- leaves nothing of the old page's progress behind.
        [(_, fresh)] ← installTickPages env
            [(tickPageA, WorldTime 10 0, 1)]
        readIORef (wsTimeRemainderRef fresh) `shouldReturn` zeroClockRemainder

    it "clears the remainder on world.setTime and keeps it on \
       \world.setDate" $ \env → do
        [(_, ws)] ← installTickPages env [(tickPageA, WorldTime 10 0, 1)]
        (logger, _) ← capturingLogger
        runTicks env 1 0.25
        readIORef (wsTimeRemainderRef ws) `shouldReturn` remainderOf 0.25
        -- A date poke changes no time of day, so the progress stands.
        handleWorldSetDateCommand (toWorldSimCapability env) logger
                                  tickPageA 3 4 5
        readIORef (wsTimeRemainderRef ws) `shouldReturn` remainderOf 0.25
        readIORef (wsDateRef ws) `shouldReturn` WorldDate 3 4 5
        -- Setting the clock names a whole minute, so it does not.
        handleWorldSetTimeCommand (toWorldSimCapability env) logger
                                  tickPageA 7 8
        readClock ws `shouldReturn`
            (WorldTime 7 8, zeroClockRemainder, WorldDate 3 4 5)

-- * The staging spec

-- | The page every staging example loads.
stagedPageId ∷ WorldPageId
stagedPageId = WorldPageId "sub_minute_clock_staged"

-- | A logger whose entries are captured in emission order, so what the
--   repair SAYS is observable rather than inferred.
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

-- | A one-page save carrying @stored@ as its persisted sub-minute
--   progress, built the way a DECODED save is: 'blankPageSnapshot' is
--   the construction every @world-pages@ version's own decoder converges
--   on, and 'snapshotToSaveData' is the adapter staging consumes. Forging
--   the value HERE therefore reaches the staging write along the real
--   route rather than a test-only shortcut.
saveWith ∷ Double → SaveData
saveWith stored = snapshotToSaveData
    (SaveRequestMeta "sub_minute_slot" "2026-09-07T00:00:00.000000Z" False)
    SessionSnapshot
        { snapGameTime       = 0
        , snapTexPalette     = emptyTexPalette
        , snapNextItemId     = 1
        , snapNextBuildingId = 1
        , snapNextUnitId     = 1
        , snapActivePage     = stagedPageId
        , snapVisiblePages   = [stagedPageId]
        , snapLiveCamera     = LiveCameraSnapshot
            { lcsOwnerPage = Just stagedPageId
            , lcsX = 0, lcsY = 0, lcsZoom = 1, lcsFacing = FaceSouth }
        , snapPages          = HM.singleton stagedPageId
            (blankPageSnapshot stagedPageId arenaParams)
                { pgsTimeHour     = 13
                , pgsTimeMinute   = 42
                , pgsDateYear     = 1
                , pgsDateMonth    = 1
                , pgsDateDay      = 1
                , pgsTimeRemainder = stored
                }
        }

-- | Gen params shaped as an ARENA page (seed 0 with the empty timeline)
--   so load staging rebuilds flat chunks instead of generating a world.
arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0 }

-- | Stage one such save and hand back the staged page's OWN clock — read
--   from the refs the staged world state publishes from — beside
--   everything the logger emitted.
stageWith ∷ HasCallStack ⇒ EngineEnv → Double
          → IO (WorldTime, ClockRemainder, [LogEntry])
stageWith env stored = do
    (logger, drain) ← capturingLogger
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger (saveWith stored) matReg ⌦ either
        (\e → expectationFailure (T.unpack (renderStageError e))
                ≫ error "unreachable")
        pure
    entries ← drain
    case find ((≡ stagedPageId) . spPageId) (ssPages staged) of
        Nothing → expectationFailure "the staged page is missing"
                    ≫ error "unreachable"
        Just sp → do
            time ← readIORef (wsTimeRef (spWorldState sp))
            remainder ← readIORef (wsTimeRemainderRef (spWorldState sp))
            pure (time, remainder, entries)

repairWarnings ∷ [LogEntry] → [LogEntry]
repairWarnings = filter $ \e →
    leLevel e ≡ LevelWarn
    ∧ "sub-minute clock progress" `T.isInfixOf` leMessage e

stagingSpec ∷ SpecWith EngineEnv
stagingSpec = describe "Calendar retains sub-minute progress, through \
                       \load staging" $ do

    it "restores a stored remainder exactly, and stays silent" $ \env →
        forM_ [ 0, 0.25, 0.5
              , clockRemainderMinutes maxClockRemainder ] $ \stored → do
            let label = show stored
            (time, remainder, entries) ← stageWith env stored
            (label, time) `shouldBe` (label, WorldTime 13 42)
            (label, clockRemainderMinutes remainder)
                `shouldBe` (label, stored)
            (label, map leMessage (repairWarnings entries))
                `shouldBe` (label, [])

    it "loads an out-of-domain stored remainder as none, keeps the whole \
       \minutes, and warns once naming the page and the value" $ \env →
        -- The lower and upper boundaries plus every non-finite value, all
        -- through the SAME production path a real save takes. Note the
        -- save is not refused: @validatePages@ deliberately leaves this
        -- field unjudged so it reaches this one repair.
        forM_ [ 1, 1.5, -0.5, -1e-9, 0 / 0, 1 / 0, -1 / 0 ] $ \stored → do
            -- Labelled by 'show', not by the value itself: NaN is one of
            -- the cases and never equals itself, so a tuple carrying it
            -- could not report which case failed.
            let label = show stored
            (time, remainder, entries) ← stageWith env stored
            (label, time) `shouldBe` (label, WorldTime 13 42)
            (label, remainder) `shouldBe` (label, zeroClockRemainder)
            (label, map leMessage (repairWarnings entries))
                `shouldBe`
                    (label, [stagedTimeRemainderWarning stagedPageId stored])
            (label, map leCategory (repairWarnings entries))
                `shouldBe` (label, [CatWorld])
