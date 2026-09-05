{-# LANGUAGE ScopedTypeVariables #-}
-- | Calendar / annual-cycle selection tests, and the canonical form of a
--   'WorldDate' at every ingress that can store one (#2339).
--
--   Two defects live here, and the module covers both.
--
--   The first is the day-of-month-aliased-as-day-of-year bug: the flora
--   renderer used to feed 'wdDay' (a 1..30 day-of-month) into annual-cycle
--   stage selection, so any stage starting after the first month was
--   unreachable. 'pureSpec' pins that 'worldDateToDayOfYear' maps a
--   'WorldDate' to a zero-based year-relative ordinal using the calendar's
--   fixed month length, and that 'findActiveCycleStage' picks the right
--   stage for ordinal days well past day 31.
--
--   The second (#2339) is that the STORED components used to disagree with
--   every derived reading of them. @world.setDate@ forwarded three
--   unchecked integers and the handler wrote them verbatim, while
--   'worldDateToDayOfYear' clamped — so @world.setDate(p, 1, 14, 40)@ left
--   @world.getDate@ reporting @month = 14, day = 40@ beside a @dayOfYear@
--   computed for month 12, day 30, and the next midnight rollover then
--   snapped the raw fields to a date the getter had never reported. A save
--   captured and restored those raw fields unchanged, so the disagreement
--   outlived the session. 'canonicalWorldDate' is now the one form both
--   ingresses store and both ordinal converters read, and the three specs
--   below cover it at each of them:
--
--     * 'pureSpec' — the form itself, and that the converters agree with it.
--     * 'setterSpec' — the REAL @world.setDate@ route: the registered Lua
--       binding, the queued 'WorldSetDate', the production handler, and
--       @world.getDate@ reading the result back. Pure canonicalization
--       examples alone would stay green if that wiring were deleted.
--     * 'stagingSpec' — a forged persisted page through the real
--       'World.Load.Stage.stageSession', inspecting the staged page's own
--       'wsDateRef'.
module Test.Headless.World.Calendar
    ( spec
    , setterSpec
    , stagingSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified HsLua as Lua
import qualified Engine.Core.Queue as Q
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (find)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.Log
    ( LogBackend(..), LogCategory(..), LogConfig(..), LogEntry(..)
    , LogLevel(..), LoggerState, defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Palette (emptyTexPalette)
import World.Flora.Render (findActiveCycleStage)
import World.Load.Stage
    (stageSession, renderStageError, stagedWorldDateWarning)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Thread.Command.Time
    (handleWorldSetDateCommand, setDateClampWarning)
import World.Thread.Time (tickWorldTime)
import World.Types

-- * Calendars

-- Default calendar is 30 days/month × 12 months = 360-day year.
daysPerYear ∷ Int
daysPerYear = ccDaysPerMonth defaultCalendarConfig
            * ccMonthsPerYear defaultCalendarConfig

-- | A calendar that is NOT the shipped default in either dimension, and
--   whose bounds STRADDLE the default's: 10 months (fewer than 12) of 40
--   days (more than 30). A date is therefore judged differently under the
--   two in both directions — month 12 is legal by default and out of range
--   here, day 40 is out of range by default and legal here — so an example
--   that clamps against it cannot be passing on hard-coded defaults.
oddCalendar ∷ CalendarConfig
oddCalendar = defaultCalendarConfig
    { ccDaysPerMonth  = 40
    , ccMonthsPerYear = 10
    }

-- * The pure spec

doy ∷ Int → Int → Int
doy month day = worldDateToDayOfYear defaultCalendarConfig
                                     (WorldDate 1 month day)

-- A four-stage annual cycle authored 0-based, like the real flora data
-- (dormant 0, budding 60, flowering 120, senescing 270).
stages ∷ [AnnualStage]
stages =
    [ AnnualStage CycleDormant    0   (TextureHandle 1)
    , AnnualStage CycleBudding    60  (TextureHandle 2)
    , AnnualStage CycleFlowering  120 (TextureHandle 3)
    , AnnualStage CycleSenescing  270 (TextureHandle 4)
    ]

selectedTag ∷ Int → Maybe AnnualStageTag
selectedTag d = asTag <$> findActiveCycleStage stages d

-- | Dates spanning every way a component can leave its range, plus
--   several that are already canonical. Shared by the agreement
--   properties below so a case added here is covered by all of them.
sampleDates ∷ [WorldDate]
sampleDates =
    [ WorldDate 1 1 1, WorldDate 1 12 30, WorldDate 7 6 15
    , WorldDate 1 14 40, WorldDate 1 0 0, WorldDate 0 0 0
    , WorldDate (-5) (-1) (-1), WorldDate 1 13 1, WorldDate 1 1 31
    , WorldDate 3 10 40, WorldDate 2 11 31
    ]

spec ∷ Spec
spec = do
    describe "worldDateToDayOfYear" $ do
        it "maps the first day of the year to 0" $
            doy 1 1 `shouldBe` 0
        it "advances within the first month" $
            doy 1 2 `shouldBe` 1
        it "accounts for whole months elapsed" $ do
            doy 3 1  `shouldBe` 60    -- two 30-day months elapsed
            doy 5 1  `shouldBe` 120
            doy 10 1 `shouldBe` 270
        it "maps the last day of the year to daysPerYear - 1" $
            doy 12 30 `shouldBe` daysPerYear - 1
        it "produces values past day 31 (the day-of-month ceiling)" $
            doy 4 15 `shouldSatisfy` (> 31)
        it "clamps out-of-range month/day instead of going negative or past year-end" $ do
            worldDateToDayOfYear defaultCalendarConfig (WorldDate 1 0 0)
                `shouldBe` 0
            worldDateToDayOfYear defaultCalendarConfig (WorldDate 1 99 99)
                `shouldBe` daysPerYear - 1

    describe "canonicalWorldDate" $ do
        it "leaves an in-range date exactly as it is" $ do
            let d = WorldDate 7 6 15
            canonicalWorldDate defaultCalendarConfig d `shouldBe` d
        it "leaves the two boundary dates of a year alone" $ do
            canonicalWorldDate defaultCalendarConfig (WorldDate 1 1 1)
                `shouldBe` WorldDate 1 1 1
            canonicalWorldDate defaultCalendarConfig (WorldDate 1 12 30)
                `shouldBe` WorldDate 1 12 30
        it "clamps a month and day above the calendar's ceilings" $
            canonicalWorldDate defaultCalendarConfig (WorldDate 1 14 40)
                `shouldBe` WorldDate 1 12 30
        it "clamps a zero or negative component up to 1, year included" $ do
            canonicalWorldDate defaultCalendarConfig (WorldDate 0 0 0)
                `shouldBe` WorldDate 1 1 1
            canonicalWorldDate defaultCalendarConfig (WorldDate (-5) (-1) (-1))
                `shouldBe` WorldDate 1 1 1
        it "moves only the components that are out of range" $ do
            -- Month over, day fine.
            canonicalWorldDate defaultCalendarConfig (WorldDate 3 13 7)
                `shouldBe` WorldDate 3 12 7
            -- Day over, month fine.
            canonicalWorldDate defaultCalendarConfig (WorldDate 3 4 31)
                `shouldBe` WorldDate 3 4 30
        it "takes its bounds from the calendar it is given, not the \
           \shipped default" $ do
            -- Under the odd calendar month 12 is out of range and day 40
            -- is legal; under the default the judgement is the opposite.
            canonicalWorldDate oddCalendar (WorldDate 1 12 40)
                `shouldBe` WorldDate 1 10 40
            canonicalWorldDate defaultCalendarConfig (WorldDate 1 12 40)
                `shouldBe` WorldDate 1 12 30
        it "keeps the ceilings at 1 for a degenerate calendar, so the \
           \clamp's own bounds never cross" $ do
            -- CalendarConfig comes from world-gen data and is not
            -- domain-validated, so zero or negative dimensions reach
            -- here. Both ordinal converters answer that with max 1.
            let degenerate = defaultCalendarConfig
                    { ccDaysPerMonth = 0, ccMonthsPerYear = -3 }
            canonicalWorldDate degenerate (WorldDate 2 9 9)
                `shouldBe` WorldDate 2 1 1
        it "is idempotent: a canonical date is already a fixed point" $
            forM_ [defaultCalendarConfig, oddCalendar] $ \cc →
                forM_ sampleDates $ \d → do
                    let once = canonicalWorldDate cc d
                    canonicalWorldDate cc once `shouldBe` once

    describe "the ordinal converters read the canonical form" $ do
        -- Requirement 4: one clamp definition, so what a stored date is
        -- repaired to and what a derived reading is computed from cannot
        -- drift apart. Both converters are pinned, because the checked
        -- one duplicated the clamps independently and supplies the
        -- midnight rollover.
        it "worldDateToDayOfYear agrees with the canonical date" $
            forM_ [defaultCalendarConfig, oddCalendar] $ \cc →
                forM_ sampleDates $ \d →
                    (d, worldDateToDayOfYear cc d)
                        `shouldBe` (d, worldDateToDayOfYear cc
                                          (canonicalWorldDate cc d))
        it "worldDateToDayOfYearChecked agrees with the canonical date" $
            forM_ [defaultCalendarConfig, oddCalendar] $ \cc →
                forM_ sampleDates $ \d →
                    (d, worldDateToDayOfYearChecked cc d)
                        `shouldBe` (d, worldDateToDayOfYearChecked cc
                                          (canonicalWorldDate cc d))
        it "a canonical date's ordinal stays inside the year" $
            forM_ [defaultCalendarConfig, oddCalendar] $ \cc →
                forM_ sampleDates $ \d → do
                    let n = worldDateToDayOfYear cc (canonicalWorldDate cc d)
                    (d, n ≥ 0 ∧ n < calendarDaysPerYear cc)
                        `shouldBe` (d, True)

    describe "findActiveCycleStage" $ do
        it "selects the dormant stage at the start of the year" $
            selectedTag (doy 1 1) `shouldBe` Just CycleDormant
        it "selects stages that begin after the first month" $ do
            selectedTag (doy 3 1)  `shouldBe` Just CycleBudding    -- day 60
            selectedTag 90         `shouldBe` Just CycleBudding
            selectedTag (doy 5 1)  `shouldBe` Just CycleFlowering  -- day 120
            selectedTag 300        `shouldBe` Just CycleSenescing
        it "wraps to the final stage for early-year days below all start days" $
            -- With a 0-day stage present this resolves to dormant; a
            -- cycle whose earliest start is > 0 must wrap to the last.
            (asTag <$> findActiveCycleStage
                [ AnnualStage CycleBudding   60  (TextureHandle 2)
                , AnnualStage CycleSenescing 270 (TextureHandle 4) ] 10)
                `shouldBe` Just CycleSenescing
        it "would never reach late stages if fed day-of-month (the bug)" $
            -- Any day-of-month (1..30) can only ever select dormant/budding,
            -- never flowering/senescing — which is exactly why the render
            -- site must convert through worldDateToDayOfYear.
            map selectedTag [1 .. 30]
                `shouldSatisfy` all (`elem` [Just CycleDormant, Just CycleBudding])

-- * Shared fixtures for the two engine specs

-- | The needle every clamp diagnostic carries, whichever ingress emitted
--   it. Taken from the production helpers rather than retyped, so a
--   reworded warning moves its own expectation.
clampNeedle ∷ Text
clampNeedle = "outside the page's calendar"

-- | Warnings that are clamp diagnostics, in emission order.
clampWarnings ∷ [LogEntry] → [LogEntry]
clampWarnings = filter $ \e →
    leLevel e ≡ LevelWarn ∧ clampNeedle `T.isInfixOf` leMessage e

-- | A logger whose entries are captured in emission order, so what each
--   ingress SAYS is observable rather than inferred.
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

-- | Assert exactly one clamp warning was emitted and that it names every
--   needle given.
expectOneClampWarning ∷ HasCallStack ⇒ [LogEntry] → [Text] → Expectation
expectOneClampWarning entries needles =
    case clampWarnings entries of
        [entry] → do
            leCategory entry `shouldBe` CatWorld
            forM_ needles $ \needle →
                (needle, leMessage entry)
                    `shouldSatisfy` \(n, m) → n `T.isInfixOf` m
        other → expectationFailure
            ("expected exactly one clamp warning, got "
             ⧺ show (map leMessage other))

-- | Gen params carrying the given calendar, shaped as an ARENA page
--   (seed 0 with the empty timeline) so load staging rebuilds flat
--   chunks instead of generating a world.
paramsWith ∷ CalendarConfig → WorldGenParams
paramsWith cc = defaultWorldGenParams { wgpSeed = 0, wgpCalender = cc }

-- * The setter spec

-- | The one page every setter example drives.
setterPage ∷ WorldPageId
setterPage = WorldPageId "calendar_date"

-- | The page's Lua-side name, spliced into every chunk.
setterPageLua ∷ BS.ByteString
setterPageLua = "'calendar_date'"

-- | Install a one-page session running the given calendar and hand back
--   the page's live 'WorldState', with the world queue drained so the
--   queue observation below starts from empty.
installPage ∷ EngineEnv → CalendarConfig → IO WorldState
installPage env cc = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just (paramsWith cc))
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(setterPage, ws)], wmVisible = [setterPage] }
    writeIORef (enginePausedRef env) True
    _ ← Q.flushQueue (worldQueue env)
    pure ws

-- | A Lua state carrying the full production API — the same registration
--   the real Lua thread performs, so @world.setDate@ and @world.getDate@
--   under test are the shipped bindings.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Everything @world.getDate@ reports, read back through Lua.
data ReportedDate = ReportedDate
    { rdDate       ∷ WorldDate
    , rdDayOfYear  ∷ Int
    , rdAbsoluteDay ∷ Int
    } deriving (Eq, Show)

-- | Call @world.getDate@ through the registered binding.
getDate ∷ HasCallStack ⇒ LuaBackendState → IO ReportedDate
getDate ls = do
    fields ← Lua.runWith (lbsLuaState ls) $ do
        _ ← Lua.dostring
            ("local d = world.getDate(" <> setterPageLua <> ")\n\
             \if d == nil then return nil end\n\
             \return d.year, d.month, d.day, d.dayOfYear, d.absoluteDay")
        read5 ← forM [-5, -4, -3, -2, -1] $ \i → do
            ty ← Lua.ltype i
            if ty ≡ Lua.TypeNumber then Lua.tointeger i else pure Nothing
        Lua.settop 0
        pure read5
    case fields of
        [Just y, Just mo, Just d, Just n, Just a] → pure ReportedDate
            { rdDate        = WorldDate (fromIntegral y) (fromIntegral mo)
                                        (fromIntegral d)
            , rdDayOfYear   = fromIntegral n
            , rdAbsoluteDay = fromIntegral a }
        other → do
            expectationFailure
                ("world.getDate returned " ⧺ show other)
            error "unreachable"

-- | Drive one @world.setDate@ end to end: the registered Lua binding
--   queues it, the queue is checked to carry the caller's RAW arguments
--   (the Lua door judges nothing — the handler is the boundary), and the
--   production handler applies it under a capturing logger.
callSetDate ∷ HasCallStack ⇒ EngineEnv → LuaBackendState → (Int, Int, Int)
            → IO [LogEntry]
callSetDate env ls (y, mo, d) = do
    let args = BS.pack (show y <> ", " <> show mo <> ", " <> show d)
    Lua.runWith (lbsLuaState ls) $ void $ Lua.dostring
        ("world.setDate(" <> setterPageLua <> ", " <> args <> ")")
    queued ← Q.flushQueue (worldQueue env)
    case queued of
        [WorldSetDate pid qy qmo qd] →
            (pid, qy, qmo, qd) `shouldBe` (setterPage, y, mo, d)
        other → expectationFailure
            ("expected one WorldSetDate, got " ⧺ show other)
    (logger, drain) ← capturingLogger
    handleWorldSetDateCommand (toWorldSimCapability env) logger
                              setterPage y mo d
    drain

-- | The canonical-storage half of the contract, for one setter call:
--   what @world.getDate@ reports, that its raw components are already
--   canonical, and that both derived fields agree with them.
expectStored ∷ HasCallStack ⇒ LuaBackendState → CalendarConfig → WorldDate
             → Expectation
expectStored ls cc expected = do
    reported ← getDate ls
    -- The stored components themselves. This is the assertion the old
    -- behaviour failed: it reported the caller's raw 14/40.
    rdDate reported `shouldBe` expected
    -- ...which is to say they are already in canonical form, so the
    -- midnight rollover has nothing to snap.
    canonicalWorldDate cc (rdDate reported) `shouldBe` rdDate reported
    -- ...and both derived readings are the ones that date implies.
    rdDayOfYear reported `shouldBe` worldDateToDayOfYear cc expected
    rdAbsoluteDay reported `shouldBe` worldAbsoluteDay cc expected

setterSpec ∷ SpecWith EngineEnv
setterSpec = describe "world.setDate canonical form" $ do

    it "clamps a month and day above the calendar's ceilings, and says so \
       \once" $ \env → do
        _ ← installPage env defaultCalendarConfig
        ls ← newBackend env
        entries ← callSetDate env ls (1, 14, 40)
        expectStored ls defaultCalendarConfig (WorldDate 1 12 30)
        expectOneClampWarning entries
            [ unWorldPageId setterPage, "1-14-40", "1-12-30" ]
        -- The whole line, so the diagnostic is pinned rather than
        -- probed by substring alone.
        map leMessage (clampWarnings entries) `shouldBe`
            [ setDateClampWarning setterPage (WorldDate 1 14 40)
                                             (WorldDate 1 12 30) ]

    it "clamps a zero year, month and day up to the world epoch" $ \env → do
        _ ← installPage env defaultCalendarConfig
        ls ← newBackend env
        entries ← callSetDate env ls (0, 0, 0)
        expectStored ls defaultCalendarConfig (WorldDate 1 1 1)
        expectOneClampWarning entries [unWorldPageId setterPage, "0-0-0"]

    it "stores an in-range date unchanged and says nothing about a clamp" $
      \env → do
        _ ← installPage env defaultCalendarConfig
        ls ← newBackend env
        entries ← callSetDate env ls (7, 6, 15)
        expectStored ls defaultCalendarConfig (WorldDate 7 6 15)
        map leMessage (clampWarnings entries) `shouldBe` []

    it "bounds the clamp by the PAGE's calendar, not the shipped default" $
      \env → do
        -- Month 12 is legal by default and out of range here; day 40 is
        -- out of range by default and legal here. Both judgements have
        -- to come from the page's own wgpCalender.
        _ ← installPage env oddCalendar
        ls ← newBackend env
        entries ← callSetDate env ls (1, 12, 40)
        expectStored ls oddCalendar (WorldDate 1 10 40)
        expectOneClampWarning entries ["1-12-40", "1-10-40"]

    it "stores a date the default calendar would have clamped, when the \
       \page's calendar allows it" $ \env → do
        _ ← installPage env oddCalendar
        ls ← newBackend env
        entries ← callSetDate env ls (2, 9, 40)
        expectStored ls oddCalendar (WorldDate 2 9 40)
        map leMessage (clampWarnings entries) `shouldBe` []

    describe "a midnight rollover after a clamped setDate" $ do

        it "advances the stored date by exactly one day, moving nothing \
           \else" $ \env → do
            -- Mid-month, so the single rolled day cannot carry: the
            -- month and year must come back untouched. Under the old
            -- behaviour the stored month was the caller's raw 99 and the
            -- rollover rewrote it to 12, which is the discontinuity
            -- requirement 2 forbids.
            _ ← installPage env defaultCalendarConfig
            ls ← newBackend env
            _ ← callSetDate env ls (4, 99, 15)
            before ← rdDate <$> getDate ls
            before `shouldBe` WorldDate 4 12 15
            rollOneMidnight env
            after ← rdDate <$> getDate ls
            after `shouldBe` WorldDate (wdYear before) (wdMonth before)
                                       (wdDay before + 1)

        it "carries into the next year exactly once from the last day" $
          \env → do
            _ ← installPage env defaultCalendarConfig
            ls ← newBackend env
            _ ← callSetDate env ls (4, 14, 40)
            before ← rdDate <$> getDate ls
            before `shouldBe` WorldDate 4 12 30
            rollOneMidnight env
            expectStored ls defaultCalendarConfig (WorldDate 5 1 1)

        it "carries under the page's own calendar" $ \env → do
            _ ← installPage env oddCalendar
            ls ← newBackend env
            _ ← callSetDate env ls (3, 12, 99)
            before ← rdDate <$> getDate ls
            before `shouldBe` WorldDate 3 10 40
            rollOneMidnight env
            expectStored ls oddCalendar (WorldDate 4 1 1)

-- | Cross exactly one midnight on the installed page, through the real
--   world-thread tick. 23:59 plus 60 game-minutes rolls one day and lands
--   at 00:59, so the advance is unambiguous.
rollOneMidnight ∷ EngineEnv → IO ()
rollOneMidnight env = do
    mgr ← readIORef (worldManagerRef env)
    case lookup setterPage (wmWorlds mgr) of
        Nothing → expectationFailure "the setter page is missing"
        Just ws → do
            writeIORef (wsTimeRef ws) (WorldTime 23 59)
            writeIORef (wsTimeScaleRef ws) 60
            writeIORef (enginePausedRef env) False
            tickWorldTime env 1
            writeIORef (enginePausedRef env) True
            readIORef (wsTimeRef ws) `shouldReturn` WorldTime 0 59

-- * The staging spec

-- | The page every staging example loads.
stagedPageId ∷ WorldPageId
stagedPageId = WorldPageId "calendar_date_staged"

-- | A one-page save carrying @cc@ as its calendar and @date@ as its
--   stored date, built the way a DECODED save is: 'blankPageSnapshot' is
--   the single construction every @world-pages@ version's own decoder
--   converges on, and 'snapshotToSaveData' is the adapter that turns such
--   a snapshot into the 'SaveData' staging consumes. Forging the date
--   HERE therefore reaches the staging write along the real route rather
--   than a test-only shortcut.
saveWith ∷ CalendarConfig → WorldDate → SaveData
saveWith cc (WorldDate y mo d) = snapshotToSaveData
    (SaveRequestMeta "calendar_slot" "2026-09-04T00:00:00.000000Z" False)
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
            (blankPageSnapshot stagedPageId (paramsWith cc))
                { pgsDateYear  = y
                , pgsDateMonth = mo
                , pgsDateDay   = d
                }
        }

-- | Stage one such save and hand back the staged page's OWN date — read
--   from the ref the staged world state publishes from — beside
--   everything the logger emitted.
stageWith ∷ HasCallStack ⇒ EngineEnv → CalendarConfig → WorldDate
          → IO (WorldDate, [LogEntry])
stageWith env cc date = do
    (logger, drain) ← capturingLogger
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger (saveWith cc date) matReg ⌦ either
        (\e → expectationFailure (T.unpack (renderStageError e))
                ≫ error "unreachable")
        pure
    entries ← drain
    case find ((≡ stagedPageId) . spPageId) (ssPages staged) of
        Nothing → expectationFailure "the staged page is missing"
                    ≫ error "unreachable"
        Just sp → do
            stagedDate ← readIORef (wsDateRef (spWorldState sp))
            pure (stagedDate, entries)

stagingSpec ∷ SpecWith EngineEnv
stagingSpec = describe "world date, through load staging" $ do

    it "clamps a restored month and day above the calendar's ceilings, \
       \and warns once" $ \env → do
        (staged, entries) ← stageWith env defaultCalendarConfig
                                      (WorldDate 6 14 40)
        staged `shouldBe` WorldDate 6 12 30
        expectOneClampWarning entries
            [ unWorldPageId stagedPageId, "6-14-40", "6-12-30" ]
        map leMessage (clampWarnings entries) `shouldBe`
            [ stagedWorldDateWarning stagedPageId (WorldDate 6 14 40)
                                                  (WorldDate 6 12 30) ]

    it "clamps a restored zero date up to the world epoch" $ \env → do
        (staged, entries) ← stageWith env defaultCalendarConfig
                                      (WorldDate 0 0 0)
        staged `shouldBe` WorldDate 1 1 1
        expectOneClampWarning entries [unWorldPageId stagedPageId, "0-0-0"]

    it "restores an in-range date unchanged and stays silent" $ \env → do
        (staged, entries) ← stageWith env defaultCalendarConfig
                                      (WorldDate 9 3 22)
        staged `shouldBe` WorldDate 9 3 22
        map leMessage (clampWarnings entries) `shouldBe` []

    it "bounds the restored date by the SAVE's own calendar" $ \env → do
        -- Month 12 is legal by default and out of range here; day 40 is
        -- out of range by default and legal here.
        (staged, entries) ← stageWith env oddCalendar (WorldDate 6 12 40)
        staged `shouldBe` WorldDate 6 10 40
        expectOneClampWarning entries ["6-12-40", "6-10-40"]

    it "restores a date the default calendar would have clamped, when \
       \the save's calendar allows it" $ \env → do
        (staged, entries) ← stageWith env oddCalendar (WorldDate 6 9 40)
        staged `shouldBe` WorldDate 6 9 40
        map leMessage (clampWarnings entries) `shouldBe` []
