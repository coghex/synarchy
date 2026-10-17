{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The sleep half of the "physiology exhaustion and sleep modes" gate
--   (#2835, S8b of the physiology stat-modes epic #2827): the sleep NEED as
--   the sleep system's effect, run through the SHIPPED
--   @scripts\/unit_ai_sleep.lua@ and the shipped facade, injury tick and
--   resource update over a live headless engine.
--
--   What is covered here:
--
--     * the wake predicate and the utility under each mode (a Nominal full
--       healthy read never pressure-wakes; a Simulated pool at 0.99 of its
--       maximum does; an explicit wake and a crossed species-local boundary
--       always do), with the first-crossing and midnight-wrap oracles;
--     * the cancellation of a started sequence, acceptance by acceptance: a
--       Simulated -> Nominal switch during walking-to-spot, lying down
--       (crouching and crawling), sleeping and waking leaves no phase, ends
--       the injury exemption, stands the unit up in a bounded number of
--       ticks and leaves it able to take a move order;
--     * a restored sequence under another mode, the durable sequence across
--       the load\/teardown resets, and the hook's scope (sleep only).
--
--   __The pose loop is closed by hand__ ('pump'): the pose verbs only
--   ENQUEUE a command, so the harness dispatches each to its REAL handler
--   and republishes the pose, exactly as "Test.Headless.Unit.SourceDrinkPose"
--   does; without it every pose assertion would be vacuous. The fixture
--   definition authors no animations, so a pose step resolves instantly,
--   which is why a separate case holds a hop IN FLIGHT.
module Test.Headless.Unit.PhysiologyExhaustionSleepModes.Sleep (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyModes.Harness hiding (usPose)
import Unit.Anim (poseTag)
import Unit.Command.Types (UnitCommand(..))
import Unit.Sim.Types
import Unit.Thread.Command.Motion (handleUnitMoveToCommand)
import Unit.Thread.Command.Pose
    ( handleUnitCollapseCommand, handleUnitCrawlCommand
    , handleUnitKillCommand, handleUnitReviveCommand
    , handleUnitTransitionToCommand )
import Unit.Types

spec ∷ Spec
spec = describe "the sleep need" $ do
    utilitySpec
    wakeSpec
    cancellationSpec
    restoreSpec
    scopeSpec
    registrationSpec

-----------------------------------------------------------------------
-- The scene and the pose loop
-----------------------------------------------------------------------

uid1 ∷ UnitId
uid1 = UnitId 1

-- | The rich-profile reference acolyte: sleep and exhaustion Simulated.
acolyte ∷ UnitSpec
acolyte = unitSpec 1 "acolyte" (Just "rich")

-- | The sim state the pose handlers mutate, planted where the unit is.
simState ∷ Pose → UnitSimState
simState pose = UnitSimState
    { usRealX = 10, usRealY = 10
    , usGridZ = 0, usRealZ = 0
    , usTarget = Nothing
    , usPose = pose, usState = Idle, usFacing = DirE
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 1
    , usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing
    , usClimbStartTime = Nothing, usClimbSlipAt = Nothing
    , usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0
    }

-- | The Lua the scene runs once: the sleep module, the injury tick, the
--   acolyte's tunables, a controllable sun and a flat dry world.
setupLua ∷ Text
setupLua = T.unlines
    [ "_G.__ai = require('scripts.unit_ai')"
    , "_G.__core = require('scripts.unit_ai_core')"
    , "_G.__sleep = require('scripts.unit_ai_sleep')"
    , "_G.__inj = require('scripts.unit_resource_injury')"
    , "_G.__params = require('scripts.unit_ai_tunables').acolyte"
    , "_G.__sun = 0.5"
    , "world.getSunAngleAt = function() return __sun end"
    , "world.getSlopeAt = function() return 0 end"
    , "world.getFluidAt = function() return nil end"
    , "__core.ensureState(1)"
    , "return __params.sleep_exhaustion_weight == 2.0"
    , "  and __params.sleep_deficit_weight == 9.0"
    , "  and __params.sleep_urge_weight == 4.0" ]

-- | A one-unit scene with its sim state, standing, and the setup run.
withSleeper ∷ UnitSpec → (Scene → IO a) → IO a
withSleeper sp act = withScene [sp] $ \sc@(env, ls) → do
    writeIORef (utsRef env) emptyUnitThreadState
        { utsSimStates = HM.singleton uid1 (simState Standing) }
    luaText ls setupLua `shouldReturn` "true"
    act sc

activityLabelOf ∷ UnitActivity → Text
activityLabelOf Idle                = "idle"
activityLabelOf Walking             = "walking"
activityLabelOf Running             = "running"
activityLabelOf Drinking            = "drinking"
activityLabelOf Eating              = "eating"
activityLabelOf Picking             = "pickup"
activityLabelOf (TransitioningTo _) = "transitioning"

-- | Drain @unitQueue@, dispatch each pose command to its real handler and
--   republish the poses (the three steps a running unit thread performs).
pump ∷ EngineEnv → IO [UnitCommand]
pump env = do
    cmds ← Q.flushQueue (unitQueue env)
    forM_ cmds $ \case
        UnitCollapse u          → handleUnitCollapseCommand (utsRef env) u
        UnitCrawl u             → handleUnitCrawlCommand (utsRef env) u
        UnitRevive u            → handleUnitReviveCommand (utsRef env) u
        UnitKill u              → handleUnitKillCommand env (utsRef env) u
        UnitTransitionTo u p st →
            handleUnitTransitionToCommand env (utsRef env) u p st
        UnitMoveTo u tx ty sp hz →
            handleUnitMoveToCommand env (utsRef env) u tx ty sp hz
        _                       → pure ()
    publishPoses env
    pure cmds

publishPoses ∷ EngineEnv → IO ()
publishPoses env = do
    uts ← readIORef (utsRef env)
    um  ← readIORef (unitManagerRef env)
    let apply uid inst = case HM.lookup uid (utsSimStates uts) of
            Nothing → inst
            Just ss → inst { uiPose     = poseTag (usPose ss)
                           , uiActivity = activityLabelOf (usState ss) }
    writeIORef (unitManagerRef env) um
        { umInstances = HM.mapWithKey apply (umInstances um) }

-- | Put the unit in a pose, idle, with nothing in flight.
setPose ∷ EngineEnv → Pose → IO ()
setPose env pose = adjustSim env $ \ss →
    ss { usPose = pose, usState = Idle, usTransitionUntil = Nothing }

adjustSim ∷ EngineEnv → (UnitSimState → UnitSimState) → IO ()
adjustSim env f = do
    modifyIORef' (utsRef env) $ \uts →
        uts { utsSimStates = HM.adjust f uid1 (utsSimStates uts) }
    publishPoses env

livePose ∷ EngineEnv → IO Text
livePose env = do
    uts ← readIORef (utsRef env)
    case HM.lookup uid1 (utsSimStates uts) of
        Just ss → pure (poseTag (usPose ss))
        Nothing → fail "the fixture unit has no sim state"

-- | The sequence fields the unit holds, as one token (@-@ when none).
sequenceOf ∷ LuaBackendState → IO Text
sequenceOf ls = luaText ls $ T.concat
    [ "local s = __ai.getState(1); local o = {}; "
    , "for _, f in ipairs({'sleepPhase', 'sleepSpot', 'sleepSession', "
    , "'sleepSpotPickedAt', 'sleepLastSunAngle', 'sleepWakeRequested'}) do "
    , "if s[f] ~= nil then o[#o + 1] = f end end; "
    , "return (#o == 0) and '-' or table.concat(o, ',')" ]

phaseOf ∷ LuaBackendState → IO Text
phaseOf ls = luaText ls "return tostring(__ai.getState(1).sleepPhase)"

-- | The sleep utility the shipped goal scores right now.
utilityNow ∷ LuaBackendState → IO Double
utilityNow ls = luaNum ls "__sleep.sleepUtility(1, __ai.getState(1), __params)"

-- | One AI think of the goal followed by one injury tick, each with its
--   commands dispatched, until the unit stands or @limit@ rounds pass.
--   Returns the rounds taken. The injury tick is what revives a healthy
--   crawler once the sequence's phase is gone.
settle ∷ EngineEnv → LuaBackendState → Int → IO Int
settle env ls limit = go 0
  where
    go n
        | n ≥ limit = pure n
        | otherwise = do
            pose ← livePose env
            if pose ≡ "standing" then pure n else do
                _ ← luaText ls "__inj.tickInjuries(1, unit.getInfo(1), \
                               \unit.getPose(1)); return true"
                _ ← pump env
                _ ← luaText ls "__sleep.sleepUtility(1, __ai.getState(1), \
                               \__params); return true"
                _ ← pump env
                go (n + 1)

-- | A real @unit.moveTo@ and whether Motion ACCEPTED it (it refuses a move
--   from anything but Standing or Crawling, so a stranded pose is a unit no
--   player order can reach).
moveAccepted ∷ EngineEnv → LuaBackendState → IO Bool
moveAccepted env ls = do
    adjustSim env (\ss → ss { usTarget = Nothing })
    luaText ls "return tostring(unit.moveTo(1, 14.5, 10.5, 1.0))"
        `shouldReturn` "true"
    _ ← pump env
    uts ← readIORef (utsRef env)
    pure $ case HM.lookup uid1 (utsSimStates uts) of
        Just ss → usTarget ss ≢ Nothing
        Nothing → False

-- | Start a sleeper: the phase, the pose and a wake baseline.
startSequence ∷ EngineEnv → LuaBackendState → Text → Pose → IO ()
startSequence env ls phase pose = do
    setPose env pose
    luaText ls ("local s = __ai.getState(1); s.sleepPhase = '" <> phase
                <> "'; s.sleepLastSunAngle = 0.5; return true")
        `shouldReturn` "true"

-- | An explicit mode through the facade.
setMode ∷ LuaBackendState → Text → Text → IO ()
setMode ls system mode = luaText ls
    ("return tostring(__physiology.setMode(1, '" <> system <> "', '" <> mode
     <> "'))") `shouldReturn` "true"

-----------------------------------------------------------------------
-- The utility
-----------------------------------------------------------------------

utilitySpec ∷ Spec
utilitySpec = describe "go_to_sleep utility" $ do
    it "is -infinity for a Nominal sleep system, whatever the circadian \
       \urge, and the same for an Unsupported one" $ do
        withSleeper acolyte { usModes = [("sleep", PhysioNominal)] } $ \(_, ls) → do
            _ ← luaText ls "__sun = 0.75; return true"   -- the dusk peak
            luaBool ls "__sleep.sleepUtility(1, __ai.getState(1), __params) \
                       \== -math.huge" `shouldReturn` True
        withScene [unitSpec 1 "white_tailed_deer" Nothing] $ \(_, ls) → do
            _ ← luaText ls setupLua
            luaBool ls "__sleep.sleepUtility(1, __ai.getState(1), __params) \
                       \== -math.huge" `shouldReturn` True
            luaText ls "return tostring(unit.getPhysioMode(1, 'sleep'))"
                `shouldReturn` "nil"

    it "keeps the Simulated formula: deficit, circadian urge and the \
       \exhaustion term, each at its own weight" $
        withSleeper acolyte { usStats = [("sleep_pressure", 5)] } $ \(_, ls) → do
            _ ← luaText ls "__sun = 0.75; return true"
            -- 9 * 0.5 + 4 * 1.0 + 2 * 0 (exhaustion full)
            utilityNow ls >>= near 1e-9 8.5
            -- a spent exhaustion pool adds the existing weighted deficit
            _ ← luaText ls "unit.setStat(1, 'exhaustion', 1.0); return true"
            utilityNow ls >>= near 1e-6 (8.5 + 2 * 0.9)
            -- below sleep_min_deficit nothing is considered
            _ ← luaText ls "unit.setStat(1, 'sleep_pressure', 9.0); return true"
            luaBool ls "__sleep.sleepUtility(1, __ai.getState(1), __params) \
                       \== -math.huge" `shouldReturn` True

    it "contributes no exhaustion term while exhaustion is Nominal: the \
       \utility equals a stub reporting a full healthy interface" $
        withSleeper acolyte { usStats = [("sleep_pressure", 5)]
                            , usModes = [("exhaustion", PhysioNominal)] } $
            \(_, ls) → do
                _ ← luaText ls "__sun = 0.75; return true"
                real ← utilityNow ls
                _ ← luaText ls $ T.concat
                    [ "local ex = require('scripts.exhaustion'); "
                    , "_G.__realFrac = ex.fraction; "
                    , "ex.fraction = function() return 1.0 end; return true" ]
                stub ← utilityNow ls
                _ ← luaText ls "require('scripts.exhaustion').fraction = \
                               \__realFrac; return true"
                abs (real - stub) `shouldSatisfy` (< 1e-9)
                real `shouldSatisfy` (> 0)

-----------------------------------------------------------------------
-- The wake predicate
-----------------------------------------------------------------------

-- | Advance a sleeper's execute @n@ times and report its phase.
executeN ∷ LuaBackendState → Int → IO Text
executeN ls n = do
    _ ← luaText ls $ T.concat
        [ "for _ = 1, ", tshow n, " do "
        , "__sleep.sleepExecute(1, __ai.getState(1), __params) end; "
        , "return true" ]
    phaseOf ls

wakeSpec ∷ Spec
wakeSpec = describe "the wake predicate" $ do
    it "never pressure-wakes a sleeper whose sleep is Nominal, although the \
       \healthy read is a full pool; explicit wake and a crossed boundary \
       \still wake it" $ do
        -- a synthetic sleeping state: no Nominal sleeper exists on an AI path
        -- (the cancellation removes it), so the predicate is driven directly
        let nominalSleeper = withSleeper acolyte
                { usModes = [("sleep", PhysioNominal)] }
        nominalSleeper $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            luaText ls "return __f(unit.readPhysio(1, 'sleep_pressure'))"
                `shouldReturn` "10"                    -- full and healthy
            executeN ls 5 `shouldReturn` "sleeping"    -- no pressure wake
        nominalSleeper $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            _ ← luaText ls "require('scripts.unit_ai').wakeUnit(1); return true"
            executeN ls 1 `shouldReturn` "waking"
        nominalSleeper $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            -- the acolyte's boundary is 0.25: 0.2 -> 0.3 crosses it
            _ ← luaText ls "__ai.getState(1).sleepLastSunAngle = 0.2; \
                           \__sun = 0.3; return true"
            executeN ls 1 `shouldReturn` "waking"

    it "pressure-wakes a Simulated pool at 0.99 of its maximum, and lets a \
       \depleted one sleep on" $ do
        withSleeper acolyte { usStats = [("sleep_pressure", 9.9)] } $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            executeN ls 1 `shouldReturn` "waking"
        withSleeper acolyte { usStats = [("sleep_pressure", 2)] } $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            executeN ls 5 `shouldReturn` "sleeping"
            _ ← luaText ls "require('scripts.unit_ai').wakeUnit(1); return true"
            executeN ls 1 `shouldReturn` "waking"

    it "keeps the seeded first-crossing baseline and the midnight-wrap \
       \arithmetic" $
        withSleeper acolyte { usStats = [("sleep_pressure", 2)] } $ \(env, ls) → do
            -- the baseline is the sun angle AT the moment sleep begins: a
            -- boundary swept in the next gap is not lost
            setPose env Sleeping
            _ ← luaText ls "__sun = 0.24; \
                           \__ai.getState(1).sleepPhase = 'lying_down'; \
                           \return true"
            executeN ls 1 `shouldReturn` "sleeping"
            luaNum ls "__ai.getState(1).sleepLastSunAngle" >>= near 1e-12 0.24
            _ ← luaText ls "__sun = 0.26; return true"
            executeN ls 1 `shouldReturn` "waking"
            -- across the 1.0 -> 0.0 wrap: 0.9 -> 0.3 sweeps 0.25, 0.9 -> 0.2 does not
            startSequence env ls "sleeping" Sleeping
            _ ← luaText ls "__ai.getState(1).sleepLastSunAngle = 0.9; \
                           \__sun = 0.2; return true"
            executeN ls 1 `shouldReturn` "sleeping"
            startSequence env ls "sleeping" Sleeping
            _ ← luaText ls "__ai.getState(1).sleepLastSunAngle = 0.9; \
                           \__sun = 0.3; return true"
            executeN ls 1 `shouldReturn` "waking"

    it "keeps each species' own circadian phase and wake boundary" $
        withSleeper acolyte $ \(_, ls) → do
            luaText ls "local c = require('scripts.circadian'); \
                       \local a, aw = c.shapeFor('acolyte'); \
                       \local b, bw = c.shapeFor('bear_brown'); \
                       \return table.concat({ a, aw, b, bw, \
                       \__sleep.wakeAngleFor('acolyte'), \
                       \__sleep.wakeAngleFor('bear_brown') }, ',')"
                `shouldReturn` "0.75,0.125,0.25,0.125,0.25,0.75"

-----------------------------------------------------------------------
-- Cancelling the sequence
-----------------------------------------------------------------------

-- | One point of the sequence a mode change can land on.
data Point = Point
    { ptName  ∷ Text
    , ptPhase ∷ Maybe Text    -- ^ the AI phase, if any
    , ptPose  ∷ Pose
    , ptSpot  ∷ Bool          -- ^ a picked sleep spot (walking to it)
    }

points ∷ [Point]
points =
    [ Point "walking to the spot"     Nothing           Standing  True
    , Point "lying down (crouching)"  (Just "lying_down") Crouching False
    , Point "lying down (crawling)"   (Just "lying_down") Crawling  False
    , Point "sleeping"                (Just "sleeping")   Sleeping  False
    , Point "waking (still asleep)"   (Just "waking")     Sleeping  False
    , Point "waking (crawling)"       (Just "waking")     Crawling  False ]

cancellationSpec ∷ Spec
cancellationSpec = describe "a Simulated -> Nominal sleep switch cancels the sequence" $ do
    forM_ points $ \pt →
        it ("during " <> T.unpack (ptName pt) <> ": no phase, no exemption, \
            \standing within a bounded number of ticks, a later move order honoured") $
            withSleeper acolyte $ \(env, ls) → do
                setPose env (ptPose pt)
                forM_ (ptPhase pt) $ \ph →
                    luaText ls ("__ai.getState(1).sleepPhase = '" <> ph
                                <> "'; __ai.getState(1).sleepLastSunAngle = 0.5; \
                                \return true") `shouldReturn` "true"
                when (ptSpot pt) $
                    luaText ls "local s = __ai.getState(1); \
                               \s.sleepSpot = { x = 12, y = 10 }; \
                               \s.sleepSession = 1; return true"
                        `shouldReturn` "true"
                sequenceOf ls `shouldNotReturn` "-"
                -- the switch, through the facade (engine transition, hooks)
                setMode ls "sleep" "nominal"
                -- every sequence field is gone at once, so the injury tick's
                -- crawling exemption (it keys on the phase alone) is over
                sequenceOf ls `shouldReturn` "-"
                rounds ← settle env ls 12
                livePose env `shouldReturn` "standing"
                rounds `shouldSatisfy` (< 12)
                moveAccepted env ls `shouldReturn` True

    it "does not strand a unit when the switch lands while a pose hop is in \
       \flight: it waits out the hop, then stands the sleeper up" $
        withSleeper acolyte $ \(env, ls) → do
            -- crawling -> sleeping in flight: getPose still says crawling
            startSequence env ls "lying_down" Crawling
            adjustSim env $ \ss → ss { usState = TransitioningTo Sleeping
                                     , usTransitionUntil = Just 1.0e9 }
            luaText ls "return unit.getActivity(1)" `shouldReturn` "transitioning"
            setMode ls "sleep" "nominal"
            sequenceOf ls `shouldReturn` "-"
            -- the hop lands: the unit is now asleep with NO sequence
            adjustSim env $ \ss → ss { usPose = Sleeping, usState = Idle
                                     , usTransitionUntil = Nothing }
            livePose env `shouldReturn` "sleeping"
            rounds ← settle env ls 12
            livePose env `shouldReturn` "standing"
            rounds `shouldSatisfy` (< 12)

    it "holds a started crawler down while the phase is set (the exemption \
       \this cancellation ends), so the cases above are not vacuous" $
        withSleeper acolyte $ \(env, ls) → do
            startSequence env ls "lying_down" Crawling
            forM_ [1 ∷ Int .. 5] $ \_ → do
                _ ← luaText ls "__inj.tickInjuries(1, unit.getInfo(1), \
                               \unit.getPose(1)); return true"
                pump env
            livePose env `shouldReturn` "crawling"

    it "does not touch a crouching unit that held no sequence" $
        withSleeper acolyte $ \(env, ls) → do
            setPose env Crouching                -- e.g. drinking at a bank
            setMode ls "sleep" "nominal"
            _ ← pump env
            livePose env `shouldReturn` "crouching"

-----------------------------------------------------------------------
-- A restored sequence and the lifecycle resets
-----------------------------------------------------------------------

restoreSpec ∷ Spec
restoreSpec = describe "a restored sequence" $ do
    it "is abandoned, not executed, when the restored sleep mode is not \
       \Simulated" $
        withSleeper acolyte { usModes = [("sleep", PhysioNominal)] } $ \(env, ls) → do
            -- what a save made under a Simulated profile looks like to a
            -- build whose default for this species is Nominal
            startSequence env ls "sleeping" Sleeping
            luaBool ls "__sleep.sleepUtility(1, __ai.getState(1), __params) \
                       \== -math.huge" `shouldReturn` True
            sequenceOf ls `shouldReturn` "-"
            _ ← settle env ls 12
            livePose env `shouldReturn` "standing"

    it "survives the load and teardown resets for a Simulated sleeper: only \
       \carry remainders, alert debounce and session caches reset" $
        withSleeper acolyte $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            _ ← luaText ls "__ai.getState(1).sleepSpot = { x = 12, y = 10 }; \
                           \return true"
            before ← sequenceOf ls
            _ ← luaText ls "require('scripts.lib.save_modules')\
                           \.resetHooks['unit_resources'](); return true"
            _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"
            sequenceOf ls `shouldReturn` before
            -- and the save snapshot of the AI state keeps the sequence
            luaText ls "local c = require('scripts.unit_ai_save_transient')\
                       \.snapshotUnitState(__ai.getState(1)); \
                       \return tostring(c.sleepPhase) .. '|' .. \
                       \tostring(c.sleepLastSunAngle) .. '|' .. \
                       \tostring(c.sleepSpot ~= nil)"
                `shouldReturn` "sleeping|0.5|true"
            phaseOf ls `shouldReturn` "sleeping"
            livePose env `shouldReturn` "sleeping"

-----------------------------------------------------------------------
-- Scope
-----------------------------------------------------------------------

scopeSpec ∷ Spec
scopeSpec = describe "the cancellation hook belongs to the sleep system" $
    it "an exhaustion mode change never interrupts a Simulated sleep sequence, \
       \in either direction" $
        withSleeper acolyte $ \(env, ls) → do
            startSequence env ls "sleeping" Sleeping
            setMode ls "exhaustion" "nominal"
            phaseOf ls `shouldReturn` "sleeping"
            setMode ls "exhaustion" "simulated"
            phaseOf ls `shouldReturn` "sleeping"
            livePose env `shouldReturn` "sleeping"
            -- a sleep change that is not an effective change is no event
            setMode ls "sleep" "simulated"
            phaseOf ls `shouldReturn` "sleeping"

-----------------------------------------------------------------------
-- Registration
-----------------------------------------------------------------------

registrationSpec ∷ Spec
registrationSpec = describe "behavior registration" $
    it "registers go_to_sleep for acolytes and bears and never for the \
       \squirrel (its sleep system is Nominal, so it has no sleep need)" $
        withScene [] $ \(_, ls) →
            luaText ls "require('scripts.red_squirrel_ai'); \
                       \require('scripts.bear_ai'); \
                       \local by = require('scripts.unit_ai_actions').byDef; \
                       \return tostring(by.acolyte.go_to_sleep) .. '|' .. \
                       \tostring(by.bear_brown.go_to_sleep) .. '|' .. \
                       \tostring(by.red_squirrel.go_to_sleep)"
                `shouldReturn` "true|true|nil"
