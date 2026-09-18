-- | Sub-threshold resource-tick changes are never discarded (#2633).
--
--   @scripts\/unit_resource_tick.lua@ used to write a resource back only
--   when the change cleared @1e-4@ and kept no remainder for the write
--   it skipped. That made @1e-4@ per tick a minimum drain\/regen RATE
--   rather than a write-batching optimisation: at the shipped 0.1 s
--   cadence (@scripts\/init_loader.lua@) a red squirrel's sleep pressure
--   at endurance 0.3 moves 8.3e-5 per tick and so never moved at all,
--   while the SAME configuration integrated at dt = 1.0 depleted
--   normally. The outcome depended on how the update was partitioned
--   rather than on simulated time.
--
--   Removing the guard alone does not establish that equivalence,
--   because @unit.setStat@ stores binary32
--   (@uiStats :: HashMap Text Float@, "Unit.Types.Instance") and ten
--   times as many writes accumulate ten times as much rounding: over
--   1,800 game-seconds of constant drain on a pool of 3.0, unconditional
--   writes land on 1.4986802 at dt = 0.1 against 1.5000372 at dt = 1.0.
--   So @scripts\/unit_resource_carry.lua@ writes every tick AND keeps
--   the sub-binary32 remainder of each write in Lua doubles, and returns
--   what storage actually holds so the threshold checks in the same tick
--   judge the committed value.
--
--   Every example here drives the SHIPPED @unit_resource_tick.lua@ over
--   a REAL 'unitManagerRef' through the REAL registered @unit.*@ API, so
--   every value asserted is a real stored 'Float' and every collapse or
--   kill is a real command on @unitQueue@. The pool is the shipped
--   @red_squirrel.sleep_pressure@ config wherever the scenario is the
--   shipped one; §3 and §5 need rates no shipped resource declares, and
--   say so where they build them.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "resource tick sub-threshold carry"'@.
module Test.Headless.Unit.ResourceTickCarry (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- * Fixture

squirrelUid ∷ UnitId
squirrelUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "resource_carry_page"

-- | The one physiology cadence the shipped @init_loader.lua@ schedules,
--   and the coarser schedule the issue integrates the same duration
--   with.
fineDt, coarseDt ∷ Double
fineDt   = 0.1
coarseDt = 1.0

-- | @max_sleep_pressure@ is @endurance × 10@ (@scripts\/unit_stats.lua@),
--   so endurance 0.3 gives the pool the issue reproduces with. It is
--   written straight into @uiStats@ so @stats.get@ reads it verbatim
--   rather than through the derived formula, which keeps the arithmetic
--   these examples assert on exactly representable.
maxPool ∷ Float
maxPool = 3.0

endurance ∷ Float
endurance = 0.3

-- | @drain_constant_frac × max@ from the shipped
--   @red_squirrel.sleep_pressure@ entry: @(1\/3600) × 3.0@ per second,
--   which is 8.33e-5 per 0.1 s tick — below the old 1e-4 guard, so this
--   pool used to be frozen solid.
drainPerSec ∷ Double
drainPerSec = (1 / 3600) * realToFrac maxPool

-- | The pool empties in exactly one hour of game time at that rate,
--   which is the issue's own schedule.
fullDrainSeconds ∷ Double
fullDrainSeconds = 3600

-- | Tolerance for the schedule-equivalence comparison, scaled to the
--   ORIGINAL pool magnitude rather than to whatever the value has
--   decayed to: @1e-6 × 3.0@ = 3e-6. That is about 25 binary32 ulps at
--   1.5 — loose enough that a single rounding step cannot fail it, and
--   still more than 400× tighter than the 1.36e-3 divergence
--   unconditional per-tick writes produce at this rate.
scheduleTol ∷ Float
scheduleTol = maxPool * 1e-6

-- | The divergence unconditional (uncarried) binary32 writes produce
--   over 1,800 s at this rate, restated so the tolerance above is
--   anchored to a measured number rather than to taste.
uncarriedDivergence ∷ Float
uncarriedDivergence = 1.3569e-3

-- | One binary32 step at 1.5. §5 works a single ulp at a time, which is
--   four orders of magnitude below the change the old guard discarded.
ulpAt15 ∷ Float
ulpAt15 = 2 ** (-23)

mkSquirrel ∷ HM.HashMap Text Float → UnitInstance
mkSquirrel stats = UnitInstance
    { uiDefName = "red_squirrel", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionWildlife, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 1.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | The stats this tick reads: the pool, its maximum, and the endurance
--   every regen factor is scaled by.
statsWith ∷ Float → HM.HashMap Text Float
statsWith start = HM.fromList
    [ ("sleep_pressure", start), ("max_sleep_pressure", maxPool)
    , ("endurance", endurance) ]

-- | The pool §8 drives instead, because @checkSurvivalAlerts@ only
--   speaks @calories@ and @hydration@. 10.5 is exactly @0.25 × 42@, the
--   dehydration trigger, and both are exactly representable.
maxHydration, triggerHydration ∷ Float
maxHydration     = 42
triggerHydration = 10.5

hydrationStats ∷ HM.HashMap Text Float
hydrationStats = HM.fromList
    [ ("hydration", triggerHydration), ("max_hydration", maxHydration)
    , ("endurance", endurance) ]

-- | Install a one-unit scene and drain anything the previous example
--   left on the command queue.
resetSceneWith ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetSceneWith env stats = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "red_squirrel"
                       (minimalDef "red_squirrel" "Red Squirrel")
        , umInstances = HM.singleton squirrelUid (mkSquirrel stats) }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

resetScene ∷ EngineEnv → Float → IO ()
resetScene env start = resetSceneWith env (statsWith start)

-- * Live readers

storedStat ∷ EngineEnv → Text → IO (Maybe Float)
storedStat env name = do
    um ← readIORef (unitManagerRef env)
    pure (HM.lookup squirrelUid (umInstances um) ⌦ HM.lookup name ∘ uiStats)

-- | The stored value, or a failure naming the pool that went missing.
readStat ∷ EngineEnv → Text → IO Float
readStat env name = storedStat env name ⌦ \case
    Just v  → pure v
    Nothing → do
        expectationFailure $
            "the fixture's " <> T.unpack name <> " entry is gone"
        pure 0

readPressure ∷ EngineEnv → IO Float
readPressure env = readStat env "sleep_pressure"

-- | Debug-console returns are JSON-encoded, so a Lua string arrives
--   quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

shouldBeNear ∷ EngineEnv → Float → Float → Expectation
shouldBeNear env expected tol = do
    actual ← readPressure env
    if abs (actual - expected) < tol
        then pure ()
        else expectationFailure $
            "expected a stored sleep_pressure within " <> show tol
            <> " of " <> show expected <> ", found " <> show actual

-- * Lua plumbing

-- | The shipped modules, loaded for real. The assertion pins the two
--   configured facts every example below reasons from, so a retune of
--   the shipped squirrel shows up here rather than silently changing
--   what these numbers mean.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    loaded ← evalDebug ls $ T.concat
        [ "_G.__tick = require('scripts.unit_resource_tick'); "
        , "_G.__carry = require('scripts.unit_resource_carry'); "
        , "_G.__cfg = require('scripts.unit_resource_config')"
        , ".red_squirrel.sleep_pressure; "
        , "return _G.__cfg.max_from == 'max_sleep_pressure' "
        , "  and math.abs(_G.__cfg.drain_constant_frac - 1/3600) < 1e-12 "
        , "  and _G.__cfg.regen_factor_idle == nil" ]
    loaded `shouldBe` "true"
    pure ls

-- | A params table built from the shipped entry plus the extra fields a
--   scenario needs. @extras@ is Lua source for table fields.
paramsExpr ∷ Text → Text
paramsExpr extras = T.concat
    [ "(function() local p = {}; "
    , "for k, v in pairs(_G.__cfg) do p[k] = v end; "
    , extras
    , "return p end)()" ]

-- | @n@ ticks of the SHIPPED tickResource over the real unit.
tickResourceN ∷ LuaBackendState → Text → Text → Text → Text → Double → Int
              → IO ()
tickResourceN ls resource params activity pose dt n = do
    r ← evalDebug ls $ T.concat
        [ "local p = ", params, "; "
        , "for _ = 1, ", tshow n, " do "
        , "_G.__tick.tickResource(1, 'red_squirrel', '", resource, "', "
        , "p, '", activity, "', '", pose, "', ", tshow dt, ") end; "
        , "return true" ]
    r `shouldBe` "true"

tickN ∷ LuaBackendState → Text → Text → Text → Double → Int → IO ()
tickN ls = tickResourceN ls "sleep_pressure"

-- | The shipped drain, with nothing added.
shippedDrain ∷ Text
shippedDrain = paramsExpr ""

-- | What the tick asked the engine to do with this unit.
poseCommands ∷ EngineEnv → IO [Text]
poseCommands env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [ label | Just label ← map classify cmds ]
  where
    classify = \case
        UnitCollapse _ → Just "collapse"
        UnitKill _     → Just "kill"
        _              → Nothing

-- | Write the pool from OUTSIDE the tick, the way a drink, a debug
--   command or a restored snapshot does.
externalWrite ∷ LuaBackendState → Float → IO ()
externalWrite ls v = do
    r ← evalDebug ls $ T.concat
        [ "unit.setStat(1, 'sleep_pressure', ", tshow v, "); return true" ]
    r `shouldBe` "true"

-- | Record every @engine.emitEventForUnit@ call. The alerts module
--   resolves it off the global @engine@ table at call time, so this is
--   the real emission path, counted rather than suppressed.
captureEvents ∷ LuaBackendState → IO ()
captureEvents ls = do
    r ← evalDebug ls $ T.concat
        [ "_G.__events = {}; "
        , "engine.emitEventForUnit = function(cat, msg) "
        , "  _G.__events[#_G.__events + 1] = tostring(cat) .. '|' .. "
        , "    tostring(msg); return true "
        , "end; return true" ]
    r `shouldBe` "true"

-- | The captured events, joined, or @"-"@ when none were emitted.
emittedEvents ∷ LuaBackendState → IO Text
emittedEvents ls = evalDebug ls
    "return (#_G.__events == 0) and '-' or table.concat(_G.__events, ';')"

-- | Ticks needed to cover @seconds@ of simulated time at @dt@.
ticksFor ∷ Double → Double → Int
ticksFor seconds dt = round (seconds / dt)

-- | The analytic constant-rate value after @seconds@, before any clamp.
analytic ∷ Double → Float
analytic seconds = maxPool - realToFrac (drainPerSec * seconds)

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "resource tick sub-threshold carry" $ do

    -- §1 The defect itself: a small pool at the routine 0.1 s cadence,
    -- whose 8.33e-5 per-tick drain sits below the guard that used to
    -- decide whether anything was written at all.
    describe "a sub-threshold drain at the shipped 0.1 s cadence (§1)" $ do
        it "empties a 3.0 pool over a full simulated hour instead of \
           \leaving it at 3.000" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            tickN ls shippedDrain "idle" "standing" fineDt
                  (ticksFor fullDrainSeconds fineDt)
            shouldBeNear env 0 scheduleTol

        it "tracks the analytic value at an intermediate, unclamped \
           \duration — 1,800 s is 1.500, not 3.000" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            tickN ls shippedDrain "idle" "standing" fineDt
                  (ticksFor 1800 fineDt)
            shouldBeNear env (analytic 1800) scheduleTol

        it "moves on the FIRST tick, so nothing is waiting on a later \
           \write to be applied" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            tickN ls shippedDrain "idle" "standing" fineDt 1
            after ← readPressure env
            after `shouldSatisfy` (< maxPool)

    -- §2 Requirement 1: the same simulated duration, partitioned two
    -- ways, has to land in the same place.
    describe "equal-duration schedules agree (§2)" $ do
        it "1,800 s as 18,000 ticks of 0.1 s and as 1,800 ticks of 1.0 s \
           \agree to within 3e-6, where uncarried writes diverge by \
           \1.4e-3" $ \env → do
            resetScene env maxPool
            lsFine ← setupLua env
            tickN lsFine shippedDrain "idle" "standing" fineDt
                  (ticksFor 1800 fineDt)
            fine ← readPressure env

            resetScene env maxPool
            lsCoarse ← setupLua env
            tickN lsCoarse shippedDrain "idle" "standing" coarseDt
                  (ticksFor 1800 coarseDt)
            coarse ← readPressure env

            abs (fine - coarse) `shouldSatisfy` (< scheduleTol)
            abs (fine - analytic 1800) `shouldSatisfy` (< scheduleTol)
            abs (coarse - analytic 1800) `shouldSatisfy` (< scheduleTol)
            -- The tolerance is meaningful only because it is far below
            -- the error it exists to reject.
            scheduleTol `shouldSatisfy` (< uncarriedDivergence / 400)

        it "agrees at the 3,600 s endpoint too, where both schedules \
           \clamp at the floor" $ \env → do
            resetScene env maxPool
            lsFine ← setupLua env
            tickN lsFine shippedDrain "idle" "standing" fineDt
                  (ticksFor fullDrainSeconds fineDt)
            fine ← readPressure env

            resetScene env maxPool
            lsCoarse ← setupLua env
            tickN lsCoarse shippedDrain "idle" "standing" coarseDt
                  (ticksFor fullDrainSeconds coarseDt)
            coarse ← readPressure env

            abs (fine - coarse) `shouldSatisfy` (< scheduleTol)

    -- §3 The guard sat on `amount`, which is signed, so regeneration
    -- froze exactly as drain did. No shipped resource declares a regen
    -- this slow, so this builds one on the shipped entry.
    describe "sub-threshold REGENERATION accumulates too (§3)" $
        it "a 6e-6-per-tick regen raises a 1.0 pool to 1.108 over \
           \18,000 ticks instead of leaving it at 1.000" $ \env → do
            resetScene env 1.0
            ls ← setupLua env
            -- regen_factor_idle × endurance = 2e-4 × 0.3 = 6e-5 per
            -- second, 6e-6 per 0.1 s tick.
            let slowRegen = paramsExpr "p.drain_constant_frac = 0; \
                                       \p.regen_factor_idle = 2e-4; "
            tickN ls slowRegen "idle" "standing" fineDt 18000
            shouldBeNear env 1.108 scheduleTol

    -- §4 Requirement 3: a threshold decision that only happens because
    -- the sub-threshold drain accumulated.
    describe "threshold decisions follow the accumulated drain (§4)" $ do
        it "does not collapse while the committed value is still above \
           \half the pool" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            let collapsing = paramsExpr "p.collapse_threshold = 0.5; "
            tickN ls collapsing "idle" "standing" fineDt 17900
            poseCommands env `shouldReturn` []
            shouldBeNear env (analytic 1790) scheduleTol

        it "collapses once 200 more sub-threshold ticks have carried it \
           \across, which the old tick never reached" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            let collapsing = paramsExpr "p.collapse_threshold = 0.5; "
            tickN ls collapsing "idle" "standing" fineDt 17900
            _ ← Q.flushQueue (unitQueue env)
            tickN ls collapsing "idle" "standing" fineDt 200
            cmds ← poseCommands env
            cmds `shouldSatisfy` elem "collapse"

        it "kills on an empty pool reached entirely by sub-threshold \
           \drain" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            let lethal = paramsExpr "p.kill_on_zero = true; "
            tickN ls lethal "idle" "standing" fineDt
                  (ticksFor fullDrainSeconds fineDt)
            cmds ← poseCommands env
            cmds `shouldSatisfy` elem "kill"

    -- §5 Requirement 3's boundary: the decision must judge the binary32
    -- value storage COMMITTED, not the Lua double that produced it. The
    -- 3e-7-per-second drain here moves 3e-8 per tick — four orders below
    -- the guard the old tick applied, and below one binary32 step at
    -- 1.5, so the first tick's write genuinely cannot move storage.
    describe "the committed Float, not the Lua arithmetic, decides (§5)" $ do
        it "leaves storage on 1.5 when the intended value rounds back to \
           \it, and does not collapse on the double that was below" $
          \env → do
            resetScene env 1.5
            ls ← setupLua env
            let creeping = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 3e-7; \
                    \p.collapse_threshold = 0.5; "
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` 1.5
            poseCommands env `shouldReturn` []

        it "collapses on the very next tick, when the carried remainder \
           \finally moves storage one ulp below 1.5" $ \env → do
            resetScene env 1.5
            ls ← setupLua env
            let creeping = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 3e-7; \
                    \p.collapse_threshold = 0.5; "
            tickN ls creeping "idle" "standing" fineDt 2
            readPressure env `shouldReturn` (1.5 - ulpAt15)
            cmds ← poseCommands env
            cmds `shouldSatisfy` elem "collapse"

    -- §6 The remainder is the last write's residue and nothing more: it
    -- never replays over somebody else's value, and never crosses a
    -- load.
    describe "the remainder never outlives what it is a remainder of (§6)" $ do
        it "an intervening external write discards it — the next tick \
           \starts from the written value, not from it plus a stale \
           \residue" $ \env → do
            resetScene env 1.5
            ls ← setupLua env
            let creeping = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 3e-7; "
            -- One tick banks a -3e-8 residue against a stored 1.5.
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` 1.5
            -- Somebody else writes one ulp below. A tick that replayed
            -- the residue would total -6e-8 and round a further ulp
            -- down; one that discarded it rounds back to what was
            -- written.
            externalWrite ls (1.5 - ulpAt15)
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` (1.5 - ulpAt15)

        it "an external write of the SAME stored value discards it too — \
           \the write barrier sees the write the value comparison \
           \cannot" $ \env → do
            resetScene env 1.5
            ls ← setupLua env
            let creeping = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 3e-7; "
            -- One tick banks a -3e-8 residue against a stored 1.5.
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` 1.5
            -- Re-committing 1.5 moves nothing, so `stored == current`
            -- still holds and only the barrier can tell this happened.
            externalWrite ls 1.5
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` 1.5
            -- …and it is discarded, not merely deferred: the second
            -- tick after the write restarts the count rather than
            -- completing the pre-write one.
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` (1.5 - ulpAt15)

        it "covers every unit verb that can write a carried resource" $
          \env → do
            resetScene env 1.5
            ls ← setupLua env
            -- Nothing is left to install, and the declared set is the
            -- one this module reasons about. A verb dropped from it
            -- fails here rather than silently stopping invalidation.
            evalDebug ls "return #_G.__carry.writeBarrierStatus().missing"
                `shouldReturn` "0"
            evalDebug ls
                "return table.concat(_G.__carry.writeBarrierStatus()\
                \.declared, ',')"
                `shouldReturn` q "setStat,addXP,feed,recomputeBody,\
                                 \recoverStance"

        it "a load's reset hook discards it, so the same second tick no \
           \longer moves storage" $ \env → do
            resetScene env 1.5
            ls ← setupLua env
            let creeping = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 3e-7; "
            tickN ls creeping "idle" "standing" fineDt 1
            cleared ← evalDebug ls
                "_G.__carry.resetOnLoad(); return true"
            cleared `shouldBe` "true"
            tickN ls creeping "idle" "standing" fineDt 1
            readPressure env `shouldReturn` 1.5

    -- §7 Requirement 2 bounds the other way: a change the pool cannot
    -- absorb is discarded at the bound, never banked up to be repaid
    -- the moment the pool moves off it.
    describe "overflow at a bound is discarded, not banked (§7)" $ do
        it "100 ticks of drain against an empty pool do not delay the \
           \regeneration that follows" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            let hardDrain = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 1.0; "
            -- 30 ticks empty it; 100 more would bank -10.0 if the
            -- clamped overflow were kept.
            tickN ls hardDrain "idle" "standing" fineDt 130
            readPressure env `shouldReturn` 0
            let fastRegen = paramsExpr
                    "p.drain_constant_frac = 0; p.regen_factor_idle = 1.0; "
            tickN ls fastRegen "idle" "standing" fineDt 10
            shouldBeNear env 0.3 1e-4

        it "100 ticks of regen against a full pool do not delay the \
           \drain that follows" $ \env → do
            resetScene env maxPool
            ls ← setupLua env
            let fastRegen = paramsExpr
                    "p.drain_constant_frac = 0; p.regen_factor_idle = 1.0; "
            tickN ls fastRegen "idle" "standing" fineDt 100
            readPressure env `shouldReturn` maxPool
            let hardDrain = paramsExpr
                    "p.drain_constant_frac = 0; p.drain_constant = 1.0; "
            tickN ls hardDrain "idle" "standing" fineDt 10
            shouldBeNear env 2.0 1e-4

    -- §8 Requirement 3's other half: a survival WARNING is a decision
    -- in the same tick, and it too has to judge the committed Float.
    -- Only calories and hydration reach checkSurvivalAlerts, so this
    -- drives hydration, whose trigger is a plain fraction of the pool.
    describe "survival warnings judge the committed Float too (§8)" $ do
        it "does not warn while storage still holds exactly the 25% \
           \trigger, although the Lua arithmetic is already below it" $
          \env → do
            resetSceneWith env hydrationStats
            ls ← setupLua env
            captureEvents ls
            let creeping = paramsExpr
                    "p.max_from = 'max_hydration'; \
                    \p.drain_constant_frac = 0; p.drain_constant = 3e-7; "
            -- 15 ticks of 3e-8 stay inside half a binary32 step at
            -- 10.5, so every one of them rounds back to the trigger.
            tickResourceN ls "hydration" creeping "idle" "standing" fineDt 15
            readStat env "hydration" `shouldReturn` triggerHydration
            emittedEvents ls `shouldReturn` q "-"

        it "warns on the tick the carried remainder finally moves \
           \storage below the trigger" $ \env → do
            resetSceneWith env hydrationStats
            ls ← setupLua env
            captureEvents ls
            let creeping = paramsExpr
                    "p.max_from = 'max_hydration'; \
                    \p.drain_constant_frac = 0; p.drain_constant = 3e-7; "
            tickResourceN ls "hydration" creeping "idle" "standing" fineDt 16
            stored ← readStat env "hydration"
            stored `shouldSatisfy` (< triggerHydration)
            emittedEvents ls
                `shouldReturn` q "survival_warning|Red Squirrel is dehydrated"

        it "warns exactly once across the crossing, so the debounce \
           \still keys on the committed value" $ \env → do
            resetSceneWith env hydrationStats
            ls ← setupLua env
            captureEvents ls
            let creeping = paramsExpr
                    "p.max_from = 'max_hydration'; \
                    \p.drain_constant_frac = 0; p.drain_constant = 3e-7; "
            tickResourceN ls "hydration" creeping "idle" "standing" fineDt 60
            evalDebug ls "return #_G.__events" `shouldReturn` "1"

