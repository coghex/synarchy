-- | Frame-aware organ-failure eligibility (#2556).
--
--   Body seeding ('Unit.Thread.Command.Body.seedBodyComposition') and
--   Lua catabolism (@scripts\/unit_resource_energy.lua@) both floor a
--   unit's fat reserves at @0.02 × frame_mass@, where
--   @frame_mass = 22·h²·bulk@ — a size-correct floor that works for a
--   mouse or a dragon. The organ-failure branch of
--   @scripts\/unit_resource_tick.lua@ kept the older height-only
--   @0.44·h²@, which the frame form only reproduces at @bulk@ exactly
--   1.0. @data\/units\/acolyte.yaml@ ships @bulk@ 0.5..1.5, so the two
--   disagreed for ordinary acolytes in both directions:
--
--     * a SLIM one (bulk 0.5) sat in permanent organ failure while fed
--       and well above its own floor — stamina bleeding at the
--       organ-failure rate, locomotion drain and the caffeine bonus
--       both switched off;
--     * a BULKY one (bulk 1.5) whose fat was actually spent, clamped
--       to its own floor by starvation, kept regenerating normally.
--
--   These specs drive the SHIPPED @scripts\/unit_resource_tick.lua@,
--   @unit_resource_energy.lua@ and @unit_resource_config.lua@ through
--   the REAL registered @unit.*@ API over a REAL 'unitManagerRef', and
--   read stamina back out of stored @uiStats@. Since #2470
--   @unit.commitStamina@ is the only stamina write path and resolves
--   the whole update engine-side, so a fixture that stubbed
--   @unit.setStat@ could not observe stamina at all.
--
--   Every body profile comes from the PRODUCTION seeder: the rolled
--   map goes through 'seedBodyComposition' exactly as spawn does, and
--   the expected floors are restated from the formula rather than read
--   back out of it, so a silent change to either side shows up here.
--   @uiStats@ is @HashMap Text Float@, so those stored values reach
--   Lua through the Float32 → Float64 round-trip that
--   @energy.FAT_FLOOR_TOL@ exists for; §5 pins both sides of that
--   tolerance band.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "frame-aware organ failure"'@.
module Test.Headless.Unit.FrameOrganFailure (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Thread.Command.Body (seedBodyComposition)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- * Fixture constants

acolyteUid ∷ UnitId
acolyteUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "frame_organ_failure_page"

-- | The one physiology tick the shipped @init_loader.lua@ schedules.
tickDt ∷ Float
tickDt = 0.1

-- | Every profile is rolled at the acolyte's MEAN height, so bulk is
--   the only thing separating them and the height-only floor is
--   identical across all three.
profileHeight ∷ Float
profileHeight = 1.8

-- | Acolyte @bodyfat@ mean. @data\/units\/acolyte.yaml@ ships
--   @{ mean: 0.2, range: 0.36 }@ → 0.02..0.38.
meanBodyfat ∷ Float
meanBodyfat = 0.2

-- | The leanest shipped roll — the bottom of that same range. A fresh
--   acolyte this lean is seeded at the spawn MARGIN above its floor
--   (@spawnFatMargin@ 1.7 × the floor), which is where the height-only
--   formula misjudged a perfectly healthy slim unit.
leanBodyfat ∷ Float
leanBodyfat = 0.02

-- | Stamina is started mid-pool: far enough above @collapse_threshold@
--   (0.1 × 10 = 1.0) and @kill_on_zero@ that no single-tick example
--   trips collapse or death, and far enough below @max_stamina@ that a
--   recovering tick is not swallowed by the clamp.
startStamina ∷ Float
startStamina = 5.0

-- | @seedBodyComposition@'s frame: @22 · h² · bulk@. Restated from the
--   formula, not read back from the seeded map.
frameMassAt ∷ Float → Float
frameMassAt bulk = 22 * profileHeight * profileHeight * bulk

-- | The frame-proportional fat floor: @minFatFrac@ (0.02) × frame.
frameFloorAt ∷ Float → Float
frameFloorAt bulk = 0.02 * frameMassAt bulk

-- | The legacy height-only floor the organ-failure check used to apply
--   to every unit regardless of bulk: @0.44 · h²@ = 1.4256 kg here.
legacyFloor ∷ Float
legacyFloor = 0.44 * profileHeight * profileHeight

-- | A fresh roll's fat when the spawn margin governs: @spawnFatMargin@
--   (1.7) × the floor. This is what a @leanBodyfat@ profile seeds at,
--   since @bodyfat × frame@ is below it.
spawnMarginFatAt ∷ Float → Float
spawnMarginFatAt bulk = 1.7 * frameFloorAt bulk

-- * Expected stamina movement, from the SHIPPED acolyte config

-- | @regen_factor_idle@ (0.5) × @endurance@ (1) × dt. Ordinary idle
--   recovery — what a unit NOT in organ failure gains.
idleGain ∷ Float
idleGain = 0.5 * 1.0 * tickDt                    -- +0.05

-- | @move_regen_factor@ (0.5) × @endurance@ (1) × dt. On the
--   @speed_drain@ path the unit recovers at its aerobic supply and
--   drains @supply × (effort\/comfort)²@; the headless fixture reports
--   @moveSpeed@ 0, so effort is 0 and the net is the supply alone.
movingGain ∷ Float
movingGain = 0.5 * 1.0 * tickDt                  -- +0.05

-- | @ORGAN_FAILURE_DRAIN_PER_SEC@ (0.5) × dt, with regen overridden to
--   0. What an IDLE unit in organ failure loses.
organFailureIdleLoss ∷ Float
organFailureIdleLoss = 0.5 * tickDt              -- -0.05

-- | Organ failure while WALKING also loses the flat @drain_walking@
--   (0.1): the @speed_drain@ branch is gated on @not inOrganFailure@,
--   so a failing unit falls back to the flat activity drain on top of
--   the organ-failure drain.
organFailureWalkingLoss ∷ Float
organFailureWalkingLoss = (0.5 + 0.1) * tickDt   -- -0.06

-- * Scene

-- | The rolled stat map a spawning acolyte carries INTO
--   'seedBodyComposition': the body block plus the attributes the
--   stamina tick reads.
rolledStats ∷ Float → Float → HM.HashMap Text Float
rolledStats bulk bodyfat = HM.fromList
    [ ("height", profileHeight), ("bulk", bulk), ("bodyfat", bodyfat)
    , ("strength", 1.0), ("endurance", 1.0)
    , ("stamina", startStamina) ]

-- | A body profile as the PRODUCTION seeder leaves it. @bulk@ and
--   @bodyfat@ are consumed and dropped; @frame_mass@, @body_mass@,
--   @lean_mass@, @fat_mass@ and the derived stats are written.
seededStats ∷ Float → Float → HM.HashMap Text Float
seededStats bulk bodyfat = seedBodyComposition (rolledStats bulk bodyfat)

-- | The same profile with its fat overwritten — what starvation leaves
--   behind once catabolism has clamped the unit at (or near) its floor.
seededWithFat ∷ Float → Float → Float → HM.HashMap Text Float
seededWithFat bulk bodyfat fat =
    HM.insert "fat_mass" fat (seededStats bulk bodyfat)

mkUnit ∷ Text → HM.HashMap Text Float → UnitInstance
mkUnit defName stats = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Install a one-unit scene under an arbitrary shipped def name.
resetSceneAs ∷ EngineEnv → Text → HM.HashMap Text Float → IO ()
resetSceneAs env defName stats = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton defName (minimalDef defName defName)
        , umInstances = HM.singleton acolyteUid (mkUnit defName stats) }

resetScene ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetScene env = resetSceneAs env "acolyte"

-- * Live readers

storedStat ∷ EngineEnv → Text → IO (Maybe Float)
storedStat env name = do
    um ← readIORef (unitManagerRef env)
    pure (HM.lookup acolyteUid (umInstances um) ⌦ HM.lookup name ∘ uiStats)

storedStamina ∷ EngineEnv → IO (Maybe Float)
storedStamina env = storedStat env "stamina"

-- | Float stored values carry a rounding step, so every comparison
--   allows 1e-4 — two orders of magnitude tighter than the 0.05 that
--   separates the recovering and failing outcomes, and tight enough
--   that §5's 0.5e-4\/2e-4 tolerance probes stay distinguishable
--   through their own separate at-floor assertions.
tol ∷ Float
tol = 1e-4

shouldBeNear ∷ IO (Maybe Float) → Float → Expectation
shouldBeNear act expected = act ⌦ \case
    Nothing → expectationFailure $
        "expected a stored value near " <> show expected
        <> ", found no entry"
    Just actual
        | abs (actual - expected) < tol → pure ()
        | otherwise → expectationFailure $
            "expected a stored value near " <> show expected
            <> ", found " <> show actual

-- * Lua plumbing

-- | The shipped modules, loaded for real. Nothing is stubbed: organ
--   failure reads only unit stats, so there is no world call to
--   control.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    loaded ← evalDebug ls $ T.concat
        [ "_G.__tick = require('scripts.unit_resource_tick'); "
        , "_G.__energy = require('scripts.unit_resource_energy'); "
        , "_G.__cfg = require('scripts.unit_resource_config'); "
        , "return _G.__cfg.acolyte.stamina.organ_failure_check == true "
        , "  and _G.__cfg.bear_brown.stamina.organ_failure_check == nil "
        , "  and _G.__energy.FAT_FLOOR_TOL == 1e-4 "
        , "  and type(_G.__energy.minFatFor) == 'function'" ]
    loaded `shouldBe` "true"
    pure ls

-- | @n@ stamina ticks of the SHIPPED tickResource with the SHIPPED
--   config table for @defName@.
tickStaminaAs ∷ LuaBackendState → Text → Text → Int → IO ()
tickStaminaAs ls defName activity n = do
    r ← evalDebug ls $ T.concat
        [ "for _ = 1, ", tshow n, " do "
        , "_G.__tick.tickResource(1, '", defName, "', 'stamina', "
        , "_G.__cfg.", defName, ".stamina, '", activity, "', 'standing', "
        , tshow (realToFrac tickDt ∷ Double), ") end; return true" ]
    r `shouldBe` "true"

-- | The ordinary case: an idle acolyte.
tickStamina ∷ LuaBackendState → Int → IO ()
tickStamina ls = tickStaminaAs ls "acolyte" "idle"

-- | One tick of the SHIPPED catabolism, which is the OTHER consumer of
--   the shared fat floor.
tickStarvation ∷ LuaBackendState → IO ()
tickStarvation ls = do
    r ← evalDebug ls $ T.concat
        [ "_G.__energy.tickStarvation(1, "
        , tshow (realToFrac tickDt ∷ Double), "); return true" ]
    r `shouldBe` "true"

-- | @math.abs(expr - value) < 1e-4@: the stats are 'Float' and Lua
--   arithmetic is 'Double', so a Lua literal never compares equal to
--   what the manager holds.
luaNear ∷ Text → Float → Text
luaNear expr v = T.concat
    [ "math.abs((", expr, ") - ", tshow (realToFrac v ∷ Double), ") < 1e-4" ]

-- | Delete stats from a seeded map — how §6 reaches a unit with one
--   floor input, the other, or neither.
without ∷ [Text] → HM.HashMap Text Float → HM.HashMap Text Float
without names stats = foldr HM.delete stats names

-- | The two outcomes every example distinguishes, asserted on STORED
--   stamina rather than on a threshold helper.
recovers ∷ EngineEnv → LuaBackendState → Expectation
recovers env ls = do
    tickStamina ls 1
    storedStamina env `shouldBeNear` (startStamina + idleGain)

fails ∷ EngineEnv → LuaBackendState → Expectation
fails env ls = do
    tickStamina ls 1
    storedStamina env `shouldBeNear` (startStamina - organFailureIdleLoss)

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "frame-aware organ failure" $ do

    -- §0 The fixture's own premises. If the seeder stops producing
    -- these numbers, every expectation below is measuring something
    -- else, so they are asserted rather than assumed.
    describe "the production seeder's own numbers (§0)" $ do
        it "seeds frame_mass = 22·h²·bulk for each supported profile, \
           \and drops the spawn-time inputs" $ \env → do
            forM_ [0.5, 1.0, 1.5] $ \bulk → do
                resetScene env (seededStats bulk meanBodyfat)
                storedStat env "frame_mass" `shouldBeNear` frameMassAt bulk
                storedStat env "bulk"    `shouldReturn` Nothing
                storedStat env "bodyfat" `shouldReturn` Nothing

        it "seeds the leanest shipped roll at the spawn margin — 1.7× \
           \its own floor, which for bulk 0.5 is the 1.21176 kg the \
           \issue reproduced" $ \env → do
            resetScene env (seededStats 0.5 leanBodyfat)
            storedStat env "fat_mass" `shouldBeNear` spawnMarginFatAt 0.5

        it "the two floors disagree exactly where bulk ≢ 1.0: slim and \
           \bulky straddle the height-only 1.4256 kg from opposite \
           \sides, and bulk 1.0 reproduces it" $ \_ → do
            frameFloorAt 0.5 `shouldSatisfy` (< legacyFloor)
            frameFloorAt 1.5 `shouldSatisfy` (> legacyFloor)
            abs (frameFloorAt 1.0 - legacyFloor) `shouldSatisfy` (< tol)

    -- §1 The headline defect: a fed, freshly seeded SLIM acolyte,
    -- above its own floor but below the height-only one.
    describe "a fresh slim acolyte, fed and above its own floor (§1)" $ do
        it "recovers at the ordinary idle rate instead of bleeding at \
           \the organ-failure rate: 5.000 → 5.050, not 4.950" $
          \env → do
            resetScene env (seededStats 0.5 leanBodyfat)
            ls ← setupLua env
            recovers env ls

        it "is genuinely between the two floors, so the example is \
           \about the formula and not about a fat value that clears \
           \both" $ \env → do
            resetScene env (seededStats 0.5 leanBodyfat)
            ls ← setupLua env
            evalDebug ls (T.concat
                [ "local fat = unit.getStat(1, 'fat_mass'); "
                , "return fat > _G.__energy.minFatFor(1) "
                , "   and fat <= 0.44 * 1.8 * 1.8" ])
                `shouldReturn` "true"

        it "keeps recovering over 40 ticks rather than draining toward \
           \collapse" $ \env → do
            resetScene env (seededStats 0.5 leanBodyfat)
            ls ← setupLua env
            tickStamina ls 40
            storedStamina env `shouldBeNear`
                (startStamina + idleGain * 40)

        it "WALKING, it takes the speed_drain path again — +0.05 at a \
           \reported speed of 0, not the flat fallback and not the \
           \-0.06 organ-failure drain" $ \env → do
            resetScene env (seededStats 0.5 leanBodyfat)
            ls ← setupLua env
            -- The fixture reports moveSpeed 0, so the branch is
            -- identifiable by its result: only the speed_drain path
            -- nets the full aerobic supply.
            evalDebug ls "return (unit.getInfo(1) or {}).moveSpeed == 0"
                `shouldReturn` "true"
            tickStaminaAs ls "acolyte" "walking" 1
            storedStamina env `shouldBeNear` (startStamina + movingGain)
            (startStamina + movingGain) `shouldSatisfy`
                (\v → abs (v - (startStamina - organFailureWalkingLoss))
                        > tol)

        it "at the acolyte's mean bodyfat it also recovers — the slim \
           \profile is not a special case of one lean roll" $ \env → do
            resetScene env (seededStats 0.5 meanBodyfat)
            ls ← setupLua env
            recovers env ls

    -- §2 The other direction: a BULKY acolyte whose reserves really
    -- are gone stayed above the height-only floor and kept recovering.
    describe "a bulky acolyte clamped at its own floor (§2)" $ do
        it "enters organ failure and drains: 5.000 → 4.950, even though \
           \its 2.1384 kg fat is well above the height-only 1.4256 kg" $
          \env → do
            resetScene env
                (seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5))
            ls ← setupLua env
            fails env ls

        it "a fresh bulky acolyte is unaffected and recovers normally" $
          \env → do
            resetScene env (seededStats 1.5 meanBodyfat)
            ls ← setupLua env
            recovers env ls

        it "drains all the way to the kill rule rather than settling — \
           \40 ticks take it from 5.000 to 3.000" $ \env → do
            resetScene env
                (seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5))
            ls ← setupLua env
            tickStamina ls 40
            storedStamina env `shouldBeNear`
                (startStamina - organFailureIdleLoss * 40)

    -- §3 The average profile, where both formulas agree. This is the
    -- control that proves the change moved the OFF-bulk cases only.
    describe "the average profile, where the formulas agree (§3)" $ do
        it "a fresh bulk-1.0 acolyte recovers normally" $ \env → do
            resetScene env (seededStats 1.0 meanBodyfat)
            ls ← setupLua env
            recovers env ls

        it "a bulk-1.0 acolyte clamped at its floor enters organ \
           \failure, exactly as it did under the height-only formula" $
          \env → do
            resetScene env
                (seededWithFat 1.0 meanBodyfat (frameFloorAt 1.0))
            ls ← setupLua env
            fails env ls

    -- §4 The slim profile's own floor still ends it.
    describe "a slim acolyte clamped at its own floor (§4)" $
        it "enters organ failure at 0.7128 kg — the floor moved, the \
           \consequence did not" $ \env → do
            resetScene env
                (seededWithFat 0.5 leanBodyfat (frameFloorAt 0.5))
            ls ← setupLua env
            fails env ls

    -- §5 The Float32 tolerance band. uiStats is HashMap Text Float and
    -- Lua reads Float64, so a value clamped to the floor comes back
    -- slightly above the recomputed one; FAT_FLOOR_TOL is what keeps
    -- the branch reachable. Both probes sit ABOVE the floor, so
    -- deleting the tolerance term changes the inside one's verdict.
    describe "the Float32 tolerance band (§5)" $ do
        it "0.5e-4 kg above the floor — inside FAT_FLOOR_TOL — still \
           \enters organ failure" $ \env → do
            resetScene env
                (seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5 + 0.5e-4))
            ls ← setupLua env
            evalDebug ls
                "return unit.getStat(1, 'fat_mass') > _G.__energy.minFatFor(1)"
                `shouldReturn` "true"
            fails env ls

        it "2e-4 kg above the floor — outside it — recovers normally" $
          \env → do
            resetScene env
                (seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5 + 2e-4))
            ls ← setupLua env
            recovers env ls

    -- §6 Which floor input applies. The old check was gated on
    -- `fat and h`, so a frame-carrying unit with no height stat was
    -- never evaluated at all.
    describe "floor inputs (§6)" $ do
        it "frame_mass without height uses the frame floor rather than \
           \skipping the check" $ \env → do
            resetScene env $ without ["height"] $
                seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5)
            ls ← setupLua env
            evalDebug ls "return unit.getStat(1, 'height') == nil"
                `shouldReturn` "true"
            fails env ls

        it "that same unit, above its frame floor, still recovers" $
          \env → do
            resetScene env $ without ["height"] $
                seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5 + 1.0)
            ls ← setupLua env
            recovers env ls

        it "height without frame_mass keeps the legacy 0.44·h² floor — \
           \a unit seeded before frame_mass existed is unchanged" $
          \env → do
            resetScene env $ without ["frame_mass"] $
                seededWithFat 1.0 meanBodyfat legacyFloor
            ls ← setupLua env
            fails env ls

        it "a legacy unit safely above the legacy floor recovers, so \
           \the fallback is a real comparison and not an always-true \
           \arm" $ \env → do
            resetScene env $ without ["frame_mass"] $
                seededWithFat 1.0 meanBodyfat (legacyFloor + 1.0)
            ls ← setupLua env
            recovers env ls

        it "the legacy floor is genuinely the one being applied: the \
           \same fat under a bulk-1.5 frame would clear a frame floor \
           \it cannot clear here" $ \env → do
            resetScene env $ without ["frame_mass"] $
                seededWithFat 1.0 meanBodyfat legacyFloor
            ls ← setupLua env
            evalDebug ls (luaNear "_G.__energy.minFatFor(1)" legacyFloor)
                `shouldReturn` "true"

        it "neither input readable keeps today's no-op: no organ \
           \failure, no drain, ordinary recovery — even at zero fat, \
           \which a missing floor defaulted to 0 would call failure" $
          \env → do
            resetScene env $ without ["height", "frame_mass"] $
                seededWithFat 1.0 meanBodyfat 0
            ls ← setupLua env
            evalDebug ls "return _G.__energy.minFatFor(1) == nil"
                `shouldReturn` "true"
            recovers env ls

        it "a unit with no floor input but ordinary fat also recovers, \
           \so the no-op is not an artefact of the zero" $ \env → do
            resetScene env $ without ["height", "frame_mass"] $
                seededWithFat 1.0 meanBodyfat 14.256
            ls ← setupLua env
            recovers env ls

    -- §7 The opt-out, exercised against a SHIPPED def rather than a
    -- synthetic table: bear_brown declares no organ_failure_check.
    describe "species that do not opt in (§7)" $ do
        it "a brown bear at its own fat floor recovers normally — the \
           \shipped def sets no organ_failure_check" $ \env → do
            resetSceneAs env "bear_brown"
                (seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5))
            ls ← setupLua env
            tickStaminaAs ls "bear_brown" "idle" 1
            storedStamina env `shouldBeNear` (startStamina + idleGain)

        it "the same body under the acolyte's opted-in config DOES \
           \fail, so §7 is about the flag and not about the body" $
          \env → do
            resetScene env
                (seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5))
            ls ← setupLua env
            fails env ls

    -- §8 One floor policy, two consumers. The drift this fixes was
    -- created by copying the formula; this asserts the copy is gone by
    -- pinning the tick's verdict to the value catabolism clamps to.
    describe "one shared floor (§8)" $ do
        it "energy.minFatFor is 0.02 × frame_mass for every supported \
           \profile" $ \env → do
            forM_ [0.5, 1.0, 1.5] $ \bulk → do
                resetScene env (seededStats bulk meanBodyfat)
                ls ← setupLua env
                evalDebug ls
                    (luaNear "_G.__energy.minFatFor(1)" (frameFloorAt bulk))
                    `shouldReturn` "true"

        it "catabolism clamps fat_mass to exactly that value, and the \
           \stamina tick's verdict flips there — recovering just above \
           \it, failing once clamped to it" $ \env → do
            -- Just above the floor, with an empty calorie store and a
            -- metabolism steep enough that one tick's deficit more than
            -- covers the gap, so the clamp — not the rate — decides
            -- where fat lands.
            resetScene env
                $ HM.insert "metabolism" 1e5
                $ HM.insert "calories" 0
                $ seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5 + 0.01)
            ls ← setupLua env

            -- Before: above the floor and outside the tolerance, so it
            -- recovers.
            recovers env ls

            tickStarvation ls
            storedStat env "fat_mass" `shouldBeNear` frameFloorAt 1.5

            -- After: the same unit, at the value catabolism chose,
            -- drains at the organ-failure rate.
            after ← storedStamina env
            tickStamina ls 1
            storedStamina env `shouldBeNear`
                (maybe 0 id after - organFailureIdleLoss)

        it "catabolism switches to pure muscle at that same value — \
           \both consumers changed regime at one floor, not two" $
          \env → do
            resetScene env
                $ HM.insert "metabolism" 1e5
                $ HM.insert "calories" 0
                $ seededWithFat 1.5 meanBodyfat (frameFloorAt 1.5 + 0.01)
            ls ← setupLua env
            leanBefore ← storedStat env "lean_mass"
            tickStarvation ls
            storedStat env "fat_mass" `shouldBeNear` frameFloorAt 1.5
            -- A second tick can take no more fat, so the whole deficit
            -- lands on muscle.
            leanMid ← storedStat env "lean_mass"
            tickStarvation ls
            storedStat env "fat_mass" `shouldBeNear` frameFloorAt 1.5
            leanAfter ← storedStat env "lean_mass"
            let shrunk a b = maybe 0 id a - maybe 0 id b
            shrunk leanMid leanAfter `shouldSatisfy`
                (> shrunk leanBefore leanMid)
