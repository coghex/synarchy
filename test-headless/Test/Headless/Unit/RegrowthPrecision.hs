-- | Surplus regrowth keeps sub-binary32 tissue increments (#2637).
--
--   @scripts\/unit_resource_energy.lua@'s @applyRegrowth@ used to read
--   @fat_mass@, @lean_mass@, @body_mass@ and @calories@ out of @uiStats@
--   (@HashMap Text Float@), add one tick, and write each result straight
--   back. At a default-scale body the 0.1 s cadence's lean increment
--   and the idle body increment are below half a binary32 step, so they
--   rounded away; walking's fat burn did too. The four writes rounded
--   independently, so total mass drifted off fat + lean + organ mass.
--
--   The corrected path keeps the sub-step residue and one organ baseline
--   (@scripts\/unit_resource_regrowth.lua@) and spends calories through
--   the existing resource carry. Both cadences are judged against the
--   analytical increments, not against each other: the old 1 s schedule
--   was biased too. Over 1,000 unclamped seconds idle growth is about
--   +0.011688312 kg fat and +0.005555556 kg lean; walking is about
--   −0.002597403 kg fat and +0.038888889 kg lean; both spend 100 kcal.
--   Walking dissipates half of that and stores the other half.
--
--   Every example drives the SHIPPED helpers through the REAL @unit.*@
--   API over a REAL 'unitManagerRef'. The long schedules call
--   @applyRegrowth@ only — metabolism and digestion stay out of the
--   oracle. Mass conservation is checked after every tick against an
--   organ baseline the fixture captured itself.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "surplus regrowth retains sub-Float32 increments"'@.
module Test.Headless.Unit.RegrowthPrecision (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Text.Read (readMaybe)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Thread.Command.Body (recomputeBodyDerivedStats)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- * Fixture

uidA, uidB ∷ UnitId
uidA = UnitId 1
uidB = UnitId 2

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "regrowth_precision_page"

-- | Shipped regrowth rate and tissue densities. Restated so a retune
--   fails here rather than silently moving the oracle.
rateKcal, kcalPerKgFat, kcalPerKgLean ∷ Double
rateKcal = 0.1
kcalPerKgFat = 7700
kcalPerKgLean = 1800

-- | Pool used by the eligibility examples. 1,600 and 0.75·1,600 = 1,200
--   are both binary32-exact, so the strict @>@ stands on integers.
calMax, threshold, calStep ∷ Float
calMax = 1600
threshold = 1200
calStep = 2 ** (-13)

derivedKeys ∷ [Text]
derivedKeys =
    [ "strength", "strength_body", "max_hydration", "max_hunger"
    , "max_calories", "carrying_capacity" ]

-- | Default-scale acolyte from the issue's reproduction, passed through
--   the production recompute so the derived stats start consistent.
acolyteStats ∷ HM.HashMap Text Float
acolyteStats = recomputeBodyDerivedStats $ HM.fromList
    [ ("height", 1.8)
    , ("body_mass", 71.28)
    , ("lean_mass", 28.512)
    , ("fat_mass", 14.256)
    , ("calories", 1400)
    , ("strength", 1) ]

-- | Fat parked in the [16, 32) bin, where one walking 0.1 s burn is
--   below half an ulp and one idle 0.1 s deposit is above it, but the
--   deposit minus that one banked burn is not. That is the split an
--   activity change must not drop.
fatSwitch ∷ Float
fatSwitch = 20

switchStats ∷ HM.HashMap Text Float
switchStats = HM.insert "max_calories" calMax
            $ HM.insert "fat_mass" fatSwitch acolyteStats

-- | Fat already on the floor, so walking's burn has nowhere to go.
--   Banking that unapplied burn would swallow the idle deposit that
--   follows.
clampStats ∷ HM.HashMap Text Float
clampStats = HM.fromList
    [ ("height", 1.8)
    , ("body_mass", 50)
    , ("lean_mass", 30)
    , ("fat_mass", 0)
    , ("calories", 1400)
    , ("strength", 1)
    , ("strength_base", 1) ]

boundaryStats ∷ Float → HM.HashMap Text Float
boundaryStats cals = HM.insert "calories" cals
                   $ HM.insert "max_calories" calMax acolyteStats

mkAcolyte ∷ HM.HashMap Text Float → UnitInstance
mkAcolyte stats = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
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

resetWith ∷ EngineEnv → [(UnitId, HM.HashMap Text Float)] → IO ()
resetWith env units = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.fromList
            [ (uid, mkAcolyte stats) | (uid, stats) ← units ] }

resetOne ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetOne env stats = resetWith env [(uidA, stats)]

-- * Oracle

-- | Unclamped tissue change over @seconds@, plus the exercise kcal
--   walking dissipates. Idle stores the whole 0.1 kcal/s; walking
--   stores 0.05 and dissipates 0.05.
analytical ∷ Text → Double → (Double, Double, Double)
analytical activity seconds =
    let rate = rateKcal
    in if activity ≡ "walking"
       then ( (0.3 * rate - 0.5 * rate) * seconds / kcalPerKgFat
            , 0.7 * rate * seconds / kcalPerKgLean
            , 0.5 * rate * seconds )
       else ( 0.9 * rate * seconds / kcalPerKgFat
            , 0.1 * rate * seconds / kcalPerKgLean
            , 0 )

-- | Gap from this binary32 value up to the next larger one.
stepAbove ∷ Float → Float
stepAbove x
    | x ≡ 0 = 0
    | otherwise =
        let (m, e) = decodeFloat x
            m' = if m >= 0 then m + 1 else m - 1
        in abs (encodeFloat m' e - x)

closeD ∷ Text → Double → Double → Double → Expectation
closeD label got want tol
    | abs (got - want) <= tol = pure ()
    | otherwise = expectationFailure $
        T.unpack label <> ": got " <> show got
        <> ", want " <> show want <> " ± " <> show tol

closeStat ∷ Text → Float → Double → Double → Expectation
closeStat label actual expected tol =
    closeD label (realToFrac actual) expected tol

-- * Live readers

readStats ∷ EngineEnv → UnitId → IO (HM.HashMap Text Float)
readStats env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Just inst → pure (uiStats inst)
        Nothing → do
            expectationFailure $ "unit " <> show uid <> " is gone"
            pure HM.empty

must ∷ Text → HM.HashMap Text Float → IO Float
must name stats = case HM.lookup name stats of
    Just v → pure v
    Nothing → do
        expectationFailure $ T.unpack name <> " is missing"
        pure 0

-- * Lua plumbing

setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    r ← evalDebug ls $ T.concat
        [ "_G.__energy = require('scripts.unit_resource_energy'); "
        , "_G.__tick = require('scripts.unit_resource_tick'); "
        , "return type(_G.__energy.applyRegrowth) == 'function' "
        , "and type(_G.__tick.tickResource) == 'function'" ]
    r `shouldBe` "true"
    pure ls

setupLifecycle ∷ EngineEnv → IO LuaBackendState
setupLifecycle env = do
    ls ← setupLua env
    r ← evalDebug ls $ T.concat
        [ "_G.__res = require('scripts.unit_resources'); "
        , "_G.__res.init(0); return true" ]
    r `shouldBe` "true"
    pure ls

-- | @n@ regrowth ticks. The organ baseline is captured HERE, from the
--   stored masses, before the loop — not read back out of the module.
--   Returns the worst |body − (fat + lean + organ)| seen after a tick.
runRegrowth ∷ LuaBackendState → Int → Text → Double → Int → IO Double
runRegrowth ls uid activity dt n = do
    out ← evalDebug ls $ T.concat
        [ "local uid = ", tshow uid, "; "
        , "local body0 = unit.getStat(uid, 'body_mass'); "
        , "local fat0 = unit.getStat(uid, 'fat_mass'); "
        , "local lean0 = unit.getStat(uid, 'lean_mass'); "
        , "local organ = body0 - fat0 - lean0; "
        , "local worst = 0; "
        , "for _ = 1, ", tshow n, " do "
        , "  _G.__energy.applyRegrowth(uid, '", activity, "', ", tshow dt, "); "
        , "  local b = unit.getStat(uid, 'body_mass'); "
        , "  local f = unit.getStat(uid, 'fat_mass'); "
        , "  local l = unit.getStat(uid, 'lean_mass'); "
        , "  local err = math.abs(b - (f + l + organ)); "
        , "  if err > worst then worst = err end; "
        , "end; "
        , "return worst" ]
    case readMaybe (T.unpack out) of
        Just d → pure d
        Nothing → do
            expectationFailure $ "conservation loop failed: " <> T.unpack out
            pure 0

luaOk ∷ LuaBackendState → Text → IO ()
luaOk ls src = evalDebug ls src ⌦ (`shouldBe` "true")

-- One binary32 step is enough room for the residue the stored fat and
-- lean still hold; zero growth is orders of magnitude past it.
massTol ∷ Double → Double
massTol expected = 2 * realToFrac (stepAbove (realToFrac expected))

cadence ∷ Text → Double → Int → SpecWith EngineEnv
cadence activity dt n =
    it (T.unpack activity <> " dt=" <> show dt <> " over "
        <> show (fromIntegral n * dt) <> "s") $ \env → do
        resetOne env acolyteStats
        ls ← setupLua env
        before ← readStats env uidA
        fat0 ← must "fat_mass" before
        lean0 ← must "lean_mass" before
        body0 ← must "body_mass" before
        cal0 ← must "calories" before
        hyd0 ← must "max_hydration" before
        let seconds = fromIntegral n * dt
            (fatKg, leanKg, exercise) = analytical activity seconds
            organ0 = realToFrac body0 - realToFrac fat0 - realToFrac lean0
        worst ← runRegrowth ls 1 activity dt n
        after ← readStats env uidA
        fat1 ← must "fat_mass" after
        lean1 ← must "lean_mass" after
        body1 ← must "body_mass" after
        cal1 ← must "calories" after
        hyd1 ← must "max_hydration" after
        let wantFat = realToFrac fat0 + fatKg
            wantLean = realToFrac lean0 + leanKg
            wantBody = realToFrac body0 + fatKg + leanKg
            spent = realToFrac cal0 - realToFrac cal1
            tissue = (realToFrac fat1 - realToFrac fat0) * kcalPerKgFat
                   + (realToFrac lean1 - realToFrac lean0) * kcalPerKgLean
            cons = abs (realToFrac body1
                        - (realToFrac fat1 + realToFrac lean1 + organ0))
            step = realToFrac (stepAbove body1)
        closeStat "fat_mass" fat1 wantFat (massTol wantFat)
        closeStat "lean_mass" lean1 wantLean (massTol wantLean)
        closeStat "body_mass" body1 wantBody (massTol wantBody)
        closeStat "calories" cal1 (realToFrac cal0 - rateKcal * seconds) 1e-3
        closeD "tissue plus exercise" (tissue + exercise) spent 0.05
        cons `shouldSatisfy` (<= step)
        worst `shouldSatisfy` (<= step)
        hyd1 `shouldNotBe` hyd0
        let again = recomputeBodyDerivedStats after
        forM_ derivedKeys $ \k →
            HM.lookup k after `shouldBe` HM.lookup k again

-- * Spec

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
    describe "surplus regrowth retains sub-Float32 increments" $ do

    it "states the 1000 s analytical increments, not the old 1 s bias" $
      \_ → do
        let (fi, li, ei) = analytical "idle" 1000
            (fw, lw, ew) = analytical "walking" 1000
        closeD "idle fat" fi 0.011688312 1e-9
        closeD "idle lean" li 0.005555556 1e-9
        closeD "idle exercise" ei 0 1e-12
        closeD "idle tissue kcal" (fi * kcalPerKgFat + li * kcalPerKgLean) 100 1e-6
        closeD "walk fat" fw (-0.002597403) 1e-9
        closeD "walk lean" lw 0.038888889 1e-9
        closeD "walk exercise" ew 50 1e-9
        closeD "walk tissue kcal" (fw * kcalPerKgFat + lw * kcalPerKgLean) 50 1e-6

    describe "both cadences match those increments" $ do
        cadence "idle" 0.1 10000
        cadence "idle" 1.0 1000
        cadence "walking" 0.1 10000
        cadence "walking" 1.0 1000

    describe "earned residue survives an activity change and a gap" $ do
        it "one banked walking burn keeps the following idle tick from \
           \depositing fat" $ \env → do
            resetOne env switchStats
            ls ← setupLua env
            luaOk ls "_G.__energy.applyRegrowth(1, 'walking', 0.1); return true"
            must "fat_mass" ⌫ readStats env uidA ⌦ (`shouldBe` fatSwitch)
            -- Idle alone would clear half an ulp and store one step
            -- higher. Minus the walking residue it does not.
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            must "fat_mass" ⌫ readStats env uidA ⌦ (`shouldBe` fatSwitch)

        it "ticks below the surplus band neither grow nor drop that \
           \residue" $ \env → do
            resetOne env switchStats
            ls ← setupLua env
            luaOk ls "_G.__energy.applyRegrowth(1, 'walking', 0.1); return true"
            -- 800/1600 is under the strict 75% line. Twenty idle
            -- resource ticks here would move fat if they regrew, and
            -- clearing the walking residue would let the last idle
            -- deposit land.
            luaOk ls "unit.setStat(1, 'calories', 800); return true"
            luaOk ls $ T.concat
                [ "local p = { max_from = 'max_calories', "
                , "surplus_regrowth = true }; "
                , "for _ = 1, 20 do "
                , "_G.__tick.tickResource(1, 'acolyte', 'calories', p, "
                , "'idle', 'standing', 0.1) end; return true" ]
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            must "fat_mass" ⌫ readStats env uidA ⌦ (`shouldBe` fatSwitch)

    describe "the surplus gate is strict" $ do
        it "does not regrow at exactly 75% of the store" $ \env → do
            resetOne env (boundaryStats threshold)
            ls ← setupLua env
            fat0 ← must "fat_mass" ⌫ readStats env uidA
            luaOk ls $ T.concat
                [ "local p = { max_from = 'max_calories', "
                , "surplus_regrowth = true }; "
                , "_G.__tick.tickResource(1, 'acolyte', 'calories', p, "
                , "'idle', 'standing', 0.1); return true" ]
            fat1 ← must "fat_mass" ⌫ readStats env uidA
            cal1 ← must "calories" ⌫ readStats env uidA
            fat1 `shouldBe` fat0
            cal1 `shouldBe` threshold

        it "does not regrow one binary32 step below 75%" $ \env → do
            let below = threshold - calStep
            resetOne env (boundaryStats below)
            ls ← setupLua env
            fat0 ← must "fat_mass" ⌫ readStats env uidA
            luaOk ls $ T.concat
                [ "local p = { max_from = 'max_calories', "
                , "surplus_regrowth = true }; "
                , "_G.__tick.tickResource(1, 'acolyte', 'calories', p, "
                , "'idle', 'standing', 0.1); return true" ]
            fat1 ← must "fat_mass" ⌫ readStats env uidA
            cal1 ← must "calories" ⌫ readStats env uidA
            fat1 `shouldBe` fat0
            cal1 `shouldBe` below

        it "does regrow one binary32 step above 75%" $ \env → do
            let above = threshold + calStep
            resetOne env (boundaryStats above)
            ls ← setupLua env
            fat0 ← must "fat_mass" ⌫ readStats env uidA
            cal0 ← must "calories" ⌫ readStats env uidA
            luaOk ls $ T.concat
                [ "local p = { max_from = 'max_calories', "
                , "surplus_regrowth = true }; "
                , "_G.__tick.tickResource(1, 'acolyte', 'calories', p, "
                , "'idle', 'standing', 0.1); return true" ]
            fat1 ← must "fat_mass" ⌫ readStats env uidA
            cal1 ← must "calories" ⌫ readStats env uidA
            fat1 `shouldNotBe` fat0
            cal1 `shouldSatisfy` (< cal0)

    describe "a clamp does not become debt" $
        it "walking against zero fat does not swallow the next idle \
           \deposit, and lean still grows" $ \env → do
            resetOne env clampStats
            ls ← setupLua env
            _ ← runRegrowth ls 1 "walking" 0.1 10000
            fatWalk ← must "fat_mass" ⌫ readStats env uidA
            leanWalk ← must "lean_mass" ⌫ readStats env uidA
            fatWalk `shouldBe` 0
            leanWalk `shouldSatisfy` (> 30.01)
            _ ← runRegrowth ls 1 "idle" 1.0 1
            fatIdle ← must "fat_mass" ⌫ readStats env uidA
            let want = 0.9 * rateKcal * 1 / kcalPerKgFat
            closeStat "fat after idle" fatIdle want (massTol want)
            fatIdle `shouldSatisfy` (> 0)

    describe "intervening writes are kept" $ do
        it "two idle ticks move lean; the residue is what the second \
           \one spends" $ \env → do
            resetOne env acolyteStats
            ls ← setupLua env
            lean0 ← must "lean_mass" ⌫ readStats env uidA
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            lean1 ← must "lean_mass" ⌫ readStats env uidA
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            lean2 ← must "lean_mass" ⌫ readStats env uidA
            lean1 `shouldBe` lean0
            lean2 `shouldNotBe` lean0

        it "rewriting the same lean value drops that residue" $ \env → do
            resetOne env acolyteStats
            ls ← setupLua env
            lean0 ← must "lean_mass" ⌫ readStats env uidA
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            luaOk ls "unit.setStat(1, 'lean_mass', unit.getStat(1, 'lean_mass')); return true"
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            lean2 ← must "lean_mass" ⌫ readStats env uidA
            lean2 `shouldBe` lean0

        it "an external fat edit is the new baseline, not overwritten \
           \by the tracked total" $ \env → do
            resetOne env acolyteStats
            ls ← setupLua env
            _ ← runRegrowth ls 1 "idle" 0.1 5
            bodyBefore ← must "body_mass" ⌫ readStats env uidA
            luaOk ls "unit.setStat(1, 'fat_mass', 20); return true"
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            fat1 ← must "fat_mass" ⌫ readStats env uidA
            body1 ← must "body_mass" ⌫ readStats env uidA
            closeStat "fat stays at the edit" fat1 20 0.001
            closeStat "body does not jump with the edit" body1
                (realToFrac bodyBefore) 0.01

        it "an external calorie edit is what the next spend subtracts \
           \from" $ \env → do
            resetOne env acolyteStats
            ls ← setupLua env
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            luaOk ls "unit.setStat(1, 'calories', 1000); return true"
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            cal1 ← must "calories" ⌫ readStats env uidA
            closeStat "calories" cal1 (1000 - rateKcal * 0.1) 0.001
            cal1 `shouldSatisfy` (< 1100)

    describe "residue does not cross units" $
        it "one tick on each unit banks nothing the other can spend" $
          \env → do
            resetWith env [(uidA, acolyteStats), (uidB, acolyteStats)]
            ls ← setupLua env
            leanA0 ← must "lean_mass" ⌫ readStats env uidA
            leanB0 ← must "lean_mass" ⌫ readStats env uidB
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            luaOk ls "_G.__energy.applyRegrowth(2, 'idle', 0.1); return true"
            leanA1 ← must "lean_mass" ⌫ readStats env uidA
            leanB1 ← must "lean_mass" ⌫ readStats env uidB
            leanA1 `shouldBe` leanA0
            leanB1 `shouldBe` leanB0
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            leanA2 ← must "lean_mass" ⌫ readStats env uidA
            leanB2 ← must "lean_mass" ⌫ readStats env uidB
            leanA2 `shouldNotBe` leanA0
            leanB2 `shouldBe` leanB0

    describe "load and Exit to Menu drop it" $ do
        it "the reset hook unit_resources registers discards a banked \
           \lean residue" $ \env → do
            resetOne env acolyteStats
            ls ← setupLifecycle env
            lean0 ← must "lean_mass" ⌫ readStats env uidA
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            luaOk ls "require('scripts.lib.save_modules').resetHooks['unit_resources'](); return true"
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            lean1 ← must "lean_mass" ⌫ readStats env uidA
            lean1 `shouldBe` lean0

        it "Exit to Menu discards it too" $ \env → do
            resetOne env acolyteStats
            ls ← setupLifecycle env
            lean0 ← must "lean_mass" ⌫ readStats env uidA
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            luaOk ls "return require('scripts.lib.session_teardown').runAll() == 0"
            luaOk ls "_G.__energy.applyRegrowth(1, 'idle', 0.1); return true"
            lean1 ← must "lean_mass" ⌫ readStats env uidA
            lean1 `shouldBe` lean0

        it "registers the same id on both session boundaries" $ \env → do
            resetOne env acolyteStats
            ls ← setupLifecycle env
            luaOk ls "return require('scripts.lib.save_modules').resetHooks['unit_resources'] ~= nil"
            luaOk ls $ T.concat
                [ "local ids = require('scripts.lib.session_teardown').registeredIds(); "
                , "for _, id in ipairs(ids) do if id == 'unit_resources' then return true end end; "
                , "return false" ]
