-- | Atomic stance recovery (#2468).
--
--   Stance is spent engine-side, inside #2328's single atomic
--   unit-manager commit, and recovered in Lua on the physiology tick.
--   The recovery used to be a read-modify-write across the engine
--   boundary — @unit.getStat@, add @rate × dt@ in script,
--   @unit.setStat@ the absolute sum — so a strike the combat worker
--   committed between the read and the write was republished away.
--
--   These specs drive the SHIPPED @scripts\/unit_resource_injury.lua@
--   @tickStance@ through the REAL registered @unit.recoverStance@
--   against a REAL unit-manager ref, with the interleaving CONTROLLED
--   rather than raced: the strike is applied from inside the Lua rate
--   calculation, by a hook interposed on @unit.getStat@ that calls
--   production 'spendStrikeCost'. So the window is hit deterministically
--   on every run, and §1 fails against the old @tickStance@ (which
--   republishes 0.659) rather than failing one run in a thousand.
--
--   The fixture pins dexterity and agility as STORED values, so the
--   recovery rate never depends on a stat roll and no example here
--   consumes 'statRNGRef' — which §4 asserts directly, since "absent
--   stance stays absent" is exactly the case a lazy roll would break.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Atomic stance recovery"'@.
module Test.Headless.Unit.StanceRecovery (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import qualified HsLua as Lua
import Combat.Resolution.Wear (spendStrikeCost)
import Combat.Types (AttackMode(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, mkItem, newBareLuaBackend)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- * Fixture

acolyteUid ∷ UnitId
acolyteUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "stance_recovery_page"

-- | @STANCE_RECOVER_BASE + STANCE_RECOVER_PER_STAT × (dex + agi)@ with
--   the fixture's dex = agi = 1, mirroring
--   @scripts\/unit_resource_injury.lua@. Kept as an independent
--   restatement so a silent change to either constant shows up here.
fixtureRate ∷ Float
fixtureRate = 0.35 + 0.12 * (1.0 + 1.0)

-- | One physiology tick's worth of recovery at 'fixtureRate'.
tickDt ∷ Float
tickDt = 0.1

tickGain ∷ Float
tickGain = fixtureRate * tickDt          -- 0.059

-- | A unit carrying exactly the stats the recovery path reads, plus
--   whatever else an example wants to watch for collateral damage.
mkStanceUnit ∷ HM.HashMap Text Float → UnitInstance
mkStanceUnit stats = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
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

-- | Stats with the two rate inputs pinned and stance set to @s@.
statsWithStance ∷ Float → HM.HashMap Text Float
statsWithStance s = HM.insert "stance" s statsWithoutStance

-- | The same fixture with NO stance entry — the "implicitly full"
--   shape a freshly spawned unit actually has.
statsWithoutStance ∷ HM.HashMap Text Float
statsWithoutStance = HM.fromList [("dexterity", 1.0), ("agility", 1.0)]

-- | Install a one-unit scene. @def@ varies only for the lazy-roll
--   template case; @mods@ only for the base-vs-effective cases.
resetSceneWith ∷ EngineEnv → UnitDef → HM.HashMap Text Float
               → HM.HashMap Text [StatModifier] → IO ()
resetSceneWith env def stats mods = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" def
        , umInstances = HM.singleton acolyteUid
            ((mkStanceUnit stats) { uiModifiers = mods }) }

acolyteDef ∷ UnitDef
acolyteDef = minimalDef "acolyte" "Acolyte"

resetScene ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetScene env stats = resetSceneWith env acolyteDef stats HM.empty

-- * Live readers

instancesOf ∷ EngineEnv → IO (HM.HashMap UnitId UnitInstance)
instancesOf env = umInstances ⊚ readIORef (unitManagerRef env)

acolyte ∷ EngineEnv → IO UnitInstance
acolyte env = do
    insts ← instancesOf env
    case HM.lookup acolyteUid insts of
        Just inst → pure inst
        Nothing   → fail "fixture acolyte is missing from the unit manager"

storedStance ∷ EngineEnv → IO (Maybe Float)
storedStance env = HM.lookup "stance" ∘ uiStats ⊚ acolyte env

-- | The stat RNG as a comparable value: an example asserting "rolled
--   nothing" compares this across the call.
statGen ∷ EngineEnv → IO String
statGen env = show ⊚ readIORef (statRNGRef env)

-- | The live scene RENDERED rather than compared by 'Eq'. §5 has to
--   assert "nothing moved" for a fixture whose stored stance is NaN,
--   and NaN never equals itself — so a straight @shouldBe@ on the
--   instances would fail on the very case that most needs the check.
sceneSnapshot ∷ EngineEnv → IO String
sceneSnapshot env = show ⊚ instancesOf env

-- * Assertions

shouldBeNear ∷ IO (Maybe Float) → Float → Expectation
shouldBeNear act expected = act ⌦ \case
    Nothing → expectationFailure $
        "expected a stored stance near " <> show expected <> ", found no entry"
    Just actual
        | abs (actual - expected) < 1e-5 → pure ()
        | otherwise → expectationFailure $
            "expected a stored stance near " <> show expected
            <> ", found " <> show actual

-- | Assert @nil, reason@ AND that nothing moved. One helper so no
--   refusal case can assert the reason and forget the mutation half.
expectRefusal ∷ EngineEnv → LuaBackendState → Text → Text → Expectation
expectRefusal env ls call reason = do
    before ← sceneSnapshot env
    genBefore ← statGen env
    r ← evalDebug ls $ T.concat
        [ "local v, why = ", call
        , "; return v == nil and why == '", reason, "'" ]
    r `shouldBe` "true"
    sceneSnapshot env `shouldReturn` before
    statGen env `shouldReturn` genBefore

-- | @math.abs(expr - value) < 1e-5@. The stats are 'Float' and Lua
--   arithmetic is 'Double', so a Lua literal such as @0.42@ never
--   compares equal to what the manager actually holds.
luaNear ∷ Text → Float → Text
luaNear expr v = T.concat
    [ "math.abs((", expr, ") - ", tshow (realToFrac v ∷ Double), ") < 1e-5" ]

-- * Engine-side interleaving

-- | Charge the fixture acolyte for one quick strike through the
--   PRODUCTION cost function, against the same ref the verb commits to.
applyQuickStrike ∷ EngineEnv → IO ()
applyQuickStrike env = do
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (spendStrikeCost now Quick (unUnitId acolyteUid) um, ())

-- | Expose 'applyQuickStrike' to Lua as @__strike(uid)@ so an example
--   can fire it from INSIDE the script's own rate calculation.
installStrikeHook ∷ EngineEnv → LuaBackendState → IO ()
installStrikeHook env ls =
    Lua.runWith (lbsLuaState ls) (installer ∷ Lua.LuaE Lua.Exception ())
  where
    installer = do
        Lua.pushHaskellFunction strike
        Lua.setglobal "__strike"
    strike ∷ Lua.LuaE Lua.Exception Lua.NumResults
    strike = do
        Lua.liftIO (applyQuickStrike env)
        return 0

-- | Wrap @unit.getStat@ so the FIRST dexterity read — which
--   @tickStance@ performs while computing its rate, after the point the
--   old code had already sampled stance — commits a quick strike. This
--   is the whole race, made deterministic.
interposeStrikeOnRateRead ∷ LuaBackendState → IO ()
interposeStrikeOnRateRead ls = do
    r ← evalDebug ls $ T.concat
        [ "_G.__fired = false; "
        , "local real = unit.getStat; "
        , "unit.getStat = function(uid, name) "
        , "  if name == 'dexterity' and not _G.__fired then "
        , "    _G.__fired = true; __strike(uid); "
        , "  end; "
        , "  return real(uid, name); "
        , "end; return true" ]
    r `shouldBe` "true"

-- | Drive the SHIPPED module's entry point.
tickStance ∷ LuaBackendState → UnitId → Float → IO Text
tickStance ls (UnitId uid) dt = evalDebug ls $ T.concat
    [ "require('scripts.unit_resource_injury').tickStance("
    , tshow uid, ", ", tshow dt, "); return true" ]

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "Atomic stance recovery" $ do

    -- §1 The defect: a debit committed inside the recovery's own
    -- window must survive it.
    describe "the lost debit (§1)" $
        it "a quick strike committing during the Lua rate calculation \
           \is not erased: 0.600 → 0.409, not 0.659" $ \env → do
            resetScene env (statsWithStance 0.6)
            ls ← newBareLuaBackend env
            installStrikeHook env ls
            interposeStrikeOnRateRead ls

            tickStance ls acolyteUid tickDt `shouldReturn` "true"

            -- 0.600 − 0.25 (quick strike) + 0.059 (recovery). The old
            -- read-modify-write publishes 0.600 + 0.059 = 0.659,
            -- silently refunding the swing.
            storedStance env `shouldBeNear` (0.6 - 0.25 + tickGain)
            evalDebug ls "return _G.__fired" `shouldReturn` "true"

    -- §2 Either serial order is fine; only the stale overwrite is not.
    describe "serial orders (§2)" $ do
        it "debit before recovery yields the serial result" $ \env → do
            resetScene env (statsWithStance 0.6)
            ls ← newBareLuaBackend env
            applyQuickStrike env
            storedStance env `shouldBeNear` 0.35

            tickStance ls acolyteUid tickDt `shouldReturn` "true"
            storedStance env `shouldBeNear` (0.35 + tickGain)

        it "recovery before debit yields the serial result" $ \env → do
            resetScene env (statsWithStance 0.6)
            ls ← newBareLuaBackend env

            tickStance ls acolyteUid tickDt `shouldReturn` "true"
            storedStance env `shouldBeNear` (0.6 + tickGain)

            applyQuickStrike env
            storedStance env `shouldBeNear` (0.6 + tickGain - 0.25)

        it "at the ceiling the two orders legitimately differ — recover \
           \then spend gives 0.75, spend then recover gives 0.80" $
          \env → do
            resetScene env (statsWithStance 0.95)
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.recoverStance(1, 0.10) == 1.0"
                `shouldReturn` "true"
            applyQuickStrike env
            storedStance env `shouldBeNear` 0.75

            resetScene env (statsWithStance 0.95)
            applyQuickStrike env
            _ ← evalDebug ls "return unit.recoverStance(1, 0.10)"
            storedStance env `shouldBeNear` 0.80

        it "the returned value is exactly the value committed" $ \env → do
            resetScene env (statsWithStance 0.4)
            ls ← newBareLuaBackend env
            evalDebug ls "_G.__v = unit.recoverStance(1, 0.125); \
                         \return _G.__v ~= nil" `shouldReturn` "true"
            committed ← storedStance env
            case committed of
                Nothing → expectationFailure "no stance was committed"
                Just v  → evalDebug ls ("return _G.__v == "
                            <> tshow (realToFrac v ∷ Double))
                              `shouldReturn` "true"

    -- §3 The arithmetic contract: add, then clamp into [0, 1].
    describe "add-then-clamp (§3)" $ do
        it "ordinary recovery adds the amount to the stored base" $ \env → do
            resetScene env (statsWithStance 0.25)
            ls ← newBareLuaBackend env
            _ ← evalDebug ls "return unit.recoverStance(1, 0.125)"
            storedStance env `shouldBeNear` 0.375

        it "a zero amount on an in-range value is a no-op returning the \
           \current value" $ \env → do
            resetScene env (statsWithStance 0.42)
            ls ← newBareLuaBackend env
            before ← instancesOf env
            evalDebug ls ("return " <> luaNear "unit.recoverStance(1, 0)" 0.42)
                `shouldReturn` "true"
            instancesOf env `shouldReturn` before

        it "a full stance stays 1" $ \env → do
            resetScene env (statsWithStance 1.0)
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.recoverStance(1, 0.059) == 1.0"
                `shouldReturn` "true"
            storedStance env `shouldBeNear` 1.0

        it "a large finite amount saturates at exactly 1" $ \env → do
            resetScene env (statsWithStance 0.1)
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.recoverStance(1, 1000000) == 1.0"
                `shouldReturn` "true"
            storedStance env `shouldReturn` Just 1.0

        it "an amount finite in Lua but outside Float range (1e100) \
           \saturates at exactly 1 rather than being rejected or \
           \publishing an infinity" $ \env → do
            resetScene env (statsWithStance 0.1)
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.recoverStance(1, 1e100) == 1.0"
                `shouldReturn` "true"
            storedStance env `shouldReturn` Just 1.0

        it "a stored value above 1 is clamped down, even by a zero \
           \recovery" $ \env → do
            resetScene env (statsWithStance 1.4)
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.recoverStance(1, 0) == 1.0"
                `shouldReturn` "true"
            storedStance env `shouldReturn` Just 1.0

        it "a stored value below 0 is clamped up, even by a zero \
           \recovery" $ \env → do
            resetScene env (statsWithStance (-0.2))
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.recoverStance(1, 0) == 0.0"
                `shouldReturn` "true"
            storedStance env `shouldReturn` Just 0.0

    -- §4 Absence means implicitly full, and must STAY absent.
    describe "an absent stance (§4)" $ do
        it "reports 1, creates no entry and rolls nothing" $ \env → do
            resetScene env statsWithoutStance
            ls ← newBareLuaBackend env
            genBefore ← statGen env

            evalDebug ls "return unit.recoverStance(1, 0.059) == 1.0"
                `shouldReturn` "true"

            storedStance env `shouldReturn` Nothing
            statGen env `shouldReturn` genBefore

        it "stays absent even when the unit's definition carries a \
           \lazy-roll template for it" $ \env → do
            resetSceneWith env
                acolyteDef { udStatTemplates =
                                HM.singleton "stance" (0.8, 0.4) }
                statsWithoutStance HM.empty
            ls ← newBareLuaBackend env
            genBefore ← statGen env

            evalDebug ls "return unit.recoverStance(1, 0.059) == 1.0"
                `shouldReturn` "true"

            storedStance env `shouldReturn` Nothing
            statGen env `shouldReturn` genBefore

    -- §5 Every refusal: nil, reason, and nothing moved.
    describe "refusals (§5)" $ do
        it "an id no unit answers to is refused as no_such_unit" $ \env → do
            resetScene env (statsWithStance 0.5)
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.recoverStance(99, 0.059)"
                "no_such_unit"

        it "a unit removed before the call is refused as no_such_unit" $
          \env → do
            resetScene env (statsWithStance 0.5)
            ls ← newBareLuaBackend env
            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "acolyte" acolyteDef }
            expectRefusal env ls "unit.recoverStance(1, 0.059)"
                "no_such_unit"

        it "every malformed id is refused as invalid_unit_id, and a \
           \numeric string never coerces onto that unit" $ \env → do
            resetScene env (statsWithStance 0.5)
            ls ← newBareLuaBackend env
            forM_ [ "'1'", "1.5", "-1", "4294967296", "nil", "true"
                  , "{}" ] $ \arg →
                expectRefusal env ls
                    ("unit.recoverStance(" <> arg <> ", 0.059)")
                    "invalid_unit_id"

        it "every malformed amount is refused as invalid_amount" $ \env → do
            resetScene env (statsWithStance 0.5)
            ls ← newBareLuaBackend env
            forM_ [ "'0.059'", "-0.059", "0/0", "math.huge", "-math.huge"
                  , "nil", "true", "{}" ] $ \arg →
                expectRefusal env ls
                    ("unit.recoverStance(1, " <> arg <> ")")
                    "invalid_amount"

        it "a malformed amount is refused at FULL stance too — the \
           \checks precede the stance read" $ \env → do
            resetScene env (statsWithStance 1.0)
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.recoverStance(1, -1)"
                "invalid_amount"

        it "a malformed amount is refused at ABSENT stance too, and \
           \still creates no entry" $ \env → do
            resetScene env statsWithoutStance
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.recoverStance(1, -1)"
                "invalid_amount"
            storedStance env `shouldReturn` Nothing

        it "a NaN stored value is refused as invalid_stance without \
           \mutating" $ \env → do
            resetScene env (statsWithStance (0 / 0))
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.recoverStance(1, 0.059)"
                "invalid_stance"

        it "an infinite stored value is refused as invalid_stance \
           \without mutating" $ \env → do
            resetScene env (statsWithStance (1 / 0))
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.recoverStance(1, 0.059)"
                "invalid_stance"

    -- §6 The other half of the old bug: an effective read written back
    -- into the base.
    describe "stored base vs effective value (§6)" $ do
        it "recovery adds to the stored base, never the \
           \modifier-adjusted effective value" $ \env → do
            resetSceneWith env acolyteDef (statsWithStance 0.5)
                (HM.singleton "stance" [permanent 0.3])
            ls ← newBareLuaBackend env
            -- getStat reports 0.8; the base combat spends is 0.5.
            evalDebug ls ("return " <> luaNear "unit.getStat(1, 'stance')" 0.8)
                `shouldReturn` "true"

            tickStance ls acolyteUid tickDt `shouldReturn` "true"

            storedStance env `shouldBeNear` (0.5 + tickGain)
            mods ← uiModifiers ⊚ acolyte env
            HM.lookup "stance" mods `shouldBe` Just [permanent 0.3]

        it "the RATE still reads effective dexterity and agility" $
          \env → do
            resetSceneWith env acolyteDef (statsWithStance 0.5)
                (HM.singleton "dexterity" [permanent 1.0])
            ls ← newBareLuaBackend env

            tickStance ls acolyteUid tickDt `shouldReturn` "true"

            -- dex reads 2.0, so the rate is 0.35 + 0.12×3 = 0.71.
            storedStance env `shouldBeNear` (0.5 + 0.71 * tickDt)

    -- §7 Blast radius.
    describe "collateral (§7)" $
        it "recovery replaces no unrelated stat, modifier, inventory, \
           \skill or pose field" $ \env → do
            let rich = HM.fromList
                    [ ("stance", 0.5), ("dexterity", 1.0), ("agility", 1.0)
                    , ("stamina", 0.42), ("carrying_capacity", 100) ]
            resetSceneWith env acolyteDef rich
                (HM.singleton "stamina" [permanent 0.1])
            atomicModifyIORef' (unitManagerRef env) $ \um →
                ( um { umInstances = HM.adjust
                        (\i → i { uiInventory = [mkItem "rock" 7 1.0]
                                , uiSkills = HM.singleton "balance" 3.0
                                , uiPose = "crouching" })
                        acolyteUid (umInstances um) }, () )
            before ← acolyte env
            ls ← newBareLuaBackend env

            _ ← evalDebug ls "return unit.recoverStance(1, 0.125)"

            after ← acolyte env
            after `shouldBe` before
                { uiStats = HM.insert "stance" 0.625 (uiStats before) }

    -- §8 The shipped caller.
    describe "the shipped tickStance caller (§8)" $ do
        it "recovers through the verb, with no stance read of its own" $
          \env → do
            resetScene env (statsWithStance 0.5)
            ls ← newBareLuaBackend env
            tickStance ls acolyteUid tickDt `shouldReturn` "true"
            storedStance env `shouldBeNear` (0.5 + tickGain)

        it "raises the refusal reason as a Lua error, so the engine's \
           \callback isolation can report it (a bare nil return would \
           \be discarded)" $ \env → do
            resetScene env (statsWithStance 0.5)
            ls ← newBareLuaBackend env
            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "acolyte" acolyteDef }

            raised ← tickStance ls acolyteUid tickDt

            raised `shouldSatisfy` T.isInfixOf "no_such_unit"
            raised `shouldSatisfy` T.isPrefixOf "error:"

-- | A permanent flat modifier, the only shape §6 needs.
permanent ∷ Float → StatModifier
permanent d = StatModifier
    { smDelta = d, smSource = "fixture", smExpiry = Nothing, smPercent = 0 }
