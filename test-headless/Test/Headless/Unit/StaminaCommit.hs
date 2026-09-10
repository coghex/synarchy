-- | Engine-committed stamina updates (#2470).
--
--   Stamina is spent engine-side, inside #2328's single atomic
--   unit-manager commit, and drained\/recovered in Lua on the physiology
--   tick. The recovery used to be a read-modify-write across the engine
--   boundary — @unit.getStat@, compute @current + (regen − drain) × dt@
--   and clamp in script, @unit.setStat@ the absolute sum — so a strike
--   the combat worker committed between the read and the write was
--   republished away. Worse, the exhaustion rules that follow then read
--   that same stale @current@ and the script's own @next@, so a pool a
--   strike had driven to exactly zero never fired @kill_on_zero@.
--
--   These specs drive the SHIPPED @scripts\/unit_resource_tick.lua@
--   @tickResource@ and the REAL registered @unit.commitStamina@ against
--   a REAL unit-manager ref, with the interleaving CONTROLLED rather
--   than raced: the strike is applied from inside the Lua rate
--   calculation, by a hook interposed on @unit.getStat@ that calls
--   production 'spendStrikeCost'. So the window is hit deterministically
--   on every run, and §1 and §2 fail against the old @tickResource@
--   rather than failing one run in a thousand.
--
--   The fixture pins @endurance@ as a STORED value and uses the SHIPPED
--   @scripts\/unit_resource_config.lua@ parameter tables (acolyte and
--   bear_brown), so no rate here is invented and no example depends on
--   a stat roll.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Stamina resource commit"'@.
module Test.Headless.Unit.StaminaCommit (spec) where

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
import Item.Types (ItemInstance(..))
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
fixturePage = WorldPageId "stamina_commit_page"

-- | The physiology tick this suite drives, in seconds.
tickDt ∷ Float
tickDt = 0.1

-- | Idle recovery for the fixture: @regen_factor_idle (0.5) × endurance
--   (1) × dt (0.1)@, from the SHIPPED acolyte and bear_brown stamina
--   tables. Restated independently so a silent change to either config
--   shows up here.
idleGain ∷ Float
idleGain = 0.5 * 1.0 * tickDt          -- 0.05

-- | @endurance × 10@, the pool the fixture's stored endurance of 1
--   resolves to through both Lua's @unit_stats@ derivation and
--   'Combat.Resolution.Common.maxStaminaFor'.
fixtureMax ∷ Float
fixtureMax = 10.0

-- | A heavy swing's cost: @staminaCostFraction Heavy (0.25) ×
--   fixtureMax@. Restated rather than imported so a change to the
--   fraction is visible as a failure here.
heavyCost ∷ Float
heavyCost = 0.25 * fixtureMax          -- 2.5

-- | What a heavy swing leaves of a full 1.0 stance
--   ('Combat.Resolution.Constants.stanceAttackCost' @Heavy@ = 0.5).
--   'Combat.Resolution.Wear.staminaDrainStats' writes stance in the
--   SAME transaction as the debit, so it is the natural probe for
--   "the update preserved the other half of the strike".
heavyStanceLeft ∷ Float
heavyStanceLeft = 0.5

-- | A unit carrying exactly the stats the stamina path reads, plus
--   whatever else an example wants to watch for collateral damage.
mkUnitWith ∷ Text → HM.HashMap Text Float → UnitInstance
mkUnitWith defName stats = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = fixturePage
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

-- | The rate input pinned, and stamina set to @s@.
statsWithStamina ∷ Float → HM.HashMap Text Float
statsWithStamina s = HM.insert "stamina" s statsWithoutStamina

-- | The same fixture with NO stamina entry — the shape a freshly
--   spawned unit actually has before its first physiology tick.
statsWithoutStamina ∷ HM.HashMap Text Float
statsWithoutStamina = HM.fromList [("endurance", 1.0)]

-- | Install a one-unit scene under an arbitrary def name, stats,
--   modifiers and pose.
resetSceneFull ∷ EngineEnv → Text → UnitDef → HM.HashMap Text Float
               → HM.HashMap Text [StatModifier] → Text → IO ()
resetSceneFull env defName def stats mods pose = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton defName def
        , umInstances = HM.singleton acolyteUid
            ((mkUnitWith defName stats) { uiModifiers = mods
                                        , uiPose = pose }) }

acolyteDef ∷ UnitDef
acolyteDef = minimalDef "acolyte" "Acolyte"

bearDef ∷ UnitDef
bearDef = minimalDef "bear_brown" "Brown Bear"

resetScene ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetScene env stats =
    resetSceneFull env "acolyte" acolyteDef stats HM.empty "standing"

resetScenePosed ∷ EngineEnv → HM.HashMap Text Float → Text → IO ()
resetScenePosed env stats =
    resetSceneFull env "acolyte" acolyteDef stats HM.empty

resetSceneMods ∷ EngineEnv → HM.HashMap Text Float
               → HM.HashMap Text [StatModifier] → IO ()
resetSceneMods env stats mods =
    resetSceneFull env "acolyte" acolyteDef stats mods "standing"

resetBearScene ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetBearScene env stats =
    resetSceneFull env "bear_brown" bearDef stats HM.empty "standing"

-- * Live readers

instancesOf ∷ EngineEnv → IO (HM.HashMap UnitId UnitInstance)
instancesOf env = umInstances ⊚ readIORef (unitManagerRef env)

acolyte ∷ EngineEnv → IO UnitInstance
acolyte env = do
    insts ← instancesOf env
    case HM.lookup acolyteUid insts of
        Just inst → pure inst
        Nothing   → fail "fixture unit is missing from the unit manager"

storedStat ∷ EngineEnv → Text → IO (Maybe Float)
storedStat env name = HM.lookup name ∘ uiStats ⊚ acolyte env

storedStamina ∷ EngineEnv → IO (Maybe Float)
storedStamina env = storedStat env "stamina"

-- | The stat RNG as a comparable value: an example asserting "rolled
--   nothing" compares this across the call.
statGen ∷ EngineEnv → IO String
statGen env = show ⊚ readIORef (statRNGRef env)

-- | The live scene RENDERED rather than compared by 'Eq'. §7 asserts
--   "nothing moved" for a fixture whose stored stamina is NaN, and NaN
--   never equals itself — so a straight @shouldBe@ on the instances
--   would fail on the very case that most needs the check.
sceneSnapshot ∷ EngineEnv → IO String
sceneSnapshot env = show ⊚ instancesOf env

-- * Assertions

shouldBeNear ∷ IO (Maybe Float) → Float → Expectation
shouldBeNear act expected = act ⌦ \case
    Nothing → expectationFailure $
        "expected a stored value near " <> show expected <> ", found no entry"
    Just actual
        | abs (actual - expected) < 1e-4 → pure ()
        | otherwise → expectationFailure $
            "expected a stored value near " <> show expected
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

-- | @math.abs(expr - value) < 1e-4@. The stats are 'Float' and Lua
--   arithmetic is 'Double', so a Lua literal such as @3.55@ never
--   compares equal to what the manager actually holds.
luaNear ∷ Text → Float → Text
luaNear expr v = T.concat
    [ "math.abs((", expr, ") - ", tshow (realToFrac v ∷ Double), ") < 1e-4" ]

-- * Engine-side interleaving

-- | Charge the fixture unit for one swing through the PRODUCTION cost
--   function, against the same ref the verb commits to. This writes
--   BOTH stamina and stance, in one transaction.
applyStrike ∷ EngineEnv → AttackMode → IO ()
applyStrike env mode = do
    now ← readIORef (gameTimeRef env)
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (spendStrikeCost now mode (unUnitId acolyteUid) um, ())

-- | An UNRELATED concurrent write landing in the same window: another
--   stat in the very map the commit rewrites, plus a non-stat field.
--   Requirement 1 promises both survive.
applyUnrelatedWrite ∷ EngineEnv → IO ()
applyUnrelatedWrite env =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.adjust
                 (\i → i { uiStats = HM.insert "carrying_capacity" 77
                                        (uiStats i)
                         , uiInventory = [mkItem "rock" 7 1.0] })
                 acolyteUid (umInstances um) }, () )

-- | Overwrite the whole stat map from inside the window — used by the
--   cases where the BOUND, not the pool, moves during the calculation.
setStatsDirect ∷ EngineEnv → HM.HashMap Text Float → IO ()
setStatsDirect env stats =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.adjust (\i → i { uiStats = stats })
                 acolyteUid (umInstances um) }, () )

setModifiersDirect ∷ EngineEnv → HM.HashMap Text [StatModifier] → IO ()
setModifiersDirect env mods =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.adjust (\i → i { uiModifiers = mods })
                 acolyteUid (umInstances um) }, () )

-- | Expose an arbitrary engine-side mutation to Lua as @__hook(uid)@ so
--   an example can fire it from INSIDE the script's own rate
--   calculation.
installHook ∷ EngineEnv → LuaBackendState → IO () → IO ()
installHook _ ls act =
    Lua.runWith (lbsLuaState ls) (installer ∷ Lua.LuaE Lua.Exception ())
  where
    installer = do
        Lua.pushHaskellFunction fire
        Lua.setglobal "__hook"
    fire ∷ Lua.LuaE Lua.Exception Lua.NumResults
    fire = do
        Lua.liftIO act
        return 0

-- | Wrap @unit.getStat@ so the FIRST read of @trigger@ AFTER the
--   stamina snapshot runs @__hook@. That is exactly the window the old
--   read-modify-write lost, made deterministic.
--
--   @'endurance'@ is the regen read (requirement-era reproduction's own
--   injection point); @'caffeine'@ is the last read before the commit,
--   used when an example must move the BOUND without also moving the
--   rate the script already computed.
interposeAfterSnapshot ∷ LuaBackendState → Text → IO ()
interposeAfterSnapshot ls trigger = do
    r ← evalDebug ls $ T.concat
        [ "_G.__fired = false; local seen = false; "
        , "local real = unit.getStat; "
        , "unit.getStat = function(uid, name) "
        , "  if name == 'stamina' then seen = true end; "
        , "  if seen and name == '", trigger, "' and not _G.__fired then "
        , "    _G.__fired = true; __hook(uid); "
        , "  end; "
        , "  return real(uid, name); "
        , "end; return true" ]
    r `shouldBe` "true"

-- | Count kill\/collapse\/revive REQUESTS while still calling through to
--   the real verbs. The tick's job is to request the right consequence
--   from the right numbers; the verbs themselves are untouched by this
--   change.
installRequestRecorders ∷ LuaBackendState → IO ()
installRequestRecorders ls = do
    r ← evalDebug ls $ T.concat
        [ "_G.__killed = 0; _G.__collapsed = 0; _G.__revived = 0; "
        , "local k, c, v = unit.kill, unit.collapse, unit.revive; "
        , "unit.kill = function(...) "
        , "  _G.__killed = _G.__killed + 1; return k(...) end; "
        , "unit.collapse = function(...) "
        , "  _G.__collapsed = _G.__collapsed + 1; return c(...) end; "
        , "unit.revive = function(...) "
        , "  _G.__revived = _G.__revived + 1; return v(...) end; "
        , "return true" ]
    r `shouldBe` "true"

-- | Count @unit.commitStamina@ calls, so "the tick skipped entirely"
--   can be asserted as a fact rather than inferred from an unchanged
--   stat.
installCommitCounter ∷ LuaBackendState → IO ()
installCommitCounter ls = do
    r ← evalDebug ls $ T.concat
        [ "_G.__commits = 0; local real = unit.commitStamina; "
        , "unit.commitStamina = function(...) "
        , "  _G.__commits = _G.__commits + 1; return real(...) end; "
        , "return true" ]
    r `shouldBe` "true"

luaCount ∷ LuaBackendState → Text → IO Text
luaCount ls name = evalDebug ls ("return _G." <> name)

-- * Drivers

-- | Drive the SHIPPED tick with the SHIPPED config table for @defName@.
tickStaminaAs ∷ LuaBackendState → Text → Text → Text → Float → IO Text
tickStaminaAs ls defName activity pose dt = evalDebug ls $ T.concat
    [ "local cfg = require('scripts.unit_resource_config'); "
    , "require('scripts.unit_resource_tick').tickResource(1, '", defName
    , "', 'stamina', cfg.", defName, ".stamina, '", activity, "', '"
    , pose, "', ", tshow (realToFrac dt ∷ Double), "); return true" ]

-- | The ordinary case: acolyte, idle, standing.
tickStamina ∷ LuaBackendState → IO Text
tickStamina ls = tickStaminaAs ls "acolyte" "idle" "standing" tickDt

-- | The shipped acolyte table with extra keys merged in — the only way
--   to reach @death_threshold@ on stamina, which no shipped stamina
--   config sets.
tickStaminaWithExtra ∷ LuaBackendState → Text → Text → IO Text
tickStaminaWithExtra ls pose extra = evalDebug ls $ T.concat
    [ "local cfg = require('scripts.unit_resource_config'); "
    , "local p = {}; for k, v in pairs(cfg.acolyte.stamina) do p[k] = v end; "
    , extra
    , "require('scripts.unit_resource_tick').tickResource(1, 'acolyte', "
    , "'stamina', p, 'idle', '", pose, "', "
    , tshow (realToFrac tickDt ∷ Double), "); return true" ]

-- | Drive the SHIPPED cross-resource revive gate with the stamina entry
--   alone, which is the entry this change could plausibly have moved.
checkRevive ∷ LuaBackendState → IO Text
checkRevive ls = evalDebug ls $ T.concat
    [ "local cfg = require('scripts.unit_resource_config'); "
    , "require('scripts.unit_resource_tick').checkRevive(1, "
    , "{ stamina = cfg.acolyte.stamina }); return true" ]

-- | A permanent flat modifier.
permanent ∷ Float → StatModifier
permanent d = StatModifier
    { smDelta = d, smSource = "fixture", smExpiry = Nothing, smPercent = 0 }

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "Stamina resource commit" $ do

    -- §1 The defect: a debit committed inside the update's own window
    -- must survive it, along with everything else that transaction did.
    describe "the lost debit (§1)" $ do
        it "a heavy strike committing during the Lua rate calculation \
           \is not erased: 6.00 → 3.55, not 6.05" $ \env → do
            resetScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            -- 6.00 − 2.5 (heavy strike) + 0.05 (idle recovery). The old
            -- read-modify-write publishes 6.00 + 0.05 = 6.05, silently
            -- refunding the swing.
            storedStamina env `shouldBeNear` (6.0 - heavyCost + idleGain)
            evalDebug ls "return _G.__fired" `shouldReturn` "true"

        it "the strike's OTHER write — its stance debit — survives the \
           \stamina update too" $ \env → do
            resetScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            storedStat env "stance" `shouldBeNear` heavyStanceLeft

        it "an unrelated concurrent write landing in the same window — \
           \another stat in the same map, and a non-stat field — is \
           \preserved" $ \env → do
            resetScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env
            installHook env ls
                (applyStrike env Heavy ≫ applyUnrelatedWrite env)
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` (6.0 - heavyCost + idleGain)
            storedStat env "carrying_capacity" `shouldBeNear` 77
            inv ← uiInventory ⊚ acolyte env
            map iiDefName inv `shouldBe` ["rock"]

    -- §2 The other half of the defect: the thresholds read committed
    -- values, so a pool a strike zeroed inside the window still dies.
    describe "exhaustion from committed values (§2)" $ do
        it "2.00 → strike to zero → recovery 0.05 reports before 0 and \
           \after 0.05, and Lua requests death without collapse" $
          \env → do
            resetScene env (statsWithStamina 2.0)
            ls ← newBareLuaBackend env
            installRequestRecorders ls
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            -- 2.00 − 2.5 floors at 0, then +0.05 is committed.
            storedStamina env `shouldBeNear` idleGain
            luaCount ls "__killed" `shouldReturn` "1"
            luaCount ls "__collapsed" `shouldReturn` "0"

        it "the engine reports before 0 and after 0.05 for that same \
           \schedule" $ \env → do
            resetScene env (statsWithStamina 0.0)
            ls ← newBareLuaBackend env
            evalDebug ls (T.concat
                [ "local r = unit.commitStamina(1, "
                , tshow (realToFrac idleGain ∷ Double), "); "
                , "return r ~= nil and r.initialized == false and "
                , luaNear "r.before" 0, " and ", luaNear "r.after" idleGain
                , " and ", luaNear "r.maximum" fixtureMax ])
                `shouldReturn` "true"

        it "control: a zero pool with NO interleaving still requests \
           \death — the missed kill is a stale-state defect, not a rule" $
          \env → do
            resetScene env (statsWithStamina 0.0)
            ls ← newBareLuaBackend env
            installRequestRecorders ls

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` idleGain
            luaCount ls "__killed" `shouldReturn` "1"
            luaCount ls "__collapsed" `shouldReturn` "0"

        it "a strike that only drives the pool BELOW the collapse \
           \threshold requests a collapse and no kill" $ \env → do
            resetScene env (statsWithStamina 3.0)
            ls ← newBareLuaBackend env
            installRequestRecorders ls
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            -- 3.00 − 2.5 + 0.05 = 0.55; 0.055 of the pool, under the
            -- shipped collapse_threshold of 0.1. The old path compares
            -- 3.05 (0.305) and collapses nothing.
            storedStamina env `shouldBeNear` (3.0 - heavyCost + idleGain)
            luaCount ls "__collapsed" `shouldReturn` "1"
            luaCount ls "__killed" `shouldReturn` "0"

    -- §3 Either serial order is fine; only the stale overwrite is not.
    describe "serial orders and signed deltas (§3)" $ do
        it "debit before the update yields the serial result" $ \env → do
            resetScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env
            applyStrike env Heavy
            storedStamina env `shouldBeNear` (6.0 - heavyCost)

            tickStamina ls `shouldReturn` "true"
            storedStamina env `shouldBeNear` (6.0 - heavyCost + idleGain)

        it "update before the debit yields the serial result" $ \env → do
            resetScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env

            tickStamina ls `shouldReturn` "true"
            storedStamina env `shouldBeNear` (6.0 + idleGain)

            applyStrike env Heavy
            storedStamina env `shouldBeNear` (6.0 + idleGain - heavyCost)

        it "at the ceiling the two orders legitimately differ — recover \
           \then spend gives 7.50, spend then recover gives 7.54" $
          \env → do
            resetScene env (statsWithStamina 9.99)
            ls ← newBareLuaBackend env
            tickStamina ls `shouldReturn` "true"
            storedStamina env `shouldBeNear` fixtureMax
            applyStrike env Heavy
            storedStamina env `shouldBeNear` (fixtureMax - heavyCost)

            resetScene env (statsWithStamina 9.99)
            applyStrike env Heavy
            tickStamina ls `shouldReturn` "true"
            storedStamina env `shouldBeNear` (9.99 - heavyCost + idleGain)

        it "a NEGATIVE physiology delta — the shipped organ-failure \
           \branch — also survives an interleaved debit" $ \env → do
            -- fat_mass 0 with height 1 is below min_fat, so regen is
            -- overridden to 0 and ORGAN_FAILURE_DRAIN_PER_SEC runs: the
            -- amount is −0.05, not +0.05.
            resetScene env (HM.union (HM.fromList
                [("fat_mass", 0.0), ("height", 1.0)])
                (statsWithStamina 6.0))
            ls ← newBareLuaBackend env
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` (6.0 - heavyCost - idleGain)

        it "the returned after is exactly the value committed" $ \env → do
            resetScene env (statsWithStamina 4.0)
            ls ← newBareLuaBackend env
            evalDebug ls "_G.__r = unit.commitStamina(1, 0.125); \
                         \return _G.__r ~= nil" `shouldReturn` "true"
            committed ← storedStamina env
            case committed of
                Nothing → expectationFailure "no stamina was committed"
                Just v  → evalDebug ls ("return _G.__r.after == "
                            <> tshow (realToFrac v ∷ Double))
                              `shouldReturn` "true"

    -- §4 Initialisation is decided at COMMIT, never from an earlier
    -- script-side nil.
    describe "initialisation at commit (§4)" $ do
        it "a still-absent pool is filled to the effective maximum and \
           \reports initialized with no before" $ \env → do
            resetScene env statsWithoutStamina
            ls ← newBareLuaBackend env
            evalDebug ls (T.concat
                [ "local r = unit.commitStamina(1, 0.05); "
                , "return r ~= nil and r.before == nil and "
                , "r.initialized == true and "
                , luaNear "r.after" fixtureMax, " and "
                , luaNear "r.maximum" fixtureMax ])
                `shouldReturn` "true"
            storedStamina env `shouldBeNear` fixtureMax

        it "the shipped tick fills an absent pool and skips that pass's \
           \consequences, exactly as the old first-tick return did" $
          \env → do
            resetScene env statsWithoutStamina
            ls ← newBareLuaBackend env
            installRequestRecorders ls

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` fixtureMax
            luaCount ls "__killed" `shouldReturn` "0"
            luaCount ls "__collapsed" `shouldReturn` "0"

        it "a pool combat CREATED after the nil observation is NOT \
           \refilled — the delta path runs and initialized is false" $
          \env → do
            resetScene env statsWithoutStamina
            ls ← newBareLuaBackend env
            -- The strike materialises the entry (absent reads as 0, so
            -- the debit stores 0) in the window between the script's nil
            -- read and the commit.
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"
            installRequestRecorders ls

            tickStamina ls `shouldReturn` "true"

            -- The old code refilled to 10 here, handing back the whole
            -- pool; the pre-recovery zero also has to still kill.
            storedStamina env `shouldBeNear` idleGain
            luaCount ls "__killed" `shouldReturn` "1"

        it "a pool SPENT after the nil observation takes the delta path \
           \and reports initialized = false" $ \env → do
            resetScene env statsWithoutStamina
            ls ← newBareLuaBackend env
            installHook env ls
                (setStatsDirect env (statsWithStamina 6.0))
            interposeAfterSnapshot ls "endurance"

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` (6.0 + idleGain)

    -- §5 The bound comes from the COMMITTING record, at one game-time
    -- sample, not from what the script read on the way in.
    describe "the bound at commit (§5)" $ do
        it "endurance-derived: the maximum reported is endurance × 10" $
          \env → do
            resetScene env (HM.insert "endurance" 3.0
                                (statsWithStamina 5.0))
            ls ← newBareLuaBackend env
            evalDebug ls ("local r = unit.commitStamina(1, 0); return "
                          <> luaNear "r.maximum" 30.0) `shouldReturn` "true"

        it "an explicit max_stamina takes precedence over endurance" $
          \env → do
            resetScene env (HM.insert "max_stamina" 4.0
                                (statsWithStamina 3.0))
            ls ← newBareLuaBackend env
            evalDebug ls ("local r = unit.commitStamina(1, 0); return "
                          <> luaNear "r.maximum" 4.0) `shouldReturn` "true"

        it "an explicit maximum SHRUNK during the rate calculation \
           \clamps the commit, and the reported maximum matches" $
          \env → do
            resetScene env (HM.insert "max_stamina" 10.0
                                (statsWithStamina 6.0))
            ls ← newBareLuaBackend env
            installHook env ls (setStatsDirect env
                (HM.insert "max_stamina" 3.0 (statsWithStamina 6.0)))
            -- caffeine is the LAST read before the commit, so the rate
            -- the script already computed is untouched by the change.
            interposeAfterSnapshot ls "caffeine"

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` 3.0

        it "a MODIFIER the maximum depends on, added during the rate \
           \calculation, moves the bound the commit uses" $ \env → do
            resetScene env (statsWithStamina 9.5)
            ls ← newBareLuaBackend env
            installHook env ls (setModifiersDirect env
                (HM.singleton "endurance" [permanent 1.0]))
            interposeAfterSnapshot ls "caffeine"

            tickStamina ls `shouldReturn` "true"

            -- endurance 1 → 2, so the pool is 20 and 9.55 fits under it
            -- rather than being clamped to the stale 10.
            storedStamina env `shouldBeNear` (9.5 + idleGain)
            evalDebug ls ("local r = unit.commitStamina(1, 0); return "
                          <> luaNear "r.maximum" 20.0) `shouldReturn` "true"

        it "an expired modifier stops counting, at the single captured \
           \game-time sample" $ \env → do
            resetSceneMods env (statsWithStamina 5.0)
                (HM.singleton "endurance"
                    [(permanent 1.0) { smExpiry = Just 1.0 }])
            ls ← newBareLuaBackend env
            evalDebug ls ("local r = unit.commitStamina(1, 0); return "
                          <> luaNear "r.maximum" 20.0) `shouldReturn` "true"
            writeIORef (gameTimeRef env) 2.0
            evalDebug ls ("local r = unit.commitStamina(1, 0); return "
                          <> luaNear "r.maximum" 10.0) `shouldReturn` "true"

        it "the no-maximum skip path still runs entirely in Lua: an \
           \undefined endurance skips the tick without an engine call" $
          \env → do
            resetScene env (HM.singleton "stamina" 6.0)
            ls ← newBareLuaBackend env
            installCommitCounter ls
            installRequestRecorders ls

            tickStamina ls `shouldReturn` "true"

            luaCount ls "__commits" `shouldReturn` "0"
            storedStamina env `shouldBeNear` 6.0
            luaCount ls "__killed" `shouldReturn` "0"

        it "a non-positive maximum skips the tick the same way" $
          \env → do
            resetScene env (HM.insert "endurance" 0.0
                                (statsWithStamina 6.0))
            ls ← newBareLuaBackend env
            installCommitCounter ls

            tickStamina ls `shouldReturn` "true"

            luaCount ls "__commits" `shouldReturn` "0"
            storedStamina env `shouldBeNear` 6.0

    -- §6 The stored base, not the modifier-adjusted effective value.
    describe "stored base vs effective value (§6)" $
        it "a modifier ON stamina moves what getStat reports but is \
           \never compounded into the stored base" $ \env → do
            resetSceneMods env (statsWithStamina 5.0)
                (HM.singleton "stamina" [permanent 0.3])
            ls ← newBareLuaBackend env
            -- getStat reports 5.3; the base combat spends is 5.0.
            evalDebug ls ("return " <> luaNear "unit.getStat(1, 'stamina')" 5.3)
                `shouldReturn` "true"

            tickStamina ls `shouldReturn` "true"

            storedStamina env `shouldBeNear` (5.0 + idleGain)
            mods ← uiModifiers ⊚ acolyte env
            HM.lookup "stamina" mods `shouldBe` Just [permanent 0.3]

    -- §7 Arithmetic edges, the in-transaction write elision, and every
    -- refusal.
    describe "deltas, saturation and refusals (§7)" $ do
        it "a zero delta at zero stamina still commits and reports \
           \before 0, after 0" $ \env → do
            resetScene env (statsWithStamina 0.0)
            ls ← newBareLuaBackend env
            evalDebug ls (T.concat
                [ "local r = unit.commitStamina(1, 0); "
                , "return r ~= nil and r.initialized == false and "
                , luaNear "r.before" 0, " and ", luaNear "r.after" 0 ])
                `shouldReturn` "true"

        it "a zero delta still drives the thresholds — the script-side \
           \1e-4 elision can no longer bypass them" $ \env → do
            resetScene env (statsWithStamina 0.0)
            ls ← newBareLuaBackend env
            installRequestRecorders ls
            -- dt = 0 makes the amount exactly zero while the shipped
            -- config and the whole code path stay real.
            tickStaminaAs ls "acolyte" "idle" "standing" 0
                `shouldReturn` "true"
            luaCount ls "__killed" `shouldReturn` "1"

        it "a NEGLIGIBLE delta with an intervening debit commits \
           \against what storage holds, not the stale snapshot" $
          \env → do
            resetScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            -- dt 4e-6 ⇒ amount 2e-6, well under the old 1e-4 write
            -- threshold, so the old path wrote nothing at all.
            tickStaminaAs ls "acolyte" "idle" "standing" 4e-6
                `shouldReturn` "true"

            st ← storedStamina env
            case st of
                Nothing → expectationFailure "no stamina was committed"
                Just v  → do
                    v `shouldSatisfy` (> (6.0 - heavyCost))
                    v `shouldSatisfy` (< (6.0 - heavyCost + 1e-4))

        it "a huge finite delta saturates at the maximum instead of \
           \publishing an infinity" $ \env → do
            resetScene env (statsWithStamina 1.0)
            ls ← newBareLuaBackend env
            evalDebug ls ("local r = unit.commitStamina(1, 1e40); return "
                          <> luaNear "r.after" fixtureMax)
                `shouldReturn` "true"
            storedStamina env `shouldReturn` Just fixtureMax

        it "a huge finite NEGATIVE delta saturates at zero" $ \env → do
            resetScene env (statsWithStamina 9.0)
            ls ← newBareLuaBackend env
            evalDebug ls "local r = unit.commitStamina(1, -1e40); \
                         \return r.after == 0" `shouldReturn` "true"
            storedStamina env `shouldReturn` Just 0.0

        it "a stored value above the maximum is clamped down, even by a \
           \zero delta" $ \env → do
            resetScene env (statsWithStamina 14.0)
            ls ← newBareLuaBackend env
            evalDebug ls ("local r = unit.commitStamina(1, 0); return "
                          <> luaNear "r.after" fixtureMax)
                `shouldReturn` "true"
            storedStamina env `shouldReturn` Just fixtureMax

        it "a stored value below zero is clamped up, even by a zero \
           \delta" $ \env → do
            resetScene env (statsWithStamina (-2.0))
            ls ← newBareLuaBackend env
            evalDebug ls "local r = unit.commitStamina(1, 0); \
                         \return r.after == 0" `shouldReturn` "true"
            storedStamina env `shouldReturn` Just 0.0

        it "an id no unit answers to is refused as no_such_unit" $
          \env → do
            resetScene env (statsWithStamina 5.0)
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.commitStamina(99, 0.05)"
                "no_such_unit"

        it "a unit removed before the commit is refused as \
           \no_such_unit and is never recreated" $ \env → do
            resetScene env (statsWithStamina 5.0)
            ls ← newBareLuaBackend env
            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "acolyte" acolyteDef }
            expectRefusal env ls "unit.commitStamina(1, 0.05)"
                "no_such_unit"
            insts ← instancesOf env
            HM.member acolyteUid insts `shouldBe` False

        it "every malformed id is refused as invalid_unit_id, and a \
           \numeric string never coerces onto that unit" $ \env → do
            resetScene env (statsWithStamina 5.0)
            ls ← newBareLuaBackend env
            forM_ [ "'1'", "1.5", "-1", "4294967296", "nil", "true"
                  , "{}" ] $ \arg →
                expectRefusal env ls
                    ("unit.commitStamina(" <> arg <> ", 0.05)")
                    "invalid_unit_id"

        it "every malformed delta is refused as invalid_amount, while a \
           \NEGATIVE delta is ordinary input" $ \env → do
            resetScene env (statsWithStamina 5.0)
            ls ← newBareLuaBackend env
            forM_ [ "'0.05'", "0/0", "math.huge", "-math.huge", "nil"
                  , "true", "{}" ] $ \arg →
                expectRefusal env ls
                    ("unit.commitStamina(1, " <> arg <> ")")
                    "invalid_amount"
            evalDebug ls ("local r = unit.commitStamina(1, -0.5); return "
                          <> luaNear "r.after" 4.5) `shouldReturn` "true"

        it "a non-finite stored stamina is refused as invalid_stamina \
           \without mutating" $ \env → do
            resetScene env (statsWithStamina (0 / 0))
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.commitStamina(1, 0.05)"
                "invalid_stamina"

            resetScene env (statsWithStamina (1 / 0))
            ls2 ← newBareLuaBackend env
            expectRefusal env ls2 "unit.commitStamina(1, 0.05)"
                "invalid_stamina"

        it "a non-positive or non-finite resolved maximum is refused as \
           \invalid_maximum without mutating" $ \env → do
            forM_ [0.0, -4.0, 1 / 0, 0 / 0] $ \m → do
                resetScene env (HM.insert "max_stamina" m
                                    (statsWithStamina 5.0))
                ls ← newBareLuaBackend env
                expectRefusal env ls "unit.commitStamina(1, 0.05)"
                    "invalid_maximum"

        it "a malformed argument is refused BEFORE the unit lookup, so \
           \an absent pool is still never created" $ \env → do
            resetScene env statsWithoutStamina
            ls ← newBareLuaBackend env
            expectRefusal env ls "unit.commitStamina(1, 0/0)"
                "invalid_amount"
            storedStamina env `shouldReturn` Nothing

        it "the shipped caller RAISES the refusal reason, so the \
           \engine's callback isolation can report it" $ \env → do
            resetScene env (statsWithStamina 5.0)
            ls ← newBareLuaBackend env
            -- An explicit max_stamina keeps the Lua eligibility gate
            -- satisfied while the engine's own resolution refuses.
            evalDebug ls "return true" `shouldReturn` "true"
            atomicModifyIORef' (unitManagerRef env) $ \um →
                ( um { umInstances = HM.adjust
                         (\i → i { uiStats = HM.insert "stamina" (0 / 0)
                                                 (uiStats i) })
                         acolyteUid (umInstances um) }, () )
            raised ← tickStamina ls
            raised `shouldSatisfy` T.isInfixOf "invalid_stamina"
            raised `shouldSatisfy` T.isPrefixOf "error:"

    -- §8 The consequence rules, their pose guards, the separate revive
    -- gate, and a SECOND shipped configuration.
    describe "thresholds, pose guards and a second config (§8)" $ do
        it "death_threshold fires from the committed values, before the \
           \pool has reached the kill_on_zero rule" $ \env → do
            resetScene env (statsWithStamina 0.2)
            ls ← newBareLuaBackend env
            installRequestRecorders ls
            -- 0.25 → 0.25 of the pool is 0.025, under a 0.05 threshold,
            -- while neither before nor after is zero.
            tickStaminaWithExtra ls "standing" "p.death_threshold = 0.05; "
                `shouldReturn` "true"
            luaCount ls "__killed" `shouldReturn` "1"
            luaCount ls "__collapsed" `shouldReturn` "0"

        it "a dead unit is neither killed again nor collapsed, and the \
           \commit still happens" $ \env → do
            resetScenePosed env (statsWithStamina 0.0) "dead"
            ls ← newBareLuaBackend env
            installRequestRecorders ls

            tickStaminaAs ls "acolyte" "idle" "dead" tickDt
                `shouldReturn` "true"

            storedStamina env `shouldBeNear` idleGain
            luaCount ls "__killed" `shouldReturn` "0"
            luaCount ls "__collapsed" `shouldReturn` "0"

        it "an already-collapsed unit is not re-collapsed, but is still \
           \killed by a committed zero" $ \env → do
            resetScenePosed env (statsWithStamina 0.0) "collapsed"
            ls ← newBareLuaBackend env
            installRequestRecorders ls

            tickStaminaAs ls "acolyte" "idle" "collapsed" tickDt
                `shouldReturn` "true"

            luaCount ls "__killed" `shouldReturn` "1"
            luaCount ls "__collapsed" `shouldReturn` "0"

        it "checkRevive still gates on the resource ratio it always \
           \did, reading the pool directly" $ \env → do
            resetScenePosed env (statsWithStamina 6.0) "collapsed"
            ls ← newBareLuaBackend env
            installRequestRecorders ls
            checkRevive ls `shouldReturn` "true"
            luaCount ls "__revived" `shouldReturn` "1"

            resetScenePosed env (statsWithStamina 3.0) "collapsed"
            ls2 ← newBareLuaBackend env
            installRequestRecorders ls2
            checkRevive ls2 `shouldReturn` "true"
            luaCount ls2 "__revived" `shouldReturn` "0"

        it "the bear_brown stamina config behaves identically through \
           \the same window" $ \env → do
            resetBearScene env (statsWithStamina 6.0)
            ls ← newBareLuaBackend env
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStaminaAs ls "bear_brown" "idle" "standing" tickDt
                `shouldReturn` "true"

            storedStamina env `shouldBeNear` (6.0 - heavyCost + idleGain)

        it "the bear_brown config's kill_on_zero also fires from the \
           \committed before" $ \env → do
            resetBearScene env (statsWithStamina 2.0)
            ls ← newBareLuaBackend env
            installRequestRecorders ls
            installHook env ls (applyStrike env Heavy)
            interposeAfterSnapshot ls "endurance"

            tickStaminaAs ls "bear_brown" "idle" "standing" tickDt
                `shouldReturn` "true"

            luaCount ls "__killed" `shouldReturn` "1"
            luaCount ls "__collapsed" `shouldReturn` "0"
