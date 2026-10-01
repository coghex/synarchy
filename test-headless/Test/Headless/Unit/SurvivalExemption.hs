-- | The survival-physiology exemption, unit by unit (#2754).
--
--   "Test.Headless.Unit.RuinOccupantSurvival" proves the exemption in
--   the reported world. These specs pin its edges on a one-page scene:
--   the SHIPPED @scripts\/unit_resources.lua@ (and everything it ticks)
--   runs through the REAL registered @unit.*@ API over a REAL
--   'unitManagerRef', with a nomad_primitive and an acolyte side by side
--   on identical stats. Only the climate is controlled: this harness has
--   no generated world, so @world.getClimateAt@ \/ @world.getAmbientAt@
--   are replaced with a fixed environment, which is the input thermo
--   reads and nothing else.
--
--   Deaths are read off @unitQueue@: @unit.kill@ only enqueues, and no
--   unit thread runs here to apply it.
module Test.Headless.Unit.SurvivalExemption (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (modifyIORef', writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), emptyWorldManager, emptyWorldState)

nomadUid, acolyteUid ∷ UnitId
nomadUid   = UnitId 1
acolyteUid = UnitId 2

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "survival_exemption_page"

-- | A healthy adult's stored stats, shared by both units so the only
--   difference between them is the definition.
healthy ∷ HM.HashMap Text Float
healthy = HM.fromList
    [ ("body_mass", 70), ("fat_mass", 14), ("lean_mass", 56), ("height", 1.8)
    , ("endurance", 1), ("constitution", 1), ("metabolism", 1)
    , ("hydration", 42), ("max_hydration", 42)
    , ("hunger", 700), ("max_hunger", 700)
    , ("calories", 1400), ("max_calories", 1400)
    , ("core_temp", 37), ("salt", 70), ("salt_conc", 1)
    , ("consciousness", 1), ("blood_oxygen", 1), ("heart_rate", 70) ]

mkUnit ∷ Text → HM.HashMap Text Float → Float → [Wound] → UnitInstance
mkUnit defName stats blood ws = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFaction = resolveLegacyFaction [] FactionHostile, uiWounds = ws
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = blood, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Full blood volume for 'healthy': @body_mass × bloodMassRatio@.
fullBlood ∷ Float
fullBlood = 70 * 0.075

-- | Both units on one ACTIVE page (unit.getAllIds lists the active page
--   only), with the given stats, blood and wounds each.
scene ∷ EngineEnv → HM.HashMap Text Float → Float → [Wound] → IO ()
scene env stats blood ws = do
    page ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, page)], wmVisible = [fixturePage] }
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("nomad_primitive", minimalDef "nomad_primitive" "Nomad")
            , ("acolyte", minimalDef "acolyte" "Acolyte") ]
        , umInstances = HM.fromList
            [ (nomadUid, mkUnit "nomad_primitive" stats blood ws)
            , (acolyteUid, mkUnit "acolyte" stats blood ws) ] }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- | The shipped modules (unit_ai first: its satellites extend the
--   singleton), the controlled climate, and a runner that ticks the real
--   physiology update and reports, per unit, the extremes it reached.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    ok ← luaText ls $ T.unlines
        [ "local ai = require('scripts.unit_ai')"
        , "local resources = require('scripts.unit_resources')"
        , "resources.init(0)"
        , "_G.__resources = resources"
        , "_G.__env = { temp = 20, humidity = 0.5 }"
        , "world.getClimateAt = function() return { temp = __env.temp,"
        , "  summerTemp = __env.temp, winterTemp = __env.temp,"
        , "  precip = 0.2, humidity = __env.humidity, snow = 0 } end"
        , "world.getAmbientAt = function() return __env.temp end"
        , "local brain = require('scripts.brain')"
        , "local SURVIVAL = { 'hypothermia', 'hyperthermia', 'salt_imbalance' }"
        -- Per unit: largest |core - 37|, largest |salt_conc - 1|, lowest
        -- consciousness, largest survival meter, and whether it was wounded.
        , "function _G.__run(seconds)"
        , "  local w = {}"
        , "  for _, uid in ipairs({ 1, 2 }) do"
        , "    w[uid] = { core = 0, salt = 0, cons = 1, meter = 0 }"
        , "  end"
        , "  for _ = 1, math.floor(seconds * 10 + 0.5) do"
        , "    resources.update(0.1)"
        , "    for uid, r in pairs(w) do"
        , "      r.core = math.max(r.core, math.abs((unit.getStat(uid, 'core_temp') or 37) - 37))"
        , "      r.salt = math.max(r.salt, math.abs((unit.getStat(uid, 'salt_conc') or 1) - 1))"
        , "      r.cons = math.min(r.cons, brain.consciousness(uid))"
        , "      for _, m in ipairs(SURVIVAL) do"
        , "        r.meter = math.max(r.meter, unit.getStat(uid, m) or 0)"
        , "      end"
        , "    end"
        , "  end"
        , "  local out = {}"
        , "  for _, uid in ipairs({ 1, 2 }) do"
        , "    local r = w[uid]"
        , "    out[#out + 1] = string.format('%.4f,%.4f,%.4f,%.4f,%d',"
        , "      r.core, r.salt, r.cons, r.meter, #(unit.getWounds(uid) or {}))"
        , "  end"
        , "  return table.concat(out, ';')"
        , "end"
        , "return true" ]
    ok `shouldBe` "true"
    pure ls

luaText ∷ LuaBackendState → Text → IO Text
luaText ls src = T.strip ∘ T.dropAround (≡ '"') <$> evalDebug ls src

data Extremes = Extremes
    { exCore ∷ Double, exSalt ∷ Double, exCons ∷ Double
    , exMeter ∷ Double, exWounds ∷ Int }
    deriving Show

-- | Run the physiology for @seconds@ and return (nomad, acolyte).
run ∷ LuaBackendState → Double → IO (Extremes, Extremes)
run ls seconds = do
    out ← luaText ls ("return __run(" <> tshow seconds <> ")")
    case map (T.splitOn ",") (T.splitOn ";" out) of
        [n, a] → pure (parse n, parse a)
        _ → error ("unexpected __run result: " <> T.unpack out)
  where
    parse [c, s, k, m, w] =
        Extremes (readT c) (readT s) (readT k) (readT m) (round (readT w ∷ Double))
    parse other = error ("unexpected unit row: " <> show other)
    readT ∷ Read a ⇒ Text → a
    readT = read ∘ T.unpack

-- | The units a tick asked to kill.
killed ∷ EngineEnv → IO [UnitId]
killed env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [u | UnitKill u ← cmds]

-- | The units a tick asked to stand back up.
revivals ∷ EngineEnv → IO [UnitId]
revivals env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [u | UnitRevive u ← cmds]

-- | Put one fixture unit into a pose, as a restored save would carry it.
setPose ∷ EngineEnv → UnitId → Text → IO ()
setPose env uid pose = modifyIORef' (unitManagerRef env) $ \um →
    um { umInstances = HM.adjust (\i → i { uiPose = pose }) uid (umInstances um) }

-- | A stat as the nomad reads it through the real API.
nomadReads ∷ LuaBackendState → Text → IO Text
nomadReads ls expr = luaText ls ("local uid = 1; return " <> expr)

neutralNomad ∷ Extremes → Expectation
neutralNomad n = do
    exCore n `shouldBe` 0
    exSalt n `shouldBe` 0
    exCons n `shouldBe` 1
    exMeter n `shouldBe` 0
    exWounds n `shouldBe` 0

punctureLung ∷ Wound
punctureLung = Wound
    { woundPart = "lungs", woundKind = "internal", woundSeverity = 0.95
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

spec ∷ Spec
spec = describe "survival exemption" $ do
    forM_ [ ("lethal heat", 60, 0.9, healthy)
          , ("lethal cold", -40, 0.3, healthy)
          , ("no water and no salt", 20, 0.5
            , HM.insert "hydration" 0 $ HM.insert "salt" 0
                $ HM.insert "salt_conc" 0.1 healthy) ] $
      \(label, temp, humidity, stats) →
        it ("keeps a nomad neutral and alive under " <> label
            <> " while an acolyte degrades") $
          withHeadlessEngineNoWorld $ \env → do
            scene env stats fullBlood []
            ls ← setupLua env
            _ ← luaText ls ("__env.temp = " <> tshow (temp ∷ Double)
                            <> "; __env.humidity = " <> tshow (humidity ∷ Double)
                            <> "; return true")
            (nomad, acolyte) ← run ls 600
            dead ← killed env
            neutralNomad nomad
            dead `shouldNotContain` [nomadUid]
            -- The acolyte's path is the unchanged shipped one: it is
            -- asked to die, or its survival meters and consciousness
            -- show the same condition taking hold.
            unless (acolyteUid `elem` dead) $ do
                exMeter acolyte `shouldSatisfy` (> 0)
                exCons acolyte `shouldSatisfy` (< 1)

    it "neutralizes a restored nomad's stale survival stats and meters \
       \while paused, leaving injury meters and the acolyte alone" $
      withHeadlessEngineNoWorld $ \env → do
        let stale = HM.union (HM.fromList
              [ ("salt_conc", 0.2), ("salt", 5), ("core_temp", 43)
              , ("hyperthermia", 0.9), ("hypothermia", 0.3)
              , ("salt_imbalance", 0.9), ("consciousness", 0.1)
              , ("state_of_mind", 0.1), ("sepsis", 0.4) ]) healthy
        scene env stale fullBlood []
        writeIORef (enginePausedRef env) True
        ls ← setupLua env
        _ ← luaText ls "__resources.onSaveLoaded({ 1, 2 }, {}); return true"
        -- No physiology tick has run: these are what a paused load shows.
        nomadReads ls "require('scripts.brain').state(uid)" `shouldReturn` "alert"
        nomadReads ls "unit.getStat(uid, 'consciousness') == 1" `shouldReturn` "true"
        nomadReads ls "unit.getStat(uid, 'state_of_mind') > 0.1" `shouldReturn` "true"
        nomadReads ls "unit.getStat(uid, 'core_temp') == 37" `shouldReturn` "true"
        nomadReads ls "unit.getStat(uid, 'salt_conc') == 1" `shouldReturn` "true"
        nomadReads ls "require('scripts.salts').speedMultiplier(uid) == 1" `shouldReturn` "true"
        nomadReads ls "require('scripts.salts').stateLabel(uid)" `shouldReturn` "Balanced"
        nomadReads ls "require('scripts.thermo').stateLabel(uid) == \
                      \require('scripts.thermo').stateLabel(-1)" `shouldReturn` "true"
        luaText ls (T.concat
            [ "local m = __resources.meterInfo(1); "
            , "return m.hypothermia.value + m.hypothermia.fail "
            , "  + m.hyperthermia.value + m.hyperthermia.fail "
            , "  + m.salt_imbalance.value + m.salt_imbalance.fail == 0" ])
            `shouldReturn` "true"
        -- An injury meter is not survival physiology: it survives the load.
        nomadReads ls "string.format('%.2f', __resources.meterInfo(uid).sepsis.value)"
            `shouldReturn` "0.40"
        -- The acolyte's stale state is its own, untouched by the hook.
        luaText ls "return unit.getStat(2, 'salt_imbalance') > 0.8 \
                   \and unit.getStat(2, 'core_temp') > 42"
            `shouldReturn` "true"
        -- And the paused update leaves everything where the hook put it.
        _ ← luaText ls "__resources.update(0.1); return true"
        nomadReads ls "unit.getStat(uid, 'core_temp') == 37" `shouldReturn` "true"

    it "stands a nomad restored collapsed from a survival failure back up \
       \once the load is unpaused" $
      withHeadlessEngineNoWorld $ \env → do
        let stale = HM.union (HM.fromList
              [ ("salt_conc", 0.2), ("salt", 5), ("salt_imbalance", 0.9)
              , ("consciousness", 0.1) ]) healthy
        scene env stale fullBlood []
        setPose env nomadUid "collapsed"
        ls ← setupLua env
        _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"
        _ ← Q.flushQueue (unitQueue env)
        _ ← run ls 1
        revived ← revivals env
        revived `shouldContain` [nomadUid]

    it "keeps a collapsed nomad down while blood loss still gates the revive" $
      withHeadlessEngineNoWorld $ \env → do
        scene env healthy (fullBlood * 0.4) []
        setPose env nomadUid "collapsed"
        ls ← setupLua env
        _ ← run ls 1
        revived ← revivals env
        revived `shouldNotContain` [nomadUid]

    it "keeps blood loss impairing an exempt nomad's circulation and oxygen" $
      withHeadlessEngineNoWorld $ \env → do
        scene env healthy (fullBlood * 0.25) []
        ls ← setupLua env
        _ ← run ls 60
        nomadReads ls "unit.getStat(uid, 'circulation') < 0.5" `shouldReturn` "true"
        nomadReads ls "unit.getStat(uid, 'heart_rate') > 100" `shouldReturn` "true"
        nomadReads ls "unit.getStat(uid, 'blood_oxygen') < 0.95" `shouldReturn` "true"
        -- The acolyte beside it is impaired alike: blood loss is not
        -- survival physiology. (Its own core drifts a few hundredths of a
        -- degree at 20 °C, which is the only difference between them.)
        luaText ls (T.concat
            [ "local a, b = unit.getStat(1, 'circulation'), unit.getStat(2, 'circulation'); "
            , "return math.abs(a - b) < 0.01" ]) `shouldReturn` "true"

    it "still lets a punctured lung suffocate an exempt nomad" $
      withHeadlessEngineNoWorld $ \env → do
        scene env healthy fullBlood [punctureLung]
        ls ← setupLua env
        _ ← run ls 400
        nomadReads ls "unit.getStat(uid, 'hypoxia') == 1" `shouldReturn` "true"
        dead ← killed env
        dead `shouldContain` [nomadUid]
