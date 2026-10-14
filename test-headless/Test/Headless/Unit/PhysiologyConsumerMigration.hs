{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology consumer migration" gate (#2830, S5 of epic #2827).
--
--   S5 moves every gameplay reader and cross-system writer of a
--   registry-owned key (hydration, hunger, calories, core_temp, salt,
--   salt_store, salt_conc and the three failure meters) behind
--   @scripts\/physiology.lua@ and Haskell's 'physioConsumerView'. While
--   production modes are INACTIVE that move must change nothing, so this
--   gate runs the SHIPPED consumer modules over the REAL registered
--   @unit.*@ API and compares them with the paths they replaced.
--
--   __Three comparison styles, each used where it is honest.__
--
--     * /Twin runs./ Two identical units; the migrated operation runs on
--       one and the verbatim pre-S5 statements (held in @legacyLua@, below)
--       on the other; the stored stats and the returned amount must match
--       exactly. Used for the facade's own operations, where the old
--       statements are a handful of lines.
--     * /Pinned module scenarios./ A whole shipped module (thermo, salts,
--       brain, cardio, starvation, the AI need utilities, the consumable
--       drink, the status rows) is run over a labelled set of states and
--       its output compared with the literal recorded by running the same
--       scenario against the pre-S5 modules at 0482a98ef. Used where the
--       old path is a whole module.
--     * /Wound tick./ 'tickOneUnitWith' under the SHIPPED registry (inactive)
--       against 'tickOneUnit', and both against the pure formulas.
--
--   __The inactive boundary is tested where it is enforced.__ A fixture
--   definition that resolves EVERY v1 system to Nominal is registered with
--   the shipped registry loaded: the resolver would answer healthy values,
--   and the migrated Lua, @unit.readPhysio@ (as the probe calls it
--   directly) and the wound tick must still return the stored ones. The
--   contrast case activates the registry, the only way a controlled test
--   reaches the mode-aware answer.
--
--   Fixed inputs throughout: states are literals and the stat generator is
--   never rolled (no authored template is read). The climate is the one
--   world input replaced, as in "Test.Headless.Unit.SaltReserve".
module Test.Headless.Unit.PhysiologyConsumerMigration (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (modifyIORef', writeIORef)
import qualified System.Random as Random
import Combat.Wounds (WoundTickOutcome(..), tickOneUnit, tickOneUnitWith)
import Combat.Wounds.Healing (bloodRecoveryDelta, calorieHealMultiplier)
import Combat.Wounds.Infection
    ( feverSuppressFloor, feverSuppressK, immuneClearRate, immuneRampRate
    , immuneSeed )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Infection.Types (emptyInfectionManager)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.PhysiologyFixture
    ( activatePhysiologyModes, ensurePhysiologyRegistry
    , readShippedPhysiologyRegistry )
import Test.Headless.Unit.TransferApi (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology.Types
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), emptyWorldManager, emptyWorldState)

-----------------------------------------------------------------------
-- The scene
-----------------------------------------------------------------------

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "physiology_consumer_migration"

-- | A definition that resolves EVERY v1 system to Nominal: the resolver
--   would answer healthy values for each owned key. Inactive production
--   must not let that reach a consumer.
nominalPhysiology ∷ UnitPhysiology
nominalPhysiology = UnitPhysiology True Nothing $ HM.fromList
    [ (s, PhysioNominal) | s ← ["thermo", "salt", "hydration", "nutrition"] ]

mkInst ∷ Text → HM.HashMap Text Float → UnitInstance
mkInst defName stats = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = []
    , uiFaction = resolveLegacyFaction [] FactionPlayer
    , uiWounds = [], uiScars = [], uiImmuneResponse = 0
    , uiImmunities = HM.empty, uiBlood = 70 * 0.075
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides
    , uiTrailState = Nothing
    }

-- | Units 1 and 2 are TWINS (identical stats, run side by side); unit 3
--   is the same stats under the all-Nominal definition. The shipped
--   registry is loaded and, as in production, INACTIVE.
resetScene ∷ EngineEnv → [(Text, Float)] → IO ()
resetScene env stats = do
    ensurePhysiologyRegistry env
    page ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, page)], wmVisible = [fixturePage] }
    writeIORef (gameTimeRef env) 0
    let st = HM.fromList stats
        defs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("nominal", (minimalDef "nominal" "Nominal")
                              { udPhysiology = nominalPhysiology }) ]
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = defs
        , umInstances = HM.fromList
            [ (UnitId 1, mkInst "acolyte" st), (UnitId 2, mkInst "acolyte" st)
            , (UnitId 3, mkInst "nominal" st) ] }

-- | A reference body of @mass@ kg with every pool full, scaled the way
--   the shipped body block scales them (0.6 L water, 10 kcal hunger, 20
--   kcal calories per kg).
bodyStats ∷ Float → [(Text, Float)]
bodyStats mass =
    [ ("body_mass", mass), ("fat_mass", 0.2 * mass), ("lean_mass", 0.8 * mass)
    , ("height", 1.8), ("endurance", 1), ("constitution", 1), ("metabolism", 1)
    , ("hydration", 0.6 * mass), ("max_hydration", 0.6 * mass)
    , ("hunger", 10 * mass), ("max_hunger", 10 * mass)
    , ("calories", 20 * mass), ("max_calories", 20 * mass)
    , ("stamina", 100), ("max_stamina", 100)
    , ("salt", mass), ("salt_store", 2 * mass), ("salt_conc", 1)
    , ("core_temp", 37), ("consciousness", 1), ("blood_oxygen", 1)
    , ("heart_rate", 70), ("circulation", 1) ]

-- | @set k v@ replaces or adds; @unset k@ removes (an ABSENT pool is a
--   different state from a zero one, and several consumers branch on it).
set ∷ Text → Float → [(Text, Float)] → [(Text, Float)]
set k v xs = (k, v) : filter ((≢ k) ∘ fst) xs

unset ∷ Text → [(Text, Float)] → [(Text, Float)]
unset k = filter ((≢ k) ∘ fst)

-----------------------------------------------------------------------
-- The Lua side
-----------------------------------------------------------------------

luaText ∷ LuaBackendState → Text → IO Text
luaText ls src = T.strip ∘ T.dropAround (≡ '"') <$> evalDebug ls src

-- | Everything is installed once on a global @S@: the pre-S5 statements
--   verbatim (@S.legacy*@), thin runners over the shipped modules, and the
--   three intercepted verbs (inventory read, the drain's answer, the
--   animation request) used only by the drinking scenarios.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    out ← luaText ls $ T.unlines
        [ "require('scripts.unit_ai')"
        , "local stats = require('scripts.unit_stats')"
        , "local clamp = require('scripts.lib.numeric').clamp"
        , "local physiology = require('scripts.physiology')"
        , "local thermo = require('scripts.thermo')"
        , "local salts = require('scripts.salts')"
        , "local brain = require('scripts.brain')"
        , "local cardio = require('scripts.cardio')"
        , "local starvation = require('scripts.starvation')"
        , "local needs = require('scripts.unit_ai_needs')"
        , "local water = require('scripts.unit_ai_water')"
        , "local consumable = require('scripts.consumable')"
        , "local PARAMS = require('scripts.unit_ai_config_defaults')"
        , "  .applyAll(require('scripts.unit_ai_tunables')).acolyte"
        , "local function f(v)"
        , "  if v == nil then return 'nil' end"
        , "  if v ~= v then return 'nan' end"
        , "  if v == math.huge then return 'inf' end"
        , "  if v == -math.huge then return '-inf' end"
        , "  return string.format('%.7g', v)"
        , "end"
        , "local S = {}"
        , "_G.S = S"
        , "S.f = f"
        , "world.getClimateAt = function() return { temp = S.temp or 20,"
        , "  summerTemp = S.temp or 20, winterTemp = S.temp or 20, precip = 0.2,"
        , "  humidity = S.humidity or 0.5, snow = 0 } end"
        , "world.getAmbientAt = function() return S.temp or 20 end"
        , "function S.climate(t, h) S.temp, S.humidity = t, h end"
        , -- ---- one line per stat -----------------------------------------
          "local KEYS = { 'hydration', 'salt', 'salt_store', 'salt_conc',"
        , "  'core_temp', 'hunger', 'calories', 'hypothermia', 'hyperthermia',"
        , "  'salt_imbalance', 'consciousness', 'mood', 'state_of_mind',"
        , "  'heart_rate', 'circulation', 'caffeine' }"
        , "function S.snap(uid)"
        , "  local o = {}"
        , "  for i, k in ipairs(KEYS) do o[i] = k .. '=' .. f(unit.getStat(uid, k)) end"
        , "  return table.concat(o, ' ')"
        , "end"
        -- ---- the pre-S5 statements, verbatim ---------------------------
        , "-- consumable.lua / unit_ai_needs.lua credit (hydration)"
        , "function S.legacyCreditHydration(uid, amount)"
        , "  local maxHyd = stats.get(uid, 'max_hydration')"
        , "  if maxHyd and maxHyd > 0 then"
        , "    local hyd = unit.getStat(uid, 'hydration') or 0"
        , "    local new = math.min(maxHyd, hyd + amount)"
        , "    unit.setStat(uid, 'hydration', new)"
        , "    return f(new - hyd)"
        , "  end"
        , "  return f(0)"
        , "end"
        , "-- thermo.lua sweat debit"
        , "function S.legacyDebit(uid, key, amount)"
        , "  local v = unit.getStat(uid, key)"
        , "  if v then"
        , "    local new = math.max(0, v - amount)"
        , "    unit.setStat(uid, key, new)"
        , "    return f(v - new)"
        , "  end"
        , "  return f(0)"
        , "end"
        , "-- salts.deposit"
        , "function S.legacyDeposit(uid, frac)"
        , "  local mx = (unit.getStat(uid, 'body_mass') or 70.0) * 1.0"
        , "  local cap = mx * 2.0"
        , "  local store = unit.getStat(uid, 'salt_store') or cap"
        , "  unit.setStat(uid, 'salt_store', clamp(store + frac * mx, 0, cap))"
        , "end"
        , "-- the readers"
        , "function S.legacyFraction(uid, key, maxKey)"
        , "  local c, m = unit.getStat(uid, key), stats.get(uid, maxKey)"
        , "  if c and m and m > 0 then return c / m end"
        , "  return nil"
        , "end"
        -- ---- the migrated operations -----------------------------------
        , "function S.credit(uid, key, amount)"
        , "  return f(physiology.credit(uid, key, amount))"
        , "end"
        , "function S.debit(uid, key, amount)"
        , "  return f(physiology.debit(uid, key, amount))"
        , "end"
        , "function S.deposit(uid, frac) salts.deposit(uid, frac) end"
        , "function S.facadeRead(uid, key)"
        , "  local v, m = physiology.get(uid, key)"
        , "  return f(v) .. '|' .. tostring(m)"
        , "end"
        , "function S.legacyRead(uid, key) return f(stats.get(uid, key)) .. '|nil' end"
        , "function S.readBase(uid, key)"
        , "  local v, m = physiology.getBase(uid, key)"
        , "  return f(v) .. '|' .. tostring(m)"
        , "end"
        , "function S.legacyBase(uid, key) return f(unit.getStatBase(uid, key)) .. '|nil' end"
        , "function S.verb(uid, key)"
        , "  local v, m = unit.readPhysio(uid, key)"
        , "  return f(v) .. '|' .. tostring(m)"
        , "end"
        , "function S.verbBase(uid, key)"
        , "  local v, m = unit.readPhysioBase(uid, key)"
        , "  return f(v) .. '|' .. tostring(m)"
        , "end"
        , "function S.getStat(uid, key) return f(unit.getStat(uid, key)) .. '|nil' end"
        , "function S.getCalories(uid) return f(unit.getCalories(uid)) .. '|nil' end"
        , "function S.getStatBase(uid, key) return f(unit.getStatBase(uid, key)) .. '|nil' end"
        , "function S.fraction(uid, key, maxKey)"
        , "  return f(physiology.fraction(uid, key, maxKey))"
        , "end"
        , "function S.legacyFractionText(uid, key, maxKey)"
        , "  return f(S.legacyFraction(uid, key, maxKey))"
        , "end"
        -- ---- module scenarios (pinned) ---------------------------------
        , "function S.thermoTicks(uid, n, dt)"
        , "  local info = unit.getInfo(uid)"
        , "  for _ = 1, n do thermo.tick(uid, info, dt) end"
        , "  return S.snap(uid)"
        , "end"
        , "function S.saltsReport(uid)"
        , "  salts.tick(uid, 1.0)"
        , "  return S.snap(uid) .. ' speed=' .. f(salts.speedMultiplier(uid))"
        , "    .. ' sweat=' .. f(salts.sweatFactor(uid))"
        , "    .. ' imb=' .. f(salts.imbalanceFailure(uid))"
        , "    .. ' conc=' .. f(salts.concentration(uid))"
        , "    .. ' label=' .. salts.stateLabel(uid)"
        , "end"
        , "function S.brainReport(uid)"
        , "  brain.tick(uid, 1.0)"
        , "  return S.snap(uid) .. ' hunger=' .. f(brain.hungerFrac(uid))"
        , "    .. ' stamina=' .. f(brain.staminaFrac(uid))"
        , "    .. ' cons=' .. f(brain.consciousness(uid))"
        , "end"
        , "function S.cardioReport(uid)"
        , "  cardio.tick(uid, 1.0)"
        , "  return S.snap(uid)"
        , "end"
        , "function S.starvationReport(uid)"
        , "  return 'frac=' .. f(starvation.fraction(uid)) .. ' band=' .. starvation.band(uid)"
        , "    .. ' speed=' .. f(starvation.speedMultiplier(uid))"
        , "    .. ' strength=' .. f(starvation.strengthMultiplier(uid))"
        , "end"
        -- ---- drinking and eating, with the three intercepted verbs -----
        , "S.inv = {}"
        , "S.drinks = 0"
        , "unit.getInventory = function(uid) return S.inv[uid] end"
        , "unit.getItemTemp = function() return 80 end"
        , "unit.drink = function() S.drinks = S.drinks + 1 end"
        , "unit.modifyItemFillById = function(uid, iid, delta)"
        , "  for _, it in ipairs(S.inv[uid] or {}) do"
        , "    if it.instanceId == iid then"
        , "      local applied = math.max(delta, -it.currentFill)"
        , "      it.currentFill = it.currentFill + applied"
        , "      return applied"
        , "    end"
        , "  end"
        , "  return nil"
        , "end"
        , "unit.feed = function(uid, defName)"
        , "  for i, it in ipairs(S.inv[uid] or {}) do"
        , "    if it.defName == defName then"
        , "      table.remove(S.inv[uid], i)"
        , "      local mx = unit.getStat(uid, 'max_hunger') or 0"
        , "      unit.setStat(uid, 'hunger', math.min(mx, (unit.getStat(uid, 'hunger') or 0) + 250))"
        , "      return 250"
        , "    end"
        , "  end"
        , "  return nil"
        , "end"
        , "function S.stock(uid, water, rations)"
        , "  S.inv[uid] = {}"
        , "  S.drinks = 0"
        , "  if water then S.inv[uid][#S.inv[uid] + 1] ="
        , "    { defName = 'canteen_steel_2l', instanceId = 11, currentFill = water } end"
        , "  for i = 1, rations do S.inv[uid][#S.inv[uid] + 1] ="
        , "    { defName = 'rations', instanceId = 20 + i,"
        , "      food = { nutrition = { calories = 250 } } } end"
        , "  S.inv[uid][#S.inv[uid] + 1] ="
        , "    { defName = 'coffee_pot', instanceId = 31, currentFill = 1.0, quality = 80 }"
        , "end"
        , "function S.needsReport(uid)"
        , "  local r = {}"
        , "  r[#r + 1] = 'drinkU=' .. f(needs.drinkUtility(uid, {}, PARAMS))"
        , "  r[#r + 1] = 'eatU=' .. f(needs.eatUtility(uid, {}, PARAMS))"
        , "  r[#r + 1] = 'srcU=' .. f(water.drinkFromSourceUtility(uid,"
        , "    { knownWaterSources = { { x = 1, y = 1 } } }, PARAMS))"
        , "  r[#r + 1] = 'searchU=' .. f(water.searchUtility(uid,"
        , "    { activeGoal = 'find_water', goalStatus = { find_water = 'in_progress' } }, PARAMS))"
        , "  needs.drinkExecute(uid, {}, PARAMS)"
        , "  r[#r + 1] = 'afterDrink=' .. f(unit.getStat(uid, 'hydration'))"
        , "    .. ' drinks=' .. S.drinks"
        , "  needs.eatExecute(uid, {}, PARAMS)"
        , "  r[#r + 1] = 'afterEat=' .. f(unit.getStat(uid, 'hunger'))"
        , "    .. ' store=' .. f(unit.getStat(uid, 'salt_store'))"
        , "  return table.concat(r, ' ')"
        , "end"
        , "function S.sourceDrinkExecute(uid)"
        , "  local s = { sourcePhase = 'drinking' }"
        , "  water.drinkFromSourceExecute(uid, s, PARAMS)"
        , "  return tostring(s.sourcePhase)"
        , "end"
        , "function S.coffee(uid)"
        , "  local r, why = consumable.drink(uid, 'coffee_pot')"
        , "  if not r then return 'refused:' .. tostring(why) end"
        , "  return 'hyd=' .. f(r.hydration) .. ' sip=' .. f(r.sip)"
        , "    .. ' after=' .. f(unit.getStat(uid, 'hydration'))"
        , "    .. ' caf=' .. f(unit.getStat(uid, 'caffeine'))"
        , "    .. ' mood=' .. f(unit.getStat(uid, 'mood'))"
        , "end"
        -- ---- the status panel, with the panel engine's build captured --
        , "function S.statusReport(uid)"
        , "  local engine = require('scripts.unit_info_v2_panel_engine')"
        , "  local status = require('scripts.unit_info_v2_status')"
        , "  local captured"
        , "  local orig = engine.buildIconStatPanel"
        , "  engine.buildIconStatPanel = function(rect, id, rows) captured = rows end"
        , "  status.buildStatusPanel({ x = 0, y = 0, width = 300, height = 400 }, uid)"
        , "  engine.buildIconStatPanel = orig"
        , "  local o = {}"
        , "  for _, row in ipairs(captured or {}) do"
        , "    if row.value and row.key and (row.key == 'salt' or row.key == 'hunger'"
        , "       or row.key == 'calories' or row.key == 'hydration') then"
        , "      local c = row.colorFn and row.colorFn(uid)"
        , "      o[#o + 1] = row.key .. ':' .. tostring(row.value(uid)) .. ':'"
        , "        .. (c and (f(c[1]) .. ',' .. f(c[2]) .. ',' .. f(c[3])) or 'none')"
        , "    end"
        , "  end"
        , "  local conds = {}"
        , "  for _, c in ipairs(status.unitConditions(uid)) do conds[#conds + 1] = c.name end"
        , "  return table.concat(o, ' ') .. ' conds=' .. table.concat(conds, ',')"
        , "end"
        , "return true" ]
    out `shouldBe` "true"
    pure ls

withLua ∷ [(Text, Float)] → (EngineEnv → LuaBackendState → IO a) → IO a
withLua stats act = withHeadlessEngineNoWorld $ \env → do
    resetScene env stats
    ls ← setupLua env
    act env ls

-- | One labelled state: reset the scene to it, run a Lua expression on
--   unit 1 and answer its text.
inState ∷ EngineEnv → LuaBackendState → [(Text, Float)] → Text → IO Text
inState env ls stats call = do
    resetScene env stats
    _ ← luaText ls "S.stock(1, nil, 0); S.stock(2, nil, 0); S.stock(3, nil, 0); return true"
    luaText ls ("return " <> call)

-- | Run the same expression over labelled states and join the answers.
scenario ∷ EngineEnv → LuaBackendState → [(Text, [(Text, Float)])] → Text
         → IO Text
scenario env ls states call =
    T.intercalate "\n" <$> forM states (\(label, st) → do
        out ← inState env ls st call
        pure (label <> ": " <> out))

-- | Compare against the literal recorded from the pre-S5 modules.
pinned ∷ Text → Text → Expectation
pinned want got = got `shouldBe` want

-----------------------------------------------------------------------
-- Labelled states
-----------------------------------------------------------------------

-- | The pool states every consumer must treat identically: populated and
--   healthy, depleted, empty, a present zero, and ABSENT; for a human and
--   a 0.3 kg body.
poolStates ∷ [(Text, [(Text, Float)])]
poolStates =
    [ ("70kg full", bodyStats 70)
    , ("70kg depleted"
      , set "hydration" 9 (set "hunger" 60 (set "calories" 210
          (set "salt" 22 (set "salt_store" 5 (set "salt_conc" 0.55
          (bodyStats 70)))))))
    , ("70kg present zero"
      , set "hydration" 0 (set "hunger" 0 (set "calories" 0
          (set "salt" 0 (set "salt_store" 0 (bodyStats 70))))))
    , ("70kg pools absent"
      , unset "hydration" (unset "hunger" (unset "calories"
          (unset "salt" (unset "salt_store" (bodyStats 70))))))
    , ("70kg max zero"
      , set "max_hydration" 0 (set "max_hunger" 0 (set "max_calories" 0
          (bodyStats 70))))
    , ("0.3kg full", bodyStats 0.3)
    , ("0.3kg depleted"
      , set "hydration" 0.04 (set "hunger" 0.4 (set "calories" 1.1
          (bodyStats 0.3))))
    , ("0.3kg pools absent"
      , unset "hydration" (unset "hunger" (unset "calories"
          (unset "salt" (unset "salt_store" (bodyStats 0.3))))))
    ]

-----------------------------------------------------------------------
-- The gate
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "physiology consumer migration" $ do
    facadeSpec
    contributionSpec
    boundarySpec
    woundSpec
    moduleSpec

-- | The facade's reads against the statements they replaced.
facadeSpec ∷ Spec
facadeSpec = describe "the inactive facade reads" $ do

    it "answers exactly what stats.get answers, for owned, derived, \
       \unowned and undefined keys, with a nil mode" $
        withLua (bodyStats 70) $ \env ls → do
            let keys = [ "hydration", "hunger", "calories", "core_temp"
                       , "salt", "salt_store", "salt_conc", "hypothermia"
                       , "circulation", "max_hydration", "max_stamina"
                       , "metabolism_rate", "body_mass", "nonesuch" ]
            forM_ poolStates $ \(label, st) → forM_ keys $ \k → do
                resetScene env st
                viaFacade ← luaText ls ("return S.facadeRead(1, '" <> k <> "')")
                viaStats  ← luaText ls ("return S.legacyRead(2, '" <> k <> "')")
                (label <> "/" <> k, viaFacade)
                    `shouldBe` (label <> "/" <> k, viaStats)
                base  ← luaText ls ("return S.readBase(1, '" <> k <> "')")
                base' ← luaText ls ("return S.legacyBase(2, '" <> k <> "')")
                (label <> "/base/" <> k, base)
                    `shouldBe` (label <> "/base/" <> k, base')

    it "computes a fraction as the old cur/max expression, nil when either \
       \side is undefined or the maximum is not positive" $
        withLua (bodyStats 70) $ \env ls →
            forM_ poolStates $ \(label, st) →
                forM_ [ ("hydration", "max_hydration"), ("hunger", "max_hunger")
                      , ("calories", "max_calories"), ("stamina", "max_stamina")
                      , ("hydration", "nonesuch") ] $ \(c, m) → do
                    resetScene env st
                    new ← luaText ls ("return S.fraction(1, '" <> c <> "', '" <> m <> "')")
                    old ← luaText ls ("return S.legacyFractionText(2, '" <> c <> "', '" <> m <> "')")
                    (label <> "/" <> c, new) `shouldBe` (label <> "/" <> c, old)

-- | The facade's contributions against the statements they replaced.
contributionSpec ∷ Spec
contributionSpec = describe "the inactive facade contributions" $ do

    it "credits hydration as consumable.lua and the AI drink did: clamped \
       \to max_hydration, an absent pool starting at 0, no write without a \
       \positive maximum; the applied amount is returned" $
        withLua (bodyStats 70) $ \env ls →
            forM_ poolStates $ \(label, st) → forM_ [0.4, 2.0, 100.0 ∷ Double] $ \k → do
                resetScene env st
                let amount = tshow (k * 42)
                newRet ← luaText ls ("return S.credit(1, 'hydration', " <> amount <> ")")
                oldRet ← luaText ls ("return S.legacyCreditHydration(2, " <> amount <> ")")
                a ← luaText ls "return S.snap(1)"
                b ← luaText ls "return S.snap(2)"
                (label, amount, newRet, a) `shouldBe` (label, amount, oldRet, b)

    it "debits hydration and salt as thermo.lua's sweat did: floored at 0, \
       \an absent pool a no-op (it stays absent), the applied amount returned" $
        withLua (bodyStats 70) $ \env ls →
            forM_ poolStates $ \(label, st) →
                forM_ ["hydration", "salt"] $ \key → forM_ [0.5, 5.0, 500.0 ∷ Double] $ \amount → do
                    resetScene env st
                    newRet ← luaText ls ("return S.debit(1, '" <> key <> "', " <> tshow amount <> ")")
                    oldRet ← luaText ls ("return S.legacyDebit(2, '" <> key <> "', " <> tshow amount <> ")")
                    a ← luaText ls "return S.snap(1)"
                    b ← luaText ls "return S.snap(2)"
                    (label, key, amount, newRet, a)
                        `shouldBe` (label, key, amount, oldRet, b)

    it "deposits a meal's salt as salts.deposit did: a reserve that is \
       \absent starts full, present zero and partial values are kept, the \
       \cap discards overflow, for a 70 kg and a 0.3 kg body" $
        withLua (bodyStats 70) $ \env ls →
            forM_ [70, 0.3 ∷ Float] $ \mass →
                forM_ [ ("absent", unset "salt_store"), ("zero", set "salt_store" 0)
                      , ("partial", set "salt_store" (0.4 * mass))
                      , ("near cap", set "salt_store" (1.9 * mass))
                      , ("full", set "salt_store" (2 * mass)) ] $ \(rl, adj) →
                    forM_ [0.3, 0.6, 5.0 ∷ Double] $ \frac → do
                        resetScene env (adj (bodyStats mass))
                        _ ← luaText ls ("S.deposit(1, " <> tshow frac <> "); return true")
                        _ ← luaText ls ("S.legacyDeposit(2, " <> tshow frac <> "); return true")
                        a ← luaText ls "return S.snap(1)"
                        b ← luaText ls "return S.snap(2)"
                        (mass, rl, frac, a) `shouldBe` (mass, rl, frac, b)

    it "refuses a key it has no contribution rule for, writing nothing" $
        withLua (bodyStats 70) $ \_ ls →
            luaText ls "local p = require('scripts.physiology'); \
                       \local r, why = p.credit(1, 'core_temp', 1); \
                       \return tostring(r) .. '|' .. tostring(why) .. '|' \
                       \.. S.f(unit.getStat(1, 'core_temp'))"
                `shouldReturn` "nil|no contribution rule for core_temp|37"

-- | The boundary itself.
boundarySpec ∷ Spec
boundarySpec = describe "the inactive compatibility boundary" $ do

    it "returns stored values from the facade, the read verbs (as the probe \
       \calls them) and getStat, for a definition that resolves EVERY system \
       \to Nominal under the loaded shipped registry" $
        withLua (bodyStats 70) $ \env ls → do
            let probeKeys = [ "core_temp", "salt_conc", "circulation"
                            , "hypothermia", "hyperthermia", "salt_imbalance"
                            , "hydration", "hunger", "calories", "salt" ]
            forM_ poolStates $ \(label, st) → forM_ probeKeys $ \k → do
                resetScene env st
                -- unit 3 is the all-Nominal definition; unit 2 the plain one.
                nominal ← luaText ls ("return S.verb(3, '" <> k <> "')")
                plain   ← luaText ls ("return S.getStat(3, '" <> k <> "')")
                (label, k, nominal) `shouldBe` (label, k, plain)
                nominalBase ← luaText ls ("return S.verbBase(3, '" <> k <> "')")
                plainBase   ← luaText ls ("return S.getStatBase(3, '" <> k <> "')")
                (label, k, nominalBase) `shouldBe` (label, k, plainBase)
                viaFacade ← luaText ls ("return S.facadeRead(3, '" <> k <> "')")
                (label, k, viaFacade) `shouldBe` (label, k, plain)
            -- the probe's numeric calorie observer reads the store through
            -- unit.readPhysio and must see what unit.getCalories reports
            forM_ poolStates $ \(label, st) → do
                resetScene env st
                viaVerb ← luaText ls "return S.verb(3, 'calories')"
                viaVerb' ← luaText ls "return S.verb(1, 'calories')"
                legacy ← luaText ls "return S.getCalories(3)"
                (label, viaVerb) `shouldBe` (label, legacy)
                (label, viaVerb') `shouldBe` (label, legacy)

    it "answers the mode-aware healthy values only once the registry is \
       \ACTIVATED — the single switch S4 flips" $
        withLua (set "hydration" 9 (bodyStats 70)) $ \env ls → do
            luaText ls "return S.facadeRead(3, 'hydration')"
                `shouldReturn` "9|nil"
            activatePhysiologyModes env
            -- Nominal hydration reads its healthy rule (max_hydration), and
            -- the facade passes the mode through.
            luaText ls "return S.facadeRead(3, 'hydration')"
                `shouldReturn` "42|nominal"
            -- the facade's unowned reads are untouched by the switch
            luaText ls "return S.facadeRead(3, 'max_stamina')"
                `shouldReturn` "100|nil"
            modifyIORef' (crPhysiologyRegistryRef (toContentRegistriesCapability env))
                (\r → r { prModesActive = False })
            luaText ls "return S.facadeRead(3, 'hydration')"
                `shouldReturn` "9|nil"

    it "publishes the flag through engine.getPhysiologyRegistry" $
        withLua (bodyStats 70) $ \env ls → do
            luaText ls "return tostring(engine.getPhysiologyRegistry().modesActive)"
                `shouldReturn` "false"
            activatePhysiologyModes env
            luaText ls "return tostring(engine.getPhysiologyRegistry().modesActive)"
                `shouldReturn` "true"

-- | The Haskell consumers: wound healing, blood recovery, fever.
woundSpec ∷ Spec
woundSpec = describe "the wound tick (Haskell consumers)" $ do

    it "is the identical tick under the shipped INACTIVE registry as with \
       \none, for populated, depleted, zero, absent and Nominal-resolving \
       \units, wounded and unwounded" $ do
        reg ← readShippedPhysiologyRegistry
        forM_ woundStates $ \(label, stats) →
            forM_ [woundedUnit, unwoundedUnit] $ \mk →
                forM_ [woundDef, woundDef { udPhysiology = nominalPhysiology }] $ \d → do
                    let u = mk stats
                        summarize (i, o, _) = (observe i, outcomeText o)
                        viaReg = tickOneUnitWith reg 1000 d 1
                            emptyInfectionManager Nothing (Random.mkStdGen 1) u False
                        viaNone = tickOneUnit 1000 d 1
                            emptyInfectionManager Nothing (Random.mkStdGen 1) u False
                    (label, summarize viaReg) `shouldBe` (label, summarize viaNone)

    it "gates healing on the calorie store exactly as calorieHealMultiplier \
       \says, and scales infection growth by the fever formula" $ do
        reg ← readShippedPhysiologyRegistry
        let tickW w s = firstWound (tickedWith reg woundDef (unitWith w (HM.fromList s)))
            -- a clean, uninfected wound: heal advance is linear in the multiplier
            healOf s = woundHeal (tickW cleanWound s) - woundHeal cleanWound
            full = healOf healthyWound
            part = healOf (set "calories" 500 healthyWound)
            mult = calorieHealMultiplier (HM.fromList (set "calories" 500 healthyWound))
        part / full `shouldSatisfy` (\r → abs (r - mult) < 1e-3)
        -- a dirty infected wound: growth before the immune clearance, which
        -- is the same at either core temperature (dt = 1, constitution 1)
        let clearance = immuneClearRate * (immuneRampRate * immuneSeed)
            growth c = woundInfection (tickW dirtyWound (set "core_temp" c healthyWound))
                       - woundInfection dirtyWound + clearance
            want = max feverSuppressFloor (1 - feverSuppressK * (41 - 38))
        growth 38 `shouldSatisfy` (\g → abs (g - growth 37) < 1e-6)   -- no fever below 38
        growth 41 / growth 37 `shouldSatisfy` (\r → abs (r - want) < 1e-3)

    it "recovers blood by the nutrition product, a present pool with no \
       \usable maximum blocking it and an absent pool reading 1.0" $ do
        reg ← readShippedPhysiologyRegistry
        let maxBlood = 70 * bloodMassRatio
            start = uiBlood (unwoundedUnit HM.empty)
            bloodAfter d s = uiBlood (tickedWith reg d (unwoundedUnit (HM.fromList s)))
            expected s = min maxBlood (start + bloodRecoveryDelta (HM.fromList s) maxBlood 1)
            close a b = abs (a - b) < 2e-6
        forM_ [ ("healthy pools", healthyBlood ⧺ [ ("calories", 2000), ("max_calories", 2000)
                                              , ("hydration", 100), ("max_hydration", 100) ])
              , ("calories empty", healthyBlood ⧺ [("calories", 0), ("max_calories", 2000)])
              , ("calories half", healthyBlood ⧺ [("calories", 1000), ("max_calories", 2000)])
              , ("calories present, no max", healthyBlood ⧺ [("calories", 1500), ("max_calories", 0)])
              , ("hydration at floor", healthyBlood ⧺ [("hydration", 25), ("max_hydration", 100)])
              , ("hydration below floor", healthyBlood ⧺ [("hydration", 24), ("max_hydration", 100)])
              , ("pools absent", healthyBlood) ] $ \(label, s) → do
            (label, close (bloodAfter woundDef s) (expected s)) `shouldBe` (label, True)
            -- the all-Nominal definition is no different while inactive
            (label, close (bloodAfter woundDef { udPhysiology = nominalPhysiology } s)
                          (expected s)) `shouldBe` (label, True)

    it "reads the prepared seam through the pure consumer view once \
       \activated: a Nominal nutrition and thermo heal fully and suppress \
       \no infection, where the same unit inactive is gated and feverish" $ do
        reg ← readShippedPhysiologyRegistry
        let active = reg { prModesActive = True }
            sick = HM.fromList (set "calories" 100 (set "core_temp" 41 healthyWound))
            fed = HM.fromList healthyWound
            tickOf r d st = firstWound (tickedWith r d (woundedUnit st))
            inactiveSick = tickOf reg woundDef sick
            nominalSick  = tickOf active woundDef { udPhysiology = nominalPhysiology } sick
            fullyFed     = tickOf reg woundDef fed
        -- inactive: the same sick unit is still gated and feverish
        woundHeal inactiveSick `shouldSatisfy` (< woundHeal fullyFed)
        woundInfection inactiveSick `shouldSatisfy` (< woundInfection nominalSick)
        -- active + Nominal: healthy values, so it behaves as the fed unit
        woundHeal nominalSick `shouldSatisfy` (\h → abs (h - woundHeal fullyFed) < 1e-6)

    it "never reads an activated invariant violation as healthy or absent: \
       \a Simulated pool with no stored value, or a healthy-rule input that \
       \is absent or not positive, returns the unit untouched with the \
       \violation, while the same unit inactive ticks normally" $ do
        reg ← readShippedPhysiologyRegistry
        let active = reg { prModesActive = True }
            simulatedNutrition = woundDef { udPhysiology =
                UnitPhysiology True Nothing
                    (HM.fromList [("nutrition", PhysioSimulated)]) }
            nominalHydration = woundDef { udPhysiology =
                UnitPhysiology True Nothing
                    (HM.fromList [("hydration", PhysioNominal)]) }
            run r d st =
                let u = woundedUnit (HM.fromList st)
                    (i, o, _) = tickOneUnitWith r 1000 d 1 emptyInfectionManager
                                    Nothing (Random.mkStdGen 1) u False
                in (observe i ≡ observe u, outcomeText o)
            violation (untouched, o) =
                untouched ∧ "invariant " `T.isPrefixOf` o
        -- a Simulated pool with no stored value
        violation (run active simulatedNutrition
            (unset "calories" healthyWound)) `shouldBe` True
        -- a healthy-rule input absent, then zero
        violation (run active nominalHydration
            (unset "max_hydration" healthyWound)) `shouldBe` True
        violation (run active nominalHydration
            (set "max_hydration" 0 healthyWound)) `shouldBe` True
        -- the same units inactive tick as before: no violation
        forM_ [ (simulatedNutrition, unset "calories" healthyWound)
              , (nominalHydration, unset "max_hydration" healthyWound) ] $
            \(d, st) → violation (run reg d st) `shouldBe` False
        -- and a valid active unit is not a violation
        violation (run active simulatedNutrition healthyWound) `shouldBe` False

    it "reads a pool against its EFFECTIVE capacity once activated, so a \
       \capacity modifier cannot make a healthy pool look depleted" $ do
        reg ← readShippedPhysiologyRegistry
        let active = reg { prModesActive = True }
            shrink k delta = HM.singleton k [StatModifier delta "test" Nothing 0]
            -- Nominal pools: healthy = effective capacity, so fraction 1.0
            healthyUnder d capKey capVal delta u0 =
                let st = HM.fromList (set capKey capVal healthyWound)
                in tickedWith active d (u0 st) { uiModifiers = shrink capKey delta }
            plain d capKey capVal u0 =
                tickedWith active d (u0 (HM.fromList (set capKey capVal healthyWound)))
            close a b = abs (a - b) < 2e-6
        -- blood recovery: stored max_hydration 100, effective 20
        let withMod = healthyUnder nominalWound "max_hydration" 100 (-80) unwoundedUnit
            bare    = plain nominalWound "max_hydration" 20 unwoundedUnit
        (close (uiBlood withMod) (uiBlood bare)) `shouldBe` True
        -- healing: stored max_calories 2000, effective 400
        let healMod = healthyUnder nominalWound "max_calories" 2000 (-1600) woundedUnit
            healBare = plain nominalWound "max_calories" 400 woundedUnit
        woundHeal (firstWound healMod) `shouldSatisfy`
            (\h → abs (h - woundHeal (firstWound healBare)) < 1e-6)
  where
    nominalWound = woundDef { udPhysiology = nominalPhysiology }


tickedWith ∷ PhysiologyRegistry → UnitDef → UnitInstance → UnitInstance
tickedWith reg d u =
    let (i, _, _) = tickOneUnitWith reg 1000 d 1 emptyInfectionManager
                        Nothing (Random.mkStdGen 1) u False
    in i

firstWound ∷ UnitInstance → Wound
firstWound i = case uiWounds i of
    w : _ → w
    []    → error "PhysiologyConsumerMigration: unit has no wound"

outcomeText ∷ WoundTickOutcome → Text
outcomeText NoChange = "no change"
outcomeText (UnconsciousNow p) = "unconscious " <> p
outcomeText (DiedNow p c) = "died " <> p <> " " <> c
outcomeText (PhysioInvariant w) = "invariant " <> w

-- | The wound tick's observable result.
observe ∷ UnitInstance → (Float, [(Float, Float, Float)], Double)
observe i = ( uiBlood i
            , [ (woundHeal w, woundInfection w, woundClot w) | w ← uiWounds i ]
            , realToFrac (uiImmuneResponse i) )

woundDef ∷ UnitDef
woundDef = (minimalDef "t" "T")
    { udBodyParts = [ BodyPart
        { bpId = "l_thigh", bpName = "l_thigh", bpParent = Nothing
        , bpVital = False, bpAreaWeight = 0.1, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
        , bpLayers = [], bpTargetable = True, bpDepth = 0.0
        , bpAffectsLocomotion = False, bpAffectsBalance = False } ] }

-- | A clotted, dressed wound that heals and (when dirty) festers.
healthyWound ∷ [(Text, Float)]
healthyWound =
    [ ("body_mass", 70), ("constitution", 1.0)
    , ("calories", 2000), ("max_calories", 2000)
    , ("hydration", 100), ("max_hydration", 100), ("core_temp", 37) ]

healthyBlood ∷ [(Text, Float)]
healthyBlood = [("body_mass", 70), ("constitution", 1.0)]

woundStates ∷ [(Text, HM.HashMap Text Float)]
woundStates =
    [ ("healthy", HM.fromList healthyWound)
    , ("depleted", HM.fromList (set "calories" 300 (set "hydration" 30 healthyWound)))
    , ("fever", HM.fromList (set "core_temp" 41 healthyWound))
    , ("present zero", HM.fromList (set "calories" 0 (set "hydration" 0 healthyWound)))
    , ("pools absent", HM.fromList (unset "calories" (unset "hydration"
                           (unset "core_temp" healthyWound)))) ]

unwoundedUnit, woundedUnit ∷ HM.HashMap Text Float → UnitInstance
unwoundedUnit st = (mkInst "t" st) { uiBlood = 4.0, uiWounds = [] }
woundedUnit = unitWith dirtyWound

unitWith ∷ Wound → HM.HashMap Text Float → UnitInstance
unitWith w st = (mkInst "t" st) { uiBlood = 5.25, uiWounds = [w] }

-- | A clotted, dressed slash that is dirty and already infected.
dirtyWound ∷ Wound
dirtyWound = Wound
    { woundPart = "l_thigh", woundKind = "slash", woundSeverity = 0.4
    , woundAt = 0, woundBandage = 0.05, woundClot = 1.0, woundHeal = 0.1
    , woundDressing = "bandage", woundInfection = 0.3, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | The same wound, disinfected and uninfected: it only heals.
cleanWound ∷ Wound
cleanWound = dirtyWound { woundInfection = 0, woundClean = True }

-----------------------------------------------------------------------
-- Pinned module scenarios
-----------------------------------------------------------------------

moduleSpec ∷ Spec
moduleSpec = describe "the shipped consumer modules" $ do

    it "thermo in the heat: sweat debits hydration and salt, scaled to a \
       \70 kg and a 0.3 kg body, absent pools staying absent" $
        withLua (bodyStats 70) $ \env ls → do
            _ ← luaText ls "S.climate(39.6, 0.26); return true"
            hot ← scenario env ls
                    [ (l, set "core_temp" 40.5 st) | (l, st) ← poolStates ]
                    "S.thermoTicks(1, 5, 0.1)"
            pinned expectedThermoHot hot

    it "thermo in the cold: a low calorie store throttles heat and shivering" $
        withLua (bodyStats 70) $ \env ls → do
            _ ← luaText ls "S.climate(0, 0.3); return true"
            cold ← scenario env ls
                    [ (l, set "calories" 0.1 (set "core_temp" 34 st))
                    | (l, st) ← poolStates ]
                    "S.thermoTicks(1, 5, 0.1)"
            pinned expectedThermoCold cold

    it "salts: concentration, cramps, sweat factor, imbalance and the \
       \reserve release" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenario env ls poolStates "S.saltsReport(1)"
            pinned expectedSalts got

    it "brain: consciousness from core temperature and salt, hunger fraction \
       \into mood" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenario env ls
                    ([ (l, set "core_temp" 41.2 (set "salt_conc" 0.5 st))
                     | (l, st) ← poolStates ]
                    ⧺ [ ("70kg cold", set "core_temp" 31 (bodyStats 70)) ])
                    "S.brainReport(1)"
            pinned expectedBrain got

    it "cardio: heart rate from core temperature, absent core defaulting to 37" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenario env ls
                    [ ("hot", set "core_temp" 41 (bodyStats 70))
                    , ("normal", bodyStats 70)
                    , ("cold", set "core_temp" 32 (bodyStats 70))
                    , ("absent", unset "core_temp" (bodyStats 70)) ]
                    "S.cardioReport(1)"
            pinned expectedCardio got

    it "starvation: the calorie fraction, its band and multipliers" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenario env ls
                    (poolStates ⧺ [ ("70kg hungry", set "calories" 500 (bodyStats 70))
                                  , ("70kg starving", set "calories" 120 (bodyStats 70)) ])
                    "S.starvationReport(1)"
            pinned expectedStarvation got

    it "AI needs: drink and eat utilities, the drink from a canteen and the \
       \meal with its salt deposit, the source drink and the water search" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenarioStocked env ls
                    (poolStates ⧺ [ ("70kg thirsty", set "hydration" 12 (bodyStats 70))
                                  , ("70kg hungry", set "hunger" 90 (bodyStats 70)) ])
                    "S.needsReport(1)"
            pinned expectedNeeds got

    it "AI source drink: holds the phase until the pool is nearly full" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenario env ls
                    [ ("low", set "hydration" 10 (bodyStats 70))
                    , ("95 percent", set "hydration" 39.9 (bodyStats 70))
                    , ("absent", unset "hydration" (bodyStats 70))
                    , ("no max", set "max_hydration" 0 (bodyStats 70)) ]
                    "S.sourceDrinkExecute(1)"
            pinned expectedSourceDrink got

    it "consumable: a sip hydrates by the quality-scaled amount, clamped, \
       \with its caffeine and mood effects" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenarioStocked env ls
                    [ ("70kg full", bodyStats 70)
                    , ("70kg half", set "hydration" 21 (bodyStats 70))
                    , ("70kg nearly full", set "hydration" 41.5 (bodyStats 70))
                    , ("70kg zero", set "hydration" 0 (bodyStats 70))
                    , ("70kg absent", unset "hydration" (bodyStats 70))
                    , ("70kg max zero", set "max_hydration" 0 (bodyStats 70))
                    , ("0.3kg half", set "hydration" 0.09 (bodyStats 0.3)) ]
                    "S.coffee(1)"
            pinned expectedCoffee got

    it "status panel: the salt row text and colour, the pool row colours \
       \and the meter conditions, for every pool state" $
        withLua (bodyStats 70) $ \env ls → do
            got ← scenario env ls
                    ([ (l, set "salt_conc" 0.5 (set "hypothermia" 0.4
                            (set "hyperthermia" 0.1 (set "salt_imbalance" 0.7 st))))
                     | (l, st) ← poolStates ]
                    ⧺ [ ("salt high", set "salt_conc" 1.5 (bodyStats 70))
                      , ("salt ideal", set "salt_conc" 1.0 (bodyStats 70))
                      , ("salt absent", unset "salt_conc" (bodyStats 70)) ])
                    "S.statusReport(1)"
            pinned expectedStatus got

-- | Like 'scenario' but stocks unit 1 with a canteen, three rations and a
--   coffee pot first.
scenarioStocked ∷ EngineEnv → LuaBackendState → [(Text, [(Text, Float)])]
                → Text → IO Text
scenarioStocked env ls states call =
    T.intercalate "\n" <$> forM states (\(label, st) → do
        resetScene env st
        _ ← luaText ls "S.stock(1, 2.0, 3); return true"
        out ← luaText ls ("return " <> call)
        pure (label <> ": " <> out))

-----------------------------------------------------------------------
-- Recorded from the pre-S5 modules at 0482a98ef
-----------------------------------------------------------------------

expectedThermoHot, expectedThermoCold, expectedSalts, expectedBrain,
    expectedCardio, expectedStarvation, expectedNeeds, expectedSourceDrink,
    expectedCoffee, expectedStatus ∷ Text
expectedThermoHot = T.intercalate "\n"
    [ "70kg full: hydration=41.91679 salt=69.83358 salt_store=140 salt_conc=1 core_temp=40.49626 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "70kg depleted: hydration=8.934987 salt=21.86997 salt_store=5 salt_conc=0.55 core_temp=40.49697 hunger=60 calories=210 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "70kg present zero: hydration=0 salt=0 salt_store=0 salt_conc=1 core_temp=40.49573 hunger=0 calories=0 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "70kg pools absent: hydration=nil salt=nil salt_store=nil salt_conc=1 core_temp=40.49626 hunger=nil calories=nil hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "70kg max zero: hydration=41.91679 salt=69.83358 salt_store=140 salt_conc=1 core_temp=40.49626 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "0.3kg full: hydration=0.1796815 salt=0.2993629 salt_store=0.6 salt_conc=1 core_temp=39.75994 hunger=3 calories=6 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "0.3kg depleted: hydration=0.03968494 salt=0.2993699 salt_store=0.6 salt_conc=1 core_temp=39.69229 hunger=0.4 calories=1.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "0.3kg pools absent: hydration=nil salt=nil salt_store=nil salt_conc=1 core_temp=39.75994 hunger=nil calories=nil hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil" ]

expectedThermoCold = T.intercalate "\n"
    [ "70kg full: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=33.99964 hunger=700 calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.8799826 caffeine=nil",
    "70kg depleted: hydration=9 salt=22 salt_store=5 salt_conc=0.55 core_temp=33.99964 hunger=60 calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.8799826 caffeine=nil",
    "70kg present zero: hydration=0 salt=0 salt_store=0 salt_conc=1 core_temp=33.99964 hunger=0 calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.8799826 caffeine=nil",
    "70kg pools absent: hydration=nil salt=nil salt_store=nil salt_conc=1 core_temp=33.99964 hunger=nil calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.8799826 caffeine=nil",
    "70kg max zero: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=34.00168 hunger=700 calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.8800806 caffeine=nil",
    "0.3kg full: hydration=0.18 salt=0.3 salt_store=0.6 salt_conc=1 core_temp=33.93221 hunger=3 calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.87673 caffeine=nil",
    "0.3kg depleted: hydration=0.04 salt=0.3 salt_store=0.6 salt_conc=1 core_temp=33.93221 hunger=0.4 calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.87673 caffeine=nil",
    "0.3kg pools absent: hydration=nil salt=nil salt_store=nil salt_conc=1 core_temp=33.93221 hunger=nil calories=0.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=0.87673 caffeine=nil" ]

expectedSalts = T.intercalate "\n"
    [ "70kg full: hydration=42 salt=70 salt_store=139.996 salt_conc=1 core_temp=37 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=0 conc=1 label=Balanced",
    "70kg depleted: hydration=9 salt=20.5386 salt_store=5 salt_conc=1.36924 core_temp=37 hunger=60 calories=210 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=0.04810011 conc=1.36924 label=High salt",
    "70kg present zero: hydration=0 salt=0 salt_store=0 salt_conc=0 core_temp=37 hunger=0 calories=0 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=0.4 sweat=0.3 imb=1 conc=0 label=Severe hyponatremia",
    "70kg pools absent: hydration=nil salt=70 salt_store=139.996 salt_conc=1 core_temp=37 hunger=nil calories=nil hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=0 conc=1 label=Balanced",
    "70kg max zero: hydration=42 salt=70 salt_store=139.996 salt_conc=1 core_temp=37 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=0 conc=1 label=Balanced",
    "0.3kg full: hydration=0.18 salt=0.3 salt_store=0.5999829 salt_conc=1 core_temp=37 hunger=3 calories=6 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=0 conc=1 label=Balanced",
    "0.3kg depleted: hydration=0.04 salt=0.2482367 salt_store=0.6 salt_conc=3.723551 core_temp=37 hunger=0.4 calories=1.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=1 conc=3.723551 label=Severe hypernatremia",
    "0.3kg pools absent: hydration=nil salt=0.3 salt_store=0.5999829 salt_conc=1 core_temp=37 hunger=nil calories=nil hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil speed=1 sweat=1 imb=0 conc=1 label=Balanced" ]

expectedBrain = T.intercalate "\n"
    [ "70kg full: hydration=42 salt=70 salt_store=140 salt_conc=0.5 core_temp=41.2 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.75 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=1 stamina=1 cons=0.2857143",
    "70kg depleted: hydration=9 salt=22 salt_store=5 salt_conc=0.5 core_temp=41.2 hunger=60 calories=210 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.5671428 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=0.08571429 stamina=1 cons=0.2857143",
    "70kg present zero: hydration=0 salt=0 salt_store=0 salt_conc=0.5 core_temp=41.2 hunger=0 calories=0 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.55 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=0 stamina=1 cons=0.2857143",
    "70kg pools absent: hydration=nil salt=nil salt_store=nil salt_conc=0.5 core_temp=41.2 hunger=nil calories=nil hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.75 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=1 stamina=1 cons=0.2857143",
    "70kg max zero: hydration=42 salt=70 salt_store=140 salt_conc=0.5 core_temp=41.2 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.75 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=1 stamina=1 cons=0.2857143",
    "0.3kg full: hydration=0.18 salt=0.3 salt_store=0.6 salt_conc=0.5 core_temp=41.2 hunger=3 calories=6 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.75 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=1 stamina=1 cons=0.2857143",
    "0.3kg depleted: hydration=0.04 salt=0.3 salt_store=0.6 salt_conc=0.5 core_temp=41.2 hunger=0.4 calories=1.1 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.5766667 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=0.1333333 stamina=1 cons=0.2857143",
    "0.3kg pools absent: hydration=nil salt=nil salt_store=nil salt_conc=0.5 core_temp=41.2 hunger=nil calories=nil hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.2857143 mood=0.75 state_of_mind=0.2857143 heart_rate=70 circulation=1 caffeine=0 hunger=1 stamina=1 cons=0.2857143",
    "70kg cold: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=31 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=0.25 mood=0.7375 state_of_mind=0.25 heart_rate=70 circulation=1 caffeine=0 hunger=1 stamina=1 cons=0.25" ]

expectedCardio = T.intercalate "\n"
    [ "hot: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=41 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=80.8 circulation=1 caffeine=nil",
    "normal: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=37 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil",
    "cold: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=32 hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=63.7 circulation=1 caffeine=nil",
    "absent: hydration=42 salt=70 salt_store=140 salt_conc=1 core_temp=nil hunger=700 calories=1400 hypothermia=nil hyperthermia=nil salt_imbalance=nil consciousness=1 mood=nil state_of_mind=nil heart_rate=70 circulation=1 caffeine=nil" ]

expectedStarvation = T.intercalate "\n"
    [ "70kg full: frac=1 band=full speed=1 strength=1",
    "70kg depleted: frac=0.15 band=hungry speed=0.7714286 strength=1",
    "70kg present zero: frac=0 band=starving speed=0.6 strength=0.5",
    "70kg pools absent: frac=nil band=full speed=1 strength=1",
    "70kg max zero: frac=nil band=full speed=1 strength=1",
    "0.3kg full: frac=1 band=full speed=1 strength=1",
    "0.3kg depleted: frac=0.1833333 band=hungry speed=0.8095238 strength=1",
    "0.3kg pools absent: frac=nil band=full speed=1 strength=1",
    "70kg hungry: frac=0.3571429 band=full speed=1 strength=1",
    "70kg starving: frac=0.08571429 band=starving speed=0.6979592 strength=0.7857143" ]

expectedNeeds = T.intercalate "\n"
    [ "70kg full: drinkU=-inf eatU=-inf srcU=-inf searchU=3 afterDrink=42 drinks=0 afterEat=700 store=140",
    "70kg depleted: drinkU=11.78571 eatU=9.142857 srcU=-inf searchU=5.357143 afterDrink=14.5 drinks=1 afterEat=700 store=68",
    "70kg present zero: drinkU=15 eatU=10 srcU=-inf searchU=6 afterDrink=5.5 drinks=1 afterEat=700 store=63",
    "70kg pools absent: drinkU=-inf eatU=-inf srcU=-inf searchU=3 afterDrink=5.5 drinks=1 afterEat=700 store=140",
    "70kg max zero: drinkU=-inf eatU=-inf srcU=-inf searchU=3 afterDrink=42 drinks=0 afterEat=700 store=140",
    "0.3kg full: drinkU=-inf eatU=-inf srcU=-inf searchU=3 afterDrink=0.18 drinks=0 afterEat=3 store=0.6",
    "0.3kg depleted: drinkU=11.66667 eatU=8.666667 srcU=-inf searchU=5.333333 afterDrink=0.18 drinks=1 afterEat=3 store=0.6",
    "0.3kg pools absent: drinkU=-inf eatU=-inf srcU=-inf searchU=3 afterDrink=0.18 drinks=1 afterEat=3 store=0.6",
    "70kg thirsty: drinkU=10.71429 eatU=-inf srcU=-inf searchU=5.142857 afterDrink=17.5 drinks=1 afterEat=700 store=140",
    "70kg hungry: drinkU=-inf eatU=8.714286 srcU=-inf searchU=3 afterDrink=42 drinks=0 afterEat=590 store=140" ]

expectedSourceDrink = T.intercalate "\n"
    [ "low: drinking",
    "95 percent: ascending",
    "absent: drinking",
    "no max: ascending" ]

expectedCoffee = T.intercalate "\n"
    [ "70kg full: hyd=2.2 sip=0.25 after=42 caf=0.2 mood=1",
    "70kg half: hyd=2.2 sip=0.25 after=23.2 caf=0.2 mood=1",
    "70kg nearly full: hyd=2.2 sip=0.25 after=42 caf=0.2 mood=1",
    "70kg zero: hyd=2.2 sip=0.25 after=2.2 caf=0.2 mood=1",
    "70kg absent: hyd=2.2 sip=0.25 after=2.2 caf=0.2 mood=1",
    "70kg max zero: hyd=2.2 sip=0.25 after=42 caf=0.2 mood=1",
    "0.3kg half: hyd=2.2 sip=0.25 after=0.18 caf=0.2 mood=1" ]

expectedStatus = T.intercalate "\n"
    [ "70kg full: salt:50%:1,0.3,0.3 hunger:700.0 / 700.0:1,1,1 calories:1400.0 / 1400.0:1,1,1 hydration:42.0 / 42.0:1,1,1 salt:Electrolyte crisis (60%):none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%)",
    "70kg depleted: salt:50%:1,0.3,0.3 hunger:60.0 / 700.0:1,0.62,0.25 calories:210.0 / 1400.0:1,0.62,0.25 hydration:9.0 / 42.0:1,0.62,0.25 salt:Electrolyte crisis (60%):none hydration:Dehydrated:none hunger:Hungry:none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%),Dehydrated,Hungry",
    "70kg present zero: salt:50%:1,0.3,0.3 hunger:0.0 / 700.0:1,0.3,0.3 calories:0.0 / 1400.0:1,0.3,0.3 hydration:0.0 / 42.0:1,0.3,0.3 salt:Electrolyte crisis (60%):none hydration:Dehydrated:none hunger:Starving:none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%),Dehydrated,Starving",
    "70kg pools absent: salt:50%:1,0.3,0.3 hunger:?:1,1,1 calories:?:1,1,1 hydration:?:1,1,1 salt:Electrolyte crisis (60%):none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%)",
    "70kg max zero: salt:50%:1,0.3,0.3 hunger:700.0 / 0.0:1,1,1 calories:1400.0 / 0.0:1,1,1 hydration:42.0 / 0.0:1,1,1 salt:Electrolyte crisis (60%):none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%)",
    "0.3kg full: salt:50%:1,0.3,0.3 hunger:3.0 / 3.0:1,1,1 calories:6.0 / 6.0:1,1,1 hydration:0.2 / 0.2:1,1,1 salt:Electrolyte crisis (60%):none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%)",
    "0.3kg depleted: salt:50%:1,0.3,0.3 hunger:0.4 / 3.0:1,0.62,0.25 calories:1.1 / 6.0:1,0.62,0.25 hydration:0.0 / 0.2:1,0.62,0.25 salt:Electrolyte crisis (60%):none hydration:Dehydrated:none hunger:Hungry:none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%),Dehydrated,Hungry",
    "0.3kg pools absent: salt:50%:1,0.3,0.3 hunger:?:1,1,1 calories:?:1,1,1 hydration:?:1,1,1 salt:Electrolyte crisis (60%):none conds=Hypothermic (40%),Overheating (10%),Electrolyte crisis (60%)",
    "salt high: salt:150%:1,0.62,0.25 hunger:700.0 / 700.0:1,1,1 calories:1400.0 / 1400.0:1,1,1 hydration:42.0 / 42.0:1,1,1 conds=",
    "salt ideal: salt:100%:0.75,0.85,0.7 hunger:700.0 / 700.0:1,1,1 calories:1400.0 / 1400.0:1,1,1 hydration:42.0 / 42.0:1,1,1 conds=",
    "salt absent: salt:100%:0.75,0.85,0.7 hunger:700.0 / 700.0:1,1,1 calories:1400.0 / 1400.0:1,1,1 hydration:42.0 / 42.0:1,1,1 conds=" ]

