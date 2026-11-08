-- | Sweat salt and water debits scale with body mass (#2825).
--
--   Each example ticks the SHIPPED @thermo.tick@ once, through the REAL
--   registered @unit.*@ API over a REAL 'unitManagerRef', for one unit
--   with fixed stats: no rolled body, no wall-clock window. Only the
--   climate is controlled (this harness has no generated world), and
--   @salts.tick@ never runs, so the salt and hydration that move are the
--   sweat debits alone — no baseline or kidney excretion.
--
--   The fixed stimulus is core 38.5 °C at 0 % humidity with a balanced
--   salt_conc: sweat intensity @1.5 × (38.5 − 37.5) × 1 × 1 = 1.5@. Over
--   'dt' that is today's 70 kg debit of @0.10 × 1.5 × 4 = 0.6@ salt and
--   @0.05 × 1.5 × 4 = 0.3@ hydration, and the documented relationship
--   (@thermo.sweatMassScale@, body_mass ÷ 70) makes every body lose that
--   same FRACTION of its pools: 0.6 ÷ 70 of max_salt (= mass) and
--   0.3 ÷ 42 of max_hydration (= 0.6 × mass).
module Test.Headless.Unit.SweatMassScaling (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), emptyWorldManager, emptyWorldState)

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "sweat_mass_scaling_page"

-- | The tick length every example uses.
dt ∷ Double
dt = 4

-- | Today's absolute 70 kg debits for the fixed stimulus over 'dt'.
saltDebit70, hydrationDebit70 ∷ Double
saltDebit70      = 0.10 * 1.5 * dt
hydrationDebit70 = 0.05 * 1.5 * dt

-- | The fractions of max_salt and max_hydration the documented
--   relationship predicts for ANY body mass.
saltFrac, hydrationFrac ∷ Double
saltFrac      = saltDebit70 / 70
hydrationFrac = hydrationDebit70 / (0.6 * 70)

-- | A body of @m@ kg with full, mass-sized pools, sweating at intensity
--   1.5 (see the module header).
body ∷ Float → HM.HashMap Text Float
body m = HM.fromList
    [ ("body_mass", m), ("fat_mass", 0.2 * m), ("lean_mass", 0.8 * m)
    , ("constitution", 1), ("metabolism", 1), ("heart_rate", 70)
    , ("hydration", 0.6 * m), ("max_hydration", 0.6 * m)
    , ("salt", m), ("salt_conc", 1), ("core_temp", 38.5) ]

-- | Brown-bear mean mass: 22 · 1.4² · 4 (data/units/bear_brown.yaml).
bearMass ∷ Float
bearMass = 172.48

mkUnit ∷ Float → HM.HashMap Text Float → UnitInstance
mkUnit m stats = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFaction = resolveLegacyFaction [] FactionHostile
    , uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 0.075 * m, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides, uiTrailState = Nothing
    }

-- | One unit (id 1) on an active page with these stats.
scene ∷ EngineEnv → HM.HashMap Text Float → IO ()
scene env stats = do
    page ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, page)], wmVisible = [fixturePage] }
    writeIORef (gameTimeRef env) 0
    let m = HM.findWithDefault 70 "body_mass" stats
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList [("acolyte", minimalDef "acolyte" "Acolyte")]
        , umInstances = HM.fromList [(UnitId 1, mkUnit m stats)] }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- | The shipped thermo module under a fixed 30 °C climate at the given
--   humidity, and @__sweat(dt)@: one real thermo.tick, then the unit's
--   salt and hydration as @salt|hydration@ (@nil@ when a stat is absent).
setupLua ∷ EngineEnv → Double → IO LuaBackendState
setupLua env humidity = do
    ls ← newBareLuaBackend env
    ok ← luaText ls $ T.unlines
        [ "_G.__thermo = require('scripts.thermo')"
        , "world.getClimateAt = function() return { temp = 30,"
        , "  summerTemp = 30, winterTemp = 30, precip = 0.2,"
        , "  humidity = " <> tshow humidity <> ", snow = 0 } end"
        , "world.getAmbientAt = function() return 30 end"
        , "local function fmt(v)"
        , "  if v == nil then return 'nil' end"
        , "  return string.format('%.9g', v)"
        , "end"
        , "function _G.__sweat(dt)"
        , "  __thermo.tick(1, { gridX = 10, gridY = 10 }, dt)"
        , "  return fmt(unit.getStat(1, 'salt')) .. '|'"
        , "    .. fmt(unit.getStat(1, 'hydration'))"
        , "end"
        , "return true" ]
    ok `shouldBe` "true"
    pure ls

luaText ∷ LuaBackendState → Text → IO Text
luaText ls src = T.strip ∘ T.dropAround (≡ '"') <$> evalDebug ls src

-- | Salt and hydration after one sweat tick of @seconds@.
data Pools = Pools { pSalt ∷ Maybe Double, pHydration ∷ Maybe Double }
    deriving (Show, Eq)

sweatTick ∷ EngineEnv → HM.HashMap Text Float → Double → Double → IO Pools
sweatTick env stats humidity seconds = do
    scene env stats
    ls ← setupLua env humidity
    out ← luaText ls ("return __sweat(" <> tshow seconds <> ")")
    case T.splitOn "|" out of
        [s, h] → pure (Pools (readMaybe' s) (readMaybe' h))
        _ → error ("unexpected __sweat result: " <> T.unpack out)
  where
    readMaybe' "nil" = Nothing
    readMaybe' t     = Just (read (T.unpack t))

-- | Stored stats are Float: compare at Float precision.
shouldBeNear ∷ Double → Double → Expectation
shouldBeNear actual expected =
    actual `shouldSatisfy` (\a → abs (a - expected) ≤ 1e-4 * max 1 (abs expected))

-- | A debit fraction, compared relative to its own size (it is small).
shouldBeFraction ∷ Double → Double → Expectation
shouldBeFraction actual expected =
    actual `shouldSatisfy` (\a → abs (a - expected) ≤ 1e-3 * expected)

-- | The fractions of max_salt and max_hydration one tick removed.
fractions ∷ Float → Pools → (Double, Double)
fractions m (Pools (Just s) (Just h)) =
    let md = realToFrac m
    in ((md - s) / md, (0.6 * md - h) / (0.6 * md))
fractions _ p = error ("missing pool: " <> show p)

spec ∷ Spec
spec = do
    sweatSpec
    heatBalanceSpec

sweatSpec ∷ Spec
sweatSpec = describe "sweat debits scale with body mass (#2825)" $
  around withHeadlessEngineNoWorld $ do
    it "keeps today's absolute salt and hydration debits at 70 kg" $ \env → do
        Pools (Just s) (Just h) ← sweatTick env (body 70) 0 dt
        (70 - s) `shouldBeNear` saltDebit70
        (42 - h) `shouldBeNear` hydrationDebit70

    it "takes a 0.3 kg body's pools down by the same fractions as 70 kg, \
       \not about 233 times more" $ \env → do
        small ← fractions 0.3 <$> sweatTick env (body 0.3) 0 dt
        human ← fractions 70  <$> sweatTick env (body 70) 0 dt
        fst small `shouldBeFraction` saltFrac
        snd small `shouldBeFraction` hydrationFrac
        (fst small / fst human) `shouldBeFraction` 1
        (snd small / snd human) `shouldBeFraction` 1

    it "takes a brown-bear-sized body's pools down by the same fractions" $
      \env → do
        (sf, hf) ← fractions bearMass <$> sweatTick env (body bearMass) 0 dt
        sf `shouldBeFraction` saltFrac
        hf `shouldBeFraction` hydrationFrac

    it "debits a unit with no body_mass as a 70 kg body" $ \env → do
        Pools (Just s) (Just h) ←
            sweatTick env (HM.delete "body_mass" (body 70)) 0 dt
        (70 - s) `shouldBeNear` saltDebit70
        (42 - h) `shouldBeNear` hydrationDebit70

    it "debits nothing, and stays finite, at zero body mass" $ \env → do
        let stats = HM.union (HM.fromList
              [ ("salt", 1), ("hydration", 1), ("max_hydration", 1) ]) (body 0)
        sweatTick env stats 0 dt `shouldReturn` Pools (Just 1) (Just 1)

    it "scales to 0 for zero, negative and non-finite mass" $ \env → do
        scene env (body 70)
        ls ← setupLua env 0
        luaText ls "local f = __thermo.sweatMassScale; \
                   \return f(0) == 0 and f(-5) == 0 and f(0/0) == 0 \
                   \and f(math.huge) == 0 and f(nil) == 0 and f(70) == 1"
            `shouldReturn` "true"

    it "debits nothing when the body does not sweat" $ \env → do
        Pools (Just s0) (Just h0) ←
            sweatTick env (HM.insert "core_temp" 37 (body 0.3)) 0 dt
        s0 `shouldBeNear` 0.3
        h0 `shouldBeNear` 0.18
        -- Saturated air: core is past onset but no sweat evaporates.
        Pools (Just s) (Just h) ← sweatTick env (body 0.3) 1 dt
        s `shouldBeNear` 0.3
        h `shouldBeNear` 0.18

    it "debits nothing over a zero-length tick" $ \env → do
        Pools (Just s) (Just h) ← sweatTick env (body 0.3) 0 0
        s `shouldBeNear` 0.3
        h `shouldBeNear` 0.18

    it "clamps both pools at zero" $ \env → do
        let nearlyDry = HM.union (HM.fromList
              [ ("salt", 0.001), ("hydration", 0.001) ]) (body 0.3)
        sweatTick env nearlyDry 0 dt `shouldReturn` Pools (Just 0) (Just 0)

    it "drains salt and grows no hydration stat for a unit with no \
       \hydration pool" $ \env → do
        let dryBody = HM.delete "hydration" $ HM.delete "max_hydration" (body 0.3)
        Pools (Just s) h ← sweatTick env dryBody 0 dt
        ((0.3 - s) / 0.3) `shouldBeFraction` saltFrac
        h `shouldBe` Nothing


-----------------------------------------------------------------------
-- The heat-balance read (#2960)
-----------------------------------------------------------------------
--
-- thermo.tick is now gather -> rule -> apply, and the rule is readable
-- through thermo.readHeatBalance without applying anything. These examples
-- pin the two promises that make the read trustworthy for the physiology
-- probe: it mutates nothing, and the tick still does exactly what the
-- pre-extraction tick did.
--
-- "Exactly" is checked against a verbatim, non-mutating transcription of the
-- pre-extraction @thermo.tick@ (the @legacy@ function below, from
-- scripts/thermo.lua at the parent of #2960) that returns what that tick
-- would have written: the new core temperature and, in call order, every
-- @physiology.debit@ and @effects.frostbite@ it would have made. The real
-- tick runs against the same state with both seams recording, and the two
-- reports must be identical strings (same floating-point operations; the
-- legacy core is rounded to binary32, as the stored stat is, so no tolerance).

-- | One tick scenario.
data HeatCase = HeatCase
    { hcName     ∷ Text
    , hcStats    ∷ HM.HashMap Text Float
    , hcAmbient  ∷ Double
    , hcHumidity ∷ Double
    , hcActivity ∷ Text
    , hcInfection ∷ Double
    , hcSweatFactor ∷ Double   -- ^ what salts.sweatFactor answers
    , hcDebug    ∷ Maybe (Double, Double)   -- ^ (debugAmbient, debugHumidity)
    }

heatCases ∷ [HeatCase]
heatCases =
    [ c "hot and dry, 70 kg: sweating" (body 70) 30 0
    , c "hot and dry, 0.3 kg: the stability clamp binds" (body 0.3) 30 0
    , c "hot and humid: no evaporation" (body 70) 30 1
    , c "cold in deep winter: shivering and frostbite" cold (-30) 0.5
    , c "mildly cold, starving: calorie-throttled heat"
        (HM.union (HM.fromList [("calories", 1), ("max_calories", 2000)])
                  (HM.insert "core_temp" 36 (body 70))) 5 0.5
    , (c "walking in the cold" (HM.insert "core_temp" 36 (body 70)) 20 0.5)
        { hcActivity = "walking" }
    , c "zero body mass" (body 0) 30 0.2
    , c "no body mass at all" (HM.delete "body_mass" (body 70)) 30 0.2
    , (c "salt-poor: sweat impaired" (body 70) 30 0) { hcSweatFactor = 0.65 }
    , (c "feverish: shivers and sweats about a raised setpoint"
        (HM.insert "core_temp" 38 (body 70)) 20 0.3) { hcInfection = 0.5 }
    , (c "debug overrides replace the sampled climate" (body 70) 30 0.9)
        { hcDebug = Just (-10, 0.1) }
    , c "a metabolic outlier" (HM.insert "metabolism" 1.7 (body 55)) 25 0.3
    ]
  where
    c n st amb hum = HeatCase n st amb hum "idle" 0 1 Nothing
    cold = HM.insert "core_temp" 34 (body 70)

-- | The Lua fixture: the shipped modules with recording seams, a
--   non-mutating transcription of the pre-extraction tick, and the two
--   comparison entry points.
heatLua ∷ EngineEnv → HeatCase → IO LuaBackendState
heatLua env hc = do
    ls ← newBareLuaBackend env
    ok ← luaText ls $ T.unlines
        [ "_G.__thermo = require('scripts.thermo')"
        , "local physiology = require('scripts.physiology')"
        , "local effects = require('scripts.unit_effects')"
        , "local salts = require('scripts.salts')"
        , "local circulation = require('scripts.circulation')"
        , "local injuries = require('scripts.injuries')"
        , "local clamp = require('scripts.lib.numeric').clamp"
        , "local function fmt(v)"
        , "  if v == nil then return 'nil' end"
        , "  return string.format('%.9g', v)"
        , "end"
        , "world.getClimateAt = function() return { temp = "
            <> tshow (hcAmbient hc) <> ", humidity = " <> tshow (hcHumidity hc)
            <> ", precip = 0.2, snow = 0 } end"
        , "world.getAmbientAt = function() return " <> tshow (hcAmbient hc) <> " end"
        , "injuries.infectionLoad = function() return " <> tshow (hcInfection hc) <> " end"
        , "salts.sweatFactor = function() return " <> tshow (hcSweatFactor hc) <> " end"
        , case hcDebug hc of
            Nothing → "__thermo.debugAmbient, __thermo.debugHumidity = nil, nil"
            Just (a, h) → "__thermo.debugAmbient, __thermo.debugHumidity = "
                          <> tshow a <> ", " <> tshow h
        , "LOG = {}"
        , -- Recording seams only: the fixture unit owns no stamina pool, and
          -- the question is what the tick ASKS for, not what the engine does.
          "physiology.debit = function(uid, key, amount)"
        , "  LOG[#LOG + 1] = 'debit:' .. key .. '=' .. fmt(amount)"
        , "  return 0"
        , "end"
        , "effects.frostbite = function(uid, part, nec)"
        , "  LOG[#LOG + 1] = 'frost:' .. part .. '=' .. fmt(nec)"
        , "end"
        , "local function report(core)"
        , "  return fmt(core) .. '|' .. table.concat(LOG, ',')"
        , "end"
        , -- The pre-extraction tick, constants and all, minus its writes.
          "local BASAL_HEAT, ACTIVITY_HEAT = 0.9, 0.6"
        , "local HUNGER_HEAT_FLOOR_FRAC, HUNGER_HEAT_MIN = 0.5, 0.6"
        , "local LOSS_COEFF, INSUL_BASE, FAT_INSUL_K = 0.10, 1.0, 2.0"
        , "local CLOTHING_INSUL_K, THERMAL_INERTIA = 0.5, 0.003"
        , "local HUMAN_MASS, MAX_DTEMP_PER_TICK = 70.0, 1.5"
        , "local SHIVER_START, SHIVER_K, SHIVER_MAX = 36.5, 1.2, 2.5"
        , "local SWEAT_START, SWEAT_K, SWEAT_MAX = 37.5, 1.5, 4.0"
        , "local SHIVER_STAMINA_K, SWEAT_HYDRATION_K, SWEAT_SALT_K = 0.04, 0.05, 0.10"
        , "local CIRC_LOSS_FLOOR = 0.5"
        , "local FROST_AMBIENT, FROST_AMBIENT_SCALE = 0.0, 25.0"
        , "local FROST_CIRC_REF, FROST_RATE = 1.05, 0.015"
        , "local FROST_PARTS = { 'l_hand', 'r_hand', 'l_foot', 'r_foot' }"
        , "local FEVER_OFFSET_K, FEVER_MAX_OFFSET = 2.0, 3.5"
        , "local MOVING = { walking = true, running = true, sprinting = true }"
        , "local function legacy(uid, info, dt)"
        , "  local gx, gy = info.gridX, info.gridY"
        , "  local clim = world.getClimateAt(gx, gy)"
        , "  local core = physiology.get(uid, 'core_temp')"
        , "  local circ = circulation.get(uid) or 1.0"
        , "  local ambient = world.getAmbientAt and world.getAmbientAt(gx, gy)"
        , "  if not ambient then ambient = clim.temp or 20.0 end"
        , "  local humidity = clamp(clim.humidity or 0.5, 0, 1)"
        , "  if __thermo.debugAmbient then ambient = __thermo.debugAmbient end"
        , "  if __thermo.debugHumidity then humidity = __thermo.debugHumidity end"
        , "  local metab = unit.getStat(uid, 'metabolism') or 1.0"
        , "  local fatM = unit.getStat(uid, 'fat_mass') or 0.0"
        , "  local bodyM = unit.getStat(uid, 'body_mass') or 70.0"
        , "  local fatFrac = (bodyM > 0) and (fatM / bodyM) or 0.2"
        , "  local clothing = (unit.getInsulation and unit.getInsulation(uid)) or 0"
        , "  local insul = INSUL_BASE + fatFrac * FAT_INSUL_K + clothing * CLOTHING_INSUL_K"
        , "  local calFactor = 1.0"
        , "  local frac = physiology.fraction(uid, 'calories', 'max_calories')"
        , "  if frac and frac < HUNGER_HEAT_FLOOR_FRAC then"
        , "    calFactor = HUNGER_HEAT_MIN + (1 - HUNGER_HEAT_MIN) * (frac / HUNGER_HEAT_FLOOR_FRAC)"
        , "  end"
        , "  local activity = unit.getActivity(uid) or 'idle'"
        , "  local production = BASAL_HEAT * metab * calFactor"
        , "  if MOVING[activity] then production = production + ACTIVITY_HEAT * calFactor end"
        , "  local feverOffset = math.min(FEVER_MAX_OFFSET, FEVER_OFFSET_K * injuries.infectionLoad(uid))"
        , "  local shiverStart = SHIVER_START + feverOffset"
        , "  local sweatStart = SWEAT_START + feverOffset"
        , "  local shiver = 0"
        , "  if core < shiverStart then"
        , "    shiver = math.min(SHIVER_MAX, SHIVER_K * (shiverStart - core)) * calFactor"
        , "    production = production + shiver"
        , "  end"
        , "  local circLoss = CIRC_LOSS_FLOOR + (1 - CIRC_LOSS_FLOOR) * circ"
        , "  local loss = (core - ambient) * LOSS_COEFF / insul * circLoss"
        , "  local sweat = 0"
        , "  if core > sweatStart then"
        , "    sweat = math.min(SWEAT_MAX, SWEAT_K * (core - sweatStart) * (1 - humidity))"
        , "    sweat = sweat * salts.sweatFactor(uid)"
        , "  end"
        , "  local inertia = THERMAL_INERTIA * (HUMAN_MASS / math.max(bodyM, 0.001))"
        , "  local dTemp = (production - loss - sweat) * dt * inertia"
        , "  dTemp = clamp(dTemp, -MAX_DTEMP_PER_TICK, MAX_DTEMP_PER_TICK)"
        , "  core = clamp(core + dTemp, 15.0, 45.0)"
        , "  if shiver > 0 then"
        , "    LOG[#LOG + 1] = 'debit:stamina=' .. fmt(SHIVER_STAMINA_K * shiver * dt)"
        , "  end"
        , "  if sweat > 0 then"
        , "    local massScale = __thermo.sweatMassScale(bodyM)"
        , "    LOG[#LOG + 1] = 'debit:hydration=' .. fmt(SWEAT_HYDRATION_K * massScale * sweat * dt)"
        , "    LOG[#LOG + 1] = 'debit:salt=' .. fmt(SWEAT_SALT_K * massScale * sweat * dt)"
        , "  end"
        , "  if ambient < FROST_AMBIENT then"
        , "    local drive = clamp((FROST_AMBIENT - ambient) / FROST_AMBIENT_SCALE, 0, 1)"
        , "                * clamp(FROST_CIRC_REF - circ, 0, 1)"
        , "    if drive > 0.001 then"
        , "      local nec = FROST_RATE * drive * dt"
        , "      for _, part in ipairs(FROST_PARTS) do"
        , "        LOG[#LOG + 1] = 'frost:' .. part .. '=' .. fmt(nec)"
        , "      end"
        , "    end"
        , "  end"
        , -- The real tick's core is read back from a binary32 stat.
          "  return (string.unpack('f', string.pack('f', core)))"
        , "end"
        , "local INFO = { gridX = 10, gridY = 10 }"
        , -- What the pre-extraction tick would write, from the current state.
          "function _G.__legacy(dt)"
        , "  LOG = {}"
        , "  local core = legacy(1, INFO, dt)"
        , "  return report(core)"
        , "end"
        , -- What the real tick wrote.
          "function _G.__real(dt)"
        , "  LOG = {}"
        , "  __thermo.tick(1, INFO, dt)"
        , "  return report(unit.getStat(1, 'core_temp'))"
        , "end"
        , "function _G.__receipt(dt)"
        , "  local r = __thermo.tick(1, INFO, dt)"
        , "  if not r then return 'nil' end"
        , "  return fmt(r.dt) .. '|' .. fmt(r.delta) .. '|' .. fmt(r.applied)"
        , "end"
        , "function _G.__snapshot()"
        , "  local keys = {}"
        , "  for k in pairs(unit.getAllStats(1) or {}) do keys[#keys + 1] = k end"
        , "  table.sort(keys)"
        , "  local out = {}"
        , "  for _, k in ipairs(keys) do out[#out + 1] = k .. '=' .. fmt(unit.getAllStats(1)[k]) end"
        , "  return table.concat(out, ';')"
        , "end"
        , "return true" ]
    ok `shouldBe` "true"
    pure ls

-- | Put the case's unit on the page, with its activity.
heatScene ∷ EngineEnv → HeatCase → IO ()
heatScene env hc = do
    scene env (hcStats hc)
    modifyIORef' (unitManagerRef env) $ \um → um
        { umInstances = HM.adjust (\u → u { uiActivity = hcActivity hc })
                                  (UnitId 1) (umInstances um) }

heatBalanceSpec ∷ Spec
heatBalanceSpec = describe "the heat-balance read and the tick it shares (#2960)" $
  around withHeadlessEngineNoWorld $ do

    it "leaves the tick's results, sweat/shiver costs and frostbite exactly as \
       \the pre-extraction tick had them, across hot, cold, starving, \
       \walking, fever, tiny and massless bodies" $ \env → do
        results ← forM heatCases $ \hc → do
            heatScene env hc
            ls ← heatLua env hc
            expected ← luaText ls "return __legacy(4)"
            actual   ← luaText ls "return __real(4)"
            pure (hcName hc, expected, actual)
        [ (n, e, a) | (n, e, a) ← results, e ≢ a ] `shouldBe` []
        -- the comparison is not vacuous: costs and clamps were exercised
        let reports = [ e | (_, e, _) ← results ]
        any (T.isInfixOf "debit:stamina") reports `shouldBe` True
        any (T.isInfixOf "debit:hydration") reports `shouldBe` True
        any (T.isInfixOf "frost:l_hand") reports `shouldBe` True

    it "moves a 70 kg sweating body by the hand-computed step, and a 0.3 kg \
       \one by exactly the stability clamp" $ \env → do
        -- core 38.5, 30 C, 0 % humidity, circulation 1, insulation 1.4:
        -- 0.9 - (8.5 * 0.1 / 1.4) - 1.5 = -1.2071428; x dt 4 x 0.003.
        let hot = head' heatCases
        heatScene env hot
        ls ← heatLua env hot
        out70 ← luaText ls "return __real(4)"
        let core70 = read (T.unpack (T.takeWhile (≢ '|') out70)) ∷ Double
        abs (core70 - (38.5 + (-1.2071428571) * 4 * 0.003)) `shouldSatisfy` (< 1e-5)
        let tiny = heatCases !! 1
        heatScene env tiny
        ls2 ← heatLua env tiny
        outTiny ← luaText ls2 "return __real(4)"
        let coreTiny = read (T.unpack (T.takeWhile (≢ '|') outTiny)) ∷ Double
        abs (coreTiny - (38.5 - 1.5)) `shouldSatisfy` (< 1e-4)

    it "reads the balance without changing anything: no stat, wound, pool \
       \debit or frostbite, however often it is read" $ \env → do
        forM_ [heatCases !! 0, heatCases !! 3] $ \hc → do
            heatScene env hc
            ls ← heatLua env hc
            um0 ← readIORef (unitManagerRef env)
            before ← luaText ls "return __snapshot()"
            readsOut ← luaText ls
                "LOG = {}; local r = 0; \
                \for i = 1, 5 do local b = __thermo.readHeatBalance(1); \
                \r = r + (b and 1 or 0) end; \
                \return r .. '|' .. #LOG"
            readsOut `shouldBe` "5|0"
            after ← luaText ls "return __snapshot()"
            um1 ← readIORef (unitManagerRef env)
            after `shouldBe` before
            let inst u = HM.lookup (UnitId 1) (umInstances u)
            fmap uiStats (inst um1) `shouldBe` fmap uiStats (inst um0)
            fmap (length ∘ uiWounds) (inst um1)
                `shouldBe` fmap (length ∘ uiWounds) (inst um0)

    it "predicts, for the state it reads, the step the tick then takes" $ \env →
      forM_ [heatCases !! 0, heatCases !! 3, heatCases !! 5] $ \hc → do
        heatScene env hc
        ls ← heatLua env hc
        out ← luaText ls
            "local b = __thermo.readHeatBalance(1); \
            \local c0 = unit.getStat(1, 'core_temp'); \
            \__thermo.tick(1, { gridX = 10, gridY = 10 }, 0.2); \
            \local c1 = unit.getStat(1, 'core_temp'); \
            \return string.format('%.9g|%.9g', b.rate * 0.2, c1 - c0)"
        case T.splitOn "|" out of
            [predicted, moved] →
                abs (read (T.unpack predicted) - read (T.unpack moved) ∷ Double)
                    `shouldSatisfy` (< 2e-5)
            _ → expectationFailure ("unexpected reply " <> T.unpack out)

    it "returns the executed step as a receipt, and nothing when there is no \
       \climate to balance" $ \env → do
        let hot = head' heatCases
        heatScene env hot
        ls ← heatLua env hot
        receipt ← luaText ls "return __receipt(4)"
        case T.splitOn "|" receipt of
            [dtT, delta, applied] → do
                (read (T.unpack dtT) ∷ Double) `shouldBe` 4
                abs (read (T.unpack delta) - (-1.2071428571) * 4 * 0.003 ∷ Double)
                    `shouldSatisfy` (< 1e-8)
                read (T.unpack applied) `shouldBe` (read (T.unpack delta) ∷ Double)
            _ → expectationFailure ("unexpected receipt " <> T.unpack receipt)
        luaText ls "world.getClimateAt = function() return nil end; \
                   \return tostring(__receipt(4)) .. '|' .. \
                   \tostring(__thermo.readHeatBalance(1))"
            `shouldReturn` "nil|nil"

    it "evaluates the rule from an explicit inputs table alone, with the \
       \world and the unit out of reach" $ \env → do
        let hot = head' heatCases
        heatScene env hot
        ls ← heatLua env hot
        luaText ls
            "local ok, r = pcall(function() \
            \  local g1, g2, g3 = unit.getStat, world.getClimateAt, unit.getInfo; \
            \  unit.getStat, world.getClimateAt, unit.getInfo = nil, nil, nil; \
            \  local b = __thermo.heatBalance({ core = 38.5, circ = 1, \
            \    ambient = 30, humidity = 0, metab = 1, fatM = 14, bodyM = 70, \
            \    clothing = 0, calFrac = nil, moving = false, feverOffset = 0, \
            \    sweatFactor = 1 }); \
            \  unit.getStat, world.getClimateAt, unit.getInfo = g1, g2, g3; \
            \  return b end); \
            \return tostring(ok) .. '|' .. string.format('%.7f', r.rate)"
            `shouldReturn` "true|-0.0036214"
  where
    head' (c : _) = c
    head' []      = error "no heat cases"
