-- | The salt reserve (#2761): a meal's salt goes into @salt_store@ and the
--   reserve refills the blood pool toward its ideal level at a bounded
--   rate, so a meal cycle at the shipped eating threshold carries a unit
--   through the gap to its next meal in the reference heat.
--
--   Same fixture family as "Test.Headless.Unit.SurvivalExemption": the
--   SHIPPED @scripts\/unit_resources.lua@ (and everything it ticks:
--   thermoregulation, salts, brain, digestion) runs through the REAL
--   registered @unit.*@ API over a REAL 'unitManagerRef'. This harness has
--   no generated world, so the climate is the one input replaced:
--   @world.getClimateAt@ \/ @world.getAmbientAt@ return a fixed
--   environment, which is all thermo reads from the world.
--
--   __The reference acolyte__ (fixed, because the shipped definition
--   randomizes body and metabolism): 70 kg, 14 kg fat, 56 kg lean, 1.8 m,
--   endurance \/ constitution \/ metabolism 1.0, a 42 L water pool, a 700
--   kcal stomach and 1400 kcal calorie store (both full at the start), core
--   37 °C, standing, wearing the habit every shipped acolyte starts in, @salt@
--   and @salt_store@ seeded full, as spawn seeds a Simulated unit (#2831). The
--   fixture definition carries the @rich@ profile, so every system is
--   Simulated. Simulation cadence is
--   the shipped one: @unit_resources@ ticks every 0.1 s, and the eat
--   decision runs once per engine second (the AI's decision cadence).
--
--   __Eating is the real action.__ The decision calls
--   @scripts.unit_ai_needs@' @eatUtility@ \/ @eatExecute@ with the acolyte's
--   shipped tunables, so the threshold (@eat_max_fraction@ 0.25), the
--   marginal-ration rule, @unit.feed@ and @salts.mealSalt@ are all the
--   production code. Rations are the shipped @data\/items\/rations.yaml@
--   shape (250 kcal, discrete). Water is supplied by topping @hydration@
--   to its maximum every tick, standing in for a full canteen: drinking is
--   not under test, and a constant pool keeps the concentration's water
--   term fixed.
module Test.Headless.Unit.SaltReserve (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import qualified Data.Serialize as S
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Item.Types (ItemDef(..), ItemFood(..), ItemInstance, ItemManager(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Test.Headless.Unit.PhysiologyFixture
    (ensurePhysiologyRegistry, withProfile)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, mkItem, newBareLuaBackend)
import World.Save.Physiology (normalizeSavedPhysiology)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Infection.Types (emptyInfectionManager)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Entities
    (fromUnitInstanceDTO, toUnitInstanceDTO)
import World.Save.Types (UnitSnapshot(..), fromUnitSnapshot, toUnitSnapshot)
import Equipment.Types (emptyEquipmentClassManager)
import World.State.Types
    (WorldManager(..), emptyWorldManager, emptyWorldState)

uid1 ∷ UnitId
uid1 = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "salt_reserve_page"

-- | The reference acolyte's stored stats: its Simulated systems' keys as
--   spawn seeds them (the pool and the reserve full), nothing else absent.
referenceStats ∷ HM.HashMap Text Float
referenceStats = HM.fromList
    [ ("salt", 70), ("salt_store", 140)
    , ("body_mass", 70), ("fat_mass", 14), ("lean_mass", 56), ("height", 1.8)
    , ("endurance", 1), ("constitution", 1), ("metabolism", 1)
    , ("hydration", 42), ("max_hydration", 42)
    , ("hunger", 700), ("max_hunger", 700)
    , ("calories", 1400), ("max_calories", 1400)
    , ("stamina", 100), ("max_stamina", 100)
    , ("core_temp", 37), ("consciousness", 1), ("blood_oxygen", 1)
    , ("heart_rate", 70) ]

mkAcolyte ∷ HM.HashMap Text Float → [ItemInstance] → UnitInstance
mkAcolyte stats inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = [mkItem "acolyte_habit" 99 1.2]
    , uiFaction = resolveLegacyFaction [] FactionPlayer
    , uiWounds = [], uiScars = [], uiImmuneResponse = 0
    , uiImmunities = HM.empty, uiBlood = 70 * 0.075
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides, uiTrailState = Nothing
    }

-- | The shipped ration: discrete, 250 kcal (data\/items\/rations.yaml).
rationDef ∷ ItemDef
rationDef = ItemDef
    { idName = "rations", idDisplayName = "Rations"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.1, idWeightSpec = Nothing
    , idBulk = 0.2, idStorage = Nothing, idKind = "misc"
    , idCategory = "Supplies", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Just (ItemFood { ifCalories = 250, ifCaloriesPerKg = 0 })
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0, idSourcePath = "test-fixture"
    }

-- | The habit every shipped acolyte is created wearing
--   (data\/items\/acolyte_habit.yaml): insulation 0.6, an accessory.
habitDef ∷ ItemDef
habitDef = rationDef
    { idName = "acolyte_habit", idDisplayName = "Acolyte's Habit"
    , idWeight = 1.2, idBulk = 6.0, idKind = "accessory", idCategory = "Armor"
    , idFood = Nothing, idUnequippable = True, idInsulation = 0.6 }

rations ∷ Int → [ItemInstance]
rations n = [ mkItem "rations" (fromIntegral (100 + i)) 0.1 | i ← [1 .. n] ]

-- | One acolyte on one ACTIVE page, carrying @n@ rations.
scene ∷ EngineEnv → HM.HashMap Text Float → Int → IO ()
scene env stats n = do
    ensurePhysiologyRegistry env
    acolyteDef ← withProfile "rich" (minimalDef "acolyte" "Acolyte")
    page ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, page)], wmVisible = [fixturePage] }
    writeIORef (gameTimeRef env) 0
    writeIORef (itemManagerRef env)
        (ItemManager (HM.fromList [("rations", rationDef), ("acolyte_habit", habitDef)]))
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.singleton uid1 (mkAcolyte stats (rations n)) }

-- | The shipped modules, the controlled climate, and the runners.
--   @__sim{seconds, eat, climate, store}@ ticks the real physiology and,
--   once per engine second, runs the real eat decision. It returns one
--   line of @key=value@ pairs so no JSON decoding is needed.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    ok ← luaText ls $ T.unlines
        [ "local ai = require('scripts.unit_ai')"
        , "local resources = require('scripts.unit_resources')"
        , "local needs = require('scripts.unit_ai_needs')"
        , "local salts = require('scripts.salts')"
        , "resources.init(0)"
        , "_G.__salts = salts"
        , "_G.__env = { temp = 20, humidity = 0.5 }"
        , "world.getClimateAt = function() return { temp = __env.temp,"
        , "  summerTemp = __env.temp, winterTemp = __env.temp,"
        , "  precip = 0.2, humidity = __env.humidity, snow = 0 } end"
        , "world.getAmbientAt = function() return __env.temp end"
        , "local PARAMS = require('scripts.unit_ai_config_defaults')"
        , "  .applyAll(require('scripts.unit_ai_tunables')).acolyte"
        , "local function rationCount()"
        , "  local n = 0"
        , "  for _, it in ipairs(unit.getInventory(1) or {}) do"
        , "    if it.defName == 'rations' then n = n + 1 end"
        , "  end"
        , "  return n"
        , "end"
        , "_G.__rations = rationCount"
        , "function _G.__sim(o)"
        , "  local dt, t, nextDecision = 0.1, 0, 1.0"
        , "  local mx = salts.maxSalt(1)"
        , "  if o.store ~= nil then unit.setStat(1, 'salt_store', o.store * mx) end"
        , "  local r = { minConc = 9, maxConc = 0, minStore = 9, maxStore = 0,"
        , "              minSalt = 9, meals = 0, rations = 0, firstBelow = -1,"
        , "              minCons = 1, maxDev = 0, belowSeconds = 0 }"
        , "  local meals = {}"
        , "  local brain = require('scripts.brain')"
        , "  for _ = 1, math.floor(o.seconds / dt + 0.5) do"
        , "    t = t + dt"
        , "    unit.setStat(1, 'hydration', unit.getStat(1, 'max_hydration'))"
        , "    resources.update(dt)"
        , "    if o.eat and t >= nextDecision then"
        , "      nextDecision = nextDecision + 1.0"
        , "      if needs.eatUtility(1, {}, PARAMS) > -math.huge then"
        , "        local before, storeBefore = rationCount(), unit.getStat(1, 'salt_store') or 0"
        , "        needs.eatExecute(1, {}, PARAMS)"
        , "        local ate = before - rationCount()"
        , "        if ate > 0 then"
        , "          r.meals = r.meals + 1; r.rations = r.rations + ate"
        , "          meals[#meals + 1] = string.format('%.0f>%.3f>%.3f', t,"
        , "            storeBefore / mx, (unit.getStat(1, 'salt_store') or 0) / mx)"
        , "        end"
        , "      end"
        , "    end"
        , "    local c = unit.readPhysio(1, 'salt_conc')"
        , "    local st = (unit.getStat(1, 'salt_store') or 0) / mx"
        , "    r.minConc = math.min(r.minConc, c); r.maxConc = math.max(r.maxConc, c)"
        , "    r.minStore = math.min(r.minStore, st); r.maxStore = math.max(r.maxStore, st)"
        , "    r.minSalt = math.min(r.minSalt, unit.getStat(1, 'salt') / mx)"
        , "    r.minCons = math.min(r.minCons, brain.consciousness(1))"
        , "    r.maxDev = math.max(r.maxDev, math.abs(c - 1))"
        , "    if c < 0.75 then"
        , "      r.belowSeconds = r.belowSeconds + dt"
        , "      if r.firstBelow < 0 then r.firstBelow = t end"
        , "    end"
        , "  end"
        , "  return string.format('minConc=%.4f maxConc=%.4f minStore=%.4f "
          <> "maxStore=%.4f minSalt=%.4f meals=%d rations=%d firstBelow=%.1f "
          <> "belowSeconds=%.1f core=%.2f minCons=%.3f maxDev=%.3f meal=%s',"
        , "    r.minConc, r.maxConc, r.minStore, r.maxStore, r.minSalt,"
        , "    r.meals, r.rations, r.firstBelow, r.belowSeconds,"
        , "    unit.getStat(1, 'core_temp'), r.minCons, r.maxDev,"
        , "    table.concat(meals, ','))"
        , "end"
        , "return true" ]
    ok `shouldBe` "true"
    pure ls

luaText ∷ LuaBackendState → Text → IO Text
luaText ls src = T.strip ∘ T.dropAround (≡ '"') <$> evalDebug ls src

-- | Parse the @key=value@ line a simulation returns.
readings ∷ Text → HM.HashMap Text Double
readings out = HM.fromList
    [ (k, read (T.unpack v))
    | w ← T.words out, let (k, v0) = T.breakOn "=" w, not (T.null v0)
    , let v = T.drop 1 v0, not (T.null v), T.all (`elem` ("-.0123456789" ∷ String)) v ]

-- | Each meal as (store before, store after), in fractions of max_salt.
mealStores ∷ Text → [(Double, Double)]
mealStores out = case [ T.drop 5 w | w ← T.words out, "meal=" `T.isPrefixOf` w ] of
    (x : _) → [ (rd b, rd a) | m ← T.splitOn "," x, not (T.null m)
              , [_, b, a] ← [T.splitOn ">" m] ]
    []      → []
  where rd = read ∘ T.unpack

sim ∷ LuaBackendState → Text → IO (HM.HashMap Text Double, Text)
sim ls opts = do
    out ← luaText ls ("return __sim(" <> opts <> ")")
    pure (readings out, out)

stat ∷ LuaBackendState → Text → IO Double
stat ls name = read ∘ T.unpack <$> luaText ls
    ("return string.format('%.6f', unit.readPhysio(1, '" <> name <> "') or -1)")

setStat ∷ LuaBackendState → Text → Double → IO ()
setStat ls name v = void $ luaText ls
    ("unit.setStat(1, '" <> name <> "', " <> tshow v <> "); return true")

hot, mild ∷ Text
hot  = "__env.temp = 39.6; __env.humidity = 0.26; return true"
mild = "__env.temp = 20.0; __env.humidity = 0.5; return true"

withScene ∷ HM.HashMap Text Float → Int → (EngineEnv → LuaBackendState → IO a)
          → IO a
withScene stats n act = withHeadlessEngineNoWorld $ \env → do
    scene env stats n
    ls ← setupLua env
    act env ls

near ∷ Double → Double → Double → Expectation
near tol want got =
    abs (got - want) `shouldSatisfy` (< tol)

spec ∷ Spec
spec = describe "salt reserve (#2761)" $ do

    describe "reserve accounting" $ do
        it "starts a fresh spawn full; a ticked reserve keeps what is present" $
            withScene referenceStats 0 $ \_ ls → do
                _ ← luaText ls "__salts.tick(1, 0); return true"
                cap ← stat ls "salt_store"
                near 1e-3 140 cap         -- STORE_CAP_FRAC (2.0) of max_salt 70
                -- Present values are kept, including zero and a partial one.
                setStat ls "salt_store" 0
                _ ← luaText ls "__salts.tick(1, 0); return true"
                stat ls "salt_store" >>= near 1e-3 0
                setStat ls "salt_store" 12.5
                _ ← luaText ls "__salts.tick(1, 0); return true"
                stat ls "salt_store" >>= near 1e-3 12.5

        it "deposits a meal's salt into the reserve, not the pool, up to the cap" $
            withScene referenceStats 0 $ \_ ls → do
                setStat ls "salt" 50
                setStat ls "salt_store" 0
                _ ← luaText ls "__salts.mealSalt(1); return true"
                stat ls "salt_store" >>= near 1e-3 21      -- 0.30 of 70
                stat ls "salt" >>= near 1e-3 50            -- pool untouched
                setStat ls "salt_store" 130
                _ ← luaText ls "__salts.mealSalt(1); __salts.mealSalt(1); return true"
                stat ls "salt_store" >>= near 1e-3 140     -- capped, overflow dropped

        it "releases at the bounded rate and deducts exactly what it moves" $
            withScene referenceStats 0 $ \_ ls → do
                setStat ls "salt" 35                       -- concentration 0.5
                setStat ls "salt_store" 70
                _ ← luaText ls "__salts.tick(1, 1.0); return true"
                -- 0.002 of max_salt (0.14) per second, less the baseline drain.
                stat ls "salt_store" >>= near 1e-3 (70 - 0.14)
                stat ls "salt" >>= near 1e-3 (35 + 0.14 - 70 * 0.004 / 70)

        it "never raises the pool past its ideal level, and never releases at or above it" $
            withScene referenceStats 0 $ \_ ls → do
                -- A deficit smaller than one tick's allowance moves exactly the deficit.
                setStat ls "salt" 69.99
                setStat ls "salt_store" 70
                _ ← luaText ls "__salts.tick(1, 1.0); return true"
                c ← stat ls "salt_conc"
                c `shouldSatisfy` (<= 1.0 + 1e-4)
                -- At the ideal level the reserve stays put (apart from nothing).
                setStat ls "salt" 70
                setStat ls "salt_store" 33
                _ ← luaText ls "__salts.tick(1, 0); return true"
                stat ls "salt_store" >>= near 1e-3 33
                -- A dehydrated unit's ideal pool is smaller: no release, so
                -- the reserve cannot push it hypernatremic.
                setStat ls "hydration" 21                  -- half the water
                setStat ls "salt" 40                       -- concentration 1.14
                setStat ls "salt_store" 33
                _ ← luaText ls "__salts.tick(1, 1.0); return true"
                stat ls "salt_store" >>= near 1e-3 33
                stat ls "salt_conc" >>= \c → c `shouldSatisfy` (<= 1.15)

        it "moves salt without creating or losing any beyond the baseline drain" $
            withScene referenceStats 0 $ \_ ls → do
                setStat ls "salt" 40
                setStat ls "salt_store" 50
                before ← (+) <$> stat ls "salt" <*> stat ls "salt_store"
                _ ← luaText ls "__salts.tick(1, 2.0); return true"
                after ← (+) <$> stat ls "salt" <*> stat ls "salt_store"
                near 1e-3 (before - 70 * (0.004 / 70) * 2.0) after

        it "with an exhausted reserve reproduces the existing pool exactly" $
            withScene referenceStats 0 $ \_ ls → do
                setStat ls "salt" 60
                setStat ls "salt_store" 0
                _ ← luaText ls "__salts.tick(1, 10.0); return true"
                stat ls "salt" >>= near 1e-3 (60 - 70 * (0.004 / 70) * 10)

    describe "a mild climate with no sweating" $ do
        it "keeps the existing trajectory, plus only what the reserve contributes" $ do
            let mildRun opts = withScene referenceStats 0 $ \_ ls → do
                    _ ← luaText ls mild
                    fst <$> sim ls opts
            e ← mildRun "{ seconds = 600, eat = false, store = 0 }"
            f ← mildRun "{ seconds = 600, eat = false }"
            -- Empty reserve: the old baseline-only drain, 600 s of
            -- 0.004 / 70 per second = 3.43 % of max_salt.
            near 2e-3 (1 - 600 * 0.004 / 70) (e HM.! "minConc")
            -- Default (full) reserve: the pool is held at its ideal level.
            (f HM.! "minConc") `shouldSatisfy` (>= 0.999)
            (f HM.! "maxConc") `shouldSatisfy` (<= 1.0 + 1e-3)
            -- Sweating never started in either run.
            near 0.2 37 (e HM.! "core")

    describe "the reference heat (39.6 C, humidity 0.26), 4320 engine seconds" $ do
        it "keeps a supplied acolyte out of hyponatremia across recurring meals" $
            withScene referenceStats 40 $ \_ ls → do
                _ ← luaText ls hot
                (r, raw) ← sim ls "{ seconds = 4320, eat = true }"
                -- Never hyponatremic, never confused from salt (the brain's
                -- salt term only bites past a 0.25 deviation).
                (r HM.! "minConc") `shouldSatisfy` (>= 0.75)
                (r HM.! "belowSeconds") `shouldBe` 0
                (r HM.! "maxDev") `shouldSatisfy` (<= 0.25)
                (r HM.! "minCons") `shouldSatisfy` (>= 0.999)
                -- The meals are the recurring input: one per ~500 s, every
                -- ration eaten through the real action, each one a real
                -- deposit (the reserve rises by the meal's salt unless it
                -- is already at the cap).
                (r HM.! "meals") `shouldSatisfy` (>= 8)
                (r HM.! "rations") `shouldBe` (2 * r HM.! "meals")
                let deposits = mealStores raw
                    cap = 2.0 ∷ Double
                forM_ deposits $ \(b, a) →
                    (a - b >= 0.55 ∨ a >= cap - 0.01) `shouldBe` True
                -- The reserve is what bridges the gap, and it is refilled
                -- by the meals rather than spent once: it leaves its full
                -- start, is topped up again, and ends the run non-empty.
                (r HM.! "minStore") `shouldSatisfy` (> 0)

        it "still reaches hyponatremia with water but no food, only later than without a reserve" $ do
            let unfed opts = withScene referenceStats 0 $ \_ ls → do
                    _ ← luaText ls hot
                    fst <$> sim ls opts
            withR ← unfed "{ seconds = 4320, eat = true }"
            without ← unfed "{ seconds = 4320, eat = true, store = 0 }"
            -- No food was available, so no meal happened in either run.
            (withR HM.! "meals") `shouldBe` 0
            (without HM.! "meals") `shouldBe` 0
            (without HM.! "firstBelow") `shouldSatisfy` (> 0)
            (withR HM.! "firstBelow") `shouldSatisfy` (> without HM.! "firstBelow")
            -- Delayed, not prevented: the full reserve is gone well inside
            -- the 3 game days, so the supplied run's survival is the meals'.
            (withR HM.! "firstBelow") `shouldSatisfy` (< 4320)

    describe "persistence" $ do
        it "restores a non-full reserve exactly and does not refill it" $
            persisted (HM.insert "salt_store" 12.5 referenceStats) $ \ls → do
                stat ls "salt_store" >>= near 1e-6 12.5
                _ ← luaText ls "__salts.tick(1, 0.1); return true"
                stat ls "salt_store" >>= near 1e-3 12.5   -- seeding is for ABSENT only

        it "restores an exhausted reserve as exhausted" $
            persisted (HM.insert "salt_store" 0 referenceStats) $ \ls → do
                stat ls "salt_store" >>= near 1e-6 0
                _ ← luaText ls "__salts.tick(1, 0.1); return true"
                stat ls "salt_store" >>= near 1e-6 0

        it "gives an older payload with no reserve a full one AT LOAD (D-6 (b)), keeping the pool it did store" $
            persisted (HM.delete "salt_store" (HM.insert "salt" 33 referenceStats)) $ \ls → do
                -- #2831: the load boundary seeds only the MISSING key, so the
                -- reserve is full before the first tick (it used to be the
                -- first tick that filled it) and the stored pool is exact.
                stat ls "salt_store" >>= near 1e-6 140
                stat ls "salt" >>= near 1e-6 33
                _ ← luaText ls "__salts.tick(1, 0.1); return true"
                stat ls "salt_store" >>= near 0.2 140   -- released a little, not refilled

-- | Take a unit with the given stats through the REAL units-component path
--   a save takes (snapshot adapter, the DTO and its cereal bytes, the
--   restore), install the restored manager and hand the Lua backend to the
--   assertions. A decode failure is surfaced as a failed expectation.
persisted ∷ HM.HashMap Text Float → (LuaBackendState → IO ()) → IO ()
persisted stats act = withScene stats 0 $ \env ls → do
    um0 ← readIORef (unitManagerRef env)
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    let snap0 = toUnitSnapshot fixturePage um0
        wire s = case S.decode (S.encode (toUnitInstanceDTO s)) of
            Right dto → Right (fromUnitInstanceDTO dto)
            Left err  → Left err
    case traverse wire (usnInstances snap0) of
        Left err → expectationFailure ("DTO round trip failed: " <> err)
        Right insts → do
            let decoded = snap0 { usnInstances = insts }
                -- the load gate's deterministic normalization (#2831, D-6),
                -- applied to what the decode produced
                normalized = case normalizeSavedPhysiology reg
                                (HM.map udPhysiology (umDefs um0)) 0
                                [(fixturePage, decoded)] of
                    Right ([(_, us)], _) → us
                    _ → decoded
                (um1, _, _, _, _) = fromUnitSnapshot fixturePage
                    (umDefs um0) emptyInfectionManager
                    emptyEquipmentClassManager
                    (ItemManager (HM.fromList [("acolyte_habit", habitDef)]))
                    normalized
            writeIORef (unitManagerRef env) um1
            act ls
