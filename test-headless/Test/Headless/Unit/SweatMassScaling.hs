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
import Data.IORef (writeIORef)
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
    , uiClimbDest = Nothing, uiTrailState = Nothing
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
spec = describe "sweat debits scale with body mass (#2825)" $
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
