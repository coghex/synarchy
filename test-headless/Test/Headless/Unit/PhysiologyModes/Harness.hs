{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The shared harness of the "physiology stat modes" group (#2831, S4 of
--   epic #2827): a one-page headless scene, the SHIPPED physiology Lua
--   (@scripts\/unit_resources.lua@ and everything it ticks) over the REAL
--   registered @unit.*@ API and a REAL 'unitManagerRef', and units built by
--   the same seeding rule spawn and load use.
--
--   The pattern is "Test.Headless.Unit.SurvivalExemption"'s: this harness
--   has no generated world, so @world.getClimateAt@ \/ @world.getAmbientAt@
--   return a fixed environment, which is all thermo reads from the world,
--   and the engine runs no unit thread, so @unit.kill@ \/ @unit.collapse@
--   only ENQUEUE (read back with 'killed' \/ 'collapses'). Nothing here is
--   rolled: every input is a literal.
--
--   __Units are built, not parsed.__ 'buildUnit' takes a profile or explicit
--   per-system modes, a body and extra stats, and runs
--   'Unit.Physiology.Transition.reconcileStoredKeys' on them: a Simulated
--   system gets exactly the keys a spawned unit would, a Nominal or
--   Unsupported one none. A test that wants a different stored state
--   overrides it afterwards through the real verbs.
module Test.Headless.Unit.PhysiologyModes.Harness
    ( -- * Bodies
      Body(..)
    , acolyteBody
    , squirrelBody
    , bodyStats
      -- * Units and scenes
    , UnitSpec(..)
    , unitSpec
    , buildUnit
    , Scene
    , withScene
    , loadScene
    , readInstance
    , modifyInstance
    , killed
    , collapses
    , drainCommands
    , writeGameTime
      -- * Lua
    , luaText
    , luaNum
    , luaBool
    , scriptErrors
    , readOwned
    , climate
    , runSeconds
    , Extremes(..)
    , track
    , trackWith
    , survivalEvents
      -- * Comparison
    , near
    , nominalKeys
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified Engine.Core.Queue as Q
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.PhysiologyFixture
    ( ensurePhysiologyRegistry, profilePhysiology
    , readShippedPhysiologyRegistry )
import Test.Headless.Unit.TransferApi (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology (PhysioContext(..))
import Unit.Physiology.Transition (reconcileStoredKeys)
import Unit.Physiology.Types
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), emptyWorldManager, emptyWorldState)

-----------------------------------------------------------------------
-- Bodies
-----------------------------------------------------------------------

-- | A body: mass in kg and height in metres. The shipped body block's
--   composition rules give everything else ('bodyStats').
data Body = Body { bodyMass ∷ Float, bodyHeight ∷ Float }

-- | The reference 70 kg acolyte (#2761's fixture).
acolyteBody ∷ Body
acolyteBody = Body 70 1.8

-- | A red squirrel's 0.3 kg (T-1, T-12, T-13).
squirrelBody ∷ Body
squirrelBody = Body 0.3 0.22

-- | The stored stats a body carries, scaled the way the shipped body block
--   scales them: 0.6 L of water, 10 kcal of stomach and 20 kcal of
--   calorie store per kg, a 20 % fat share, and a @frame_mass@ equal to
--   the mass (bulk 1.0). The unowned inputs every physiology reader needs
--   are present; the OWNED keys are not here, they are seeded.
bodyStats ∷ Body → [(Text, Float)]
bodyStats (Body m h) =
    [ ("body_mass", m), ("frame_mass", m)
    , ("fat_mass", 0.2 * m), ("lean_mass", 0.8 * m), ("height", h)
    , ("strength", 1), ("strength_base", 1), ("strength_body", 1)
    , ("endurance", 1), ("constitution", 1), ("metabolism", 1)
    , ("max_hydration", 0.6 * m), ("max_hunger", 10 * m)
    , ("max_calories", 20 * m)
    , ("stamina", 100), ("max_stamina", 100)
    , ("consciousness", 1), ("blood_oxygen", 1), ("heart_rate", 70) ]

-----------------------------------------------------------------------
-- Units
-----------------------------------------------------------------------

-- | One unit to place in the scene.
data UnitSpec = UnitSpec
    { usId        ∷ !Int
    , usDefName   ∷ !Text
      -- ^ the definition name: it selects the SPECIES resource config
      --   (@acolyte@, @red_squirrel@ …) as in the shipped game.
    , usProfile   ∷ !(Maybe Text)
      -- ^ a shipped profile; 'Nothing' is a bodyless definition (every
      --   system Unsupported).
    , usModes     ∷ ![(Text, PhysioMode)]
      -- ^ per-system DEFAULTS laid over the profile (a definition's own
      --   per-system entries), not overrides.
    , usOverrides ∷ ![(Text, PhysioMode)]
      -- ^ the unit's stored per-system overrides.
    , usBody      ∷ !Body
    , usStats     ∷ ![(Text, Float)]
      -- ^ extra stats, replacing a body stat of the same name.
    , usDropStats ∷ ![Text]
      -- ^ body stats the unit does NOT carry (the default body stores an
      --   explicit @max_stamina@ and a @stamina@ pool, which the stamina
      --   specs must be able to omit: a unit with no explicit attribute
      --   resolves its maximum from @endurance@).
    , usEngagement ∷ ![(Text, PhysioMode)]
      -- ^ the definition's @on_combat_engagement:@ entries (SD-20).
    , usModifiers ∷ ![(Text, [StatModifier])]
    , usActivity  ∷ !Text
    , usPose      ∷ !Text
    , usBlood     ∷ !(Maybe Float)
    , usFaction   ∷ !Faction
    , usWounds    ∷ ![Wound]
    , usBodyTemplates ∷ ![(Text, (Float, Float))]
      -- ^ the definition's @body:@ templates (centre, range): the species
      --   mean body fat the Nominal upkeep recovers toward is
      --   @bodyfat@'s centre.
    }

-- | A standing, idle, player-faction unit of the given definition and
--   profile, with the reference body.
unitSpec ∷ Int → Text → Maybe Text → UnitSpec
unitSpec uid def profile = UnitSpec
    { usId = uid, usDefName = def, usProfile = profile
    , usModes = [], usOverrides = [], usBody = acolyteBody
    , usStats = [], usDropStats = [], usEngagement = []
    , usModifiers = [], usActivity = "idle"
    , usPose = "standing", usBlood = Nothing, usFaction = FactionPlayer
    , usWounds = [], usBodyTemplates = [] }

-- | The definition a spec names: a minimal one carrying its physiology.
specDef ∷ UnitSpec → IO UnitDef
specDef sp = do
    up ← case usProfile sp of
        Nothing → pure noUnitPhysiology
        Just p  → do
            base ← profilePhysiology p
            pure base { upDefaults = HM.union (HM.fromList (usModes sp))
                                              (upDefaults base)
                      , upEngagement = HM.fromList (usEngagement sp) }
    pure (minimalDef (usDefName sp) (usDefName sp))
        { udPhysiology = up
        , udBodyTemplates = HM.fromList (usBodyTemplates sp) }

-- | The unit instance: the body and extra stats, then the Simulated
--   systems' keys seeded by the SAME rule spawn uses. A seed that cannot
--   be computed (a spec bug) fails the test loudly.
buildUnit ∷ UnitDef → UnitSpec → IO UnitInstance
buildUnit def sp = do
    reg ← readShippedPhysiologyRegistry
    let stats0 = HM.filterWithKey (\k _ → k `notElem` usDropStats sp) $
            HM.union (HM.fromList (usStats sp))
                     (HM.fromList (bodyStats (usBody sp)))
        mods   = HM.fromList (usModifiers sp)
        ctx    = PhysioContext
            { pcRegistry = reg, pcPhysiology = udPhysiology def
            , pcOverrides = HM.fromList (usOverrides sp), pcNow = 0
            , pcStats = stats0, pcModifiers = mods }
    stats ← case reconcileStoredKeys ctx of
        Right (s, _) → pure s
        Left why → do
            expectationFailure ("cannot seed " <> show (usDefName sp)
                                <> ": " <> show why)
            pure stats0
    let mass = bodyMass (usBody sp)
    pure UnitInstance
        { uiDefName = usDefName sp, uiName = "", uiPage = fixturePage
        , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
        , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
        , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
        , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
        , uiActivity = usActivity sp, uiPose = usPose sp, uiAnimStride = 1
        , uiStats = stats, uiModifiers = mods, uiSkills = HM.empty
        , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
        , uiAccessories = []
        , uiFaction = resolveLegacyFaction [] (usFaction sp)
        , uiWounds = usWounds sp, uiScars = [], uiImmuneResponse = 0
        , uiImmunities = HM.empty
        , uiBlood = fromMaybe (mass * bloodMassRatio) (usBlood sp)
        , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
        , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
        , uiClimbDest = Nothing
        , uiPhysioOverrides = HM.fromList (usOverrides sp)
        , uiTrailState = Nothing }

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "physiology_modes_page"

-- | Everything a spec needs: the engine and its Lua backend.
type Scene = (EngineEnv, LuaBackendState)

-- | Replace the session with the given units on one ACTIVE page
--   (@unit.getAllIds@ lists the active page only) and game time 0.
loadScene ∷ EngineEnv → [UnitSpec] → IO ()
loadScene env specs = do
    ensurePhysiologyRegistry env
    pg ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, pg)], wmVisible = [fixturePage] }
    writeIORef (gameTimeRef env) 0
    built ← forM specs $ \sp → do
        def ← specDef sp
        inst ← buildUnit def sp
        pure (sp, def, inst)
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList [ (udName d, d) | (_, d, _) ← built ]
        , umInstances = HM.fromList
            [ (UnitId (fromIntegral (usId sp)), i) | (sp, _, i) ← built ] }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- | An engine with the real Lua API, the shipped physiology modules, a
--   fixed mild climate and a few shared runners, loaded with @specs@.
withScene ∷ [UnitSpec] → (Scene → IO a) → IO a
withScene specs act = withHeadlessEngineNoWorld $ \env → do
    loadScene env specs
    ls ← newBareLuaBackend env
    ok ← luaText ls $ T.unlines
        [ "local ai = require('scripts.unit_ai')"
        , "local resources = require('scripts.unit_resources')"
        , "resources.init(0)"
        , "_G.__resources = resources"
        , "_G.__physiology = require('scripts.physiology')"
        , "_G.__env = { temp = 22, humidity = 0.5 }"
        , "world.getClimateAt = function() return { temp = __env.temp,"
        , "  summerTemp = __env.temp, winterTemp = __env.temp,"
        , "  precip = 0.2, humidity = __env.humidity, snow = 0 } end"
        , "world.getAmbientAt = function() return __env.temp end"
        , "_G.__errors = {}"
        , "local logError = engine.logError"
        , "engine.logError = function(m)"
        , "  __errors[#__errors + 1] = tostring(m)"
        , "  if logError then logError(m) end"
        , "end"
        , "function _G.__f(v)"
        , "  if v == nil then return 'nil' end"
        , "  if v ~= v then return 'nan' end"
        , "  return string.format('%.6g', v)"
        , "end"
        , "function _G.__read(uid, key)"
        , "  local v, m = unit.readPhysio(uid, key)"
        , "  return __f(v) .. '|' .. tostring(m)"
        , "end"
        , "_G.__events = {}"
        , "local emit = engine.emitEventForUnit"
        , "engine.emitEventForUnit = function(kind, msg, ...)"
        , "  __events[#__events + 1] = tostring(kind) .. ':' .. tostring(msg)"
        , "  if emit then return emit(kind, msg, ...) end"
        , "end"
        , "function _G.__track(uid, seconds, dt, keys, hook)"
        , "  local out = {}"
        , "  for _, k in ipairs(keys) do out[k] = { min = math.huge, max = -math.huge, nan = 0 } end"
        , "  for step = 1, math.floor(seconds / dt + 0.5) do"
        , "    resources.update(dt)"
        , "    if hook then hook(step, step * dt) end"
        , "    for _, k in ipairs(keys) do"
        , "      local v"
        , "      if k == 'cons' then v = unit.getStat(uid, 'consciousness')"
        , "      else v = unit.readPhysio(uid, k) end"
        , "      local o = out[k]"
        , "      if v ~= v then o.nan = o.nan + 1"
        , "      elseif v ~= nil then o.min = math.min(o.min, v); o.max = math.max(o.max, v) end"
        , "    end"
        , "  end"
        , "  local parts = {}"
        , "  for _, k in ipairs(keys) do"
        , "    local o = out[k]"
        , "    parts[#parts + 1] = string.format('%s=%.9g,%.9g,%d', k, o.min, o.max, o.nan)"
        , "  end"
        , "  return table.concat(parts, ';')"
        , "end"
        , "function _G.__run(seconds, dt)"
        , "  for _ = 1, math.floor(seconds / dt + 0.5) do resources.update(dt) end"
        , "end"
        , "return true" ]
    ok `shouldBe` "true"
    act (env, ls)

-- | Evaluate a Lua chunk and return its result without JSON quotes.
luaText ∷ LuaBackendState → Text → IO Text
luaText ls src = T.strip ∘ T.dropAround (≡ '"') <$> evalDebug ls src

-- | A Lua expression's number, or the spec fails.
luaNum ∷ LuaBackendState → Text → IO Double
luaNum ls expr = do
    out ← luaText ls ("return (" <> expr <> ")")
    case reads (T.unpack out) of
        [(v, "")] → pure v
        _ → do
            expectationFailure ("expected a number from `" <> T.unpack expr
                                <> "`, got " <> T.unpack out)
            pure 0

luaBool ∷ LuaBackendState → Text → IO Bool
luaBool ls expr = (≡ "true") <$> luaText ls ("return " <> expr)

-- | The errors the shipped scripts logged through @engine.logError@ since
--   the scene booted, joined (empty when none): a faulting tick is a
--   per-unit pcall that logs, so a spec that wants "no errors" asks here.
scriptErrors ∷ LuaBackendState → IO Text
scriptErrors ls = luaText ls "return table.concat(__errors, ' || ')"

-- | @value|mode@ of an owned key through the read verb (value to six
--   significant figures, @nil@ when undefined or Unsupported).
readOwned ∷ LuaBackendState → Int → Text → IO Text
readOwned ls uid key =
    luaText ls ("return __read(" <> tshow uid <> ", '" <> key <> "')")

-- | Set the fixed climate.
climate ∷ LuaBackendState → Double → Double → IO ()
climate ls temp humidity = void $ luaText ls
    ("__env.temp = " <> tshow temp <> "; __env.humidity = " <> tshow humidity
     <> "; return true")

-- | Tick the real physiology for @seconds@ at @dt@.
runSeconds ∷ LuaBackendState → Double → Double → IO ()
runSeconds ls seconds dt = void $ luaText ls
    ("__run(" <> tshow seconds <> ", " <> tshow dt <> "); return true")

-- | The range one read took over a tracked run, and how often it was NaN.
data Extremes = Extremes
    { exMin ∷ !Double, exMax ∷ !Double, exNaN ∷ !Int } deriving Show

-- | Tick for @seconds@ at @dt@ and report, per read key, its minimum,
--   maximum and NaN count over the run. A key is read through
--   @unit.readPhysio@ (so an owned key answers its mode-aware value);
--   @cons@ is the stored consciousness. A key that was never defined
--   reports @(inf, -inf)@.
track ∷ LuaBackendState → Int → Double → Double → [Text]
      → IO (HM.HashMap Text Extremes)
track ls uid seconds dt keys = trackWith ls uid seconds dt keys "nil"

-- | 'track' with a Lua per-tick hook (@function(step, t) ... end@).
trackWith ∷ LuaBackendState → Int → Double → Double → [Text] → Text
          → IO (HM.HashMap Text Extremes)
trackWith ls uid seconds dt keys hook = do
    let keyList = "{" <> T.intercalate "," [ "'" <> k <> "'" | k ← keys ] <> "}"
    out ← luaText ls ("return __track(" <> tshow uid <> ", " <> tshow seconds
                      <> ", " <> tshow dt <> ", " <> keyList <> ", " <> hook <> ")")
    pure $ HM.fromList
        [ (k, Extremes (num lo) (num hi) (round (num n)))
        | part ← T.splitOn ";" out
        , let (k, rest) = T.breakOn "=" part
        , [lo, hi, n] ← [T.splitOn "," (T.drop 1 rest)] ]
  where
    num ∷ Text → Double
    num t = case reads (T.unpack (fixInf t)) of
        [(v, "")] → v
        _ → 0
    fixInf t | t `elem` ["inf", "+inf"] = "Infinity"
             | t ≡ "-inf"               = "-Infinity"
             | otherwise                = t

-- | The survival warnings and critical events the shipped scripts emitted
--   since the scene booted (@kind:message@, in order).
survivalEvents ∷ LuaBackendState → IO [Text]
survivalEvents ls = do
    out ← luaText ls
        "local o = {}; for _, e in ipairs(__events) do \
        \if e:find('^survival_') then o[#o + 1] = e end end; \
        \return table.concat(o, '|')"
    pure (filter (not ∘ T.null) (T.splitOn "|" out))

-- | The unit as the engine holds it right now.
readInstance ∷ EngineEnv → Int → IO UnitInstance
readInstance env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup (UnitId (fromIntegral uid)) (umInstances um) of
        Just i  → pure i
        Nothing → do
            expectationFailure ("no unit " <> show uid)
            error "unreachable"

-- | Edit a unit in place, as a restored save or a test fixture would.
modifyInstance ∷ EngineEnv → Int → (UnitInstance → UnitInstance) → IO ()
modifyInstance env uid f = modifyIORef' (unitManagerRef env) $ \um →
    um { umInstances = HM.adjust f (UnitId (fromIntegral uid)) (umInstances um) }

-- | The units a tick asked to kill (@unit.kill@ only enqueues here).
killed ∷ EngineEnv → IO [UnitId]
killed env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [u | UnitKill u ← cmds]

-- | Every command the units' queue holds (and empty it).
drainCommands ∷ EngineEnv → IO [UnitCommand]
drainCommands env = Q.flushQueue (unitQueue env)

-- | Set the game clock (modifier expiry reads it, nothing ticks).
writeGameTime ∷ EngineEnv → Double → IO ()
writeGameTime env = writeIORef (gameTimeRef env)

-- | The units a tick asked to collapse.
collapses ∷ EngineEnv → IO [UnitId]
collapses env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [u | UnitCollapse u ← cmds]

near ∷ Double → Double → Double → Expectation
near tol want got = abs (got - want) `shouldSatisfy` (< tol)

-- | Every key a unit holds that a Nominal or Unsupported system must not:
--   the owned keys, the derived key and the meters of the whole registry.
nominalKeys ∷ [Text]
nominalKeys =
    [ "core_temp", "salt", "salt_store", "salt_conc"
    , "hydration", "hunger", "calories"
    , "hypothermia", "hyperthermia", "salt_imbalance" ]
