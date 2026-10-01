-- | Mental-state wandering on a config that never mentions wandering
--   (#2753).
--
--   @scripts\/unit_ai_mental.lua@ sends a delirious unit, and a mental
--   break's wander, flee-with-no-one-near and lash-out-with-no-target,
--   through @needs.wanderExecute@ on the unit's OWN AI config.
--   @nomad_primitive@'s (@scripts\/unit_ai_encounter.lua@) carries no
--   @wander_radius@, so the wander sampler raised on a nil — inside
--   @unitAi.update@, whose per-unit loop has no error isolation, so the
--   rest of that tick's units never ran.
--
--   Everything runs through the PRODUCTION modules: the real
--   @scripts\/unit_ai.lua@ (so the nomad config is the one
--   @unit_ai_encounter.register@ installs), the real @shortCircuit@ and
--   wander executor, and the real registered @unit.*@ verbs over real
--   manager refs. Delirium and a break are entered through the stats
--   @brain.lua@ and @mental_state.lua@ read, not by stubbing either. The
--   wander leg is observed as the 'UnitMoveTo' it enqueues.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "mental wander config fallback"'@.
module Test.Headless.Unit.MentalWanderFallback (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Pathing.Hazard (MoveHazardPolicy(..))
import Unit.Sim.Types (Direction(..), emptyUnitThreadState)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), emptyWorldManager, emptyWorldState)

-- * Fixture

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "mental_wander_fallback_page"

nomadDef ∷ Text
nomadDef = "nomad_primitive"

-- | Where every occupant stands. Far enough apart that a flee never
--   finds anyone inside @FLEE_RADIUS@ (15) and a lash-out never finds
--   anyone inside @LASHOUT_RANGE@ (8).
homeOf ∷ UnitId → (Float, Float)
homeOf (UnitId n) = (10.5 + 100 * fromIntegral n, 10.5)

-- | @unit_ai_config_defaults.lua@'s fallback, restated so a silent
--   change to it shows up here.
fallbackRadius ∷ Float
fallbackRadius = 5.0

-- | Consciousness inside @brain.lua@'s delirium band [0.15, 0.40).
deliriousConsciousness ∷ Float
deliriousConsciousness = 0.25

-- | @mental_state.lua@'s state and break-behaviour codes.
breakState, behaviorWander, behaviorFlee, behaviorLashout ∷ Float
breakState      = 2
behaviorWander  = 0
behaviorFlee    = 1
behaviorLashout = 3

mkNomad ∷ UnitId → HM.HashMap Text Float → UnitInstance
mkNomad uid stats = UnitInstance
    { uiDefName = nomadDef, uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = fst (homeOf uid), uiGridY = snd (homeOf uid)
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.union stats (HM.singleton "consciousness" 1.0)
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFaction = resolveLegacyFaction [] FactionWildlife, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Install one active page carrying the given nomads, at game time
--   zero, with every queue drained.
resetScene ∷ EngineEnv → [(UnitId, HM.HashMap Text Float)] → IO ()
resetScene env units = do
    writeIORef (gameTimeRef env) 0
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton nomadDef (minimalDef nomadDef "Nomad")
        , umInstances = HM.fromList
            [ (uid, mkNomad uid stats) | (uid, stats) ← units ] }
    writeIORef (utsRef env) emptyUnitThreadState
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- | Overwrite one stat on a live instance.
setStat ∷ EngineEnv → UnitId → Text → Float → IO ()
setStat env uid name v = do
    um ← readIORef (unitManagerRef env)
    let bump inst = inst { uiStats = HM.insert name v (uiStats inst) }
    writeIORef (unitManagerRef env) um
        { umInstances = HM.adjust bump uid (umInstances um) }

-- | The shipped dispatcher and the module the four branches live in.
--   @scripts.unit_ai@ first: its submodules read its singleton at load.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    loaded ← evalDebug ls $ T.concat
        [ "_G.__ai = require('scripts.unit_ai'); "
        , "_G.__core = require('scripts.unit_ai_core'); "
        , "_G.__mental = require('scripts.unit_ai_mental'); "
        , "_G.__cfg = require('scripts.unit_ai_tunables'); "
        , "return _G.__cfg.nomad_primitive ~= nil" ]
    loaded `shouldBe` "true"
    pure ls

-- | Every wander leg the drained queue carries for @uid@.
movesOf ∷ UnitId → [UnitCommand] → [(Float, Float, MoveHazardPolicy)]
movesOf uid cmds =
    [ (tx, ty, hz) | UnitMoveTo u tx ty _ hz ← cmds, u ≡ uid ]

-- | One finite leg, fall-permitted (mental-state wandering keeps
--   @unit.moveTo@'s default policy), inside the fallback radius of home.
expectWanderLeg ∷ UnitId → [UnitCommand] → Expectation
expectWanderLeg uid cmds = case movesOf uid cmds of
    [(tx, ty, hz)] → do
        let (hx, hy) = homeOf uid
            d = sqrt ((tx - hx) * (tx - hx) + (ty - hy) * (ty - hy))
        (isNaN tx ∨ isInfinite tx ∨ isNaN ty ∨ isInfinite ty)
            `shouldBe` False
        hz `shouldBe` FallPermitted
        d `shouldSatisfy` (≤ fallbackRadius + 1.0e-4)
    other → expectationFailure $
        "expected exactly one wander leg, got " <> show other

-- | One direct short-circuit call on the nomad's real config, from idle.
shortCircuitOnce ∷ EngineEnv → LuaBackendState → UnitId → IO [UnitCommand]
shortCircuitOnce env ls (UnitId n) = do
    r ← evalDebug ls $ T.concat
        [ "local uid = ", tshow n, "; "
        , "local s = _G.__core.ensureState(uid); "
        , "return _G.__mental.shortCircuit(uid, s, _G.__cfg.nomad_primitive, "
        , "  'idle', {})" ]
    r `shouldBe` "true"
    Q.flushQueue (unitQueue env)

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "mental wander config fallback (#2753)" $ do

    describe "the config fallback" $ do
        it "fills a missing wander_radius and never overrides a def's own" $
          \env → do
            resetScene env []
            ls ← setupLua env
            evalDebug ls (T.concat
                [ "local c = _G.__cfg; "
                , "local fb = require('scripts.unit_ai_config_defaults')"
                , "  .FALLBACK.wander_radius; "
                , "_G.__ai.setConfig('fixture_def', { wander_radius = 2.5 }); "
                , "return rawget(c.nomad_primitive, 'wander_radius') == nil"
                , " and c.nomad_primitive.wander_radius == fb"
                , " and fb == ", tshow fallbackRadius
                , " and c.fixture_def.wander_radius == 2.5"
                , " and c.acolyte.wander_radius == 5.0"
                , " and c.technomule.wander_radius == 3.0"
                , " and c.bear_brown.wander_radius == 8.0"
                , " and c.red_squirrel.wander_radius == 6.0" ])
                `shouldReturn` "true"

    describe "every mental-state wander branch on a sparse config" $ do
        it "delirium" $ \env → do
            let u = UnitId 1
            resetScene env [(u, HM.empty)]
            ls ← setupLua env
            setStat env u "consciousness" deliriousConsciousness
            shortCircuitOnce env ls u ⌦ expectWanderLeg u

        it "a break's wander" $ \env → do
            let u = UnitId 1
            resetScene env [(u, HM.fromList
                [ ("mental_state", breakState)
                , ("mental_break_behavior", behaviorWander) ])]
            ls ← setupLua env
            shortCircuitOnce env ls u ⌦ expectWanderLeg u

        it "a break's flee with no other unit near" $ \env → do
            let u = UnitId 1
            resetScene env [(u, HM.fromList
                [ ("mental_state", breakState)
                , ("mental_break_behavior", behaviorFlee) ])]
            ls ← setupLua env
            shortCircuitOnce env ls u ⌦ expectWanderLeg u

        it "a break's lash-out with no eligible target" $ \env → do
            let u = UnitId 1
            resetScene env [(u, HM.fromList
                [ ("mental_state", breakState)
                , ("mental_break_behavior", behaviorLashout) ])]
            ls ← setupLua env
            shortCircuitOnce env ls u ⌦ expectWanderLeg u

    describe "the shared unit_ai tick" $
        it "issues the delirious nomad's leg and still reaches the next unit" $
          \env → do
            resetScene env [(UnitId 1, HM.empty), (UnitId 2, HM.empty)]
            ls ← setupLua env
            -- Make whichever unit the dispatcher visits FIRST the
            -- delirious one, so a raise would starve the other.
            firstId ← evalDebug ls "return unit.getAllIds()[1]"
            let (first, second) = if firstId ≡ "1"
                    then (UnitId 1, UnitId 2) else (UnitId 2, UnitId 1)
                UnitId secondN = second
            setStat env first "consciousness" deliriousConsciousness
            evalDebug ls "_G.__ai.update(1.0); return true"
                `shouldReturn` "true"
            -- The next unit got as far as scoring: only that stamps a
            -- future decision slot over the fresh row's 0.
            evalDebug ls (T.concat
                [ "local s = _G.__ai.getState(", tshow secondN, "); "
                , "return s ~= nil and s.nextActionAt > 0" ])
                `shouldReturn` "true"
            Q.flushQueue (unitQueue env) ⌦ expectWanderLeg first
