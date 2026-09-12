-- | Source-drinking hydration eligibility (#2541).
--
--   Acolyte hydration declared its fast recovery factor for ONE
--   situation — on all fours at the bank of a lake or river, actually
--   drinking — but @scripts\/unit_resource_tick.lua@ applied it to any
--   unit whose POSE was Crawling. Crawling is also ordinary injured
--   locomotion (@scripts\/unit_resource_injury.lua@ puts a conscious
--   unit with a disabling leg injury into it) and the sleep goal routes
--   its descent and wake ascent through Crawling as a deliberate
--   waypoint (#612), so a broken leg supplied unlimited free water with
--   no source, no drinking action and no world query at all.
--
--   These specs drive the SHIPPED @scripts\/unit_resource_tick.lua@,
--   @unit_resource_injury.lua@, @unit_resource_config.lua@,
--   @unit_stats.lua@ and @scripts\/unit_ai.lua@ through the REAL
--   registered @unit.*@ API over a REAL 'unitManagerRef'. Only
--   @world.getFluidAt@ is controlled — 'withHeadlessEngineNoWorld' has
--   no tile data for the real one to read — and the stub COUNTS its
--   calls, which is how §4 proves the gate re-reads live world state on
--   every hydration tick instead of latching a flag at drink admission.
--
--   Poses are written straight into the fixture 'UnitInstance'. @unit.
--   crawl@ \/ @unit.revive@ \/ @unit.transitionTo@ only ENQUEUE a command
--   on @unitQueue@, and this harness starts no worker to drain it, so a
--   pose the injury tick "requests" would never reach the manager and
--   @unit.getPose@ would keep reporting the fixture value. §1 asserts
--   what the injury tick requested by draining that queue directly.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "source drinking hydration eligibility"'@.
module Test.Headless.Unit.SourceDrinkingHydration (spec) where

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
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- * Fixture

acolyteUid ∷ UnitId
acolyteUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "source_drinking_page"

-- | The acolyte stands here; @(11, 10)@ is its eastern neighbour, which
--   every eligible case fills with water.
unitTileX, unitTileY ∷ Float
unitTileX = 10
unitTileY = 10

-- | The one physiology tick the shipped @init_loader.lua@ schedules.
tickDt ∷ Double
tickDt = 0.1

-- | @drain_constant_frac × max_hydration × activityMultiplier@ from
--   @scripts\/unit_resource_config.lua@, restated independently so a
--   silent change to the configured drain shows up here. The fixture is
--   idle with no animation, so the multiplier is exactly 1.
drainPerSec ∷ Float
drainPerSec = (0.01 / 42) * maxHydration * 1.0

-- | @regen_factor_source_drinking × endurance@.
regenPerSec ∷ Float
regenPerSec = 5.0 * 1.0

maxHydration ∷ Float
maxHydration = 42

startHydration ∷ Float
startHydration = 30

-- | Float stored values accumulate a rounding step per tick, so every
--   assertion compares within a tolerance two orders of magnitude
--   tighter than the smallest signal it has to distinguish (the 0.04 L
--   that 40 ticks of ordinary drain move).
tol ∷ Float
tol = 1e-3

-- | The stats the hydration tick and the injury tick actually read.
--   @max_hydration@ is an attribute (the engine recomputes it into
--   @uiStats@), @endurance@ scales regen, and @consciousness@ pins the
--   collapse\/crawl machine so no case depends on a defaulted brain
--   reading.
baseStats ∷ HM.HashMap Text Float
baseStats = HM.fromList
    [ ("hydration", startHydration), ("max_hydration", maxHydration)
    , ("endurance", 1.0), ("consciousness", 1.0) ]

mkAcolyte ∷ Text → HM.HashMap Text Float → [Wound] → UnitInstance
mkAcolyte pose stats ws = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = unitTileX, uiGridY = unitTileY
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = pose, uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = ws
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A shattered leg: @woundEffSeverity@ 0.9 clears
--   @injuries.cannotWalk@'s 0.85 single-limb bar, which is the
--   production reason such a unit is left Crawling.
shatteredLeg ∷ Wound
shatteredLeg = Wound
    { woundPart = "l_leg", woundKind = "fracture", woundSeverity = 0.9
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | Install a one-unit scene and drain any command the previous example
--   left on the queue, so nothing leaks across examples.
resetScene ∷ EngineEnv → Text → HM.HashMap Text Float → [Wound] → IO ()
resetScene env pose stats ws = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.singleton acolyteUid (mkAcolyte pose stats ws) }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- * Live readers

storedHydration ∷ EngineEnv → IO (Maybe Float)
storedHydration env = do
    um ← readIORef (unitManagerRef env)
    pure (HM.lookup acolyteUid (umInstances um) ⌦ HM.lookup "hydration" ∘ uiStats)

storedStamina ∷ EngineEnv → IO (Maybe Float)
storedStamina env = do
    um ← readIORef (unitManagerRef env)
    pure (HM.lookup acolyteUid (umInstances um) ⌦ HM.lookup "stamina" ∘ uiStats)

shouldBeNear ∷ IO (Maybe Float) → Float → Expectation
shouldBeNear act expected = act ⌦ \case
    Nothing → expectationFailure $
        "expected a stored hydration near " <> show expected
        <> ", found no entry"
    Just actual
        | abs (actual - expected) < tol → pure ()
        | otherwise → expectationFailure $
            "expected a stored value near " <> show expected
            <> ", found " <> show actual

-- * Lua plumbing

-- | The shipped modules, loaded for real, plus the counted fluid stub.
--
--   @scripts.unit_ai@ is required by name rather than stubbed for two
--   reasons: the gate reaches per-unit AI state through its production
--   @getState@, and @scripts.unit_ai_core@ reads the
--   @package.loaded[\"scripts.unit_ai\"]@ singleton at load time, so it
--   cannot be loaded on its own at all.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    loaded ← evalDebug ls $ T.concat
        [ "_G.__ai = require('scripts.unit_ai'); "
        , "_G.__core = require('scripts.unit_ai_core'); "
        , "_G.__tick = require('scripts.unit_resource_tick'); "
        , "_G.__inj = require('scripts.unit_resource_injury'); "
        , "_G.__injuries = require('scripts.injuries'); "
        , "_G.__cfg = require('scripts.unit_resource_config').acolyte; "
        , "return type(_G.__ai.getState) == 'function' "
        , "  and _G.__cfg.hydration.regen_factor_source_drinking == 5.0 "
        , "  and _G.__cfg.hydration.regen_factor_crawling == nil" ]
    loaded `shouldBe` "true"
    stubbed ← evalDebug ls $ T.concat
        [ "_G.__fluid = {}; _G.__reads = 0; "
        , "world.getFluidAt = function(gx, gy) "
        , "  _G.__reads = _G.__reads + 1; "
        , "  local k = _G.__fluid[tostring(gx) .. ',' .. tostring(gy)]; "
        , "  if k == nil then return nil end; "
        , "  return k, 0; "
        , "end; return true" ]
    stubbed `shouldBe` "true"
    pure ls

-- | Fill the named tiles with the given fluid kind; everything else is
--   dry. Passing @[]@ is dry ground.
setFluid ∷ LuaBackendState → [((Int, Int), Text)] → IO ()
setFluid ls entries = do
    r ← evalDebug ls $ T.concat $
        ["_G.__fluid = {"]
        <> [ T.concat [ "['", tshow x, ",", tshow y, "'] = '", kind, "'," ]
           | ((x, y), kind) ← entries ]
        <> ["}; return true"]
    r `shouldBe` "true"

-- | Per-unit AI state, created through the production
--   @unit_ai_core.ensureState@ and then given the phase flags the case
--   needs. @nil@ for a field leaves it unset.
setAiState ∷ LuaBackendState → [(Text, Text)] → IO ()
setAiState ls fields = do
    r ← evalDebug ls $ T.concat $
        ["local s = _G.__core.ensureState(1); "]
        <> [ T.concat ["s.", k, " = ", v, "; "] | (k, v) ← fields ]
        <> ["return _G.__ai.getState(1) == s"]
    r `shouldBe` "true"

-- | @n@ hydration ticks of the SHIPPED tickResource, with the real
--   config's hydration params.
tickHydration ∷ LuaBackendState → Text → Text → Int → IO ()
tickHydration ls activity pose n = do
    r ← evalDebug ls $ T.concat
        [ "for _ = 1, ", tshow n, " do "
        , "_G.__tick.tickResource(1, 'acolyte', 'hydration', "
        , "_G.__cfg.hydration, '", activity, "', '", pose, "', "
        , tshow tickDt, ") end; return true" ]
    r `shouldBe` "true"

-- | The gate itself, asserted directly rather than only through the
--   hydration it does or does not produce.
eligible ∷ LuaBackendState → IO Text
eligible ls = evalDebug ls "return _G.__tick.sourceDrinkingEligible(1) == true"

fluidReads ∷ LuaBackendState → IO Text
fluidReads ls = evalDebug ls "return tostring(_G.__reads)"

-- | Drive the shipped injury tick with the fixture's own live info
--   table, exactly as @unit_resources.update@ does.
tickInjuries ∷ LuaBackendState → Text → IO Text
tickInjuries ls pose = evalDebug ls $ T.concat
    [ "return _G.__inj.tickInjuries(1, unit.getInfo(1), '", pose, "')" ]

-- | What the injury tick asked the engine to do with this unit's pose.
poseCommands ∷ EngineEnv → IO [Text]
poseCommands env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [ label | Just label ← map classify cmds ]
  where
    classify = \case
        UnitCrawl _    → Just "crawl"
        UnitRevive _   → Just "revive"
        UnitCollapse _ → Just "collapse"
        _              → Nothing

-- | Hydration after @n@ ordinary drain ticks from 'startHydration'.
drained ∷ Int → Float
drained n = startHydration - drainPerSec * realToFrac tickDt * fromIntegral n

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "source drinking hydration eligibility" $ do

    -- §1 The defect. A conscious acolyte the injury tick holds in
    -- Crawling, on dry ground, must DRAIN at the ordinary configured
    -- rate — not merely fail to gain, which an implementation that
    -- zeroed hydration movement for crawlers would also satisfy.
    describe "an injured crawler on dry ground (§1)" $ do
        it "gains nothing and drains at the configured rate over 40 \
           \ticks: 30.000 → 29.960, not 42.000" $ \env → do
            resetScene env "crawling" baseStats [shatteredLeg]
            ls ← setupLua env
            setFluid ls []

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

        it "is crawling for the production reason — cannotWalk — and the \
           \injury tick leaves it there rather than standing it up" $
          \env → do
            resetScene env "crawling" baseStats [shatteredLeg]
            ls ← setupLua env
            setFluid ls []

            evalDebug ls "return _G.__injuries.cannotWalk(1)"
                `shouldReturn` "true"
            tickInjuries ls "crawling" `shouldReturn` "false"
            poseCommands env `shouldReturn` []

        it "an uninjured crawler on dry ground drains identically, so \
           \the gate keys on drinking and not on the injury" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls []

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

    -- §2 The sleep waypoint (#612). Both legs of the chain pass through
    -- Crawling with no water anywhere.
    describe "crawling inside the sleep chain (§2)" $ do
        it "the descent leg supplies no hydration" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls []
            setAiState ls [("sleepPhase", "'descending'")]

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

        it "the wake ascent leg supplies no hydration" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls []
            setAiState ls [("sleepPhase", "'ascending'")]

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

        it "a sleeper beside real water still supplies none — the sleep \
           \phase is not a drinking phase" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sleepPhase", "'descending'")]

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

    -- §3 Controls: the poses that never regened hydration still drain
    -- exactly as before.
    describe "standing and sleeping controls (§3)" $ do
        it "a standing unit retains ordinary drain" $ \env → do
            resetScene env "standing" baseStats []
            ls ← setupLua env
            setFluid ls []
            tickHydration ls "idle" "standing" 40
            storedHydration env `shouldBeNear` drained 40

        it "a sleeping unit retains ordinary drain" $ \env → do
            resetScene env "sleeping" baseStats []
            ls ← setupLua env
            setFluid ls []
            setAiState ls [("sleepPhase", "'sleeping'")]
            tickHydration ls "idle" "sleeping" 40
            storedHydration env `shouldBeNear` drained 40

        it "a standing unit beside real water, with no drinking phase, \
           \still only drains" $ \env → do
            resetScene env "standing" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            tickHydration ls "idle" "standing" 40
            storedHydration env `shouldBeNear` drained 40

    -- §4 The positive case, and the live re-read that requirement 3
    -- turns on.
    describe "eligible source drinking (§4)" $ do
        it "gains at the configured rate: one tick of \
           \(5.0 × endurance − drain) × dt" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            eligible ls `shouldReturn` "true"
            tickHydration ls "idle" "crawling" 1
            storedHydration env `shouldBeNear`
                (startHydration + (regenPerSec - drainPerSec)
                                  * realToFrac tickDt)

        it "a river bank is eligible on the same terms as a lake" $
          \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "river")]
            setAiState ls [("sourcePhase", "'drinking'")]

            eligible ls `shouldReturn` "true"
            tickHydration ls "idle" "crawling" 1
            storedHydration env `shouldBeNear`
                (startHydration + (regenPerSec - drainPerSec)
                                  * realToFrac tickDt)

        it "stays bounded by max_hydration and never overshoots it" $
          \env → do
            resetScene env "crawling"
                (HM.insert "hydration" 41.9 baseStats) []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            tickHydration ls "idle" "crawling" 1
            storedHydration env `shouldReturn` Just maxHydration
            tickHydration ls "idle" "crawling" 10
            storedHydration env `shouldReturn` Just maxHydration

        it "stops gaining the moment source access is lost, and resumes \
           \ordinary drain" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            tickHydration ls "idle" "crawling" 1
            let afterDrink = startHydration
                           + (regenPerSec - drainPerSec) * realToFrac tickDt
            storedHydration env `shouldBeNear` afterDrink

            -- The source dries up. The AI phase flag is UNCHANGED — it
            -- is re-verified only at the next AI decision, roughly ten
            -- hydration ticks away — so only the live re-read can stop
            -- the payout here.
            setFluid ls []
            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 10
            storedHydration env `shouldBeNear`
                (afterDrink - drainPerSec * realToFrac tickDt * 10)

        it "re-reads the world on EVERY tick rather than latching the \
           \admission" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            _ ← evalDebug ls "_G.__reads = 0; return true"
            tickHydration ls "idle" "crawling" 5
            -- Each eligible tick reads the actor's own tile plus at
            -- least one neighbour, so five ticks cannot be served by
            -- fewer than ten reads.
            reads' ← fluidReads ls
            (read (T.unpack (T.filter (/= '"') reads')) ∷ Int)
                `shouldSatisfy` (≥ 10)

    -- §5 Remembered water is a lead, not a source.
    describe "remembered water without access (§5)" $
        it "a populated knownWaterSources with the drinking phase set \
           \authorizes nothing while every queried tile is dry" $
          \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls []
            setAiState ls
                [ ("knownWaterSources", "{ { x = 11, y = 10 } }")
                , ("sourcePhase", "'drinking'") ]

            -- The memory really is populated, by the production reader.
            evalDebug ls
                "return _G.__core.hasKnownWaterSource(_G.__ai.getState(1))"
                `shouldReturn` "true"
            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

    -- §6 Everything else the gate has to refuse.
    describe "refusals (§6)" $ do
        it "only the drinking phase pays — descending, ascending and no \
           \phase are all refused beside real water" $ \env → do
            forM_ ["'descending'", "'ascending'", "nil"] $ \phase → do
                resetScene env "crawling" baseStats []
                ls ← setupLua env
                setFluid ls [((11, 10), "lake")]
                setAiState ls [("sourcePhase", phase)]

                eligible ls `shouldReturn` "false"
                tickHydration ls "idle" "crawling" 40
                storedHydration env `shouldBeNear` drained 40

        it "a unit standing IN the water is refused — drink_from_source \
           \only descends from a dry bank tile" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((10, 10), "lake"), ((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

        it "water two tiles away is refused — the gate mirrors the \
           \sequence's own adjacency" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((12, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

        it "an adjacent ocean or lava tile is refused — neither is a \
           \drinkable source" $ \env → do
            forM_ ["ocean", "lava"] $ \kind → do
                resetScene env "crawling" baseStats []
                ls ← setupLua env
                setFluid ls [((11, 10), kind)]
                setAiState ls [("sourcePhase", "'drinking'")]

                eligible ls `shouldReturn` "false"
                tickHydration ls "idle" "crawling" 40
                storedHydration env `shouldBeNear` drained 40

        it "each of the eight adjacent tiles IS eligible, so the \
           \refusals above are about the geometry and not a dead check" $
          \env → do
            forM_ [ (dx, dy) | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                  , (dx, dy) ≢ (0 ∷ Int, 0 ∷ Int) ] $ \(dx, dy) → do
                resetScene env "crawling" baseStats []
                ls ← setupLua env
                setFluid ls [((10 + dx, 10 + dy), "lake")]
                setAiState ls [("sourcePhase", "'drinking'")]
                eligible ls `shouldReturn` "true"

        it "a unit whose projection cannot establish a page is refused \
           \— unknown is never a match" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]
            eligible ls `shouldReturn` "true"

            -- Same live position, same water, page erased.
            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "acolyte"
                             (minimalDef "acolyte" "Acolyte")
                , umInstances = HM.singleton acolyteUid
                    ((mkAcolyte "crawling" baseStats [])
                        { uiPage = WorldPageId "" }) }
            eligible ls `shouldReturn` "false"
            tickHydration ls "idle" "crawling" 40
            storedHydration env `shouldBeNear` drained 40

        it "a unit whose projection has gone is refused, and the tick \
           \writes nothing" $ \env → do
            resetScene env "crawling" baseStats []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]
            eligible ls `shouldReturn` "true"

            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "acolyte"
                             (minimalDef "acolyte" "Acolyte") }
            eligible ls `shouldReturn` "false"
            storedHydration env `shouldReturn` Nothing

    -- §7 Blast radius: other resources keep their own recovery, and
    -- never reach this gate at all.
    describe "other resources (§7)" $ do
        it "stamina still recovers by its own crouching factor" $
          \env → do
            resetScene env "crouching"
                (HM.insert "stamina" 5.0 baseStats) []
            ls ← setupLua env
            setFluid ls []
            r ← evalDebug ls $ T.concat
                [ "_G.__tick.tickResource(1, 'acolyte', 'stamina', "
                , "_G.__cfg.stamina, 'idle', 'crouching', "
                , tshow tickDt, "); return true" ]
            r `shouldBe` "true"
            -- regen_factor_crouching 0.5 × endurance 1 × dt.
            storedStamina env `shouldBeNear` (5.0 + 0.5 * realToFrac tickDt)

        it "a resource declaring no source-drinking factor never \
           \consults the AI or the world" $ \env → do
            resetScene env "crawling"
                (HM.insert "stamina" 5.0 baseStats) []
            ls ← setupLua env
            setFluid ls [((11, 10), "lake")]
            setAiState ls [("sourcePhase", "'drinking'")]

            _ ← evalDebug ls "_G.__reads = 0; return true"
            r ← evalDebug ls $ T.concat
                [ "_G.__tick.tickResource(1, 'acolyte', 'stamina', "
                , "_G.__cfg.stamina, 'idle', 'crawling', "
                , tshow tickDt, "); return true" ]
            r `shouldBe` "true"
            fluidReads ls `shouldReturn` "\"0\""
