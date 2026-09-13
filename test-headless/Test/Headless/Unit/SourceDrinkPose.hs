-- | Source-drinking pose and phase lifecycle (#2545).
--
--   @drink_from_source@ (@scripts\/unit_ai_water.lua@) holds its action
--   at @math.huge@ for as long as @s.sourcePhase@ is set, and dwells in
--   Crawling from the bottom of its descent to the top of its ascent.
--   @scripts\/unit_resource_injury.lua@ stands any healthy crawling unit
--   back up, exempting only the sleep chain (#612) — so the descent was
--   re-issued once per thought tick against a revive that undid it at
--   10 Hz, the drinking phase was never reached, and nothing could
--   outscore the lock. Two more entries reach the same nonprogressing
--   state: @unit_ai_mental.preempt@ swapped the action out on delirium
--   and fired only the outgoing action's @onExit@, of which
--   @drink_from_source@ had none; and any interruption that takes the
--   SOURCE away leaves the drinking phase waiting on a 95 % threshold
--   that @unit_resource_tick.sourceDrinkingEligible@ (#2541) will never
--   let it reach.
--
--   Everything here is driven through the PRODUCTION path: the shipped
--   @scripts\/unit_ai.lua@ dispatcher (so arbitration, preemption and
--   @onExit@ are the real ones), the shipped physiology tick, and the
--   REAL registered @unit.*@ \/ @world.*@ verbs over real manager refs.
--   @world.getFluidAt@ is a real query against a real one-chunk page
--   carrying one lake tile, not a stub.
--
--   The harness closes the loop the pose verbs leave open. @unit.crawl@
--   \/ @unit.revive@ \/ @unit.transitionTo@ only ENQUEUE a 'UnitCommand'
--   on @unitQueue@; the handlers in "Unit.Thread.Command.Pose" mutate
--   @utsSimStates@; and @unit.getPose@ reads @uiPose@, which only
--   'Unit.Thread.publishToRender' republishes. With no unit thread
--   running, 'pump' below performs all three steps — drain, dispatch,
--   republish — so the production Lua observes the pose changes it asked
--   for. Without it every example would pass vacuously against a pose
--   that never moved.
--
--   Scheduling is CONTROLLED, never raced: 'simSecond' advances game
--   time by exactly one second, runs the ten 0.1 s physiology ticks
--   @init_loader.lua@ schedules, and runs one AI thought tick, with the
--   command pump between them. No example sleeps or polls.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "source drinking pose lifecycle"'@.
module Test.Headless.Unit.SourceDrinkPose (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Anim (poseTag)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Sim.Types
import Unit.Thread.Command.Pose
    ( handleUnitCollapseCommand, handleUnitCrawlCommand
    , handleUnitKillCommand, handleUnitReviveCommand
    , handleUnitTransitionToCommand )
import Unit.Types
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (FluidCell(..), FluidType(..), emptyIceMap)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identities

acolyteUid ∷ UnitId
acolyteUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "source_drink_pose_page"

-- | The acolyte's dry bank tile, and the lake tile east of it. Chebyshev
--   distance 1 with the actor's own tile dry is exactly the geometry
--   @drink_from_source@ descends from and
--   @unit_resource_tick.sourceDrinkingEligible@ admits.
bankX, bankY, lakeX, lakeY ∷ Int
bankX = 10
bankY = 10
lakeX = 11
lakeY = 10

-- | Tile centre of the bank. Floors to @(bankX, bankY)@.
bankPos ∷ (Float, Float)
bankPos = (fromIntegral bankX + 0.5, fromIntegral bankY + 0.5)

-- * Tunables restated
--
--   Each of these is an independent restatement of a shipped constant,
--   so a silent change to either side shows up as a failure here rather
--   than as an example that quietly stops meaning anything.

-- | @unit_resource_config.lua@'s @max_hydration@ for the fixture.
maxHydration ∷ Float
maxHydration = 42

-- | Thirst @1 − 30\/42 ≈ 0.286@ clears @unit_ai_tunables@'s
--   @drink_min_thirst@ of 0.2, and @30\/42 ≈ 0.714@ is well BELOW the
--   0.95 ratio @unit_ai_water.lua@'s drinking phase exits on — so no
--   example starts already satisfied.
startHydration ∷ Float
startHydration = 30

-- | The ratio the drinking phase ascends at.
drinkExitRatio ∷ Float
drinkExitRatio = 0.95

-- | @scripts\/unit_ai_source_phase.lua@'s @BUDGET@, in game seconds.
phaseBudget ∷ Int
phaseBudget = 60

-- | The physiology tick @init_loader.lua@ schedules, and how many of
--   them fall in one simulated second.
tickDt ∷ Double
tickDt = 0.1

ticksPerSecond ∷ Int
ticksPerSecond = 10

-- | Stats the hydration tick, the injury tick and the AI all read.
--   @consciousness@ is pinned at 1.0 so the collapse\/crawl machine
--   never depends on a defaulted brain reading, and @endurance@ at 1.0
--   so the regen factor is exactly the configured 5 L\/s.
baseStats ∷ HM.HashMap Text Float
baseStats = HM.fromList
    [ ("hydration", startHydration), ("max_hydration", maxHydration)
    , ("endurance", 1.0), ("consciousness", 1.0) ]

-- | Consciousness inside @brain.lua@'s delirium band
--   @[UNCONSCIOUS_BELOW, DELIRIOUS_BELOW) = [0.15, 0.40)@ — enough for
--   @unit_ai_mental.shortCircuit@ to preempt the running action, and NOT
--   enough to knock the unit out, so the injury tick's collapse branch
--   stays out of the way.
deliriousConsciousness ∷ Float
deliriousConsciousness = 0.25

-- | A shattered leg: @woundEffSeverity@ 0.9 clears @injuries.cannotWalk@'s
--   0.85 single-limb bar, which is the production reason such a unit is
--   held in Crawling.
shatteredLeg ∷ Wound
shatteredLeg = Wound
    { woundPart = "l_leg", woundKind = "fracture", woundSeverity = 0.9
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- * World fixture

-- | One origin chunk, flat at z 0, dry except for the single lake tile
--   at @(lakeX, lakeY)@. That tile is what makes @world.getFluidAt@ a
--   real query here rather than a stub.
bankChunk ∷ Bool → LoadedChunk
bankChunk wet =
    let area  = chunkSize * chunkSize
        terrV = VU.replicate area (0 ∷ Int)
        idx   = lakeY * chunkSize + lakeX
        fluid = V.replicate area Nothing
        fluid' | wet       = fluid V.// [(idx, Just (FluidCell Lake 0))]
               | otherwise = fluid
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = terrV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = fluid'
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

tilesWith ∷ Bool → WorldTileData
tilesWith wet = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) (bankChunk wet)
    , wtdMaxChunks = 1 }

-- | Install (or replace) the one visible page. Called again mid-example
--   to dry the lake up under a drinking unit.
setLake ∷ EngineEnv → Bool → IO ()
setLake env wet = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) (tilesWith wet)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }

-- * Unit fixture

mkAcolyte ∷ HM.HashMap Text Float → [Wound] → UnitInstance
mkAcolyte stats ws = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = fst bankPos, uiGridY = snd bankPos
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = ws
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | The sim state the pose handlers actually mutate, standing on the
--   bank tile with nothing in flight.
bankSimState ∷ Pose → UnitSimState
bankSimState pose = UnitSimState
    { usRealX = fst bankPos, usRealY = snd bankPos
    , usGridZ = 0, usRealZ = 0
    , usTarget = Nothing
    , usPose = pose, usState = Idle, usFacing = DirE
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 1
    , usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing
    , usClimbStartTime = Nothing, usClimbSlipAt = Nothing
    , usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0
    }

-- | A one-unit scene with a full lake, at game time zero, with every
--   queue drained so nothing leaks in from the previous example.
resetScene ∷ EngineEnv → HM.HashMap Text Float → [Wound] → IO ()
resetScene env stats ws = do
    writeIORef (gameTimeRef env) 0
    setLake env True
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.singleton acolyteUid (mkAcolyte stats ws) }
    writeIORef (utsRef env) emptyUnitThreadState
        { utsSimStates = HM.singleton acolyteUid (bankSimState Standing) }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- * The command pump
--
--   The three steps a running 'Unit.Thread' would perform between two
--   AI ticks, and the reason this gate can assert on poses at all.

-- | Mirrors 'Unit.Thread.activityLabel', which that module does not
--   export. Only the labels this fixture can actually produce are
--   spelled out; anything else is a fixture bug rather than a silent
--   "idle".
activityLabelOf ∷ UnitActivity → Text
activityLabelOf Idle                = "idle"
activityLabelOf Walking             = "walking"
activityLabelOf Running             = "running"
activityLabelOf Drinking            = "drinking"
activityLabelOf Eating              = "eating"
activityLabelOf Picking             = "pickup"
activityLabelOf (TransitioningTo _) = "transitioning"

-- | Drain @unitQueue@, dispatch every pose command to its REAL handler,
--   then republish @usPose@ \/ @usState@ into @uiPose@ \/ @uiActivity@
--   the way 'Unit.Thread.publishToRender' does. Returns the commands
--   that were drained, so an example can assert on what the tick ASKED
--   for as well as on what it achieved.
pump ∷ EngineEnv → IO [UnitCommand]
pump env = do
    cmds ← Q.flushQueue (unitQueue env)
    forM_ cmds $ \case
        UnitCollapse u          → handleUnitCollapseCommand (utsRef env) u
        UnitCrawl u             → handleUnitCrawlCommand (utsRef env) u
        UnitRevive u            → handleUnitReviveCommand (utsRef env) u
        UnitKill u              → handleUnitKillCommand env (utsRef env) u
        UnitTransitionTo u p st →
            handleUnitTransitionToCommand env (utsRef env) u p st
        -- Movement, animation and inventory commands are recorded and
        -- otherwise ignored: no example here depends on the unit
        -- actually travelling, and leaving it planted on the bank is
        -- what makes "the phase did not end" attributable to the phase
        -- machine rather than to a walk.
        _                       → pure ()
    publishPoses env
    pure cmds

publishPoses ∷ EngineEnv → IO ()
publishPoses env = do
    uts ← readIORef (utsRef env)
    um  ← readIORef (unitManagerRef env)
    let apply uid inst = case HM.lookup uid (utsSimStates uts) of
            Nothing → inst
            Just ss → inst { uiPose     = poseTag (usPose ss)
                           , uiActivity = activityLabelOf (usState ss) }
    writeIORef (unitManagerRef env) um
        { umInstances = HM.mapWithKey apply (umInstances um) }

-- | Labels for the pose commands a tick requested, in order.
poseLabels ∷ [UnitCommand] → [Text]
poseLabels cmds = [ label | Just label ← map classify cmds ]
  where
    classify = \case
        UnitCrawl _          → Just "crawl"
        UnitRevive _         → Just "revive"
        UnitCollapse _       → Just "collapse"
        UnitKill _           → Just "kill"
        UnitTransitionTo _ p _ → Just ("transition:" <> poseTag p)
        _                    → Nothing

-- * Lua plumbing

-- | The shipped modules, loaded for real. @scripts.unit_ai@ must come
--   first: @scripts.unit_ai_core@ reads the
--   @package.loaded[\"scripts.unit_ai\"]@ singleton at load time and
--   cannot be required on its own.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    loaded ← evalDebug ls $ T.concat
        [ "_G.__ai = require('scripts.unit_ai'); "
        , "_G.__core = require('scripts.unit_ai_core'); "
        , "_G.__water = require('scripts.unit_ai_water'); "
        , "_G.__phase = require('scripts.unit_ai_source_phase'); "
        , "_G.__tick = require('scripts.unit_resource_tick'); "
        , "_G.__inj = require('scripts.unit_resource_injury'); "
        , "_G.__cfg = require('scripts.unit_resource_config').acolyte; "
        , "_G.__params = require('scripts.unit_ai_tunables').acolyte; "
        , "return _G.__phase.BUDGET == ", tshow (fromIntegral phaseBudget ∷ Double)
        , " and _G.__params.drink_min_thirst == 0.2"
        , " and _G.__cfg.hydration.regen_factor_source_drinking == 5.0" ]
    loaded `shouldBe` "true"
    pure ls

-- | Per-unit AI state through the production 'ensureState', with the
--   remembered lake the sequence walks to. Water memory is seeded rather
--   than discovered because @unit.getVisibleTiles@ needs a render-side
--   FOV this harness does not run; everything downstream of it is the
--   production path.
seedState ∷ LuaBackendState → IO ()
seedState ls = do
    r ← evalDebug ls $ T.concat
        [ "local s = _G.__core.ensureState(1); "
        , "s.knownWaterSources = { { x = ", tshow lakeX
        , ", y = ", tshow lakeY, " } }; "
        , "return _G.__core.hasKnownWaterSource(_G.__ai.getState(1))" ]
    r `shouldBe` "true"

-- | Ten physiology ticks — hydration and injuries, exactly what
--   @unit_resources.update@ runs — then one command pump.
physiologySecond ∷ EngineEnv → LuaBackendState → IO [UnitCommand]
physiologySecond env ls = do
    r ← evalDebug ls $ T.concat
        [ "for _ = 1, ", tshow ticksPerSecond, " do "
        , "  local pose = unit.getPose(1); "
        , "  if pose == nil then break end; "
        , "  _G.__inj.tickInjuries(1, unit.getInfo(1), pose); "
        , "  _G.__tick.tickResource(1, 'acolyte', 'hydration', "
        , "    _G.__cfg.hydration, unit.getActivity(1), pose, "
        , tshow tickDt, "); "
        , "end; return true" ]
    r `shouldBe` "true"
    pump env

-- | One AI thought tick through the shipped dispatcher, then a pump.
aiSecond ∷ EngineEnv → LuaBackendState → IO [UnitCommand]
aiSecond env ls = do
    r ← evalDebug ls "_G.__ai.update(1.0); return true"
    r `shouldBe` "true"
    pump env

-- | One simulated second of controlled scheduling: advance game time by
--   exactly one second, run the physiology burst, then the AI tick.
--   @physiologyFirst@ chooses which of the two observes the other's pose
--   changes first — the two callback orderings the issue asks for.
simSecond ∷ Bool → EngineEnv → LuaBackendState → IO [UnitCommand]
simSecond physiologyFirst env ls = do
    now ← readIORef (gameTimeRef env)
    writeIORef (gameTimeRef env) (now + 1.0)
    if physiologyFirst
        then (<>) <$> physiologySecond env ls <*> aiSecond env ls
        else (<>) <$> aiSecond env ls <*> physiologySecond env ls

-- | Run simulated seconds until @done@ holds or @limit@ is reached.
--   Returns the number of seconds actually run and every pose command
--   requested along the way — a bounded loop with no sleeping and no
--   polling of wall-clock time.
runUntil ∷ Bool → EngineEnv → LuaBackendState → Int → IO Bool
         → IO (Int, [Text])
runUntil physiologyFirst env ls limit done = go 0 []
  where
    go n acc
        | n ≥ limit = pure (n, acc)
        | otherwise = do
            finished ← done
            if finished then pure (n, acc) else do
                cmds ← simSecond physiologyFirst env ls
                go (n + 1) (acc <> poseLabels cmds)

-- * Live readers

storedHydration ∷ EngineEnv → IO Float
storedHydration env = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup acolyteUid (umInstances um) ⌦ HM.lookup "hydration" ∘ uiStats of
        Just v  → pure v
        Nothing → fail "fixture acolyte has no stored hydration"

livePose ∷ EngineEnv → IO Text
livePose env = do
    uts ← readIORef (utsRef env)
    case HM.lookup acolyteUid (utsSimStates uts) of
        Just ss → pure (poseTag (usPose ss))
        Nothing → fail "fixture acolyte has no sim state"

-- | @s.sourcePhase@ as a flat string, @\"nil\"@ when cleared.
phaseOf ∷ LuaBackendState → IO Text
phaseOf ls = evalDebug ls
    "return tostring(_G.__ai.getState(1).sourcePhase)"

-- | @s.sourcePhaseAt@ presence. The deadline must die with the phase.
deadlineSet ∷ LuaBackendState → IO Text
deadlineSet ls = evalDebug ls
    "return _G.__ai.getState(1).sourcePhaseAt ~= nil"

-- | The game time the LIVE phase started at. Read before a bound is
--   expected to fire, since the deadline dies with the phase it bounds.
phaseStartedAt ∷ LuaBackendState → IO Double
phaseStartedAt ls = do
    r ← evalDebug ls "return _G.__ai.getState(1).sourcePhaseAt or -1"
    case reads (T.unpack (T.filter (≢ '"') r)) of
        [(v, _)] → pure v
        _        → fail ("no live phase deadline to read: " <> T.unpack r)

gameNow ∷ EngineEnv → IO Double
gameNow env = readIORef (gameTimeRef env)

-- | The bound fired, and fired ON TIME. A phase released a second after
--   it began was ended by something other than its budget; one still
--   alive past the budget was not bounded at all.
shouldHaveRunTheBudget ∷ Double → Double → Expectation
shouldHaveRunTheBudget startedAt endedAt =
    let held   = endedAt - startedAt
        budget = fromIntegral phaseBudget
    in held `shouldSatisfy` \d → d ≥ budget ∧ d ≤ budget + 2

-- | The scored utility of @drink_from_source@, as the arbitration loop
--   sees it. The bug's signature is this value pinned at @math.huge@.
drinkUtility ∷ LuaBackendState → IO Text
drinkUtility ls = evalDebug ls $ T.concat
    [ "local u = _G.__water.drinkFromSourceUtility(1, _G.__ai.getState(1), "
    , "_G.__params); "
    , "if u == math.huge then return 'inf' end; "
    , "if u == -math.huge then return 'neg_inf' end; "
    , "return 'finite'" ]

currentAction ∷ LuaBackendState → IO Text
currentAction ls = evalDebug ls
    "return tostring(_G.__ai.getState(1).currentAction)"

-- | The eligibility gate itself, so a hydration assertion is never the
--   only thing standing between an example and a vacuous pass.
eligible ∷ LuaBackendState → IO Text
eligible ls = evalDebug ls
    "return _G.__tick.sourceDrinkingEligible(1) == true"

-- | Quoted, because debug-console string returns arrive JSON-encoded.
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | A fully-hydrated acolyte's exit hydration, as the drinking phase
--   computes it.
drinkExitHydration ∷ Float
drinkExitHydration = drinkExitRatio * maxHydration

-- | Drive the whole sequence to completion (or give up at the budget),
--   asserting the shape every healthy run must have.
completesSequence ∷ Bool → EngineEnv → IO ()
completesSequence physiologyFirst env = do
    resetScene env baseStats []
    ls ← setupLua env
    seedState ls

    -- Nothing is satisfied at the start: the unit is thirsty enough to
    -- drink and far enough from the exit ratio to have to.
    storedHydration env `shouldReturn` startHydration
    phaseOf ls `shouldReturn` q "nil"
    drinkUtility ls `shouldReturn` q "finite"

    -- Staged deliberately, because "no phase and standing" is ALSO the
    -- state this example starts in: waiting for it in one step would
    -- return on the zeroth second having driven nothing at all.
    --
    -- Stage 1 -- the descent reaches the drinking posture. Pre-fix this
    -- never happens: the injury tick revives the crawl at 10 Hz, the AI
    -- re-issues the descent once a second, and the loop runs out its
    -- limit with the phase still "descending".
    (toDrink, descent) ← runUntil physiologyFirst env ls (phaseBudget + 5)
        (( ≡ q "drinking") <$> phaseOf ls)
    phaseOf ls `shouldReturn` q "drinking"
    livePose env `shouldReturn` "crawling"
    descent `shouldSatisfy` elem "transition:crouching"
    descent `shouldSatisfy` elem "transition:crawling"
    descent `shouldSatisfy` notElem "revive"
    -- Earned through the real gate, against the real lake tile.
    eligible ls `shouldReturn` "true"
    atDrink ← storedHydration env

    -- Stage 2 -- it drinks its fill and starts back up. The exit ratio
    -- is asserted HERE, at the tick the phase actually tests it, rather
    -- than at the end of the sequence: eligibility pays only the
    -- DRINKING phase (#2541), so the ascent drains again and the final
    -- value is legitimately a little under 0.95 × max.
    (toFull, _) ← runUntil physiologyFirst env ls (phaseBudget + 5)
        (( ≡ q "ascending") <$> phaseOf ls)
    peak ← storedHydration env
    peak `shouldSatisfy` (> atDrink)
    peak `shouldSatisfy` (≥ drinkExitHydration)

    -- Stage 3 -- the ascent completes and releases the lock.
    (toDone, ascent) ← runUntil physiologyFirst env ls (phaseBudget + 5) $ do
        phase ← phaseOf ls
        pose  ← livePose env
        pure (phase ≡ q "nil" ∧ pose ≡ "standing")
    ascent `shouldSatisfy` elem "transition:standing"
    ascent `shouldSatisfy` notElem "revive"

    -- Hydration STRICTLY increased across the whole sequence -- not
    -- merely "the phase cleared", which an abandonment also gives.
    final ← storedHydration env
    final `shouldSatisfy` (> startHydration)
    final `shouldSatisfy` (> atDrink)

    livePose env `shouldReturn` "standing"
    phaseOf ls `shouldReturn` q "nil"
    deadlineSet ls `shouldReturn` "false"
    -- Well inside the budget, so this is completion and not the bound.
    (toDrink + toFull + toDone) `shouldSatisfy` (< phaseBudget)

    -- Ordinary action selection resumes. The loop above stops the
    -- instant the lock is released, which is BEFORE the dispatcher next
    -- scores anything, so the switch is driven here rather than assumed.
    --
    -- Bounded rather than a single step, because the production thought
    -- schedule is jittered: `core.scheduleNext` puts the next decision
    -- 0.5-1.5 s out (@thought_interval@ 1.0 ± @thought_jitter@ 0.5 in
    -- unit_ai_tunables.lua), so a fixed one-second step lands before it
    -- about half the time. Three seconds covers the widest jitter; five
    -- is the limit, and the assertions after the loop are what fail if
    -- the switch never comes.
    drinkUtility ls `shouldReturn` q "neg_inf"
    currentAction ls `shouldReturn` q "drink_from_source"
    (_, _) ← runUntil physiologyFirst env ls 5
        ((≢ q "drink_from_source") <$> currentAction ls)
    action ← currentAction ls
    action `shouldSatisfy` (≢ q "drink_from_source")
    phaseOf ls `shouldReturn` q "nil"

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "source drinking pose lifecycle" $ do

    -- §1 The defect, and the ordering the issue asks to be covered both
    -- ways round. Pre-fix, the injury tick revives the descent at 10 Hz
    -- and `transition:crawling` is re-issued forever: the phase never
    -- reaches "drinking", hydration only drains, and the loop below runs
    -- out its limit with the lock still held.
    describe "a healthy thirsty acolyte completes the sequence (§1)" $ do
        it "with injury processing observing the crawl BEFORE the AI does"
            $ \env → completesSequence True env

        it "with the AI observing its own crawl first" $ \env →
            completesSequence False env

        it "earns its hydration through the real eligibility gate, at a \
           \real lake tile" $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            -- Descend far enough to be drinking, then assert the gate
            -- directly rather than only through the hydration it makes.
            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            phaseOf ls `shouldReturn` q "drinking"
            livePose env `shouldReturn` "crawling"
            eligible ls `shouldReturn` "true"
            -- getFluidAt's arity IS its contract: two values on a fluid
            -- tile, a single nil on dry ground.
            evalDebug ls "return (world.getFluidAt(11, 10))"
                `shouldReturn` q "lake"
            evalDebug ls "return (world.getFluidAt(10, 10)) == nil"
                `shouldReturn` "true"

    -- §2 The bound. An interruption the exemption deliberately does not
    -- cover — something else standing the unit up, over and over —
    -- cannot hold the lock forever.
    describe "a posture interrupted after entry (§2)" $ do
        it "terminates at the budget when an outside revive keeps \
           \undoing the descent, instead of locking at math.huge" $
          \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "descending") <$> phaseOf ls)
            phaseOf ls `shouldReturn` q "descending"
            drinkUtility ls `shouldReturn` q "inf"
            startedAt ← phaseStartedAt ls

            -- Something outside the AI stands it up every second. The
            -- sequence can re-issue the descent as often as it likes;
            -- what it cannot do is hold infinite priority forever.
            let revived = do
                    Q.writeQueue (unitQueue env) (UnitRevive acolyteUid)
                    _ ← pump env
                    (≡ q "nil") <$> phaseOf ls
            (_, _) ← runUntil True env ls (phaseBudget + 5) revived

            phaseOf ls `shouldReturn` q "nil"
            deadlineSet ls `shouldReturn` "false"
            drinkUtility ls `shouldReturn` q "finite"
            -- It held the lock for its whole budget and no longer: an
            -- early release would mean something other than the bound
            -- ended it, and a late one that nothing did.
            gameNow env ⌦ shouldHaveRunTheBudget startedAt

        it "terminates when the source dries up mid-drink and the 95% \
           \exit becomes unreachable" $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            phaseOf ls `shouldReturn` q "drinking"
            eligible ls `shouldReturn` "true"
            startedAt ← phaseStartedAt ls

            -- The lake is gone. Hydration can no longer rise, so the
            -- phase's own exit condition is unreachable.
            setLake env False
            eligible ls `shouldReturn` "false"
            before ← storedHydration env

            (_, _) ← runUntil True env ls (phaseBudget + 5)
                (( ≡ q "nil") <$> phaseOf ls)

            phaseOf ls `shouldReturn` q "nil"
            drinkUtility ls `shouldReturn` q "finite"
            gameNow env ⌦ shouldHaveRunTheBudget startedAt
            -- And it really was starved rather than quietly satisfied.
            after ← storedHydration env
            after `shouldSatisfy` (< before)

    -- §3 The second entry into the same lock: a preemption fires only
    -- the OUTGOING action's onExit, and the phase used to survive it.
    describe "preemption by a mental short-circuit (§3)" $ do
        it "ends the phase instead of leaving it latched at math.huge \
           \for after the episode" $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            phaseOf ls `shouldReturn` q "drinking"
            drinkUtility ls `shouldReturn` q "inf"

            -- Delirium, through the production predicate.
            setConsciousness env deliriousConsciousness
            evalDebug ls "return require('scripts.brain').isDelirious(1)"
                `shouldReturn` "true"

            _ ← aiSecond env ls
            currentAction ls `shouldReturn` q "delirious"
            phaseOf ls `shouldReturn` q "nil"
            deadlineSet ls `shouldReturn` "false"

            -- The episode ends. The lock must not come back on its own
            -- from the state the episode left behind.
            setConsciousness env 1.0
            drinkUtility ls `shouldReturn` q "finite"

    -- §4 The exemption must not latch. Whatever marks the posture as
    -- deliberate has to stop applying the moment the phase ends — a
    -- s.sourcePhase-keyed exemption that outlived its phase would turn
    -- the reported stranding into permanent revive suppression.
    describe "the exemption stops with the phase (§4)" $ do
        it "a unit still crawling when the phase clears is revived by \
           \the very next injury tick" $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            livePose env `shouldReturn` "crawling"

            -- While the phase is live the injury tick leaves it alone.
            exempt ← physiologySecond env ls
            poseLabels exempt `shouldBe` []
            livePose env `shouldReturn` "crawling"

            -- Clear the phase the way an abandonment does, leaving the
            -- unit exactly where it was: still healthy, still crawling.
            r ← evalDebug ls
                    "_G.__phase.clear(_G.__ai.getState(1)); return true"
            r `shouldBe` "true"

            revived ← physiologySecond env ls
            poseLabels revived `shouldSatisfy` elem "revive"
            livePose env `shouldReturn` "standing"

        it "the same crawling unit with no phase at all is revived — so \
           \the exemption above is doing the work, not the fixture" $
          \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls
            crawlNow env

            livePose env `shouldReturn` "crawling"
            phaseOf ls `shouldReturn` q "nil"
            cmds ← physiologySecond env ls
            poseLabels cmds `shouldSatisfy` elem "revive"
            livePose env `shouldReturn` "standing"

    -- §5 Requirement 4's controls: genuine injury handling and the
    -- pre-existing sleep waypoint both survive the new exemption.
    describe "preserved injury and sleep behaviour (§5)" $ do
        it "a newly disabling leg injury during the drinking phase still \
           \holds the unit down" $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            livePose env `shouldReturn` "crawling"

            -- The leg shatters mid-drink. cannotWalk now holds, so the
            -- crawl branch above the exemption owns the decision.
            woundNow env shatteredLeg
            evalDebug ls "return require('scripts.injuries').cannotWalk(1)"
                `shouldReturn` "true"
            cmds ← physiologySecond env ls
            poseLabels cmds `shouldSatisfy` notElem "revive"
            livePose env `shouldReturn` "crawling"

        it "a knockout during the drinking phase still collapses the \
           \unit — the exemption covers only the revive branch" $
          \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            livePose env `shouldReturn` "crawling"

            -- Below brain.lua's UNCONSCIOUS_BELOW of 0.15.
            setConsciousness env 0.05
            cmds ← physiologySecond env ls
            poseLabels cmds `shouldSatisfy` elem "collapse"
            livePose env `shouldReturn` "collapsed"

        it "an injured crawler with a disabling leg is never revived, \
           \phase or no phase" $ \env → do
            resetScene env baseStats [shatteredLeg]
            ls ← setupLua env
            seedState ls
            crawlNow env

            cmds ← physiologySecond env ls
            poseLabels cmds `shouldSatisfy` notElem "revive"
            livePose env `shouldReturn` "crawling"

        it "the sleep waypoint keeps its own exemption (#612)" $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls
            crawlNow env
            r ← evalDebug ls
                    "_G.__core.ensureState(1).sleepPhase = 'lying_down'; \
                    \return _G.__ai.getState(1).sleepPhase == 'lying_down'"
            r `shouldBe` "true"

            cmds ← physiologySecond env ls
            poseLabels cmds `shouldSatisfy` notElem "revive"
            livePose env `shouldReturn` "crawling"

    -- §6 The save disposition the new deadline needs. It is a CLOCK, so
    -- it must not ride into a payload; the phase it bounds is durable.
    describe "the phase deadline is session-scoped (§6)" $
        it "snapshotUnitState strips sourcePhaseAt and keeps sourcePhase"
            $ \env → do
            resetScene env baseStats []
            ls ← setupLua env
            seedState ls

            (_, _) ← runUntil True env ls 10 (( ≡ q "drinking") <$> phaseOf ls)
            deadlineSet ls `shouldReturn` "true"

            -- The REAL registered save component, snapshotting the
            -- REAL aiState singleton -- unitAi.init is what registers
            -- it, exactly as a booting session does.
            r ← evalDebug ls $ T.concat
                [ "_G.__ai.init(1); "
                , "local reg = require('scripts.lib.save_modules')"
                , "  .registry['unit_ai']; "
                , "local payload = reg.snapshot(); "
                , "local row = nil; "
                , "for _, entry in pairs(payload or {}) do row = entry end; "
                , "if row == nil then return 'no row' end; "
                , "return tostring(row.sourcePhase) .. '/' "
                , "  .. tostring(row.sourcePhaseAt)" ]
            r `shouldBe` q "drinking/nil"

-- * Scene mutations used mid-example

-- | Overwrite one stat on the live instance, the way another subsystem
--   committing to the unit manager would.
setStat ∷ EngineEnv → Text → Float → IO ()
setStat env name v = do
    um ← readIORef (unitManagerRef env)
    let bump inst = inst { uiStats = HM.insert name v (uiStats inst) }
    writeIORef (unitManagerRef env) um
        { umInstances = HM.adjust bump acolyteUid (umInstances um) }

setConsciousness ∷ EngineEnv → Float → IO ()
setConsciousness env = setStat env "consciousness"

-- | Add a wound to the live instance mid-example.
woundNow ∷ EngineEnv → Wound → IO ()
woundNow env w = do
    um ← readIORef (unitManagerRef env)
    let bump inst = inst { uiWounds = w : uiWounds inst }
    writeIORef (unitManagerRef env) um
        { umInstances = HM.adjust bump acolyteUid (umInstances um) }

-- | Put the unit on all fours through the real command path, so a
--   control case starts from the same pose the sequence reaches.
crawlNow ∷ EngineEnv → IO ()
crawlNow env = do
    Q.writeQueue (unitQueue env) (UnitCrawl acolyteUid)
    _ ← pump env
    pure ()
