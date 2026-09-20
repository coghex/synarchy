-- | The starvation lean floor decides death within a tolerance (#2630).
--
--   @scripts\/unit_resource_energy.lua@'s @tickStarvation@ recomputes
--   the respiratory-failure floor in Lua doubles —
--   @0.20 × frame_mass@, or the legacy @4.4 · h²@ for units seeded
--   before @frame_mass@ existed — and compares it against the STORED
--   @lean_mass@. Its own catabolism branch clamps @lean_mass@ to that
--   floor and writes it back through @unit.setStat@, and @uiStats@ is
--   @HashMap Text Float@ ("Unit.Types.Instance"), so the clamped value
--   makes a binary32 round-trip on the way out and a binary64 one on
--   the way back in.
--
--   That round-trip can round UP. At frame mass 100.1 the Lua floor is
--   20.019999694824 and the value storage hands back is
--   20.020000457764, so a strict @lean <= minLean@ was false for a
--   unit sitting exactly on its floor: catabolism re-clamped to the
--   same rounded value every tick and the unit never died, however
--   long it starved. @energy.LEAN_FLOOR_TOL@ (1e-4 kg) closes that,
--   exactly as @energy.FAT_FLOOR_TOL@ closed the same hazard for the
--   fat floor in #2556.
--
--   Every example here drives the SHIPPED @unit_resource_energy.lua@
--   over a REAL 'unitManagerRef' through the REAL registered @unit.*@
--   API, so every floor is computed from a real stored 'Float', every
--   clamp is a real @unit.setStat@, and every death is a real
--   'UnitKill' on @unitQueue@ rather than a stubbed counter. §1 pins
--   the rounding premise each later section depends on, by writing the
--   floor through that same production path and reporting which side
--   of it storage landed on.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "starvation lean floor"'@.
module Test.Headless.Unit.LeanFloorDeath (spec) where

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

-- * Fixture constants

acolyteUid ∷ UnitId
acolyteUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "lean_floor_death_page"

-- | The one physiology tick the shipped @init_loader.lua@ schedules.
tickDt ∷ Double
tickDt = 0.1

-- | The lean floor's fraction of frame mass, restated from
--   @scripts\/unit_resource_energy.lua@ rather than read back out of
--   it, so a silent change to either side shows up here.
minLeanFrac ∷ Double
minLeanFrac = 0.20

-- | The legacy height-only floor, for units seeded before
--   @frame_mass@ existed: @4.4 · h²@.
legacyMinLeanPerH2 ∷ Double
legacyMinLeanPerH2 = 4.4

-- | The fat floor's fraction of frame mass. Only needed to park fat
--   ON its floor, which is what puts catabolism in its pure-muscle
--   regime — the regime whose clamp is the bug's own mechanism.
minFatFrac ∷ Double
minFatFrac = 0.02

-- | The tolerance the fix introduces, restated.
leanFloorTol ∷ Double
leanFloorTol = 1e-4

-- | Three frame masses whose lean floors round three different ways
--   through binary32 storage. §1 proves each premise rather than
--   trusting these names.
frameRoundsUp, frameRoundsDown, frameExact ∷ Float
frameRoundsUp   = 100.1     -- floor 20.019999694824 → stored 20.020000457764
frameRoundsDown = 100.2     -- floor 20.039999389648 → stored 20.039999008179
frameExact      = 70.0      -- floor 14.0, exactly representable

-- | The legacy fixture's height. 2.0 gives a floor of 17.6, which
--   storage rounds UP to 17.600000381470 — the same hazard reached
--   through the fallback branch.
legacyHeight ∷ Float
legacyHeight = 2.0

-- | An ordinary height for the frame-mass fixtures. The frame branch
--   wins whenever @frame_mass@ is readable, so this never decides a
--   floor; it is here because @tickStarvation@ reads @height@ before
--   it reads anything else and returns early without one.
fixtureHeight ∷ Float
fixtureHeight = 1.8

-- | @metabolism_rate@ is stored directly rather than left to
--   @unit_stats@'s derivation, so the deficit §4 clamps with is a
--   fixture input and not a second formula that could drift. 1800
--   kcal\/s × 0.1 s = 180 kcal, which is exactly 0.1 kg of muscle at
--   @KCAL_PER_KG_LEAN@ — ten times the gap §4 opens.
fixtureMetabolismRate ∷ Float
fixtureMetabolismRate = 1800

-- * The floors, computed the way the shipped module computes them

-- | The floor @tickStarvation@ arrives at for a STORED frame mass:
--   binary32 out of storage, widened to the binary64 Lua multiplies
--   in.
luaFloorFor ∷ Float → Double
luaFloorFor frame = minLeanFrac * realToFrac frame

-- | The same, for the legacy height-only branch.
luaLegacyFloorFor ∷ Float → Double
luaLegacyFloorFor h = legacyMinLeanPerH2 * realToFrac h * realToFrac h

-- | What storage hands back after that floor is written to
--   @lean_mass@ — the value the NEXT tick actually compares.
storedFloorFor ∷ Float → Float
storedFloorFor frame = realToFrac (luaFloorFor frame)

-- * Scene

-- | Everything @tickStarvation@ reads, and nothing else.
--   @body_mass@ is generous enough that neither clamp can drive it to
--   zero, and @calories@ defaults to an EMPTY store so the catabolism
--   branch is reachable; §5 refills it to show the death check runs
--   first.
baseStats ∷ HM.HashMap Text Float
baseStats = HM.fromList
    [ ("height", fixtureHeight), ("body_mass", 100.0)
    , ("fat_mass", 5.0), ("lean_mass", 40.0)
    , ("calories", 0.0), ("metabolism_rate", fixtureMetabolismRate)
    , ("strength", 1.0), ("endurance", 1.0) ]

-- | A frame-mass unit: the branch every modern unit takes.
framedStats ∷ Float → HM.HashMap Text Float
framedStats frame = HM.insert "frame_mass" frame baseStats

-- | A legacy unit: @frame_mass@ genuinely ABSENT, so the height-only
--   fallback is the floor in force rather than a second formula
--   sitting beside the real one.
legacyStats ∷ HM.HashMap Text Float
legacyStats = HM.insert "height" legacyHeight baseStats

mkUnit ∷ HM.HashMap Text Float → UnitInstance
mkUnit stats = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10, uiGridZ = 0
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

-- | Install a one-unit scene and drain whatever the previous example
--   left on the command queue, so every count below starts at zero.
resetScene ∷ EngineEnv → HM.HashMap Text Float → IO ()
resetScene env stats = do
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.singleton acolyteUid (mkUnit stats) }
    _ ← Q.flushQueue (unitQueue env)
    pure ()

-- * Live readers

storedStat ∷ EngineEnv → Text → IO (Maybe Float)
storedStat env name = do
    um ← readIORef (unitManagerRef env)
    pure (HM.lookup acolyteUid (umInstances um) ⌦ HM.lookup name ∘ uiStats)

-- | How many deaths the shipped module has REQUESTED since the last
--   check. @unit.kill@ enqueues 'UnitKill'
--   ("Engine.Scripting.Lua.API.Units.Spawn"), no unit thread runs in
--   this harness, and 'Q.flushQueue' takes the whole queue — so this
--   counts real production kill requests and resets the window.
killRequests ∷ EngineEnv → IO Int
killRequests env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure (length [ () | UnitKill u ← cmds, u ≡ acolyteUid ])

-- | Float stored values carry a rounding step, so mass comparisons
--   allow 1e-5 — ten times tighter than @LEAN_FLOOR_TOL@ itself, so a
--   value asserted "at the floor" here is unambiguously inside the
--   band, and far above the ~1e-6 binary32 step at 20 kg.
massTol ∷ Float
massTol = 1e-5

shouldBeNear ∷ IO (Maybe Float) → Float → Expectation
shouldBeNear act expected = act ⌦ \case
    Nothing → expectationFailure $
        "expected a stored value near " <> show expected
        <> ", found no entry"
    Just actual
        | abs (actual - expected) < massTol → pure ()
        | otherwise → expectationFailure $
            "expected a stored value near " <> show expected
            <> ", found " <> show actual

-- * Lua plumbing

-- | The shipped module, loaded for real, with the tolerance the whole
--   file is about pinned at load time so a rename or a retuning fails
--   here rather than silently changing what every example measures.
setupLua ∷ EngineEnv → IO LuaBackendState
setupLua env = do
    ls ← newBareLuaBackend env
    loaded ← evalDebug ls $ T.concat
        [ "_G.__energy = require('scripts.unit_resource_energy'); "
        , "return _G.__energy.LEAN_FLOOR_TOL == ", tshow leanFloorTol
        , " and _G.__energy.FAT_FLOOR_TOL == 1e-4" ]
    loaded `shouldBe` "true"
    pure ls

-- | One tick of the SHIPPED catabolism.
tickStarvation ∷ LuaBackendState → IO ()
tickStarvation ls = do
    r ← evalDebug ls $ T.concat
        [ "_G.__energy.tickStarvation(1, ", tshow tickDt, "); return true" ]
    r `shouldBe` "true"

-- | Write @lean_mass@ at @offset@ kg above the floor
--   @tickStarvation@ itself computes — recomputed IN LUA from the
--   read-back stats, through the same @unit.setStat@ the catabolism
--   clamp writes with — and report which side of that floor storage
--   landed on. @offset@ 0 is the clamp's own value, which is the case
--   the bug lives in.
placeLean ∷ LuaBackendState → Double → IO Text
placeLean ls offset = fmap (T.dropAround (≡ '"')) $ evalDebug ls $ T.concat
    [ "local frame = unit.getStat(1, 'frame_mass'); "
    , "local h = unit.getStat(1, 'height'); "
    , "local floor = frame and (", tshow minLeanFrac, " * frame) "
    , "  or (", tshow legacyMinLeanPerH2, " * h * h); "
    , "unit.setStat(1, 'lean_mass', floor + ", tshow offset, "); "
    , "local stored = unit.getStat(1, 'lean_mass'); "
    , "if stored > floor then return 'above' "
    , "elseif stored < floor then return 'below' "
    , "else return 'exact' end" ]

-- | Park @fat_mass@ ON its floor, which switches catabolism to its
--   pure-muscle regime — the one whose clamp writes @lean_mass@ back.
parkFatOnFloor ∷ LuaBackendState → IO ()
parkFatOnFloor ls = do
    r ← evalDebug ls $ T.concat
        [ "local frame = unit.getStat(1, 'frame_mass'); "
        , "unit.setStat(1, 'fat_mass', ", tshow minFatFrac, " * frame); "
        , "return _G.__energy.atFatFloor(1, unit.getStat(1, 'fat_mass'))" ]
    r `shouldBe` "true"

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "starvation lean floor" $ do

    -- Every later section asserts a death (or its absence) for a unit
    -- whose lean mass sits ON its floor. That only means anything if
    -- the three fixtures really do round three different ways, so the
    -- premise is established through the production write path rather
    -- than asserted in a comment.
    describe "the Float32 round-trip really moves a clamped floor (§1)" $ do
        it "frame mass 100.1 stores a lean floor ABOVE the Lua double — \
           \the case a strict comparison never fires on" $ \env → do
            resetScene env (framedStats frameRoundsUp)
            ls ← setupLua env
            placeLean ls 0 `shouldReturn` "above"
            storedStat env "lean_mass" `shouldBeNear`
                storedFloorFor frameRoundsUp

        it "frame mass 100.2 stores one BELOW it, so the fix cannot be \
           \a one-sided accident" $ \env → do
            resetScene env (framedStats frameRoundsDown)
            ls ← setupLua env
            placeLean ls 0 `shouldReturn` "below"

        it "frame mass 70 stores one EXACTLY, the case that always \
           \worked" $ \env → do
            resetScene env (framedStats frameExact)
            ls ← setupLua env
            placeLean ls 0 `shouldReturn` "exact"

        it "the legacy height-only fallback rounds UP too at height 2 — \
           \4.4·h² = 17.6 stores as 17.600000381470" $ \env → do
            resetScene env legacyStats
            ls ← setupLua env
            placeLean ls 0 `shouldReturn` "above"
            storedStat env "lean_mass" `shouldBeNear`
                realToFrac (luaLegacyFloorFor legacyHeight)

    -- Requirement 1: the verdict is the same whichever way the store
    -- rounded. Without LEAN_FLOOR_TOL the first example here reports
    -- 0 deaths forever.
    describe "a unit clamped to its lean floor dies (§2)" $ do
        forM_ [ ("an upward-rounding", frameRoundsUp)
              , ("a downward-rounding", frameRoundsDown)
              , ("an exactly representable", frameExact) ] $ \(label, frame) →
            it ("requests exactly one death on one tick at " <> T.unpack label
                <> " floor") $ \env → do
                resetScene env (framedStats frame)
                ls ← setupLua env
                _ ← placeLean ls 0
                _ ← killRequests env
                tickStarvation ls
                killRequests env `shouldReturn` 1

        it "the legacy height-only unit dies too, so the fallback \
           \shares the tolerance rather than keeping a strict \
           \comparison of its own" $ \env → do
            resetScene env legacyStats
            ls ← setupLua env
            _ ← placeLean ls 0
            _ ← killRequests env
            tickStarvation ls
            killRequests env `shouldReturn` 1

    -- Requirement 3, and the other edge of the band: a tolerance wide
    -- enough to kill a healthy unit would be a worse bug than the one
    -- being fixed.
    describe "a unit above its lean floor lives (§3)" $ do
        it "1 kg clear of the floor is not killed, however many ticks \
           \it starves" $ \env → do
            resetScene env (framedStats frameRoundsUp)
            ls ← setupLua env
            _ ← placeLean ls 1.0
            _ ← killRequests env
            forM_ [1 ∷ Int .. 5] $ \_ → tickStarvation ls
            killRequests env `shouldReturn` 0

        it "2e-4 kg above it — OUTSIDE the tolerance — also lives" $
          \env → do
            resetScene env (framedStats frameExact)
            ls ← setupLua env
            _ ← placeLean ls 2e-4
            _ ← killRequests env
            tickStarvation ls
            killRequests env `shouldReturn` 0

        it "0.5e-4 kg above it — INSIDE the tolerance — dies, so the \
           \band is the one the constant declares" $ \env → do
            resetScene env (framedStats frameExact)
            ls ← setupLua env
            _ ← placeLean ls 0.5e-4
            _ ← killRequests env
            tickStarvation ls
            killRequests env `shouldReturn` 1

    -- The end-to-end shape of the reported bug: nothing external
    -- places the unit on its floor, production catabolism does, and
    -- the very next tick has to act on what it wrote.
    describe "production catabolism clamps a unit into death (§4)" $
        it "a unit 0.01 kg above an upward-rounding floor is clamped \
           \onto it by one tick and killed by the next" $ \env → do
            resetScene env (framedStats frameRoundsUp)
            ls ← setupLua env
            parkFatOnFloor ls
            _ ← placeLean ls 0.01
            _ ← killRequests env

            -- Tick 1: above the tolerance, so no death — the deficit
            -- eats 0.1 kg of muscle and the clamp stops it at the
            -- floor, writing the rounded value back through storage.
            tickStarvation ls
            killRequests env `shouldReturn` 0
            storedStat env "lean_mass" `shouldBeNear`
                storedFloorFor frameRoundsUp

            -- Tick 2: the value catabolism itself chose is now the
            -- value the check sees.
            tickStarvation ls
            killRequests env `shouldReturn` 1

    -- The tolerance must not disturb the order tickStarvation asks its
    -- questions in: respiratory failure is checked BEFORE the calorie
    -- store, so a fed unit that has already wasted to its floor still
    -- dies.
    describe "the death check still precedes the calorie check (§5)" $
        it "a unit at its floor with a FULL calorie store is killed \
           \without catabolizing anything" $ \env → do
            resetScene env
                (HM.insert "calories" 1000 (framedStats frameRoundsUp))
            ls ← setupLua env
            _ ← placeLean ls 0
            _ ← killRequests env
            fatBefore ← storedStat env "fat_mass"
            tickStarvation ls
            killRequests env `shouldReturn` 1
            storedStat env "fat_mass" `shouldBeNear` maybe 0 id fatBefore
            storedStat env "calories" `shouldBeNear` 1000
