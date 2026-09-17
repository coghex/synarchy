{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Terminal death survives an ordinary pose transition (#2651).
--
--   @unit.kill@ commits 'Dead' and clears every in-flight field;
--   @scripts\/unit_ai.lua@ short-circuits a dead pose on the strength
--   of that ("Dead pose: terminal. No AI, no resources, no revival").
--   'handleUnitTransitionToCommand' used to guard only "already at
--   target" and "already mid-transition", and a corpse satisfies
--   neither — so @UnitTransitionTo uid Standing@ overwrote @usPose@
--   with the living target, @publishToRender@ republished it,
--   @unit.getPose@ reported it, and ordinary AI resumed on the corpse.
--
--   The refusal has to live at COMMAND EXECUTION, not in Lua and not
--   at enqueue time: @unitQueue@ is FIFO, so an ordinary transition
--   enqueued while the unit was still alive drains AFTER a 'UnitKill'
--   enqueued ahead of it, by which point the caller's precondition is
--   stale. §3 below builds exactly that ordering.
--
--   Everything here runs the REAL handlers and the REAL registered
--   @unit.*@ verbs; nothing is stubbed. As in
--   "Test.Headless.Unit.SourceDrinkPose", 'pump' closes the loop the
--   pose verbs leave open — the verbs only ENQUEUE, the handlers
--   mutate @utsSimStates@, and @unit.getPose@ reads @uiPose@, which
--   only 'Unit.Thread.publishToRender' republishes. Without all three
--   steps every published-getter assertion would pass vacuously
--   against a field that never moved.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "terminal death pose transitions"'@.
module Test.Headless.Unit.TerminalDeathPose (spec) where

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
import Unit.Anim (poseTag, stateKey)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Sim.Types
import Unit.Thread.Command.Pose
    ( handleUnitCollapseCommand, handleUnitCrawlCommand
    , handleUnitKillCommand, handleUnitReviveCommand
    , handleUnitTransitionToCommand )
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types (emptyWorldManager)

-- * Fixture identities

subjectUid ∷ UnitId
subjectUid = UnitId 1

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "terminal_death_pose_page"

-- | Every destination @parsePose@ accepts, in its own order. Restated
--   here rather than derived, so a destination added to the Lua parser
--   without a decision about terminal death fails §4's sweep instead
--   of silently falling outside it.
livingDestinations ∷ [Pose]
livingDestinations =
    [Standing, Crouching, Crawling, Collapsed, Climbing, Falling, Sleeping]

-- * The synthetic transition clip
--
--   'handleUnitTransitionToCommand' reads a clip's LENGTH only —
--   'storageMaxFrameCount' and 'aFps' — so a clip with no image behind
--   it is enough to drive the positive-duration branch. No artwork is
--   authored or needed (#2651 out-of-scope).

clipFrames ∷ Int
clipFrames = 8

clipFps ∷ Float
clipFps = 8

-- | One direction, 'clipFrames' real frames, 'clipFps'. At stride 1
--   the handler's own arithmetic gives
--   @(((8 - 1 + 0) \`div\` 1) + 1) \/ 8 = 1.0@ second — comfortably
--   positive, which is the whole point: §2's corpse is refused on the
--   branch that would otherwise install a 'TransitioningTo'.
syntheticClip ∷ Animation
syntheticClip = atlasAnimation clipFps False False
    (ResidentAtlas syntheticAtlas (TextureHandle 42))

syntheticAtlas ∷ AtlasAnimation
syntheticAtlas = AtlasAnimation
    { aaName = "synthetic-transition"
    , aaFormat = AtlasFormatPng
    , aaPath = "test/synthetic.png"
    , aaAtlasWidth = 8, aaAtlasHeight = 1
    , aaCellWidth = 1, aaCellHeight = 1, aaCellPadding = 0
    , aaColumns = clipFrames, aaRows = 1
    , aaFps = clipFps, aaLoop = False, aaFlip = False
    , aaDirections = Map.singleton DirS
        (AtlasDirectionRow { adrDirection = DirS
                           , adrRow = 0
                           , adrFrameCount = clipFrames })
    , aaSourceDigest = "synthetic", aaAtlasDigest = "synthetic"
    }

-- | The expected duration of 'syntheticClip' at stride 1, restated
--   independently of the handler.
clipDuration ∷ Double
clipDuration = fromIntegral clipFrames / realToFrac clipFps

-- * Scene

mkUnit ∷ UnitInstance
mkUnit = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10.5, uiGridY = 10.5
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A sim state with nothing in flight, in the given pose.
simState ∷ Pose → UnitSimState
simState pose = UnitSimState
    { usRealX = 10.5, usRealY = 10.5
    , usGridZ = 0, usRealZ = 0
    , usTarget = Nothing
    , usPose = pose, usState = Idle, usFacing = DirS
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

-- | One living unit at game time zero, every queue drained, carrying
--   exactly the animations the example wants the duration lookup to
--   find. @[]@ is the no-clip fixture (the shipped acolyte declares no
--   @dead-to-standing@ composite either), which is what makes the
--   handler take its zero-duration branch.
resetScene ∷ EngineEnv → Pose → [(Text, Animation)] → IO ()
resetScene env pose anims = do
    writeIORef (gameTimeRef env) 0
    writeIORef (worldManagerRef env) emptyWorldManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte"
            (minimalDef "acolyte" "Acolyte") { udAnimations = HM.fromList anims }
        , umInstances = HM.singleton subjectUid mkUnit }
    writeIORef (utsRef env) emptyUnitThreadState
        { utsSimStates = HM.singleton subjectUid (simState pose) }
    _ ← Q.flushQueue (unitQueue env)
    publishPoses env

-- * The command pump

-- | Mirrors 'Unit.Thread.activityLabel', which that module does not
--   export. Only the labels this fixture can produce are spelled out.
activityLabelOf ∷ UnitActivity → Text
activityLabelOf Idle                = "idle"
activityLabelOf Walking             = "walking"
activityLabelOf Running             = "running"
activityLabelOf Drinking            = "drinking"
activityLabelOf Eating              = "eating"
activityLabelOf Picking             = "pickup"
activityLabelOf (TransitioningTo _) = "transitioning"

-- | Drain @unitQueue@ IN ORDER, dispatch every pose command to its
--   REAL handler, then republish @usPose@ \/ @usState@ into @uiPose@ \/
--   @uiActivity@ the way 'Unit.Thread.publishToRender' does.
--
--   The in-order drain is load-bearing for §3: 'Q.flushQueue' returns
--   the FIFO's contents in enqueue order, so a 'UnitKill' written
--   before a 'UnitTransitionTo' executes before it, which is the
--   ordering the guard has to survive.
pump ∷ EngineEnv → IO ()
pump env = do
    cmds ← Q.flushQueue (unitQueue env)
    forM_ cmds $ \case
        UnitCollapse u          → handleUnitCollapseCommand (utsRef env) u
        UnitCrawl u             → handleUnitCrawlCommand (utsRef env) u
        UnitRevive u            → handleUnitReviveCommand (utsRef env) u
        UnitKill u              → handleUnitKillCommand env (utsRef env) u
        UnitTransitionTo u p st →
            handleUnitTransitionToCommand env (utsRef env) u p st
        _                       → pure ()
    publishPoses env

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

-- * Authoritative-state readers

liveSim ∷ EngineEnv → IO UnitSimState
liveSim env = do
    uts ← readIORef (utsRef env)
    case HM.lookup subjectUid (utsSimStates uts) of
        Just ss → pure ss
        Nothing → do
            expectationFailure "fixture unit lost its sim state"
            error "unreachable: expectationFailure throws"

-- | The four fields requirement 1 names, as one comparable tuple.
poseSnapshot ∷ EngineEnv → IO (Pose, UnitActivity, Maybe Double, Int)
poseSnapshot env = do
    ss ← liveSim env
    pure ( usPose ss, usState ss, usTransitionUntil ss, usTransitionStride ss )

-- | What 'handleUnitKillCommand' leaves behind, restated
--   independently: 'Dead', 'Idle', no deadline, stride reset to 1.
corpseSnapshot ∷ (Pose, UnitActivity, Maybe Double, Int)
corpseSnapshot = (Dead, Idle, Nothing, 1)

-- * Lua plumbing

-- | Debug-console string returns come back JSON-quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

transitionTo ∷ LuaBackendState → Pose → IO Text
transitionTo ls target = evalDebug ls $ T.concat
    [ "return unit.transitionTo(1, '", poseTag target, "')" ]

publishedPose ∷ LuaBackendState → IO Text
publishedPose ls = evalDebug ls "return unit.getPose(1)"

publishedActivity ∷ LuaBackendState → IO Text
publishedActivity ls = evalDebug ls "return unit.getActivity(1)"

spec ∷ Spec
spec = aroundAll withHeadlessEngineNoWorld $
  describe "terminal death pose transitions" $ do

    -- §1 The defect on the branch it shipped on. The shipped acolyte
    -- declares no `dead-to-standing` composite, so the duration lookup
    -- misses and the handler used to snap `usPose` straight to the
    -- living target.
    describe "a killed unit with NO transition clip (§1)" $ do
        it "keeps Dead/Idle and its cleared deadline and stride" $ \env → do
            resetScene env Standing []
            Q.writeQueue (unitQueue env) (UnitKill subjectUid)
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

            Q.writeQueue (unitQueue env)
                (UnitTransitionTo subjectUid Standing 1)
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

        it "stays dead through every parsePose destination, repeatedly" $
          \env → do
            resetScene env Standing []
            Q.writeQueue (unitQueue env) (UnitKill subjectUid)
            pump env

            forM_ (livingDestinations ⧺ livingDestinations) $ \target → do
                Q.writeQueue (unitQueue env)
                    (UnitTransitionTo subjectUid target 1)
                pump env
                poseSnapshot env `shouldReturn` corpseSnapshot

    -- §2 The other branch. A clip whose key the handler ACTUALLY
    -- resolves, with a length that ACTUALLY yields a positive
    -- duration -- both pinned below rather than assumed, because a
    -- misnamed key or a zero-length clip would silently retarget this
    -- example at §1's branch and it would keep passing while covering
    -- nothing.
    describe "a killed unit WITH an available transition clip (§2)" $ do
        it "resolves its clip under the key the handler looks up" $ \_ →
            stateKey Dead (TransitioningTo Standing) `shouldBe` "dead-to-standing"

        it "uses a clip long enough to be a TIMED transition, proved on a \
           \living unit with the same metadata" $ \env → do
            -- The control: identical frame count and fps, under a
            -- LIVING key. If this does not install a TransitioningTo
            -- with a real deadline, the clip is too short to prove
            -- anything about the corpse either.
            resetScene env Standing [("standing-to-crouching", syntheticClip)]
            Q.writeQueue (unitQueue env)
                (UnitTransitionTo subjectUid Crouching 1)
            pump env
            (pose, act, deadline, stride) ← poseSnapshot env
            pose `shouldBe` Standing
            act `shouldBe` TransitioningTo Crouching
            deadline `shouldBe` Just clipDuration
            stride `shouldBe` 1

        it "keeps Dead/Idle and its cleared deadline and stride" $ \env → do
            resetScene env Standing [("dead-to-standing", syntheticClip)]
            Q.writeQueue (unitQueue env) (UnitKill subjectUid)
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

            Q.writeQueue (unitQueue env)
                (UnitTransitionTo subjectUid Standing 1)
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

    -- §3 The ordering a Lua-side precheck cannot cover. Both commands
    -- are enqueued while the unit is still ALIVE, so the transition
    -- was legitimate when it was asked for; the kill executes first,
    -- and the transition has to be refused against state that only the
    -- handler can see.
    describe "a transition accepted before the kill, drained after it (§3)" $ do
        it "is refused, with the pose still living at enqueue time" $
          \env → do
            resetScene env Standing []
            -- Alive when BOTH requests are made.
            (usPose <$> liveSim env) `shouldReturn` Standing
            Q.writeQueue (unitQueue env) (UnitKill subjectUid)
            Q.writeQueue (unitQueue env)
                (UnitTransitionTo subjectUid Standing 1)
            (usPose <$> liveSim env) `shouldReturn` Standing

            -- One drain, FIFO: kill, then transition.
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

        it "is refused on the timed branch too" $ \env → do
            resetScene env Standing [("dead-to-standing", syntheticClip)]
            Q.writeQueue (unitQueue env) (UnitKill subjectUid)
            Q.writeQueue (unitQueue env)
                (UnitTransitionTo subjectUid Crouching 1)
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

    -- §4 The controls. The guard must cost living units nothing.
    describe "living units are unaffected (§4)" $ do
        it "still reaches every parsePose destination instantly with no \
           \clip" $ \env →
            forM_ livingDestinations $ \target → do
                -- Crouching is the source for Standing so no example is
                -- a no-op "already there".
                let from = if target ≡ Standing then Crouching else Standing
                resetScene env from []
                Q.writeQueue (unitQueue env)
                    (UnitTransitionTo subjectUid target 1)
                pump env
                (pose, act, deadline, _) ← poseSnapshot env
                pose `shouldBe` target
                act `shouldBe` Idle
                deadline `shouldBe` Nothing

        it "still takes the TIMED branch when a clip exists" $ \env → do
            resetScene env Standing [("standing-to-sleeping", syntheticClip)]
            Q.writeQueue (unitQueue env)
                (UnitTransitionTo subjectUid Sleeping 2)
            pump env
            (pose, act, deadline, stride) ← poseSnapshot env
            pose `shouldBe` Standing
            act `shouldBe` TransitioningTo Sleeping
            stride `shouldBe` 2
            -- Stride 2 halves the frames shown: ceil(7/2) + 1 = 5.
            deadline `shouldBe` Just (5 / realToFrac clipFps)

        it "still revives Collapsed and Crawling, and still leaves Dead \
           \alone" $ \env → do
            forM_ [Collapsed, Crawling] $ \pose → do
                resetScene env pose []
                Q.writeQueue (unitQueue env) (UnitRevive subjectUid)
                pump env
                (usPose <$> liveSim env) `shouldReturn` Standing

            resetScene env Standing []
            Q.writeQueue (unitQueue env) (UnitKill subjectUid)
            pump env
            Q.writeQueue (unitQueue env) (UnitRevive subjectUid)
            pump env
            poseSnapshot env `shouldReturn` corpseSnapshot

    -- §5 What Lua sees. The published getters are the surface
    -- `scripts/unit_ai.lua` short-circuits on, and the return value is
    -- the contract this issue pins down: `unit.transitionTo` reports
    -- whether the request was ENQUEUED, not whether it was honoured.
    describe "through the registered Lua API (§5)" $ do
        it "returns false for the destination \"dead\" -- an ARGUMENT \
           \error, refused by parsePose before anything is enqueued" $
          \env → do
            resetScene env Standing []
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.transitionTo(1, 'dead')"
                `shouldReturn` "false"
            -- Nothing reached the queue, so nothing to refuse later.
            (null <$> Q.flushQueue (unitQueue env)) `shouldReturn` True
            (usPose <$> liveSim env) `shouldReturn` Standing

        it "returns true for a well-formed request on an already-dead \
           \unit, and the corpse stays dead and idle" $ \env → do
            resetScene env Standing []
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.kill(1)" `shouldReturn` "true"
            pump env
            publishedPose ls `shouldReturn` q "dead"
            publishedActivity ls `shouldReturn` q "idle"

            forM_ (livingDestinations ⧺ livingDestinations) $ \target → do
                -- ACCEPTED: the request was well formed and enqueued.
                transitionTo ls target `shouldReturn` "true"
                pump env
                -- REFUSED: execution left the corpse untouched.
                publishedPose ls `shouldReturn` q "dead"
                publishedActivity ls `shouldReturn` q "idle"
                poseSnapshot env `shouldReturn` corpseSnapshot

        it "still moves a LIVING unit's published pose, so the getters \
           \are not stuck" $ \env → do
            resetScene env Standing []
            ls ← newBareLuaBackend env
            publishedPose ls `shouldReturn` q "standing"
            transitionTo ls Crawling `shouldReturn` "true"
            pump env
            publishedPose ls `shouldReturn` q "crawling"
            publishedActivity ls `shouldReturn` q "idle"
