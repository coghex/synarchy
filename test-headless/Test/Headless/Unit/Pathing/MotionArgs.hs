{-# LANGUAGE OverloadedStrings #-}
-- | The numeric argument domains of the three unit MOTION verbs and of
--   a unit def's @max_speed@ (#2290).
--
--   @unit.setPos@, @unit.moveTo@ and @unit.setMoveSpeed@ each used to
--   convert their numeric slots with a bare @realToFrac@ and substitute
--   a zero for a slot that was missing or not a number, then enqueue
--   unconditionally. Nothing downstream rejected the result: @floor@
--   does not throw in GHC and @globalToChunk@ maps any @Int@ somewhere,
--   so a NaN, an infinity, a dropped argument or a negative speed all
--   reached the simulation as ordinary values. The damage was silent
--   and durable — a NaN target makes every step NaN, the arrival test
--   never becomes true, and the unstuck-able position is persisted
--   verbatim on the next save.
--
--   Three independent things are gated here, because a fix to any one
--   of them alone would leave the others open:
--
--     * the LIVE Lua boundary, driven through the real registered API,
--       asserting the return value, the diagnostic, that the command
--       queue stayed EMPTY, and — after the real dispatcher has run —
--       that the simulation did not move. The queue assertion is what
--       keeps this non-vacuous: an ingress that enqueued a NaN would
--       still leave a refused-looking @false@ behind if nobody looked;
--     * the unit thread's own defensive check, driven by calling the
--       handlers directly with a malformed command, since no Lua call
--       can produce one any more;
--     * the @max_speed@ decoder, which is the base every AI speed
--       derives from and the run-gait threshold besides.
--
--   Every group carries a control that must MUTATE the fixture, so a
--   \"nothing changed\" assertion cannot pass by the fixture being inert.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unit motion argument domains"'@
module Test.Headless.Unit.Pathing.MotionArgs (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import Control.Exception (bracket)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlUnits (UnitYamlDef(..), UnitYamlFile(..))
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Log
    ( LogConfig(..), LogBackend(..), LogEntry(..), LogLevel(..)
    , LoggerState, defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.TransferApi (evalDebug, newBareLuaBackend)
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Sim.Types
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Command.Lifecycle (handleUnitTeleportCommand)
import Unit.Thread.Command.Motion
    (handleUnitMoveToCommand, handleUnitSetMoveSpeedCommand)
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))

-----------------------------------------------------------------------
-- Fixture
-----------------------------------------------------------------------

-- | The one unit every example drives.
movedUnit ∷ Integer
movedUnit = 4242

movedUid ∷ UnitId
movedUid = UnitId (fromIntegral movedUnit)

-- | Where the unit starts, and the speed of the move it starts with.
--   Deliberately not (0, 0): the pre-fix behaviour substituted @0.0@
--   for a missing coordinate, so a fixture sitting at the origin could
--   not tell a refused teleport from an accepted one.
startX, startY ∷ Float
startX = 3.5
startY = 6.5

startZ ∷ Int
startZ = 2

startSpeed ∷ Float
startSpeed = 0.75

-- | Where an ACCEPTED call sends it. Different from the start on every
--   axis, so any accepted call is unambiguously visible.
goalX, goalY ∷ Float
goalX = 12.5
goalY = 9.25

goalZ ∷ Int
goalZ = 7

goalSpeed ∷ Float
goalSpeed = 1.5

-- | A def with one body part (so 'injurySpeedMult' has something to
--   walk) and a run threshold high enough that 'goalSpeed' stays a
--   walk — the gait is not what this gate is about.
motionDef ∷ UnitDef
motionDef = UnitDef
    { udName = "motion_arg_unit", udNamePool = Nothing
    , udDisplayName = Just "motion arg unit"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 3.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | An uninjured instance at a FULL blood pool, so 'injurySpeedMult'
--   is exactly 1.0 and an accepted speed reaches the sim state
--   unscaled.
--
--   The blood figure is not arbitrary: that multiplier's blood term is
--   @0.5 + 0.5 × (uiBlood \/ (body_mass × bloodMassRatio))@, and with
--   no @body_mass@ stat the default 70 kg gives a 5.25 L pool. At
--   anything less than full, every speed assertion below would be
--   measuring the wound model rather than the argument boundary.
motionInstance ∷ UnitInstance
motionInstance = UnitInstance
    { uiDefName = "motion_arg_unit", uiName = ""
    , uiPage = motionPage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0
    , uiGridX = startX, uiGridY = startY
    , uiGridZ = startZ, uiRealZ = fromIntegral startZ, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.25, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | The page the instance names. Never loaded: every accepted teleport
--   below supplies an explicit Z, so the surface lookup is not on any
--   path here and needs no terrain.
motionPage ∷ WorldPageId
motionPage = WorldPageId "motion_arg_page"

-- | A standing unit with an in-flight move — the state
--   @unit.setMoveSpeed@ retargets, and the one a refused call must
--   leave alone.
movingSimState ∷ UnitSimState
movingSimState = UnitSimState
    { usRealX = startX, usRealY = startY
    , usGridZ = startZ, usRealZ = fromIntegral startZ
    , usTarget = Just MoveTarget
        { mtTargetX = startX + 1, mtTargetY = startY + 1
        , mtSpeed = startSpeed, mtHazard = FallPermitted }
    , usPose = Standing, usState = Walking, usFacing = DirE
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

-- | Install the def, the instance and one sim state, returning the sim
--   ref the handlers write. Called per example so no example can
--   inherit another's mutation.
resetFixture ∷ EngineEnv → IO (IORef UnitThreadState)
resetFixture env = do
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "motion_arg_unit" motionDef
        , umInstances = HM.singleton movedUid motionInstance }
    _ ← Q.flushQueue (ucUnitQueue (toUnitCombatCapability env))
    newIORef emptyUnitThreadState
        { utsSimStates = HM.singleton movedUid movingSimState }

-----------------------------------------------------------------------
-- Driving one call
-----------------------------------------------------------------------

-- | Everything one Lua motion call did: what it returned, what it
--   warned, what it queued, and what the simulation looked like once
--   the REAL dispatcher had applied whatever was queued.
data Outcome = Outcome
    { oReturned ∷ Text
    , oWarnings ∷ [Text]
    , oQueued   ∷ [UnitCommand]
    , oSim      ∷ Maybe UnitSimState
    }

-- | Drive @return <expr>@ through the registered API and observe all of
--   it.
--
--   The queue is drained to observe it and then REFILLED with exactly
--   what was drained, so \"nothing was queued\" and \"the simulation did
--   not move\" stay two independent observations of one run rather than
--   one restated: a refusal that leaked a command would be caught by
--   the queue assertion, and a refusal that leaked a MUTATION (a future
--   verb that wrote the manager before checking) by the sim assertion.
--
--   The engine's logger is swapped for a capturing one only across the
--   Lua call and the dispatch, since both read @loggerRef@ at the
--   moment they warn.
runCall ∷ EngineEnv → LuaBackendState → Text → Text → IO Outcome
runCall env ls prefix expr = do
    utsRef ← resetFixture env
    let queue = ucUnitQueue (toUnitCombatCapability env)
    entriesRef ← newIORef []
    captured ← capturingLogger entriesRef
    (returned, queued) ←
        bracket (swapLogger env captured) (swapLogger env) $ \_ → do
            r ← evalDebug ls ("return " <> expr)
            q ← Q.flushQueue queue
            mapM_ (Q.writeQueue queue) q
            -- The drain answers whether it stopped at a session
            -- boundary (#2291). No motion verb may ever queue one, so
            -- this is asserted rather than discarded: a `True` here
            -- would mean the pass stopped early and every "the
            -- simulation did not move" assertion below had simply not
            -- run the commands yet.
            endedSession ← processAllUnitCommands env utsRef
            endedSession `shouldBe` False
            pure (r, q)
    warns ← warningsWith prefix entriesRef
    sim ← HM.lookup movedUid ∘ utsSimStates <$> readIORef utsRef
    pure Outcome { oReturned = returned, oWarnings = warns
                 , oQueued = queued, oSim = sim }

capturingLogger ∷ IORef [LogEntry] → IO LoggerState
capturingLogger entriesRef = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }

-- | Install @logger@ and return the one it replaced, so 'bracket' can
--   put the original back.
swapLogger ∷ EngineEnv → LoggerState → IO LoggerState
swapLogger env logger = do
    previous ← readIORef (loggerRef env)
    writeIORef (loggerRef env) logger
    pure previous

-- | Warnings whose message starts with @prefix@. Keyed on the prefix
--   rather than on \"the only warning captured\" because the harness may
--   log warnings of its own at any moment, and a bare count would make
--   this spec fail for something it is not testing.
warningsWith ∷ Text → IORef [LogEntry] → IO [Text]
warningsWith prefix entriesRef =
    filter (prefix `T.isPrefixOf`)
        ∘ reverse ∘ map leMessage ∘ filter ((≡ LevelWarn) ∘ leLevel)
        <$> readIORef entriesRef

-- | The whole refusal contract in one assertion: false, exactly one
--   warning naming the verb AND the offending argument, an EMPTY
--   queue, and a simulation still in its fixture state.
refused ∷ Text → Text → Outcome → Expectation
refused verb argName o = do
    oReturned o `shouldBe` "false"
    case oWarnings o of
        [w] → do
            w `shouldSatisfy` T.isInfixOf ("'" <> argName <> "'")
            w `shouldSatisfy` T.isInfixOf verb
        ws  → expectationFailure
                ("expected one warning, got " ⧺ show ws)
    map commandTag (oQueued o) `shouldBe` []
    oSim o `shouldBe` Just movingSimState

-- | A queued command's constructor name, so an unexpected enqueue is
--   reported by what it was rather than by a 'Show' of the whole
--   payload ('UnitCommand' has no 'Eq').
commandTag ∷ UnitCommand → Text
commandTag = T.takeWhile (≢ ' ') ∘ tshow

-----------------------------------------------------------------------
-- The Lua expressions each group drives
-----------------------------------------------------------------------

setPos ∷ Text → Text
setPos args = "unit.setPos(" <> T.pack (show movedUnit) <> ", " <> args <> ")"

moveTo ∷ Text → Text
moveTo args = "unit.moveTo(" <> T.pack (show movedUnit) <> ", " <> args <> ")"

setMoveSpeed ∷ Text → Text
setMoveSpeed args =
    "unit.setMoveSpeed(" <> T.pack (show movedUnit) <> ", " <> args <> ")"

-- | Lua source for the numeric shapes every slot must refuse, paired
--   with the name each example is reported under.
--
--   @1e300@ is the post-narrowing case (requirement 4): a perfectly
--   ordinary finite Lua number that is @Infinity@ once it reaches the
--   32-bit 'Float' the command carries. A check written before the
--   narrowing would let exactly it through.
badNumbers ∷ [(String, Text)]
badNumbers =
    [ ("NaN",                    "0/0")
    , ("positive infinity",      "math.huge")
    , ("negative infinity",      "-math.huge")
    , ("a finite Float overflow", "1e300")
    , ("a string",               "\"5\"")
    , ("a table",                "{}")
    , ("an explicit nil",        "nil")
    ]

-----------------------------------------------------------------------
-- The spec
-----------------------------------------------------------------------

spec ∷ SpecWith EngineEnv
spec = describe "unit motion argument domains" $ do

    describe "unit.setPos" $ do

        forM_ badNumbers $ \(label, lua) → do
            it ("refuses " ⧺ label ⧺ " as x") $ \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.setPos:"
                        (setPos (lua <> ", 9.25, 7"))
                refused "unit.setPos" "x" o

            it ("refuses " ⧺ label ⧺ " as y") $ \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.setPos:"
                        (setPos ("12.5, " <> lua <> ", 7"))
                refused "unit.setPos" "y" o

        it "refuses a call with no coordinates at all" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.setPos:"
                    ("unit.setPos(" <> T.pack (show movedUnit) <> ")")
            refused "unit.setPos" "x" o

        it "refuses a call that supplied only x" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.setPos:" (setPos "12.5")
            refused "unit.setPos" "y" o

        -- The control. Without it every assertion above could pass on a
        -- fixture that never moves for any input.
        it "accepts a finite pair and teleports the unit" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.setPos:" (setPos "12.5, 9.25, 7")
            oReturned o `shouldBe` "true"
            oWarnings o `shouldBe` []
            map commandTag (oQueued o) `shouldBe` ["UnitTeleport"]
            fmap usRealX (oSim o) `shouldBe` Just goalX
            fmap usRealY (oSim o) `shouldBe` Just goalY
            fmap usGridZ (oSim o) `shouldBe` Just goalZ

        -- Requirement 1's Z carve-out: Z is optional by design and
        -- 'Lua.tointeger' cannot produce a non-finite Int, so an
        -- omitted Z is still a surface lookup and not a refusal.
        it "still accepts an omitted z" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.setPos:" (setPos "12.5, 9.25")
            oReturned o `shouldBe` "true"
            oWarnings o `shouldBe` []
            map commandTag (oQueued o) `shouldBe` ["UnitTeleport"]
            fmap usRealX (oSim o) `shouldBe` Just goalX

    describe "unit.moveTo" $ do

        forM_ badNumbers $ \(label, lua) → do
            it ("refuses " ⧺ label ⧺ " as x") $ \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.moveTo:"
                        (moveTo (lua <> ", 9.25, 1.5"))
                refused "unit.moveTo" "x" o

            it ("refuses " ⧺ label ⧺ " as y") $ \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.moveTo:"
                        (moveTo ("12.5, " <> lua <> ", 1.5"))
                refused "unit.moveTo" "y" o

        -- The speed slot's own list is SHORTER by one: an explicit nil
        -- there is an omission, which keeps the default (below).
        forM_ (filter ((≢ "an explicit nil") ∘ fst) badNumbers) $
            \(label, lua) →
                it ("refuses " ⧺ label ⧺ " as speed") $ \env → do
                    ls ← newBareLuaBackend env
                    o ← runCall env ls "unit.moveTo:"
                            (moveTo ("12.5, 9.25, " <> lua))
                    refused "unit.moveTo" "speed" o

        it "refuses a negative speed" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.moveTo:" (moveTo "12.5, 9.25, -1.5")
            refused "unit.moveTo" "speed" o

        -- A call that is wrong in two ways names the argument, not the
        -- token: the numeric domains are checked before the hazard.
        it "names the coordinate when the hazard token is also bad" $
            \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.moveTo:"
                        (moveTo "0/0, 9.25, 1.5, \"nonsense\"")
                refused "unit.moveTo" "x" o

        it "accepts a finite target and installs it" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.moveTo:" (moveTo "12.5, 9.25, 1.5")
            oReturned o `shouldBe` "true"
            oWarnings o `shouldBe` []
            map commandTag (oQueued o) `shouldBe` ["UnitMoveTo"]
            fmap (fmap mtTargetX ∘ usTarget) (oSim o)
                `shouldBe` Just (Just goalX)
            fmap (fmap mtTargetY ∘ usTarget) (oSim o)
                `shouldBe` Just (Just goalY)
            fmap (fmap mtSpeed ∘ usTarget) (oSim o)
                `shouldBe` Just (Just goalSpeed)

        it "accepts a zero speed" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.moveTo:" (moveTo "12.5, 9.25, 0")
            oReturned o `shouldBe` "true"
            oWarnings o `shouldBe` []
            fmap (fmap mtSpeed ∘ usTarget) (oSim o) `shouldBe` Just (Just 0)

        it "keeps the 2.0 default for an omitted speed" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.moveTo:" (moveTo "12.5, 9.25")
            oReturned o `shouldBe` "true"
            oWarnings o `shouldBe` []
            fmap (fmap mtSpeed ∘ usTarget) (oSim o) `shouldBe` Just (Just 2.0)

        -- The review's parsing clarification: an explicit nil in the
        -- optional slot is an OMISSION, not a malformed value, even
        -- with a later argument present to prove the slot was reached.
        it "keeps that default for an explicit nil speed" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.moveTo:"
                    (moveTo "12.5, 9.25, nil, \"avoid_falls\"")
            oReturned o `shouldBe` "true"
            oWarnings o `shouldBe` []
            fmap (fmap mtSpeed ∘ usTarget) (oSim o) `shouldBe` Just (Just 2.0)
            fmap (fmap mtHazard ∘ usTarget) (oSim o)
                `shouldBe` Just (Just FallProhibited)

    describe "unit.setMoveSpeed" $ do

        forM_ badNumbers $ \(label, lua) →
            it ("refuses " ⧺ label) $ \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.setMoveSpeed:" (setMoveSpeed lua)
                refused "unit.setMoveSpeed" "speed" o

        it "refuses a negative speed" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.setMoveSpeed:" (setMoveSpeed "-0.5")
            refused "unit.setMoveSpeed" "speed" o

        it "refuses a call with no speed at all" $ \env → do
            ls ← newBareLuaBackend env
            o ← runCall env ls "unit.setMoveSpeed:"
                    ("unit.setMoveSpeed(" <> T.pack (show movedUnit) <> ")")
            refused "unit.setMoveSpeed" "speed" o

        it "accepts a finite speed and retargets the in-flight move" $
            \env → do
                ls ← newBareLuaBackend env
                o ← runCall env ls "unit.setMoveSpeed:" (setMoveSpeed "1.5")
                oReturned o `shouldBe` "true"
                oWarnings o `shouldBe` []
                map commandTag (oQueued o) `shouldBe` ["UnitSetMoveSpeed"]
                fmap (fmap mtSpeed ∘ usTarget) (oSim o)
                    `shouldBe` Just (Just goalSpeed)
                -- and leaves the destination alone, as documented.
                fmap (fmap mtTargetX ∘ usTarget) (oSim o)
                    `shouldBe` Just (Just (startX + 1))

    -- Requirement 5. No Lua call can produce a malformed command any
    -- more, so the handlers are called directly — the only way to
    -- reach the defensive check at all.
    describe "the unit thread's defensive check" $ do

        let injecting env prefix act = do
                utsRef ← resetFixture env
                entriesRef ← newIORef []
                captured ← capturingLogger entriesRef
                _ ← bracket (swapLogger env captured) (swapLogger env) $
                        \_ → act utsRef
                warns ← warningsWith prefix entriesRef
                sim ← HM.lookup movedUid ∘ utsSimStates <$> readIORef utsRef
                pure (warns, sim)

            dropped (warns, sim) = do
                length warns `shouldBe` 1
                sim `shouldBe` Just movingSimState

        forM_ [("NaN", 0 / 0), ("infinity", 1 / 0)] $ \(label, bad) → do

            it ("drops a UnitTeleport whose x is " ⧺ label) $ \env → do
                r ← injecting env "UnitTeleport:" $ \utsRef →
                        handleUnitTeleportCommand env utsRef movedUid
                            bad goalY (Just goalZ)
                dropped r

            it ("drops a UnitTeleport whose y is " ⧺ label) $ \env → do
                r ← injecting env "UnitTeleport:" $ \utsRef →
                        handleUnitTeleportCommand env utsRef movedUid
                            goalX bad (Just goalZ)
                dropped r

            it ("drops a UnitMoveTo whose target is " ⧺ label) $ \env → do
                r ← injecting env "UnitMoveTo:" $ \utsRef →
                        handleUnitMoveToCommand env utsRef movedUid
                            bad goalY goalSpeed FallPermitted
                dropped r

            it ("drops a UnitMoveTo whose speed is " ⧺ label) $ \env → do
                r ← injecting env "UnitMoveTo:" $ \utsRef →
                        handleUnitMoveToCommand env utsRef movedUid
                            goalX goalY bad FallPermitted
                dropped r

            it ("drops a UnitSetMoveSpeed whose speed is " ⧺ label) $
                \env → do
                    r ← injecting env "UnitSetMoveSpeed:" $ \utsRef →
                            handleUnitSetMoveSpeedCommand env utsRef
                                movedUid bad
                    dropped r

        -- The review's addition: the worker's domain is the INGRESS
        -- domain, so a negative speed is dropped here too, not only a
        -- non-finite one.
        it "drops a UnitMoveTo whose speed is negative" $ \env → do
            r ← injecting env "UnitMoveTo:" $ \utsRef →
                    handleUnitMoveToCommand env utsRef movedUid
                        goalX goalY (-1.5) FallPermitted
            dropped r

        it "drops a UnitSetMoveSpeed whose speed is negative" $ \env → do
            r ← injecting env "UnitSetMoveSpeed:" $ \utsRef →
                    handleUnitSetMoveSpeedCommand env utsRef movedUid (-1.5)
            dropped r

        it "installs an in-domain UnitTeleport" $ \env → do
            (warns, sim) ← injecting env "UnitTeleport:" $ \utsRef →
                    handleUnitTeleportCommand env utsRef movedUid
                        goalX goalY (Just goalZ)
            warns `shouldBe` []
            fmap usRealX sim `shouldBe` Just goalX
            fmap usGridZ sim `shouldBe` Just goalZ

        it "installs an in-domain UnitMoveTo" $ \env → do
            (warns, sim) ← injecting env "UnitMoveTo:" $ \utsRef →
                    handleUnitMoveToCommand env utsRef movedUid
                        goalX goalY goalSpeed FallPermitted
            warns `shouldBe` []
            fmap (fmap mtTargetX ∘ usTarget) sim `shouldBe` Just (Just goalX)
            fmap (fmap mtSpeed ∘ usTarget) sim `shouldBe` Just (Just goalSpeed)

        it "installs an in-domain UnitSetMoveSpeed" $ \env → do
            (warns, sim) ← injecting env "UnitSetMoveSpeed:" $ \utsRef →
                    handleUnitSetMoveSpeedCommand env utsRef movedUid goalSpeed
            warns `shouldBe` []
            fmap (fmap mtSpeed ∘ usTarget) sim `shouldBe` Just (Just goalSpeed)

    -- Requirement 6. Engine-free: the decoder is the whole subject.
    describe "the max_speed decoder" $ do

        let decodeUnit ∷ Text → Either String UnitYamlDef
            decodeUnit body = case Yaml.decodeEither' (TE.encodeUtf8 body) of
                Left err → Left (show err)
                Right d  → Right d

            unitYaml ∷ Text → Text
            unitYaml field = T.unlines
                [ "name: motion_arg_unit"
                , "sprite: units/none.png"
                , field ]

            rejects ∷ Text → Text → Expectation
            rejects field expected = case decodeUnit (unitYaml field) of
                Right d → expectationFailure
                    ("expected a decode failure, got max_speed "
                     ⧺ show (uydMaxSpeed d))
                Left err → do
                    err `shouldSatisfy` isInfixOfStr "motion_arg_unit"
                    err `shouldSatisfy` isInfixOfStr "max_speed"
                    err `shouldSatisfy` isInfixOfStr (T.unpack expected)

            isInfixOfStr needle = T.isInfixOf (T.pack needle) ∘ T.pack

        it "rejects zero" $ \_ → rejects "max_speed: 0" "strictly positive"
        it "rejects a negative speed" $ \_ →
            rejects "max_speed: -1.4" "strictly positive"

        -- YAML's .nan / .inf resolve to STRINGS, so these take the
        -- not-a-number branch rather than the finiteness one — which is
        -- exactly why the parser takes a whole Value.
        it "rejects .nan" $ \_ → rejects "max_speed: .nan" "max_speed"
        it "rejects .inf" $ \_ → rejects "max_speed: .inf" "max_speed"
        it "rejects -.inf" $ \_ → rejects "max_speed: -.inf" "max_speed"

        -- Requirement 4's post-narrowing rule, on the YAML side: a
        -- valid Scientific that is Infinity once stored as a Float.
        it "rejects a finite value that overflows Float" $ \_ →
            rejects "max_speed: 1.0e+100" "finite"

        it "rejects a non-numeric value" $ \_ →
            rejects "max_speed: fast" "max_speed"

        -- An explicitly written null is a value the author supplied,
        -- not an omission — and Aeson's `.:?` reports the two
        -- identically, so an absence test written with it would hand
        -- this the 3.0 default. All three YAML spellings, since they
        -- all resolve to the same Null and a lookup that missed one
        -- would reopen the hole.
        forM_ ["null", "~", ""] $ \spelling →
            it ("rejects an explicit null written as " ⧺ show spelling) $
                \_ → rejects ("max_speed: " <> spelling) "max_speed"

        it "keeps the 3.0 default when the key is absent" $ \_ →
            fmap uydMaxSpeed (decodeUnit (unitYaml "display_name: x"))
                `shouldBe` Right 3.0

        it "accepts an ordinary positive speed" $ \_ →
            fmap uydMaxSpeed (decodeUnit (unitYaml "max_speed: 4.5"))
                `shouldBe` Right 4.5

        -- Requirement 6's other half: the rule must not cost the
        -- shipped data. Enumerated from disk rather than listed here,
        -- so a unit file added or edited later is checked too.
        it "loads every shipped unit unchanged" $ \_ → do
            names ← listDirectory unitDataDir
            let paths = [ unitDataDir ⊘ n
                        | n ← names, takeExtension n ≡ ".yaml" ]
            paths `shouldSatisfy` (not ∘ null)
            forM_ paths $ \path → do
                loaded ← Yaml.decodeFileEither path
                case loaded ∷ Either Yaml.ParseException UnitYamlFile of
                    Left err → expectationFailure (path ⧺ ": " ⧺ show err)
                    Right f  → forM_ (uyfUnits f) $ \d →
                        (path, uydName d, uydMaxSpeed d)
                            `shouldSatisfy` \(_, _, sp) →
                                sp > 0 ∧ not (isNaN sp) ∧ not (isInfinite sp)

-- | Where the shipped unit definitions live, relative to the resource
--   root every headless example already runs in.
unitDataDir ∷ FilePath
unitDataDir = "data/units"
