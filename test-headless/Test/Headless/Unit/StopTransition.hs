{-# LANGUAGE Strict #-}
-- | Regression coverage for #1709: a @UnitStop@ that lands while a unit
--   is mid-POSE-TRANSITION must not strand it above its grid layer.
--
--   The defect. A mental break (@scripts/mental_state.lua@'s
--   @enterBreak@) calls @unit.stop@ from the physiology tick, which has
--   no mid-transition guard — unlike @scripts/unit_ai.lua@'s @tickOne@,
--   through which every other @unit.stop@ caller reaches the command.
--   'handleUnitStopCommand' used to clear @usState@ and
--   @usTransitionUntil@ unconditionally, and BOTH functions that could
--   finish an in-flight arc match on exactly that state
--   ('Unit.Thread.Movement.Fall.tickFallZ' and
--   'Unit.Thread.Movement.Timers.handleTransitionExpiry', whose landing
--   snap is the only thing that reconciles the continuous position with
--   the grid). A break entered mid-leap therefore produced an @Idle@
--   unit frozen at an interpolated @usRealZ@ — rendered floating for the
--   rest of the episode, and round-tripped verbatim by the @unit-sim@
--   save component.
--
--   Everything here is PURE, driven through the production per-unit tick
--   ('Unit.Thread.Movement.PathAdvance.tickUnit') and the production stop
--   ('Unit.Thread.Command.Motion.stopUnitSimState'), so it asserts on the
--   authoritative unit-thread state rather than on the render mirror a
--   Lua caller would observe. @tickUnit@ never consults terrain on the
--   transitioning / target-less paths these cases take, so the inert
--   'noWorld' below is the whole world it needs.
--
--   The engine-level end-to-end proof (a real leap through @unit.jump@,
--   a real forced break, and the observed @realZ@/@gridZ@ agreement)
--   lives in @tools/mental_state_probe.py@'s catatonia stage.
module Test.Headless.Unit.StopTransition (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Building.Knowledge (emptyContainerKnowledge)
import Craft.Bills (emptyCraftBills)
import Item.Ground (emptyGroundItems)
import Power.Types (emptyPowerNodes)
import Unit.Direction (Direction(..))
import Unit.Sim.Types
    ( UnitSimState(..), MoveTarget(..), Pose(..), UnitActivity(..)
    , MoveHazardPolicy(..) )
import Unit.Thread.Command.Motion (stopUnitSimState)
import Unit.Thread.Movement.Fall (startFall)
import Unit.Thread.Movement.Leap (startJump)
import Unit.Thread.Movement.PathAdvance (tickUnit, MoveWorld(..))
import Unit.Thread.Movement.Types (defaultMoveStats)
import Unit.Pathing.Config (defaultPathingConfig)
import Unit.Types (UnitId(..))
import World.Edit.Types (emptyWorldEdits)
import World.Flora.CropPlot (emptyCropPlots)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Generate.Types (WorldGenParams, defaultWorldGenParams)
import World.Material (emptyMaterialRegistry)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Save.Component.Entities
    ( unitSimCodec, UnitSimDTO(..), PageSimDTO(..), UnitSimStateDTO(..) )
import World.Save.Component.Types
    (ComponentCodec(..), renderComponentError)
import World.Save.Envelope (encodeSessionSnapshot, decodeSessionEnvelope)
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Reference (SamePageRef(..))
import World.Save.Snapshot
    ( SessionSnapshot(..), SessionGlobals(..), PageSnapshot(..)
    , LiveCameraSnapshot(..), captureSessionSnapshot )
import World.Save.Types
    (BuildingSnapshot(..), UnitSnapshot(..), UnitInstanceSnapshot(..))
import World.Spoil.Types (emptySpoilPiles)
import Unit.Transfer.Orders (emptyTransferOrders)
import qualified Data.Serialize as S
import World.Construct.Attempt (firstConstructAttemptId)
import World.Flora.Identity (firstPlantedFloraCursor)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

-- ---------------------------------------------------------------------
-- Pure tick harness
-- ---------------------------------------------------------------------

-- | Tick length. The unit thread's own cadence is irrelevant to the
--   invariants here (they hold at every sample), so this just needs to
--   be fine enough to land several samples inside each arc.
dt ∷ Double
dt = 1 / 60

-- | Arbitrary non-zero start time. Non-zero on purpose: every deadline
--   in a sim state is an ABSOLUTE game time, so a zero origin would let
--   a "forgot to add now" bug pass.
t0 ∷ Double
t0 = 1000

-- | No terrain, flagged as the mover's own page. 'tickUnit' returns
--   before reading either field for a transitioning unit and for an
--   Idle one with no target, which is every state these cases reach.
noWorld ∷ MoveWorld
noWorld = MoveWorld { mwTiles = Nothing, mwOwnPage = True }

-- | One production tick: advance game time, then run the real per-unit
--   movement tick against it.
step ∷ (Double, UnitSimState) → (Double, UnitSimState)
step (t, s) =
    let t' = t + dt
    in ( t'
       , tickUnit defaultPathingConfig emptyMaterialRegistry t' dt
                  noWorld defaultMoveStats s )

-- | @n@ ticks of trajectory from @(t0, s)@, sample 0 being the input.
trajectory ∷ Int → UnitSimState → [(Double, UnitSimState)]
trajectory n s = take (n + 1) (iterate step (t0, s))

-- | Long enough for a 2-tile leap (~1.16 s of flight) or a 3-z fall
--   (~0.96 s) PLUS its 0.8 s chained landing step, with slack.
settleTicks ∷ Int
settleTicks = 300

-- | Run to a settled state and return it.
settle ∷ UnitSimState → UnitSimState
settle s = snd (last (trajectory settleTicks s))

-- | Settle from an explicit clock — for a state restored from a save,
--   whose absolute transition deadline is only meaningful against the
--   game time the save carried.
settleFrom ∷ Double → UnitSimState → UnitSimState
settleFrom t s = snd (last (take (settleTicks + 1) (iterate step (t, s))))

isTransition ∷ UnitActivity → Bool
isTransition (TransitioningTo _) = True
isTransition _                   = False

-- | The invariant #1709 is about, stated once: a unit whose activity is
--   not a transition carries no interpolated position and no leftover
--   transition machinery.
groundedAndClean ∷ UnitSimState → Expectation
groundedAndClean s = do
    usRealZ s          `shouldBe` fromIntegral (usGridZ s)
    usTransitionUntil s `shouldBe` Nothing
    usPostTransition s  `shouldBe` []
    usFallFromTile s    `shouldBe` Nothing
    usFallToTile s      `shouldBe` Nothing
    usJumpApex s        `shouldBe` Nothing

-- | The pre-#1709 stop: clear the transition machinery unconditionally.
--   Kept here ONLY to prove the fixtures below really reproduce the
--   defect — if a future refactor made the arcs land some other way,
--   this would stop stranding the unit and the "reproduces" case fails
--   loudly rather than the regression quietly testing nothing.
legacyStop ∷ UnitSimState → UnitSimState
legacyStop ss = ss { usTarget          = Nothing
                   , usState           = Idle
                   , usLocalPath       = []
                   , usDrinkUntil      = Nothing
                   , usEatUntil        = Nothing
                   , usPickupUntil     = Nothing
                   , usTransitionUntil = Nothing
                   , usGetUpAt         = Nothing
                   }

-- ---------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------

-- | A standing, idle unit on the centre of tile (0,0) at z = 4.
grounded ∷ UnitSimState
grounded = UnitSimState
    { usRealX = 0.5, usRealY = 0.5, usGridZ = 4, usRealZ = 4
    , usTarget = Nothing, usPose = Standing, usState = Idle, usFacing = DirS
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

-- | A 2-tile flat leap from (0,0) to (2,0) — the shape the issue's
--   headless repro used.
leaping ∷ UnitSimState
leaping = startJump t0 grounded 2 0

-- | A genuine 3-z fall off the same tile onto (0,3) at z = 1 — NOT a
--   leap (no @usJumpApex@), so it must keep its knockdown outcome.
falling ∷ UnitSimState
falling = startFall t0 grounded ((0, 3), 1) 4 (0, 1)

-- | The first sample that is unambiguously MID-ARC: transitioning, with
--   the continuous Z genuinely off the grid layer. Mirrors the probe's
--   pre-break coherence sample — a case that stopped before the arc left
--   the ground would prove nothing. Signed either way on purpose: a leap
--   arcs UP over its chord, a fall descends BELOW its still-unsnapped
--   @usGridZ@, and both are the same stranding hazard.
firstOffGrid ∷ UnitSimState → (Double, UnitSimState)
firstOffGrid s = case [ p | p@(_, u) ← trajectory settleTicks s
                          , isTransition (usState u)
                          , abs (usRealZ u - fromIntegral (usGridZ u)) > 0.05 ] of
    (p : _) → p
    []      → error "fixture never left its grid layer"

-- | The first sample inside the CHAINED landing transition — the leg
--   after the arc has already touched down and snapped, which starts a
--   second transition (Falling→Standing for a leap, Falling→Collapsed
--   for a fall) with its own timer.
firstChained ∷ UnitSimState → (Double, UnitSimState)
firstChained s = case [ p | p@(_, u) ← trajectory settleTicks s
                          , usState u ≢ TransitioningTo Falling
                          , isTransition (usState u) ] of
    (p : _) → p
    []      → error "fixture never chained a landing transition"

-- | Apply the stop at time @t@ and settle from there.
stopAt ∷ (Double, UnitSimState) → UnitSimState
stopAt (t, s) = snd (last (take (settleTicks + 1)
                                (iterate step (t, stopUnitSimState s))))

-- ---------------------------------------------------------------------
-- Persistence fixture: one page carrying one sim state
-- ---------------------------------------------------------------------

testPage ∷ WorldPageId
testPage = WorldPageId "stop-transition"

testUnit ∷ UnitId
testUnit = UnitId 1

-- | 'WorldGenParams' derives a few nested fields on decode rather than
--   storing them, so a hand-built default is only a serialize fixpoint
--   after one decode∘encode. Same reasoning as
--   "Test.Headless.World.Save.Components"' @canon@.
canonGP ∷ WorldGenParams
canonGP = case S.decode (S.encode defaultWorldGenParams) of
    Right gp → gp
    Left err → error ("canonGP: " ⧺ err)

-- | The owning unit instance. A sim state with no unit behind it is an
--   @OrphanedUnitSimState@ the snapshot validator rejects outright, so
--   the page has to carry both halves for the encode to be reachable at
--   all. Only the identity matters here — the @unit-sim@ component reads
--   nothing off this record.
owningUnit ∷ UnitInstanceSnapshot
owningUnit = UnitInstanceSnapshot
    { uisDefName = "test_unit", uisBaseWidth = 1
    , uisGridX = 0, uisGridY = 0, uisGridZ = 0
    , uisFacing = DirS, uisCurrentAnim = "", uisAnimStart = 0
    , uisAnimReverse = False, uisActivity = "idle", uisPose = "standing"
    , uisAnimStride = 0, uisStats = HM.empty, uisModifiers = HM.empty
    , uisSkills = HM.empty, uisKnowledge = HM.empty, uisInventory = []
    , uisEquipped = HM.empty, uisAccessories = [], uisFactionId = ""
    , uisWounds = [], uisScars = [], uisImmuneResponse = 0
    , uisImmunities = HM.empty, uisBlood = 5, uisName = "" }

pageWithSim ∷ UnitSimState → PageSnapshot
pageWithSim ss = PageSnapshot
    { pgsPageId       = testPage
    , pgsGenParams    = canonGP
    , pgsCameraX      = 0
    , pgsCameraY      = 0
    , pgsTimeHour     = 12
    , pgsTimeMinute   = 0
    , pgsDateYear     = 1
    , pgsDateMonth    = 1
    , pgsDateDay      = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsConstructNextAttempt = firstConstructAttemptId
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty
                                         , bsnNextId = 10 }
    , pgsUnits        = UnitSnapshot
        { usnInstances = HM.singleton testUnit owningUnit
        , usnNextId = 10 }
    , pgsUnitSimStates = HM.singleton testUnit ss
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.empty
    , pgsPendingChopMigration = HM.empty
    , pgsPendingFloraHarvests = HM.empty
    , pgsPlantedFloraCursor = firstPlantedFloraCursor
    , pgsCraftBills   = emptyCraftBills
    , pgsTransferOrders = emptyTransferOrders
    , pgsPowerNodes   = emptyPowerNodes
    , pgsTillDesignations = HM.empty
    , pgsCropPlots    = emptyCropPlots
    , pgsPlantDesignations = HM.empty
    , pgsContainerKnowledge = emptyContainerKnowledge
    , pgsIdentity     = Nothing
    , pgsGeneratedId  = Just (fixtureGeneratedWorldIdForPage testPage)
    }

-- | Session globals stamped with the game time the save was taken at.
--   Load restores this clock, and every deadline in a sim state is an
--   ABSOLUTE game time, so an in-flight transition only resumes
--   correctly when the two travel together.
globalsAt ∷ Double → SessionGlobals
globalsAt now = SessionGlobals
    { sgGameTime       = now
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 10
    , sgNextUnitId     = 10
    , sgActivePage     = testPage
    , sgVisiblePages   = [testPage]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just testPage
        , lcsX = 0, lcsY = 0, lcsZoom = 1, lcsFacing = FaceEast }
    }

snapshotAt ∷ Double → UnitSimState → SessionSnapshot
snapshotAt now ss = case captureSessionSnapshot (globalsAt now) [pageWithSim ss] of
    Right snap → snap
    Left errs  → error ("snapshotAt invalid: " ⧺ show errs)

snapshotOf ∷ UnitSimState → SessionSnapshot
snapshotOf = snapshotAt t0

-- | Round-trip one sim state through the REAL @unit-sim@ save component
--   — the same codec a save writes and a load reads — and hand back the
--   decoded wire record. Requirement 6 is about not WRITING an
--   inconsistent state, so the assertion belongs on what actually
--   reaches (and comes back off) disk.
roundTripSim ∷ UnitSimState → IO UnitSimStateDTO
roundTripSim ss = do
    let bytes = ccEncode unitSimCodec (snapshotOf ss)
    case ccDecode unitSimCodec (ccVersion unitSimCodec) bytes of
        Left e → fail ("unit-sim decode failed: "
                       ⧺ T.unpack (renderComponentError e))
        Right (UnitSimDTO pages) →
            case [ d | p ← pages
                     , psPageId p ≡ testPage
                     , Just d ← [HM.lookup (SamePageRef testUnit) (psSim p)] ] of
                (d : _) → pure d
                []      → fail "unit-sim payload carried no sim state"

-- | A REAL save and reload of the whole session: the production
--   envelope encoder, then the production decoder, handing back the
--   restored game clock and the restored sim state. Unlike
--   'roundTripSim' (one component's bytes) this is the path
--   @engine.saveWorld@ / @engine.loadSave@ actually take.
saveAndReload ∷ Double → UnitSimState → IO (Double, UnitSimState)
saveAndReload now ss = do
    let snap = snapshotAt now ss
        meta = snapshotSaveMetadata
                 (SaveRequestMeta { srmSlotName = "stop-transition"
                                  , srmTimestamp = "ts"
                                  , srmAutosave = False })
                 snap
        bytes = encodeSessionSnapshot meta snap []
    case decodeSessionEnvelope HS.empty HS.empty bytes of
        Left err → fail ("save reload failed: " ⧺ T.unpack err)
        Right (_meta, restored, _lua, _migrated) →
            case HM.lookup testPage (snapPages restored)
                     ⌦ HM.lookup testUnit . pgsUnitSimStates of
                Just back → pure (snapGameTime restored, back)
                Nothing   → fail "reloaded session carried no sim state"

-- ---------------------------------------------------------------------

spec ∷ Spec
spec = do
    describe "the fixtures really reproduce #1709" $ do
        it "a leap is genuinely airborne mid-arc: transitioning, apex \
           \set, continuous Z above the grid layer" $ do
            let (_, air) = firstOffGrid leaping
            usState air `shouldBe` TransitioningTo Falling
            usJumpApex air `shouldSatisfy` isJust
            usRealZ air `shouldSatisfy` (> fromIntegral (usGridZ air))

        it "the PRE-FIX unconditional clear strands that leap forever — \
           \Idle, above its grid layer, with nothing left to finish it" $ do
            let (t, air) = firstOffGrid leaping
                stranded = snd (last (take (settleTicks + 1)
                                           (iterate step (t, legacyStop air))))
            usState stranded `shouldBe` Idle
            usRealZ stranded `shouldSatisfy` (> fromIntegral (usGridZ stranded))

    describe "a stop mid-leap defers to the arc" $ do
        it "keeps the transition machinery the arc still needs" $ do
            let (_, air)  = firstOffGrid leaping
                stopped   = stopUnitSimState air
            usState stopped           `shouldBe` usState air
            usTransitionUntil stopped `shouldBe` usTransitionUntil air
            usPostTransition stopped  `shouldBe` usPostTransition air
            usJumpApex stopped        `shouldBe` usJumpApex air
            usFallFromTile stopped    `shouldBe` usFallFromTile air
            usFallToTile stopped      `shouldBe` usFallToTile air
            usRealZ stopped           `shouldBe` usRealZ air

        it "still drops the move target, path and activity timers" $ do
            let (_, air) = firstOffGrid leaping
                loaded   = air { usTarget = Just (MoveTarget 9 9 1 FallPermitted)
                               , usLocalPath   = [(1, 1)]
                               , usDrinkUntil  = Just (t0 + 5)
                               , usEatUntil    = Just (t0 + 5)
                               , usPickupUntil = Just (t0 + 5) }
                stopped  = stopUnitSimState loaded
            usTarget stopped      `shouldBe` Nothing
            usLocalPath stopped   `shouldBe` []
            usDrinkUntil stopped  `shouldBe` Nothing
            usEatUntil stopped    `shouldBe` Nothing
            usPickupUntil stopped `shouldBe` Nothing

        it "lands on its feet, grounded, on the target tile, with no \
           \knockdown" $ do
            let landed = stopAt (firstOffGrid leaping)
            usState landed  `shouldBe` Idle
            usPose landed   `shouldBe` Standing
            usGridZ landed  `shouldBe` 4
            usRealX landed  `shouldBe` 2.5
            usRealY landed  `shouldBe` 0.5
            usPendingFallDrop landed `shouldBe` Nothing
            groundedAndClean landed

        it "lands identically whether or not the break stopped it" $
            stopAt (firstOffGrid leaping) `shouldBe` settle leaping

    describe "a stop during the CHAINED landing transition" $ do
        it "still reaches the grounded standing pose" $ do
            let (_, chained) = firstChained leaping
                landed       = stopAt (firstChained leaping)
            usState chained `shouldBe` TransitioningTo Standing
            usState landed  `shouldBe` Idle
            usPose landed   `shouldBe` Standing
            groundedAndClean landed

    describe "a genuine fall keeps its physics" $ do
        it "is not mistaken for a leap, and is genuinely mid-descent \
           \below its still-unsnapped grid layer" $ do
            let (_, air) = firstOffGrid falling
            usJumpApex falling `shouldBe` Nothing
            usState falling    `shouldBe` TransitioningTo Falling
            usJumpApex air     `shouldBe` Nothing
            usGridZ air        `shouldBe` 4
            usRealZ air `shouldSatisfy` (< fromIntegral (usGridZ air))

        it "still stamps its drop magnitude and chains the knockdown \
           \after a mid-air stop" $ do
            let (t, air)   = firstOffGrid falling
                afterStop  = iterate step (t, stopUnitSimState air)
                landedTick = case [ u | (_, u) ← take (settleTicks + 1) afterStop
                                      , isJust (usPendingFallDrop u) ] of
                    (u : _) → u
                    []      → error "fall never stamped usPendingFallDrop"
            -- The drop the Unit.Fall injury model is fed (4 → 1).
            usPendingFallDrop landedTick `shouldBe` Just 3
            usPostTransition landedTick  `shouldBe` []
            usState landedTick           `shouldBe` TransitioningTo Collapsed

        it "settles knocked down at the landing tile, grounded" $ do
            let landed = stopAt (firstOffGrid falling)
            usPose landed  `shouldBe` Collapsed
            usState landed `shouldBe` Idle
            usGridZ landed `shouldBe` 1
            usRealX landed `shouldBe` 0.5
            usRealY landed `shouldBe` 3.5
            usRealZ landed `shouldBe` 1
            usFallFromTile landed `shouldBe` Nothing
            usFallToTile landed   `shouldBe` Nothing
            usJumpApex landed     `shouldBe` Nothing

        it "reaches the same landing state as an uninterrupted fall" $
            -- usGetUpAt is the IO-side injury pass's to set
            -- (Unit.Thread.Movement), so both branches settle with it
            -- unset; what matters is that the stop changed nothing.
            stopAt (firstOffGrid falling) `shouldBe` settle falling

    describe "no stop, at any point of an arc, can produce a stale Idle" $
        it "holds at every tick of both a leap and a fall, and at every \
           \tick of the settle that follows each stop" $ do
            let arcs = [("leap", leaping), ("fall", falling)]
                checkOne label (t, s) =
                    let after = take (settleTicks + 1)
                                     (iterate step (t, stopUnitSimState s))
                    in [ (label, u) | (_, u) ← after
                                    , not (isTransition (usState u))
                                    , usRealZ u ≢ fromIntegral (usGridZ u)
                                      ∨ isJust (usTransitionUntil u)
                                      ∨ not (null (usPostTransition u))
                                      ∨ isJust (usFallFromTile u)
                                      ∨ isJust (usFallToTile u)
                                      ∨ isJust (usJumpApex u) ]
                bad = [ b
                      | (label, arc) ← arcs
                      , sample ← trajectory settleTicks arc
                      , b ← checkOne (label ∷ String) sample ]
            bad `shouldBe` []

    describe "ordinary preemption is untouched" $ do
        it "stops a walking unit on the spot" $ do
            let walking = grounded { usState  = Walking
                                   , usTarget = Just (MoveTarget 9 9 1
                                                                FallPermitted)
                                   , usLocalPath = [(1, 1), (2, 2)] }
                stopped = stopUnitSimState walking
            usState stopped     `shouldBe` Idle
            usTarget stopped    `shouldBe` Nothing
            usLocalPath stopped `shouldBe` []

        it "stops a running unit on the spot" $
            usState (stopUnitSimState grounded { usState = Running })
                `shouldBe` Idle

        it "clears the drink/eat/pickup deadlines of a stationary \
           \activity" $ do
            let busy = grounded { usState = Drinking
                                , usDrinkUntil  = Just (t0 + 3)
                                , usEatUntil    = Just (t0 + 3)
                                , usPickupUntil = Just (t0 + 3) }
                stopped = stopUnitSimState busy
            usState stopped       `shouldBe` Idle
            usDrinkUntil stopped  `shouldBe` Nothing
            usEatUntil stopped    `shouldBe` Nothing
            usPickupUntil stopped `shouldBe` Nothing

        it "still clears a get-up timer OUTSIDE a transition" $
            usGetUpAt (stopUnitSimState
                        grounded { usPose = Collapsed
                                 , usGetUpAt = Just (t0 + 2) })
                `shouldBe` Nothing

        it "keeps a get-up timer DURING one, so a landed fall still \
           \stands back up" $ do
            let (_, chained) = firstChained falling
                knocked = chained { usGetUpAt = Just (t0 + 2) }
            usGetUpAt (stopUnitSimState knocked) `shouldBe` Just (t0 + 2)

    describe "requirement 6: nothing inconsistent reaches the save" $ do
        it "a mid-leap break round-trips through the real unit-sim \
           \component as a grounded, non-transitioning unit" $ do
            let landed = stopAt (firstOffGrid leaping)
            d ← roundTripSim landed
            isTransition (simState d) `shouldBe` False
            simRealZ d `shouldBe` fromIntegral (simGridZ d)
            simTransitionUntil d `shouldBe` Nothing
            simPostTransition d  `shouldBe` []
            simFallFromTile d    `shouldBe` Nothing
            simFallToTile d      `shouldBe` Nothing
            simJumpApex d        `shouldBe` Nothing
            simPose d            `shouldBe` Standing

        it "the payload is non-empty, so the check above is not vacuous" $ do
            let bytes = ccEncode unitSimCodec (snapshotOf (settle leaping))
            BS.length bytes `shouldSatisfy` (> 0)

        it "a session saved WHILE the interrupted leap is still in flight \
           \reloads and finishes the arc, landing grounded and standing" $ do
            -- The save is taken at the instant the break lands, with the
            -- unit still airborne and still TransitioningTo Falling —
            -- the case requirement 6 is actually about. The settled
            -- round trip above covers the save taken afterwards.
            let (t, air)   = firstOffGrid leaping
                interrupted = stopUnitSimState air
            usState interrupted `shouldBe` TransitioningTo Falling
            usRealZ interrupted `shouldSatisfy`
                (> fromIntegral (usGridZ interrupted))
            (clock, back) ← saveAndReload t interrupted
            -- Nothing is lost or reconciled by the round trip itself:
            -- the arc comes back exactly as it went in, on its own clock.
            back  `shouldBe` interrupted
            clock `shouldBe` t
            let landed = settleFrom clock back
            usState landed `shouldBe` Idle
            usPose landed  `shouldBe` Standing
            usRealX landed `shouldBe` 2.5
            usRealY landed `shouldBe` 0.5
            groundedAndClean landed

        it "the reloaded in-flight leap settles exactly where an \
           \uninterrupted, never-saved one does" $ do
            let (t, air) = firstOffGrid leaping
            (clock, back) ← saveAndReload t (stopUnitSimState air)
            settleFrom clock back `shouldBe` settle leaping
