{-# LANGUAGE Strict #-}
-- | Commit-time admission for a queued strike (#2328).
--
--   `combat.attack` only ENQUEUES. The combat worker drains the queue
--   on its own tick, so every precondition the Lua AI checked at
--   admission is stale by the time "Combat.Resolution" commits. These
--   specs drive that exact window: queue a 'CombatAttack', mutate the
--   world underneath it, then run the REAL drain
--   ('Combat.Thread.processAllCommands') and grade what committed.
--
--   Against master every refusal case here resolves a normal swing —
--   a cross-page strike lands UNDODGEABLY (#797 zeroed cross-page
--   awareness without refusing the strike), a fled target is struck
--   from any distance (nothing ever read the separation), and a
--   second swing is spent against a stance floor.
--
--   The negative cases assert the FULL "mutating nothing" contract:
--   the named refusal event, no wound on the target, no attacker
--   stamina drain, no attacker stance drain, and no unit-kill command.
module Test.Headless.Combat.Admission (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Foldable (toList)
import Data.IORef (readIORef, writeIORef)
import qualified System.Random as Random
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Combat.Resolution.Admission
    ( AttackRefusal(..), refusalReason, attackRangeTiles
    , noHeightAttackRange, chebyshevSeparation, attackerStance
    , checkAdmission )
import Combat.Resolution.Constants (stanceAttackCost)
import Combat.Thread (processAllCommands)
import Combat.Types (AttackMode(..), CombatCommand(..), CombatEvent(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldState(..), emptyWorldState, WorldManager(..), emptyWorldManager)
import World.Tile.Types (WorldTileData(..))
import World.Time.Types (WorldTime(..))
import Structure.Types (emptyChunkStructures)

-- ---- Fixture ------------------------------------------------------

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "admission_a"
pageB = WorldPageId "admission_b"

attackerUid, targetUid ∷ Word32
attackerUid = 1
targetUid   = 2

-- | The attacker's height, in metres. With no equipped weapon and no
--   natural weapon the blade term is 0, so its live attack range is
--   exactly @attackerHeight / 2.4@ — spelled through 'attackRangeTiles'
--   rather than baked in as a literal so a change to the formula moves
--   the fixture with it instead of silently making these cases vacuous.
--
--   1.2 m specifically: 2.4 is exactly twice it, so the quotient is
--   0.5 with no rounding. That matters only for the ON-the-bound case,
--   which compares the separation against the range for EQUALITY —
--   at 1.8 m the compile-time constant fold of the fixture's own
--   @1.8 / 2.4@ lands on 0.75 while the engine's runtime Float
--   division lands on 0.74999994, and the case would fail on an
--   artefact of the fixture rather than on the contract.
attackerHeight ∷ Float
attackerHeight = 1.2

baseRange ∷ Float
baseRange = attackRangeTiles attackerHeight 0

-- Flat terrain, so nothing in the resolvable case is refused for a
-- reason these specs are not about (LOS is only read for the dodge
-- save, which needs a page to resolve at all).
flatChunk ∷ LoadedChunk
flatChunk =
    let area = chunkSize * chunkSize
        v = VU.replicate area 5
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.empty
        , lcSurfaceMap = v, lcTerrainSurfaceMap = v
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

setupPages ∷ IO WorldManager
setupPages = do
    wsA ← emptyWorldState
    writeIORef (wsTilesRef wsA)
        (WorldTileData (HM.singleton (ChunkCoord 0 0) flatChunk) 1)
    writeIORef (wsTimeRef wsA) (WorldTime 12 0)
    writeIORef (wsGenParamsRef wsA)
        (Just defaultWorldGenParams { wgpWorldSize = 4 })
    wsB ← emptyWorldState
    writeIORef (wsTilesRef wsB)
        (WorldTileData (HM.singleton (ChunkCoord 0 0) flatChunk) 1)
    writeIORef (wsTimeRef wsB) (WorldTime 12 0)
    writeIORef (wsGenParamsRef wsB)
        (Just defaultWorldGenParams { wgpWorldSize = 4 })
    pure (emptyWorldManager
        { wmWorlds = [(pageA, wsA), (pageB, wsB)]
        , wmVisible = [pageA, pageB] })

-- A one-part body, targetable at the attacker's own strike height so
-- the vertical reach band inside runResolution always has a candidate.
testDef ∷ UnitDef
testDef = UnitDef
    { udName = "admission_dummy", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing, udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
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
            , bpBleedFactor = 1.0, bpHeightLow = 0.5, bpHeightHigh = 1.5
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | Starting stamina and stance every fixture unit carries. Both are
--   read back after the drain: a refusal must leave them EXACTLY here.
startStamina, startStance ∷ Float
startStamina = 100.0
startStance  = 1.0

testUnit ∷ WorldPageId → Float → Float → Float → UnitInstance
testUnit page gx gy stance = UnitInstance
    { uiDefName = "admission_dummy", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 5
    , uiRealZ = 5, uiFacing = DirE
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList
        [ ("height", attackerHeight), ("body_mass", 70)
        , ("constitution", 1.0), ("strength", 1.0), ("endurance", 1.0)
        , ("stamina", startStamina), ("stance", stance) ]
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 100, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing }

-- ---- Driving one queued strike through the real drain --------------

-- | What the drain left behind, read back off the live engine.
data Outcome = Outcome
    { oEvents  ∷ [CombatEvent]
    , oStamina ∷ Float
    , oStance  ∷ Float
    , oWounds  ∷ Int
    , oKills   ∷ Int
    } deriving (Show)

-- | Queue one strike against @attacker@/@target@ and run the REAL
--   drain. The instances handed in are the ones present at COMMIT —
--   that is the whole point: a caller passes the post-mutation world
--   to model the AI having admitted the swing against an earlier one.
drive
    ∷ EngineEnv → AttackMode → Float
    → UnitInstance → UnitInstance → IO Outcome
drive env mode reachBonus attacker target = do
    wm ← setupPages
    writeIORef (worldManagerRef env) wm
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "admission_dummy" testDef
        , umInstances = HM.fromList
            [ (UnitId attackerUid, attacker), (UnitId targetUid, target) ] }
    writeIORef (combatEventsRef env) Seq.empty
    -- Fixed generator: the hit/dodge/part draws are then the same on
    -- every run, so the resolvable case's outcome is a constant rather
    -- than a coin flip that could mask a refusal.
    writeIORef (statRNGRef env) (Random.mkStdGen 20260903)
    _ ← drainUnitQueue env
    Q.writeQueue (combatQueue env)
        (CombatAttack attackerUid targetUid mode reachBonus 0)
    processAllCommands env
    evs ← readIORef (combatEventsRef env)
    um  ← readIORef (unitManagerRef env)
    kills ← length ∘ filter isKill ⊚ drainUnitQueue env
    let atk' = HM.lookup (UnitId attackerUid) (umInstances um)
        tgt' = HM.lookup (UnitId targetUid) (umInstances um)
    pure Outcome
        { oEvents  = toList evs
        , oStamina = maybe (-1) (HM.lookupDefault (-1) "stamina" ∘ uiStats) atk'
        , oStance  = maybe (-1) (HM.lookupDefault (-1) "stance" ∘ uiStats) atk'
        , oWounds  = maybe (-1) (length ∘ uiWounds) tgt'
        , oKills   = kills
        }

-- | Empty the unit-command queue, returning what was on it. Used both
--   to clear it before the strike and to read back whether the strike
--   queued a 'UnitKill' — the death path's only observable here, since
--   'Combat.Resolution.Events.setDead' writes the command rather than
--   the pose.
drainUnitQueue ∷ EngineEnv → IO [UnitCommand]
drainUnitQueue env = reverse ⊚ go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (unitQueue env)
        case mCmd of
            Nothing  → pure acc
            Just cmd → go (cmd : acc)

isKill ∷ UnitCommand → Bool
isKill (UnitKill _) = True
isKill _            = False

-- | 'drive' with the pair placed so their Chebyshev separation is
--   EXACTLY @sep@ — the attacker at the origin, the target @sep@ tiles
--   along x. Offsetting both from some other anchor would round the
--   separation, which is the difference between testing the bound and
--   testing a float near it.
driveAtSeparation
    ∷ EngineEnv → AttackMode → Float → Float → IO Outcome
driveAtSeparation env mode reachBonus sep =
    drive env mode reachBonus
        (testUnit pageA 0 0 startStance)
        (testUnit pageA sep 0 startStance)

kinds ∷ Outcome → [Text]
kinds = map ceKind ∘ oEvents

reasons ∷ Outcome → [Text]
reasons o =
    [ r | ev ← oEvents o
        , Just r ← [HM.lookup "reason" (cePayload ev)] ]

-- | Every "nothing committed" assertion in one place, so a refusal
--   case cannot pass by checking the event and forgetting the effects.
expectRefusal ∷ AttackRefusal → Outcome → Expectation
expectRefusal refusal o = do
    kinds o `shouldBe` ["refused"]
    reasons o `shouldBe` [refusalReason refusal]
    oWounds o `shouldBe` 0
    oStamina o `shouldBe` startStamina
    oStance o `shouldBe` startStance
    oKills o `shouldBe` 0

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    pure env

spec ∷ Spec
spec = beforeAll initEnv $ do

  describe "Combat admission revalidates at commit (#2328)" $ do

    describe "the pure policy" $ do
        it "orders page before reach before stance" $ \_ → do
            -- All three conditions violated at once: the FIRST is what
            -- is reported. Pins the order the refusal reasons are
            -- documented in, which a consumer distinguishing them
            -- depends on.
            let atk = testUnit pageA 0 0 0
                tgt = testUnit pageB 99 99 1
            checkAdmission baseRange 0 Quick atk tgt
                `shouldBe` Just RefusedDifferentPage
            checkAdmission baseRange 0 Quick atk (tgt { uiPage = pageA })
                `shouldBe` Just RefusedOutOfReach
            checkAdmission baseRange 0 Quick atk
                (tgt { uiPage = pageA, uiGridX = 0, uiGridY = 0 })
                `shouldBe` Just RefusedInsufficientStance

        it "a negative reach bonus cannot tighten the plain range" $ \_ → do
            let atk = testUnit pageA 0 0 1
                tgt = testUnit pageA baseRange 0 1
            checkAdmission baseRange 0 Quick atk tgt `shouldBe` Nothing
            checkAdmission baseRange (-5) Quick atk tgt `shouldBe` Nothing

        it "measures Chebyshev separation, not Euclidean" $ \_ → do
            let atk = testUnit pageA 0 0 1
                tgt = testUnit pageA 0.4 0.4 1
            -- Euclidean 0.566 > baseRange 0.5; Chebyshev 0.4 < 0.5.
            chebyshevSeparation atk tgt `shouldBe` 0.4
            checkAdmission baseRange 0 Quick atk tgt `shouldBe` Nothing

        it "spells the same range formula unit.getAttackRange reports" $ \_ → do
            -- height/2.4 + blade/100, and the no-height fallback every
            -- Lua call site spells as `or 1.0`.
            attackRangeTiles 1.2 0 `shouldBe` 0.5
            attackRangeTiles 1.2 50 `shouldBe` 1.0
            noHeightAttackRange `shouldBe` 1.0

        it "reads an absent stance as the full 1.0 the drain spends" $ \_ → do
            let bare = (testUnit pageA 0 0 1)
                    { uiStats = HM.delete "stance"
                        (uiStats (testUnit pageA 0 0 1)) }
            attackerStance bare `shouldBe` 1.0
            checkAdmission baseRange 0 Heavy bare (testUnit pageA 0 0 1)
                `shouldBe` Nothing

    describe "a queued strike drained after the world moved" $ do

        it "refuses a target that crossed to another page, mutating nothing" $
          \env → do
            o ← drive env Quick 0
                    (testUnit pageA 10 10 startStance)
                    (testUnit pageB 10 10 startStance)
            expectRefusal RefusedDifferentPage o

        it "refuses a target that fled out of reach, mutating nothing" $
          \env → do
            -- One hundredth of a tile beyond the attacker's own range:
            -- the bound itself is what refuses this, not a wide margin.
            o ← driveAtSeparation env Quick 0 (baseRange + 0.01)
            expectRefusal RefusedOutOfReach o

        it "still commits a strike exactly ON the reach bound" $ \env → do
            -- The comparison is `>`, so the bound itself is INSIDE
            -- reach. Without this the refusal could be off by one tick
            -- of float and still pass every case above.
            o ← driveAtSeparation env Quick 0 baseRange
            kinds o `shouldNotContain` ["refused"]

        -- Requirement 2's lunge clause: the request's own reachBonus
        -- extends the bound, so an extended strike admitted at a
        -- separation beyond the BASE range still lands. Measured on a
        -- live red squirrel this is the real lunge geometry — it
        -- strikes at Chebyshev 0.5 with a 0.12-tile base range and a
        -- ~1.95 stored reach, so the bonus is what carries it.
        it "commits a strike beyond base range when the request carries a reach bonus" $
          \env → do
            o ← driveAtSeparation env Quick 1.0 (baseRange + 0.5)
            kinds o `shouldNotContain` ["refused"]
            -- …and it really resolved: the swing was paid for.
            oStamina o `shouldSatisfy` (< startStamina)
            oStance o `shouldSatisfy` (< startStance)

        it "refuses that same separation without the reach bonus" $ \env → do
            o ← driveAtSeparation env Quick 0 (baseRange + 0.5)
            expectRefusal RefusedOutOfReach o

    describe "stance spent between admission and commit" $ do
        -- Table-driven, one row per mode, each pinned just BELOW its
        -- own threshold and again exactly AT it. The two thresholds
        -- differ (quick 0.25, heavy 0.5), so a single-mode case could
        -- pass against a check that used the wrong constant.
        let below m = stanceAttackCost m - 0.01
        mapM_ (\(label, mode) →
            describe label $ do
                it "refuses just below the mode's own cost, mutating nothing" $
                  \env → do
                    let atk = (testUnit pageA 10 10 (below mode))
                    o ← drive env mode 0 atk
                            (testUnit pageA 10 10 startStance)
                    kinds o `shouldBe` ["refused"]
                    reasons o `shouldBe`
                        [refusalReason RefusedInsufficientStance]
                    oWounds o `shouldBe` 0
                    oStamina o `shouldBe` startStamina
                    -- The stance the strike could not pay is left
                    -- exactly as found — not floored at 0 by a drain
                    -- that ran anyway.
                    oStance o `shouldBe` below mode
                    oKills o `shouldBe` 0

                it "commits exactly at the mode's own cost" $ \env → do
                    let atk = testUnit pageA 10 10 (stanceAttackCost mode)
                    o ← drive env mode 0 atk
                            (testUnit pageA 10 10 startStance)
                    kinds o `shouldNotContain` ["refused"]
                    oStance o `shouldBe` 0)
            [ ("quick", Quick), ("heavy", Heavy) ]

        it "refuses a quick swing at a stance a heavy one also could not pay" $
          \env → do
            -- The heavy row above is the one that would pass against a
            -- check hard-coded to 0.25; this is its mirror.
            let atk = testUnit pageA 10 10 0.3
            o ← drive env Heavy 0 atk (testUnit pageA 10 10 startStance)
            reasons o `shouldBe` [refusalReason RefusedInsufficientStance]
            o' ← drive env Quick 0 atk (testUnit pageA 10 10 startStance)
            kinds o' `shouldNotContain` ["refused"]
