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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Foldable (toList)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef', newIORef)
import qualified System.Random as Random
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Combat.Resolution.Admission
    ( AttackRefusal(..), StrikeCommit(..), refusalReason, attackRangeTiles
    , noHeightAttackRange, chebyshevSeparation, attackerStance
    , checkAdmission, commitIfAdmitted )
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
    , oGenIn   ∷ Random.StdGen
      -- ^ the strike stream handed to the drain…
    , oGenOut  ∷ Random.StdGen
      -- ^ …and the one it handed back. Equal ⇒ nothing was rolled away.
    , oStatGen ∷ Random.StdGen
      -- ^ the SHARED stat pool after the drain. Resolution must never
      --   touch it at all (#2328).
    } deriving (Show)

-- | The strike stream every fixture drain starts from. Fixed, so a
--   refusal's "the generator came back untouched" is an equality
--   against a known value rather than against whatever was there.
seedGen ∷ Random.StdGen
seedGen = Random.mkStdGen 20260903

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
    -- The shared stat pool is set to a KNOWN value so the assertion
    -- that resolution left it alone is an equality, not an absence.
    writeIORef (statRNGRef env) statGenBefore
    _ ← drainUnitQueue env
    Q.writeQueue (combatQueue env)
        (CombatAttack attackerUid targetUid mode reachBonus 0)
    -- The strike stream is a VALUE the worker carries, so the drain
    -- takes one and hands one back: fixed here, which also makes the
    -- resolvable case's hit/dodge/part draws the same on every run
    -- rather than a coin flip that could mask a refusal.
    genOut ← processAllCommands env seedGen
    statAfter ← readIORef (statRNGRef env)
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
        , oGenIn   = seedGen
        , oGenOut  = genOut
        , oStatGen = statAfter
        }

-- | A recognisable value for the shared stat pool, so "resolution never
--   touched it" is checkable by equality.
statGenBefore ∷ Random.StdGen
statGenBefore = Random.mkStdGen 4242

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
    -- Randomness is a mutation too: a strike that never happened must
    -- not shift what the next one rolls, on either stream.
    oGenOut o `shouldBe` oGenIn o
    oStatGen o `shouldBe` statGenBefore

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
            -- …and it really resolved: the swing was paid for, and the
            -- reservation it rolled from was spent. Without this the
            -- refusal cases' "the stream came back untouched" would
            -- pass against a resolution that never rolls at all.
            oStamina o `shouldSatisfy` (< startStamina)
            oStance o `shouldSatisfy` (< startStance)
            oGenOut o `shouldNotBe` oGenIn o
            -- The shared stat pool is still not resolution's business.
            oStatGen o `shouldBe` statGenBefore

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

    -- The early check in resolveAttack reads a snapshot; the awareness
    -- lookup and the RNG transaction happen after it, and the unit
    -- thread publishes uiGridX/uiGridY into the same IORef the whole
    -- time. These cases are about the AUTHORITY that closes that
    -- window: commitIfAdmitted, which re-checks against the very
    -- manager value the strike's writes are applied to, in one
    -- transaction.
    describe "the commit transaction (invalidation after the snapshot)" $ do

        let range _ = baseRange
            -- A write with a visible signature, so "applied" and "not
            -- applied" are distinguishable without inferring it from
            -- some other effect.
            markStruck _ _ um = um
                { umInstances = HM.adjust
                    (\i → i { uiName = "struck" }) (UnitId targetUid)
                    (umInstances um) }
            struck env = do
                um ← readIORef (unitManagerRef env)
                pure (maybe "" uiName
                        (HM.lookup (UnitId targetUid) (umInstances um)))
            put env atk tgt = writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "admission_dummy" testDef
                , umInstances = HM.fromList
                    [ (UnitId attackerUid, atk), (UnitId targetUid, tgt) ] }

        it "applies the writes when the policy still holds" $ \env → do
            put env (testUnit pageA 0 0 startStance)
                    (testUnit pageA 0 0 startStance)
            r ← commitIfAdmitted env range 0 Quick attackerUid targetUid
                    markStruck
            r `shouldBe` CommitApplied
            struck env `shouldReturn` "struck"

        -- One row per condition: the strike was admitted against a
        -- snapshot in which it held, and the LIVE manager it is about
        -- to write to no longer satisfies it.
        forM_
            [ ( "the target moved out of reach"
              , testUnit pageA (baseRange + 0.01) 0 startStance
              , Quick, RefusedOutOfReach )
            , ( "the target crossed to another page"
              , testUnit pageB 0 0 startStance
              , Quick, RefusedDifferentPage )
            ] $ \(label, liveTarget, mode, expected) →
            it ("refuses and writes nothing when " <> label) $ \env → do
                put env (testUnit pageA 0 0 startStance) liveTarget
                r ← commitIfAdmitted env range 0 mode attackerUid targetUid
                        markStruck
                r `shouldBe` CommitRefused expected
                struck env `shouldReturn` ""

        it "refuses and writes nothing when the attacker's stance was spent" $
          \env → do
            put env (testUnit pageA 0 0 (stanceAttackCost Heavy - 0.01))
                    (testUnit pageA 0 0 startStance)
            r ← commitIfAdmitted env range 0 Heavy attackerUid targetUid
                    markStruck
            r `shouldBe` CommitRefused RefusedInsufficientStance
            struck env `shouldReturn` ""

        it "recomputes reach from the LIVE attacker, not a captured one" $
          \env → do
            -- The separation is inside a TALL attacker's reach and
            -- outside a short one's. Passing the real per-instance
            -- range function is what makes the live instance decide;
            -- a captured constant would answer for the wrong unit.
            let liveRange inst = attackRangeTiles
                    (HM.lookupDefault 0 "height" (uiStats inst)) 0
                shortAtk = testUnit pageA 0 0 startStance
                tallAtk  = shortAtk
                    { uiStats = HM.insert "height" 4.8 (uiStats shortAtk) }
                tgt = testUnit pageA (baseRange + 0.25) 0 startStance
            put env shortAtk tgt
            r1 ← commitIfAdmitted env liveRange 0 Quick attackerUid targetUid
                    markStruck
            r1 `shouldBe` CommitRefused RefusedOutOfReach
            put env tallAtk tgt
            r2 ← commitIfAdmitted env liveRange 0 Quick attackerUid targetUid
                    markStruck
            r2 `shouldBe` CommitApplied

        it "is silent, not a refusal, when a unit left the manager" $ \env → do
            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "admission_dummy" testDef
                , umInstances = HM.singleton (UnitId attackerUid)
                    (testUnit pageA 0 0 startStance) }
            r ← commitIfAdmitted env range 0 Quick attackerUid targetUid
                    markStruck
            r `shouldBe` CommitVanished

        it "cannot be interleaved: the write sees the value the check passed" $
          \env → do
            -- The property IS the transaction, so the fixture is a real
            -- interleaving, and it races the value the write actually
            -- consumes. A competing thread hammers the SAME IORef,
            -- alternating the attacker's stance just above and just
            -- below a heavy swing's cost, while this thread commits
            -- strikes whose write RECORDS the stance it was handed.
            --
            -- One transaction means the instance the write sees is the
            -- instance the check passed, so every recorded value is at
            -- or above the cost. Split the check and the write into two
            -- transactions — which is what a snapshot check plus a
            -- later effect is — and the competitor lands between them,
            -- handing the write a stance the check would have refused.
            let cost  = stanceAttackCost Heavy
                above = cost + 0.05
                below = cost - 0.05
                rounds = 400 ∷ Int
                -- The write records what it was handed, on the TARGET,
                -- which the competitor never touches.
                recordSeen atkLive _ um = um
                    { umInstances = HM.adjust
                        (\i → i { uiStats = HM.insert "seen"
                                    (attackerStance atkLive) (uiStats i) })
                        (UnitId targetUid) (umInstances um) }
                readSeen = do
                    um ← readIORef (unitManagerRef env)
                    pure (maybe (-1) (HM.lookupDefault (-1) "seen" ∘ uiStats)
                            (HM.lookup (UnitId targetUid) (umInstances um)))
            put env (testUnit pageA 0 0 above) (testUnit pageA 0 0 startStance)
            stop ← newIORef False
            done ← newEmptyMVar
            _ ← forkIO $
                let loop lo = do
                        halt ← readIORef stop
                        if halt then putMVar done () else do
                            atomicModifyIORef' (unitManagerRef env) $ \um →
                                ( um { umInstances = HM.adjust
                                        (\i → i { uiStats = HM.insert "stance"
                                                    (if lo then below else above)
                                                    (uiStats i) })
                                        (UnitId attackerUid) (umInstances um) }
                                , () )
                            loop (not lo)
                in loop True
            applied ← newIORef (0 ∷ Int)
            refused ← newIORef (0 ∷ Int)
            worst   ← newIORef (1 / 0 ∷ Float)
            replicateM_ rounds $ do
                r ← commitIfAdmitted env range 0 Heavy attackerUid targetUid
                        recordSeen
                case r of
                    CommitApplied → do
                        seen ← readSeen
                        atomicModifyIORef' worst (\w → (min w seen, ()))
                        atomicModifyIORef' applied (\n → (n + 1, ()))
                    CommitRefused _ → atomicModifyIORef' refused (\n → (n + 1, ()))
                    CommitVanished  → pure ()
                threadDelay 20
            writeIORef stop True
            takeMVar done
            nApplied ← readIORef applied
            nRefused ← readIORef refused
            lowest   ← readIORef worst
            -- The interleaving really happened: neither outcome is
            -- vacuous, so the window this asserts about was open.
            nApplied `shouldSatisfy` (> 0)
            nRefused `shouldSatisfy` (> 0)
            -- …and no write was ever handed a stance the check refused.
            lowest `shouldSatisfy` (≥ cost)

    -- The review's own ask: force invalidation AFTER the snapshot check
    -- and prove the shared pool is untouched. The snapshot is read at
    -- the top of resolveAttack and the commit happens after the
    -- awareness lookup and the roll, so the only way to land inside
    -- that window is a real concurrent writer on the manager.
    describe "randomness is spent only by a strike that commits" $ do

        it "leaves both streams alone when the world moves under a queued strike" $
          \env → do
            -- A competing thread flips the target's page while a long
            -- run of strikes drains, so refusals land at BOTH points —
            -- the snapshot check and the commit transaction. Whichever
            -- one fires, the strike stream must come back exactly as
            -- many splits along as there were commits, and the shared
            -- stat pool must not have moved at all.
            let queued = 200 ∷ Int
            wm ← setupPages
            writeIORef (worldManagerRef env) wm
            writeIORef (unitManagerRef env) emptyUnitManager
                { umDefs = HM.singleton "admission_dummy" testDef
                , umInstances = HM.fromList
                    [ (UnitId attackerUid, testUnit pageA 0 0 1.0e9)
                    , (UnitId targetUid,   testUnit pageA 0 0 startStance) ] }
            writeIORef (combatEventsRef env) Seq.empty
            writeIORef (statRNGRef env) statGenBefore
            _ ← drainUnitQueue env
            replicateM_ queued $ Q.writeQueue (combatQueue env)
                (CombatAttack attackerUid targetUid Quick 0 0)
            stop ← newIORef False
            done ← newEmptyMVar
            _ ← forkIO $
                let loop other = do
                        halt ← readIORef stop
                        if halt then putMVar done () else do
                            atomicModifyIORef' (unitManagerRef env) $ \um →
                                ( um { umInstances = HM.adjust
                                        (\i → i { uiPage = if other
                                                    then pageB else pageA })
                                        (UnitId targetUid) (umInstances um) }
                                , () )
                            loop (not other)
                in loop True
            genOut ← processAllCommands env seedGen
            writeIORef stop True
            takeMVar done
            statAfter ← readIORef (statRNGRef env)
            evs ← toList ⊚ readIORef (combatEventsRef env)
            let refusalCount = length
                    [ () | ev ← evs, ceKind ev ≡ "refused" ]
                commitCount = length evs - refusalCount
                -- One split per COMMITTED strike, none for a refusal.
                expected = iterate (fst ∘ Random.splitGen) seedGen
                            !! commitCount
            -- Both outcomes really occurred, so neither half is vacuous.
            refusalCount `shouldSatisfy` (> 0)
            commitCount `shouldSatisfy` (> 0)
            length evs `shouldBe` queued
            -- The shared four-writer pool was never resolution's to
            -- spend, refusal or not.
            statAfter `shouldBe` statGenBefore
            -- …and the strike stream advanced exactly once per commit.
            genOut `shouldBe` expected
