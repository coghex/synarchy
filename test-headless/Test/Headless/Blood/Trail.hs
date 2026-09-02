{-# LANGUAGE Strict #-}
-- | Tests for the pure ongoing-bleeding accumulator/mapping math —
--   BOTH halves, which share one 'Unit.Types.Trail.TrailState':
--
--   * the moving half (issue #882): "Blood.Trail"'s 'consumeTrailMarks'
--     (distance+cadence gating, conserved volume, partition invariance)
--     and its volume -> texture-request mapping;
--   * the stationary half (issue #883): "Blood.Pool"'s
--     'classifyOngoing' arbitration, 'consumePoolLayers' cadence/volume/
--     bound math, layer placement, and its own texture mapping;
--
--   plus "Combat.Wounds.Bleed"'s external- vs internal-bleed kind
--   classification the wound tick's conserved external-loss accounting
--   relies on, and the shared lifecycle (destroy / save-load / death).
module Test.Headless.Blood.Trail (spec) where

import UPrelude
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Test.Hspec
import Data.List (sort, nub, find)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Exception (finally)
import System.Directory (removePathForcibly)
import Data.IORef (newIORef, readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Sim.Types (emptyUnitThreadState)
import Unit.Thread.Command.Lifecycle (handleUnitDestroyCommand)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )
import World.Save.Types (toUnitSnapshot, fromUnitSnapshot)
import World.Save.Serialize (loadWorld)
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Types (StagedSession(..), StagedPage(..))
import World.Thread.Command.Save.WriteWorld (handleWorldSaveCommand)
import World.Thread.Command.Init (handleWorldInitCommand)
import Infection.Types (emptyInfectionManager)
import Combat.Wounds (tickOneUnit)
import qualified System.Random as Random
import Blood.Trail
import Blood.Pool
import Blood.Types
    (BloodStyle(..), FootprintBucket(..), BloodStore(..), BloodDecal(..)
    , allDecals, emptyBloodStore, defaultBloodTextureCap)
import Combat.Wounds.Bleed (isExternallyBleedingKind)

spec ∷ Spec
spec = do
  kindSpec
  gatingSpec
  conservationSpec
  partitionInvarianceSpec
  textureMappingSpec
  pageTargetingSpec
  lifecycleSpec
  worldPersistenceSpec
  poolArbitrationSpec
  poolCadenceSpec
  poolBoundSpec
  poolPartitionInvarianceSpec
  poolPlacementSpec
  poolTextureMappingSpec
  poolImmutabilitySpec

kindSpec ∷ Spec
kindSpec = describe "Combat.Wounds.Bleed.isExternallyBleedingKind" $ do
    it "excludes internal, fracture, and concussion" $ do
        isExternallyBleedingKind "internal"   `shouldBe` False
        isExternallyBleedingKind "fracture"   `shouldBe` False
        isExternallyBleedingKind "concussion" `shouldBe` False

    it "includes every other wound kind" $ do
        mapM_ (\k → isExternallyBleedingKind k `shouldBe` True)
            ["slash", "stab", "blunt", "arterial", "severed", "frostbite"]

gatingSpec ∷ Spec
gatingSpec = describe "Blood.Trail.consumeTrailMarks — distance/cadence gating" $ do

    it "pops nothing when distance hasn't been covered, even with ample time and volume" $ do
        let ts0 = emptyTrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (ts', marks) = consumeTrailMarks defaultTrailThresholds 0.1 100.0 100.0 ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0   -- untouched — nothing consumed

    it "pops nothing when cadence hasn't elapsed, even with ample distance and volume" $ do
        let ts0 = emptyTrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0, tsLastMarkAt = 100.0 }
            (ts', marks) = consumeTrailMarks defaultTrailThresholds 50.0 0.01 100.01 ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0

    it "pops exactly one mark once BOTH gates are cleared" $ do
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 0.2, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (_, marks) = consumeTrailMarks tp (ttMinDistance tp) (ttMinCadence tp)
                            (ttMinCadence tp) ts0
        length marks `shouldBe` 1

    it "never pops while stationary (stepDist=0), even with both gates already banked \
       \— #883 defers stationary/collapsed pooling (round-4 review regression)" $ do
        -- Distance and cadence can both already be satisfied from EARLIER
        -- movement while the unit has since stopped — cadence keeps
        -- advancing with real time regardless of motion. A call with
        -- stepDist=0 must never pop (that would be stationary pooling,
        -- explicitly #883's scope, not this issue's), and must leave the
        -- banked state untouched for whenever movement actually resumes.
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 0.5
                             , tsDistSinceMark = ttMinDistance tp
                             , tsLastMarkAt = 0 }
            (ts', marks) = consumeTrailMarks tp 0 (ttMinCadence tp) (ttMinCadence tp) ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 0.5
        tsDistSinceMark ts' `shouldBe` ttMinDistance tp

    it "consecutive marks in the SAME call are genuinely >= ttMinDistance apart \
       \(round-5 review regression)" $ do
        -- The exact reviewer-supplied counter-example: with defaults
        -- (ttMinDistance=1.0, ttMinCadence=0.5), distSinceMark=0.9 and a
        -- 10-tile/6s call used to place marks at ~0.667 and ~1.5 (only
        -- 0.833 tiles apart) because each mark's fraction was computed
        -- independently against the call's ORIGINAL baseline rather
        -- than against the PRECEDING mark's own position — reachable on
        -- any catch-up tick after a speed change.
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0.9, tsLastMarkAt = 0 }
            (_, marks) = consumeTrailMarks tp 10 6 6.1 ts0
            positions = map ((* 10) . tmoFraction) marks
            gaps = zipWith (-) (drop 1 positions) positions
        length marks `shouldSatisfy` (≥ 2)
        gaps `shouldSatisfy` all (\g → g ≥ ttMinDistance tp - 1e-4)

    it "never pops a mark with no real blood behind it, even when both gates clear" $ do
        -- A stale distance/cadence bank from a bleed that has since
        -- stopped (clotted/bandaged to zero) must not manufacture a
        -- mark once nothing is left to spend (issue #882 requirement
        -- 2/3 — never a fixed per-tick/per-gate stamp).
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (ts', marks) = consumeTrailMarks tp (ttMinDistance tp) (ttMinCadence tp)
                            (ttMinCadence tp) ts0
        marks `shouldBe` []
        -- The banked progress itself is preserved (not silently reset),
        -- so real volume arriving later still needs a full fresh gate.
        tsDistSinceMark ts' `shouldBe` ttMinDistance tp

conservationSpec ∷ Spec
conservationSpec = describe "Blood.Trail.consumeTrailMarks — conserved volume" $ do

    it "never invents or drops volume: popped marks sum to the pending total" $ do
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 3.7, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            -- A single big jump clearing 10 multiples of BOTH gates at once
            -- (the "catch-up" case) — every drop of pendingVolume must show
            -- up across the popped marks, never silently dropped.
            (ts', marks) = consumeTrailMarks tp (10 * ttMinDistance tp)
                                              (10 * ttMinCadence tp) (10 * ttMinCadence tp) ts0
        length marks `shouldBe` 10
        sum (map tmoVolume marks) `shouldSatisfy` (\v → abs (v - 3.7) < 1e-4)
        tsPendingVolume ts' `shouldBe` 0

    it "catch-up marks spread across the step (never all stamped at one endpoint)" $ do
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 1.0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (_, marks) = consumeTrailMarks tp (10 * ttMinDistance tp)
                                            (10 * ttMinCadence tp) (10 * ttMinCadence tp) ts0
            fracs = map tmoFraction marks
        length marks `shouldBe` 10
        fracs `shouldBe` sort fracs   -- non-decreasing, so spread in order
        fracs `shouldBe` nub fracs    -- and all distinct (evenly spaced)
        all (\f → f > 0 ∧ f ≤ 1) fracs `shouldBe` True

    it "leftover distance/time banked below a gate carries forward, never lost" $ do
        let tp  = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            -- Half the distance gate, half the cadence gate: no mark yet,
            -- but the progress must still be recorded for next tick.
            (ts', marks) = consumeTrailMarks tp (ttMinDistance tp / 2)
                                              (ttMinCadence tp / 2) (ttMinCadence tp / 2) ts0
        marks `shouldBe` []
        tsDistSinceMark ts' `shouldSatisfy` (\d → abs (d - ttMinDistance tp / 2) < 1e-6)

partitionInvarianceSpec ∷ Spec
partitionInvarianceSpec =
  describe "Blood.Trail.consumeTrailMarks — timestep-partition invariance" $ do

    it "an evenly-divisible path splits into the SAME total marks/volume/positions regardless of step count" $
        checkPartitionInvariance 10 10 2.0 100

    it "an UNEVENLY-divisible path still matches within a small tolerance" $
        checkPartitionInvariance 10.3 10.3 2.37 37

    it "a CADENCE-limited path (distance alone would allow more marks than \
       \cadence does) still matches — round-2 review regression" $
        -- distGates=3 (3 tiles at ttMinDistance=1.0) but cadenceGates=2
        -- (1 second at ttMinCadence=0.5): distance alone would allow 3
        -- marks, cadence caps it to 2 — exactly the asymmetric scenario
        -- where distance-ONLY positioning broke (it placed marks at the
        -- distance multiples 1,2 instead of where cadence actually
        -- cleared, 1.5,3.0, and disagreed across different chunkings).
        checkPartitionInvariance 3.0 2.0 2.0 4

    it "a DISTANCE-limited path (cadence alone would allow more marks than \
       \distance does) still matches" $
        -- distGates=2 (2 tiles) but cadenceGates=4 (2 seconds): cadence
        -- alone would allow 4 marks, distance caps it to 2 — the mirror
        -- image of the cadence-limited case above.
        checkPartitionInvariance 2.0 4.0 2.0 4

    it "a follow-up tiny step never pops a mark under ttMinDistance away \
       \from the last one (round-3 review regression)" $ do
        -- The exact scenario the round-3 review flagged: after a
        -- cadence-limited mark lands SHORT of a whole distance multiple
        -- (3 tiles/1s -> marks at 1.5 and 3.0, i.e. the unit has only
        -- travelled up to the 3.0 mark itself, nothing "extra" banked),
        -- a tiny follow-up step must NOT immediately pop another mark —
        -- the old "d1 - n*minDistance" bookkeeping fictionally banked a
        -- whole leftover tile the unit never actually walked past the
        -- last mark, letting a 0.01-tile nudge pop one right on top of it.
        let tp = defaultTrailThresholds
            ts0 = emptyTrailState { tsPendingVolume = 2.0 }
            (afterBig, bigMarks) = consumeTrailMarks tp 3.0 1.0 1.0 ts0
            (_, followUpMarks) =
                consumeTrailMarks tp 0.01 0.5 (1.0 + 0.5) afterBig
        length bigMarks `shouldBe` 2
        followUpMarks `shouldBe` []

-- | Shared partition-invariance check (issue #882 spec addition, tightened
--   by round-2 review): the same continuous path (@distGates@/
--   @cadenceGates@ multiples of the thresholds) and external-loss budget
--   (@totalVol@), split into a single big update vs @steps@ small ones,
--   must converge to the SAME emitted mark count, total volume, AND
--   absolute positions — within a documented tolerance, since chunking
--   can shift exactly which step a boundary-crossing mark pops on by at
--   most one (Float summation: e.g. @0.05*10@ landing a hair below
--   @0.5@). Volume conservation itself (popped + still-pending =
--   everything ever added) is checked EXACTLY for both the single big
--   call and the many-small-steps run — nothing may be invented or
--   dropped regardless of how the same journey is chunked.
checkPartitionInvariance ∷ Float → Double → Float → Int → Expectation
checkPartitionInvariance distGates cadenceGates totalVol steps = do
    let tp = defaultTrailThresholds
        totalDist = distGates * ttMinDistance tp
        totalTime = cadenceGates * ttMinCadence tp
        (bigFinal, bigMarks) = consumeTrailMarks tp totalDist totalTime totalTime
                                    emptyTrailState { tsPendingVolume = totalVol }
        bigPositions = map (\m → tmoFraction m * totalDist) bigMarks

        stepDist = totalDist / fromIntegral steps
        stepTime = totalTime / fromIntegral steps
        stepVol  = totalVol  / fromIntegral steps
        goSteps _ _ ts acc 0 = (ts, acc)
        goSteps clock cumDist ts acc k =
            let ts1 = ts { tsPendingVolume = tsPendingVolume ts + stepVol }
                clock' = clock + stepTime
                (ts2, popped) = consumeTrailMarks tp stepDist stepTime clock' ts1
                positioned = [ (cumDist + tmoFraction m * stepDist, m) | m ← popped ]
            in goSteps clock' (cumDist + stepDist) ts2 (acc ++ positioned) (k - 1 ∷ Int)
        (smallFinal, smallPositioned) = goSteps (0 ∷ Double) (0 ∷ Float) emptyTrailState [] steps
        smallMarks = map snd smallPositioned
        smallPositions = map fst smallPositioned
        conserved final marks = sum (map tmoVolume marks) + tsPendingVolume final
    -- Exact conservation for EACH chunking, regardless of pop timing.
    conserved bigFinal bigMarks `shouldSatisfy` (\v → abs (v - totalVol) < 1e-4)
    conserved smallFinal smallMarks `shouldSatisfy` (\v → abs (v - totalVol) < 1e-3)
    -- Documented cross-chunking tolerance: a boundary-crossing mark can
    -- land one step early/late, never further.
    abs (length smallMarks - length bigMarks) `shouldSatisfy` (≤ 1)
    -- Positional invariance (round-2 review requirement): when chunking
    -- doesn't shift the mark COUNT, the absolute positions must agree
    -- closely too — not just fall in the same ballpark from re-derived
    -- totals.
    when (length smallMarks ≡ length bigMarks) $
        zipWith (\a b → abs (a - b)) bigPositions smallPositions
            `shouldSatisfy` all (< 0.05)

textureMappingSpec ∷ Spec
textureMappingSpec =
  describe "Blood.Trail volume -> texture-request mapping" $ do

    it "severity bucket never gets lighter as volume grows" $
        monotonic trailSeverityBucket

    it "footprint never shrinks as volume grows" $
        monotonic trailFootprint

    it "opacity never decreases as volume grows" $
        monotonic trailOpacity

    it "style stays within the drops/smear vocabulary (never pool/spatter/streak)" $
        mapM_ (\v → trailStyleFor v `shouldSatisfy` (`elem` [StyleDrops, StyleSmear]))
              [0, 0.001, 0.01, 0.05, 0.1, 0.3, 0.4, 1.0, 5.0]

    it "a heavier mark is never LESS opaque than a lighter one at the same style" $ do
        trailOpacity 0.4 `shouldSatisfy` (> trailOpacity 0.01)

  where
    volumes = [0, 0.001, 0.01, 0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 1.0, 5.0]
    monotonic ∷ (Ord a, Show a) ⇒ (Float → a) → Expectation
    monotonic f =
        let vals = map f volumes
        in vals `shouldBe` sort vals

-- ----- Round-3 review: deterministic lifecycle/page/load coverage -----
-- (the probe only exercises death against a real engine; these three
-- exercise the OTHER acceptance items directly and pin them down
-- exactly, rather than relying on a live-engine timing window.)

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    pure env

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "trail_test_a"
pageB = WorldPageId "trail_test_b"

pageTargetingSpec ∷ Spec
pageTargetingSpec =
  describe "Blood.Trail.spawnTrailMark places decals on the unit's OWN page (#882)" $
    it "targets the given page's own BloodStore, never whichever page happens to be head/active" $ do
        env ← initEnv
        wsA ← emptyWorldState
        wsB ← emptyWorldState
        -- pageA is wmWorlds' head / wmVisible's only entry — the kind of
        -- "active" page a page-blind implementation would wrongly use.
        writeIORef (worldManagerRef env) (emptyWorldManager
            { wmWorlds = [(pageA, wsA), (pageB, wsB)]
            , wmVisible = [pageA] })
        now ← readIORef (gameTimeRef env)
        spawnTrailMark (toWorldSimCapability env) pageB 5 5 0 "slash" 0.1 0 0 Nothing now
        storeA ← readIORef (wsBloodStoreRef wsA)
        storeB ← readIORef (wsBloodStoreRef wsB)
        allDecals (bstDecals storeA) `shouldBe` []
        length (allDecals (bstDecals storeB)) `shouldBe` 1

-- Minimal UnitDef/UnitInstance fixtures — mirrors Test.Headless.Combat.
-- Wounds' pattern; only the fields these two specs touch matter.
minimalDef ∷ UnitDef
minimalDef = UnitDef
    { udName = "t", udNamePool = Nothing, udDisplayName = Nothing
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
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

minimalInst ∷ WorldPageId → Maybe TrailState → UnitInstance
minimalInst page ts = UnitInstance
    { uiDefName = "t", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = ts
    }

lifecycleSpec ∷ Spec
lifecycleSpec = describe "Bleeding-trail lifecycle: destroy and save/load (#882)" $ do

    it "handleUnitDestroyCommand removes the unit — the trail-state query surface sees nothing" $ do
        env ← initEnv
        let uid = UnitId 1
            liveTs = emptyTrailState { tsPendingVolume = 0.4, tsDistSinceMark = 0.2, tsLastMarkAt = 5.0 }
        writeIORef (unitManagerRef env)
            (emptyUnitManager { umDefs = HM.singleton "t" minimalDef
                              , umInstances = HM.singleton uid (minimalInst pageA (Just liveTs)) })
        utsRef ← newIORef emptyUnitThreadState
        handleUnitDestroyCommand env utsRef uid
        um' ← readIORef (unitManagerRef env)
        HM.lookup uid (umInstances um') `shouldBe` Nothing

    it "a save/load round-trip resets the trail accumulator, even if the original session had one" $ do
        let uid = UnitId 1
            liveTs = emptyTrailState { tsPendingVolume = 0.4, tsDistSinceMark = 0.2, tsLastMarkAt = 5.0 }
            defs = HM.singleton "t" minimalDef
            um0 = emptyUnitManager
                { umDefs = defs, umInstances = HM.singleton uid (minimalInst pageA (Just liveTs)) }
            snap = toUnitSnapshot pageA um0
            (um1, orphans, unknownFactions) = fromUnitSnapshot pageA defs snap
        orphans `shouldBe` []
        unknownFactions `shouldBe` []
        case HM.lookup uid (umInstances um1) of
            Nothing    → expectationFailure "unit vanished across the save/load round-trip"
            Just inst' → uiTrailState inst' `shouldBe` Nothing

    it "a dead unit's wound tick never recreates the trail accumulator, even with an \
       \externally-bleeding wound still in uiWounds (round-5 review regression)" $ do
        -- handleUnitKillCommand clears uiTrailState AND stamps
        -- uiPose="dead" synchronously, so Combat.Wounds.Tick's own
        -- "uiPose inst == dead" early-exit guard is what makes death
        -- terminal for the accumulator on every SUBSEQUENT wound tick
        -- (the corpse's wounds are never cleared, so without this
        -- guard a still-bleeding kind would keep re-deriving a
        -- positive externalPortion and recreate Just from Nothing).
        let w = Wound { woundPart = "torso", woundKind = "slash", woundSeverity = 0.6
                      , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
                      , woundDressing = "", woundInfection = 0.0, woundClean = False
                      , woundInfectionType = "", woundNecrosis = 0.0 }
            inst = (minimalInst pageA Nothing)
                { uiPose = "dead", uiWounds = [w], uiBlood = 3.0 }
            (inst', _, _) = tickOneUnit 100 minimalDef 0.1 emptyInfectionManager Nothing
                                (Random.mkStdGen 1) inst False
        uiTrailState inst' `shouldBe` Nothing

    -- #883: the SAME three lifecycle paths, now with a non-empty pool
    -- cluster riding along — the cluster is transient state like the
    -- rest of the accumulator and must never survive any of them.
    it "a NON-EMPTY pool cluster resets on load, clears on death, and \
       \disappears on destroy (#883 requirement 10)" $ do
        let uid = UnitId 1
            clusterTs = emptyTrailState
                { tsPendingVolume = 0.4, tsDistSinceMark = 0.2, tsLastMarkAt = 5.0
                , tsClusterAnchor = Just (12.5, 7.25), tsClusterLayers = 7 }
            defs = HM.singleton "t" minimalDef
            um0 = emptyUnitManager
                { umDefs = defs
                , umInstances = HM.singleton uid (minimalInst pageA (Just clusterTs)) }

        -- (1) save/load round-trip: the whole accumulator, cluster
        -- bookkeeping included, comes back Nothing.
        let (um1, _, _) = fromUnitSnapshot pageA defs (toUnitSnapshot pageA um0)
        case HM.lookup uid (umInstances um1) of
            Nothing    → expectationFailure "unit vanished across the save/load round-trip"
            Just inst' → uiTrailState inst' `shouldBe` Nothing

        -- (2) destroy: the unit (and with it the cluster) is gone,
        -- while decals spawned from that cluster stay independently
        -- queryable — marks outlive their source (requirement 6).
        env ← initEnv
        ws ← emptyWorldState
        writeIORef (worldManagerRef env) (emptyWorldManager
            { wmWorlds = [(pageA, ws)]
            , wmVisible = [pageA] })
        now ← readIORef (gameTimeRef env)
        spawnPoolLayer (toWorldSimCapability env) pageA 12.5 7.25 0 "slash"
            0.06 0 0 (Just uid) now
        writeIORef (unitManagerRef env) um0
        utsRef ← newIORef emptyUnitThreadState
        handleUnitDestroyCommand env utsRef uid
        um2 ← readIORef (unitManagerRef env)
        HM.lookup uid (umInstances um2) `shouldBe` Nothing
        store ← readIORef (wsBloodStoreRef ws)
        length (allDecals (bstDecals store)) `shouldBe` 1

        -- (3) death: the tick a unit bleeds out, the WHOLE accumulator
        -- goes — cluster anchor and layer budget with it — so nothing
        -- can keep layering onto a corpse's pool.
        let w = Wound { woundPart = "torso", woundKind = "slash", woundSeverity = 0.6
                      , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
                      , woundDressing = "", woundInfection = 0.0, woundClean = False
                      , woundInfectionType = "", woundNecrosis = 0.0 }
            tick i = let (i', _, _) = tickOneUnit 100 minimalDef 0.1
                                          emptyInfectionManager Nothing
                                          (Random.mkStdGen 1) i False
                     in i'
            dying   = (minimalInst pageA (Just clusterTs))
                        { uiWounds = [w], uiBlood = 0.001 }
            surviving = dying { uiBlood = 5.0 }
            dying'  = tick dying
            surviving' = tick surviving
        -- The exsanguination clamp confirms this tick really was the
        -- fatal one (the only path that clears while the wound is still
        -- bleeding externally).
        uiBlood dying' `shouldBe` 0
        uiTrailState dying' `shouldBe` Nothing
        -- ...and the clearing is death-specific: the SAME instance with
        -- blood to spare keeps its cluster and keeps accumulating.
        uiBlood surviving' `shouldSatisfy` (> 0)
        (tsClusterLayers <$> uiTrailState surviving') `shouldBe` Just 7
        (tsClusterAnchor <$> uiTrailState surviving') `shouldBe` Just (Just (12.5, 7.25))

-- | Issue #885: blood is transient BY DESIGN — a loaded session starts
--   with no marks. Unlike 'lifecycleSpec' above (which round-trips only
--   the Lua-owned 'toUnitSnapshot'/'fromUnitSnapshot' unit slice), this
--   exercises the REAL disk-backed save/load path
--   ('World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand' →
--   'World.Save.Serialize.loadWorld' → 'World.Load.Stage.stageSession')
--   starting from a session that genuinely has both a live decal AND a
--   live trail/pool accumulator, proving neither survives — not just
--   that the save DTOs happen to lack the fields.  Staging only, never
--   publishing (same reasoning 'Test.Headless.World.Identity' documents:
--   this module's own 'initEnv' is a PRIVATE engine, so publishing would
--   be safe here too, but staging alone is already sufficient proof and
--   keeps the test cheap).
worldPersistenceSpec ∷ Spec
worldPersistenceSpec =
  describe "Blood world-level persistence: transience across a real \
           \save/load (#885)" $
    it "a real save/load round trip restores no decals and no trail \
       \state, even though the live session had both" $ do
        env ← initEnv
        logger ← readIORef (loggerRef env)
        -- A real (cheap, w8) world init — matching the codebase's own
        -- "private cheap w8 page" convention — rather than a hand-built
        -- 'WorldState', so staging's chunk regen has real plates/timeline
        -- to work from instead of 'defaultWorldGenParams' placeholders.
        handleWorldInitCommand env logger pageA 42 8 3 Nothing
        mWs ← lookup pageA . wmWorlds ⊚ readIORef (worldManagerRef env)
        ws ← case mWs of
            Just ws' → pure ws'
            Nothing  → expectationFailure "world init did not register the page"
                        ≫ error "unreachable"

        now ← readIORef (gameTimeRef env)
        spawnTrailMark (toWorldSimCapability env) pageA 3 3 0 "slash" 0.2 0 0 Nothing now

        let uid = UnitId 1
            liveTs = emptyTrailState
                { tsPendingVolume = 0.4, tsClusterAnchor = Just (3, 3)
                , tsClusterLayers = 2 }
        writeIORef (unitManagerRef env)
            (emptyUnitManager { umDefs = HM.singleton "t" minimalDef
                              , umInstances = HM.singleton uid (minimalInst pageA (Just liveTs))
                              , umNextId = 2 })

        storeBefore ← readIORef (wsBloodStoreRef ws)
        length (allDecals (bstDecals storeBefore)) `shouldSatisfy` (> 0)

        let slot = "hspec_blood_transience_885"
            cleanup = removePathForcibly ("saves/" <> slot)
        cleanup
        (`finally` cleanup) $ do
            handleWorldSaveCommand env logger pageA slot
                "2026-07-29T00:00:00.000000Z" [] [] Nothing
            matReg ← readIORef (materialRegistryRef env)
            loaded ← loadWorld logger slot HS.empty HS.empty
            case loaded of
                Left (_, e) → expectationFailure (T.unpack e)
                Right (sd, _, _) → do
                    stagedOrErr ← stageSession env logger sd matReg
                    case stagedOrErr of
                        Left e → expectationFailure
                            (T.unpack (renderStageError e))
                        Right staged → do
                            case find ((≡ pageA) . spPageId) (ssPages staged) of
                                Nothing → expectationFailure
                                    "saved page missing from staged session"
                                Just sp → do
                                    store' ← readIORef
                                        (wsBloodStoreRef (spWorldState sp))
                                    store' `shouldBe`
                                        emptyBloodStore defaultBloodTextureCap
                            case HM.lookup uid (umInstances (ssUnits staged)) of
                                Nothing → expectationFailure
                                    "saved unit missing from staged session"
                                Just inst' →
                                    uiTrailState inst' `shouldBe` Nothing

-- ----- #883: stationary/collapsed pooling -----

poolArbitrationSpec ∷ Spec
poolArbitrationSpec =
  describe "Blood.Pool.classifyOngoing — travel vs dwell arbitration" $ do

    it "anchors where the unit stands on the first tick, and dwells" $ do
        let (mode, ts') = classifyOngoing defaultPoolThresholds (4, 9) emptyTrailState
        mode `shouldBe` ModeDwell
        tsClusterAnchor ts' `shouldBe` Just (4, 9)
        tsClusterLayers ts' `shouldBe` 0

    it "in-radius shuffling keeps feeding the SAME cluster — anchor and \
       \layer budget both survive (#883 requirement 5)" $ do
        let pt  = defaultPoolThresholds
            r   = ptClusterRadius pt
            ts0 = emptyTrailState { tsClusterAnchor = Just (10, 10)
                                  , tsClusterLayers = 5 }
            -- A ring of shuffle positions, all strictly inside the
            -- radius but in different directions and adding up to far
            -- more PATH distance than the radius itself.
            shuffles = [ (10 + 0.9 * r, 10), (10, 10 + 0.9 * r)
                       , (10 - 0.9 * r, 10), (10, 10 - 0.9 * r)
                       , (10 + 0.6 * r, 10 + 0.6 * r) ]
            step ts p = let (m, ts') = classifyOngoing pt p ts in (ts', m)
            (tsN, modes) = foldl' (\(ts, ms) p → let (ts', m) = step ts p
                                                 in (ts', ms ++ [m]))
                                  (ts0, []) shuffles
        modes `shouldBe` replicate (length shuffles) ModeDwell
        tsClusterAnchor tsN `shouldBe` Just (10, 10)
        tsClusterLayers tsN `shouldBe` 5

    it "genuinely leaving the radius ends the cluster: fresh anchor AND a \
       \fresh layer budget (#883 spec addition)" $ do
        let pt  = defaultPoolThresholds
            r   = ptClusterRadius pt
            ts0 = emptyTrailState { tsClusterAnchor = Just (10, 10)
                                  , tsClusterLayers = ptMaxLayers pt }
            (mode, ts') = classifyOngoing pt (10 + r * 1.01, 10) ts0
        mode `shouldBe` ModeTravel
        tsClusterAnchor ts' `shouldBe` Just (10 + r * 1.01, 10)
        tsClusterLayers ts' `shouldBe` 0
        poolAtBound pt ts' `shouldBe` False

    it "exactly at the radius is still dwelling (the boundary is inclusive)" $ do
        let pt  = defaultPoolThresholds
            ts0 = emptyTrailState { tsClusterAnchor = Just (10, 10)
                                  , tsClusterLayers = 3 }
            (mode, ts') = classifyOngoing pt (10 + ptClusterRadius pt, 10) ts0
        mode `shouldBe` ModeDwell
        tsClusterLayers ts' `shouldBe` 3

    it "a walk-then-stop pools UNDER the stopped unit, not back at the last \
       \radius crossing (round-1 review regression)" $ do
        -- classifyOngoing can only park its provisional anchor where the
        -- last crossing happened. A unit that walks in and stops short
        -- of another full crossing is up to ptClusterRadius past that
        -- point, so placing layers at the provisional anchor would drop
        -- the pool a tile behind the corpse/casualty. The first layer
        -- re-anchors to where the unit actually is.
        let pt      = defaultPoolThresholds
            crossed = (17.05, 40.0)   -- where the last trail mark landed
            stopped = (18.0,  40.0)   -- where the unit actually came to rest
            walking = emptyTrailState { tsClusterAnchor = Just crossed
                                      , tsClusterLayers = 0
                                      , tsPendingVolume = ptMinVolume pt }
            (mode, tsC) = classifyOngoing pt stopped walking
            (tsP, first) = consumePoolLayers pt stopped (ptMinCadence pt) tsC
        -- Still within the radius of the crossing, so this is a dwell —
        -- the provisional anchor survives classification unchanged...
        mode `shouldBe` ModeDwell
        tsClusterAnchor tsC `shouldBe` Just crossed
        -- ...and the first layer moves it onto the unit.
        length first `shouldBe` 1
        tsClusterAnchor tsP `shouldBe` Just stopped

    it "the anchor FREEZES after the first layer, so an in-radius shuffle \
       \keeps feeding one pool instead of smearing it" $ do
        let pt = defaultPoolThresholds
            started = emptyTrailState { tsClusterAnchor = Just (18.0, 40.0)
                                      , tsClusterLayers = 1
                                      , tsPendingVolume = ptMinVolume pt }
            -- The unit shuffles half a radius away before the next layer.
            shuffled = (18.0 + ptClusterRadius pt / 2, 40.0)
            (mode, tsC) = classifyOngoing pt shuffled started
            (tsP, more) = consumePoolLayers pt shuffled (ptMinCadence pt) tsC
        mode `shouldBe` ModeDwell
        length more `shouldBe` 1
        tsClusterAnchor tsP `shouldBe` Just (18.0, 40.0)

poolCadenceSpec ∷ Spec
poolCadenceSpec = describe "Blood.Pool.consumePoolLayers — cadence/volume gating" $ do

    it "pops nothing before the cadence has elapsed, however much blood is pending" $ do
        let pt  = defaultPoolThresholds
            ts0 = emptyTrailState { tsPendingVolume = 5.0, tsLastMarkAt = 0 }
            (ts', ls) = consumePoolLayers pt origin (ptMinCadence pt * 0.99) ts0
        ls `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0

    it "pops nothing below the volume floor, however long the unit has stood there" $ do
        let pt  = defaultPoolThresholds
            ts0 = emptyTrailState { tsPendingVolume = ptMinVolume pt * 0.9
                                  , tsLastMarkAt = 0 }
            (ts', ls) = consumePoolLayers pt origin 1000 ts0
        ls `shouldBe` []
        tsPendingVolume ts' `shouldSatisfy` (\v → abs (v - ptMinVolume pt * 0.9) < 1e-6)

    it "pops nothing with no blood behind it at all (a cleared/clotted bleed)" $ do
        let pt = defaultPoolThresholds
        snd (consumePoolLayers pt origin 1000 emptyTrailState) `shouldBe` []

    it "a popped layer resets BOTH gates, so the next trail mark is a full \
       \gate away rather than stamped on the pool" $ do
        let pt  = defaultPoolThresholds
            ts0 = emptyTrailState { tsPendingVolume = ptMinVolume pt
                                  , tsDistSinceMark = 0.8, tsLastMarkAt = 0 }
            now = ptMinCadence pt
            (ts', ls) = consumePoolLayers pt origin now ts0
        length ls `shouldBe` 1
        tsPendingVolume ts' `shouldBe` 0
        tsDistSinceMark ts' `shouldBe` 0
        tsLastMarkAt ts'    `shouldSatisfy` (\t → abs (t - now) < 1e-9)

    it "ptMinCadence is a HARD floor between consecutive layers, even after a \
       \long volume-limited wait (round-1 review regression)" $ do
        -- A trickle takes far longer than one cadence window to afford
        -- its first layer, so a big elapsed interval is standing behind
        -- it. Carrying that unspent interval forward used to bank
        -- arbitrary cadence credit: the moment the bleed rate jumped,
        -- the accumulator could cash it in as a burst of layers
        -- milliseconds apart. The clock must restart at the emission.
        let pt  = defaultPoolThresholds
            ts0 = emptyTrailState { tsPendingVolume = ptMinVolume pt
                                  , tsLastMarkAt = 0 }
            (ts1, first) = consumePoolLayers pt origin 100 ts0
        length first `shouldBe` 1
        tsLastMarkAt ts1 `shouldBe` 100
        -- An arterial jump right afterwards: plenty of volume for many
        -- layers, but no time has passed, so nothing may pop.
        let (ts2, burst) = consumePoolLayers pt origin 100.05
                               ts1 { tsPendingVolume = 20 * ptMinVolume pt }
        burst `shouldBe` []
        tsPendingVolume ts2 `shouldSatisfy` (\v → abs (v - 20 * ptMinVolume pt) < 1e-6)
        -- ...and exactly one lands once a FULL fresh cadence has passed.
        let (_, next) = consumePoolLayers pt origin (100 + ptMinCadence pt) ts1
                            { tsPendingVolume = 20 * ptMinVolume pt }
        length next `shouldBe` 1

    it "a heavy bleed pools FASTER and HEAVIER than a trickle over the same \
       \dwell (#883 requirement 4)" $ do
        let pt      = defaultPoolThresholds
            -- Same 15 s of standing still (short of the layer bound for
            -- BOTH, so this compares rates rather than saturation), same
            -- tick size; only the per-tick external loss differs by 20x.
            trickle = dwell pt 0.0004 150 0.1
            heavy   = dwell pt 0.008  150 0.1
        length (snd heavy) `shouldSatisfy` (> length (snd trickle))
        length (snd trickle) `shouldSatisfy` (> 0)
        meanVolume (snd heavy) `shouldSatisfy` (> meanVolume (snd trickle))

poolBoundSpec ∷ Spec
poolBoundSpec = describe "Blood.Pool — the per-cluster layer bound (#883 requirement 3)" $ do

    it "saturates at EXACTLY ptMaxLayers and then adds nothing more, however \
       \long the unit keeps bleeding in place" $ do
        let pt = defaultPoolThresholds
            -- Far more time and blood than the bound could ever spend.
            (tsSat, ls) = dwell pt 0.05 2000 0.5
        length ls `shouldBe` ptMaxLayers pt
        tsClusterLayers tsSat `shouldBe` ptMaxLayers pt
        poolAtBound pt tsSat `shouldBe` True
        -- Indices are the cluster's own 0..n-1, never restarted.
        map ploIndex ls `shouldBe` [0 .. ptMaxLayers pt - 1]

    it "at the bound the pending blood stays BANKED and the gates stay put \
       \— no discard, and no catch-up burst when movement resumes \
       \(#883 spec addition)" $ do
        let pt = defaultPoolThresholds
            tp = defaultTrailThresholds
            (tsSat, _) = dwell pt 0.05 400 0.5
            -- Keep bleeding in place well past saturation.
            tsMore = tsSat { tsPendingVolume = tsPendingVolume tsSat + 2.0 }
            (tsAfter, more) = consumePoolLayers pt origin 100000 tsMore
        more `shouldBe` []
        tsPendingVolume tsAfter `shouldBe` tsPendingVolume tsMore
        tsDistSinceMark tsAfter `shouldBe` tsDistSinceMark tsMore
        tsLastMarkAt tsAfter    `shouldBe` tsLastMarkAt tsMore
        -- Resuming movement: the FIRST step still has to clear the
        -- trail's own distance gate — a tiny step pops nothing even
        -- though a huge volume and a huge elapsed time are banked.
        let (_, popped) = consumeTrailMarks tp 0.05 0.05 100000.05 tsAfter
        popped `shouldBe` []
        -- And once the distance gate IS cleared, it is ONE mark, not a
        -- burst proportional to the banked volume.
        let (_, popped') = consumeTrailMarks tp (ttMinDistance tp) 0.05
                                              100000.05 tsAfter
        length popped' `shouldBe` 1

    it "leaving the cluster radius grants a fresh budget — a second dwell \
       \elsewhere pools again (the bound is per ACTIVE cluster)" $ do
        let pt = defaultPoolThresholds
            (tsSat, first) = dwell pt 0.05 400 0.5
            -- Walk clean out of the radius, then stand still again.
            (mode, tsMoved) =
                classifyOngoing pt (100, 100) tsSat { tsClusterAnchor = Just (0, 0) }
            (_, second) = dwellFrom pt tsMoved 200000 0.05 400 0.5
        mode `shouldBe` ModeTravel
        length first  `shouldBe` ptMaxLayers pt
        length second `shouldBe` ptMaxLayers pt

    it "the layer count is never re-derived from the decal store, so a \
       \global FIFO eviction cannot reopen the budget" $ do
        -- Structural: consumePoolLayers only ever reads/writes
        -- tsClusterLayers, so an emptied store leaves it untouched.
        let pt = defaultPoolThresholds
            (tsSat, _) = dwell pt 0.05 400 0.5
        tsClusterLayers tsSat `shouldBe` ptMaxLayers pt
        snd (consumePoolLayers pt origin 1e6 tsSat { tsPendingVolume = 5 }) `shouldBe` []

poolPartitionInvarianceSpec ∷ Spec
poolPartitionInvarianceSpec =
  describe "Blood.Pool.consumePoolLayers — timestep-partition invariance" $ do

    it "an evenly-divisible dwell splits into the SAME layers/volume \
       \regardless of tick count" $
        checkPoolPartitionInvariance 8 0.4 80

    it "an UNEVENLY-divisible dwell still matches within the documented \
       \one-layer tolerance" $
        checkPoolPartitionInvariance 6.4 0.37 37

    it "a VOLUME-limited dwell (time alone would allow more layers) matches" $
        -- 10 cadence windows' worth of time but only ~4 layers' worth of
        -- blood: the volume floor, not the clock, sets the count.
        checkPoolPartitionInvariance 10 (defaultPoolVolume * 4) 40

    it "walk-then-stop-then-walk conserves every drop across the seam \
       \(#883 requirement 5)" $ do
        let pt = defaultPoolThresholds
            tp = defaultTrailThresholds
            totalVol = 0.9 ∷ Float
            -- Leg 1: travelling — 3 whole distance/cadence gates.
            ts0 = emptyTrailState { tsPendingVolume = totalVol / 3 }
            (ts1, walked) = consumeTrailMarks tp (3 * ttMinDistance tp)
                                (3 * ttMinCadence tp) (3 * ttMinCadence tp) ts0
            -- Leg 2: stationary — the same accumulator, now pooling.
            -- 200 ticks x 0.05 s = 10 s of standing still, over which
            -- the wound tick credits the SAME per-leg share of blood.
            (ts2, pooled) = dwellFrom pt ts1 (3 * ttMinCadence tp) 0.05 200
                                (totalVol / 3 / 200)
            -- Leg 3: moving again — the trail picks up where it left off.
            ts2' = ts2 { tsPendingVolume = tsPendingVolume ts2 + totalVol / 3 }
            (ts3, walked') = consumeTrailMarks tp (3 * ttMinDistance tp)
                                (3 * ttMinCadence tp)
                                (tsLastMarkAt ts2' + 3 * ttMinCadence tp) ts2'
            emitted = sum (map tmoVolume walked) + sum (map ploVolume pooled)
                    + sum (map tmoVolume walked')
        walked  `shouldSatisfy` (not . null)
        pooled  `shouldSatisfy` (not . null)
        walked' `shouldSatisfy` (not . null)
        (emitted + tsPendingVolume ts3) `shouldSatisfy` (\v → abs (v - totalVol) < 1e-3)

poolPlacementSpec ∷ Spec
poolPlacementSpec = describe "Blood.Pool.poolLayerOffset — placement" $ do

    it "every layer lands within ptJitterRadius of the anchor" $
        mapM_ (\(s, i) → magnitude (poolLayerOffset defaultPoolThresholds s i)
                  `shouldSatisfy` (≤ ptJitterRadius defaultPoolThresholds + 1e-5))
              [ (s, i) | s ← [0, 1, 7, 4242], i ← [0 .. 40] ]

    -- The expectation below is HAND-DERIVED from the formula documented
    -- on 'Blood.Pool.poolLayerOffset', not transcribed from a run of the
    -- implementation, so it is a real oracle rather than a restatement:
    --
    --   n    = max 1 (ptMaxLayers pt)                  = 12
    --   frac = fromIntegral (max 0 (min idx n)) / n    = 4 / 12
    --   r    = max 0 (ptJitterRadius pt) * sqrt frac
    --        = 0.35 * sqrt (1/3)                       = 0.20207259
    --   ang  = idx * 2.399963 + (seed `mod` 360) * (pi / 180)
    --        = 4 * 2.399963 + 99 * (pi / 180)          = 11.32772796
    --   x    = r * cos ang                             =  0.06589178
    --   y    = r * sin ang                             = -0.19102776
    --
    -- Components are compared separately against those hard-coded
    -- numbers; recomputing the formula on the expectation side would
    -- reproduce whatever the implementation does and prove nothing.
    -- 1e-5 is the tolerance this file already uses for Float placement
    -- comparisons, and comfortably covers single-precision rounding
    -- (the derivation above is exact to ~1e-7 of what Float produces).
    --
    -- A failure here means the spiral MOVED — a deliberate decision
    -- about how a pool spreads (the golden angle, the radius growth, or
    -- the seed rotation), to be re-derived from the new formula rather
    -- than copied from the new output.
    it "places seed 99's layer 4 at the golden-angle spiral point the \
       \documented formula gives" $ do
        let (x, y) = poolLayerOffset defaultPoolThresholds 99 4
        x `shouldSatisfy` (\v → abs (v -   0.06589178) < 1e-5)
        y `shouldSatisfy` (\v → abs (v - (-0.19102776)) < 1e-5)

    it "successive layers never land on top of each other (overlapping, \
       \not stacked)" $ do
        let pt = defaultPoolThresholds
            pts = [ poolLayerOffset pt 5 i | i ← [0 .. ptMaxLayers pt - 1] ]
        length (nub pts) `shouldBe` length pts

    it "two adjacent bleeders' clusters are not in lockstep (the seed \
       \rotates the spiral) — #883 requirement 8" $
        poolLayerOffset defaultPoolThresholds 0 3
            `shouldNotBe` poolLayerOffset defaultPoolThresholds 90 3

poolTextureMappingSpec ∷ Spec
poolTextureMappingSpec = describe "Blood.Pool volume -> texture-request mapping" $ do

    it "stays within the pool/drops vocabulary (never smear/spatter/streak)" $
        mapM_ (\v → poolStyleFor v `shouldSatisfy` (`elem` [StylePool, StyleDrops]))
              [0, 0.001, 0.01, 0.05, 0.1, 0.3, 0.4, 1.0, 5.0]

    it "footprint is ALWAYS small — a pool grows by layering, never by one \
       \bigger mark (the immutable-record decision)" $
        mapM_ (\v → pbFootprint (poolBloodForVolume v) `shouldBe` FootprintSmall)
              [0, 0.01, 0.05, 0.4, 5.0]

    it "severity and opacity never get lighter as a layer's volume grows" $ do
        let vols = [0, 0.001, 0.01, 0.05, 0.15, 0.4, 1.0]
            sevs = map (pbSeverity . poolBloodForVolume) vols
            opas = map (pbOpacity  . poolBloodForVolume) vols
        sevs `shouldBe` sort sevs
        opas `shouldBe` sort opas

    it "the documented travel speed above which a unit never pools follows \
       \from the radius and cadence" $
        poolTravelSpeed defaultPoolThresholds `shouldSatisfy`
            (\s → abs (s - ptClusterRadius defaultPoolThresholds
                         / realToFrac (ptMinCadence defaultPoolThresholds)) < 1e-6)

poolImmutabilitySpec ∷ Spec
poolImmutabilitySpec =
  describe "Blood.Pool growth is strictly ADDITIVE (#883 requirement 2 / \
           \acceptance (g))" $
    it "layering never mutates an existing BloodDecal — pre-existing records \
       \compare EQUAL field-for-field afterwards, only new ids appear" $ do
        env ← initEnv
        ws  ← emptyWorldState
        writeIORef (worldManagerRef env) (emptyWorldManager
            { wmWorlds = [(pageA, ws)]
            , wmVisible = [pageA] })
        now ← readIORef (gameTimeRef env)
        let wsc = toWorldSimCapability env
            pt  = defaultPoolThresholds
            layer i = let (dx, dy) = poolLayerOffset pt 3 i
                      in spawnPoolLayer wsc pageA (10 + dx) (10 + dy) 0 "slash"
                             0.06 0 (3 + i) (Just (UnitId 1)) now
        mapM_ layer [0 .. 2]
        before ← allDecals . bstDecals <$> readIORef (wsBloodStoreRef ws)
        length before `shouldBe` 3
        mapM_ layer [3 .. 6]
        after ← allDecals . bstDecals <$> readIORef (wsBloodStoreRef ws)
        length after `shouldBe` 7
        -- Whole-record equality (derived Eq on BloodDecal covers every
        -- field, including the ones blood.listDecals cannot expose) for
        -- each pre-existing id, and no id was reused.
        let byId = HM.fromList [ (bdeId d, d) | d ← after ]
        mapM_ (\d → HM.lookup (bdeId d) byId `shouldBe` Just d) before
        length (nub (map bdeId after)) `shouldBe` 7

-- ----- #883 shared helpers -----

-- | 'ptMinVolume' of the defaults, as a plain value so test names can
--   talk about "four layers' worth of blood" without repeating the
--   record accessor.
defaultPoolVolume ∷ Float
defaultPoolVolume = ptMinVolume defaultPoolThresholds

-- | The unit position handed to 'consumePoolLayers' in the cases that
--   are about cadence/volume/bound math rather than placement.
origin ∷ (Float, Float)
origin = (0, 0)

magnitude ∷ (Float, Float) → Float
magnitude (x, y) = sqrt (x * x + y * y)

meanVolume ∷ [PoolLayerOut] → Float
meanVolume [] = 0
meanVolume ls = sum (map ploVolume ls) / fromIntegral (length ls)

-- | Simulate a unit standing still and bleeding: @ticks@ ticks of
--   @dt@ seconds each, crediting @volPerTick@ litres of external loss
--   per tick (exactly what "Combat.Wounds.Tick" does), from a fresh
--   accumulator at game time 0.
dwell ∷ PoolThresholds → Float → Int → Double → (TrailState, [PoolLayerOut])
dwell pt volPerTick ticks dt = dwellFrom pt emptyTrailState 0 dt ticks volPerTick

-- | 'dwell' from an arbitrary starting accumulator and clock — used to
--   test a SECOND dwell after the unit walked away, and the pooling leg
--   of a walk/stop/walk journey.
dwellFrom
    ∷ PoolThresholds → TrailState → Double → Double → Int → Float
    → (TrailState, [PoolLayerOut])
dwellFrom pt ts0 clock0 dt ticks volPerTick = go ts0 clock0 [] ticks
  where
    go ts _     acc 0 = (ts, acc)
    go ts clock acc k =
        let clock' = clock + dt
            ts1 = ts { tsPendingVolume = tsPendingVolume ts + volPerTick }
            (ts2, popped) = consumePoolLayers pt origin clock' ts1
        in go ts2 clock' (acc ++ popped) (k - 1 ∷ Int)

-- | The pool's answer to 'checkPartitionInvariance': the same dwell
--   (@cadenceGates@ multiples of 'ptMinCadence') and the same external
--   loss budget, delivered as ONE big update vs @steps@ small ones,
--   must converge on the same layer count and conserve volume exactly
--   in both — layer density must not depend on tick size (#883
--   requirement 7).
checkPoolPartitionInvariance ∷ Double → Float → Int → Expectation
checkPoolPartitionInvariance cadenceGates totalVol steps = do
    let pt = defaultPoolThresholds
        totalTime = cadenceGates * ptMinCadence pt
        (bigFinal, bigLayers) =
            consumePoolLayers pt origin totalTime emptyTrailState { tsPendingVolume = totalVol }
        (smallFinal, smallLayers) =
            dwellFrom pt emptyTrailState 0 (totalTime / fromIntegral steps)
                      steps (totalVol / fromIntegral steps)
        conserved final ls = sum (map ploVolume ls) + tsPendingVolume final
    conserved bigFinal   bigLayers   `shouldSatisfy` (\v → abs (v - totalVol) < 1e-4)
    conserved smallFinal smallLayers `shouldSatisfy` (\v → abs (v - totalVol) < 1e-3)
    abs (length smallLayers - length bigLayers) `shouldSatisfy` (≤ 1)
