{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Tests for the pure bleeding-trail accumulator/mapping math (issue
--   #882): "Blood.Trail"'s 'consumeTrailMarks' (distance+cadence
--   gating, conserved volume, partition invariance) and its volume ->
--   texture-request mapping, plus "Combat.Wounds.Bleed"'s external- vs
--   internal-bleed kind classification the wound tick's conserved
--   external-loss accounting relies on.
module Test.Headless.Blood.Trail (spec) where

import UPrelude
import Test.Hspec
import Data.List (sort, nub)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef, readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Unit.Direction (Direction(..))
import Unit.Sim.Types (emptyUnitThreadState)
import Unit.Thread.Command.Lifecycle (handleUnitDestroyCommand)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldState)
import World.Save.Types (toUnitSnapshot, fromUnitSnapshot)
import Infection.Types (emptyInfectionManager)
import Combat.Wounds (tickOneUnit)
import qualified System.Random as Random
import Blood.Trail
import Blood.Types (BloodStyle(..), BloodStore(..), allDecals)
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
        let ts0 = TrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (ts', marks) = consumeTrailMarks defaultTrailThresholds 0.1 100.0 100.0 ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0   -- untouched — nothing consumed

    it "pops nothing when cadence hasn't elapsed, even with ample distance and volume" $ do
        let ts0 = TrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0, tsLastMarkAt = 100.0 }
            (ts', marks) = consumeTrailMarks defaultTrailThresholds 50.0 0.01 100.01 ts0
        marks `shouldBe` []
        tsPendingVolume ts' `shouldBe` 5.0

    it "pops exactly one mark once BOTH gates are cleared" $ do
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 0.2, tsDistSinceMark = 0, tsLastMarkAt = 0 }
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
            ts0 = TrailState { tsPendingVolume = 0.5
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
            ts0 = TrailState { tsPendingVolume = 5.0, tsDistSinceMark = 0.9, tsLastMarkAt = 0 }
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
            ts0 = TrailState { tsPendingVolume = 0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
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
            ts0 = TrailState { tsPendingVolume = 3.7, tsDistSinceMark = 0, tsLastMarkAt = 0 }
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
            ts0 = TrailState { tsPendingVolume = 1.0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
            (_, marks) = consumeTrailMarks tp (10 * ttMinDistance tp)
                                            (10 * ttMinCadence tp) (10 * ttMinCadence tp) ts0
            fracs = map tmoFraction marks
        length marks `shouldBe` 10
        fracs `shouldBe` sort fracs   -- non-decreasing, so spread in order
        fracs `shouldBe` nub fracs    -- and all distinct (evenly spaced)
        all (\f → f > 0 ∧ f ≤ 1) fracs `shouldBe` True

    it "leftover distance/time banked below a gate carries forward, never lost" $ do
        let tp  = defaultTrailThresholds
            ts0 = TrailState { tsPendingVolume = 0, tsDistSinceMark = 0, tsLastMarkAt = 0 }
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
    EngineInitResult env ← initializeEngineHeadless
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
        writeIORef (worldManagerRef env) (WorldManager [(pageA, wsA), (pageB, wsB)] [pageA])
        now ← readIORef (gameTimeRef env)
        spawnTrailMark env pageB 5 5 0 "slash" 0.1 0 0 Nothing now
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
    , uiAccessories = [], uiFactionId = "player", uiWounds = []
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
            liveTs = TrailState { tsPendingVolume = 0.4, tsDistSinceMark = 0.2, tsLastMarkAt = 5.0 }
        writeIORef (unitManagerRef env)
            (emptyUnitManager { umDefs = HM.singleton "t" minimalDef
                              , umInstances = HM.singleton uid (minimalInst pageA (Just liveTs)) })
        utsRef ← newIORef emptyUnitThreadState
        handleUnitDestroyCommand env utsRef uid
        um' ← readIORef (unitManagerRef env)
        HM.lookup uid (umInstances um') `shouldBe` Nothing

    it "a save/load round-trip resets the trail accumulator, even if the original session had one" $ do
        let uid = UnitId 1
            liveTs = TrailState { tsPendingVolume = 0.4, tsDistSinceMark = 0.2, tsLastMarkAt = 5.0 }
            defs = HM.singleton "t" minimalDef
            um0 = emptyUnitManager
                { umDefs = defs, umInstances = HM.singleton uid (minimalInst pageA (Just liveTs)) }
            snap = toUnitSnapshot pageA um0
            (um1, orphans) = fromUnitSnapshot pageA defs snap
        orphans `shouldBe` []
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
