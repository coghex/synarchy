{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The combat thread's wound fold under each blood mode (#2837, S10;
--   T-S10-1 and the I1/I3 trajectories): the SAME pure per-unit tick the
--   combat thread folds inside its one atomic unit-manager update
--   ('tickOneUnitWith'), run under the SHIPPED registry.
--
--   What is established: a Nominal blood debits no volume, recovers none and
--   raises no blood-loss collapse or exsanguination, while every wound
--   effect (clotting, healing, infection, necrosis, gangrene death, the
--   bleed rate itself) is identical to a Simulated twin's, so Nominal is not
--   injury immunity. The bleeding-trail accumulator is credited the
--   wound-derived external flow DIRECTLY under Nominal (the one explicit
--   exception to #882's conserved accounting), keeps its conserved external
--   share under Simulated, and is cleared the instant external bleeding
--   reaches zero in both. Nothing is rolled: every input is a literal.
module Test.Headless.Unit.PhysiologyBloodModes.WoundFold (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import qualified System.Random as Random
import Blood.Pool (consumePoolLayers, defaultPoolThresholds)
import Blood.Trail (consumeTrailMarks, defaultTrailThresholds, TrailMarkOut(..))
import Combat.Wounds (WoundTickOutcome(..), tickOneUnitWith)
import Combat.Wounds.Bleed (bleedRateFor, externalBleedRateFor)
import Infection.Types (InfectionDef(..), InfectionManager(..), emptyInfectionManager)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology.Types
import Unit.Types
import World.Page.Types (WorldPageId(..))

spec ∷ Spec
spec = beforeAll readShippedPhysiologyRegistry $
    describe "the wound fold under each blood mode" $ do
        twinSpec
        flowSpec
        cleanupSpec
        marksSpec
        deathSpec
        trajectorySpec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | A definition with one non-vital and one vital part; blood Simulated by
--   default, every other system Nominal so no pool is read.
fixtureDef ∷ UnitDef
fixtureDef = UnitDef
    { udName = "blood_fold", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing, udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts = [part "torso" False, part "head" True]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [], udFactionTags = []
    , udPhysiology = plainUnitPhysiology True Nothing $ HM.fromList
        ([ (s, PhysioNominal)
         | s ← ["thermo", "salt", "hydration", "nutrition", "stamina"
               , "exhaustion", "sleep", "cardio"] ]
         ⧺ [("blood", PhysioSimulated)]) }
  where
    part pid vital = BodyPart
        { bpId = pid, bpName = pid, bpParent = Nothing
        , bpVital = vital, bpAreaWeight = 1.0, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
        , bpLayers = [], bpTargetable = True, bpDepth = 0.0
        , bpAffectsLocomotion = False, bpAffectsBalance = False }

maxBlood ∷ Float
maxBlood = 70 * bloodMassRatio

-- | A unit with the given blood volume and mode PIN (Nothing = the
--   definition's Simulated default), wounds and trail accumulator.
unit ∷ Maybe PhysioMode → Float → [Wound] → Maybe TrailState → UnitInstance
unit pin blood ws ts = UnitInstance
    { uiDefName = "blood_fold", uiName = "", uiPage = WorldPageId "blood_fold"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList
        [ ("body_mass", 70), ("constitution", 1), ("max_hydration", 42)
        , ("max_hunger", 700), ("max_calories", 1400), ("endurance", 1) ]
    , uiModifiers = HM.empty, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFaction = resolveLegacyFaction [] FactionPlayer, uiWounds = ws
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = blood, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiPhysioOverrides = maybe HM.empty (\m → HM.singleton "blood" m) pin
    , uiTrailState = ts }

simulated, nominal ∷ Float → [Wound] → Maybe TrailState → UnitInstance
simulated = unit (Just PhysioSimulated)
nominal   = unit (Just PhysioNominal)

-- | A clean wound (no infection is ever eligible).
cut ∷ Text → Text → Float → Float → Float → Wound
cut part kind sev bandage clot = Wound
    { woundPart = part, woundKind = kind, woundSeverity = sev
    , woundAt = 0, woundBandage = bandage, woundClot = clot, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = True
    , woundInfectionType = "", woundNecrosis = 0.0 }

pendingVol ∷ UnitInstance → Maybe Float
pendingVol = fmap tsPendingVolume ∘ uiTrailState

-- | @n@ ticks of @dt@ under an infection manager, with the outcomes.
runWith ∷ InfectionManager → PhysiologyRegistry → Int → Float → UnitInstance
        → (UnitInstance, [WoundTickOutcome])
runWith inf reg n dt inst0 = go n inst0 (Random.mkStdGen 7) []
  where
    go k i g acc
        | k ≤ 0 = (i, reverse acc)
        | otherwise =
            let (i', o, g') = tickOneUnitWith reg 100 fixtureDef dt inf Nothing g i False
            in go (k - 1) i' g' (o : acc)

run ∷ PhysiologyRegistry → Int → Float → UnitInstance
    → (UnitInstance, [WoundTickOutcome])
run = runWith emptyInfectionManager

quiet ∷ [WoundTickOutcome] → Bool
quiet = all (\o → case o of NoChange → True; _ → False)

near ∷ Float → Float → Float → Bool
near tol want got = abs (got - want) < tol

-----------------------------------------------------------------------
-- A Nominal twin differs from a Simulated one only in volume
-----------------------------------------------------------------------

twinSpec ∷ SpecWith PhysiologyRegistry
twinSpec = describe "twin (I1/I3, T-S10-1)" $ do
    it "leaves a severe external slash's volume at the stale stored value, \
       \raises no collapse or exsanguination, and still progresses every \
       \wound field exactly as a Simulated twin does" $ \reg → do
        let w = cut "torso" "slash" 0.5 1.0 0.0
            (n, no) = run reg 30 0.1 (nominal 3.0 [w] Nothing)
            (s, so) = run reg 30 0.1 (simulated 3.0 [w] Nothing)
        -- the Nominal volume is untouched (a stale partial reading, never read)
        uiBlood n `shouldBe` 3.0
        quiet no `shouldBe` True
        -- the Simulated twin really drained (so this is not comparing constants)
        uiBlood s `shouldSatisfy` (< 2.2)
        quiet so `shouldBe` True
        -- every wound field, scar, immunity: identical
        uiWounds n `shouldBe` uiWounds s
        uiScars n `shouldBe` uiScars s
        -- the bleed rate is live under Nominal
        bleedRateFor fixtureDef n `shouldSatisfy` (> 0.05)
        bleedRateFor fixtureDef n `shouldBe` bleedRateFor fixtureDef s
        -- clotting progressed: the wound is not frozen
        [ woundClot x | x ← uiWounds n ] `shouldSatisfy` all (> 0)

    it "raises no blood-loss collapse or exsanguination for a Nominal unit \
       \whose stored volume is zero, where the same Simulated unit dies" $ \reg → do
        let w = cut "torso" "slash" 0.9 1.0 0.0
            (_, no) = run reg 3 0.5 (nominal 0.2 [w] Nothing)
            (_, so) = run reg 3 0.5 (simulated 0.2 [w] Nothing)
        quiet no `shouldBe` True
        [ c | DiedNow _ c ← so ] `shouldBe` ["exsanguination"]
        -- and a low-but-alive stored volume collapses a Simulated unit only
        let (_, no2) = run reg 1 0.1 (nominal 1.0 [cut "torso" "slash" 0.2 1.0 0.0] Nothing)
            (_, so2) = run reg 1 0.1 (simulated 1.0 [cut "torso" "slash" 0.2 1.0 0.0] Nothing)
        quiet no2 `shouldBe` True
        [ () | UnconsciousNow _ ← so2 ] `shouldBe` [()]

-----------------------------------------------------------------------
-- The visual flow
-----------------------------------------------------------------------

flowSpec ∷ SpecWith PhysiologyRegistry
flowSpec = describe "visual flow (T-S10-1)" $ do
    it "credits the accumulator the wound-derived external flow under \
       \Nominal, equal to the conserved share a non-fatal Simulated twin \
       \credits (the flow is the same, only the bookkeeping differs)" $ \reg → do
        let w = cut "torso" "slash" 0.5 1.0 0.0
            (n, _) = run reg 30 0.1 (nominal 5.0 [w] Nothing)
            (s, _) = run reg 30 0.1 (simulated 5.0 [w] Nothing)
        pendingVol n `shouldSatisfy` maybe False (> 0.5)
        pendingVol n `shouldSatisfy` maybe False (\p → near 1e-3 (maybe 0 id (pendingVol s)) p)

    it "keeps the explicit exception: with ZERO volume left a Simulated unit \
       \credits no trail (nothing was removed), a Nominal one the full flow" $ \reg → do
        let w = cut "torso" "slash" 0.5 1.0 0.0
            (n, _) = run reg 10 0.1 (nominal 0 [w] Nothing)
            (s, _) = run reg 10 0.1 (simulated 0 [w] Nothing)
        pendingVol s `shouldSatisfy` (\p → p ≡ Nothing ∨ p ≡ Just 0)
        pendingVol n `shouldSatisfy` maybe False (> 0.2)

    it "credits an internal-only wound no external flow, though it bleeds, \
       \and drains no Nominal volume" $ \reg → do
        let w = cut "torso" "internal" 0.7 1.0 0.0
            (n, no) = run reg 30 0.1 (nominal 4.0 [w] Nothing)
        externalBleedRateFor fixtureDef n `shouldBe` 0
        bleedRateFor fixtureDef n `shouldSatisfy` (> 0)
        uiTrailState n `shouldBe` Nothing
        uiBlood n `shouldBe` 4.0
        quiet no `shouldBe` True

-----------------------------------------------------------------------
-- Zero-bleed cleanup versus reduced bleeding
-----------------------------------------------------------------------

cleanupSpec ∷ SpecWith PhysiologyRegistry
cleanupSpec = describe "zero-bleed cleanup (T-S10-1)" $ do
    let banked = Just (TrailState 0.3 0 0 Nothing 0)
        slash b c = cut "torso" "slash" 0.6 b c
    forM_ [("Nominal", nominal), ("Simulated", simulated)] $ \(label, mk) → do
        it (label <> ": a fully sealing dressing (seep 0) clears the pendingVol \
            \flow at once and stops further marks") $ \reg → do
            let (i, _) = run reg 1 0.1 (mk 4.0 [slash 0.0 0.0] banked)
            uiTrailState i `shouldBe` Nothing
            let (j, _) = run reg 20 0.1 (mk 4.0 [slash 0.0 0.0] banked)
            uiTrailState j `shouldBe` Nothing
        it (label <> ": a fully clotted wound clears the pending flow at once") $ \reg → do
            let (i, _) = run reg 1 0.1 (mk 4.0 [slash 1.0 1.0] banked)
            uiTrailState i `shouldBe` Nothing
        it (label <> ": a competent dressing (seep 0.05) keeps a REDUCED positive \
            \flow and continued marks, not a stop") $ \reg → do
            let (open, _) = run reg 10 0.1 (mk 4.0 [slash 1.0 0.0] Nothing)
                (dressed, _) = run reg 10 0.1 (mk 4.0 [slash 0.05 0.0] Nothing)
                p0 = fromMaybe 0 (pendingVol open)
                p1 = fromMaybe 0 (pendingVol dressed)
            p1 `shouldSatisfy` (> 0)
            p1 `shouldSatisfy` (< 0.2 * p0)
            externalBleedRateFor fixtureDef dressed `shouldSatisfy` (> 0)

-----------------------------------------------------------------------
-- Real marks and pools come out of the credited accumulator
-----------------------------------------------------------------------

marksSpec ∷ SpecWith PhysiologyRegistry
marksSpec = describe "marks and pools (T-S10-1)" $
    it "turns a Nominal accumulator into trail marks while moving and pool \
       \layers while stationary, conserving the credited volume" $ \reg → do
        let w = cut "torso" "slash" 0.6 1.0 0.0
            (n, _) = run reg 30 0.1 (nominal 5.0 [w] Nothing)
        case uiTrailState n of
            Nothing → expectationFailure "no accumulator under a bleeding Nominal unit"
            Just ts → do
                let credited = tsPendingVolume ts
                    anchored = ts { tsDistSinceMark = 0, tsLastMarkAt = 0 }
                    (ts', marks) = consumeTrailMarks defaultTrailThresholds 2.0 3.0 3.0 anchored
                marks `shouldSatisfy` (not ∘ null)
                sum (map tmoVolume marks) + tsPendingVolume ts'
                    `shouldSatisfy` (\v → near 1e-4 credited v)
                -- stationary: the same accumulator pools instead
                let (ps, layers) = consumePoolLayers defaultPoolThresholds (0, 0) 10.0 anchored
                length layers `shouldSatisfy` (> 0)
                tsPendingVolume ps `shouldSatisfy` (< credited)

-----------------------------------------------------------------------
-- Gangrene and necrosis stay possible
-----------------------------------------------------------------------

rot ∷ InfectionManager
rot = InfectionManager $ HM.singleton "rot" InfectionDef
    { infId = "rot", infName = "Rot", infIcon = "rot", infCategory = "bacterial"
    , infSites = ["surface", "deep"], infBaseWeight = 1
    , infTempMin = -100, infTempMax = 100, infMoistMin = 0, infMoistMax = 1
    , infAggressiveness = 1, infInfectability = 1
    , infCurableBy = [], infCureRate = 1, infWoundInfectable = True
    , infEffects = ["necrosis"], infTransmissibility = 0, infTransmission = [] }

deathSpec ∷ SpecWith PhysiologyRegistry
deathSpec = describe "injury death and necrosis (T-S10-1)" $ do
    it "kills a Nominal unit by gangrene of a vital part, like a Simulated one" $ \reg → do
        let w = (cut "head" "slash" 0.3 1.0 0.9) { woundNecrosis = 0.9 }
            (_, no) = run reg 1 0.1 (nominal 5.0 [w] Nothing)
            (_, so) = run reg 1 0.1 (simulated 5.0 [w] Nothing)
        [ c | DiedNow _ c ← no ] `shouldBe` ["gangrene"]
        [ c | DiedNow _ c ← so ] `shouldBe` ["gangrene"]

    it "grows necrosis in an established necrotic infection under Nominal \
       \exactly as under Simulated" $ \reg → do
        let w = (cut "torso" "slash" 0.4 1.0 0.5)
                    { woundInfection = 0.9, woundInfectionType = "rot", woundClean = False }
            (n, _) = runWith rot reg 50 0.1 (nominal 5.0 [w] Nothing)
            (s, _) = runWith rot reg 50 0.1 (simulated 5.0 [w] Nothing)
        [ woundNecrosis x | x ← uiWounds n ] `shouldSatisfy` all (> 0)
        uiWounds n `shouldBe` uiWounds s

-----------------------------------------------------------------------
-- Controlled bleed / stabilize / recover trajectory over 600 tick-seconds
-----------------------------------------------------------------------

trajectorySpec ∷ SpecWith PhysiologyRegistry
trajectorySpec = describe "600 tick-second trajectory (I1/I3)" $ do
    it "bleeds, stabilizes and recovers a Simulated unit at the #2639 rate \
       \(a bounded climb, blocked by ANY aggregate bleeding, never above the \
       \maximum), while a Nominal twin's volume never moves" $ \reg → do
        let w0 = cut "torso" "slash" 0.4 1.0 0.0
            (bled, _) = run reg 100 0.1 (simulated maxBlood [w0] Nothing)
            -- first aid: the dressing seals the wound completely
            sealed = bled { uiWounds = [ w { woundBandage = 0.0 } | w ← uiWounds bled ] }
            (rec1, _) = run reg 100 1.0 sealed
            (rec2, _) = run reg 500 1.0 rec1
            perSecond = (uiBlood rec2 - uiBlood rec1) / 500
        uiBlood bled `shouldSatisfy` (< maxBlood - 1)
        -- one tick-second of #2639 recovery at healthy nutrition, constitution 1
        perSecond `shouldSatisfy` (\r → r > 0.0008 ∧ r < 0.0009)
        uiBlood rec2 `shouldSatisfy` (≤ maxBlood)
        -- a still-seeping wound anywhere blocks recovery outright
        let seeping = bled { uiWounds = uiWounds bled ⧺ [cut "torso" "internal" 0.3 1.0 0.0
                                                        , cut "torso" "fracture" 0.9 1.0 0.0] }
            sealedAll = seeping { uiWounds =
                [ w { woundBandage = if woundKind w ≡ "slash" then 0 else woundBandage w }
                | w ← uiWounds seeping ] }
            (blocked, _) = run reg 100 1.0 sealedAll
        uiBlood blocked `shouldSatisfy` (≤ uiBlood bled + 1e-4)
        -- the Nominal twin: nothing moves in 600 tick-seconds
        let (n, no) = run reg 600 1.0 (nominal 2.5 [] Nothing)
        uiBlood n `shouldBe` 2.5
        quiet no `shouldBe` True
