{-# LANGUAGE Strict #-}
-- | Tests for the per-wound INFECTION mechanic in Combat.Wounds.tickOneUnit.
--   Infection is DETERMINISTIC and gated on cleanliness: an open, un-
--   disinfected wound accrues `woundInfection` after a grace period; a
--   wound marked `woundClean` (antiseptic) never does; a heavily-infected
--   wound stops healing and WORSENS (woundHeal reverses).
module Test.Headless.Combat.Wounds (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.IORef (writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, newBareLuaBackend)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Combat.Wounds
    ( tickOneUnit, bleedRateFor, externalBleedRateFor
    , WoundTickOutcome(..) )
import Combat.Wounds.Constants (woundCleanupThreshold)
import Infection.Types (InfectionManager(..), InfectionDef(..)
                       , emptyInfectionManager)
import qualified System.Random as Random

-- An infection catalogue with one bug carrying the given effect tags, keyed
-- "bug". Used to drive the necrosis path deterministically.
mgrWith ∷ [Text] → InfectionManager
mgrWith effs = InfectionManager (HM.fromList [("bug", d)])
  where d = InfectionDef
            { infId = "bug", infName = "Bug", infIcon = "", infCategory = "bacterial"
            , infSites = ["surface"], infBaseWeight = 1
            , infTempMin = -50, infTempMax = 50, infMoistMin = 0, infMoistMax = 1
            , infAggressiveness = 1, infInfectability = 1
            , infCurableBy = ["antibiotics"], infCureRate = 1
            , infWoundInfectable = True, infEffects = effs
            , infTransmissibility = 0, infTransmission = [] }

-- A minimal single-part body (one targetable "l_thigh").
def ∷ UnitDef
def = UnitDef
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
            { bpId = "l_thigh", bpName = "l_thigh", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 0.1, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- A unit carrying exactly the given wounds, with plenty of blood so a
-- bleed can't kill it mid-test (we're testing infection, not death).
inst ∷ [Wound] → UnitInstance
inst ws = UnitInstance
    { uiDefName = "t", uiName = "", uiPage = WorldPageId "test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList [("body_mass", 70), ("constitution", 1.0)]
    , uiModifiers = HM.empty, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = FactionNeutral, uiWounds = ws, uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty, uiBlood = 100
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing }

-- A wound built with explicit infection-relevant fields.
mkWound ∷ Text → Float → Float → Float → Bool → Wound
mkWound kind sev heal infection clean = Wound
    { woundPart = "l_thigh", woundKind = kind, woundSeverity = sev
    , woundAt = 0, woundBandage = 0.05   -- pre-dressed so it can't bleed out
    , woundClot = 0.0, woundHeal = heal, woundDressing = "bandage"
    , woundInfection = infection, woundClean = clean, woundInfectionType = ""
    , woundNecrosis = 0.0 }

-- Tick one unit once at game-time `gt` with step `dt`; return the first
-- (and only) wound afterward. No infection catalogue / climate (empty mgr +
-- Nothing) so these tests exercise the pure clot/heal/infection-growth math
-- with type selection inert and aggressiveness/climate factors = 1.0.
tick1 ∷ Double → Float → Wound → Wound
tick1 gt dt w =
    let (inst', _, _) = tickOneUnit gt def dt emptyInfectionManager Nothing
                            (Random.mkStdGen 1) (inst [w]) False
    in firstWound inst'

-- | Total 'head': every unit in this spec is built with exactly one wound.
firstWound ∷ UnitInstance → Wound
firstWound i = case uiWounds i of
    w : _ → w
    []    → error "Wounds spec: unit has no wounds"

spec ∷ Spec
spec = do
  effSeveritySpec
  tickEffSeverityLockstepSpec
  bloodRecoverySpec
  reviveGateSpec
  describe "Combat.Wounds infection" $ do

    it "a dirty open wound accrues infection after the grace period" $ do
        -- gt 100 s is past the 60 s grace; a big dt makes the growth clear.
        let w' = tick1 100 10 (mkWound "slash" 0.5 0.0 0.0 False)
        woundInfection w' `shouldSatisfy` (> 0.005)

    it "a disinfected (clean) wound never accrues infection" $ do
        let w' = tick1 100 10 (mkWound "slash" 0.5 0.0 0.0 True)
        woundInfection w' `shouldBe` 0.0

    it "no infection grows within the grace period" $ do
        -- gt 10 s is inside the 60 s grace window.
        let w' = tick1 10 10 (mkWound "slash" 0.5 0.0 0.0 False)
        woundInfection w' `shouldBe` 0.0

    it "a closed concussion does not get infected (kind factor 0)" $ do
        let w' = tick1 100 10 (mkWound "concussion" 0.5 0.0 0.0 False)
        woundInfection w' `shouldBe` 0.0

    it "a heavily-infected wound WORSENS — healing reverses" $ do
        -- infection 0.9 > worsen threshold (0.6); start partly healed so
        -- there's room to reverse. Clean=True isolates the worsen term from
        -- further growth.
        let w' = tick1 100 10 (mkWound "slash" 0.5 0.5 0.9 True)
        woundHeal w' `shouldSatisfy` (< 0.5)

    it "infection gates healing — a clean wound heals faster than an infected one" $ do
        let cleanW = tick1 100 10 (mkWound "slash" 0.5 0.2 0.0 True)
            infW   = tick1 100 10 (mkWound "slash" 0.5 0.2 0.8 True)
        woundHeal cleanW `shouldSatisfy` (> woundHeal infW)

    it "a NECROTIC infection rots tissue (woundNecrosis grows)" $ do
        -- High infection of a bug tagged "necrosis" → dead tissue accrues.
        let w  = (mkWound "slash" 0.5 0.0 0.8 False) { woundInfectionType = "bug" }
            (i', _, _) = tickOneUnit 100 def 10 (mgrWith ["necrosis"]) Nothing
                            (Random.mkStdGen 1) (inst [w]) False
        woundNecrosis (firstWound i') `shouldSatisfy` (> 0.01)

    it "a NON-necrotic infection does NOT rot tissue" $ do
        let w  = (mkWound "slash" 0.5 0.0 0.8 False) { woundInfectionType = "bug" }
            (i', _, _) = tickOneUnit 100 def 10 (mgrWith []) Nothing
                            (Random.mkStdGen 1) (inst [w]) False
        woundNecrosis (firstWound i') `shouldBe` 0.0

    it "necrosis needs an ESTABLISHED infection (low infection doesn't rot)" $ do
        -- infection 0.1 < necrosisInfThreshold → no rot even for a necrotic bug.
        let w  = (mkWound "slash" 0.5 0.0 0.1 False) { woundInfectionType = "bug" }
            (i', _, _) = tickOneUnit 100 def 10 (mgrWith ["necrosis"]) Nothing
                            (Random.mkStdGen 1) (inst [w]) False
        woundNecrosis (firstWound i') `shouldBe` 0.0

    it "fever (high core temp) slows infection growth" $ do
        -- A dirty, already-infected wound; compare growth at a febrile core
        -- (41°C) vs normal (37°C). The fever should suppress the growth.
        let w = (mkWound "slash" 0.5 0.0 0.3 False)
            withCore c = (inst [w])
                { uiStats = HM.fromList
                    [("body_mass", 70), ("constitution", 1.0), ("core_temp", c)] }
            grow c = let (i', _, _) = tickOneUnit 100 def 10 emptyInfectionManager
                                        Nothing (Random.mkStdGen 1) (withCore c) False
                     in woundInfection (firstWound i')
        grow 41.0 `shouldSatisfy` (< grow 37.0)

-- The one definition of effective severity, which every consumer (bleed
-- display, medic targeting, injured-anim, pain, movement, attack-gate)
-- routes through — including the wound tick itself, which calls it on
-- the wound it has just advanced. These four cases pin the formula;
-- 'tickEffSeverityLockstepSpec' below pins the tick's use of it.
effSeveritySpec ∷ Spec
effSeveritySpec = describe "Wound.woundEffSeverity" $ do

    it "a fresh wound's effective severity equals its inflicted severity" $
        woundEffSeverity (mkWound "slash" 0.5 0.0 0.0 False) `shouldBe` 0.5

    it "healing eases effective severity (sev × (1 − heal))" $
        -- 0.8 inflicted, half-healed → 0.4.
        woundEffSeverity (mkWound "slash" 0.8 0.5 0.0 False) `shouldBe` 0.4

    it "necrosis is a permanent floor below which healing can't drop it" $
        -- 0.5 inflicted healed to 0.9 → 0.05 by heal, but 0.3 necrosis floors it.
        woundEffSeverity ((mkWound "slash" 0.5 0.9 0.0 False)
                            { woundNecrosis = 0.3 }) `shouldBe` 0.3

    it "a festering wound (heal reversed below 0) climbs above the inflicted value" $
        -- heal −0.5 → sev × 1.5 = 0.75 > the inflicted 0.5.
        woundEffSeverity (mkWound "slash" 0.5 (-0.5) 0.0 False) `shouldBe` 0.75

-- The tick does not respell the formula: the effective severity it bleeds
-- and cleans up on is 'woundEffSeverity' of the wound it hands back. A
-- helper-only test cannot see that, so both cases here run a REAL
-- 'tickOneUnit' and observe the value through the tick's own behavior —
-- blood removed, and the healed-out decision — on wounds where the
-- necrosis floor (not the acute severity term) is what governs.
tickEffSeverityLockstepSpec ∷ Spec
tickEffSeverityLockstepSpec = describe "Combat.Wounds tick effective severity" $ do

    it "the tick's bleed uses woundEffSeverity of the wound it returns" $ do
        -- 0.3 of dead tissue floors effective severity far above the acute
        -- term (0.5 × (1 − 0.9) = 0.05), and a "necrosis"-tagged infection
        -- makes the rot GROW during the tick — so the governing value
        -- exists only on the POST-tick wound. Untreated (bandage 1.0) so
        -- the drain is comfortably readable back out of uiBlood.
        let dt     = 10
            w      = (mkWound "slash" 0.5 0.9 0.5 False)
                       { woundBandage = 1.0, woundNecrosis = 0.3
                       , woundInfectionType = "bug" }
            before = inst [w]
            (after, _, _) = tickOneUnit 100 def dt (mgrWith ["necrosis"]) Nothing
                                (Random.mkStdGen 1) before False
            w'     = firstWound after
            -- What the tick actually did: blood removed over the step.
            observed  = (uiBlood before - uiBlood after) / dt
            -- What woundEffSeverity of the RETURNED wound predicts, through
            -- the same public per-wound bleed formula.
            expected  = bleedRateFor def after
            -- What the acute term alone would have predicted. Far smaller,
            -- which is what makes the equality above discriminating rather
            -- than a tautology.
            acuteOnly = bleedRateFor def
                          (after { uiWounds = [w' { woundNecrosis = 0 }] })
        -- Preconditions: the rot really advanced, and it really governs.
        woundNecrosis w' `shouldSatisfy` (> 0.3)
        woundNecrosis w' `shouldSatisfy`
            (> woundSeverity w' * (1 - woundHeal w'))
        expected `shouldSatisfy` (> 4 * acuteOnly)
        -- Lockstep. The blood readback costs a few ulps against uiBlood's
        -- magnitude, hence a relative bound rather than shouldBe.
        abs (observed - expected) `shouldSatisfy` (< 1e-3 * expected)

    it "the tick's cleanup decision uses woundEffSeverity, not the acute term" $ do
        -- Acute severity 0.5 × (1 − 0.995) is under the 0.01 cleanup
        -- threshold; 0.05 of dead tissue is not. Clean, uninfected and with
        -- no infection catalogue, so necrosis holds still and the ONLY
        -- difference between the two runs is the floor.
        let base   = mkWound "slash" 0.5 0.995 0.0 True
            rotted = base { woundNecrosis = 0.05 }
            run w  = let (i', _, _) = tickOneUnit 100 def 10 emptyInfectionManager
                                        Nothing (Random.mkStdGen 1) (inst [w]) False
                     in uiWounds i'
        -- Acute severity alone heals it out …
        run base `shouldBe` []
        -- … while the necrosis floor keeps it, at exactly the effective
        -- severity woundEffSeverity reports for the wound that survived.
        case run rotted of
            [w'] → do
                woundEffSeverity w' `shouldBe` 0.05
                woundEffSeverity w' `shouldSatisfy` (≥ woundCleanupThreshold)
            ws   → expectationFailure
                     ("the necrotic wound should have survived cleanup; got "
                      <> show (length ws) <> " wounds")

-- ----------------------------------------------------------------------
-- Blood recovery and revival (#2639)
-- ----------------------------------------------------------------------
--
-- Before this, blood only ever went DOWN in the wound tick: the drain was
-- subtracted and nothing added any back, so a unit that collapsed from
-- blood loss (below 30 % of maximum) could never reach the 50 % the
-- shipped revive gate demands, however completely it was stabilized.
--
-- The pure half of this group drives the REAL 'tickOneUnit'; the engine
-- half drives the SHIPPED @scripts/unit_resource_tick.lua@ @checkRevive@
-- against a real unit manager, so the 30 %→50 % band is crossed and then
-- acted on by production code rather than by a restatement of it.

-- | The 70 kg fixture's derived maximum blood volume, 5.25 L.
maxBlood ∷ Float
maxBlood = 70 * bloodMassRatio

-- | The tick step every recovery example uses.
recoveryDt ∷ Float
recoveryDt = 10

-- | Litres recovered in ONE 'recoveryDt' tick at full nutrition and
--   constitution 1.0. Restated from the issue's own policy rather than
--   imported, so a silent change to the rate constants fails here.
fullGain ∷ Float
fullGain = maxBlood * 0.70 / (3 * 1440) * recoveryDt

-- | The fixture's stat map with arbitrary extra entries merged in.
statsWith ∷ [(Text, Float)] → HM.HashMap Text Float
statsWith extra =
    HM.union (HM.fromList extra)
             (HM.fromList [("body_mass", 70), ("constitution", 1.0)])

-- | A collapsed patient at @blood@ litres carrying @ws@, with @extra@
--   merged over the base stats. Collapsed rather than standing because
--   recovery must run while unconscious — that is the whole point — and
--   because it keeps 'UnconsciousNow' from re-firing every tick.
patient ∷ Float → [(Text, Float)] → [Wound] → UnitInstance
patient blood extra ws =
    (inst ws) { uiBlood = blood, uiStats = statsWith extra
              , uiPose = "collapsed" }

-- | One tick of the real per-unit wound tick, with no infection
--   catalogue or climate so only the clot/heal/bleed/recovery math runs.
step ∷ Float → UnitInstance → UnitInstance
step dt i = let (i', _, _) = tickOneUnit 100 def dt emptyInfectionManager
                                 Nothing (Random.mkStdGen 1) i False
            in i'

-- | One tick, keeping the verdict as a comparable label.
stepOutcome ∷ Float → UnitInstance → (UnitInstance, Text)
stepOutcome dt i =
    let (i', o, _) = tickOneUnit 100 def dt emptyInfectionManager
                         Nothing (Random.mkStdGen 1) i False
    in (i', label o)
  where
    label NoChange            = "none"
    label (UnconsciousNow p)  = "collapsed:" <> p
    label (DiedNow p cause)   = "died:" <> p <> ":" <> cause

-- | @n@ ticks of 'recoveryDt' seconds each.
steps ∷ Int → UnitInstance → UnitInstance
steps n i = L.foldl' (\acc _ → step recoveryDt acc) i [1 .. n]

-- | Blood gained by one 'recoveryDt' tick of a WOUND-FREE unit with the
--   given extra stats — the isolated recovery rate, with no drain, no
--   clot and no heal in the way.
gainWith ∷ [(Text, Float)] → Float
gainWith extra =
    let before = patient 1.0 extra []
    in uiBlood (step recoveryDt before) - uiBlood before

-- | A fully clotted (stabilized) wound: present, treated, and bleeding
--   at exactly zero because @1 − woundClot@ is zero.
stabilized ∷ Wound
stabilized = (mkWound "slash" 0.5 0.0 0.0 True) { woundClot = 1.0 }

nearly ∷ Float → Float → Expectation
nearly actual expected
    | abs (actual - expected) < 1e-5 = pure ()
    | otherwise = expectationFailure $
        "expected " <> show expected <> ", got " <> show actual

bloodRecoverySpec ∷ Spec
bloodRecoverySpec = describe "Combat.Wounds blood recovery and revival" $ do

  describe "the recovery rate" $ do

    it "a wound-free unit with no live nutrition pools recovers at the \
       \full base rate" $
        -- Absent pools are UNGATED (wildlife, and acolytes before their
        -- first resource tick): the factor is 1.0, not "starving".
        gainWith [] `nearly` fullGain

    it "recovery continues with a stabilized wound still PRESENT" $ do
        -- The wound has not healed out — what matters is that its bleed
        -- rate is zero, not that it is gone.
        let before = patient 1.0 [] [stabilized]
            after  = step recoveryDt before
        uiWounds after `shouldSatisfy` (not ∘ null)
        (uiBlood after - uiBlood before) `nearly` fullGain

    it "recovery continues after the final wound is removed" $ do
        -- The wound-free early return is a SEPARATE code path; a unit
        -- whose last wound heals away must keep recovering across it.
        let healedOut = patient 1.0 [] []
        uiWounds healedOut `shouldBe` []
        (uiBlood (step recoveryDt healedOut) - uiBlood healedOut)
            `nearly` fullGain

    it "the gain is clamped to the derived maximum volume" $ do
        -- A step far larger than the remaining headroom lands exactly on
        -- the maximum rather than overshooting it.
        let before = patient (maxBlood - 0.01) [] []
        uiBlood (step 100000 before) `nearly` maxBlood

    it "a unit already ABOVE its derived maximum is not lowered" $ do
        -- The clamp bounds recovery from above only. Fixtures elsewhere
        -- in this suite seed uiBlood = 100 against a 5.25 L maximum, and
        -- a tick that "clamped" them down would silently break every one.
        let before = patient 100 [] []
        uiBlood (step recoveryDt before) `shouldBe` 100

    it "a unit with no body_mass stat never recovers" $ do
        -- Units whose YAML declares no body block are spawn-seeded at
        -- 0 L precisely so they cannot bleed; the tick's maximum-volume
        -- formula defaults body mass to 70 kg, so without an explicit
        -- guard a tiller or a deer would regenerate toward 5.25 L.
        let bodiless = (inst []) { uiBlood = 0, uiPose = "collapsed"
                                 , uiStats = HM.fromList [("constitution", 1.0)] }
        uiBlood (step recoveryDt bodiless) `shouldBe` 0
        uiBlood (steps 500 bodiless) `shouldBe` 0

  describe "nutrition and constitution scaling" $ do

    it "calories at zero block recovery entirely" $
        gainWith [("calories", 0), ("max_calories", 2000)] `shouldBe` 0

    it "a positive calorie fraction scales the rate linearly" $
        gainWith [("calories", 1000), ("max_calories", 2000)]
            `nearly` (0.5 * fullGain)

    it "a present calorie pool with no usable maximum blocks recovery" $
        -- Present but unevaluable is NOT the same as absent.
        gainWith [("calories", 1500), ("max_calories", 0)] `shouldBe` 0

    it "hydration just below 25 % blocks recovery" $
        gainWith [("hydration", 24), ("max_hydration", 100)] `shouldBe` 0

    it "hydration at exactly 25 % permits recovery, scaled to 0.25" $
        -- The survival-alert boundary itself is on the PERMITTED side.
        gainWith [("hydration", 25), ("max_hydration", 100)]
            `nearly` (0.25 * fullGain)

    it "calorie and hydration scaling are independent, and the LOWER \
       \factor governs" $ do
        -- Full hydration, half calories → calories governs …
        gainWith [ ("calories", 1000), ("max_calories", 2000)
                 , ("hydration", 100), ("max_hydration", 100) ]
            `nearly` (0.5 * fullGain)
        -- … full calories, 40 % hydration → hydration governs.
        gainWith [ ("calories", 2000), ("max_calories", 2000)
                 , ("hydration", 40), ("max_hydration", 100) ]
            `nearly` (0.4 * fullGain)

    it "a missing constitution stat defaults to 1.0" $ do
        let noCon = (inst []) { uiBlood = 1.0, uiPose = "collapsed"
                              , uiStats = HM.fromList [("body_mass", 70)] }
        (uiBlood (step recoveryDt noCon) - uiBlood noCon) `nearly` fullGain

    it "distinct constitutions produce the specified distinct rates" $ do
        -- 1 + 0.25 × (con − 1), clamped to [0.75, 1.5].
        gainWith [("constitution", 2.0)] `nearly` (1.25 * fullGain)
        gainWith [("constitution", 0.5)] `nearly` (0.875 * fullGain)
        -- Both ends of the clamp, from values that would otherwise
        -- overshoot it in either direction.
        gainWith [("constitution", 5.0)] `nearly` (1.5 * fullGain)
        gainWith [("constitution", 0.0)] `nearly` (0.75 * fullGain)

  describe "eligibility: any bleeding at all blocks recovery" $ do

    it "positive EXTERNAL bleeding blocks recovery" $ do
        let w      = (mkWound "slash" 0.5 0.0 0.0 True) { woundBandage = 1.0 }
            before = patient 3.0 [] [w]
            after  = step recoveryDt before
        -- Strictly down, and down by the full gross drain the post-tick
        -- wound predicts — no recovery netted off it.
        uiBlood after `shouldSatisfy` (< uiBlood before)
        (uiBlood before - uiBlood after)
            `nearly` (bleedRateFor def after * recoveryDt)

    it "positive INTERNAL bleeding blocks recovery even though nothing \
       \bleeds externally" $ do
        -- The discriminating case: `internal` contributes nothing to the
        -- external rate, so an eligibility test written against external
        -- bleeding alone would wrongly top this unit up.
        let w      = (mkWound "internal" 0.5 0.0 0.0 True) { woundBandage = 1.0 }
            before = patient 3.0 [] [w]
            after  = step recoveryDt before
        externalBleedRateFor def after `shouldBe` 0
        bleedRateFor def after `shouldSatisfy` (> 0)
        uiBlood after `shouldSatisfy` (< uiBlood before)

    it "a fracture's internal seep blocks recovery too" $ do
        let w      = (mkWound "fracture" 0.8 0.0 0.0 True) { woundBandage = 1.0 }
            before = patient 3.0 [] [w]
            after  = step recoveryDt before
        uiBlood after `shouldSatisfy` (< uiBlood before)

  describe "gross blood-loss accounting is independent of recovery" $ do

    it "a truly exsanguinating tick dies at zero blood and is not rescued" $ do
        let w        = (mkWound "arterial" 1.0 0.0 0.0 True) { woundBandage = 1.0 }
            before   = patient 0.2 [] [w]
            (after, verdict) = stepOutcome recoveryDt before
        verdict `shouldBe` "died:l_thigh:exsanguination"
        uiBlood after `shouldBe` 0
        uiTrailState after `shouldBe` Nothing

    it "a dead unit is returned completely unchanged" $ do
        let corpse = (patient 1.0 [] []) { uiPose = "dead" }
        uiBlood (step recoveryDt corpse) `shouldBe` 1.0
        uiBlood (steps 500 corpse) `shouldBe` 1.0

    it "the external trail accumulates the GROSS volume actually lost" $ do
        let w        = (mkWound "slash" 0.5 0.0 0.0 True) { woundBandage = 1.0 }
            before   = patient 3.0 [] [w]
            after    = step recoveryDt before
            lost     = uiBlood before - uiBlood after
        lost `shouldSatisfy` (> 0)
        case uiTrailState after of
            Nothing → expectationFailure
                "an externally bleeding unit should have a trail state"
            Just ts → tsPendingVolume ts `nearly` lost

  describe "the 30 %-to-50 % band" $ do

    it "a unit bled below 30 % climbs back past 50 % once stabilized" $ do
        -- Phase 1: bleed it down until the tick itself calls the collapse.
        let bleeding = (mkWound "slash" 0.6 0.0 0.0 True) { woundBandage = 1.0 }
            wounded  = ((inst [bleeding]) { uiBlood = maxBlood * 0.35
                                          , uiStats = statsWith [] })
            bleedDown i n
                | n ≤ (0 ∷ Int) = (i, "none")
                | otherwise = case stepOutcome 1 i of
                    (i', "none") → bleedDown i' (n - 1)
                    r            → r
            (collapsed, verdict) = bleedDown wounded 60
        verdict `shouldBe` "collapsed:l_thigh"
        uiBlood collapsed `shouldSatisfy` (< maxBlood * 0.30)
        -- Phase 2: first aid lands — the wound clots shut and stays put.
        let stable = collapsed
                { uiPose = "collapsed"
                , uiWounds = map (\w → w { woundClot = 1.0 }) (uiWounds collapsed) }
        bleedRateFor def stable `shouldBe` 0
        -- Phase 3: recovery alone carries it across the revive threshold,
        -- monotonically, and through the moment the wound heals away.
        let recovering = scanSteps 200 stable
            volumes    = map uiBlood recovering
        and (zipWith (≤) volumes (drop 1 volumes)) `shouldBe` True
        uiWounds (last recovering) `shouldBe` []
        uiBlood (last recovering) `shouldSatisfy` (≥ maxBlood * 0.50)

  where
    -- Every intermediate state of @n@ ticks, so monotonicity can be
    -- asserted over the whole run rather than at its endpoints.
    scanSteps ∷ Int → UnitInstance → [UnitInstance]
    scanSteps n i = L.scanl' (\acc _ → step recoveryDt acc) i [1 .. n]

-- | The SHIPPED cross-resource revive gate, driven against a real unit
--   manager. Requirement 4 is a claim about production Lua, so it is
--   asserted through production Lua: @checkRevive@ is called with an
--   EMPTY resource config, which leaves the blood gate as the only
--   threshold in play, and @unit.revive@ is counted rather than stubbed.
reviveGateSpec ∷ Spec
reviveGateSpec = aroundAll withHeadlessEngineNoWorld $
  describe "Combat.Wounds blood recovery and revival — the shipped \
           \revive gate" $ do

    it "revives a stabilized unit at EXACTLY 50 % of maximum blood" $ \env → do
        ls ← reviveScene env (maxBlood * 0.5)
        checkReviveNow ls `shouldReturn` "true"
        evalDebug ls "return _G.__revived" `shouldReturn` "1"

    it "does NOT revive just below 50 %" $ \env → do
        ls ← reviveScene env (maxBlood * 0.5 - 0.01)
        checkReviveNow ls `shouldReturn` "true"
        evalDebug ls "return _G.__revived" `shouldReturn` "0"

    it "revives a unit the wound tick itself carried across the \
       \threshold" $ \env → do
        -- End to end: a collapsed, stabilized unit below 30 % is ticked
        -- by the REAL wound tick until recovery lifts it over 50 %, and
        -- the SHIPPED gate is what stands it up. Neither half restates
        -- the other's threshold.
        let start = (inst [stabilized]) { uiBlood = maxBlood * 0.29
                                        , uiStats = statsWith []
                                        , uiPose = "collapsed" }
            ended = steps 250 start
        uiBlood start `shouldSatisfy` (< maxBlood * 0.30)
        uiBlood ended `shouldSatisfy` (≥ maxBlood * 0.50)
        ls ← reviveScene env (uiBlood ended)
        checkReviveNow ls `shouldReturn` "true"
        evalDebug ls "return _G.__revived" `shouldReturn` "1"

-- | A one-unit collapsed scene at the given blood volume, with
--   @unit.revive@ counted into @_G.__revived@.
reviveScene ∷ EngineEnv → Float → IO LuaBackendState
reviveScene env blood = do
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.singleton (UnitId 1)
            ((inst []) { uiDefName = "acolyte", uiPose = "collapsed"
                       , uiBlood = blood
                       , uiStats = statsWith [] }) }
    ls ← newBareLuaBackend env
    r ← evalDebug ls
          "_G.__revived = 0; local v = unit.revive; \
          \unit.revive = function(...) \
          \  _G.__revived = _G.__revived + 1; return v(...) end; return true"
    r `shouldBe` "true"
    pure ls

-- | Drive the shipped gate with an EMPTY resource config, so blood is
--   the only threshold that can refuse.
checkReviveNow ∷ LuaBackendState → IO Text
checkReviveNow ls = evalDebug ls
    "require('scripts.unit_resource_tick').checkRevive(1, {}); return true"
