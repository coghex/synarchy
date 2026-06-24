{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
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
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Types
import Unit.Direction (Direction(..))
import Combat.Wounds (tickOneUnit)
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
    { udName = "t", udTexture = TextureHandle 0, udDirSprites = Map.empty
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
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0 } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- A unit carrying exactly the given wounds, with plenty of blood so a
-- bleed can't kill it mid-test (we're testing infection, not death).
inst ∷ [Wound] → UnitInstance
inst ws = UnitInstance
    { uiDefName = "t", uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList [("body_mass", 70), ("constitution", 1.0)]
    , uiModifiers = HM.empty, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = "t", uiWounds = ws, uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty, uiBlood = 100
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing }

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
                            (Random.mkStdGen 1) (inst [w])
    in head (uiWounds inst')

spec ∷ Spec
spec = describe "Combat.Wounds infection" $ do

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
                            (Random.mkStdGen 1) (inst [w])
        woundNecrosis (head (uiWounds i')) `shouldSatisfy` (> 0.01)

    it "a NON-necrotic infection does NOT rot tissue" $ do
        let w  = (mkWound "slash" 0.5 0.0 0.8 False) { woundInfectionType = "bug" }
            (i', _, _) = tickOneUnit 100 def 10 (mgrWith []) Nothing
                            (Random.mkStdGen 1) (inst [w])
        woundNecrosis (head (uiWounds i')) `shouldBe` 0.0

    it "necrosis needs an ESTABLISHED infection (low infection doesn't rot)" $ do
        -- infection 0.1 < necrosisInfThreshold → no rot even for a necrotic bug.
        let w  = (mkWound "slash" 0.5 0.0 0.1 False) { woundInfectionType = "bug" }
            (i', _, _) = tickOneUnit 100 def 10 (mgrWith ["necrosis"]) Nothing
                            (Random.mkStdGen 1) (inst [w])
        woundNecrosis (head (uiWounds i')) `shouldBe` 0.0

    it "fever (high core temp) slows infection growth" $ do
        -- A dirty, already-infected wound; compare growth at a febrile core
        -- (41°C) vs normal (37°C). The fever should suppress the growth.
        let w = (mkWound "slash" 0.5 0.0 0.3 False)
            withCore c = (inst [w])
                { uiStats = HM.fromList
                    [("body_mass", 70), ("constitution", 1.0), ("core_temp", c)] }
            grow c = let (i', _, _) = tickOneUnit 100 def 10 emptyInfectionManager
                                        Nothing (Random.mkStdGen 1) (withCore c)
                     in woundInfection (head (uiWounds i'))
        grow 41.0 `shouldSatisfy` (< grow 37.0)
