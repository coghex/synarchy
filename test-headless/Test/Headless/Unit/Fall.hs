{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Calibration + behaviour tests for Unit.Fall — the physics that turns
--   a drop into a DISTRIBUTION of injuries (bruise + fracture +
--   concussion …) with no RNG. Deterministic, so it doubles as the
--   calibration guard: if a tuning change makes a 2-z step lethal or a
--   15-z plunge survivable, a test here breaks.
module Test.Headless.Unit.Fall (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Substance.Types (emptySubstanceManager)
import Unit.Types
import Unit.Injury (bruiseCap)
import Unit.Fall

-- A minimal humanoid def: only the fields Unit.Fall reads (udBodyParts)
-- matter; the rest are inert placeholders.
part ∷ Text → Maybe Text → Bool → Float → [(Text, Float)] → BodyPart
part pid parent vital hLow layers = BodyPart
    { bpId = pid, bpName = pid, bpParent = parent, bpVital = vital
    , bpAreaWeight = 0.1, bpTacticalValue = 0.5
    , bpBleedFactor = 1.0, bpHeightLow = hLow, bpHeightHigh = hLow + 0.5
    , bpLayers = [ (m, m, t) | (m, t) ← layers ]   -- name defaults to material
    , bpTargetable = True, bpDepth = 0.0 }

humanoid ∷ UnitDef
humanoid = UnitDef
    { udName = "t", udTexture = TextureHandle 0, udPortrait = Nothing, udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty, udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ part "head"  (Just "neck")  True  1.65
            [("flesh",3),("bone",7),("organ",80)]
        , part "neck"  (Just "torso") True  1.55 [("flesh",40),("organ",10)]
        , part "torso" Nothing        True  1.0
            [("flesh",25),("bone",12),("organ",100)]
        , part "l_leg" (Just "torso") False 0.0  [("flesh",40),("bone",35)]
        , part "r_leg" (Just "torso") False 0.0  [("flesh",40),("bone",35)]
        , part "l_foot" (Just "l_leg") False 0.0 []
        , part "r_foot" (Just "r_leg") False 0.0 []
        ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- Baseline acolyte: 70 kg, toughness 1.0.
fall ∷ Int → [FallInjury]
fall dz = fallInjuries emptySubstanceManager humanoid 70.0 1.0 dz

kinds ∷ [FallInjury] → [Text]
kinds = map fiKind

-- Worst SINGLE injury severity on a vital part (the death driver — a
-- catastrophic single wound kills, not an accumulation of moderate ones).
worstVital ∷ [FallInjury] → Float
worstVital injs = maximum
    (0 : [ fiSeverity i | i ← injs, fiPart i `elem` ["head","neck","torso"] ])

spec ∷ Spec
spec = do
    describe "fall injury distribution" $ do
        it "a 1-z step-down is harmless (free walk-off)" $
            fall 1 `shouldBe` []

        it "a short fall (2-z) hurts but is survivable + bruises" $ do
            let injs = fall 2
            injs `shouldNotBe` []
            kinds injs `shouldSatisfy` elem "blunt"
            worstVital injs `shouldSatisfy` (< 1.0)

        it "a moderate fall (6-z) breaks something (leg/ankle fracture)" $
            kinds (fall 6) `shouldSatisfy` elem "fracture"

        it "a hard fall produces a DISTRIBUTION, not one wound" $ do
            -- bruise + fracture both present on the same fall
            let ks = kinds (fall 8)
            ks `shouldSatisfy` elem "blunt"
            ks `shouldSatisfy` elem "fracture"

        it "bruises never exceed the bruise cap (a contusion isn't lethal)" $
            fall 12 `shouldSatisfy`
                all (\i → fiKind i ≢ "blunt" ∨ fiSeverity i ≤ bruiseCap + 1e-4)

    describe "fall lethality ladder (death is emergent from injuries)" $ do
        it "survivable falls leave every vital part below the lethal sum" $
            worstVital (fall 6) `shouldSatisfy` (< 1.0)

        it "a tall plunge inflicts a lethal (≥1) vital injury" $
            worstVital (fall 16) `shouldSatisfy` (≥ 1.0)

        it "lethality is monotone in height" $
            worstVital (fall 8) `shouldSatisfy` (≤ worstVital (fall 14))
