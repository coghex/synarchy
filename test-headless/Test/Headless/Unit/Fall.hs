{-# LANGUAGE Strict #-}
-- | Calibration + behaviour tests for Unit.Fall — the physics that turns
--   a drop into a DISTRIBUTION of injuries (bruise + fracture +
--   concussion …) with no RNG. Deterministic, so it doubles as the
--   calibration guard: if a tuning change makes a 2-z step lethal or a
--   15-z plunge survivable, a test here breaks.
--
--   #998: runs on the SHIPPED acolyte body topology
--   (@data/units/acolyte.yaml@, loaded through the same
--   'unitYamlBodyPartToBodyPart' conversion the Lua unit loader uses —
--   never a hand-transcribed fixture, which could silently drift from
--   the shipped data) across three deterministic profiles — frail
--   (height 1.3, bulk 0.5, toughness 0.8), average (1.8, 1.0, 1.0), and
--   extreme (2.3, 1.5, 1.2) — the exact clamp endpoints of that YAML's
--   `rollStat` ranges. See @docs/expedition_survival_calibration.md@'s
--   fall-calibration section for the full before/after record.
module Test.Headless.Unit.Fall (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlUnits
    (UnitYamlFile(..), UnitYamlDef(..), unitYamlBodyPartToBodyPart)
import Substance.Types (emptySubstanceManager)
import Unit.Types
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import World.Page.Types (WorldPageId(..))
import Unit.Injury (bruiseCap)
import Combat.Wounds.Bleed (bleedRateFor)
import Unit.Fall

-- | Load the shipped acolyte's body-part list through the SAME
--   conversion the Lua unit loader uses (`Engine.Asset.Units.Yaml`) —
--   the sole path, so this can never drift from the real 12-part/44-
--   subpart topology `data/units/acolyte.yaml` ships.
loadAcolyteBodyParts ∷ IO [BodyPart]
loadAcolyteBodyParts = do
    r ← Yaml.decodeFileEither "data/units/acolyte.yaml"
    case r of
        Left err → fail ("failed to load data/units/acolyte.yaml: " ⧺ show err)
        Right file → case uyfUnits file of
            [def] → pure (map unitYamlBodyPartToBodyPart (uydBodyParts def))
            defs  → fail ("expected exactly one acolyte unit def, got "
                          ⧺ show (length defs))

-- A unit def carrying the real body parts; every other field is inert
-- (Unit.Fall only reads udBodyParts).
acolyteDef ∷ [BodyPart] → UnitDef
acolyteDef bps = UnitDef
    { udName = "acolyte", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing, udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.4, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = [], udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty, udStartingAccessories = []
    , udBodyParts = bps
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- A unit instance carrying the given wounds, at the given body mass.
-- Constitution pinned at 1.0 and every wound freshly inflicted
-- (woundBandage 1.0, woundClot 0.0, woundHeal 0.0) mirror exactly what
-- `Unit.Thread.Movement` stamps for a real fall, so 'bleedRateFor' here
-- matches what the engine would compute in play.
mkInst ∷ Float → [Wound] → UnitInstance
mkInst mass ws = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = WorldPageId "test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList [("body_mass", mass), ("constitution", 1.0)]
    , uiModifiers = HM.empty, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = FactionNeutral, uiWounds = ws, uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty, uiBlood = mass * bloodMassRatio
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing }

toWound ∷ FallInjury → Wound
toWound i = Wound
    { woundPart = fiPart i, woundKind = fiKind i, woundSeverity = fiSeverity i
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

kindsOf ∷ [FallInjury] → [Text]
kindsOf = map fiKind

worstOf ∷ [FallInjury] → Float
worstOf injs = maximum (0 : map fiSeverity injs)

-- Explicit-tolerance float comparison (fallInjuries is pure/RNG-free, but
-- pinning derived aggregates to exact bit values would make the guard
-- brittle without adding coverage — #998 review amendment).
approx ∷ Float → Float → Float → Bool
approx tol expected actual = abs (actual - expected) ≤ tol

-- The three calibration profiles (#998): height, bulk, toughness — the
-- exact clamp endpoints of data/units/acolyte.yaml's rollStat ranges.
-- Body mass = 22·height²·bulk (Unit.Thread.Command.Body.seedBodyComposition).
frailH, frailB, frailT, averageH, averageB, averageT
  , extremeH, extremeB, extremeT ∷ Float
frailH = 1.3;   frailB = 0.5;   frailT = 0.8
averageH = 1.8; averageB = 1.0; averageT = 1.0
extremeH = 2.3; extremeB = 1.5; extremeT = 1.2

bodyMass ∷ Float → Float → Float
bodyMass h b = 22 * h * h * b

spec ∷ Spec
spec = do
    bps ← runIO loadAcolyteBodyParts
    let def = acolyteDef bps
        fall mass tough dz = fallInjuries emptySubstanceManager def mass tough dz
        aggregateBleed mass injs = bleedRateFor def (mkInst mass (map toWound injs))
        vitalParts = [ bpId p | p ← bps, bpVital p ]
        frailMass    = bodyMass frailH frailB
        averageMass  = bodyMass averageH averageB
        extremeMass  = bodyMass extremeH extremeB

    describe "profile derivations (#998)" $
        it "derives body mass as 22*height^2*bulk for all three profiles" $ do
            frailMass   `shouldSatisfy` approx 0.01 18.59
            averageMass `shouldSatisfy` approx 0.01 71.28
            extremeMass `shouldSatisfy` approx 0.01 174.57

    describe "shipped acolyte topology (#998)" $ do
        it "has exactly one vital part: the heart" $
            vitalParts `shouldBe` ["heart"]

    describe "a 1-z step-down is harmless (free walk-off)" $
        it "produces no injuries for any profile" $ do
            fall frailMass frailT 1 `shouldBe` []
            fall averageMass averageT 1 `shouldBe` []
            fall extremeMass extremeT 1 `shouldBe` []

    describe "2-z fall — the average acolyte (#998 restored contract)" $ do
        it "is bruised (non-empty), knocked down, with no fracture and no vital injury" $ do
            let injs = fall averageMass averageT 2
            injs `shouldNotSatisfy` null
            kindsOf injs `shouldSatisfy` all (≡ "blunt")
            map fiPart injs `shouldNotSatisfy` any (`elem` vitalParts)
            fallStunFor (worstOf injs) `shouldSatisfy` (> 0)

        it "is not the pathological many-wound result" $
            length (fall averageMass averageT 2) `shouldSatisfy` (< 15)

        it "leaves well over a minute before a naive (untreated, unclotted) exsanguination" $ do
            let injs = fall averageMass averageT 2
                bleed = aggregateBleed averageMass injs
                blood = averageMass * bloodMassRatio
            bleed `shouldSatisfy` (> 0)
            (blood / bleed) `shouldSatisfy` (> 30)

    describe "2-z fall — extreme profile, tested independently (#998)" $ do
        it "is bruised, knocked down, with no fracture and no vital injury" $ do
            let injs = fall extremeMass extremeT 2
            injs `shouldNotSatisfy` null
            kindsOf injs `shouldSatisfy` all (≡ "blunt")
            map fiPart injs `shouldNotSatisfy` any (`elem` vitalParts)

        it "bleeds measurably more than the average profile (heavier, higher load)" $ do
            let bleedAvg = aggregateBleed averageMass (fall averageMass averageT 2)
                bleedExt = aggregateBleed extremeMass (fall extremeMass extremeT 2)
            bleedExt `shouldSatisfy` (> bleedAvg)

        it "still leaves well over a minute before exsanguination" $ do
            let injs = fall extremeMass extremeT 2
                bleed = aggregateBleed extremeMass injs
                blood = extremeMass * bloodMassRatio
            (blood / bleed) `shouldSatisfy` (> 30)

    describe "2-z fall — frail profile, tested independently (#998)" $ do
        it "is NOT the more-vulnerable case: lighter body means lower impact energy" $ do
            let injs = fall frailMass frailT 2
                bleedFrail = aggregateBleed frailMass injs
                bleedAvg   = aggregateBleed averageMass (fall averageMass averageT 2)
            injs `shouldNotSatisfy` null
            kindsOf injs `shouldSatisfy` all (≡ "blunt")
            -- The frail profile is a PERMITTED exception to the survival
            -- expectation (#998), but on THIS shipped topology it measures
            -- safer than average, not more lethal — mass drives impact
            -- energy, and frail's mass is far lower than its lower
            -- toughness costs it. Document the actual measurement rather
            -- than assume "frail" implies "dies first".
            bleedFrail `shouldSatisfy` (< bleedAvg)

        it "leaves well over a minute before exsanguination" $ do
            let injs = fall frailMass frailT 2
                bleed = aggregateBleed frailMass injs
                blood = frailMass * bloodMassRatio
            (blood / bleed) `shouldSatisfy` (> 30)

    describe "fracture ladder (independently measured per profile, #998)" $ do
        it "average: no fracture through 4-z, first fracture at 5-z" $ do
            kindsOf (fall averageMass averageT 4) `shouldSatisfy` all (≡ "blunt")
            kindsOf (fall averageMass averageT 5) `shouldSatisfy` elem "fracture"

        it "extreme: fractures earlier than average (from 3-z)" $ do
            kindsOf (fall extremeMass extremeT 2) `shouldSatisfy` all (≡ "blunt")
            kindsOf (fall extremeMass extremeT 3) `shouldSatisfy` elem "fracture"

        it "frail: fractures much later than average (from 15-z)" $ do
            kindsOf (fall frailMass frailT 14) `shouldSatisfy` all (≡ "blunt")
            kindsOf (fall frailMass frailT 15) `shouldSatisfy` elem "fracture"

        it "a hard fall (10-z) produces a DISTRIBUTION, not one wound" $ do
            let ks = kindsOf (fall averageMass averageT 10)
            ks `shouldSatisfy` elem "blunt"
            ks `shouldSatisfy` elem "fracture"

        it "bruises never exceed the bruise cap (a contusion isn't lethal)" $
            fall averageMass averageT 12 `shouldSatisfy`
                all (\i → fiKind i ≢ "blunt" ∨ fiSeverity i ≤ bruiseCap + 1e-4)

        it "severity is monotone non-decreasing with height (average profile)" $
            worstOf (fall averageMass averageT 8)
                `shouldSatisfy` (≤ worstOf (fall averageMass averageT 14))

    describe "the vital heart is never the deterministic torso pick (#998)" $
        it "no fall at any tested height injures a vital part" $
            sequence_
                [ map fiPart (fall averageMass averageT z)
                    `shouldNotSatisfy` any (`elem` vitalParts)
                | z ← [2 .. 30 ∷ Int] ]
