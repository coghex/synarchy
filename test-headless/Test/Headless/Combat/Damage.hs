{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for the combat damage model (Combat.Resolution):
--   the Tier 3 swing kinematics, weapon penetrating power, the tissue
--   woundFactor table, and the layered-penetration target model.
--
--   These assert physical RELATIONSHIPS and orderings (sharper cuts
--   more, a skull turns a blade but not a blow, a strong thrust punches
--   through, necks are deadly) rather than balance numbers. Substance
--   values mirror data/substances/{metals,biological}.yaml.
module Test.Headless.Combat.Damage (spec) where

import UPrelude
import Test.Hspec
import Substance.Types (SubstanceDef(..))
import Combat.Resolution
    ( ResolvedStrike(..), swingKinematics, weaponPenetration, penetrate
    , woundFactor, weaponWear )
import Combat.Types (AttackMode(..))

-- Substances (matching the yaml: name density tensile yield shear
-- fracture hardness stabR slashR bluntR).
steel, keratin, enamel, bone, flesh, organ, wool ∷ SubstanceDef
steel   = SubstanceDef "steel"   7.87 1000 500 800 100 600 0.95 0.95 0.60
keratin = SubstanceDef "keratin" 1.30  180 120  90   8  25 0.20 0.35 0.30
enamel  = SubstanceDef "enamel"  2.97   35 330  90   1 350 0.50 0.45 0.10
bone    = SubstanceDef "bone"    1.90  130 115  65   4  45 0.25 0.40 0.30
flesh   = SubstanceDef "flesh"   1.05    2   1   1   2   1 0.05 0.05 0.15
organ   = SubstanceDef "organ"   1.05    1   1   1   1   1 0.02 0.02 0.05
wool    = SubstanceDef "pleated_wool" 0.20 5 2 3 2 5 0.20 0.60 0.25

-- A manufactured-blade strike; tweak per test.
strike ∷ SubstanceDef → Float → Float → Float → ResolvedStrike
strike sub sharp blade eff = ResolvedStrike
    { rsSub = Just sub, rsBladeCm = blade, rsSharpness = sharp
    , rsImpactArea = 50, rsMass = 1.0, rsLength = blade, rsCoM = 0.5
    , rsEff = eff, rsQuality = 1.0, rsCondition = 1.0 }

-- Body part tissue stacks (outer→inner).
skull, neckSoft, torso ∷ [(Maybe SubstanceDef, Float)]
skull    = [(Just flesh, 3), (Just bone, 7), (Just organ, 80)]
neckSoft = [(Just flesh, 40), (Just organ, 10)]
torso    = [(Just flesh, 25), (Just bone, 12), (Just organ, 100)]

wpDagger ∷ Float
wpDagger = weaponPenetration (strike steel 200 22 0.5) "stab"

spec ∷ Spec
spec = do
    describe "weaponPenetration (edge quality)" $ do
        it "a sharper point penetrates more (stab)" $
            weaponPenetration (strike steel 80 22 0.5) "stab"
              `shouldSatisfy` (> weaponPenetration (strike steel 300 22 0.5) "stab")
        it "a harder point penetrates more than a soft one (stab)" $
            weaponPenetration (strike steel 150 18 0.5) "stab"
              `shouldSatisfy` (> weaponPenetration (strike keratin 150 18 0.5) "stab")
        it "enamel fangs out-penetrate keratin claws on a stab" $
            weaponPenetration (strike enamel 90 4 0.5) "stab"
              `shouldSatisfy` (> weaponPenetration (strike keratin 120 9 0.5) "stab")
        it "a duller effective edge (higher sharpness value) penetrates less" $
            -- iiSharpness wear is folded into rsSharpness upstream; here a
            -- higher value = a duller edge = less penetration.
            weaponPenetration (strike steel 200 22 0.5) "stab"
              `shouldSatisfy`
                (> weaponPenetration (strike steel 600 22 0.5) "stab")
        it "a better-made weapon penetrates more" $
            weaponPenetration ((strike steel 200 22 0.5) { rsQuality = 1.0 }) "stab"
              `shouldSatisfy`
                (> weaponPenetration ((strike steel 200 22 0.5) { rsQuality = 0.0 }) "stab")

    describe "woundFactor (tissue lethality)" $ do
        it "organs are deadlier to wound than flesh, flesh than bone (cut)" $ do
            woundFactor "organ" "stab" `shouldSatisfy` (> woundFactor "flesh" "stab")
            woundFactor "flesh" "stab" `shouldSatisfy` (> woundFactor "bone" "stab")
        it "bone matters more under a blunt blow than a cut" $
            woundFactor "bone" "blunt" `shouldSatisfy` (> woundFactor "bone" "stab")
        it "flesh cuts worse than it bruises" $
            woundFactor "flesh" "stab" `shouldSatisfy` (> woundFactor "flesh" "blunt")

    describe "layered penetration (target materials)" $ do
        it "a skull turns a hand-knife stab the soft neck can't resist" $
            -- same dagger energy + penetration; the skull protects.
            penetrate skull 14 wpDagger "stab"
              `shouldSatisfy` (< penetrate neckSoft 14 wpDagger "stab")
        it "a blunt blow defeats the skull a blade is turned by" $
            -- equal budget number: the blade is stopped at the bone, the
            -- impact transmits through it to the brain.
            penetrate skull 14 1.0 "blunt"
              `shouldSatisfy` (> penetrate skull 14 wpDagger "stab")
        it "a strong enough thrust punches through the skull" $
            penetrate skull 40 wpDagger "stab"
              `shouldSatisfy` (> penetrate skull 14 wpDagger "stab")
        it "a sharper/harder point gets deeper into the skull" $
            penetrate skull 14 (4 * wpDagger) "stab"
              `shouldSatisfy` (> penetrate skull 14 wpDagger "stab")
        it "ribs blunt a slash before it reaches the organs" $
            -- torso (ribs) protects the core vs the open neck.
            penetrate torso 14 wpDagger "slash"
              `shouldSatisfy` (< penetrate neckSoft 14 wpDagger "slash")
        it "an unspecified-material layer behaves like flesh (non-zero)" $
            penetrate [(Nothing, 40)] 14 wpDagger "stab" `shouldSatisfy` (> 0)
        it "worn armour (a steel plate) reduces what reaches the tissue" $
            -- a plate prepended to the torso stack turns a stab the bare
            -- torso wouldn't.
            penetrate ((Just steel, 2) : torso) 14 wpDagger "stab"
              `shouldSatisfy`
                (< penetrate torso 14 wpDagger "stab")

    describe "weaponWear (degradation per hit)" $ do
        -- weaponWear material load sharpness condition → (sharp', cond', broke)
        let sharpOf (s,_,_) = s; condOf (_,c,_) = c; brokeOf (_,_,b) = b
        it "a heavier load dulls the edge more" $
            sharpOf (weaponWear steel 10 100 100)
              `shouldSatisfy` (< sharpOf (weaponWear steel 1 100 100))
        it "a near-zero load (cutting flesh) barely wears the edge" $
            sharpOf (weaponWear steel 0.02 100 100) `shouldSatisfy` (> 99.9)
        it "a brittle material fractures faster than a tough one" $
            -- enamel (fracture toughness 1) vs steel (100), same load.
            condOf (weaponWear enamel 10 100 100)
              `shouldSatisfy` (< condOf (weaponWear steel 10 100 100))
        it "a sound blade survives a load that snaps a fractured one" $ do
            brokeOf (weaponWear steel 20 100 20)  `shouldBe` True   -- worn
            brokeOf (weaponWear steel 20 100 100) `shouldBe` False  -- sound
        it "breaking drives condition to zero" $
            condOf (weaponWear steel 100 100 100) `shouldBe` 0.0
        it "soft gear wears instead of catastrophically snapping" $
            -- a wool gambeson (yield 2) under a load that would overload-
            -- snap a rigid material survives (only its condition drops) —
            -- the rigidity gate. A worn-down gambeson still ends broken
            -- via condition reaching 0, just not in one hit.
            brokeOf (weaponWear wool 3 100 100) `shouldBe` False

    describe "swingKinematics (Tier 3 lever: energy + momentum)" $ do
        -- args: work wMass wLen wCoM armLen armMass dexterity mode
        let energy = fst; momentum = snd
            dagger = swingKinematics 120 0.4 30 0.4  72 3.5 1.0 Heavy
            maul   = swingKinematics 120 5.0 70 0.8  72 3.5 1.0 Heavy
            hammer = swingKinematics 120 3.0 60 0.85 72 3.5 1.0 Heavy
            pipe   = swingKinematics 120 3.0 60 0.5  72 3.5 1.0 Heavy
        it "a heavier weapon carries more momentum (crush driver)" $
            momentum maul `shouldSatisfy` (> momentum dagger)
        it "a head-heavy weapon out-crushes an equal-mass balanced one" $
            momentum hammer `shouldSatisfy` (> momentum pipe)
        it "the speed cap bites for a light weapon + huge work" $
            energy (swingKinematics 2000 0.1 20 0.5 72 3.5 3.0 Quick)
              `shouldSatisfy` (< 2000)
        it "more dexterity raises the speed cap (more energy when capped)" $
            energy (swingKinematics 2000 0.1 20 0.5 72 3.5 3.0 Quick)
              `shouldSatisfy`
                (< energy (swingKinematics 2000 0.1 20 0.5 72 3.5 6.0 Quick))
