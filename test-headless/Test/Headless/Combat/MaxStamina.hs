{-# LANGUAGE Strict #-}
-- | #1735: combat's @max_stamina@ mirror resolves through EFFECTIVE
--   stats, so an equipped accessory's @buffs:@ or a unit def's innate
--   @modifiers:@ move the combat stamina pool by exactly as much as
--   they move the pool every Lua consumer reads.
--
--   Lua owns the canonical derivation (@scripts/unit_stats.lua@'s
--   @stats.get@: an explicit per-unit @max_stamina@ attribute wins,
--   else @endurance × 10@) and reads each input through
--   @unit.getStat@, which is base plus the unit's active
--   'uiModifiers' at the current game time. The combat thread can't
--   call into Lua, so 'Combat.Resolution.Common.maxStaminaFor' mirrors
--   the whole thing — dispatch AND effective resolution — and these
--   examples pin both halves plus the two consumers that read it:
--   'Combat.Resolution.Wear.staminaDrainStats' (how much a swing
--   costs) and 'Combat.Resolution.Damage.computeSeverity' (the winded
--   attacker's stamina fraction).
--
--   Expected pool sizes are written as literal numbers rather than
--   recomputed from 'Unit.Stats.effectiveStat', so an edit to the
--   composition formula fails here instead of moving both sides at
--   once. Every value below is exact in binary floating point.
module Test.Headless.Combat.MaxStamina (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Types
import World.Page.Types (WorldPageId(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Combat.Types (AttackMode(..))
import Combat.Resolution.Common (maxStaminaFor)
import Combat.Resolution.Wear (staminaDrainStats)
import Combat.Resolution.Damage (computeSeverity)
import Substance.Types (emptySubstanceManager)
import Item.Types (emptyItemManager)

-- A minimal unit carrying exactly the given stats and stat modifiers;
-- every other field is inert boilerplate, mirroring
-- Test.Headless.Combat.MentalEffectiveness's fixture.
mkInst ∷ HM.HashMap Text Float → HM.HashMap Text [StatModifier]
       → UnitInstance
mkInst stats mods = UnitInstance
    { uiDefName = "t", uiName = "", uiPage = WorldPageId "test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = mods, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = FactionNeutral, uiWounds = [], uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty, uiBlood = 100
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing }

-- A single-part target body for computeSeverity — the same fixture
-- shape Test.Headless.Combat.MentalEffectiveness uses (bpLayers = []
-- takes Combat.Resolution.Damage's synthetic-flesh fallback, so no
-- substance catalog is needed).
targetDef ∷ UnitDef
targetDef = UnitDef
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
            , bpVital = False, bpAreaWeight = 0.3, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 2
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | The game-time instant every example resolves against — the stand-in
--   for the single sample Combat.Resolution.resolveAttack captures and
--   threads through the whole resolution.
now ∷ Double
now = 100.0

-- | One modifier, in the shape both shipped writers produce: an item
--   YAML @buffs:@ entry through 'Unit.Stats.applyItemBuffs', and a unit
--   def's @modifiers:@ through Unit.Thread.Command.Spawn.
modifier ∷ Float → Float → Maybe Double → StatModifier
modifier delta percent expiry = StatModifier
    { smDelta = delta, smSource = "test", smExpiry = expiry
    , smPercent = percent }

-- Attacker stats that pin every OTHER computeSeverity input, so the
-- only thing that can move the result is the stamina fraction.
atkBase ∷ HM.HashMap Text Float
atkBase = HM.fromList
    [ ("strength", 1.0), ("dexterity", 1.0)
    , ("body_mass", 70.0), ("height", 1.8) ]

-- | Within half a thousandth. The pool arithmetic above is exact in
--   binary, but a drain multiplies by 'staminaCostFraction' Quick =
--   0.05, which is not — so the drain examples state the intended
--   value with a tolerance rather than asserting a rounding.
nearStamina ∷ Float → Maybe Float → Bool
nearStamina expect = maybe False (\x → abs (x - expect) < 0.0005)

severityOf ∷ UnitInstance → ( Float, Float, Float, Float, Float
                            , [(Text, Text, Float)]
                            , [(Text, Text, Text, Float)] )
severityOf atk = computeSeverity emptySubstanceManager emptyItemManager
    now atk targetDef Nothing Nothing
    (mkInst (HM.fromList [("body_mass", 70.0)]) HM.empty)
    "torso" "blunt" Quick 0.5 1.0 0.0

spec ∷ Spec
spec = do
    -- ---- The pool itself ----
    describe "maxStaminaFor resolves max_stamina through effective stats" $ do
        it "no modifiers: the derived endurance x 10 pool is unchanged" $
            maxStaminaFor now (mkInst (HM.fromList [("endurance", 2.0)])
                                      HM.empty)
                `shouldBe` 20.0

        it "no modifiers: an explicit max_stamina attribute is unchanged" $
            maxStaminaFor now
                (mkInst (HM.fromList [("max_stamina", 33.0), ("endurance", 2.0)])
                        HM.empty)
                `shouldBe` 33.0

        it "an additive modifier on endurance scales the derived pool" $
            -- Lua: unit.getStat "endurance" = 2 + 1 = 3, x 10 = 30.
            maxStaminaFor now
                (mkInst (HM.fromList [("endurance", 2.0)])
                        (HM.fromList [("endurance", [modifier 1.0 0.0 Nothing])]))
                `shouldBe` 30.0

        it "a percentage modifier on endurance scales the derived pool" $
            -- Lua: 2 x (1 + 0.5) = 3, x 10 = 30.
            maxStaminaFor now
                (mkInst (HM.fromList [("endurance", 2.0)])
                        (HM.fromList [("endurance", [modifier 0.0 0.5 Nothing])]))
                `shouldBe` 30.0

        it "additive and percentage modifiers on endurance compose as (base + sum delta) x (1 + sum percent)" $
            -- Lua/effectiveStat: (2 + 1 + 1) x (1 + 0.5 + 0.25) = 7,
            -- x 10 = 70. Two modifiers per component, so a formula that
            -- took only the first would fail here too.
            maxStaminaFor now
                (mkInst (HM.fromList [("endurance", 2.0)])
                        (HM.fromList
                            [ ("endurance"
                              , [ modifier 1.0 0.5  Nothing
                                , modifier 1.0 0.25 Nothing ]) ]))
                `shouldBe` 70.0

        it "an additive modifier on an explicit max_stamina attribute applies" $
            maxStaminaFor now
                (mkInst (HM.fromList [("max_stamina", 20.0)])
                        (HM.fromList [("max_stamina", [modifier 5.0 0.0 Nothing])]))
                `shouldBe` 25.0

        it "a percentage modifier on an explicit max_stamina attribute applies" $
            maxStaminaFor now
                (mkInst (HM.fromList [("max_stamina", 20.0)])
                        (HM.fromList [("max_stamina", [modifier 5.0 0.25 Nothing])]))
                `shouldBe` 31.25

        it "the explicit attribute wins outright: an endurance modifier is ignored when max_stamina is set" $
            -- scripts/unit_stats.lua's stats.get never reaches the
            -- derived arm when the attribute exists, so the +8
            -- endurance modifier (which would give a 100 pool) must
            -- contribute nothing.
            maxStaminaFor now
                (mkInst (HM.fromList [("max_stamina", 20.0), ("endurance", 2.0)])
                        (HM.fromList [("endurance", [modifier 8.0 0.0 Nothing])]))
                `shouldBe` 20.0

        it "the explicit attribute's OWN modifier still applies alongside an ignored endurance one" $
            maxStaminaFor now
                (mkInst (HM.fromList [("max_stamina", 20.0), ("endurance", 2.0)])
                        (HM.fromList
                            [ ("max_stamina", [modifier 5.0  0.0 Nothing])
                            , ("endurance",   [modifier 8.0  0.0 Nothing]) ]))
                `shouldBe` 25.0

        it "effectiveStat's clamp holds: a debuff can't drive the pool negative" $
            maxStaminaFor now
                (mkInst (HM.fromList [("endurance", 2.0)])
                        (HM.fromList [("endurance", [modifier (-5.0) 0.0 Nothing])]))
                `shouldBe` 0.0

    -- ---- Expiry, on the exact Unit.Stats.effectiveStat boundary ----
    describe "maxStaminaFor honours modifier expiry at the resolution's game time" $ do
        let expiring t = mkInst (HM.fromList [("endurance", 2.0)])
                                (HM.fromList
                                    [("endurance", [modifier 1.0 0.0 (Just t)])])

        it "a modifier expiring in the future contributes" $
            maxStaminaFor now (expiring 100.5) `shouldBe` 30.0

        it "a modifier is INACTIVE exactly at its expiry (active iff now < smExpiry)" $
            maxStaminaFor now (expiring 100.0) `shouldBe` 20.0

        it "an already-expired modifier contributes nothing" $
            maxStaminaFor now (expiring 99.5) `shouldBe` 20.0

    -- ---- Consumer 1: the per-swing drain ----
    describe "maxStaminaFor's drain consumer (staminaDrainStats) sizes the swing against the effective pool" $ do
        let drainedStamina inst mode =
                HM.lookup "stamina" (staminaDrainStats now mode inst)
            withStamina stats mods =
                mkInst (HM.insert "stamina" 40.0 (HM.fromList stats)) mods

        it "an endurance modifier changes what a heavy swing costs" $ do
            -- Raw endurance 2 -> pool 20 -> heavy cost 5 -> 35 left.
            drainedStamina (withStamina [("endurance", 2.0)] HM.empty) Heavy
                `shouldSatisfy` nearStamina 35.0
            -- Effective endurance 3 -> pool 30 -> heavy cost 7.5 -> 32.5.
            drainedStamina
                (withStamina [("endurance", 2.0)]
                    (HM.fromList [("endurance", [modifier 1.0 0.0 Nothing])]))
                Heavy `shouldSatisfy` nearStamina 32.5

        it "a modifier on an explicit max_stamina attribute changes what a quick swing costs" $ do
            -- Pool 20 -> quick cost 1 -> 39 left.
            drainedStamina (withStamina [("max_stamina", 20.0)] HM.empty) Quick
                `shouldSatisfy` nearStamina 39.0
            -- Pool 20 + 20% = 24 -> quick cost 1.2 -> 38.8.
            drainedStamina
                (withStamina [("max_stamina", 20.0)]
                    (HM.fromList [("max_stamina", [modifier 0.0 0.2 Nothing])]))
                Quick `shouldSatisfy` nearStamina 38.8

        it "an expired modifier costs exactly what no modifier costs" $
            drainedStamina
                (withStamina [("endurance", 2.0)]
                    (HM.fromList
                        [("endurance", [modifier 1.0 0.0 (Just 99.5)])]))
                Heavy
                `shouldBe` drainedStamina
                    (withStamina [("endurance", 2.0)] HM.empty) Heavy

    -- ---- Consumer 2: the winded attacker's damage multiplier ----
    describe "maxStaminaFor's damage consumer (computeSeverity) divides stamina by the effective pool" $ do
        -- staminaFrac = clamp 0.3 1.0 (stamina / maxStaminaFor). All
        -- three fixtures carry stamina 10 and identical strength /
        -- dexterity / body_mass / height, so severity can only move
        -- through the pool.
        let buffed = mkInst
                (HM.insert "stamina" 10.0 (HM.insert "endurance" 1.0 atkBase))
                (HM.fromList [("endurance", [modifier 1.0 0.0 Nothing])])
            rawEquivalent = mkInst
                (HM.insert "stamina" 10.0 (HM.insert "endurance" 2.0 atkBase))
                HM.empty
            unbuffed = mkInst
                (HM.insert "stamina" 10.0 (HM.insert "endurance" 1.0 atkBase))
                HM.empty

        it "a buffed attacker resolves exactly like one whose RAW endurance already equals the effective value" $
            -- Both pools are 20, so both are half-winded (frac 0.5).
            severityOf buffed `shouldBe` severityOf rawEquivalent

        it "and differs from the same attacker without the buff" $
            -- Pool 10, frac 1.0 — a bigger pool means a MORE winded
            -- fighter at the same absolute stamina, so ignoring the
            -- modifier is observable in the damage, not just the pool.
            severityOf buffed `shouldNotBe` severityOf unbuffed

        it "an expired modifier resolves exactly like no modifier" $ do
            let expired = mkInst
                    (HM.insert "stamina" 10.0 (HM.insert "endurance" 1.0 atkBase))
                    (HM.fromList
                        [("endurance", [modifier 1.0 0.0 (Just 99.5)])])
            severityOf expired `shouldBe` severityOf unbuffed

    -- ---- Both consumers, one instant ----
    describe "maxStaminaFor's two combat consumers agree at an expiry boundary" $
        it "a modifier expiring exactly at the resolution's game time is inactive for the drain AND the damage fraction" $ do
            -- One captured sample (`now`) reaches both call sites, so
            -- the boundary cannot resolve one way for the swing's cost
            -- and the other way for its energy.
            let stats   = HM.insert "stamina" 10.0
                            (HM.insert "endurance" 1.0 atkBase)
                atBound = mkInst stats
                    (HM.fromList
                        [("endurance", [modifier 1.0 0.0 (Just now)])])
                plain   = mkInst stats HM.empty
            maxStaminaFor now atBound `shouldBe` maxStaminaFor now plain
            HM.lookup "stamina" (staminaDrainStats now Heavy atBound)
                `shouldBe` HM.lookup "stamina" (staminaDrainStats now Heavy plain)
            severityOf atBound `shouldBe` severityOf plain
