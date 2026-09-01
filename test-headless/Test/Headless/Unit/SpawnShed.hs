{-# LANGUAGE Strict #-}
-- | Pure tests for the spawn-time capacity shed (#1213): the shed
--   decision judges the starting loadout against the EFFECTIVE
--   carrying capacity — the rolled base with the def's innate
--   modifiers and starting-accessory buffs applied, the same
--   'Unit.Stats.effectiveStat' measure live gameplay (pickup gating,
--   the strict transfer policy) uses — never the bare base stat.
module Test.Headless.Unit.SpawnShed (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemDef(..), ItemInstance(..), ItemBuff(..),
                   ItemManager(..), emptyItemManager, itemTotalWeight)
import Unit.Thread.Command.Spawn (spawnModifierMap, spawnEffectiveCapacity,
                                  shedPlan, ShedEvent(..))
import Unit.Types

-- | Minimal UnitDef — only udModifiers matters here.
mkUnitDef ∷ [(Text, StatModifier)] → UnitDef
mkUnitDef mods = UnitDef
    { udName          = "test_unit"
    , udNamePool      = Nothing
    , udDisplayName   = Nothing
    , udTexture       = TextureHandle 0
    , udPortrait      = Nothing
    , udDirSprites    = Map.empty
    , udBaseWidth     = 0
    , udMaxSpeed      = 1.0
    , udRunThreshold  = 0.6
    , udAnimations    = HM.empty
    , udStateAnims    = HM.empty
    , udEagerStats    = False
    , udStatTemplates = HM.empty
    , udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts          = []
    , udNaturalResistance  = defaultNaturalResistance
    , udNaturalWeapon      = Nothing
    , udModifiers          = mods
    }

-- | Minimal ItemDef — only idBuffs (and the name key) matter here.
mkItemDef ∷ Text → [ItemBuff] → ItemDef
mkItemDef name buffs = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0, idWeight = 0, idWeightSpec = Nothing
    , idBulk = 1.0, idStorage = Nothing
    , idKind = "misc", idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing
    , idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = buffs, idInsulation = 0
    , idSourcePath = "test-fixture"
    }

-- | Bare item instance of a given empty weight.
mkItem ∷ Text → Word64 → Float → ItemInstance
mkItem name iid w = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = w
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

-- | The technomule shape: an innate, permanent +50% capacity modifier.
pctCapMod ∷ Float → (Text, StatModifier)
pctCapMod p = ("carrying_capacity"
              , StatModifier { smDelta = 0, smSource = "cybernetic"
                             , smExpiry = Nothing, smPercent = p })

-- | An accessory def name whose buff raises carrying capacity.
harnessDef ∷ Float → Bool → ItemDef
harnessDef pct scales = mkItemDef "cargo_harness"
    [ ItemBuff { ibStat = "carrying_capacity", ibAmount = 0
               , ibPercent = pct, ibScalesWithCondition = scales } ]

statsWithCap ∷ Float → HM.HashMap Text Float
statsWithCap c = HM.fromList [("carrying_capacity", c)]

-- | Total projection of the effective capacity for a rolled base —
--   the Nothing case has its own dedicated test below.
capFor ∷ HM.HashMap Text [StatModifier] → Float → Float
capFor mods base =
    fromMaybe 0 (spawnEffectiveCapacity 0 (statsWithCap base) mods)

spec ∷ Spec
spec = describe "Spawn capacity shedding" $ do
    let weigh = itemTotalWeight emptyItemManager
        -- Base capacity 100, +50% innate → effective 150.
        muleDef  = mkUnitDef [pctCapMod 0.5]
        plainDef = mkUnitDef []

    describe "spawnEffectiveCapacity" $ do
        it "applies an innate percentage modifier to the base stat" $ do
            let mods = spawnModifierMap emptyItemManager muleDef []
            spawnEffectiveCapacity 0 (statsWithCap 100) mods
                `shouldBe` Just 150
        it "returns the bare base for a modifier-free def" $ do
            let mods = spawnModifierMap emptyItemManager plainDef []
            spawnEffectiveCapacity 0 (statsWithCap 100) mods
                `shouldBe` Just 100
        it "returns Nothing when the def rolls no carrying_capacity" $ do
            let mods = spawnModifierMap emptyItemManager muleDef []
            spawnEffectiveCapacity 0 HM.empty mods `shouldBe` Nothing

    describe "innate percentage modifier (technomule shape)" $ do
        let mods = spawnModifierMap emptyItemManager muleDef []
            cap  = capFor mods 100   -- 150
            -- 120 kg of un-prioritized cargo: fits 150, exceeds 100.
            cargo = [ (mkItem "crate" 1 60, 0)
                    , (mkItem "crate" 2 60, 0) ]
        it "a load between base and effective capacity sheds nothing" $
            fst (shedPlan weigh cap 0 cargo)
                `shouldBe` map fst cargo
        it "and warns nothing (no events at all)" $
            snd (shedPlan weigh cap 0 cargo) `shouldBe` []
        it "a load exceeding effective capacity still sheds by priority" $ do
            -- 190 kg total; pick (prio 2) goes before shovel (prio 1).
            let tagged = [ (mkItem "shovel" 1 20, 1)
                         , (mkItem "crate"  2 140, 0)
                         , (mkItem "pick"   3 30, 2) ]
                (kept, evs) = shedPlan weigh cap 0 tagged
            map iiDefName kept `shouldBe` ["crate"]
            evs `shouldBe` [ ShedDrop "pick"   190
                           , ShedDrop "shovel" 160 ]

    describe "modifier-free def (acolyte shape) is unchanged" $ do
        let mods = spawnModifierMap emptyItemManager plainDef []
            cap  = capFor mods 100
        it "sheds highest priority first until the load fits" $ do
            let tagged = [ (mkItem "pick"   1 30, 2)
                         , (mkItem "shovel" 2 25, 1)
                         , (mkItem "ration" 3 50, 0) ]
                (kept, evs) = shedPlan weigh cap 0 tagged
            -- 105 > 100 → drop the pick (75 fits); shovel survives.
            map iiDefName kept `shouldBe` ["shovel", "ration"]
            evs `shouldBe` [ShedDrop "pick" 105]
        it "warns when over capacity with nothing sheddable" $ do
            let tagged = [(mkItem "anvil" 1 500, 0)]
                (kept, evs) = shedPlan weigh cap 20 tagged
            map iiDefName kept `shouldBe` ["anvil"]
            evs `shouldBe` [ShedOverCapacity 520]

    describe "starting-accessory capacity buff (#1213 spec addition)" $ do
        -- 100 kg of cargo, base capacity 80: only the accessory's
        -- +50% buff (→ 120) makes it fit.
        let cargo = [ (mkItem "pick"  1 40, 1)
                    , (mkItem "crate" 2 60, 0) ]
        it "the buff changes the shedding decision" $ do
            let mgr  = ItemManager (HM.fromList
                           [("cargo_harness", harnessDef 0.5 False)])
                acc  = mkItem "cargo_harness" 9 0
                mods = spawnModifierMap mgr plainDef [acc]
                cap  = capFor mods 80
            cap `shouldBe` 120
            shedPlan (itemTotalWeight mgr) cap 0 cargo
                `shouldBe` (map fst cargo, [])
        it "without the buff the same load sheds" $ do
            let mods = spawnModifierMap emptyItemManager plainDef []
                cap  = capFor mods 80
                (kept, evs) = shedPlan weigh cap 0 cargo
            map iiDefName kept `shouldBe` ["crate"]
            evs `shouldBe` [ShedDrop "pick" 100]
        it "a condition-scaling buff confers condition/100 of itself" $ do
            -- 50% condition on a scales_with_condition +50% buff →
            -- +25% → capacity 100 < 100 kg load? 80×1.25 = 100 →
            -- fits exactly at the ≤ boundary.
            let mgr  = ItemManager (HM.fromList
                           [("cargo_harness", harnessDef 0.5 True)])
                acc  = (mkItem "cargo_harness" 9 0) { iiCondition = 50 }
                mods = spawnModifierMap mgr plainDef [acc]
            spawnEffectiveCapacity 0 (statsWithCap 80) mods
                `shouldBe` Just 100
        it "innate and accessory modifiers compose on one map" $ do
            -- +50% innate and +50% accessory sum on the percent axis:
            -- 80 × (1 + 0.5 + 0.5) = 160.
            let mgr  = ItemManager (HM.fromList
                           [("cargo_harness", harnessDef 0.5 False)])
                acc  = mkItem "cargo_harness" 9 0
                mods = spawnModifierMap mgr muleDef [acc]
            spawnEffectiveCapacity 0 (statsWithCap 80) mods
                `shouldBe` Just 160
