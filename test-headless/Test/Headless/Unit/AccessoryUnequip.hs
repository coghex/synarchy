-- | Accessory unequip re-derives worn-accessory buffs (#1209, epic
--   #299's duplicate-accessory contract).
--
--   Accessory stat modifiers key on the item's DISPLAY NAME as their
--   shared source, and duplicate same-source accessories are a
--   supported live state: two technogoggles, or two different defs that
--   happen to share one @display_name@. Unequip used to remove every
--   modifier carrying the departing item's source and stop there, which
--   silently dropped the buff of any copy still worn.
--
--   These run the pure core
--   ('Engine.Scripting.Lua.API.Equipment.Accessory.unequipAccessoryAt'
--   and its 'UnitManager' wrapper) that @equipment.unequipAccessory@
--   wraps verbatim, so the derivation is gated without booting Lua. The
--   live end-to-end duplicate behavior has its own gate in
--   @tools/repair_item_probe.py@.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "accessory unequip"'@
module Test.Headless.Unit.AccessoryUnequip (spec) where

import UPrelude
import Test.Hspec
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.API.Equipment.Accessory
    (unequipAccessoryAt, unequipAccessoryFromUnit)
import Item.Types (ItemBuff(..), ItemDef(..), ItemInstance(..),
                   ItemManager(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Stats (applyItemBuffs)
import Unit.Types (StatModifier(..), UnitId(..), UnitInstance(..),
                   UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))

-- | A bare accessory def. @disp@ is the MODIFIER SOURCE — deliberately
--   independent of @name@ (the def key), because two distinct defs
--   sharing one display name collapse onto the same source and are the
--   case requirement 2 turns on.
accDef ∷ Text → Text → [ItemBuff] → ItemDef
accDef name disp buffs = ItemDef
    { idName = name, idDisplayName = disp, idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.2, idWeightSpec = Nothing, idBulk = 0.8
    , idStorage = Nothing, idKind = "accessory"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = buffs, idInsulation = 0
    , idSourcePath = "test-fixture"
    }

buff ∷ Text → Float → Bool → ItemBuff
buff stat amt scales = ItemBuff
    { ibStat = stat, ibAmount = amt, ibPercent = 0
    , ibScalesWithCondition = scales }

-- | Two DEFS, one display name. @goggles@ buffs perception only;
--   @visor@ overlaps on perception and adds a stat nothing else touches,
--   so an ordered re-derivation is observable on both axes at once.
--   @implant@ is the `unequippable` refusal case.
itemMgr ∷ ItemManager
itemMgr = ItemManager $ HM.fromList
    [ ("technogoggles", accDef "technogoggles" "Technogoggles"
                          [buff "perception" 1.0 True])
    , ("salvaged_visor", accDef "salvaged_visor" "Technogoggles"
                          [buff "perception" 0.5 False
                          ,buff "willpower"  3.0 False])
    , ("bonded_implant", (accDef "bonded_implant" "Bonded Implant"
                            [buff "strength" 1.0 False])
                            { idUnequippable = True })
    , ("field_ration", accDef "field_ration" "Field Ration" [])
    ]

acc ∷ Text → Word64 → Float → ItemInstance
acc defName iid condition = ItemInstance
    { iiDefName     = defName
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = condition
    , iiWeight      = 0.2
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 0.8
    , iiStorage     = Nothing
    }

-- | A modifier from a source no accessory owns. Requirement 3's guard:
--   it sits on an AFFECTED stat, so an implementation that rebuilt
--   `uiModifiers` from an empty map instead of from the live one would
--   drop it.
stimulant ∷ StatModifier
stimulant = StatModifier
    { smDelta = 5.0, smSource = "Stimulant"
    , smExpiry = Just 900.0, smPercent = 0.2 }

-- | A modifier on a stat NO worn accessory touches — the other half of
--   requirement 3.
oldWound ∷ StatModifier
oldWound = StatModifier
    { smDelta = -2.0, smSource = "wounded-left-arm"
    , smExpiry = Nothing, smPercent = 0 }

-- | A unit wearing @worn@, carrying @inv@, with the two unrelated
--   modifiers above already live and every accessory's buffs folded in
--   as `equipAccessory` would have left them.
unitWearing ∷ [ItemInstance] → [ItemInstance] → UnitInstance
unitWearing worn inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = WorldPageId "main_world"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty
    , uiModifiers = HM.fromList
        [ ("perception", [stimulant]), ("strength", [oldWound]) ]
    , uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = worn, uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- | Equip-time state: the unit above with each worn accessory's buffs
--   folded in through the SHIPPED equip primitive, in list order —
--   exactly what a sequence of `equipAccessory` calls leaves behind, so
--   every case starts from a state the engine can really be in. Each
--   group's first example pins the resulting values against hand-computed
--   literals, so the fixture is checked rather than assumed.
wearing ∷ [ItemInstance] → [ItemInstance] → UnitInstance
wearing worn inv =
    let inst = unitWearing worn inv
    in inst { uiModifiers = foldl' equipOne (uiModifiers inst) worn }
  where
    ItemManager defs = itemMgr
    equipOne mods it = case HM.lookup (iiDefName it) defs of
        Nothing → mods
        Just d  → applyItemBuffs (idDisplayName d) (iiCondition it)
                                 (idBuffs d) mods

-- | Every modifier on @stat@ as (source, delta) pairs, source-sorted so
--   the assertion is order-insensitive — the modifier LIST order within
--   a stat is not a contract, which source is present with which value
--   is.
modsOn ∷ Text → UnitInstance → [(Text, Float)]
modsOn stat inst =
    sortOn fst [ (smSource m, smDelta m)
               | m ← HM.lookupDefault [] stat (uiModifiers inst) ]

wornIds ∷ UnitInstance → [Word64]
wornIds = fmap iiInstanceId . uiAccessories

invIds ∷ UnitInstance → [Word64]
invIds = fmap iiInstanceId . uiInventory

-- | Unequip at a 0-based index, failing the example if it refused.
unequipped ∷ Int → UnitInstance → IO UnitInstance
unequipped idx inst = case unequipAccessoryAt itemMgr idx inst of
    Just inst' → pure inst'
    Nothing    → do
        expectationFailure ("unequip at index " ⧺ show idx ⧺ " refused")
        pure inst

spec ∷ Spec
spec = describe "accessory unequip re-derives worn buffs (#1209)" $ do

    describe "duplicate instances of one definition" $ do
        -- Older pair at 80% condition (0.8 perception), newer at 40%
        -- (0.4). Last-equipped wins, so 0.4 is live before either goes.
        let worn = [acc "technogoggles" 1 80, acc "technogoggles" 2 40]

        it "starts with the newer copy's condition-scaled buff live" $
            modsOn "perception" (wearing worn [])
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.4)]

        it "unequipping the OLDER copy leaves the newer one's buff" $ do
            inst ← unequipped 0 (wearing worn [])
            modsOn "perception" inst
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.4)]

        it "unequipping the NEWER copy reactivates the older one" $ do
            inst ← unequipped 1 (wearing worn [])
            modsOn "perception" inst
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.8)]

    describe "different definitions sharing a display name" $ do
        -- goggles → perception 0.8 (80% of 1.0); visor → perception 0.5
        -- and willpower 3.0, both flat. Same source: "Technogoggles".
        let worn = [acc "technogoggles" 1 80, acc "salvaged_visor" 2 100]

        it "starts with the visor (last) owning both its stats" $ do
            let inst = wearing worn []
            modsOn "perception" inst
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.5)]
            modsOn "willpower" inst `shouldBe` [("Technogoggles", 3.0)]

        it "unequipping the visor restores the goggles and drops its \
           \willpower" $ do
            inst ← unequipped 1 (wearing worn [])
            modsOn "perception" inst
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.8)]
            modsOn "willpower" inst `shouldBe` []

        it "unequipping the goggles keeps the visor on both stats" $ do
            inst ← unequipped 0 (wearing worn [])
            modsOn "perception" inst
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.5)]
            modsOn "willpower" inst `shouldBe` [("Technogoggles", 3.0)]

        it "retains a non-overlapping buff owned by an EARLIER remaining \
           \accessory" $ do
            -- [visor, goggles, goggles']: perception belongs to the last
            -- goggles, willpower only to the first visor. Dropping the
            -- last must re-derive BOTH from what is still worn.
            let worn3 = [ acc "salvaged_visor" 1 100
                        , acc "technogoggles"  2 80
                        , acc "technogoggles"  3 40 ]
            inst ← unequipped 2 (wearing worn3 [])
            modsOn "perception" inst
                `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.8)]
            modsOn "willpower" inst `shouldBe` [("Technogoggles", 3.0)]

    describe "the only copy" $ do
        it "removes the accessory's modifier entirely" $ do
            let worn = [acc "technogoggles" 1 80]
            inst ← unequipped 0 (wearing worn [])
            modsOn "perception" inst `shouldBe` [("Stimulant", 5.0)]
            uiAccessories inst `shouldBe` []

        it "removes every stat the departing source owned" $ do
            let worn = [acc "salvaged_visor" 1 100]
            inst ← unequipped 0 (wearing worn [])
            modsOn "perception" inst `shouldBe` [("Stimulant", 5.0)]
            modsOn "willpower"  inst `shouldBe` []

    describe "unrelated modifier sources" $ do
        it "leaves an unrelated modifier on an AFFECTED stat untouched" $ do
            let worn = [acc "technogoggles" 1 80, acc "technogoggles" 2 40]
            inst ← unequipped 0 (wearing worn [])
            let live = [ m | m ← HM.lookupDefault [] "perception"
                                    (uiModifiers inst)
                           , smSource m ≡ "Stimulant" ]
            live `shouldBe` [stimulant]

        it "leaves a modifier on an untouched stat alone" $ do
            let worn = [acc "technogoggles" 1 80]
            inst ← unequipped 0 (wearing worn [])
            HM.lookupDefault [] "strength" (uiModifiers inst)
                `shouldBe` [oldWound]

    describe "mutation contract" $ do
        let worn = [ acc "technogoggles" 1 80
                   , acc "salvaged_visor" 2 100
                   , acc "technogoggles"  3 40 ]
            inv  = [acc "field_ration" 9 100]

        it "appends the popped instance to the END of inventory" $ do
            inst ← unequipped 1 (wearing worn inv)
            invIds inst `shouldBe` [9, 2]
            fmap iiCondition (uiInventory inst) `shouldBe` [100, 100]

        it "preserves the order of the remaining accessories" $ do
            inst ← unequipped 1 (wearing worn inv)
            wornIds inst `shouldBe` [1, 3]

        it "moves the instance verbatim" $ do
            inst ← unequipped 0 (wearing worn inv)
            drop 1 (uiInventory inst) `shouldBe` [acc "technogoggles" 1 80]

    describe "refusals leave the unit untouched" $ do
        let worn = [acc "technogoggles" 1 80, acc "bonded_implant" 2 100]
            inst = wearing worn []

        it "refuses an `unequippable` definition" $
            unequipAccessoryAt itemMgr 1 inst `shouldBe` Nothing

        it "refuses an index past the end" $
            unequipAccessoryAt itemMgr 2 inst `shouldBe` Nothing

        it "refuses a negative index" $
            unequipAccessoryAt itemMgr (-1) inst `shouldBe` Nothing

        it "still unequips a normal accessory from the same unit" $ do
            inst' ← unequipped 0 inst
            wornIds inst' `shouldBe` [2]

    describe "UnitManager wrapper" $ do
        let uid = UnitId 1
            um  = emptyUnitManager
                    { umInstances = HM.singleton uid
                        (wearing [ acc "technogoggles" 1 80
                                 , acc "technogoggles" 2 40 ] []) }

        it "reports success and rebuilds from the remaining copy" $ do
            let (um', ok) = unequipAccessoryFromUnit itemMgr uid 0 um
            ok `shouldBe` True
            case HM.lookup uid (umInstances um') of
                Just inst → modsOn "perception" inst
                    `shouldBe` [("Stimulant", 5.0), ("Technogoggles", 0.4)]
                Nothing → expectationFailure "unit vanished from the manager"

        it "a missing unit returns False with no mutation" $
            unequipAccessoryFromUnit itemMgr (UnitId 99) 0 um
                `shouldBe` (um, False)

        it "an out-of-range index returns False with no mutation" $
            unequipAccessoryFromUnit itemMgr uid 7 um `shouldBe` (um, False)
