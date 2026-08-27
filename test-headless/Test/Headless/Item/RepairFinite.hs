-- | Non-finite repair deltas are refused, at both entry points (#1732).
--
--   `unit.repairItem` narrows its two Lua arguments straight to the
--   engine's 32-bit `Float` and hands them to a shared pure core whose
--   only bound is `max 0 (min 100 x)`. Those bare `max`/`min` calls do
--   not define a non-finite policy: against a 50/50 item, a NaN delta
--   used to leave the axis at `0.0` and report `-50` as a successful
--   repair, while `math.huge` was a free, complete restore of both axes
--   that bypassed `repair.repairAt`'s recipe gate and ingredient cost.
--   Both results landed inside the documented 0..100 wear domain, so no
--   downstream range check could tell them from an intentional `-1000`
--   wear or a legitimate full repair.
--
--   Four claims are gated here, each against the production functions
--   rather than a restatement of them:
--
--   1. __The refusal is the shared core's, not the verb's.__ Every case
--      below calls `applyRepairToUnit` — the one function
--      `unitRepairItemFn` and `applyRepairAt` both go through — so a
--      future third caller inherits the rule instead of needing its own
--      copy. A rejected call returns `Nothing`, which is the verb's
--      existing nil failure shape, and the whole `UnitInstance` is
--      therefore left untouched by construction.
--
--   2. __A mixed call is rejected WHOLE.__ Both orientations are
--      checked with a NONZERO finite sibling, so a pass cannot come
--      from the finite axis happening to move by zero.
--
--   3. __Finiteness is judged AFTER narrowing.__ `1.0e300` is a
--      perfectly ordinary finite `Double` that becomes `+Infinity` in
--      the engine's `Float` field — the same post-narrowing rule
--      `Engine.Asset.YamlItems.requirePositiveQuantity` applies to
--      authored scalars. The test asserts the narrowing itself first,
--      so it fails loudly if `Float` ever stops being 32-bit rather
--      than quietly testing nothing.
--
--   4. __`repairAt` refuses before it charges.__ Its delta is DERIVED
--      (`100 - current`), so a NaN stored axis produces a NaN delta —
--      and `current ≥ 100` is False for NaN, which is what lets such a
--      value reach the mutation at all. The refusal must come out as
--      the verb's existing `Left` reason with the recipe's ingredients
--      still in the unit's inventory.
--
--   The finite-delta control cases sit beside each rejection so the
--   guard cannot pass by refusing everything.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "repair"'@. The live-engine half is
--   @tools/repair_item_probe.py@ (the primitive) and
--   @tools/repair_probe.py@ (the station/recipe/ingredient policy).
module Test.Headless.Item.RepairFinite (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Craft.Types
    ( RecipeDef(..), RecipeIngredient(..), RepairAxis(..) )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.API.Repair (applyRepairAt)
import Engine.Scripting.Lua.API.Units (applyRepairToUnit)
import Item.Types (ItemInstance(..), ItemManager, emptyItemManager)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))

-- | The two non-finite `Float`s a Lua argument can narrow to, plus NaN.
--   `0/0` and `1/0` are written as divisions rather than named
--   constants because Haskell has no literal for either.
nan, posInf, negInf ∷ Float
nan    = 0 / 0
posInf = 1 / 0
negInf = -1 / 0

-- | Every non-finite value the guard must refuse, labelled for the
--   failure message.
nonFinites ∷ [(String, Float)]
nonFinites = [("NaN", nan), ("+Infinity", posInf), ("-Infinity", negInf)]

targetId ∷ Word64
targetId = 7

-- | A worn tool at 50/50 — mid-range on BOTH axes, so a rejected call
--   that nevertheless clamped would be visible in either direction.
target ∷ ItemInstance
target = mkItem "pick_steel" targetId 50 50

mkItem ∷ Text → Word64 → Float → Float → ItemInstance
mkItem name iid cond sharp = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = cond
    , iiWeight      = 1
    , iiSharpness   = sharp
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

mkUnit ∷ [ItemInstance] → UnitInstance
mkUnit inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = WorldPageId "test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

itemMgr ∷ ItemManager
itemMgr = emptyItemManager

-- | Apply against the standard 50/50 unit, returning just the report.
repairOn ∷ UnitInstance → Float → Float
         → Maybe (UnitInstance, (Text, Float, Float, Float, Float))
repairOn u condD sharpD = applyRepairToUnit targetId condD sharpD itemMgr u

-- | The condition/sharpness pair a report carries.
axes ∷ (Text, Float, Float, Float, Float) → (Float, Float)
axes (_, cond, sharp, _, _) = (cond, sharp)

-- | The applied-delta pair a report carries.
applied ∷ (Text, Float, Float, Float, Float) → (Float, Float)
applied (_, _, _, cApp, sApp) = (cApp, sApp)

-- ---------------------------------------------------------------------
-- repairAt fixtures
-- ---------------------------------------------------------------------

repairUid ∷ UnitId
repairUid = UnitId 1

-- | The shipped condition-repair recipe's shape: one consumed input,
--   no outputs, axis = condition.
conditionRecipe ∷ RecipeDef
conditionRecipe = RecipeDef
    { rdId         = "repair_condition"
    , rdName       = "Repair Condition"
    , rdStation    = "repair_condition"
    , rdInputs     = [RecipeIngredient "lignite_chunk" 1]
    , rdFuel       = Nothing
    , rdWork       = 20
    , rdOutputs    = []
    , rdKnowledge  = Nothing
    , rdSkill      = Nothing
    , rdRepairAxis = Just RepairCondition
    , rdOutputTemp = Nothing
    , rdPowerDraw  = 0
    }

fuelId ∷ Word64
fuelId = 99

-- | A unit holding the recipe's ingredient plus the targeted tool.
repairManager ∷ ItemInstance → UnitManager
repairManager it = emptyUnitManager
    { umInstances = HM.singleton repairUid
        (mkUnit [mkItem "lignite_chunk" fuelId 100 100, it]) }

fuelCount ∷ UnitManager → Int
fuelCount um = case HM.lookup repairUid (umInstances um) of
    Nothing → -1
    Just u  → length [ () | i ← uiInventory u
                          , iiDefName i ≡ "lignite_chunk" ]

heldCondition ∷ UnitManager → Maybe Float
heldCondition um = do
    u  ← HM.lookup repairUid (umInstances um)
    it ← case [ i | i ← uiInventory u, iiInstanceId i ≡ targetId ] of
        (i:_) → Just i
        []    → Nothing
    pure (iiCondition it)

spec ∷ Spec
spec = do
    describe "non-finite repair deltas are refused (#1732)" $ do
        forM_ nonFinites $ \(label, bad) → do
            it ("rejects a " <> label <> " condition delta") $
                repairOn (mkUnit [target]) bad 0 `shouldSatisfy` isNothingResult

            it ("rejects a " <> label <> " sharpness delta") $
                repairOn (mkUnit [target]) 0 bad `shouldSatisfy` isNothingResult

            -- A mixed call must reject WHOLE. The finite sibling is
            -- nonzero in both orientations, so "the finite axis did not
            -- move" is a real observation rather than a no-op delta.
            it ("rejects a finite condition delta paired with "
                <> label <> " sharpness") $
                repairOn (mkUnit [target]) (-20) bad
                    `shouldSatisfy` isNothingResult

            it ("rejects a finite sharpness delta paired with "
                <> label <> " condition") $
                repairOn (mkUnit [target]) bad (-20)
                    `shouldSatisfy` isNothingResult

        it "reaches the equipment and accessory branches with the same refusal" $ do
            let equipped = (mkUnit []) { uiEquipment =
                    HM.singleton "right_hand" target }
                worn     = (mkUnit []) { uiAccessories = [target] }
            repairOn equipped nan 0    `shouldSatisfy` isNothingResult
            repairOn worn     posInf 0 `shouldSatisfy` isNothingResult
            -- …and finds them when the deltas ARE finite, so the two
            -- refusals above are not just a failed lookup.
            fmap (axes . snd) (repairOn equipped 10 10) `shouldBe` Just (60, 60)
            fmap (axes . snd) (repairOn worn     10 10) `shouldBe` Just (60, 60)

        it "judges finiteness AFTER narrowing to the engine's Float" $ do
            -- 1.0e300 is a finite Double; the engine's field is 32-bit,
            -- so the value the core actually sees is +Infinity. Assert
            -- the narrowing first, or this case could silently stop
            -- testing anything.
            let narrowed = realToFrac (1.0e300 ∷ Double) ∷ Float
            isInfinite narrowed `shouldBe` True
            repairOn (mkUnit [target]) narrowed 0
                `shouldSatisfy` isNothingResult
            repairOn (mkUnit [target]) 0 narrowed
                `shouldSatisfy` isNothingResult
            -- The largest value that still narrows finitely keeps its
            -- existing behaviour: an ordinary saturating full restore.
            let big = realToFrac (1.0e30 ∷ Double) ∷ Float
            isInfinite big `shouldBe` False
            fmap (axes . snd) (repairOn (mkUnit [target]) big big)
                `shouldBe` Just (100, 100)

        it "leaves every finite-delta behaviour unchanged" $ do
            let u = mkUnit [target]
            fmap (axes . snd)    (repairOn u 25 (-30)) `shouldBe` Just (75, 20)
            fmap (applied . snd) (repairOn u 25 (-30)) `shouldBe` Just (25, -30)
            -- Saturation and the partial applied amounts at each bound.
            fmap (axes . snd)    (repairOn u 1000 1000) `shouldBe` Just (100, 100)
            fmap (applied . snd) (repairOn u 1000 1000) `shouldBe` Just (50, 50)
            fmap (axes . snd)    (repairOn u (-1000) (-1000))
                `shouldBe` Just (0, 0)
            -- A zero delta is finite, so it still reports an applied 0
            -- rather than being swept up by the guard.
            fmap (applied . snd) (repairOn u 0 0) `shouldBe` Just (0, 0)
            -- A miss is still a miss, and still indistinguishable in
            -- shape from a rejection.
            applyRepairToUnit 4242 10 10 itemMgr u
                `shouldSatisfy` isNothingResult

        it "preserves list order and the untargeted instance" $ do
            let other = mkItem "pick_steel" 8 50 50
                u     = mkUnit [other, target, mkItem "canteen" 9 50 50]
            case repairOn u 20 20 of
                Nothing → expectationFailure "expected the repair to land"
                Just (u', _) → do
                    map iiInstanceId (uiInventory u')
                        `shouldBe` [8, targetId, 9]
                    map iiCondition (uiInventory u')
                        `shouldBe` [50, 70, 50]

    describe "repair.repairAt refuses a non-finite derived delta (#1732)" $ do
        it "refuses without consuming the recipe's ingredients" $ do
            -- repairAt derives `100 - current`, so a NaN stored axis
            -- yields a NaN delta. `current ≥ 100` is False for NaN,
            -- which is exactly what lets it past the already-full gate.
            let um0 = repairManager (mkItem "pick_steel" targetId nan 50)
                (um1, res) = applyRepairAt RepairCondition conditionRecipe
                                 targetId itemMgr repairUid um0
            res `shouldSatisfy` isLeftResult
            fuelCount um1 `shouldBe` 1
            fuelCount um1 `shouldBe` fuelCount um0

        it "names the failure instead of reporting a missing instance" $ do
            let um0 = repairManager (mkItem "pick_steel" targetId nan 50)
                (_, res) = applyRepairAt RepairCondition conditionRecipe
                               targetId itemMgr repairUid um0
            case res of
                Left err → err `shouldBe` "non-finite condition repair delta"
                Right _  → expectationFailure "expected a refusal"

        it "still repairs and charges for an ordinary worn item" $ do
            let um0 = repairManager (mkItem "pick_steel" targetId 40 50)
                (um1, res) = applyRepairAt RepairCondition conditionRecipe
                                 targetId itemMgr repairUid um0
            case res of
                Left err → expectationFailure ("unexpected refusal: " <> show err)
                Right r  → fst (axes r) `shouldBe` 100
            heldCondition um1 `shouldBe` Just 100
            fuelCount um1 `shouldBe` 0

        it "still refuses an already-full axis before charging" $ do
            let um0 = repairManager (mkItem "pick_steel" targetId 100 50)
                (um1, res) = applyRepairAt RepairCondition conditionRecipe
                                 targetId itemMgr repairUid um0
            res `shouldSatisfy` isLeftResult
            fuelCount um1 `shouldBe` 1
  where
    isNothingResult ∷ Maybe α → Bool
    isNothingResult Nothing = True
    isNothingResult _       = False

    isLeftResult ∷ Either α β → Bool
    isLeftResult (Left _) = True
    isLeftResult _        = False
