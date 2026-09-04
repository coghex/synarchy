{-# LANGUAGE ScopedTypeVariables #-}
-- | #2307: reconciling a SAVED equipment map against the equipment
--   class the unit's definition declares today.
--
--   'Equipment.Reconcile.reconcileUnitEquipment' is the pure half and
--   'pureSpec' pins it on the same terms
--   'Test.Headless.Building.Knowledge''s @dangling records@ examples pin
--   'Building.Knowledge.retainContainers': a retired slot id, a kind
--   mismatch on a slot that survived, and an all-valid map that comes
--   back untouched.
--
--   That is necessary and not sufficient — every example there would
--   stay green if the wiring in 'World.Save.Types.fromUnitSnapshot' or
--   'World.Load.Stage.stagePage' were deleted. 'stagingSpec' therefore
--   drives a forged persisted save through the REAL staging entry point
--   against REAL registered content, and asserts on the units staging
--   actually produced, on the diagnostics it actually emitted, and on
--   the save that session would write back.
module Test.Headless.Equipment.Reconcile (pureSpec, stagingSpec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..), LoggerState
    , defaultLogConfig, initLogger )
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Unit.Direction (Direction(..))
import Equipment.Reconcile
import Equipment.Types
    ( EquipmentClass(..), EquipmentClassManager(..), EquipmentSlot(..)
    , emptyEquipmentClassManager )
import Item.Types
    (ItemDef(..), ItemInstance(..), ItemManager(..), emptyItemManager)
import Structure.Palette (emptyTexPalette)
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance )
import qualified Data.Map.Strict as Map
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Types (StagedSession(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types
    ( SaveData, UnitInstanceSnapshot(..), UnitSnapshot(..), toUnitSnapshot )

-- * Content the examples reconcile against

pageId ∷ WorldPageId
pageId = WorldPageId "equipment_reconcile_staged"

-- | The class every fixture unit declares: two slots, each accepting a
--   DIFFERENT kind, so a kind mismatch on a surviving slot is a real
--   case rather than an unreachable one. @left_hand@ is deliberately
--   absent — it is the id the saves below still carry.
humanoidClass ∷ EquipmentClass
humanoidClass = EquipmentClass
    { ecName = "humanoid", ecSilhouetteTex = TextureHandle 0
    , ecSilhouetteW = 64, ecSilhouetteH = 64
    , ecSlots =
        [ EquipmentSlot { esId = "head", esName = "Headwear"
                        , esKind = "headwear"
                        , esX = 0, esY = 0, esW = 32, esH = 32 }
        , EquipmentSlot { esId = "right_hand", esName = "Right hand"
                        , esKind = "weapon"
                        , esX = 0, esY = 32, esW = 32, esH = 32 } ] }

classMgr ∷ EquipmentClassManager
classMgr = EquipmentClassManager (HM.singleton "humanoid" humanoidClass)

-- | A class declaring one slot id TWICE with different kinds, which the
--   YAML decoder accepts and nothing validates against. The shipped
--   consumers disagree about which declaration wins — @equipment.equip@
--   resolves first-match, unit spawn builds a map and so resolves
--   last-match — so both orderings are exercised below.
duplicateSlotClass ∷ Text → Text → EquipmentClassManager
duplicateSlotClass firstKind secondKind =
    EquipmentClassManager $ HM.singleton "humanoid" humanoidClass
        { ecSlots =
            [ EquipmentSlot { esId = "hand", esName = "Hand"
                            , esKind = firstKind
                            , esX = 0, esY = 0, esW = 32, esH = 32 }
            , EquipmentSlot { esId = "hand", esName = "Hand (alt)"
                            , esKind = secondKind
                            , esX = 0, esY = 32, esW = 32, esH = 32 } ] }

-- | Reconcile one @hand@ entry against a class declaring that id twice.
reconcileDuplicate
    ∷ Text → Text → ItemInstance
    → (HM.HashMap Text ItemInstance, [ItemInstance], [EquipmentOrphan])
reconcileDuplicate firstKind secondKind it =
    reconcileUnitEquipment (duplicateSlotClass firstKind secondKind) itemMgr
        (Just "humanoid") uid1 (HM.singleton "hand" it) []

itemDef ∷ Text → Text → ItemDef
itemDef name kind = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 1.5, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = kind
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0, idSourcePath = "test-fixture" }

-- | @steel_helmet@ fits @head@ only, @steel_dagger@ fits @right_hand@
--   only, so swapping one for the other is exactly a kind mismatch.
itemMgr ∷ ItemManager
itemMgr = ItemManager $ HM.fromList
    [ ("steel_helmet", itemDef "steel_helmet" "headwear")
    , ("steel_dagger", itemDef "steel_dagger" "weapon")
    , ("ration",       itemDef "ration"       "food") ]

-- | A physically distinctive instance: nested contents, an off-default
--   quality/condition/sharpness/fill and its own instance id, so
--   "preserved exactly" is observable rather than trivially true of a
--   default-shaped record.
itemOf ∷ Text → Word64 → ItemInstance
itemOf name iid = ItemInstance
    { iiDefName = name, iiCurrentFill = 0.75, iiQuality = 82
    , iiCondition = 61.5, iiWeight = 1.25, iiSharpness = 37.5
    , iiInstanceId = iid, iiTemp = Just 18.5
    , iiBulk = Just 2.25, iiStorage = Nothing
    , iiContents =
        [ ItemInstance
            { iiDefName = "ration", iiCurrentFill = 0, iiQuality = 40
            , iiCondition = 20, iiWeight = 0.1, iiSharpness = 0
            , iiInstanceId = iid + 500, iiTemp = Nothing
            , iiBulk = Nothing, iiStorage = Nothing, iiContents = [] } ] }

unitDefWithClass ∷ Maybe Text → UnitDef
unitDefWithClass cls = UnitDef
    { udName = "test_unit", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = cls, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | The equipment map every drift example starts from: one entry that
--   is still correct, one under a slot id the class retired, and one
--   under a slot that survived but no longer accepts that kind.
driftedEquipment ∷ HM.HashMap Text ItemInstance
driftedEquipment = HM.fromList
    [ ("head",       itemOf "steel_helmet" 10)
    , ("left_hand",  itemOf "steel_dagger" 11)
    , ("right_hand", itemOf "steel_helmet" 12) ]

-- | Two loose items whose ORDER (and position ahead of anything
--   migrated) the reconciliation must preserve.
looseInventory ∷ [ItemInstance]
looseInventory = [itemOf "ration" 20, itemOf "steel_dagger" 21]

uid1 ∷ UnitId
uid1 = UnitId 1

reconcile
    ∷ Maybe Text → HM.HashMap Text ItemInstance → [ItemInstance]
    → (HM.HashMap Text ItemInstance, [ItemInstance], [EquipmentOrphan])
reconcile cls = reconcileUnitEquipment classMgr itemMgr cls uid1

-- * The pure half

pureSpec ∷ Spec
pureSpec = describe "saved equipment slot reconciliation (#2307)" $ do

  describe "drifted slot keys" $ do

    it "moves an entry whose slot id the class no longer declares" $ do
        let (eq', inv', _) = reconcile (Just "humanoid") driftedEquipment []
        HM.member "left_hand" eq' `shouldBe` False
        map iiInstanceId inv' `shouldContain` [11]

    it "moves an entry whose item kind the SURVIVING slot no longer \
       \accepts" $ do
        let (eq', inv', _) = reconcile (Just "humanoid") driftedEquipment []
        HM.member "right_hand" eq' `shouldBe` False
        map iiInstanceId inv' `shouldContain` [12]

    it "returns an all-valid map completely untouched, and adds nothing \
       \to the inventory" $ do
        let valid = HM.fromList [ ("head",       itemOf "steel_helmet" 10)
                                , ("right_hand", itemOf "steel_dagger" 13) ]
            (eq', inv', orphs) = reconcile (Just "humanoid") valid looseInventory
        eq'   `shouldBe` valid
        inv'  `shouldBe` looseInventory
        orphs `shouldBe` []

    it "restores the still-valid entry in its OWN slot, as the exact \
       \instance the save stored" $ do
        let (eq', _, _) = reconcile (Just "humanoid") driftedEquipment []
        HM.toList eq' `shouldBe` [("head", itemOf "steel_helmet" 10)]

    it "preserves each migrated instance exactly — id, quality, \
       \condition, sharpness, fill and nested contents" $ do
        let (_, inv', _) = reconcile (Just "humanoid") driftedEquipment []
        find ((≡ 11) . iiInstanceId) inv'
            `shouldBe` Just (itemOf "steel_dagger" 11)
        find ((≡ 12) . iiInstanceId) inv'
            `shouldBe` Just (itemOf "steel_helmet" 12)

    it "keeps the saved inventory's own order and puts it FIRST, then \
       \appends the migrated entries in slot-id order" $ do
        let (_, inv', _) = reconcile (Just "humanoid")
                               driftedEquipment looseInventory
        -- left_hand before right_hand: the order unitHeldItems already
        -- presents equipped slots in, never a hashmap enumeration.
        map iiInstanceId inv' `shouldBe` [20, 21, 11, 12]

    it "moves EVERY entry when the unit's definition declares no \
       \equipment class at all" $ do
        let (eq', inv', orphs) = reconcile Nothing driftedEquipment []
        eq' `shouldBe` HM.empty
        map iiInstanceId inv' `shouldBe` [10, 11, 12]
        map eqoCause orphs `shouldBe` replicate 3 EquipmentSlotRetired

    it "moves EVERY entry when the declared class no longer resolves in \
       \the manager" $ do
        let (eq', inv', _) = reconcile (Just "quadruped") driftedEquipment []
        eq' `shouldBe` HM.empty
        map iiInstanceId inv' `shouldBe` [10, 11, 12]

    it "moves EVERY entry when NOTHING is registered, and an all-empty \
       \map still migrates nothing" $ do
        let bare = reconcileUnitEquipment emptyEquipmentClassManager
                       emptyItemManager (Just "humanoid") uid1
            (eq', inv', _) = bare driftedEquipment []
            (eq2, inv2, orphs2) = bare HM.empty looseInventory
        eq'  `shouldBe` HM.empty
        map iiInstanceId inv' `shouldBe` [10, 11, 12]
        eq2   `shouldBe` HM.empty
        inv2  `shouldBe` looseInventory
        orphs2 `shouldBe` []

    -- The item DEFINITION reference stays a hard load rejection
    -- (missingItemDefReferences, run before staging is ever queued), so
    -- this repair must not quietly absorb it by calling an unresolvable
    -- def a kind mismatch.
    it "leaves an entry whose item definition does not resolve in its \
       \own slot, because that reference is still fatal elsewhere" $ do
        let ghost = HM.singleton "head" (itemOf "ghost_helm" 30)
            (eq', inv', orphs) = reconcile (Just "humanoid") ghost []
        eq'   `shouldBe` ghost
        inv'  `shouldBe` []
        orphs `shouldBe` []

    it "still moves an unresolvable-def item when its SLOT is the thing \
       \that went away, which needs no definition to decide" $ do
        let ghost = HM.singleton "left_hand" (itemOf "ghost_dagger" 31)
            (eq', inv', orphs) = reconcile (Just "humanoid") ghost []
        eq'  `shouldBe` HM.empty
        map iiInstanceId inv' `shouldBe` [31]
        map eqoCause orphs `shouldBe` [EquipmentSlotRetired]

  -- Slot ids are not validated for uniqueness, and the two live
  -- producers resolve a duplicated id differently. This repair must
  -- never migrate out an item either of them legitimately placed, so it
  -- accepts an entry that ANY declaration under its id accepts.
  describe "a class declaring one slot id twice" $ do

    it "keeps an item the FIRST declaration accepts, which is the \
       \declaration equipment.equip resolves to" $ do
        let (eq', inv', orphs) =
                reconcileDuplicate "weapon" "headwear"
                    (itemOf "steel_dagger" 40)
        HM.keys eq' `shouldBe` ["hand"]
        inv'  `shouldBe` []
        orphs `shouldBe` []

    it "keeps an item the LAST declaration accepts, which is the \
       \declaration unit spawn resolves to" $ do
        let (eq', inv', orphs) =
                reconcileDuplicate "headwear" "weapon"
                    (itemOf "steel_dagger" 41)
        HM.keys eq' `shouldBe` ["hand"]
        inv'  `shouldBe` []
        orphs `shouldBe` []

    it "still migrates an item NO declaration under that id accepts, \
       \and names the FIRST declaration's kind in the diagnostic" $ do
        let (eq', inv', orphs) =
                reconcileDuplicate "weapon" "accessory"
                    (itemOf "steel_helmet" 42)
        eq' `shouldBe` HM.empty
        map iiInstanceId inv' `shouldBe` [42]
        orphs `shouldBe`
            [ EquipmentOrphan uid1 "hand" "steel_helmet" 42
                  (EquipmentKindMismatch "headwear" "weapon") ]

  describe "the migration diagnostic" $ do

    it "names the unit, the affected slot, the item def, the instance id \
       \and the cause, one entry at a time" $ do
        let (_, _, orphs) = reconcile (Just "humanoid") driftedEquipment []
        orphs `shouldBe`
            [ EquipmentOrphan uid1 "left_hand" "steel_dagger" 11
                  EquipmentSlotRetired
            , EquipmentOrphan uid1 "right_hand" "steel_helmet" 12
                  (EquipmentKindMismatch "headwear" "weapon") ]

    it "renders a retired slot with the page, the unit and the item" $
        renderEquipmentOrphan pageId
            (EquipmentOrphan uid1 "left_hand" "steel_dagger" 11
                 EquipmentSlotRetired)
            `shouldBe`
              "unit #1 on page 'equipment_reconcile_staged': equipped item \
              \'steel_dagger' (instance 11) in slot 'left_hand' is in a \
              \slot the unit's equipment class no longer declares; moved \
              \to the unit's inventory"

    it "renders a kind mismatch with BOTH kinds, so the content edit \
       \that stranded the item is named" $
        renderEquipmentOrphan pageId
            (EquipmentOrphan uid1 "right_hand" "steel_helmet" 12
                 (EquipmentKindMismatch "headwear" "weapon"))
            `shouldBe`
              "unit #1 on page 'equipment_reconcile_staged': equipped item \
              \'steel_helmet' (instance 12) in slot 'right_hand' is kind \
              \'headwear' but that slot now accepts 'weapon'; moved to the \
              \unit's inventory"

-- * The staging half

-- | A logger whose entries are captured in emission order, so what
--   staging SAYS is observable rather than inferred.
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

minimalUnitSnapshot
    ∷ HM.HashMap Text ItemInstance → [ItemInstance] → UnitInstanceSnapshot
minimalUnitSnapshot equipped inventory = UnitInstanceSnapshot
    { uisDefName = "test_unit", uisBaseWidth = 0
    , uisGridX = 0, uisGridY = 0, uisGridZ = 0
    , uisFacing = DirS, uisCurrentAnim = "", uisAnimStart = 0
    , uisAnimReverse = False, uisActivity = "idle", uisPose = "standing"
    , uisAnimStride = 0, uisStats = HM.empty, uisModifiers = HM.empty
    , uisSkills = HM.empty, uisKnowledge = HM.empty
    , uisInventory = inventory, uisEquipped = equipped
    , uisAccessories = [], uisFactionId = "neutral"
    , uisWounds = [], uisScars = [], uisImmuneResponse = 0
    , uisImmunities = HM.empty, uisBlood = 5, uisName = "" }

-- | A one-page save carrying @units@, built the way a DECODED save is:
--   'blankPageSnapshot' is the construction every @world-pages@
--   version's decoder converges on, and 'snapshotToSaveData' is the
--   adapter staging consumes — so this reaches 'stagePage' along the
--   real route rather than a test-only shortcut.
--
--   An ARENA page (seed 0 with the empty timeline), so staging rebuilds
--   flat chunks instead of generating a world.
saveWith ∷ UnitSnapshot → SaveData
saveWith units = snapshotToSaveData
    (SaveRequestMeta "equipment_reconcile_slot"
                     "2026-09-04T00:00:00.000000Z" False)
    SessionSnapshot
        { snapGameTime       = 0
        , snapTexPalette     = emptyTexPalette
        , snapNextItemId     = 1000
        , snapNextBuildingId = 1
        , snapNextUnitId     = 2
        , snapActivePage     = pageId
        , snapVisiblePages   = [pageId]
        , snapLiveCamera     = LiveCameraSnapshot
            { lcsOwnerPage = Just pageId
            , lcsX = 0, lcsY = 0, lcsZoom = 1, lcsFacing = FaceSouth }
        , snapPages          = HM.singleton pageId
            ((blankPageSnapshot pageId
                  defaultWorldGenParams { wgpSeed = 0 })
                 { pgsUnits = units })
        }

-- | Install the fixture content on the live registries, stage a save
--   carrying @equipped@/@inventory@ on one unit, and hand back the unit
--   staging produced beside everything the logger emitted.
stageWith
    ∷ HasCallStack ⇒ EngineEnv → Maybe Text
    → HM.HashMap Text ItemInstance → [ItemInstance]
    → IO (UnitManager, UnitInstance, [LogEntry])
stageWith env cls equipped inventory = do
    writeIORef (equipmentClassManagerRef env) classMgr
    writeIORef (itemManagerRef env) itemMgr
    atomicModifyIORef' (unitManagerRef env)
        (\um → (um { umDefs = HM.singleton "test_unit"
                                  (unitDefWithClass cls) }, ()))
    (logger, drain) ← capturingLogger
    matReg ← readIORef (materialRegistryRef env)
    let units = UnitSnapshot
            { usnInstances =
                HM.singleton uid1 (minimalUnitSnapshot equipped inventory)
            , usnNextId = 2 }
    staged ← stageSession env logger (saveWith units) matReg ⌦ either
        (\e → expectationFailure (T.unpack (renderStageError e))
                ≫ error "unreachable")
        pure
    entries ← drain
    case HM.lookup uid1 (umInstances (ssUnits staged)) of
        Nothing → expectationFailure "the staged unit is missing"
                    ≫ error "unreachable"
        Just u  → pure (ssUnits staged, u, entries)

-- | Every diagnostic staging emitted about a migrated equipment entry.
equipmentDiagnostics ∷ [LogEntry] → [Text]
equipmentDiagnostics =
    map leMessage
      . filter (\e → "moved to the unit's inventory" `T.isInfixOf` leMessage e)

stagingSpec ∷ SpecWith EngineEnv
stagingSpec = describe "saved equipment slot reconciliation (#2307)" $
  describe "a persisted save, through staging" $ do

    it "publishes the unit with the drifted entries GONE from its \
       \equipment and present in its inventory" $ \env → do
        (_, u, _) ← stageWith env (Just "humanoid") driftedEquipment
                        looseInventory
        HM.keys (uiEquipment u) `shouldBe` ["head"]
        HM.lookup "head" (uiEquipment u)
            `shouldBe` Just (itemOf "steel_helmet" 10)
        map iiInstanceId (uiInventory u) `shouldBe` [20, 21, 11, 12]
        find ((≡ 12) . iiInstanceId) (uiInventory u)
            `shouldBe` Just (itemOf "steel_helmet" 12)

    it "leaves a unit whose every entry is valid exactly as the save \
       \stored it, and says nothing" $ \env → do
        let valid = HM.fromList [ ("head",       itemOf "steel_helmet" 10)
                                , ("right_hand", itemOf "steel_dagger" 13) ]
        (_, u, entries) ← stageWith env (Just "humanoid") valid looseInventory
        uiEquipment u `shouldBe` valid
        uiInventory u `shouldBe` looseInventory
        equipmentDiagnostics entries `shouldBe` []

    it "emits ONE diagnostic per migrated entry, naming the unit, the \
       \slot, the item and the cause — and never fails the load" $
      \env → do
        (_, _, entries) ← stageWith env (Just "humanoid") driftedEquipment []
        equipmentDiagnostics entries `shouldBe`
            [ "Save load: unit #1 on page 'equipment_reconcile_staged': \
              \equipped item 'steel_dagger' (instance 11) in slot \
              \'left_hand' is in a slot the unit's equipment class no \
              \longer declares; moved to the unit's inventory"
            , "Save load: unit #1 on page 'equipment_reconcile_staged': \
              \equipped item 'steel_helmet' (instance 12) in slot \
              \'right_hand' is kind 'headwear' but that slot now accepts \
              \'weapon'; moved to the unit's inventory" ]
        -- A diagnostic, not a failure: nothing about this was logged as
        -- an error, and the staged session exists at all.
        map leMessage (filter ((≡ LevelError) . leLevel) entries)
            `shouldBe` []

    it "moves everything and stays non-blocking when the unit's \
       \definition has lost its equipment class entirely" $ \env → do
        (_, u, entries) ← stageWith env Nothing driftedEquipment []
        uiEquipment u `shouldBe` HM.empty
        map iiInstanceId (uiInventory u) `shouldBe` [10, 11, 12]
        length (equipmentDiagnostics entries) `shouldBe` 3

    -- Requirement 6: the save this staged session would write back
    -- carries no stale key, so the orphan cannot reappear in a later
    -- generation. Taken through 'toUnitSnapshot', the same adapter
    -- 'World.Thread.Command.Save.WriteWorld' writes a save with.
    it "saves back clean: the stale slots are absent from the next \
       \generation, and re-staging that save migrates nothing" $
      \env → do
        (um, _, _) ← stageWith env (Just "humanoid") driftedEquipment
                         looseInventory
        let written = toUnitSnapshot pageId um
        case HM.lookup uid1 (usnInstances written) of
            Nothing → expectationFailure "the unit vanished across the \
                                         \round trip"
            Just s  → do
                HM.keys (uisEquipped s) `shouldBe` ["head"]
                map iiInstanceId (uisInventory s) `shouldBe` [20, 21, 11, 12]
        (_, u2, entries2) ← stageWith env (Just "humanoid")
            (maybe HM.empty uisEquipped (HM.lookup uid1 (usnInstances written)))
            (maybe [] uisInventory (HM.lookup uid1 (usnInstances written)))
        HM.keys (uiEquipment u2) `shouldBe` ["head"]
        equipmentDiagnostics entries2 `shouldBe` []
