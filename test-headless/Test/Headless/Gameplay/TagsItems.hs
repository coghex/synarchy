{-# LANGUAGE OverloadedStrings #-}
-- | "Gameplay.Tags.Items" (#2703, SCN-04 of epic #2698): general
--   gameplay tags on PHYSICAL ITEMS, keyed by 'iiInstanceId' and held
--   by the session owner ('World.State.Types.wmItemTags'), kept through
--   every move and in-place change, removed by every destruction, and
--   carried by every save and load.
--
--   Every transition is a production path: the registered Lua verbs
--   (@unit.commitTransfer@, @unit.transferItemToBuilding@,
--   @unit.dropItemById@, @item.pickupGround@, @equipment.unequip@,
--   @equipment.unequipAccessory@, @unit.modifyItemFillById@,
--   @unit.repairItem@, @unit.feed@, @unit.removeItem@, @craft.execute@,
--   @unit.treatBleeding@, @construction.payMaterials@,
--   @item.removeGround@), the transfer executor's own 'commitCross'
--   with a failing push for the rollback, the canonical nested
--   ownership boundary ("Item.Ownership"), and the real unit, building
--   and world command handlers for destruction. Edits and reads go
--   through "Gameplay.Tags.Live".
--
--   The scene is written straight into the manager refs, the
--   'Test.Headless.Unit.TransferApi' technique: every endpoint stands
--   within one tile of the acolyte, so a transfer completes instead of
--   queueing an order.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Gameplay.Tags.Items"'@.
module Test.Headless.Gameplay.TagsItems (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (sort)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified System.Random as Random

import Building.Command.Types (BuildingCommand(..))
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..), BuildingManager(..)
    , emptyBuildingManager )
import Craft.Types
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log (LoggerState, LogEntry(..), LogConfig(..), LogBackend(..)
                       , initLogger, defaultLogConfig)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API.Units.Transfer (commitCross, popUnit)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Index
import Gameplay.Tags.ItemLifecycle (forgetItemTags, itemTagEpoch, placeOnGround)
import World.Thread.Command.Init (registerPageIncarnationWith)
import Gameplay.Tags.Live
import Gameplay.Tags.Query
import Gameplay.Tags.Registry (registryInvariantViolations)
import Gameplay.Tags.Types
import Scenario.Items.Refs (insertItemRefs, itemRefsFromList, itemRefsToAscList, lookupItemRef)
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Knowledge (emptyPortableKnowledge)
import Item.Ownership
import Item.Types
    ( ItemDef(..), ItemFood(..), ItemInstance(..), ItemManager(..), ItemStorage(..) )
import Structure.Palette (emptyTexPalette)
import Engine.Graphics.Camera (CameraFacing(..))
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Unit.TransferApi
    (evalDebug, mkBuilding, mkItem, mkUnit, minimalDef, newBareLuaBackend, storageDef)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Transfer (TransferEndpoint(..), TransferItemRef(..), TransferReason(..))
import Unit.Types
    ( UnitId(..), UnitInstance(..), UnitManager(..), Wound(..), emptyUnitManager )
import Test.Headless.World.PageIncarnation (drainEntities)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), StructurePiece(..)
    , newConstructDesignation )
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Attempt (ConstructAttemptId(..))
import World.Construct.Revalidate (constructRefundDeps, refundConstructDesignation)
import World.Thread.Command.Cursor
    (handleWorldCancelConstructCommand, handleWorldDesignateConstructCommand)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Tile.Types (WorldTileData(..))
import Structure.Types (emptyChunkStructures)
import Test.Headless.Construct.Fixture
    (fixtureItem, fixtureItems, fixturePackName, payerUnit, registerFixturePacks)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams, withVolcanoCtx)
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (stageSession, renderStageError)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Session
    (CoreSessionDTOv1(..), LiveCameraDTO(..), applyCoreSession, coreSessionCodec)
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Integrity (IntegrityError(..), sessionIntegrityWarnings)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( UnitSnapshot(..), UnitInstanceSnapshot(..), toBuildingSnapshot, toUnitSnapshot )
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command.Basic
    (handleWorldDestroyCommand, handleWorldDestroyAllCommand)
import qualified Data.Serialize as S

-- * Vocabulary

tg ∷ Text → GameplayTag
tg t = fromMaybe (error "an empty fixture tag") (mkGameplayTag t)

tags ∷ [Text] → Set.Set GameplayTag
tags = Set.fromList . map tg

it' ∷ Word64 → ItemTarget
it' i = fromMaybe (error "id 0 in a fixture") (mkItemTarget i)

-- * The scene

pageP, pageQ ∷ WorldPageId
pageP = WorldPageId "item_tags_page"
pageQ = WorldPageId "item_tags_other"

acolyteUid, muleUid, wolfUid, strangerUid ∷ UnitId
acolyteUid  = UnitId 1
muleUid     = UnitId 2
wolfUid     = UnitId 3
strangerUid = UnitId 4

holdBid, siteBid ∷ BuildingId
holdBid = BuildingId 7
siteBid = BuildingId 9

-- | Item ids, by role. The acolyte's satchel is the three-level tree:
--   satchel 10 ⊃ pouch 11 ⊃ { ration 12, ration 13 }.
satchel, pouch, rationA, rationB, rationRoot, barA, barB, grain, kit
    , bandageA, bandageB, dagger, habit, crate, crateBar, siteBar, groundSack
    , groundRation, muleBar, strangerBar ∷ Word64
satchel = 10; pouch = 11; rationA = 12; rationB = 13; rationRoot = 14
barA = 15; barB = 16; grain = 17; kit = 18; bandageA = 19; bandageB = 20
dagger = 21; habit = 22; crate = 30; crateBar = 31; siteBar = 32
groundSack = 40; groundRation = 41; muleBar = 50; strangerBar = 60

-- | Everything the acolyte carries, equips and wears.
acolyteTree ∷ [Word64]
acolyteTree = [ satchel, pouch, rationA, rationB, rationRoot, barA, barB
              , grain, kit, bandageA, bandageB, dagger, habit ]

-- | Every live item in the scene, at every owner and depth.
sceneIds ∷ [Word64]
sceneIds =
    [ satchel, pouch, rationA, rationB, rationRoot, barA, barB, grain, kit
    , bandageA, bandageB, dagger, habit, crate, crateBar, siteBar, groundSack
    , groundRation, muleBar, strangerBar ]

roomy ∷ ItemStorage
roomy = ItemStorage { isWeightCapacity = 50, isBulkCapacity = 50 }

container ∷ Text → Word64 → [ItemInstance] → ItemInstance
container name iid kids = (mkItem name iid 0.5) { iiContents = kids, iiStorage = Just roomy }

itemDef ∷ Text → Text → ItemDef
itemDef name kind = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = kind, idCategory = "Test"
    , idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture" }

sceneItems ∷ ItemManager
sceneItems = ItemManager $ HM.fromList
    [ ("satchel", itemDef "satchel" "container")
    , ("pouch", itemDef "pouch" "container")
    , ("ration", (itemDef "ration" "food") { idFood = Just (ItemFood 400 0) })
    , ("grain_sack", (itemDef "grain_sack" "food") { idFood = Just (ItemFood 0 3000) })
    , ("steel_bar", itemDef "steel_bar" "misc")
    , ("first_aid_kit", itemDef "first_aid_kit" "container")
    , ("bandage", itemDef "bandage" "misc")
    , ("tweezers", itemDef "tweezers" "misc")
    , ("scissors", itemDef "scissors" "misc")
    , ("steel_dagger", itemDef "steel_dagger" "misc")
    , ("acolyte_habit", itemDef "acolyte_habit" "misc")
    , ("crate", itemDef "crate" "container")
    , ("ingot", itemDef "ingot" "misc") ]

-- | Smelt one steel bar into one ingot.
smeltRecipe ∷ RecipeDef
smeltRecipe = RecipeDef
    { rdId = "tag_smelt", rdName = "Tag Smelt", rdStation = "smelt"
    , rdInputs = [RecipeIngredient "steel_bar" 1], rdFuel = Nothing, rdWork = 1
    , rdOutputs = [RecipeIngredient "ingot" 1]
    , rdKnowledge = Nothing, rdSkill = Nothing
    , rdRepairAxis = Nothing, rdOutputTemp = Nothing, rdPowerDraw = 0 }

acolyteItems ∷ [ItemInstance]
acolyteItems =
    [ container "satchel" satchel
        [ container "pouch" pouch
            [ mkItem "ration" rationA 0.5, mkItem "ration" rationB 0.5 ] ]
    , mkItem "ration" rationRoot 0.5
    , mkItem "steel_bar" barA 1.0
    , mkItem "steel_bar" barB 1.0
    , (mkItem "grain_sack" grain 0.5) { iiCurrentFill = 5.0 }
    , (mkItem "first_aid_kit" kit 0.5)
        { iiContents = [ mkItem "bandage" bandageA 0.05, mkItem "bandage" bandageB 0.05
                       , mkItem "tweezers" 23 0.05, mkItem "scissors" 24 0.05 ] }
    ]

bleedingWound ∷ Wound
bleedingWound = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.5
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | Rebuild both pages, every manager and the item index. Every case
--   starts here.
resetScene ∷ EngineEnv → IO ()
resetScene env = do
    writeIORef (treatRNGRef env) (Random.mkStdGen 20260906)
    wsP ← emptyWorldState
    wsQ ← emptyWorldState
    writeIORef (wsGenParamsRef wsP) (Just defaultWorldGenParams)
    writeIORef (wsGenParamsRef wsQ) (Just defaultWorldGenParams)
    atomicModifyIORef' (wsGroundItemsRef wsP) $ \gis →
        let sack = container "crate" groundSack [mkItem "ration" groundRation 0.5]
        in (fst (spawnGroundItem sack 10.5 10.5 gis), ())
    epoch ← wmSessionEpoch ⊚ readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageP, wsP), (pageQ, wsQ)], wmVisible = [pageP]
        , wmSessionEpoch = epoch }
    writeIORef (itemManagerRef env) sceneItems
    writeIORef (recipeManagerRef env) (RecipeManager (HM.singleton "tag_smelt" smeltRecipe))
    let acolyte = (mkUnit "acolyte" FactionPlayer (10, 10) 100 acolyteItems
                          [mkItem "acolyte_habit" habit 1.0])
            { uiPage = pageP
            , uiEquipment = HM.singleton "right_hand" (mkItem "steel_dagger" dagger 1.0)
            , uiStats = HM.fromList [ ("carrying_capacity", 100), ("hunger", 100)
                                    , ("max_hunger", 2000) ]
            , uiKnowledge = HM.fromList [("bleed_control", 100)] }
        mule = (mkUnit "technomule" FactionPlayer (11, 10) 250 [mkItem "steel_bar" muleBar 1.0] [])
            { uiPage = pageP, uiWounds = [bleedingWound] }
        wolf = (mkUnit "wolf" FactionWildlife (11, 11) 100 [] []) { uiPage = pageP }
        stranger = (mkUnit "acolyte" FactionPlayer (10, 10) 100
                           [mkItem "steel_bar" strangerBar 1.0] []) { uiPage = pageQ }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("technomule", minimalDef "technomule" "Technomule")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (acolyteUid, acolyte), (muleUid, mule), (wolfUid, wolf)
            , (strangerUid, stranger) ]
        , umNextId = 5 }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ ("cargo_hold", storageDef "cargo_hold" "Cargo Hold" (1, 1) 0 200)
            , ("site", (storageDef "site" "Build Site" (1, 1) 100 200)
                         { bdMaterials = HM.singleton "steel_bar" 3 }) ]
        , bmInstances = HM.fromList
            [ (holdBid, (mkBuilding "cargo_hold" (11, 9) (1, 1)
                          [container "crate" crate [mkItem "steel_bar" crateBar 1.0]])
                          { biPage = pageP })
            , (siteBid, (mkBuilding "site" (9, 10) (1, 1) [])
                          { biPage = pageP
                          , biMaterialsDelivered =
                              HM.singleton "steel_bar" [mkItem "steel_bar" siteBar 1.0] }) ]
        , bmNextId = 10 }
    writeIORef (nextItemInstanceIdRef env) 100

-- | Tag every scene item with its own @iN@ tag, the three rations with
--   a shared @food@ tag and both acolyte bars with @metal@. @barB@ and
--   the mule's bar stay UNTAGGED, so a negative query has something to
--   find.
tagScene ∷ EngineEnv → IO ()
tagScene env = do
    forM_ sceneIds $ \i → unless (i ∈ [barB, muleBar]) $
        editItem env i (AddTagsEdit (tags ["i" <> tshow i]))
            ⌦ (`shouldBe` TagEditApplied)
    forM_ [rationA, rationB, rationRoot] $ \i →
        editItem env i (AddTagsEdit (tags ["food"])) ⌦ (`shouldBe` TagEditApplied)
    editItem env barA (AddTagsEdit (tags ["metal"])) ⌦ (`shouldBe` TagEditApplied)
    -- #2709: every scene item also carries a scenario reference, so each
    -- move, consumption and destruction below proves the reference
    -- follows (or dies with) its item exactly as the tags do.
    commitItemTreeAssignments (unitManagerRef env) (buildingManagerRef env)
        (worldManagerRef env) [] [ (it' i, sceneRef i) | i ← sceneIds ]
        ⌦ (`shouldBe` Right ())

-- | The scenario reference 'tagScene' gives an item.
sceneRef ∷ Word64 → Text
sceneRef i = "ref-" <> tshow i

-- * The one internal API, over the live refs

editItem ∷ EngineEnv → Word64 → TagEdit → IO TagEditResult
editItem env i = editItemOn env Nothing i

editItemOn ∷ EngineEnv → Maybe WorldPageId → Word64 → TagEdit → IO TagEditResult
editItemOn env scope i e = case mkItemTarget i of
    Nothing → pure TagEditNoTarget
    Just t  → mutateItemTags (unitManagerRef env) (buildingManagerRef env)
                             (worldManagerRef env) scope t e

view ∷ EngineEnv → IO LiveTagView
view env = captureLiveTagView (unitManagerRef env) (buildingManagerRef env)
                              (worldManagerRef env)

-- | An item's live tags; 'Nothing' when no live item has that id.
tagsOf ∷ EngineEnv → Word64 → IO (Maybe (Set.Set GameplayTag))
tagsOf env i = do
    v ← view env
    pure (mkItemTarget i ⌦ itemTagsIn v)

-- | The ids a query over the whole live item universe selects.
queryItems ∷ EngineEnv → TagQuery → IO [Word64]
queryItems env q = do
    v ← view env
    pure [ itemTargetId t
         | TargetItem t ← targetSetsToList (runLiveTagQuery v (selectCategories [ItemCategory]) q) ]

-- | Both index directions agree, every assigned key names a live item,
--   and each named survivor answers exactly its expected tags from BOTH
--   directions: its forward set, and membership in every tag's reverse
--   set.
coherent ∷ EngineEnv → [(Word64, [Text])] → IO ()
coherent env expected = do
    v ← view env
    registryInvariantViolations (liveTagRegistry v) `shouldBe` []
    let ix = wmItemTags (ltvManager v)
    categoryTargets ix `shouldSatisfy` (`Set.isSubsetOf` liveItemTargets v)
    -- #2709: a scenario reference never outlives its item
    let refs = wmItemScenarioRefs (ltvManager v)
    forM_ (itemRefsToAscList refs) $ \(i, r) → do
        Set.member (it' i) (liveItemTargets v) `shouldBe` True
        r `shouldBe` sceneRef i
    forM_ expected $ \(i, ts) → do
        itemTagsIn v (it' i) `shouldBe` Just (tags ts)
        forM_ ts $ \t → indexMembers (tg t) ix `shouldSatisfy` Set.member (it' i)

-- | The tags 'tagScene' gives an item.
sceneTags ∷ Word64 → [Text]
sceneTags i
    | i ∈ [barB, muleBar] = []
    | i ∈ [rationA, rationB, rationRoot] = ["i" <> tshow i, "food"]
    | i ≡ barA = ["i" <> tshow i, "metal"]
    | otherwise = ["i" <> tshow i]

-- | Every scene item except these still carries its scene tags.
survivorsKeep ∷ EngineEnv → [Word64] → IO ()
survivorsKeep env gone = do
    coherent env [ (i, sceneTags i) | i ← sceneIds, i ∉ gone ]
    -- #2709: and every survivor still has ITS reference
    v ← view env
    forM_ [ i | i ← sceneIds, i ∉ gone ] $ \i →
        lookupItemRef i (wmItemScenarioRefs (ltvManager v)) `shouldBe` Just (sceneRef i)

-- | Gone from the live universe AND from both indexes.
goneEverywhere ∷ EngineEnv → [Word64] → IO ()
goneEverywhere env gone = do
    v ← view env
    let ix = wmItemTags (ltvManager v)
    forM_ gone $ \i → do
        itemTagsIn v (it' i) `shouldBe` Nothing
        categoryTagsOf (it' i) ix `shouldBe` Set.empty
        lookupItemRef i (wmItemScenarioRefs (ltvManager v)) `shouldBe` Nothing
        forM_ (sceneTags i) $ \t →
            indexMembers (tg t) ix `shouldSatisfy` (not . Set.member (it' i))

-- * Lua plumbing

q ∷ Text → Text
q t = "\"" <> t <> "\""

lua ∷ LuaBackendState → Text → IO Text
lua ls src = do
    r ← evalDebug ls src
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t)
    pure r

transfer ∷ LuaBackendState → (Text, Int) → (Text, Int) → [(Word64, Text)] → IO Text
transfer ls (sk, si) (dk, di) items = lua ls $ T.concat
    [ "return _G.__fmt(unit.commitTransfer({ source = { kind = '", sk, "', id = ", tshow si
    , " }, destination = { kind = '", dk, "', id = ", tshow di, " }, items = { "
    , T.intercalate ", " [ "{ instanceId = " <> tshow i <> ", defName = '" <> d <> "' }"
                         | (i, d) ← items ]
    , " } }))" ]

-- | The live ground row holding an item, if any.
groundGidOf ∷ EngineEnv → Word64 → IO (Maybe Int)
groundGidOf env i = do
    wm ← readIORef (worldManagerRef env)
    case lookup pageP (wmWorlds wm) of
        Nothing → pure Nothing
        Just ws → do
            gis ← readIORef (wsGroundItemsRef ws)
            pure (listToMaybe [ gid | (gid, gi) ← HM.toList (gisItems gis)
                                    , iiInstanceId (giInst gi) ≡ i ])

unitHolds ∷ EngineEnv → UnitId → IO [Word64]
unitHolds env u = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe [] (map iiInstanceId . uiInventory) (HM.lookup u (umInstances um))

-- * Spec

spec ∷ Spec
spec = describe "Gameplay.Tags.Items" $ aroundAll setup $ do
    universeSpec
    moveSpec
    refusalSpec
    consumptionSpec
    paymentSpec
    destructionSpec
    attachmentSpec
    sessionSpec
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- | Requirement 2.
universeSpec ∷ SpecWith (EngineEnv, LuaBackendState)
universeSpec = describe "the live item universe" $ do

    it "every owner category and nesting depth is taggable, by physical id" $
        \(env, _) → do
            resetScene env
            v ← view env
            Set.map itemTargetId (liveItemTargets v)
                `shouldBe` Set.fromList (sceneIds ⧺ [23, 24])
            tagScene env
            survivorsKeep env []

    it "same-definition siblings are independent, and a query names the \
       \actual physical item" $ \(env, _) → do
        resetScene env
        tagScene env
        sort ⊚ queryItems env (tagged (tg "food"))
            `shouldReturn` [rationA, rationB, rationRoot]
        queryItems env (tagged (tg ("i" <> tshow rationB))) `shouldReturn` [rationB]
        _ ← editItem env rationA (RemoveTagsEdit (tags ["food"]))
        sort ⊚ queryItems env (tagged (tg "food"))
            `shouldReturn` [rationB, rationRoot]
        coherent env [(rationA, ["i12"]), (rationB, ["i13", "food"])]

    it "an untagged item is in the universe, so a negative query finds it" $
        \(env, _) → do
            resetScene env
            tagScene env
            found ← queryItems env (QComplement (tagged (tg "metal")))
            found `shouldSatisfy` (\xs → barB ∈ xs ∧ muleBar ∈ xs ∧ barA ∉ xs)
            unmatched ← queryItems env (QComplement QUniverse)
            unmatched `shouldBe` []

    it "the zero sentinel, a destroyed or never-minted id, and another \
       \page's item under a page scope are refused and create nothing" $
        \(env, _) → do
            resetScene env
            mkItemTarget 0 `shouldBe` Nothing
            editItem env 0 (AddTagsEdit (tags ["x"])) `shouldReturn` TagEditNoTarget
            editItem env 999 (AddTagsEdit (tags ["x"])) `shouldReturn` TagEditNoTarget
            editItemOn env (Just pageP) strangerBar (AddTagsEdit (tags ["x"]))
                `shouldReturn` TagEditNoTarget
            editItemOn env (Just pageQ) strangerBar (AddTagsEdit (tags ["x"]))
                `shouldReturn` TagEditApplied
            v ← view env
            categoryForward (wmItemTags (ltvManager v))
                `shouldBe` Map.singleton (it' strangerBar) (tags ["x"])
            -- The selected live context narrows a query to its page.
            [ itemTargetId t | TargetItem t ← targetSetsToList
                (runLiveTagQuery v (onPage v pageQ . selectCategories [ItemCategory])
                                 QUniverse) ]
                `shouldBe` [strangerBar]

-- | Requirement 3: successful moves and in-place changes keep tags.
moveSpec ∷ SpecWith (EngineEnv, LuaBackendState)
moveSpec = describe "moves and in-place changes keep every tag" $ do

    it "a whole tree moves unit → unit with its root and descendants tagged" $
        \(env, ls) → do
            resetScene env
            tagScene env
            transfer ls ("unit", 1) ("unit", 2) [(satchel, "satchel")]
                `shouldReturn` q "all|10:satchel:completed"
            unitHolds env muleUid ⌦ (`shouldSatisfy` (satchel ∈))
            survivorsKeep env []

    it "unit → building storage and back, and a building's nested crate to \
       \a unit" $ \(env, ls) → do
        resetScene env
        tagScene env
        transfer ls ("unit", 1) ("building", 7) [(rationRoot, "ration")]
            `shouldReturn` q "all|14:ration:completed"
        transfer ls ("building", 7) ("unit", 1) [(crate, "crate")]
            `shouldReturn` q "all|30:crate:completed"
        unitHolds env acolyteUid ⌦ (`shouldSatisfy` (crate ∈))
        survivorsKeep env []

    it "drop to the ground, then pick up, keeps the item's id and tags" $
        \(env, ls) → do
            resetScene env
            tagScene env
            lua ls ("return unit.dropItemById(1, " <> tshow kit <> ")")
                ⌦ (`shouldBe` "true")
            Just gid ← groundGidOf env kit
            survivorsKeep env []
            lua ls ("return item.pickupGround(1, " <> tshow gid <> ")")
                ⌦ (`shouldBe` "true")
            groundGidOf env kit `shouldReturn` Nothing
            unitHolds env acolyteUid ⌦ (`shouldSatisfy` (kit ∈))
            survivorsKeep env []

    it "unequip and accessory removal keep the worn items' tags" $
        \(env, ls) → do
            resetScene env
            tagScene env
            lua ls "return equipment.unequip(1, 'right_hand')" ⌦ (`shouldBe` "true")
            lua ls "return equipment.unequipAccessory(1, 1)" ⌦ (`shouldBe` "true")
            unitHolds env acolyteUid ⌦ (`shouldSatisfy` (\xs → dagger ∈ xs ∧ habit ∈ xs))
            survivorsKeep env []

    it "construction delivery moves the instance into delivered materials \
       \with its tags" $ \(env, ls) → do
        resetScene env
        tagScene env
        lua ls "return unit.transferItemToBuilding(1, 9, 'steel_bar')"
            ⌦ (`shouldBe` "true")
        bm ← readIORef (buildingManagerRef env)
        (map iiInstanceId . concat . HM.elems . biMaterialsDelivered
            ⊚ HM.lookup siteBid (bmInstances bm))
            `shouldSatisfy` maybe False (barA ∈)
        survivorsKeep env []

    it "fill, condition and sharpness changes keep identity and tags; a \
       \container emptied of its last item is not destroyed" $ \(env, ls) → do
        resetScene env
        tagScene env
        lua ls ("return unit.modifyItemFillById(1, " <> tshow grain <> ", -1)")
            ⌦ (`shouldNotBe` "0")
        _ ← lua ls ("return unit.repairItem(1, " <> tshow barA <> ", -5, -5)")
        -- Empty the pouch: both rations move up into the satchel through
        -- the nested ownership boundary, leaving the pouch empty.
        forM_ [rationA, rationB] $ \r → nestedMove env r satchel
        um ← readIORef (unitManagerRef env)
        let pouchNow = [ c | Just u ← [HM.lookup acolyteUid (umInstances um)]
                           , s0 ← uiInventory u, c ← iiContents s0, iiInstanceId c ≡ pouch ]
        map iiContents pouchNow `shouldBe` [[]]
        survivorsKeep env []

    it "a nested move through Item.Ownership keeps the moved instance's and \
       \every bystander's tags" $ \(env, _) → do
        resetScene env
        tagScene env
        nestedMove env rationRoot pouch
        survivorsKeep env []

-- | Detach the acolyte's satchel tree (nothing destroyed, the instance
--   kept in hand), as a transfer does between its pop and its push.
detachSatchel ∷ EngineEnv → IO ItemInstance
detachSatchel env = atomicModifyIORef' (unitManagerRef env) $ \um →
    case HM.lookup acolyteUid (umInstances um) of
        Nothing → error "no acolyte"
        Just u → case break ((≡ satchel) . iiInstanceId) (uiInventory u) of
            (pre, it0 : post) →
                (um { umInstances = HM.insert acolyteUid u { uiInventory = pre ⧺ post }
                                              (umInstances um) }, it0)
            _ → error "no satchel"

reattach ∷ EngineEnv → ItemInstance → IO ()
reattach env it0 = atomicModifyIORef' (unitManagerRef env) $ \um →
    (um { umInstances = HM.adjust (\u → u { uiInventory = it0 : uiInventory u })
                                  acolyteUid (umInstances um) }, ())

-- | Requirement 4: refused and rolled-back operations keep everything.
refusalSpec ∷ SpecWith (EngineEnv, LuaBackendState)
refusalSpec = describe "refused, rolled-back and partial operations keep tags" $ do

    it "an edit while a tagged tree is momentarily detached is refused and \
       \strips nothing the tree already carries" $ \(env, _) → do
        resetScene env
        tagScene env
        held ← detachSatchel env
        editItem env satchel (AddTagsEdit (tags ["late"])) `shouldReturn` TagEditNoTarget
        editItem env pouch ClearTagsEdit `shouldReturn` TagEditNoTarget
        reattach env held
        survivorsKeep env []

    it "an edit whose commit lands while the tree is detached keeps the \
       \tree's earlier tags and the new one" $ \(env, _) → do
        resetScene env
        tagScene env
        heldRef ← newIORef Nothing
        r ← mutateItemTagsWith (onCommit (detachSatchel env ⌦ writeIORef heldRef . Just))
                (unitManagerRef env) (buildingManagerRef env) (worldManagerRef env)
                Nothing (it' satchel) (AddTagsEdit (tags ["late"]))
        r `shouldBe` TagEditApplied
        readIORef heldRef ⌦ maybe (fail "never detached") (reattach env)
        coherent env [(satchel, sceneTags satchel ⧺ ["late"]), (pouch, sceneTags pouch)]

    it "a retry that crosses a session replacement fails instead of \
       \adopting the new session" $ \(env, _) → do
        resetScene env
        -- Attempt 1 is raced by an unrelated destruction (a generation
        -- bump); the session is replaced before attempt 2 starts.
        let seam = ItemWriteSeam
                { iwsBeforeCommit = \n → when (n ≡ 1) $ do
                    e ← itemTagEpoch (worldManagerRef env)
                    forgetItemTags (worldManagerRef env) e (HS.singleton 4242)
                , iwsAtAttempt = \n → when (n ≡ 2) $
                    atomicModifyIORef' (worldManagerRef env) $ \m →
                        (m { wmSessionEpoch = wmSessionEpoch m + 1 }, ()) }
        r ← mutateItemTagsWith seam (unitManagerRef env) (buildingManagerRef env)
                (worldManagerRef env) Nothing (it' satchel) (AddTagsEdit (tags ["late"]))
        r `shouldBe` TagEditNoTarget
        queryItems env (tagged (tg "late")) `shouldReturn` []

    it "an edit racing a destruction between check and commit installs \
       \nothing" $ \(env, _) → do
        resetScene env
        let destroy = Q.writeQueue (unitQueue env) (UnitDestroy acolyteUid) >> drainEntities env
        r ← mutateItemTagsWith (onCommit destroy) (unitManagerRef env) (buildingManagerRef env)
                (worldManagerRef env) Nothing (it' satchel) (AddTagsEdit (tags ["late"]))
        r `shouldBe` TagEditNoTarget
        queryItems env (tagged (tg "late")) `shouldReturn` []
        coherent env []

    it "a refused transfer to a non-commandable unit moves and forgets \
       \nothing" $ \(env, ls) → do
        resetScene env
        tagScene env
        transfer ls ("unit", 1) ("unit", 3) [(satchel, "satchel")]
            `shouldReturn` q "none|10:satchel:failed:receiver_ineligible"
        unitHolds env acolyteUid ⌦ (`shouldSatisfy` (satchel ∈))
        survivorsKeep env []

    it "a partial batch keeps tags on the moved AND the unmoved items" $
        \(env, ls) → do
            resetScene env
            tagScene env
            r ← transfer ls ("unit", 1) ("unit", 2)
                    [(rationRoot, "ration"), (999, "ration")]
            r `shouldBe` q "partial|14:ration:completed 999:ration:failed:instance_missing"
            survivorsKeep env []

    it "a push that fails after the pop rolls the instance back to its \
       \index, a temporary detach that forgets nothing" $ \(env, _) → do
        resetScene env
        tagScene env
        before ← unitHolds env acolyteUid
        let failing _ _ _ = pure (Left ReasonReceiverFull)
        r ← commitCross env (EndpointUnit acolyteUid) (EndpointUnit muleUid)
                (popUnit acolyteUid) failing
                (TransferItemRef (fromIntegral satchel) "satchel")
        either (const True) (const False) r `shouldBe` True
        unitHolds env acolyteUid `shouldReturn` before
        survivorsKeep env []

    it "a failed push whose source was destroyed in between forgets the \
       \discarded tree, root and descendants" $ \(env, _) → do
        resetScene env
        tagScene env
        -- #1274's sequence: popped, then the source unit is destroyed
        -- by the real handler, then the push fails and the restore
        -- answers source-missing.
        let destroySourceThenFail _ _ _ = do
                Q.writeQueue (unitQueue env) (UnitDestroy acolyteUid)
                drainEntities env
                pure (Left ReasonReceiverFull)
        _ ← commitCross env (EndpointUnit acolyteUid) (EndpointUnit muleUid)
                (popUnit acolyteUid) destroySourceThenFail
                (TransferItemRef (fromIntegral satchel) "satchel")
        goneEverywhere env acolyteTree
        survivorsKeep env acolyteTree

    it "a nested insert refused by the ownership boundary changes nothing" $
        \(env, _) → do
            resetScene env
            tagScene env
            um ← readIORef (unitManagerRef env)
            u ← maybe (fail "no acolyte") pure (HM.lookup acolyteUid (umInstances um))
            -- A container into its own descendant: a cycle.
            moveInstance (sceneOf u) satchel pouch `shouldSatisfy` isLeftE
            survivorsKeep env []

-- | Requirement 5: consumption removes exactly the consumed items' tags.
consumptionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
consumptionSpec = describe "consumption forgets exactly what it destroyed" $ do

    it "a discrete meal destroys one ration; its same-definition siblings \
       \and their shared tag survive" $ \(env, ls) → do
        resetScene env
        tagScene env
        lua ls "return unit.feed(1, 'ration')" ⌦ (`shouldNotBe` "nil")
        goneEverywhere env [rationRoot]
        survivorsKeep env [rationRoot]
        sort ⊚ queryItems env (tagged (tg "food")) `shouldReturn` [rationA, rationB]

    it "a bulk sack keeps its tags while eaten from, and loses them once \
       \eaten dry" $ \(env, ls) → do
        resetScene env
        tagScene env
        _ ← lua ls "return unit.feed(1, 'grain_sack')"
        tagsOf env grain `shouldReturn` Just (tags (sceneTags grain))
        -- Drain the rest of the sack, then eat it dry.
        atomicModifyIORef' (unitManagerRef env) $ \um →
            (um { umInstances = HM.adjust
                    (\u → u { uiInventory = map (\i → if iiInstanceId i ≡ grain
                                                         then i { iiCurrentFill = 0.01 }
                                                         else i) (uiInventory u)
                            , uiStats = HM.insert "hunger" 0 (uiStats u) })
                    acolyteUid (umInstances um) }, ())
        lua ls "return unit.feed(1, 'grain_sack')" ⌦ (`shouldNotBe` "nil")
        goneEverywhere env [grain]
        survivorsKeep env [grain]

    it "unit.removeItem and a craft's input destroy one bar each; the \
       \crafted output is new and untagged" $ \(env, ls) → do
        resetScene env
        tagScene env
        lua ls "return unit.removeItem(1, 'steel_bar')" ⌦ (`shouldBe` "true")
        goneEverywhere env [barA]
        r ← lua ls "local ok, ids = craft.execute(1, 'tag_smelt'); return tostring(ok) .. '|' .. tostring(ids and ids[1])"
        r `shouldSatisfy` ("\"true|" `T.isPrefixOf`)
        let outId = read (T.unpack (T.dropEnd 1 (T.drop 6 r))) ∷ Word64
        goneEverywhere env [barB]
        tagsOf env outId `shouldReturn` Just Set.empty
        survivorsKeep env [barA, barB]

    it "a bandage drawn from a kit loses its tags; the kit and the other \
       \supplies keep theirs" $ \(env, ls) → do
        resetScene env
        tagScene env
        r ← lua ls "local r = unit.treatBleeding(1, 2); return tostring(r and r.ok)"
        r `shouldBe` q "true"
        um ← readIORef (unitManagerRef env)
        let kitNow = [ i | Just u ← [HM.lookup acolyteUid (umInstances um)]
                         , i ← uiInventory u, iiInstanceId i ≡ kit ]
            left = [ iiInstanceId c | k ← kitNow, c ← iiContents k, iiDefName c ≡ "bandage" ]
            spent = [ b | b ← [bandageA, bandageB], b ∉ left ]
        length kitNow `shouldBe` 1
        spent `shouldSatisfy` (not . null)
        goneEverywhere env spent
        survivorsKeep env spent


-- | Requirement 5: destruction of the item or its owner.
destructionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
destructionSpec = describe "destruction forgets the destroyed trees" $ do

    it "item.removeGround destroys the whole ground tree; a pickup never \
       \forgets" $ \(env, ls) → do
        resetScene env
        tagScene env
        Just gid ← groundGidOf env groundSack
        lua ls ("return item.removeGround(" <> tshow gid <> ")") ⌦ (`shouldBe` "true")
        goneEverywhere env [groundSack, groundRation]
        survivorsKeep env [groundSack, groundRation]

    it "a destroyed unit takes its carried, equipped and worn trees; other \
       \units keep theirs" $ \(env, _) → do
        resetScene env
        tagScene env
        Q.writeQueue (unitQueue env) (UnitDestroy acolyteUid)
        drainEntities env
        goneEverywhere env acolyteTree
        survivorsKeep env acolyteTree

    it "a demolished building takes its storage and delivered materials" $
        \(env, _) → do
            resetScene env
            tagScene env
            Q.writeQueue (buildingQueue env) (BuildingDestroy holdBid)
            Q.writeQueue (buildingQueue env) (BuildingDestroy siteBid)
            drainEntities env
            goneEverywhere env [crate, crateBar, siteBar]
            survivorsKeep env [crate, crateBar, siteBar]

    it "a destroyed page takes its ground, units and buildings; the other \
       \page's items keep their tags" $ \(env, _) → do
        resetScene env
        tagScene env
        logger ← readIORef (loggerRef env)
        handleWorldDestroyCommand env logger pageP
        drainEntities env
        let onP = filter (≢ strangerBar) sceneIds
        goneEverywhere env onP
        coherent env [(strangerBar, sceneTags strangerBar)]

    it "a page replacement forgets the old ground only once it is \
       \unreachable, so an edit validated against it cannot outlive it" $
        \(env, _) → do
            resetScene env
            tagScene env
            fresh ← emptyWorldState
            let replacePage m = m { wmWorlds = [ if p ≡ pageP then (p, fresh) else (p, w)
                                               | (p, w) ← wmWorlds m ] }
                -- Runs inside the replacement, just before the new page is
                -- registered: the old ground is still what the page shows.
                lateEdit = editItem env groundRation (AddTagsEdit (tags ["late"]))
                               ⌦ (`shouldBe` TagEditApplied)
            registerPageIncarnationWith lateEdit env pageP replacePage
            goneEverywhere env [groundSack, groundRation]
            queryItems env (tagged (tg "late")) `shouldReturn` []
            coherent env []

    it "a drop onto a page torn down after the pop forgets the dropped \
       \tree; onto a live page it keeps every tag" $ \(env, _) → do
        resetScene env
        tagScene env
        wm0 ← readIORef (worldManagerRef env)
        oldWs ← maybe (fail "no page") pure (lookup pageP (wmWorlds wm0))
        epoch ← itemTagEpoch (worldManagerRef env)
        held ← detachSatchel env
        logger ← readIORef (loggerRef env)
        handleWorldDestroyCommand env logger pageP
        drainEntities env
        _ ← placeOnGround (worldManagerRef env) epoch oldWs held 10 10
        goneEverywhere env [satchel, pouch, rationA, rationB]
        coherent env []
        -- Control: the same placement on a live page forgets nothing.
        resetScene env
        tagScene env
        wm1 ← readIORef (worldManagerRef env)
        liveWs ← maybe (fail "no page") pure (lookup pageP (wmWorlds wm1))
        epoch1 ← itemTagEpoch (worldManagerRef env)
        held1 ← detachSatchel env
        _ ← placeOnGround (worldManagerRef env) epoch1 liveWs held1 10 10
        survivorsKeep env []

    it "Exit to Menu clears every item tag with the session" $ \(env, _) → do
        resetScene env
        tagScene env
        logger ← readIORef (loggerRef env)
        handleWorldDestroyAllCommand env logger
        drainEntities env
        wm ← readIORef (worldManagerRef env)
        nullCategoryIndex (wmItemTags wm) `shouldBe` True

    it "a forget computed in a replaced session never reaches the new one" $
        \(env, _) → do
            resetScene env
            tagScene env
            epoch ← wmSessionEpoch ⊚ readIORef (worldManagerRef env)
            atomicModifyIORef' (worldManagerRef env) $ \m →
                (m { wmSessionEpoch = wmSessionEpoch m + 1 }, ())
            forgetItemTags (worldManagerRef env) epoch (HS.fromList sceneIds)
            survivorsKeep env []

-- | Requirement 6: new trees inherit nothing; the attachment boundary.
attachmentSpec ∷ SpecWith (EngineEnv, LuaBackendState)
attachmentSpec = describe "the item-tree attachment boundary" $ do

    it "installs nothing unless every target is committed and live" $
        \(env, _) → do
            resetScene env
            r ← commitItemTreeTags (unitManagerRef env) (buildingManagerRef env)
                    (worldManagerRef env)
                    [(it' satchel, tags ["looted"]), (it' 777, tags ["looted"])]
            r `shouldBe` Left [it' 777]
            queryItems env (tagged (tg "looted")) `shouldReturn` []
            r2 ← commitItemTreeTags (unitManagerRef env) (buildingManagerRef env)
                    (worldManagerRef env)
                    [(it' satchel, tags ["looted"]), (it' pouch, tags ["looted"])]
            r2 `shouldBe` Right ()
            sort ⊚ queryItems env (tagged (tg "looted")) `shouldReturn` [satchel, pouch]
            coherent env [(satchel, ["looted"]), (pouch, ["looted"])]

    it "a tree destroyed between the check and the install leaves no \
       \assignment, and the attach fails" $ \(env, _) → do
        resetScene env
        let destroy = Q.writeQueue (unitQueue env) (UnitDestroy acolyteUid) >> drainEntities env
        r ← commitItemTreeTagsWith (onCommit destroy) (unitManagerRef env) (buildingManagerRef env)
                (worldManagerRef env) [(it' satchel, tags ["looted"]), (it' pouch, tags ["looted"])]
        r `shouldSatisfy` isLeftE
        queryItems env (tagged (tg "looted")) `shouldReturn` []
        coherent env []

    it "a session replacement between the check and the install installs \
       \nothing and reports every target" $ \(env, _) → do
        resetScene env
        let replace = atomicModifyIORef' (worldManagerRef env) $ \m →
                (m { wmSessionEpoch = wmSessionEpoch m + 1 }, ())
        r ← commitItemTreeTagsWith (onCommit replace) (unitManagerRef env) (buildingManagerRef env)
                (worldManagerRef env) [(it' satchel, tags ["looted"]), (it' pouch, tags ["looted"])]
        r `shouldBe` Left [it' satchel, it' pouch]
        queryItems env (tagged (tg "looted")) `shouldReturn` []

    it "a newly added item carries no tags of its own" $ \(env, ls) → do
        resetScene env
        tagScene env
        r ← lua ls "return unit.addItem(1, 'ration', 0)"
        r `shouldBe` "true"
        newest ← maximum ⊚ unitHolds env acolyteUid
        tagsOf env newest `shouldReturn` Just Set.empty

-- | Requirements 7 and 8.
sessionSpec ∷ SpecWith (EngineEnv, LuaBackendState)
sessionSpec = describe "whole-session save and load" $ do

    it "captures every owner's and depth's item tags, and round-trips them \
       \through the real codec exactly" $ \(env, _) → do
        resetScene env
        tagScene env
        snap ← capturedSession env
        wm ← readIORef (worldManagerRef env)
        snapItemTags snap `shouldBe` categoryForward (wmItemTags wm)
        Map.size (snapItemTags snap) `shouldBe` length sceneIds - 2
        back ← throughCodec snap
        snapItemTags back `shouldBe` snapItemTags snap
        -- #2709: the scenario references ride the same capture and codec
        snapItemRefs snap `shouldBe` wmItemScenarioRefs wm
        length (itemRefsToAscList (snapItemRefs snap)) `shouldBe` length sceneIds
        snapItemRefs back `shouldBe` snapItemRefs snap

    it "a load replaces the outgoing item tags and rebuilds both indexes" $
        \(env, _) → do
            resetScene env
            tagScene env
            snap ← capturedSession env ⌦ throughCodec
            _ ← editItem env barB (AddTagsEdit (tags ["outgoing"]))
            (logger, _) ← capturingLogger
            loadSession env logger snap
            queryItems env (tagged (tg "outgoing")) `shouldReturn` []
            survivorsKeep env []

    it "a load installs the saved scenario references and replaces the \
       \outgoing ones" $ \(env, _) → do
        resetScene env
        tagScene env
        snap ← capturedSession env ⌦ throughCodec
        -- an outgoing reference on an item the restored session also holds
        atomicModifyIORef' (worldManagerRef env) $ \m →
            (m { wmItemScenarioRefs = itemRefsFromList [(barB, "outgoing")] }, ())
        (logger, _) ← capturingLogger
        loadSession env logger snap
        wm ← readIORef (worldManagerRef env)
        itemRefsToAscList (wmItemScenarioRefs wm)
            `shouldBe` [ (i, sceneRef i) | i ← sort sceneIds ]
        survivorsKeep env []

    it "a dangling scenario reference is a diagnostic and is dropped at \
       \restore" $ \(env, _) → do
        resetScene env
        tagScene env
        snap0 ← capturedSession env
        let snap = snap0 { snapItemRefs = insertItemRefs [(4242, "ghost-ref")]
                                                         (snapItemRefs snap0) }
        [ ieRefValue e | e ← sessionIntegrityWarnings snap, ieCode e ≡ "dangling-reference"
                       , "core-session.item-refs" `T.isPrefixOf` iePath e ]
            `shouldBe` ["4242"]
        (logger, entries) ← capturingLogger
        loadSession env logger snap
        msgs ← map leMessage ⊚ entries
        msgs `shouldSatisfy` any ("dropped scenario references on 1 item id" `T.isInfixOf`)
        wm ← readIORef (worldManagerRef env)
        lookupItemRef 4242 (wmItemScenarioRefs wm) `shouldBe` Nothing
        survivorsKeep env []

    it "a dangling scenario reference at the saved allocator cursor never \
       \reaches the item a staging refund mints with that id" $ \(env, _) → do
        resetScene env
        tagScene env
        snap0 ← capturedSession env
        let cursor = snapNextItemId snap0
            ghost = (newConstructDesignation 0
                        (CtStructure (StructurePiece "no_such_pack" "floor" Nothing))
                        (ConstructAttemptId 7))
                        { cdPayment = CpPaid (mkMaterialReceipt [("steel_bar", 1)]) }
            snap = snap0
                { snapItemRefs = insertItemRefs [(cursor, "ghost-ref")] (snapItemRefs snap0)
                , snapPages = HM.adjust
                    (\page → page { pgsConstructDesignations = HM.singleton (5, 5) ghost
                                  , pgsConstructNextAttempt = ConstructAttemptId 8 })
                    pageP (snapPages snap0) }
        (logger, _) ← capturingLogger
        loadSession env logger snap
        Just _ ← groundGidOf env cursor
        wm ← readIORef (worldManagerRef env)
        lookupItemRef cursor (wmItemScenarioRefs wm) `shouldBe` Nothing
        survivorsKeep env []

    it "a dangling item assignment is a diagnostic and is dropped at \
       \restore" $ \(env, _) → do
        resetScene env
        tagScene env
        snap0 ← capturedSession env
        let snap = snap0 { snapItemTags = Map.insert (it' 4242) (tags ["ghost"])
                                                     (snapItemTags snap0) }
        [ ieRefValue e | e ← sessionIntegrityWarnings snap, ieCode e ≡ "dangling-reference"
                       , "core-session" `T.isPrefixOf` iePath e ]
            `shouldBe` ["4242"]
        (logger, entries) ← capturingLogger
        loadSession env logger snap
        queryItems env (tagged (tg "ghost")) `shouldReturn` []
        msgs ← map leMessage ⊚ entries
        msgs `shouldSatisfy` any ("dropped gameplay tags on 1 item id" `T.isInfixOf`)
        survivorsKeep env []

    it "a dangling assignment at the saved allocator cursor never reaches \
       \the item a staging refund mints with that id" $ \(env, _) → do
        resetScene env
        tagScene env
        snap0 ← capturedSession env
        let cursor = snapNextItemId snap0
            ghost = (newConstructDesignation 0
                        (CtStructure (StructurePiece "no_such_pack" "floor" Nothing))
                        (ConstructAttemptId 7))
                        { cdPayment = CpPaid (mkMaterialReceipt [("steel_bar", 1)]) }
            snap = snap0
                { snapItemTags = Map.insert (it' cursor) (tags ["ghost"]) (snapItemTags snap0)
                , snapPages = HM.adjust
                    (\page → page { pgsConstructDesignations = HM.singleton (5, 5) ghost
                                  , pgsConstructNextAttempt = ConstructAttemptId 8 })
                    pageP (snapPages snap0) }
        (logger, entries) ← capturingLogger
        loadSession env logger snap
        -- The refund really took the cursor id …
        Just _ ← groundGidOf env cursor
        -- … and carries no tag of the dead item that id once named.
        tagsOf env cursor `shouldReturn` Just Set.empty
        queryItems env (tagged (tg "ghost")) `shouldReturn` []
        msgs ← map leMessage ⊚ entries
        msgs `shouldSatisfy` any ("dropped gameplay tags on 1 item id" `T.isInfixOf`)
        survivorsKeep env []

    it "a pre-publication staging failure keeps the outgoing item tags" $
        \(env, _) → do
            resetScene env
            tagScene env
            snap ← capturedSession env
            let broken = snap { snapPages = HM.adjust
                    (\page → page { pgsUnits = (pgsUnits page)
                        { usnInstances = HM.map (\u → u { uisDefName = "no_such_unit" })
                                                (usnInstances (pgsUnits page)) } })
                    pageP (snapPages snap) }
            (logger, _) ← capturingLogger
            matReg ← readIORef (materialRegistryRef env)
            r ← stageSession env logger
                    (snapshotToSaveData (SaveRequestMeta "tags" "t" False) broken) matReg
            either (const True) (const False) r `shouldBe` True
            survivorsKeep env []

    it "a core-session v1 payload decodes with no item tags" $ \_ → do
        let v1 = CoreSessionDTOv1
                { cs1GameTime = 0, cs1NextItemId = 5, cs1NextBuildingId = 1
                , cs1NextUnitId = 1, cs1ActivePage = pageP, cs1VisiblePages = [pageP]
                , cs1LiveCamera = LiveCameraDTO (Just pageP) 0 0 1 FaceSouth }
        case ccDecode coreSessionCodec 1 (S.encode v1) of
            Left e → expectationFailure (show e)
            Right dto → snapItemTags (applyCoreSession dto skeleton) `shouldBe` Map.empty
  where
    skeleton = SessionSnapshot
        { snapGameTime = 1, snapTexPalette = emptyTexPalette, snapNextItemId = 1
        , snapNextBuildingId = 1, snapNextUnitId = 1, snapActivePage = pageQ
        , snapVisiblePages = [], snapLiveCamera = LiveCameraSnapshot Nothing 0 0 1 FaceSouth
        , snapPages = HM.empty
        , snapItemTags = Map.singleton (it' 1) (tags ["stale"])
        , snapItemRefs = mempty
        , snapPortableKnowledge = emptyPortableKnowledge }

-- * Session helpers

-- | The session as the save barrier captures it: both pages' entities
--   and ground, and the item index read off the same manager.
capturedSession ∷ EngineEnv → IO SessionSnapshot
capturedSession env = do
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    wm ← readIORef (worldManagerRef env)
    pages ← forM [pageP, pageQ] $ \pid → do
        gis ← maybe (pure (GroundItems 0 HM.empty)) (readIORef . wsGroundItemsRef)
                    (lookup pid (wmWorlds wm))
        pure (blankPageSnapshot pid (withVolcanoCtx arenaParams))
            { pgsUnits = toUnitSnapshot pid um
            , pgsBuildings = toBuildingSnapshot pid bm
            , pgsGroundItems = gis
            , pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid)
            , pgsDateYear = 1, pgsDateMonth = 1, pgsDateDay = 1 }
    nextItem ← readIORef (nextItemInstanceIdRef env)
    either (\e → fail ("capture refused: " <> show e)) pure $
        captureSessionSnapshot SessionGlobals
            { sgGameTime = 0, sgTexPalette = emptyTexPalette
            , sgNextItemId = nextItem, sgNextBuildingId = bmNextId bm
            , sgNextUnitId = umNextId um, sgActivePage = pageP
            , sgVisiblePages = [pageP]
            , sgLiveCamera = LiveCameraSnapshot (Just pageP) 0 0 1 FaceSouth
            , sgItemTags = categoryForward (wmItemTags wm)
            , sgItemRefs = wmItemScenarioRefs wm
            , sgPortableKnowledge = emptyPortableKnowledge }
            pages

arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 8 }

throughCodec ∷ SessionSnapshot → IO SessionSnapshot
throughCodec snap = do
    let meta  = snapshotSaveMetadata (SaveRequestMeta "tags" "t" False) snap
        bytes = encodeSessionSnapshot meta snap []
    case decodeSessionEnvelope HS.empty HS.empty bytes of
        Left err → fail ("decode: " <> T.unpack err)
        Right (_, back, _, _) → pure back

capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

loadSession ∷ EngineEnv → LoggerState → SessionSnapshot → IO ()
loadSession env logger snap = do
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger
        (snapshotToSaveData (SaveRequestMeta "tags" "t" False) snap) matReg
        ⌦ either (\e → fail ("stage: " <> T.unpack (renderStageError e))) pure
    publishStagedSession env logger 424242 staged

-- * Other helpers

sceneOf ∷ UnitInstance → OwnershipScene
sceneOf u = OwnershipScene
    { oscItems = uiInventory u
    , oscOther = HM.elems (uiEquipment u) ⧺ uiAccessories u
    , oscCapacity = RootUnlimited
    , oscWeigh = iiWeight }

-- | One nested move through "Item.Ownership", applied to the
--   acolyte's inventory the way a production caller applies a verdict.
nestedMove ∷ EngineEnv → Word64 → Word64 → IO ()
nestedMove env moved target = atomicModifyIORef' (unitManagerRef env) $ \um →
    case HM.lookup acolyteUid (umInstances um) of
        Nothing → error "no acolyte"
        Just u → case moveInstance (sceneOf u) moved target of
            Left r → error ("refused: " <> show r)
            Right m → (um { umInstances = HM.insert acolyteUid
                               u { uiInventory = omItems m } (umInstances um) }, ())

-- | A seam that runs @act@ once, in the first attempt's commit window.
onCommit ∷ IO () → ItemWriteSeam
onCommit act = noItemWriteSeam { iwsBeforeCommit = \n → when (n ≡ 1) act }

isLeftE ∷ Either a b → Bool
isLeftE = either (const True) (const False)

-- * Construction payment (owner decision, 2026-10-17)

constructPage ∷ WorldPageId
constructPage = WorldPageId "item_tags_construct"

-- | Requirement 3's "refund", as the owner settled it for #2703:
--   designation payment CONSUMES the paid items (removing their tags),
--   a refund mints NEW untagged items, and ordinary delivery (covered
--   above) keeps tags.
paymentSpec ∷ SpecWith (EngineEnv, LuaBackendState)
paymentSpec = describe "construction payment and refund (owner decision)" $ do

    it "payment consumes the paid instance and removes its tags; the \
       \unpaid sibling keeps its own" $ \(env, ls) → do
        ws ← resetConstructScene env
        payDesignation env ls ws `shouldReturn` "true"
        goneEverywhere env [701]
        coherent env [(702, ["wiring_tag"])]

    it "a refused payment keeps every instance and tag" $ \(env, ls) → do
        ws ← resetConstructScene env
        logger ← readIORef (loggerRef env)
        aid ← attemptOf ws
        handleWorldCancelConstructCommand env logger constructPage 5 5 (Just aid)
        payAttempt ls aid `shouldReturn` "false"
        coherent env [(701, ["plate_tag"]), (702, ["wiring_tag"])]

    it "a refund mints a NEW item that carries no tags" $ \(env, ls) → do
        ws ← resetConstructScene env
        payDesignation env ls ws `shouldReturn` "true"
        [cd] ← HM.elems ⊚ readIORef (wsConstructDesignationsRef ws)
        deps ← constructRefundDeps env
        refundConstructDesignation deps ws (5, 5) cd
        gis ← readIORef (wsGroundItemsRef ws)
        let refunded = [ iiInstanceId (giInst gi) | gi ← HM.elems (gisItems gis) ]
        length refunded `shouldBe` 1
        refunded `shouldSatisfy` all (∉ [701, 702])
        forM_ refunded $ \i → tagsOf env i `shouldReturn` Just Set.empty
        queryItems env (tagged (tg "plate_tag")) `shouldReturn` []

-- | One flat chunk, the fixture packs, a payer holding a tagged plate
--   (the floor's cost) and a tagged wiring, and one floor designation.
resetConstructScene ∷ EngineEnv → IO WorldState
resetConstructScene env = do
    registerFixturePacks env
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.singleton (UnitId 1)
            (payerUnit constructPage [fixtureItem "steel_plate" 701, fixtureItem "wiring" 702]) }
    writeIORef (buildingManagerRef env) emptyBuildingManager
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just defaultWorldGenParams { wgpWorldSize = 64 })
    writeIORef (wsTilesRef ws) flatTiles
    epoch ← wmSessionEpoch ⊚ readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(constructPage, ws)], wmVisible = [constructPage]
        , wmSessionEpoch = epoch }
    writeIORef (nextItemInstanceIdRef env) 800
    logger ← readIORef (loggerRef env)
    handleWorldDesignateConstructCommand env logger constructPage 5 5 5 5
        (CtStructure (StructurePiece fixturePackName "floor" Nothing)) Nothing
    editItem env 701 (AddTagsEdit (tags ["plate_tag"])) ⌦ (`shouldBe` TagEditApplied)
    editItem env 702 (AddTagsEdit (tags ["wiring_tag"])) ⌦ (`shouldBe` TagEditApplied)
    pure ws

payDesignation ∷ EngineEnv → LuaBackendState → WorldState → IO Text
payDesignation _ ls ws = attemptOf ws ⌦ payAttempt ls

attemptOf ∷ WorldState → IO ConstructAttemptId
attemptOf ws = do
    ds ← HM.elems ⊚ readIORef (wsConstructDesignationsRef ws)
    case ds of
        [cd] → pure (cdAttempt cd)
        _    → fail "expected exactly one designation"

payAttempt ∷ LuaBackendState → ConstructAttemptId → IO Text
payAttempt ls (ConstructAttemptId n) =
    T.replace "\"" "" ⊚ lua ls (T.concat
        [ "return tostring(construction.payMaterials('item_tags_construct', 5, 5, "
        , tshow n, ", 1))" ])

flatTiles ∷ WorldTileData
flatTiles =
    let coord = ChunkCoord 0 0
        area  = chunkSize * chunkSize
        col   = ColumnTiles
                  { ctStartZ = 0, ctMats = VU.replicate 20 1
                  , ctSlopes = VU.replicate 20 0, ctVeg = VU.replicate 20 0 }
        lc = LoadedChunk
               { lcCoord = coord, lcTiles = V.replicate area col
               , lcSurfaceMap = VU.replicate area 10
               , lcTerrainSurfaceMap = VU.replicate area 10
               , lcFluidMap = V.replicate area Nothing
               , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
               , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
               , lcMagma = Nothing, lcStructures = emptyChunkStructures }
    in WorldTileData { wtdChunks = HM.singleton coord lc, wtdMaxChunks = 200 }
