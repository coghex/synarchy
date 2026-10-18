{-# LANGUAGE OverloadedStrings #-}
-- | The "Scenario.Items" gate (#2709, epic #2698 SCN-09): validated
--   scenario item trees materialized into real runtime trees through the
--   production mint boundary ("Item.Materialize"), shaped for each owner
--   category, committed into the live tag and reference owners, captured
--   back, and round-tripped through the real YAML decoder.
--
--   Nothing here fakes a layer. Construction goes through the real
--   'materializeSpec', owners are the real 'UnitInstance' /
--   'BuildingInstance' / 'GroundItems', live attachment goes through
--   "Gameplay.Tags.Live"'s boundary over real manager refs, YAML goes
--   through the real 'Scenario.Schema' decoder, and capacity refusals
--   come from the real "Item.Ownership" policy.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Items"'@.
module Test.Headless.Scenario.Items (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.List (sort, sortOn, nub)
import System.Random (mkStdGen, randomR)
import qualified Data.Serialize as S

import Building.Types (BuildingInstance(..), BuildingManager(..), BuildingId(..), emptyBuildingManager)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Log (LoggerState, LogConfig(..), LogBackend(..), initLogger, defaultLogConfig)
import Engine.Graphics.Camera (CameraFacing(..))
import Gameplay.Tags.Index (CategoryIndex, nullCategoryIndex)
import Gameplay.Tags.ItemLifecycle (forgetItemTags, itemTagEpoch)
import Item.Knowledge (emptyPortableKnowledge)
import Structure.Palette (emptyTexPalette)
import World.Save.Component.Session
    ( CoreSessionDTO(..), CoreSessionDTOv1(..), CoreSessionDTOv2(..), LiveCameraDTO(..)
    , applyCoreSession, coreSessionCodec )
import World.Save.Component.Types (ComponentCodec(..), ComponentError(..))
import World.Save.Snapshot (SessionSnapshot(..), LiveCameraSnapshot(..))
import Gameplay.Tags.Live
import Gameplay.Tags.Types
import qualified Item.Ground as G
import Item.Ground (GroundItems(..), emptyGroundItems)
import Item.Materialize
import Item.Ownership
import Item.Types
import Scenario.Items.Build
import Scenario.Items.Capture
import Scenario.Items.Encode
import Scenario.Items.Owners
import Scenario.Items.Refs
import Scenario.Schema (decodeScenarioBytes)
import Scenario.Types
import Test.Headless.Unit.TransferApi (mkBuilding, mkUnit)
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)

-- * Definitions

bareDef ∷ Text → ItemDef
bareDef name = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = "misc", idCategory = "Misc"
    , idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture" }

entry ∷ Text → Int → ItemContentEntry
entry name n = ItemContentEntry
    { iceItem = name, iceCount = n, iceFill = Nothing, iceContents = Nothing }

fluid ∷ Float → Float → ItemContainer
fluid cap dflt = ItemContainer
    { icCapacity = cap, icHolds = "water", icFillWeight = 1.0, icDefaultFill = dflt }

-- | The fixture registry. @kit@ is a shipped-style kit (default contents,
--   a storage cap); @crate@ is a plain storage item; @bare_kit@ holds
--   items but states no storage (the legacy shape D-57 keeps
--   representable); @gem@ rolls quality and empty weight; the @cyc_@
--   and @bad_kit@ definitions are the three failure shapes of default
--   expansion.
defsWith ∷ (HM.HashMap Text ItemDef → HM.HashMap Text ItemDef) → ItemManager
defsWith tweak = ItemManager $ tweak $ HM.fromList
    [ ("canteen", (bareDef "canteen") { idWeight = 0.4, idBulk = 1.2
                                       , idContainer = Just (fluid 1.0 0) })
    , ("flask", (bareDef "flask") { idWeight = 0.3, idBulk = 0.8
                                   , idContainer = Just (fluid 1.0 0.8) })
    , ("knife", (bareDef "knife") { idWeight = 0.3, idBulk = 0.2
                                   , idQualitySpec = Just (40, 90) })
    , ("bandage", (bareDef "bandage") { idWeight = 0.05, idBulk = 0.1 })
    , ("ring", (bareDef "ring") { idWeight = 0.01, idBulk = 0.01 })
    , ("steel_plate", (bareDef "steel_plate") { idWeight = 2.0, idBulk = 1.5 })
    , ("gem", (bareDef "gem") { idWeight = 0.4, idWeightSpec = Just (0.4, 0.2)
                               , idQualitySpec = Just (20, 80) })
    , ("kit", (bareDef "kit") { idWeight = 0.6, idBulk = 2.0
                               , idStorage = Just (ItemStorage 5.0 6.0)
                               , idDefaultContents = [entry "bandage" 2, entry "pouch" 1] })
    , ("pouch", (bareDef "pouch") { idWeight = 0.2, idBulk = 0.5
                                   , idStorage = Just (ItemStorage 2.0 2.0)
                                   , idDefaultContents = [entry "bandage" 1] })
    , ("crate", (bareDef "crate") { idWeight = 3.0, idBulk = 10
                                   , idStorage = Just (ItemStorage 0.5 0.3) })
    , ("bare_kit", (bareDef "bare_kit") { idWeight = 0.5, idBulk = 2.0
                                         , idDefaultContents = [entry "bandage" 1] })
    , ("cyc_self", (bareDef "cyc_self") { idDefaultContents = [entry "cyc_self" 1] })
    , ("cyc_a", (bareDef "cyc_a") { idDefaultContents = [entry "cyc_b" 1] })
    , ("cyc_b", (bareDef "cyc_b") { idDefaultContents = [entry "cyc_a" 1] })
    , ("bad_kit", (bareDef "bad_kit") { idDefaultContents = [entry "bandage" 1, entry "no_such_item" 1] })
    ]

defs ∷ ItemManager
defs = defsWith id

catalog ∷ ScenarioCatalog
catalog = emptyScenarioCatalog
    { catUnits = HM.fromList
        [ ("acolyte", UnitCatalogEntry
            { ucStats = HM.empty, ucSkills = HS.empty, ucBodyParts = HS.empty
            , ucEquipmentSlots = HS.fromList ["main_hand", "back"] }) ]
    , catItems = itemCatalogOf defs
    , catBuildings = HM.fromList
        [ ("cargo_hold", BuildingCatalogEntry (Footprint 0 0 1 1) 10
                            (HS.fromList ["steel_plate"]) Nothing) ]
    }

-- * Harness

-- | An allocator starting after @start@, and a probe of how many ids it
--   has issued.
newEnv ∷ ItemManager → Word64 → IO (ItemBuildEnv, IO Word64)
newEnv mgr start = do
    ref ← newIORef start
    pure ( ItemBuildEnv mgr (atomicModifyIORef' ref (\n → (n + 1, n + 1)))
         , readIORef ref )

decodeDoc ∷ BS.ByteString → IO (Scenario, [ScenarioDiagnostic])
decodeDoc bytes = case decodeScenarioBytes catalog bytes of
    ScenarioLoaded s ds → pure (s, ds)
    ScenarioFailed f → expectationFailure ("scenario failed: " ⧺ show f) ≫ error "unreachable"

-- | A scenario that must decode with no diagnostic at all.
cleanDoc ∷ String → IO Scenario
cleanDoc yaml = do
    (s, ds) ← decodeDoc (BC.pack yaml)
    ds `shouldBe` []
    pure s

tg ∷ Text → GameplayTag
tg t = fromMaybe (error "empty fixture tag") (mkGameplayTag t)

-- | One item entry with only a definition stated.
bare ∷ ScenarioId → Text → ItemEntry
bare i d = ItemEntry i [] d Omitted Omitted Omitted Omitted Omitted Omitted Omitted Omitted Omitted

allNodes ∷ ItemInstance → [ItemInstance]
allNodes i = i : concatMap allNodes (iiContents i)

-- | Everything physical about one node, instance id excluded.
data Phys = Phys
    { pDef ∷ Text, pFill ∷ Float, pQuality ∷ Float, pCondition ∷ Float
    , pWeight ∷ Float, pSharp ∷ Float, pTemp ∷ Maybe Float
    , pBulk ∷ Maybe Float, pStorage ∷ Maybe (Float, Float) }
    deriving (Show, Eq)

phys ∷ ItemInstance → Phys
phys i = Phys (iiDefName i) (iiCurrentFill i) (iiQuality i) (iiCondition i)
              (iiWeight i) (iiSharpness i) (iiTemp i) (iiBulk i)
              (fmap (\s → (isWeightCapacity s, isBulkCapacity s)) (iiStorage i))

-- | A whole tree as values: physical state, stable reference, tags and
--   the children in order — the equivalence the round trip compares
--   while ignoring freshly allocated runtime ids.
data Node = Node Phys (Maybe Text) [Text] [Node]
    deriving (Show, Eq)

nodeOf ∷ (Word64 → Maybe Text) → (Word64 → [Text]) → ItemInstance → Node
nodeOf refOf tagOf i = Node (phys i) (refOf (iiInstanceId i)) (tagOf (iiInstanceId i))
                            (map (nodeOf refOf tagOf) (iiContents i))

-- | A live test world: real manager refs holding the built owners.
data Live = Live
    { lvUm ∷ IORef UnitManager, lvBm ∷ IORef BuildingManager, lvWm ∷ IORef WorldManager }

pageP ∷ WorldPageId
pageP = WorldPageId "scenario_items_page"

newLive ∷ UnitInstance → BuildingInstance → GroundItems → IO Live
newLive u b g = do
    ws ← emptyWorldState
    _ ← atomicModifyIORef' (wsGroundItemsRef ws) (const (g, ()))
    um ← newIORef emptyUnitManager
        { umInstances = HM.singleton (UnitId 1) u { uiPage = pageP }, umNextId = 2 }
    bm ← newIORef emptyBuildingManager
        { bmInstances = HM.singleton (BuildingId 1) b { biPage = pageP }, bmNextId = 2 }
    wm ← newIORef emptyWorldManager { wmWorlds = [(pageP, ws)], wmVisible = [pageP] }
    pure (Live um bm wm)

itemTarget ∷ Word64 → ItemTarget
itemTarget i = fromMaybe (error "instance id 0") (mkItemTarget i)

-- | Commit a build's tags and references the way an orchestrator would.
commitBuild ∷ Live → [ItemBuild] → IO (Either [ItemTarget] ())
commitBuild lv bs = commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv)
    (concatMap itemTagTargets bs) (concatMap itemRefTargets bs)

-- * The documents

-- | Every owner category in one scenario: carried inventory (a canteen,
--   a three-level kit tree), equipment, an accessory, building storage
--   (an overloaded crate) and delivered materials, and a ground gem.
mainYaml ∷ String
mainYaml = unlines
    [ "version: 1"
    , "units:"
    , "  - id: scout"
    , "    definition: acolyte"
    , "    x: 0"
    , "    y: 0"
    , "    inventory:"
    , "      - {id: canteen-1, definition: canteen, fill: 0.5}"
    , "      - id: kit-1"
    , "        definition: kit"
    , "        tags: [medical]"
    , "        contents:"
    , "          - {id: bandage-a, definition: bandage, quality: 80, tags: [fresh]}"
    , "          - {id: bandage-b, definition: bandage, quality: 40, tags: [worn]}"
    , "          - id: pouch-1"
    , "            definition: pouch"
    , "            contents:"
    , "              - {id: bandage-c, definition: bandage, condition: 0}"
    , "    equipment:"
    , "      main_hand: {id: knife-1, definition: knife, condition: 5, sharpness: 15, quality: 90, temperature: 36.5}"
    , "    accessories:"
    , "      - {id: ring-1, definition: ring}"
    , "buildings:"
    , "  - id: hold"
    , "    definition: cargo_hold"
    , "    x: 3"
    , "    y: 3"
    , "    storage:"
    , "      - id: crate-1"
    , "        definition: crate"
    , "        contents:"
    , "          - {id: knife-2, definition: knife}"
    , "          - {id: knife-3, definition: knife}"
    , "          - {id: knife-4, definition: knife}"
    , "    materials_delivered:"
    , "      - {id: plate-1, definition: steel_plate}"
    , "ground_items:"
    , "  - {id: relic, definition: gem, x: 5, y: 5, tags: [idol]}"
    ]

-- | The runtime world a document builds.
data World = World
    { wUnit ∷ UnitInstance, wBuilding ∷ BuildingInstance, wGround ∷ GroundItems
    , wBuilds ∷ [ItemBuild] }

buildWorld ∷ ItemBuildEnv → Scenario → IO World
buildWorld env sc = do
    let (u, b) = case (scUnits sc, scBuildings sc) of
            ([u0], [b0]) → (u0, b0)
            _ → error "the fixture holds one unit and one building"
    (ui, ub) ← buildUnitItems env "units[0]" u
    (bi, bb) ← buildBuildingItems env "buildings[0]" b
    (gp, gb) ← buildGroundItems env (scGroundItems sc)
    let unit = (mkUnit "acolyte" FactionPlayer (0, 0) 100
                       (authoredOr [] (uitInventory ui)) (authoredOr [] (uitAccessories ui)))
                   { uiEquipment = authoredOr HM.empty (uitEquipment ui) }
        bld = (mkBuilding "cargo_hold" (3, 3) (2, 2) (authoredOr [] (bitStorage bi)))
                  { biMaterialsDelivered = authoredOr HM.empty (bitDelivered bi) }
        (ground, _) = installGroundItems (catMaybes gp) emptyGroundItems
    pure World { wUnit = unit, wBuilding = bld, wGround = ground, wBuilds = [ub, bb, gb] }

worldRefs ∷ World → [(Word64, Text)]
worldRefs = concatMap ibRefs ∘ wBuilds

worldTags ∷ World → [(Word64, Set.Set GameplayTag)]
worldTags = concatMap ibTags ∘ wBuilds

ctxOf ∷ [(Word64, Text)] → [(Word64, Set.Set GameplayTag)] → CaptureCtx
ctxOf refs tgs = CaptureCtx
    { ccRef = \i → lookup i refs
    , ccTags = \i → fromMaybe Set.empty (lookup i tgs) }

-- | A scenario document from captured entries.
docOf ∷ UnitCapture → BuildingCapture → [(Float, Float, ItemEntry)] → A.Value
docOf uc bc ground = A.object
    [ "version" A..= (1 ∷ Int)
    , "units" A..= [ A.object
        [ "id" A..= ("scout" ∷ Text), "definition" A..= ("acolyte" ∷ Text)
        , "x" A..= (0 ∷ Int), "y" A..= (0 ∷ Int)
        , "inventory" A..= map itemEntryValue (ucInventory uc)
        , "equipment" A..= A.object [ (K.fromText s, itemEntryValue e) | (s, e) ← ucEquipment uc ]
        , "accessories" A..= map itemEntryValue (ucAccessories uc) ] ]
    , "buildings" A..= [ A.object
        [ "id" A..= ("hold" ∷ Text), "definition" A..= ("cargo_hold" ∷ Text)
        , "x" A..= (3 ∷ Int), "y" A..= (3 ∷ Int)
        , "storage" A..= map itemEntryValue (bcStorage bc)
        , "materials_delivered" A..= map itemEntryValue (bcDelivered bc) ] ]
    , "ground_items" A..= [ A.object (("x", A.toJSON x) : ("y", A.toJSON y) : itemEntryFields e)
                          | (x, y, e) ← ground ]
    ]

-- | What a world holds, as comparable values.
worldNodes ∷ (Word64 → Maybe Text) → (Word64 → [Text]) → World
           → ([Node], [(Text, Node)], [Node], [Node], [Node], [(Float, Float, Node)])
worldNodes refOf tagOf w =
    ( map n (uiInventory (wUnit w))
    , sortOn fst [ (s, n i) | (s, i) ← HM.toList (uiEquipment (wUnit w)) ]
    , map n (uiAccessories (wUnit w))
    , map n (biStorage (wBuilding w))
    , concatMap (map n) (map snd (sortOn fst (HM.toList (biMaterialsDelivered (wBuilding w)))))
    , sortOn (\(x, y, _) → (x, y))
        [ (G.giX g, G.giY g, n (G.giInst g)) | g ← HM.elems (gisItems (wGround w)) ] )
  where n = nodeOf refOf tagOf

-- * The examples

spec ∷ Spec
spec = describe "Scenario.Items" $ do

    describe "explicit values are applied exactly" $ do

        it "a half-full canteen and a near-broken knife keep their stated values; \
           \empty weight is not the total" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            [canteen, _kit] ← pure (uiInventory (wUnit w))
            let knife = uiEquipment (wUnit w) HM.! "main_hand"
            iiCurrentFill canteen `shouldBe` 0.5
            iiWeight canteen `shouldBe` 0.4
            itemTotalWeight defs canteen `shouldBe` 0.9
            (iiCondition knife, iiSharpness knife, iiQuality knife, iiTemp knife)
                `shouldBe` (5, 15, 90, Just 36.5)

        it "valid zeros and empty collections are authoritative" $ do
            let e = (bare (ExplicitId "z") "kit")
                    { ieFill = Authored 0, ieQuality = Authored 0, ieCondition = Authored 0
                    , ieSharpness = Authored 0, ieWeight = Authored 0
                    , ieBulk = Authored (Just 0)
                    , ieStorage = Authored (Just (StorageCapacity 0 0))
                    , ieTemperature = Authored (TrackedTemp 0)
                    , ieContents = Authored [] }
                f = (bare (ExplicitId "f") "flask") { ieFill = Authored 0 }
            (env, _) ← newEnv defs 1
            b ← buildItemForest env [("units[0].inventory[0]", e), ("units[0].inventory[1]", f)]
            ibDiagnostics b `shouldBe` []
            [Just z, Just flask] ← pure (ibTrees b)
            (iiQuality z, iiCondition z, iiSharpness z, iiWeight z, iiTemp z, iiBulk z)
                `shouldBe` (0, 0, 0, 0, Just 0, Just 0)
            iiStorage z `shouldBe` Just (ItemStorage 0 0)
            iiContents z `shouldBe` []
            -- the flask's definition default is 0.8; an explicit 0 wins
            iiCurrentFill flask `shouldBe` 0

        it "supported absence is represented, not defaulted: null bulk, null \
           \storage, ambient temperature" $ do
            let e = (bare (ExplicitId "k") "kit")
                    { ieBulk = Authored Nothing, ieStorage = Authored Nothing
                    , ieTemperature = Authored AtAmbient }
                o = bare (ExplicitId "o") "kit"
            (env, _) ← newEnv defs 1
            b ← buildItemForest env [("a", e), ("b", o)]
            ibDiagnostics b `shouldBe` []
            [Just k, Just omitted] ← pure (ibTrees b)
            (iiBulk k, iiStorage k, iiTemp k) `shouldBe` (Nothing, Nothing, Nothing)
            -- omitted = the definition's snapshot
            (iiBulk omitted, iiStorage omitted) `shouldBe` (Just 2.0, Just (ItemStorage 5.0 6.0))

        it "an out-of-range fill, a fill on a non-container and malformed \
           \numbers are diagnosed, never clamped into a success" $ do
            let overfull = (bare (ExplicitId "c") "canteen") { ieFill = Authored 5 }
                dry = (bare (ExplicitId "k") "knife") { ieFill = Authored 0.3 }
                nan = (bare (ExplicitId "n") "knife")
                        { ieQuality = Authored 120, ieWeight = Authored (-1)
                        , ieTemperature = Authored (TrackedTemp (-300))
                        , ieStorage = Authored (Just (StorageCapacity (-1) 2)) }
            (env, spent) ← newEnv defs 0
            b ← buildItemForest env [("p[0]", overfull), ("p[1]", dry), ("p[2]", nan)]
            ibDiagnostics b `shouldBe` sortOn sdPath
                [ ScenarioDiagnostic "p[0].fill" (InvalidValue "a fill in [0, 1.0]") FieldRejected
                , ScenarioDiagnostic "p[1].fill" (InvalidValue "0: this item holds no fluid") FieldRejected
                , ScenarioDiagnostic "p[2].quality" (InvalidValue "a number in [0, 100]") FieldRejected
                , ScenarioDiagnostic "p[2].weight" (InvalidValue "a number ≥ 0") FieldRejected
                , ScenarioDiagnostic "p[2].storage_capacity"
                    (InvalidValue "weight and bulk, each a number ≥ 0") FieldRejected
                , ScenarioDiagnostic "p[2].temperature"
                    (InvalidValue "\"ambient\" or a temperature in °C ≥ -273.15") FieldRejected ]
            -- every entry survives with its omission default, and the
            -- invalid value is nowhere in the instance
            [Just c, Just k, Just n] ← pure (ibTrees b)
            iiCurrentFill c `shouldBe` 0
            iiCurrentFill k `shouldBe` 0
            (iiQuality n < 100, iiWeight n, iiTemp n, iiStorage n) `shouldBe` (True, 0.3, Nothing, Nothing)
            spent `shouldReturn` 3

        it "explicit values survive changed definition defaults; omitted ones follow them" $ do
            sc ← cleanDoc mainYaml
            let changed = defsWith $ HM.adjust (\d → d { idWeight = 9, idBulk = 9
                                                         , idQualitySpec = Just (1, 2) }) "knife"
                                   ∘ HM.adjust (\d → d { idWeight = 7 }) "canteen"
            (envA, _) ← newEnv defs 1000
            (envB, _) ← newEnv changed 1000
            a ← buildWorld envA sc
            b ← buildWorld envB sc
            [canteenA, _] ← pure (uiInventory (wUnit a))
            [canteenB, _] ← pure (uiInventory (wUnit b))
            let knifeOf = (HM.! "main_hand") ∘ uiEquipment ∘ wUnit
            -- stated: condition, sharpness, quality, temperature, fill
            (iiCondition (knifeOf b), iiSharpness (knifeOf b), iiQuality (knifeOf b), iiTemp (knifeOf b))
                `shouldBe` (5, 15, 90, Just 36.5)
            iiCurrentFill canteenB `shouldBe` iiCurrentFill canteenA
            -- omitted: empty weight and bulk are the CURRENT definition's
            (iiWeight (knifeOf a), iiBulk (knifeOf a)) `shouldBe` (0.3, Just 0.2)
            (iiWeight (knifeOf b), iiBulk (knifeOf b)) `shouldBe` (9, Just 9)
            (iiWeight canteenA, iiWeight canteenB) `shouldBe` (0.4, 7)

    describe "omitted values are reproducible" $ do

        let gems = [ (T.pack ("p[" <> show i <> "]"), bare (ExplicitId (T.pack ("gem-" <> show i))) "gem")
                   | i ← [0 .. 3 ∷ Int] ]
            rolled b = [ (iiQuality g, iiWeight g) | Just g ← ibTrees b ]

        it "the same scenario gives the same values whatever the allocator \
           \numbering or prior gameplay RNG activity" $ do
            gameplay ← newIORef (mkStdGen 7)
            (envA, _) ← newEnv defs 0
            a ← buildItemForest envA gems
            -- unrelated gameplay RNG activity and a different allocator
            replicateM_ 50 (atomicModifyIORef' gameplay (\g → let (_, g') = randomR (0 ∷ Int, 9) g in (g', ())))
            (envB, _) ← newEnv defs 987654
            b ← buildItemForest envB gems
            rolled a `shouldBe` rolled b
            map iiInstanceId (catMaybes (ibTrees a)) `shouldNotBe` map iiInstanceId (catMaybes (ibTrees b))
            -- the values really vary between identities (the roll is not constant)
            length (nub (rolled a)) `shouldSatisfy` (> 1)
            -- and they are real rolls inside the definition's range
            forM_ (rolled a) $ \(q, wt) → do
                q `shouldSatisfy` (\x → x ≥ 20 ∧ x ≤ 80)
                wt `shouldSatisfy` (> 0)

        it "tags never change a default" $ do
            let plain = bare (ExplicitId "same") "gem"
                tagged = plain { ieTags = [tg "alpha", tg "beta"] }
            (env, _) ← newEnv defs 0
            a ← buildItemForest env [("x", plain)]
            b ← buildItemForest env [("x", tagged)]
            rolled a `shouldBe` rolled b

        it "an omitted roll does not depend on whether a sibling field is stated" $ do
            let plain = bare (ExplicitId "same") "gem"
                weighted = plain { ieWeight = Authored 0.123 }
                qualified = plain { ieQuality = Authored 77 }
            (env, _) ← newEnv defs 0
            built ← forM [plain, weighted, qualified] $ \e →
                catMaybes ∘ ibTrees ⊚ buildItemForest env [("x", e)]
            [p, w, q] ← pure (concat built)
            iiQuality w `shouldBe` iiQuality p      -- stating weight leaves quality alone
            iiWeight q `shouldBe` iiWeight p        -- stating quality leaves weight alone
            iiWeight w `shouldBe` 0.123
            iiQuality q `shouldBe` 77

        it "construction and capture leave the gameplay RNG and allocator alone" $ do
            sc ← cleanDoc mainYaml
            gameplay ← newIORef (mkStdGen 20261018)
            before ← show ⊚ readIORef gameplay
            (env, spent) ← newEnv defs 500
            w ← buildWorld env sc
            afterBuild ← spent
            let ctx = ctxOf (worldRefs w) (worldTags w)
                (_, uc) = captureUnitItems ctx emptyCaptureState "scout" (wUnit w)
                (_, bc) = captureBuildingItems ctx emptyCaptureState "hold" (wBuilding w)
                (_, ground, _) = captureGroundItems ctx emptyCaptureState (wGround w)
            -- force the whole capture
            length (show (ucInventory uc, bcStorage bc, ground)) `shouldSatisfy` (> 0)
            spent `shouldReturn` afterBuild                   -- capture minted nothing
            (show ⊚ readIORef gameplay) `shouldReturn` before -- and drew nothing
            -- default-expanded children draw from the node's own stream
            -- too: an omitted-contents kit reproduces its bandages' qualities
            (envC, _) ← newEnv defs 9
            (envD, _) ← newEnv defs 90000
            [Just k1] ← ibTrees ⊚ buildItemForest envC [("k", bare (ExplicitId "kk") "kit")]
            [Just k2] ← ibTrees ⊚ buildItemForest envD [("k", bare (ExplicitId "kk") "kit")]
            map phys (allNodes k1) `shouldBe` map phys (allNodes k2)

    describe "exact contents and ownership" $ do

        it "omitted contents expand the definition; [] is exactly empty; a stated \
           \list REPLACES the defaults" $ do
            let omitted = bare (ExplicitId "k1") "kit"
                emptied = (bare (ExplicitId "k2") "kit") { ieContents = Authored [] }
                partial = (bare (ExplicitId "k3") "kit")
                    { ieContents = Authored [bare (ExplicitId "only") "bandage"] }
            (env, _) ← newEnv defs 1
            b ← buildItemForest env [("a", omitted), ("b", emptied), ("c", partial)]
            ibDiagnostics b `shouldBe` []
            [Just k1, Just k2, Just k3] ← pure (ibTrees b)
            map iiDefName (iiContents k1) `shouldBe` ["bandage", "bandage", "pouch"]
            map iiDefName (concatMap iiContents (iiContents k1)) `shouldBe` ["bandage"]
            iiContents k2 `shouldBe` []
            map iiDefName (iiContents k3) `shouldBe` ["bandage"]
            -- only stated nodes carry a scenario reference
            map snd (ibRefs b) `shouldBe` ["k1", "k2", "k3", "only"]

        it "parent overrides do not leak, repeated definitions are separate items, \
           \and child order is kept" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            [_, kit] ← pure (uiInventory (wUnit w))
            [a, b, pouch] ← pure (iiContents kit)
            [c] ← pure (iiContents pouch)
            map iiDefName (iiContents kit) `shouldBe` ["bandage", "bandage", "pouch"]
            (iiQuality a, iiQuality b, iiQuality c) `shouldBe` (80, 40, 100)
            iiCondition c `shouldBe` 0
            -- three bandages, three distinct physical identities
            length (nub (map iiInstanceId [a, b, c])) `shouldBe` 3

        it "an overloaded container is restored exactly while ordinary insertion \
           \still refuses it" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            [crate] ← pure (biStorage (wBuilding w))
            map iiDefName (iiContents crate) `shouldBe` ["knife", "knife", "knife"]
            -- 0.9 kg and 0.6 L of knives inside a 0.5 kg / 0.3 L crate
            sum (map iiWeight (iiContents crate)) > isWeightCapacity (fromJust' (iiStorage crate))
                `shouldBe` True
            -- the ordinary policy still refuses another bandage
            let scene = OwnershipScene
                    { oscItems = biStorage (wBuilding w), oscOther = []
                    , oscCapacity = RootUnlimited, oscWeigh = iiWeight }
            (env2, _) ← newEnv defs 5000
            [Just extra] ← ibTrees ⊚ buildItemForest env2 [("x", bare (ExplicitId "extra") "bandage")]
            insertInstance scene (iiInstanceId crate) extra
                `shouldSatisfy` (\r → r `elem` [Left OverTargetWeight, Left OverTargetBulk])

        it "supported trees lacking capacity or bulk data are built exactly, and \
           \ordinary insertion keeps failing closed on them" $ do
            let inner = (bare (ExplicitId "inner") "bandage") { ieBulk = Authored Nothing }
                outer = (bare (ExplicitId "outer") "bare_kit")
                    { ieStorage = Authored Nothing, ieContents = Authored [inner] }
            (env, _) ← newEnv defs 1
            b ← buildItemForest env [("o", outer)]
            ibDiagnostics b `shouldBe` []
            [Just o] ← pure (ibTrees b)
            [i] ← pure (iiContents o)
            (iiStorage o, iiBulk i, length (iiContents o)) `shouldBe` (Nothing, Nothing, 1)
            let scene = OwnershipScene [o] [] RootUnlimited iiWeight
            (env2, _) ← newEnv defs 5000
            [Just spare] ← ibTrees ⊚ buildItemForest env2 [("s", bare (ExplicitId "spare") "bandage")]
            insertInstance scene (iiInstanceId o) spare `shouldBe` Left TargetNotStorage
            -- removal stays legal, so the tree can always be emptied
            fmap (const ()) (removeInstance scene (iiInstanceId i)) `shouldSatisfy` isRight'

        it "contents on an item that holds none are a rejected field, and the \
           \listed items vanish rather than spill" $ do
            let k = (bare (ExplicitId "knife") "knife")
                    { ieContents = Authored [ (bare (ExplicitId "x") "bandage")
                                                { ieContents = Authored [bare (ExplicitId "y") "ring"] } ] }
                sibling = bare (ExplicitId "sib") "ring"
            (env, spent) ← newEnv defs 0
            b ← buildItemForest env [("units[0].inventory[0]", k), ("units[0].inventory[1]", sibling)]
            ibDiagnostics b `shouldBe`
                [ ScenarioDiagnostic "units[0].inventory[0].contents"
                    (InvalidValue "no contents: this item holds no items") FieldRejected
                , ScenarioDiagnostic "units[0].inventory[0].contents[0]"
                    (OwnerRejected "units[0].inventory[0].contents") CascadeRejected
                , ScenarioDiagnostic "units[0].inventory[0].contents[0].contents[0]"
                    (OwnerRejected "units[0].inventory[0].contents") CascadeRejected ]
            map (fmap iiDefName) (ibTrees b) `shouldBe` [Just "knife", Just "ring"]
            concatMap (map iiDefName ∘ allNodes) (catMaybes (ibTrees b)) `shouldBe` ["knife", "ring"]
            spent `shouldReturn` 2

    describe "identity, tags and the live attachment boundary" $ do

        it "scenario references, instance ids and ground-row ids are three \
           \different things" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            let refs = worldRefs w
                ids = map fst refs
            sort (map snd refs) `shouldBe` sort
                [ "canteen-1", "kit-1", "bandage-a", "bandage-b", "pouch-1", "bandage-c"
                , "knife-1", "ring-1"
                , "crate-1", "knife-2", "knife-3", "knife-4", "plate-1", "relic" ]
            length (nub ids) `shouldBe` length ids
            all (> 1000) ids `shouldBe` True
            -- the ground row is slot 0 of its store; its item is not "item 0"
            HM.keys (gisItems (wGround w)) `shouldBe` [0]
            ids `shouldSatisfy` notElem 0
            -- default-expanded children (the pouch's would-be defaults) do not
            -- appear: nothing named them
            length ids `shouldBe` 14

        it "tags follow only the nodes that stated them; same-definition siblings \
           \keep their own" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            let refOf i = lookup i (worldRefs w)
                tagged = sort [ (fromMaybe "?" (refOf i), map gameplayTagText (Set.toAscList ts))
                              | (i, ts) ← worldTags w ]
            tagged `shouldBe` sort
                [ ("kit-1", ["medical"]), ("bandage-a", ["fresh"]), ("bandage-b", ["worn"])
                , ("relic", ["idol"]) ]

        it "commits tags and references atomically into live owners, and a \
           \forgotten item takes both with it" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            lv ← newLive (wUnit w) (wBuilding w) (wGround w)
            commitBuild lv (wBuilds w) `shouldReturn` Right ()
            wm ← readIORef (lvWm lv)
            let refs = itemRefsToAscList (wmItemScenarioRefs wm)
            sort refs `shouldBe` sort (worldRefs w)
            v ← captureLiveTagView (lvUm lv) (lvBm lv) (lvWm lv)
            let idOf r = fromMaybe 0 (lookup r [ (r', i) | (i, r') ← refs ])
            itemTagsIn v (itemTarget (idOf "bandage-a")) `shouldBe` Just (Set.fromList [tg "fresh"])
            itemTagsIn v (itemTarget (idOf "bandage-c")) `shouldBe` Just Set.empty
            -- destroying an item forgets its tags and reference together
            epoch ← itemTagEpoch (lvWm lv)
            forgetItemTags (lvWm lv) epoch (HS.fromList [idOf "bandage-a", idOf "kit-1"])
            wm2 ← readIORef (lvWm lv)
            map snd (itemRefsToAscList (wmItemScenarioRefs wm2)) `shouldSatisfy`
                (\rs → "bandage-a" `notElem` rs ∧ "kit-1" `notElem` rs ∧ "bandage-b" `elem` rs)

        it "a tree with any non-live target installs nothing: no tags, no references" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            lv ← newLive (wUnit w) (wBuilding w) (wGround w)
            let ghost = itemTarget 424242
                tgts = concatMap itemTagTargets (wBuilds w) ⧺ [(ghost, Set.fromList [tg "ghost"])]
            r ← commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv) tgts
                    (concatMap itemRefTargets (wBuilds w) ⧺ [(ghost, "ghost-ref")])
            r `shouldBe` Left [ghost, ghost]
            wm ← readIORef (lvWm lv)
            itemRefsToAscList (wmItemScenarioRefs wm) `shouldBe` []
            wmItemTags wm `shouldSatisfy` nullCategoryIndex'

        it "a reference is one item's and an item has one reference: a clash \
           \refuses the commit whole" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            lv ← newLive (wUnit w) (wBuilding w) (wGround w)
            commitBuild lv (wBuilds w) `shouldReturn` Right ()
            let refs = worldRefs w
            ((i1, r1) : (i2, _) : _) ← pure refs
            -- a second item claiming an already-held reference
            commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv) [] [(itemTarget i2, r1)]
                `shouldReturn` Left [itemTarget i2]
            -- an item re-labelled
            commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv) [] [(itemTarget i1, "renamed")]
                `shouldReturn` Left [itemTarget i1]
            -- the same pair again is an idempotent retry
            commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv) [] [(itemTarget i1, r1)]
                `shouldReturn` Right ()
            wm ← readIORef (lvWm lv)
            lookupItemRef i2 (wmItemScenarioRefs wm) `shouldBe` lookup i2 refs

        it "an identity stated twice rejects every holder and what they own" $ do
            let a = (bare (ExplicitId "dup") "kit")
                    { ieContents = Authored [bare (ExplicitId "child") "bandage"] }
                b = bare (ExplicitId "dup") "ring"
                c = bare (ExplicitId "fine") "ring"
            (env, spent) ← newEnv defs 0
            r ← buildItemForest env [("u[0]", a), ("u[1]", b), ("u[2]", c)]
            ibDiagnostics r `shouldBe`
                [ ScenarioDiagnostic "u[0]" (DuplicateId "dup") EntryRejected
                , ScenarioDiagnostic "u[0].contents[0]" (OwnerRejected "u[0]") CascadeRejected
                , ScenarioDiagnostic "u[1]" (DuplicateId "dup") EntryRejected ]
            map (fmap iiDefName) (ibTrees r) `shouldBe` [Nothing, Nothing, Just "ring"]
            map snd (ibRefs r) `shouldBe` ["fine"]
            spent `shouldReturn` 1

    describe "recoverable failures are structured, precise and leave nothing behind" $ do

        it "an unknown definition rejects the entry and its descendants only; \
           \siblings survive and no id is spent on the dead tree" $ do
            let ghost = (bare (ExplicitId "ghost") "no_such_kit")
                    { ieTags = [tg "haunted"]
                    , ieContents = Authored [ (bare (ExplicitId "inside") "kit")
                                                { ieTags = [tg "orphan"] } ] }
                ok = bare (ExplicitId "ok") "ring"
            (env, spent) ← newEnv defs 0
            b ← buildItemForest env [("units[1].inventory[0]", ghost), ("units[1].inventory[1]", ok)]
            ibDiagnostics b `shouldBe`
                [ ScenarioDiagnostic "units[1].inventory[0]" (UnknownDefinition "no_such_kit") EntryRejected
                , ScenarioDiagnostic "units[1].inventory[0].contents[0]"
                    (OwnerRejected "units[1].inventory[0]") CascadeRejected ]
            map (fmap iiDefName) (ibTrees b) `shouldBe` [Nothing, Just "ring"]
            -- no orphan promoted, no tag installed, no reference kept
            (ibRefs b, ibTags b) `shouldBe` ([(1, "ok")], [])
            spent `shouldReturn` 1

        it "a self-cycle in default contents drops the back edge and says so" $ do
            (env, _) ← newEnv defs 0
            b ← buildItemForest env [("u[0]", bare (ExplicitId "c") "cyc_self")]
            [Just c] ← pure (ibTrees b)
            iiContents c `shouldBe` []
            ibDiagnostics b `shouldBe`
                [ ScenarioDiagnostic "u[0].contents[0]" (CyclicContents "cyc_self → cyc_self") EntryRejected ]

        it "a cycle across definitions keeps the acyclic prefix and reports the closed loop once" $ do
            (env, _) ← newEnv defs 0
            b ← buildItemForest env [("u[0]", bare (ExplicitId "c") "cyc_a")]
            [Just a] ← pure (ibTrees b)
            map iiDefName (allNodes a) `shouldBe` ["cyc_a", "cyc_b"]
            ibDiagnostics b `shouldBe`
                [ ScenarioDiagnostic "u[0].contents[0].contents[0]"
                    (CyclicContents "cyc_a → cyc_b → cyc_a") EntryRejected ]

        it "an unknown default entry is reported and its valid sibling kept" $ do
            (env, _) ← newEnv defs 0
            b ← buildItemForest env [("u[0]", bare (ExplicitId "k") "bad_kit")]
            [Just k] ← pure (ibTrees b)
            map iiDefName (iiContents k) `shouldBe` ["bandage"]
            ibDiagnostics b `shouldBe`
                [ ScenarioDiagnostic "u[0].contents[1]" (UnknownDefinition "no_such_item") EntryRejected ]

        it "ordinary materialization is unchanged: the same defaults, the same draws, \
           \and a cyclic or unknown default never throws" $ do
            logger ← quietLogger
            rngA ← newIORef (mkStdGen 1)
            (envA, _) ← newEnv defs 0
            Just viaProduction ← materializeItem defs logger rngA (ibeAllocId envA) pristineItem "kit"
            -- the spec form, on the shared generator with no per-node rolls
            rngB ← newIORef (mkStdGen 1)
            (envB, _) ← newEnv defs 0
            (Just viaSpec, notes) ← materializeSpec defs rngB (ibeAllocId envB) (itemSpec "kit")
            notes `shouldBe` []
            viaSpec `shouldBe` viaProduction
            Just cyc ← materializeItem defs logger rngA (ibeAllocId envA) pristineItem "cyc_a"
            map iiDefName (allNodes cyc) `shouldBe` ["cyc_a", "cyc_b"]
            Just bad ← materializeItem defs logger rngA (ibeAllocId envA) pristineItem "bad_kit"
            map iiDefName (iiContents bad) `shouldBe` ["bandage"]

    describe "owner adapters" $ do

        it "a stated-empty owner is distinguished from an omitted one" $ do
            sc ← cleanDoc $ unlines
                [ "version: 1", "units:"
                , "  - {id: bare, definition: acolyte, x: 0, y: 0}"
                , "  - {id: empty, definition: acolyte, x: 1, y: 0, inventory: [], equipment: {}, accessories: []}" ]
            (env, _) ← newEnv defs 1
            [bareU, emptyU] ← forM (zip [0 ∷ Int ..] (scUnits sc)) $ \(i, u) →
                fst ⊚ buildUnitItems env ("units[" <> tshow i <> "]") u
            (isAuthored (uitInventory bareU), isAuthored (uitEquipment bareU), isAuthored (uitAccessories bareU))
                `shouldBe` (False, False, False)
            (fmap length (uitInventory emptyU), fmap HM.size (uitEquipment emptyU)
                , fmap length (uitAccessories emptyU))
                `shouldBe` (Authored 0, Authored 0, Authored 0)

        it "equipment is built in slot order, so allocation never follows a hash map" $ do
            sc ← cleanDoc $ unlines
                [ "version: 1", "units:"
                , "  - id: u"
                , "    definition: acolyte"
                , "    x: 0"
                , "    y: 0"
                , "    equipment:"
                , "      main_hand: {id: a, definition: ring}"
                , "      back: {id: b, definition: ring}" ]
            (env, _) ← newEnv defs 0
            [unitE] ← pure (scUnits sc)
            (items, built) ← buildUnitItems env "units[0]" unitE
            -- back < main_hand: the first id goes to "back"
            map snd (ibRefs built) `shouldBe` ["b", "a"]
            fmap (HM.map iiInstanceId) (uitEquipment items)
                `shouldBe` Authored (HM.fromList [("back", 1), ("main_hand", 2)])

        it "a rejected container takes its contents with it and nothing is promoted \
           \into the owner's collection" $ do
            let ghost = (bare (ExplicitId "g") "ghost_box")
                    { ieContents = Authored [bare (ExplicitId "in") "ring"] }
                u = unitWithInventory [ghost, bare (ExplicitId "ok") "ring"]
            (env, _) ← newEnv defs 0
            (items, built) ← buildUnitItems env "units[0]" u
            fmap (map iiDefName) (uitInventory items) `shouldBe` Authored ["ring"]
            map snd (ibRefs built) `shouldBe` ["ok"]
            map sdPath (ibDiagnostics built) `shouldBe`
                ["units[0].inventory[0]", "units[0].inventory[0].contents[0]"]

        it "delivered materials group by definition in stated order; building and ground \
           \items need no capacity" $ do
            sc ← cleanDoc $ unlines
                [ "version: 1", "buildings:"
                , "  - id: h"
                , "    definition: cargo_hold"
                , "    x: 0"
                , "    y: 0"
                , "    materials_delivered:"
                , "      - {id: p1, definition: steel_plate, quality: 10}"
                , "      - {id: p2, definition: steel_plate, quality: 20}"
                , "ground_items:"
                , "  - {id: g, definition: crate, x: 2.5, y: 2.5}" ]
            (env, _) ← newEnv defs 0
            [buildingE] ← pure (scBuildings sc)
            (bi, _) ← buildBuildingItems env "buildings[0]" buildingE
            fmap (HM.map (map iiQuality)) (bitDelivered bi)
                `shouldBe` Authored (HM.singleton "steel_plate" [10, 20])
            (placements, gb) ← buildGroundItems env (scGroundItems sc)
            ibDiagnostics gb `shouldBe` []
            let (store, gids) = installGroundItems (catMaybes placements) emptyGroundItems
            gids `shouldBe` [0]
            fmap (iiDefName ∘ G.giInst) (HM.lookup 0 (gisItems store)) `shouldBe` Just "crate"
            -- ground quality/condition are the stated or default ones, never salvage-worn
            fmap (iiCondition ∘ G.giInst) (HM.lookup 0 (gisItems store)) `shouldBe` Just 100

    describe "capture and the round trip" $ do

        it "materialize → mutate → capture → YAML encode/decode → rematerialize is equivalent" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w0 ← buildWorld env sc
            -- one real crafted item arrives after the scenario was built
            (env2, _) ← newEnv defs 7000
            [Just crafted] ← ibTrees ⊚ buildItemForest env2
                [("x", (bare (ExplicitId "scratch") "gem") { ieQuality = Authored 61 })]
            [canteen, kit] ← pure (uiInventory (wUnit w0))
            [b1, _b2, pouch] ← pure (iiContents kit)
            let usedKit = kit { iiContents = [b1, pouch] }                -- a bandage consumed
                drunk = canteen { iiCurrentFill = 0.25, iiTemp = Just 4 }
                knife = (uiEquipment (wUnit w0) HM.! "main_hand") { iiCondition = 3, iiSharpness = 0 }
                unit' = (wUnit w0) { uiInventory = [drunk, usedKit, crafted]
                                   , uiEquipment = HM.singleton "main_hand" knife }
                w1 = w0 { wUnit = unit' }
                ctx = ctxOf (worldRefs w0) (worldTags w0)
                (st1, uc) = captureUnitItems ctx emptyCaptureState "scout" unit'
                (st2, bc) = captureBuildingItems ctx st1 "hold" (wBuilding w1)
                (_, ground, gcap) = captureGroundItems ctx st2 (wGround w1)
            -- the stated reference comes back; the crafted item gets one that
            -- depends on where it sits, not on its id
            map ieId (ucInventory uc) `shouldBe`
                [ExplicitId "canteen-1", ExplicitId "kit-1", ExplicitId "scout.inventory.2"]
            -- current values, explicit, not the scenario's originals
            [cE, kE, xE] ← pure (ucInventory uc)
            (ieFill cE, ieTemperature cE) `shouldBe` (Authored 0.25, Authored (TrackedTemp 4))
            (map ieId (authoredOr [] (ieContents kE))) `shouldBe`
                [ExplicitId "bandage-a", ExplicitId "pouch-1"]
            ieContents xE `shouldBe` Authored []
            -- encode → decode
            let bytes = Y.encode (docOf uc bc ground)
            (decoded, ds) ← decodeDoc bytes
            ds `shouldBe` []
            [du] ← pure (scUnits decoded)
            [db] ← pure (scBuildings decoded)
            ueInventory du `shouldBe` Authored (ucInventory uc)
            ueEquipment du `shouldBe` Authored (HM.fromList (ucEquipment uc))
            beStorage db `shouldBe` Authored (bcStorage bc)
            map giItem (scGroundItems decoded) `shouldBe` [ e | (_, _, e) ← ground ]
            -- rematerialize with a different allocator and compare everything
            (env3, _) ← newEnv defs 90000
            w2 ← buildWorld env3 decoded
            let refsAfter = capRefs (ucCaptured uc) ⧺ capRefs (bcCaptured bc) ⧺ capRefs gcap
                refOf0 i = lookup i refsAfter
                tagsOf0 i = map gameplayTagText (Set.toAscList (ccTags ctx i))
                refOf1 i = lookup i (worldRefs w2)
                tagsOf1 i = map gameplayTagText (Set.toAscList (fromMaybe Set.empty (lookup i (worldTags w2))))
            worldNodes refOf0 tagsOf0 w1 `shouldBe` worldNodes refOf1 tagsOf1 w2

        it "an emptied kit stays empty and a used one does not regain its loadout" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            [canteen, kit] ← pure (uiInventory (wUnit w))
            let unit' = (wUnit w) { uiInventory = [canteen, kit { iiContents = [] }] }
                ctx = ctxOf (worldRefs w) (worldTags w)
                (_, uc) = captureUnitItems ctx emptyCaptureState "scout" unit'
            map ieContents (ucInventory uc) `shouldBe` [Authored [], Authored []]
            ieDefinition (ucInventory uc !! 1) `shouldBe` "kit"

        it "capture writes every supported value explicitly and the item's own empty \
           \weight, whatever the definition says now" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            let ctx = ctxOf (worldRefs w) (worldTags w)
                (_, uc) = captureUnitItems ctx emptyCaptureState "scout" (wUnit w)
            [cE, kE] ← pure (ucInventory uc)
            ieWeight cE `shouldBe` Authored 0.4              -- empty weight, not 0.9
            ieQuality kE `shouldSatisfy` isAuthored
            (ieBulk kE, ieStorage kE) `shouldBe` (Authored (Just 2.0), Authored (Just (StorageCapacity 5.0 6.0)))

        it "recapture is independent of instance ids: the same arena built twice \
           \with different numbering captures to the same entries" $ do
            sc ← cleanDoc mainYaml
            let run start = do
                    (env, _) ← newEnv defs start
                    w ← buildWorld env sc
                    -- drop the references so every node derives one from where it sits
                    let ctx = CaptureCtx (const Nothing) (const Set.empty)
                        (st1, uc) = captureUnitItems ctx emptyCaptureState "scout" (wUnit w)
                        (st2, bc) = captureBuildingItems ctx st1 "hold" (wBuilding w)
                        (_, ground, _) = captureGroundItems ctx st2 (wGround w)
                    pure (ucInventory uc, ucEquipment uc, ucAccessories uc, bcStorage bc, bcDelivered bc, ground)
            a ← run 10
            b ← run 5000
            a `shouldBe` b
            let (inv, _, acc, _, _, _) = a
            map ieId inv `shouldBe` [ExplicitId "scout.inventory.0", ExplicitId "scout.inventory.1"]
            map ieId acc `shouldBe` [ExplicitId "scout.accessories.0"]

        it "ground capture ignores the order and the row ids the items were laid in" $ do
            let mk i d x = do
                    (env, _) ← newEnv defs i
                    [Just it'] ← ibTrees ⊚ buildItemForest env
                        [("g", bare (ExplicitId (d <> tshow i)) d)]
                    pure (it', x)
            a1 ← mk (0 ∷ Word64) "ring" (1 ∷ Float)
            a2 ← mk 100 "knife" 2
            a3 ← mk 200 "ring" 1
            let lay xs = fst (installGroundItems [ GroundPlacement x 0 i | (i, x) ← xs ] emptyGroundItems)
                capture g = let ctx = CaptureCtx (const Nothing) (const Set.empty)
                                (_, rows, _) = captureGroundItems ctx emptyCaptureState g
                            in [ (x, y, ieDefinition e, ieId e) | (x, y, e) ← rows ]
            capture (lay [a1, a2, a3]) `shouldBe` capture (lay [a3, a2, a1])

        it "an automatic id is captured in its explicit form, and a taken reference \
           \is never given twice" $ do
            (env, _) ← newEnv defs 0
            let autoA = bare (AutoId "units[0].inventory[0]") "ring"
                autoB = bare (AutoId "units[0].inventory[1]") "ring"
            b ← buildItemForest env [("units[0].inventory[0]", autoA), ("units[0].inventory[1]", autoB)]
            [Just r0, Just r1] ← pure (ibTrees b)
            let -- a third item that would take "units.0.inventory.0" by position
                ctx = ctxOf (ibRefs b) []
                (_, cap) = captureNodes ctx emptyCaptureState
                    [("units.0.inventory.0", r1), ("units.0.inventory.1", r0)]
            -- both keep their own (sanitized) references despite crossed bases
            map ieId (capEntries cap) `shouldBe`
                [ExplicitId "units.0.inventory.1", ExplicitId "units.0.inventory.0"]
            -- two unmapped nodes proposing one base get distinct, deterministic references
            let (_, clash) = captureNodes (CaptureCtx (const Nothing) (const Set.empty)) emptyCaptureState
                    [("x", r0), ("x", r1)]
            map ieId (capEntries clash) `shouldBe` [ExplicitId "x", ExplicitId "x-2"]

    describe "an instance with its own storage owns what it holds" $ do

        it "a definition that holds nothing still round-trips contents once the \
           \instance states a storage capacity, and ordinary insertion into it works" $ do
            let storing = (bare (ExplicitId "knife-box") "knife")
                    { ieStorage = Authored (Just (StorageCapacity 1.0 1.0)) }
            (env, _) ← newEnv defs 1
            built ← buildItemForest env [("units[0].inventory[0]", storing)]
            ibDiagnostics built `shouldBe` []
            [Just knife] ← pure (ibTrees built)
            (env2, _) ← newEnv defs 500
            [Just bandage] ← ibTrees ⊚ buildItemForest env2
                [("x", bare (ExplicitId "dressing") "bandage")]
            -- the ordinary policy keys the insert on the INSTANCE's storage
            let scene = OwnershipScene [knife] [] RootUnlimited iiWeight
            filled ← case insertInstance scene (iiInstanceId knife) bandage of
                Right [k] → pure k
                other → expectationFailure ("insert refused: " ⧺ show (fmap (map iiDefName) other))
                          ≫ error "unreachable"
            map iiDefName (iiContents filled) `shouldBe` ["bandage"]
            -- capture states the child; the decoder accepts it; rebuild keeps it
            let ctx = CaptureCtx (const Nothing) (const Set.empty)
                (_, cap) = captureNodes ctx emptyCaptureState [("scout.inventory.0", filled)]
                doc = A.object
                    [ "version" A..= (1 ∷ Int)
                    , "units" A..= [ A.object
                        [ "id" A..= ("scout" ∷ Text), "definition" A..= ("acolyte" ∷ Text)
                        , "x" A..= (0 ∷ Int), "y" A..= (0 ∷ Int)
                        , "inventory" A..= map itemEntryValue (capEntries cap) ] ] ]
            (decoded, ds) ← decodeDoc (Y.encode doc)
            ds `shouldBe` []
            (env3, _) ← newEnv defs 9000
            [u] ← pure (scUnits decoded)
            (items, _) ← buildUnitItems env3 "units[0]" u
            fmap (map (map iiDefName ∘ iiContents)) (uitInventory items)
                `shouldBe` Authored [["bandage"]]

        it "an item that neither holds items nor states storage still cannot own contents" $ do
            sc ← decodeDoc $ BC.pack $ unlines
                [ "version: 1", "units:"
                , "  - id: u"
                , "    definition: acolyte"
                , "    x: 0"
                , "    y: 0"
                , "    inventory:"
                , "      - id: k"
                , "        definition: knife"
                , "        contents: [{id: b, definition: bandage}]" ]
            map sdPath (snd sc) `shouldBe` ["units[0].inventory[0].contents", "units[0].inventory[0].contents[0]"]

    describe "references stay unique, persistable and attached atomically" $ do

        let autoRing = bare (AutoId "units[0].inventory[0]") "ring"
            plainRing = bare (ExplicitId "units.0.inventory.0") "ring"

        it "an automatic id and an explicit id that spells like it stay two names, \
           \whichever is traversed first" $ do
            (env, _) ← newEnv defs 0
            b ← buildItemForest env [("a", autoRing), ("b", plainRing)]
            [Just ra, Just rb] ← pure (ibTrees b)
            let ctx = ctxOf (ibRefs b) []
                names order = map ieId $ capEntries $ snd $
                    captureNodes ctx emptyCaptureState order
            -- the authored explicit id is never renamed; the converted one yields
            names [("p.0", ra), ("p.1", rb)]
                `shouldBe` [ExplicitId "units.0.inventory.0-2", ExplicitId "units.0.inventory.0"]
            names [("p.0", rb), ("p.1", ra)]
                `shouldBe` [ExplicitId "units.0.inventory.0", ExplicitId "units.0.inventory.0-2"]

        it "a losing concurrent attachment installs nothing: the reference is \
           \re-checked in the atomic step" $ do
            sc ← cleanDoc mainYaml
            (env, _) ← newEnv defs 1000
            w ← buildWorld env sc
            lv ← newLive (wUnit w) (wBuilding w) (wGround w)
            ((ia, _) : (ib, _) : _) ← pure (worldRefs w)
            let rival = commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv)
                            [] [(itemTarget ib, "contested")]
                seam = noItemWriteSeam
                    { iwsBeforeCommit = \n → when (n ≡ 1) (rival ⌦ (`shouldBe` Right ())) }
            r ← commitItemTreeAssignmentsWith seam (lvUm lv) (lvBm lv) (lvWm lv)
                    [(itemTarget ia, Set.fromList [tg "late"])] [(itemTarget ia, "contested")]
            r `shouldBe` Left [itemTarget ia]
            wm ← readIORef (lvWm lv)
            lookupItemRef ia (wmItemScenarioRefs wm) `shouldBe` Nothing
            lookupItemRef ib (wmItemScenarioRefs wm) `shouldBe` Just "contested"
            -- the loser's tags were not installed either
            v ← captureLiveTagView (lvUm lv) (lvBm lv) (lvWm lv)
            itemTagsIn v (itemTarget ia) `shouldBe` Just Set.empty

        it "only persistable references are admitted: the decoder, the builder and \
           \the attachment boundary agree on the limit" $ do
            let long = T.replicate (maxItemRefLength + 1) "a"
                ok = T.replicate maxItemRefLength "a"
            -- decoder: an over-long explicit id is a rejected field (falls back to the automatic id)
            (sc, ds) ← decodeDoc $ BC.pack $ unlines
                [ "version: 1", "ground_items:"
                , "  - {id: " ⧺ T.unpack long ⧺ ", definition: ring, x: 0, y: 0}"
                , "  - {id: " ⧺ T.unpack ok ⧺ ", definition: ring, x: 1, y: 0}" ]
            map sdPath ds `shouldBe` ["ground_items[0].id"]
            map (scenarioIdText ∘ ieId ∘ giItem) (scGroundItems sc)
                `shouldBe` ["ground_items[0]", ok]
            -- builder: a caller-built entry with a reference the save cannot carry is rejected whole
            (env, spent) ← newEnv defs 0
            b ← buildItemForest env [("p[0]", bare (ExplicitId long) "ring"), ("p[1]", bare (ExplicitId "fine") "ring")]
            map sdEffect (ibDiagnostics b) `shouldBe` [EntryRejected]
            map snd (ibRefs b) `shouldBe` ["fine"]
            spent `shouldReturn` 1
            -- boundary: refuses it too (and a malformed one), installing nothing
            sc2 ← cleanDoc mainYaml
            (env2, _) ← newEnv defs 1000
            w ← buildWorld env2 sc2
            lv ← newLive (wUnit w) (wBuilding w) (wGround w)
            ((ia, _) : _) ← pure (worldRefs w)
            commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv) [] [(itemTarget ia, long)]
                `shouldReturn` Left [itemTarget ia]
            commitItemTreeAssignments (lvUm lv) (lvBm lv) (lvWm lv) [] [(itemTarget ia, "has space")]
                `shouldReturn` Left [itemTarget ia]
            readIORef (lvWm lv) ⌦ \wm → itemRefsToAscList (wmItemScenarioRefs wm) `shouldBe` []

        it "a derived reference always fits: long owner names are shortened with a \
           \stable hash and stay distinct" $ do
            (env, _) ← newEnv defs 0
            b ← buildItemForest env
                [("a", bare (ExplicitId "x") "ring"), ("b", bare (ExplicitId "y") "ring")]
            [Just r0, Just r1] ← pure (ibTrees b)
            let ctx = CaptureCtx (const Nothing) (const Set.empty)
                owner suffix = T.replicate 700 "o" <> suffix
                cap = snd (captureNodes ctx emptyCaptureState
                    [(owner ".one", r0), (owner ".two", r1)])
                names = map (scenarioIdText ∘ ieId) (capEntries cap)
            map T.length names `shouldSatisfy` all (≤ maxItemRefLength)
            length (nub names) `shouldBe` 2
            map itemRefTextError names `shouldBe` [Nothing, Nothing]
            -- and the same arena captures to the same names
            map (scenarioIdText ∘ ieId) (capEntries (snd (captureNodes ctx emptyCaptureState
                [(owner ".one", r0), (owner ".two", r1)]))) `shouldBe` names

        it "an authored id of any admitted length is preserved verbatim through \
           \materialize, capture, encode and decode" $
            forM_ [maxItemRefLength - 12 .. maxItemRefLength] $ \n → do
                let name = T.replicate n "a"
                (decoded, ds) ← decodeDoc $ BC.pack $ unlines
                    [ "version: 1", "ground_items:"
                    , "  - {id: " ⧺ T.unpack name ⧺ ", definition: ring, x: 0, y: 0}" ]
                ds `shouldBe` []
                (env, _) ← newEnv defs 0
                (placements, gb) ← buildGroundItems env (scGroundItems decoded)
                ibDiagnostics gb `shouldBe` []
                let (store, _) = installGroundItems (catMaybes placements) emptyGroundItems
                    ctx = ctxOf (ibRefs gb) []
                    (_, rows, _) = captureGroundItems ctx emptyCaptureState store
                    doc = A.object [ "version" A..= (1 ∷ Int)
                        , "ground_items" A..= [ A.object (("x", A.toJSON x) : ("y", A.toJSON y) : itemEntryFields e)
                                              | (x, y, e) ← rows ] ]
                map (\(_, _, e) → ieId e) rows `shouldBe` [ExplicitId name]
                (back, ds2) ← decodeDoc (Y.encode doc)
                ds2 `shouldBe` []
                map (ieId ∘ giItem) (scGroundItems back) `shouldBe` [ExplicitId name]

        it "ground rows that differ only in tags or references keep their own \
           \identity however they were laid down" $ do
            let mk i d = do
                    (env, _) ← newEnv defs i
                    [Just it'] ← ibTrees ⊚ buildItemForest env [("g", bare (ExplicitId d) "ring")]
                    pure it'
            ra ← mk (0 ∷ Word64) "scratch-a"
            rb ← mk 100 "scratch-b"
            let lay xs = fst (installGroundItems [ GroundPlacement 1 1 i | i ← xs ] emptyGroundItems)
                tagsOf = [ (iiInstanceId ra, Set.fromList [tg "alpha"])
                         , (iiInstanceId rb, Set.fromList [tg "beta"]) ]
                ctx = CaptureCtx (const Nothing) (\i → fromMaybe Set.empty (lookup i tagsOf))
                capture g = let (_, rows, _) = captureGroundItems ctx emptyCaptureState g in rows
            capture (lay [ra, rb]) `shouldBe` capture (lay [rb, ra])
            -- a tag may contain any character: ["alpha,beta"] and ["alpha", "beta"]
            -- are different sets and must not tie
            let tagsB = [ (iiInstanceId ra, Set.fromList [tg "alpha,beta"])
                        , (iiInstanceId rb, Set.fromList [tg "alpha", tg "beta"]) ]
                ctxB = CaptureCtx (const Nothing) (\i → fromMaybe Set.empty (lookup i tagsB))
                captureB g = let (_, rows, _) = captureGroundItems ctxB emptyCaptureState g in rows
            captureB (lay [ra, rb]) `shouldBe` captureB (lay [rb, ra])
            -- and the tags really did travel with the item that held them
            [ (ieId e, ieTags e) | (_, _, e) ← capture (lay [rb, ra]) ]
                `shouldBe` [ (ExplicitId "ground.0", [tg "alpha"]), (ExplicitId "ground.1", [tg "beta"]) ]

    describe "ordinary-save identity metadata" $ do

        it "core-session v3 carries the references through the real codec; v2 and v1 \
           \payloads decode with none" $ do
            let refs = itemRefsFromList [(11, "kit-1"), (12, "units[0].inventory[0]"), (40, "relic")]
                snap = skeleton { snapItemRefs = refs }
                bytes = ccEncode coreSessionCodec snap
            fmap (\dto → snapItemRefs (applyCoreSession dto skeleton)) (ccDecode coreSessionCodec 3 bytes)
                `shouldSatisfy` either (const False) (≡ refs)
            let v2 = CoreSessionDTOv2
                    { cs2GameTime = 0, cs2NextItemId = 5, cs2NextBuildingId = 1, cs2NextUnitId = 1
                    , cs2ActivePage = pageP, cs2VisiblePages = [pageP]
                    , cs2LiveCamera = LiveCameraDTO (Just pageP) 0 0 1 FaceSouth
                    , cs2ItemTags = [(9, ["tagged"])] }
            case ccDecode coreSessionCodec 2 (S.encode v2) of
                Left e → expectationFailure (show e)
                Right dto → do
                    snapItemRefs (applyCoreSession dto skeleton) `shouldBe` emptyItemRefs
                    Map.size (snapItemTags (applyCoreSession dto skeleton)) `shouldBe` 1
            let v1 = CoreSessionDTOv1
                    { cs1GameTime = 0, cs1NextItemId = 5, cs1NextBuildingId = 1, cs1NextUnitId = 1
                    , cs1ActivePage = pageP, cs1VisiblePages = [pageP]
                    , cs1LiveCamera = LiveCameraDTO (Just pageP) 0 0 1 FaceSouth }
            fmap (\dto → snapItemRefs (applyCoreSession dto skeleton)) (ccDecode coreSessionCodec 1 (S.encode v1))
                `shouldSatisfy` either (const False) (≡ emptyItemRefs)

        it "the wire validator refuses id 0, a malformed reference, a repeated id and a \
           \repeated reference" $ do
            let base = fromRight' (ccDecode coreSessionCodec 3 (ccEncode coreSessionCodec skeleton))
                dtoWith rs = base { csItemRefs = rs }
                problems rs = map ceMessage (ccValidate coreSessionCodec (dtoWith rs))
            problems [(5, "ok")] `shouldBe` []
            problems [(0, "zero")] `shouldSatisfy` any ("instance id 0" `T.isInfixOf`)
            problems [(5, "")] `shouldSatisfy` any ("empty scenario reference" `T.isInfixOf`)
            problems [(5, "has space")] `shouldSatisfy` any ("outside the scenario id vocabulary" `T.isInfixOf`)
            problems [(5, "a"), (5, "b")] `shouldSatisfy` any ("listed twice" `T.isInfixOf`)
            problems [(5, "a"), (6, "a")] `shouldSatisfy` any ("claimed by more than one" `T.isInfixOf`)

        it "the mapping is injective however it is built" $ do
            itemRefsToAscList (itemRefsFromList [(2, "a"), (1, "a"), (3, "b")])
                `shouldBe` [(1, "a"), (3, "b")]
            itemRefsToAscList (itemRefsFromList [(0, "zero")]) `shouldBe` []
  where
    skeleton = SessionSnapshot
        { snapGameTime = 1, snapTexPalette = emptyTexPalette, snapNextItemId = 1
        , snapNextBuildingId = 1, snapNextUnitId = 1, snapActivePage = pageP
        , snapVisiblePages = [], snapLiveCamera = LiveCameraSnapshot Nothing 0 0 1 FaceSouth
        , snapPages = HM.empty, snapItemTags = mempty, snapItemRefs = mempty
        , snapPortableKnowledge = emptyPortableKnowledge }
    fromRight' = either (error ∘ show) id

-- * Small helpers

-- | A bare unit holding the given inventory.
unitWithInventory ∷ [ItemEntry] → UnitEntry
unitWithInventory inv = UnitEntry
    { ueId = ExplicitId "u", ueTags = [], ueDefinition = "acolyte", ueX = 0, ueY = 0
    , ueZ = Omitted, ueName = Omitted, ueFacing = Omitted, ueEncounter = Nothing
    , ueStats = HM.empty, ueSkills = HM.empty, ueKnowledge = Omitted
    , ueModifiers = Omitted, ueWounds = Omitted, ueScars = Omitted
    , ueBlood = Omitted, ueImmuneResponse = Omitted, ueImmunities = Omitted
    , ueInventory = Authored inv, ueEquipment = Omitted, ueAccessories = Omitted }

fromJust' ∷ Maybe α → α
fromJust' = fromMaybe (error "fromJust'")

isRight' ∷ Either α β → Bool
isRight' = either (const False) (const True)

nullCategoryIndex' ∷ CategoryIndex ItemTarget → Bool
nullCategoryIndex' = nullCategoryIndex

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }
