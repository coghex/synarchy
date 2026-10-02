-- | The "Scenario.Schema" gate (#2699, epic #2698 SCN-01): the versioned
--   scenario YAML format, decoded and validated with no engine, no world
--   and no GPU.
--
--   Every fixture is inline YAML written to a scratch file and read back
--   through the real 'loadScenarioFile'. Expectations are the complete
--   validated structures and the exact diagnostics, never just a
--   success flag: a diagnostic list that merely "contains a warning"
--   would pass a test of a different scenario (D-15).
--
--   The v1 fixtures below are FROZEN. When a later format version lands
--   they stay byte-for-byte as they are and must keep decoding to the
--   same content through the migration chain (D-44).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Schema"'@.
module Test.Headless.Scenario.Schema (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import System.FilePath ((</>))
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Unit.Direction (Direction(..))
import Scenario.Types
import Scenario.Bounds
import Scenario.Schema
import Scenario.Validate (fallbackStatNames, fallbackSkillNames)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- * Catalog fixture

acolyteEntry ∷ UnitCatalogEntry
acolyteEntry = UnitCatalogEntry
    { ucStats = HM.fromList
        [ ("strength", AuthorableStat), ("endurance", AuthorableStat)
        , ("carrying_capacity", DerivedStat) ]
    , ucSkills = HS.fromList ["dagger", "mining"]
    , ucBodyParts = HS.fromList ["head", "torso", "left_leg"]
    , ucEquipmentSlots = HS.fromList ["main_hand", "back"]
    }

catalog ∷ ScenarioCatalog
catalog = emptyScenarioCatalog
    { catUnits = HM.fromList [("acolyte", acolyteEntry)]
    , catItems = HM.fromList
        [ ("canteen", ItemCatalogEntry (Just 1.0) False)
        , ("knife", ItemCatalogEntry Nothing False)
        , ("bandage", ItemCatalogEntry Nothing False)
        , ("steel_plate", ItemCatalogEntry Nothing False)
        , ("first_aid_kit", ItemCatalogEntry Nothing True) ]
    , catBuildings = HM.fromList
        [ ("cargo_hold", BuildingCatalogEntry (Footprint 0 0 1 1) 10
                            (HS.fromList ["steel_plate"]) Nothing)
        , ("battery", BuildingCatalogEntry (Footprint 0 0 0 0) 5 HS.empty (Just 100)) ]
    , catLocations = HM.fromList
        [ ("ruin", LocationCatalogEntry (Footprint (-2) (-2) 2 2) 2) ]
    , catFlora = HS.fromList ["pine"]
    , catMaterials = HS.fromList ["granite", "loam"]
    , catStructurePacks = HM.fromList
        [ ("dungeon_1", HS.fromList ["floor", "ceiling", "wall", "post"]) ]
    , catInfections = HS.fromList ["staph"]
    , catKnowledge = HS.fromList ["bleed_control", "basic_cuisine"]
    }

-- * Harness

withScenarioFile ∷ String → (FilePath → IO α) → IO α
withScenarioFile contents action =
    withExclusiveTempDirectory "synarchy-scenario-schema-spec" $ \dir → do
        let path = dir </> "scenario.yaml"
        writeFile path contents
        action path

loadWith ∷ ScenarioCatalog → String → IO ScenarioOutcome
loadWith cat contents = withScenarioFile contents (loadScenarioFile cat)

load ∷ String → IO ScenarioOutcome
load = loadWith catalog

loaded ∷ ScenarioOutcome → IO (Scenario, [ScenarioDiagnostic])
loaded (ScenarioLoaded s ds) = pure (s, ds)
loaded (ScenarioFailed f)    = expectationFailure ("unexpected failure: " ⧺ show f)
                             ≫ error "unreachable"

failed ∷ ScenarioOutcome → IO ScenarioFailure
failed (ScenarioFailed f) = pure f
failed other = expectationFailure ("expected a failure, got " ⧺ show other)
             ≫ error "unreachable"

tag ∷ Text → GameplayTag
tag t = fromMaybe (error "empty fixture tag") (mkGameplayTag t)

d ∷ Text → DiagnosticReason → DiagnosticEffect → ScenarioDiagnostic
d = ScenarioDiagnostic

shouldHaveDiagnostics ∷ [ScenarioDiagnostic] → [ScenarioDiagnostic] → Expectation
shouldHaveDiagnostics got want = got `shouldBe` sortOn sdPath want

emptyScenario ∷ Scenario
emptyScenario = Scenario 1 Nothing (0, 0) [] [] [] [] [] [] [] []

-- | A bare item: definition only, everything else omitted.
item ∷ ScenarioId → Text → ItemEntry
item i defn = ItemEntry i [] defn Omitted Omitted Omitted Omitted Omitted
                        Omitted Omitted Omitted

-- | A bare unit at a position.
unit ∷ ScenarioId → Float → Float → UnitEntry
unit i x y = UnitEntry
    { ueId = i, ueTags = [], ueDefinition = "acolyte", ueX = x, ueY = y
    , ueZ = Omitted, ueName = Omitted, ueFacing = Omitted, ueEncounter = Nothing
    , ueStats = HM.empty, ueSkills = HM.empty, ueKnowledge = Omitted
    , ueModifiers = Omitted, ueWounds = Omitted, ueScars = Omitted
    , ueBlood = Omitted, ueImmuneResponse = Omitted, ueImmunities = Omitted
    , ueInventory = Omitted, ueEquipment = Omitted, ueAccessories = Omitted }

wound ∷ Text → Text → Float → WoundSpec
wound part kind sev = WoundSpec part kind sev Omitted Omitted Omitted Omitted
                                Omitted Omitted Omitted Omitted Omitted

ex ∷ Text → ScenarioId
ex = ExplicitId

-- * Fixtures (frozen v1)

minimalV1 ∷ String
minimalV1 = "version: 1\n"

-- | Every content family and every supported field, explicitly.
fullV1 ∷ String
fullV1 = unlines
    [ "version: 1"
    , "map: {width: 20, height: 10}"
    , "camera: {x: 2.5, y: -1}"
    , "terrain:"
    , "  - id: floor"
    , "    tags: [arena-floor]"
    , "    rect: {x0: 9, y0: 5, x1: -10, y1: -4}"
    , "    material: granite"
    , "    surface_z: 3"
    , "    slope: 0"
    , "fluids:"
    , "  - id: pond"
    , "    tiles: [[1, 1], [2, 1], [1, 1]]"
    , "    fluid: lake"
    , "    surface_z: 4.5"
    , "flora:"
    , "  - {id: tree-1, species: pine, x: -3, y: 2, z: 4, age: 120, health: 0.75}"
    , "structures:"
    , "  - {id: wall-1, pack: dungeon_1, piece: wall_ne, x: 0, y: 0, z: 4}"
    , "buildings:"
    , "  - id: hold"
    , "    definition: cargo_hold"
    , "    x: 4"
    , "    y: 1"
    , "    z: 4"
    , "    build_progress: 2.5"
    , "    materials_delivered:"
    , "      - {id: plate-1, definition: steel_plate}"
    , "    storage:"
    , "      - {id: stored-canteen, definition: canteen, fill: 0}"
    , "  - {id: bank, definition: battery, x: -6, y: -2, power_charge: 40}"
    , "locations:"
    , "  - id: ruin-1"
    , "    definition: ruin"
    , "    x: -5"
    , "    y: 2"
    , "    significant_items: {\"1\": relic}"
    , "units:"
    , "  - id: scout"
    , "    tags: [escort, scout]"
    , "    definition: acolyte"
    , "    x: 0.5"
    , "    y: -1.25"
    , "    z: 4"
    , "    name: Ama Oru"
    , "    facing: north-east"
    , "    encounter: ruin-1"
    , "    stats: {strength: 12, endurance: 0}"
    , "    skills: {dagger: 55}"
    , "    knowledge: {bleed_control: 40}"
    , "    modifiers:"
    , "      strength:"
    , "        - {source: poison-A, delta: -2, remaining: 30}"
    , "        - {source: age, percent: -0.25}"
    , "    wounds:"
    , "      - {part: left_leg, kind: slash, severity: 0.5, age: 60, bandage: 0.05,"
    , "         clot: 0.5, heal: 0.25, dressing: bandage, infection: 0.125,"
    , "         clean: false, infection_type: staph, necrosis: 0}"
    , "    scars:"
    , "      - {part: head, kind: blunt, severity: 0.25, age: 3600}"
    , "    blood: 4.5"
    , "    immune_response: 0.25"
    , "    immunities: {staph: 0.5}"
    , "    inventory:"
    , "      - id: kit"
    , "        definition: first_aid_kit"
    , "        contents:"
    , "          - {id: kit-bandage, definition: bandage, condition: 100}"
    , "      - {id: canteen-1, definition: canteen, fill: 0.5, quality: 80,"
    , "         condition: 90, weight: 0.25, bulk: 1.5, temperature: ambient}"
    , "    equipment:"
    , "      main_hand: {id: knife-1, definition: knife, sharpness: 15,"
    , "                  condition: 5, temperature: 60}"
    , "    accessories: []"
    , "ground_items:"
    , "  - {id: relic, definition: knife, x: -5, y: 2.5, quality: 100}"
    ]

fullExpected ∷ Scenario
fullExpected = Scenario
    { scSourceVersion = 1
    , scMap = Just (MapDimensions 20 10)
    , scCamera = (2.5, -1)
    , scTerrain =
        [ TerrainPatch (ex "floor") [tag "arena-floor"] (RegionRect (-10) (-4) 9 5)
                       "granite" 3 (Authored 0) ]
    , scFluids =
        [ FluidPatch (ex "pond") [] (RegionTiles [(1, 1), (2, 1)]) FluidLake 36 ]
    , scFlora =
        [ FloraEntry (ex "tree-1") [] "pine" (-3) 2 (Authored 4) (Authored 120)
                     (Authored 0.75) ]
    , scStructures =
        [ StructurePiece (ex "wall-1") [] "dungeon_1" "wall_ne" 0 0 (Authored 4) ]
    , scBuildings =
        [ BuildingEntry (ex "hold") [] "cargo_hold" 4 1 (Authored 4)
            (Authored [ (item (ex "stored-canteen") "canteen") { ieFill = Authored 0 } ])
            (Authored 2.5)
            (Authored [ item (ex "plate-1") "steel_plate" ])
            Omitted
        , BuildingEntry (ex "bank") [] "battery" (-6) (-2) Omitted Omitted Omitted
            Omitted (Authored 40) ]
    , scLocations =
        [ LocationEntry (ex "ruin-1") [] "ruin" (-5) 2
            (HM.fromList [(1, ex "relic")]) ]
    , scUnits =
        [ (unit (ex "scout") 0.5 (-1.25))
            { ueTags = [tag "escort", tag "scout"]
            , ueZ = Authored 4
            , ueName = Authored "Ama Oru"
            , ueFacing = Authored DirNE
            , ueEncounter = Just (ex "ruin-1")
            , ueStats = HM.fromList [("strength", 12), ("endurance", 0)]
            , ueSkills = HM.fromList [("dagger", 55)]
            , ueKnowledge = Authored (HM.fromList [("bleed_control", 40)])
            , ueModifiers = Authored (HM.fromList
                [ ("strength", [ ModifierSpec "poison-A" (-2) 0 (Just 30)
                               , ModifierSpec "age" 0 (-0.25) Nothing ]) ])
            , ueWounds = Authored
                [ WoundSpec "left_leg" "slash" 0.5 (Authored 60) (Authored 0.05)
                    (Authored 0.5) (Authored 0.25) (Authored "bandage")
                    (Authored 0.125) (Authored False) (Authored "staph")
                    (Authored 0) ]
            , ueScars = Authored [ ScarSpec "head" "blunt" 0.25 (Authored 3600) ]
            , ueBlood = Authored 4.5
            , ueImmuneResponse = Authored 0.25
            , ueImmunities = Authored (HM.fromList [("staph", 0.5)])
            , ueInventory = Authored
                [ (item (ex "kit") "first_aid_kit")
                    { ieContents = Authored
                        [ (item (ex "kit-bandage") "bandage") { ieCondition = Authored 100 } ] }
                , (item (ex "canteen-1") "canteen")
                    { ieFill = Authored 0.5, ieQuality = Authored 80
                    , ieCondition = Authored 90, ieWeight = Authored 0.25
                    , ieBulk = Authored 1.5, ieTemperature = Authored AtAmbient } ]
            , ueEquipment = Authored (HM.fromList
                [ ("main_hand", (item (ex "knife-1") "knife")
                    { ieSharpness = Authored 15, ieCondition = Authored 5
                    , ieTemperature = Authored (TrackedTemp 60) }) ])
            , ueAccessories = Authored [] } ]
    , scGroundItems =
        [ GroundItemEntry (-5) 2.5 ((item (ex "relic") "knife") { ieQuality = Authored 100 }) ]
    }

-- | Recoverable problems inside a parsed v1 document.
recoverableV1 ∷ String
recoverableV1 = unlines
    [ "version: 1"
    , "units:"
    , "  - id: ghost"
    , "    definition: wraith"
    , "    x: 0"
    , "    y: 0"
    , "    blood: -1"
    , "    inventory:"
    , "      - id: ghost-kit"
    , "        definition: first_aid_kit"
    , "        contents: [{definition: bandage}]"
    , "  - id: keeper"
    , "    definition: acolyte"
    , "    x: 1"
    , "    y: 1"
    , "    colour: red"
    , "    tags: [ok, \"\"]"
    , "    stats: {strength: 9, carrying_capacity: 40, luck: 3, endurance: high}"
    , "    blood: -1"
    , "    wounds:"
    , "      - {part: tail, kind: slash, severity: 0.5}"
    , "      - {part: head, kind: blunt, severity: 0.5}"
    , "      - {part: head, kind: stab, severity: 0.25, clot: 2}"
    , "    equipment:"
    , "      cape: {definition: knife, contents: [{id: cape-pin, definition: knife}]}"
    , "    inventory:"
    , "      - id: broken-kit"
    , "        definition: mystery_box"
    , "        contents: [{id: inner, definition: bandage}]"
    , "      - {definition: knife, fill: 1}"
    , "  - id: \"bad id!\""
    , "    definition: acolyte"
    , "    x: 2"
    , "    y: 2"
    ]

-- | Required references and their transitive rejection.
referencesV1 ∷ String
referencesV1 = unlines
    [ "version: 1"
    , "locations:"
    , "  - {id: lost-ruin, definition: atlantis, x: 0, y: 0}"
    , "units:"
    , "  - id: guard-a"
    , "    definition: acolyte"
    , "    x: 0"
    , "    y: 0"
    , "    encounter: lost-ruin"
    , "    inventory: [{id: guard-knife, definition: knife}]"
    , "  - {id: guard-b, definition: acolyte, x: 0, y: 0, encounter: nowhere}"
    , "  - {id: guard-c, definition: acolyte, x: 0, y: 0, encounter: guard-d}"
    , "  - {id: guard-d, definition: acolyte, x: 0, y: 0}"
    , "  - {id: twin, definition: acolyte, x: 0, y: 0}"
    , "  - {id: twin, definition: acolyte, x: 1, y: 0}"
    ]

-- | Optional significant-item bindings.
bindingsV1 ∷ String
bindingsV1 = unlines
    [ "version: 1"
    , "locations:"
    , "  - {id: ruin-a, definition: ruin, x: 0, y: 0,"
    , "     significant_items: {\"1\": gone, \"2\": doomed-relic}}"
    , "  - {id: ruin-b, definition: ruin, x: 0, y: 0,"
    , "     significant_items: {\"1\": shared, \"3\": shared}}"
    , "  - {id: ruin-c, definition: ruin, x: 0, y: 0, significant_items: {\"1\": shared}}"
    , "units:"
    , "  - {id: doomed, definition: wraith, x: 0, y: 0,"
    , "     inventory: [{id: doomed-relic, definition: knife}]}"
    , "ground_items:"
    , "  - {id: shared, definition: knife, x: 0, y: 0}"
    ]

boundsV1 ∷ String
boundsV1 = unlines
    [ "version: 1"
    , "map: {width: 20, height: 10}"
    , "terrain:"
    , "  - {id: wide, rect: {x0: 5, y0: 0, x1: 14, y1: 0}, material: granite, surface_z: 1}"
    , "  - {id: offmap, tiles: [[30, 0], [31, 0]], material: granite, surface_z: 1}"
    , "fluids:"
    , "  - {id: edge, tiles: [[9, 5], [10, 5], [9, 6]], fluid: lake, surface_z: 2}"
    , "flora:"
    , "  - {id: far-tree, species: pine, x: 0, y: 6}"
    , "buildings:"
    , "  - {id: crossing, definition: cargo_hold, x: 9, y: 0,"
    , "     storage: [{id: cargo, definition: knife}]}"
    , "  - {id: fits, definition: cargo_hold, x: 8, y: 4}"
    , "locations:"
    , "  - {id: edge-ruin, definition: ruin, x: 8, y: 0}"
    , "units:"
    , "  - {id: inside, definition: acolyte, x: 9.4, y: -4.5}"
    , "  - {id: outside, definition: acolyte, x: 9.5, y: 0}"
    , "ground_items:"
    , "  - {id: far, definition: knife, x: -11, y: 0}"
    ]

-- * Spec

spec ∷ Spec
spec = do
    boundsSpec
    decodeSpec
    recoverySpec
    referenceSpec
    overrideSpec
    contentBoundsSpec
    failureSpec
    preservationSpec

boundsSpec ∷ Spec
boundsSpec = describe "finite map bounds" $ do
    let rect w h = mapTileBounds (MapDimensions w h)
    it "pins the five exact rectangles with their tile counts" $ do
        rect 20 10 `shouldBe` Just (TileBounds (-10) (-4) 9 5)
        rect 21 11 `shouldBe` Just (TileBounds (-10) (-5) 10 5)
        rect 20 11 `shouldBe` Just (TileBounds (-10) (-5) 9 5)
        rect 21 10 `shouldBe` Just (TileBounds (-10) (-4) 10 5)
        rect 1 1   `shouldBe` Just (TileBounds 0 0 0 0)
        map (fmap boundsTileCount ∘ uncurry rect)
            [(20, 10), (21, 11), (20, 11), (21, 10), (1, 1)]
            `shouldBe` map Just [200, 231, 220, 210, 1]
    it "contains exactly width × height tiles, enumerated" $
        forM_ [(20, 10), (21, 11), (20, 11), (21, 10), (1, 1), (2, 3)] $ \(w, h) → do
            let tiles = [ (x, y) | Just b ← [rect w h]
                                 , x ← [-30 .. 30], y ← [-30 .. 30], inBounds b (x, y) ]
            toInteger (length tiles) `shouldBe` toInteger w * toInteger h
    it "refuses dimensions outside the schema's domain" $ do
        rect 0 10 `shouldBe` Nothing
        rect 10 (-1) `shouldBe` Nothing
        rect (maxScenarioDimension + 1) 1 `shouldBe` Nothing
        fmap boundsTileCount (rect maxScenarioDimension maxScenarioDimension)
            `shouldBe` Just (toInteger maxScenarioDimension ^ (2 ∷ Int))

decodeSpec ∷ Spec
decodeSpec = describe "v1 decoding" $ do
    it "decodes the minimal fixture to an empty, expandable scenario" $ do
        (s, ds) ← loaded =≪ load minimalV1
        s `shouldBe` emptyScenario
        ds `shouldBe` []
    it "decodes the fully explicit fixture to the expected typed content" $ do
        (s, ds) ← loaded =≪ load fullV1
        ds `shouldBe` []
        s `shouldBe` fullExpected
    it "keeps omitted fields apart from explicit zero and empty values" $ do
        (s, ds) ← loaded =≪ load (unlines
            [ "version: 1"
            , "units:"
            , "  - {id: plain, definition: acolyte, x: 0, y: 0}"
            , "  - id: explicit"
            , "    definition: acolyte"
            , "    x: 0"
            , "    y: 0"
            , "    name: \"\""
            , "    stats: {strength: 0}"
            , "    knowledge: {}"
            , "    modifiers: {}"
            , "    wounds: []"
            , "    scars: []"
            , "    blood: 0"
            , "    immunities: {}"
            , "    inventory: [{id: empty-kit, definition: first_aid_kit, contents: [],"
            , "                 temperature: 0}]"
            , "    equipment: {}"
            , "    accessories: []"
            , "ground_items:"
            , "  - {id: dry, definition: canteen, x: 0, y: 0, fill: 0, quality: 0}"
            ])
        ds `shouldBe` []
        scUnits s `shouldBe`
            [ unit (ex "plain") 0 0
            , (unit (ex "explicit") 0 0)
                { ueName = Authored "", ueStats = HM.fromList [("strength", 0)]
                , ueKnowledge = Authored HM.empty, ueModifiers = Authored HM.empty
                , ueWounds = Authored [], ueScars = Authored [], ueBlood = Authored 0
                , ueImmunities = Authored HM.empty
                , ueInventory = Authored
                    [ (item (ex "empty-kit") "first_aid_kit")
                        { ieContents = Authored [], ieTemperature = Authored (TrackedTemp 0) } ]
                , ueEquipment = Authored HM.empty, ueAccessories = Authored [] } ]
        scGroundItems s `shouldBe`
            [ GroundItemEntry 0 0 ((item (ex "dry") "canteen")
                { ieFill = Authored 0, ieQuality = Authored 0 }) ]
    it "assigns automatic path identities that tags never change" $ do
        (s, _) ← loaded =≪ load (unlines
            [ "version: 1"
            , "units:"
            , "  - {definition: acolyte, x: 0, y: 0, tags: [a, b, a]}"
            , "  - {definition: acolyte, x: 0, y: 0, inventory: [{definition: knife}]}"
            ])
        map ueId (scUnits s) `shouldBe` [AutoId "units[0]", AutoId "units[1]"]
        map ueTags (scUnits s) `shouldBe` [[tag "a", tag "b", tag "a"], []]
        fmap (map ieId) (ueInventory (scUnits s !! 1))
            `shouldBe` Authored [AutoId "units[1].inventory[0]"]

recoverySpec ∷ Spec
recoverySpec = describe "recoverable content" $ do
    it "rejects only the affected entries and fields, with exact diagnostics" $ do
        (s, ds) ← loaded =≪ load recoverableV1
        ds `shouldHaveDiagnostics`
            [ d "units[0].definition" (UnknownDefinition "wraith") EntryRejected
            , d "units[0].inventory[0]" (OwnerRejected "units[0]") CascadeRejected
            , d "units[0].inventory[0].contents[0]" (OwnerRejected "units[0]") CascadeRejected
            , d "units[1].colour" UnknownField FieldRejected
            , d "units[1].tags[1]" EmptyTag FieldRejected
            , d "units[1].stats.carrying_capacity" DerivedValue FieldRejected
            , d "units[1].stats.endurance" (InvalidValue "a finite number") FieldRejected
            , d "units[1].stats.luck" UnknownField FieldRejected
            , d "units[1].blood" (InvalidValue "a number ≥ 0.0") FieldRejected
            , d "units[1].wounds[0].part" (UnknownDefinition "tail") EntryRejected
            , d "units[1].wounds[1].severity" (InvalidValue "a number in [0.0, 0.4]") EntryRejected
            , d "units[1].wounds[2].clot" (InvalidValue "a number in [0.0, 1.0]") FieldRejected
            , d "units[1].equipment.cape" UnknownField FieldRejected
            , d "units[1].equipment.cape.contents[0]"
                (OwnerRejected "units[1].equipment.cape") CascadeRejected
            , d "units[1].inventory[0].definition" (UnknownDefinition "mystery_box") EntryRejected
            , d "units[1].inventory[0].contents[0]"
                (OwnerRejected "units[1].inventory[0]") CascadeRejected
            , d "units[1].inventory[1].fill" (InvalidValue "0: this item holds no fluid") FieldRejected
            , d "units[2].id"
                (InvalidValue "an id of letters, digits, '_', '-', ':' or '.'") FieldRejected
            ]
        scUnits s `shouldBe`
            [ (unit (ex "keeper") 1 1)
                { ueTags = [tag "ok"]
                , ueStats = HM.fromList [("strength", 9)]
                , ueWounds = Authored [ wound "head" "stab" 0.25 ]
                , ueEquipment = Authored HM.empty
                , ueInventory = Authored [ item (AutoId "units[1].inventory[1]") "knife" ] }
            , unit (AutoId "units[2]") 2 2 ]

    it "checks unit and building state overrides field by field" $ do
        (s, ds) ← loaded =≪ load (unlines
            [ "version: 1"
            , "buildings:"
            , "  - {id: hold, definition: cargo_hold, x: 0, y: 0, build_progress: 11,"
            , "     power_charge: 5,"
            , "     materials_delivered: [{id: wrong, definition: knife},"
            , "                           {definition: steel_plate}]}"
            , "  - {id: bank, definition: battery, x: 3, y: 3, power_charge: 101}"
            , "units:"
            , "  - id: u"
            , "    definition: acolyte"
            , "    x: 0"
            , "    y: 0"
            , "    facing: up"
            , "    knowledge: {necromancy: 5, bleed_control: -1, basic_cuisine: 10}"
            , "    skills: {dagger: 30, flying: 2}"
            , "    modifiers:"
            , "      strength: [{source: a, remaining: 0}, {delta: 1},"
            , "                 {source: b, delta: 1, expires: 5}]"
            , "      mining: {source: c}"
            , "      luck: [{source: d}]"
            , "    immunities: {plague: 0.5, staph: 2}"
            ])
        ds `shouldHaveDiagnostics`
            [ d "buildings[0].build_progress" (InvalidValue "a number in [0.0, 10.0]") FieldRejected
            , d "buildings[0].materials_delivered[0].definition"
                (InvalidValue "a material this building consumes") EntryRejected
            , d "buildings[0].power_charge"
                (InvalidValue "no charge: this building stores no power") FieldRejected
            , d "buildings[1].power_charge" (InvalidValue "a number in [0.0, 100.0]") FieldRejected
            , d "units[0].facing"
                (InvalidValue "a compass direction (south, north-east, sw, …)") FieldRejected
            , d "units[0].immunities.plague" (UnknownDefinition "plague") FieldRejected
            , d "units[0].immunities.staph" (InvalidValue "a number in [0.0, 1.0]") FieldRejected
            , d "units[0].knowledge.bleed_control" (InvalidValue "a number ≥ 0.0") FieldRejected
            , d "units[0].knowledge.necromancy" UnknownField FieldRejected
            , d "units[0].modifiers.luck" UnknownField FieldRejected
            , d "units[0].modifiers.mining" (InvalidValue "a list of modifiers") FieldRejected
            , d "units[0].modifiers.strength[0].remaining"
                (InvalidValue "a number of seconds > 0") EntryRejected
            , d "units[0].modifiers.strength[1].source" MissingRequired EntryRejected
            , d "units[0].modifiers.strength[2].expires" UnknownField FieldRejected
            , d "units[0].skills.flying" UnknownField FieldRejected
            ]
        scBuildings s `shouldBe`
            [ BuildingEntry (ex "hold") [] "cargo_hold" 0 0 Omitted Omitted Omitted
                (Authored [item (AutoId "buildings[0].materials_delivered[1]") "steel_plate"])
                Omitted
            , BuildingEntry (ex "bank") [] "battery" 3 3 Omitted Omitted Omitted Omitted Omitted ]
        scUnits s `shouldBe`
            [ (unit (ex "u") 0 0)
                { ueKnowledge = Authored (HM.fromList [("basic_cuisine", 10)])
                , ueSkills = HM.fromList [("dagger", 30)]
                , ueModifiers = Authored (HM.fromList
                    [("strength", [ModifierSpec "b" 1 0 Nothing])])
                , ueImmunities = Authored HM.empty } ]

referenceSpec ∷ Spec
referenceSpec = describe "identities and references" $ do
    it "rejects missing, rejected and wrong-kind required targets transitively" $ do
        (s, ds) ← loaded =≪ load referencesV1
        ds `shouldHaveDiagnostics`
            [ d "locations[0].definition" (UnknownDefinition "atlantis") EntryRejected
            , d "units[0].encounter" (RejectedReference "lost-ruin") CascadeRejected
            , d "units[0].inventory[0]" (OwnerRejected "units[0]") CascadeRejected
            , d "units[1].encounter" (MissingReference "nowhere") EntryRejected
            , d "units[2].encounter" (WrongReferenceKind "guard-d") EntryRejected
            , d "units[4].id" (DuplicateId "twin") EntryRejected
            , d "units[5].id" (DuplicateId "twin") EntryRejected
            ]
        scLocations s `shouldBe` []
        scUnits s `shouldBe` [unit (ex "guard-d") 0 0]
    it "drops only an invalid optional binding and keeps its location" $ do
        (s, ds) ← loaded =≪ load bindingsV1
        ds `shouldHaveDiagnostics`
            [ d "locations[0].significant_items.1" (MissingReference "gone") FieldRejected
            , d "locations[0].significant_items.2" (RejectedReference "doomed-relic") FieldRejected
            , d "locations[1].significant_items.1" (AmbiguousBinding "shared") FieldRejected
            , d "locations[1].significant_items.3"
                (InvalidValue "a significant-item slot in [1, 2]") FieldRejected
            , d "locations[2].significant_items.1" (AmbiguousBinding "shared") FieldRejected
            , d "units[0].definition" (UnknownDefinition "wraith") EntryRejected
            , d "units[0].inventory[0]" (OwnerRejected "units[0]") CascadeRejected
            ]
        scLocations s `shouldBe`
            [ LocationEntry (ex r) [] "ruin" 0 0 HM.empty | r ← ["ruin-a", "ruin-b", "ruin-c"] ]
        map (ieId ∘ giItem) (scGroundItems s) `shouldBe` [ex "shared"]
    it "resolves references independently of declaration order" $ do
        let locs = [ "locations:"
                   , "  - {id: ruin-1, definition: ruin, x: 0, y: 0,"
                   , "     significant_items: {\"2\": relic}}" ]
            us = [ "units:"
                 , "  - {id: a, definition: acolyte, x: 0, y: 0, encounter: ruin-1}"
                 , "  - {id: b, definition: acolyte, x: 1, y: 0, encounter: ruin-1}" ]
            usRev = [ "units:"
                    , "  - {id: b, definition: acolyte, x: 1, y: 0, encounter: ruin-1}"
                    , "  - {id: a, definition: acolyte, x: 0, y: 0, encounter: ruin-1}" ]
            ground = [ "ground_items:"
                     , "  - {id: relic, definition: knife, x: 0, y: 0}" ]
        (forward, dsF) ← loaded =≪ load (unlines ("version: 1" : locs ⧺ us ⧺ ground))
        (backward, dsB) ← loaded =≪ load (unlines ("version: 1" : ground ⧺ usRev ⧺ locs))
        dsF `shouldBe` []
        dsB `shouldBe` []
        let byId = sortOn (scenarioIdText ∘ ueId)
        byId (scUnits backward) `shouldBe` byId (scUnits forward)
        map ueEncounter (scUnits forward) `shouldBe` [Just (ex "ruin-1"), Just (ex "ruin-1")]
        scLocations backward `shouldBe` scLocations forward
        map leSignificant (scLocations forward) `shouldBe` [HM.fromList [(2, ex "relic")]]

overrideSpec ∷ Spec
overrideSpec = describe "explicit overrides across definition changes" $ do
    let doc = unlines
            [ "version: 1"
            , "units:"
            , "  - {id: u, definition: acolyte, x: 0, y: 0, stats: {strength: 12},"
            , "     skills: {dagger: 40}}" ]
        grown = catalog { catUnits = HM.fromList [("acolyte", acolyteEntry
            { ucStats = HM.insert "agility" AuthorableStat (ucStats acolyteEntry)
            , ucSkills = HS.insert "climbing" (ucSkills acolyteEntry) })] }
    it "keeps an explicit stat and leaves newly added stats fallback-eligible" $ do
        (s1, ds1) ← loaded =≪ loadWith catalog doc
        (s2, ds2) ← loaded =≪ loadWith grown doc
        ds1 `shouldBe` []
        ds2 `shouldBe` []
        map ueStats (scUnits s1) `shouldBe` [HM.fromList [("strength", 12)]]
        map ueStats (scUnits s2) `shouldBe` [HM.fromList [("strength", 12)]]
        map (fallbackStatNames acolyteEntry) (scUnits s1) `shouldBe` [["endurance"]]
        map (fallbackStatNames (catUnits grown HM.! "acolyte")) (scUnits s2)
            `shouldBe` [["agility", "endurance"]]
        map (fallbackSkillNames (catUnits grown HM.! "acolyte")) (scUnits s2)
            `shouldBe` [["climbing", "mining"]]
    it "applies the ordinary rejection rule to a removed definition" $ do
        (s, ds) ← loaded =≪ loadWith catalog { catUnits = HM.empty } doc
        ds `shouldBe` [d "units[0].definition" (UnknownDefinition "acolyte") EntryRejected]
        scUnits s `shouldBe` []

contentBoundsSpec ∷ Spec
contentBoundsSpec = describe "content against finite bounds" $ do
    it "clips patches by tile and rejects whole objects and footprints" $ do
        (s, ds) ← loaded =≪ load boundsV1
        ds `shouldHaveDiagnostics`
            [ d "buildings[0]" FootprintOutsideBounds EntryRejected
            , d "buildings[0].storage[0]" (OwnerRejected "buildings[0]") CascadeRejected
            , d "flora[0]" OutsideBounds EntryRejected
            , d "fluids[0]" (ClippedTiles 2) TilesClipped
            , d "ground_items[0]" OutsideBounds EntryRejected
            , d "locations[0]" FootprintOutsideBounds EntryRejected
            , d "terrain[0]" (ClippedTiles 5) TilesClipped
            , d "terrain[1]" OutsideBounds EntryRejected
            , d "units[1]" OutsideBounds EntryRejected
            ]
        map tpRegion (scTerrain s) `shouldBe` [RegionRect 5 0 9 0]
        map fpRegion (scFluids s) `shouldBe` [RegionTiles [(9, 5)]]
        map beId (scBuildings s) `shouldBe` [ex "fits"]
        map ueId (scUnits s) `shouldBe` [ex "inside"]
        scLocations s `shouldBe` []
        scGroundItems s `shouldBe` []
        scFlora s `shouldBe` []
    it "keeps everything when the map is the expandable arena" $ do
        let expandable = unlines (filter (≢ "map: {width: 20, height: 10}") (lines boundsV1))
        (s, ds) ← loaded =≪ load expandable
        ds `shouldBe` []
        scMap s `shouldBe` Nothing
        length (scTerrain s) `shouldBe` 2
        map ueId (scUnits s) `shouldBe` [ex "inside", ex "outside"]
    it "rejects an invalid map size as a field, falling back to the arena" $ do
        (s, ds) ← loaded =≪ load "version: 1\nmap: {width: 0, height: 10}\n"
        ds `shouldBe` [ d "map" (InvalidValue "exactly width and height, integers in [1, 100000]")
                          FieldRejected ]
        scMap s `shouldBe` Nothing

failureSpec ∷ Spec
failureSpec = describe "unsuccessful loads" $ do
    it "reports a missing file" $
        withExclusiveTempDirectory "synarchy-scenario-schema-spec" $ \dir → do
            f ← failed =≪ loadScenarioFile catalog (dir </> "absent.yaml")
            f `shouldSatisfy` \case ScenarioUnreadable _ → True; _ → False
    it "reports invalid YAML syntax, never an empty scenario" $ do
        f ← failed =≪ load "version: 1\nunits: [ {definition: acolyte\n"
        f `shouldSatisfy` \case ScenarioSyntaxError _ → True; _ → False
    it "refuses a document that is not a mapping" $
        (failed =≪ load "- version: 1\n") `shouldReturn` ScenarioNotAMapping
    it "refuses a missing or malformed version" $ do
        (failed =≪ load "units: []\n") `shouldReturn` ScenarioVersionMissing
        (failed =≪ load "version: one\n") `shouldReturn` ScenarioVersionMalformed "String \"one\""
        f ← failed =≪ load "version: 1.5\n"
        f `shouldSatisfy` \case ScenarioVersionMalformed _ → True; _ → False
    it "refuses unsupported versions without interpreting the content" $ do
        (failed =≪ load "version: 2\nunits: [{definition: acolyte, x: 0, y: 0}]\n")
            `shouldReturn` ScenarioVersionUnsupported 2
        (failed =≪ load "version: 0\n") `shouldReturn` ScenarioVersionUnsupported 0
    it "migrates a v1 document in memory before validating it" $ do
        let toV2 o = Right (KM.insert "version" (A.Number 2) o)
            v2 = ScenarioFormat 2 [(1, toV2)]
        (s, ds) ← loaded =≪ withScenarioFile fullV1 (loadScenarioFileWith v2 catalog)
        ds `shouldBe` []
        s `shouldBe` fullExpected
    it "reports a failed migration as unsuccessful" $ do
        let v2 = ScenarioFormat 2 [(1, \_ → Left "boom")]
        f ← failed =≪ withScenarioFile fullV1 (loadScenarioFileWith v2 catalog)
        f `shouldBe` ScenarioMigrationFailed 1 "boom"
    it "refuses a version whose migration chain has a gap" $ do
        let v3 = ScenarioFormat 3 [(2, Right)]
        f ← failed =≪ withScenarioFile fullV1 (loadScenarioFileWith v3 catalog)
        f `shouldBe` ScenarioVersionUnsupported 1

preservationSpec ∷ Spec
preservationSpec = describe "source preservation" $
    it "never changes the source bytes, on success, failure or migration" $ do
        let v2 = ScenarioFormat 2 [(1, \o → Right (KM.insert "version" (A.Number 2) o))]
            cases =
                [ (fullV1, loadScenarioFile catalog)
                , (recoverableV1, loadScenarioFile catalog)
                , ("version: 1\nunits: [\n", loadScenarioFile catalog)
                , ("version: 9\n", loadScenarioFile catalog)
                , (fullV1, loadScenarioFileWith v2 catalog) ]
        forM_ cases $ \(contents, run) → withScenarioFile contents $ \path → do
            before ← BS.readFile path
            _ ← run path
            after ← BS.readFile path
            after `shouldBe` before
