-- | Recipe catalogue + craft execution tests (#325): the YAML schema
--   (required fields, defaults, fuel/knowledge options) and the pure
--   all-or-nothing consumption core that craft.execute wraps. The
--   engine-integrated path (Lua verb → unit inventory) is gated by
--   tools/craft_probe.py.
module Test.Headless.Craft.Execute (spec) where

import UPrelude
import Test.Hspec
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Craft.Execute (consumeIngredients, takeItemsByName, craftQuality)
import Craft.Types
import Engine.Asset.YamlRecipes
import Item.Types (ItemInstance(..))

-- | A minimal inventory instance — only iiDefName matters to the
--   consumption logic; ids distinguish instances in identity checks.
mkInst ∷ Text → Word64 → ItemInstance
mkInst name iid = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 1
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

-- | The dagger recipe shape shipped in data/recipes/basic.yaml.
daggerRecipe ∷ RecipeDef
daggerRecipe = RecipeDef
    { rdId        = "forge_steel_dagger"
    , rdName      = "Forge Steel Dagger"
    , rdStation   = "forge"
    , rdInputs    = [RecipeIngredient "steel_bar" 2]
    , rdFuel      = Nothing
    , rdWork      = 20
    , rdOutputs   = [RecipeIngredient "steel_dagger" 1]
    , rdKnowledge = Nothing
    , rdSkill     = Just "smithing"
    , rdRepairAxis = Nothing
    , rdOutputTemp = Nothing
    , rdPowerDraw  = 0
    }

-- | A fuelled variant whose fuel line repeats an input item.
fuelledRecipe ∷ RecipeDef
fuelledRecipe = daggerRecipe
    { rdId   = "fuelled"
    , rdFuel = Just (RecipeIngredient "steel_bar" 1)
    }

parseFile ∷ BS8.ByteString → Either Yaml.ParseException RecipeYamlFile
parseFile = Yaml.decodeEither'

-- | The diagnostic from a source that MUST fail to decode. Failing the
--   example here rather than returning an Either keeps each count case
--   to its four field assertions.
parseFailureMessage ∷ BS8.ByteString → IO String
parseFailureMessage src = case parseFile src of
    Left err → pure (show err)
    Right f  → do
        expectationFailure $
            "expected a parse failure, decoded "
            <> show (length (ryfRecipes f)) <> " recipe(s)"
        pure ""

-- | A one-recipe file wrapped around the given entry body lines.
recipeSrc ∷ [String] → BS8.ByteString
recipeSrc entry = BS8.pack $ unlines ("recipes:" : entry)

-- | Requirements 1-3 promise four fields in the message. Each count
--   case asserts all four, so a rejection that drops one — the recipe
--   id in particular, which is the whole reason the check lives in
--   'RecipeYamlDef' rather than the ingredient parser — fails here.
--   The value is matched as @got <n>@ so it cannot be satisfied by the
--   index in aeson's own @$.recipes[0]@ path prefix.
shouldNameCountFields ∷ String → (String, String, String, Int) → Expectation
shouldNameCountFields msg (rid, kind, item, value) = do
    msg `shouldContain` rid
    msg `shouldContain` (kind <> " count")
    msg `shouldContain` item
    msg `shouldContain` ("got " <> show value)

spec ∷ Spec
spec = do
    describe "Craft.RecipeYaml" $ do
        it "parses the shipped data/recipes/basic.yaml" $ do
            r ← Yaml.decodeFileEither "data/recipes/basic.yaml"
            case r of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → do
                        ryId d `shouldBe` "forge_steel_dagger"
                        ryStation d `shouldBe` "forge"
                        ryWork d `shouldBe` 20
                        map ryiItem (ryInputs d) `shouldBe` ["steel_bar"]
                        map ryiCount (ryInputs d) `shouldBe` [2]
                        ryFuel d `shouldBe` Nothing
                        ryKnowledge d `shouldBe` Nothing
                        rySkill d `shouldBe` Just "smithing"
                        ryRepairAxis d `shouldBe` Nothing
                        map ryiItem (ryOutputs d) `shouldBe` ["steel_dagger"]
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "parses the shipped data/recipes/repair.yaml (#301)" $ do
            r ← Yaml.decodeFileEither "data/recipes/repair.yaml"
            case r of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [cond, sharp] → do
                        ryId cond `shouldBe` "repair_condition"
                        ryStation cond `shouldBe` "repair_condition"
                        ryRepairAxis cond `shouldBe` Just "condition"
                        map ryiItem (ryInputs cond) `shouldBe` ["lignite_chunk"]
                        ryOutputs cond `shouldBe` []
                        -- #1965: a repair visit is one synchronous
                        -- repair.repairAt call — no consumer burns
                        -- `work` down on the repair path, so the
                        -- shipped value must stay 0 rather than
                        -- advertising effort nothing spends. Only
                        -- craft recipes are operative here.
                        ryWork cond `shouldBe` 0
                        ryId sharp `shouldBe` "repair_sharpness"
                        ryStation sharp `shouldBe` "repair_sharpness"
                        ryRepairAxis sharp `shouldBe` Just "sharpness"
                        map ryiItem (ryInputs sharp) `shouldBe` ["whetstone"]
                        ryOutputs sharp `shouldBe` []
                        ryWork sharp `shouldBe` 0
                    ds → expectationFailure $
                        "expected exactly two recipes, got " <> show (length ds)

        it "repair_axis defaults to Nothing when omitted" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: plain_craft"
                    , "    station: forge"
                    , "    inputs: []"
                    , "    outputs: []"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → ryRepairAxis d `shouldBe` Nothing
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "rejects an invalid repair_axis instead of silently defaulting" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: broken_repair"
                    , "    station: repair_sharpness"
                    , "    repair_axis: sharpnes"  -- typo: not "sharpness"
                    , "    inputs: []"
                    , "    outputs: []"
                    ]
            case parseFile src of
                Left _  → pure ()
                Right _ → expectationFailure
                    "expected a parse failure for an invalid repair_axis"

        -- #1940: a `count` has no meaning at zero or below, and both
        -- ends absorbed one with SUCCESS semantics — a mistyped
        -- `count: 0` on an input consumed nothing while the output was
        -- still produced. Rejected in RecipeYamlDef's parser, beside
        -- the repair_axis check above, so the recipe id is in the
        -- message and the whole file's load fails.
        it "rejects a count: 0 input, naming recipe, kind, item and value" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: free_craft"
                    , "    station: forge"
                    , "    inputs:"
                    , "      - item: steel_bar"
                    , "        count: 0"
                    , "    outputs:"
                    , "      - item: steel_dagger"
                    ]
            shouldNameCountFields msg ("free_craft", "input", "steel_bar", 0)

        it "rejects a negative input count" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: negative_input"
                    , "    station: forge"
                    , "    inputs:"
                    , "      - item: steel_bar"
                    , "        count: -3"
                    , "    outputs: []"
                    ]
            shouldNameCountFields msg
                ("negative_input", "input", "steel_bar", -3)

        it "rejects a count: 0 fuel line" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: free_fuel"
                    , "    station: furnace"
                    , "    inputs: []"
                    , "    fuel:"
                    , "      item: coal_lump"
                    , "      count: 0"
                    , "    outputs: []"
                    ]
            shouldNameCountFields msg ("free_fuel", "fuel", "coal_lump", 0)

        it "rejects a negative fuel count" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: negative_fuel"
                    , "    station: furnace"
                    , "    inputs: []"
                    , "    fuel:"
                    , "      item: coal_lump"
                    , "      count: -1"
                    , "    outputs: []"
                    ]
            shouldNameCountFields msg
                ("negative_fuel", "fuel", "coal_lump", -1)

        it "rejects a count: 0 output line" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: empty_output"
                    , "    station: forge"
                    , "    inputs: []"
                    , "    outputs:"
                    , "      - item: steel_dagger"
                    , "        count: 0"
                    ]
            shouldNameCountFields msg
                ("empty_output", "output", "steel_dagger", 0)

        it "rejects a negative output count" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: negative_output"
                    , "    station: forge"
                    , "    inputs: []"
                    , "    outputs:"
                    , "      - item: steel_dagger"
                    , "        count: -2"
                    ]
            shouldNameCountFields msg
                ("negative_output", "output", "steel_dagger", -2)

        it "fails the WHOLE file when a valid recipe precedes an invalid \
           \one, so loadRecipeYamlFn never begins its insertion fold" $ do
            msg ← parseFailureMessage $ recipeSrc
                    [ "  - id: good_craft"
                    , "    station: forge"
                    , "    inputs:"
                    , "      - item: steel_bar"
                    , "        count: 2"
                    , "    outputs:"
                    , "      - item: steel_dagger"
                    , "  - id: bad_craft"
                    , "    station: forge"
                    , "    inputs:"
                    , "      - item: iron_bar"
                    , "        count: 0"
                    , "    outputs:"
                    , "      - item: iron_dagger"
                    ]
            shouldNameCountFields msg ("bad_craft", "input", "iron_bar", 0)

        it "keeps every positive count on input, fuel and output lines" $ do
            let src = recipeSrc
                    [ "  - id: positive_counts"
                    , "    station: furnace"
                    , "    inputs:"
                    , "      - item: iron_ore"
                    , "        count: 3"
                    , "      - item: flux"
                    , "        count: 1"
                    , "    fuel:"
                    , "      item: coal_lump"
                    , "      count: 2"
                    , "    outputs:"
                    , "      - item: iron_bar"
                    , "        count: 4"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → do
                        map ryiCount (ryInputs d) `shouldBe` [3, 1]
                        ryFuel d `shouldBe`
                            Just (RecipeYamlIngredient "coal_lump" 2)
                        map ryiCount (ryOutputs d) `shouldBe` [4]
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "still defaults an omitted count — and an explicit null one, \
           \which .:? reads as absent — to 1 rather than rejecting it" $ do
            let src = recipeSrc
                    [ "  - id: default_counts"
                    , "    station: furnace"
                    , "    inputs:"
                    , "      - item: iron_ore"
                    , "      - item: flux"
                    , "        count:"
                    , "    fuel:"
                    , "      item: coal_lump"
                    , "    outputs:"
                    , "      - item: iron_bar"
                    , "        count:"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → do
                        map ryiCount (ryInputs d) `shouldBe` [1, 1]
                        ryFuel d `shouldBe`
                            Just (RecipeYamlIngredient "coal_lump" 1)
                        map ryiCount (ryOutputs d) `shouldBe` [1]
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "leaves an empty inputs:/outputs: list valid — emptiness is \
           \not a non-positive count (the two repair recipes ship one)" $ do
            let src = recipeSrc
                    [ "  - id: empty_lists"
                    , "    station: repair_condition"
                    , "    repair_axis: condition"
                    , "    inputs: []"
                    , "    outputs: []"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → do
                        ryInputs d `shouldBe` []
                        ryOutputs d `shouldBe` []
                        ryFuel d `shouldBe` Nothing
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "decodes every tracked data/recipes/*.yaml — the whole corpus \
           \scripts/startup_loader.lua enumerates at boot" $ do
            names ← listDirectory "data/recipes"
            let files = sort [ n | n ← names, takeExtension n ≡ ".yaml" ]
            files `shouldSatisfy` ((≥ 6) ∘ length)
            mapM_ (\n → do
                    r ← Yaml.decodeFileEither ("data/recipes" ⊘ n)
                    case r ∷ Either Yaml.ParseException RecipeYamlFile of
                        Left err → expectationFailure (n <> ": " <> show err)
                        Right f  → ryfRecipes f `shouldSatisfy` (not ∘ null))
                  files

        it "defaults name/work/count and reads fuel + knowledge" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: smelt_test"
                    , "    station: furnace"
                    , "    inputs:"
                    , "      - item: iron_ore"
                    , "        count: 3"
                    , "    fuel:"
                    , "      item: coal_lump"
                    , "    outputs:"
                    , "      - item: iron_bar"
                    , "    knowledge: metallurgy"
                    , "    skill: smithing"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → do
                        ryName d `shouldBe` "smelt_test"   -- defaults to id
                        ryWork d `shouldBe` 0
                        ryFuel d `shouldBe`
                            Just (RecipeYamlIngredient "coal_lump" 1)
                        ryKnowledge d `shouldBe` Just "metallurgy"
                        rySkill d `shouldBe` Just "smithing"
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "rejects an entry with no station" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: broken"
                    , "    inputs: []"
                    , "    outputs: []"
                    ]
            case parseFile src of
                Left _  → pure ()
                Right _ → expectationFailure "expected a parse failure"

        it "output_temp defaults to Nothing when omitted" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: plain_craft"
                    , "    station: forge"
                    , "    inputs: []"
                    , "    outputs: []"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → ryOutputTemp d `shouldBe` Nothing
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "reads output_temp (#344/#346)" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: brew_test"
                    , "    station: cooking"
                    , "    inputs: []"
                    , "    outputs: []"
                    , "    output_temp: 100"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → ryOutputTemp d `shouldBe` Just 100
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "power_draw defaults to 0 when omitted (#590)" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: plain_craft"
                    , "    station: forge"
                    , "    inputs: []"
                    , "    outputs: []"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → ryPowerDraw d `shouldBe` 0
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "reads power_draw (#590)" $ do
            let src = BS8.pack $ unlines
                    [ "recipes:"
                    , "  - id: powered_test"
                    , "    station: assemble"
                    , "    inputs: []"
                    , "    outputs: []"
                    , "    power_draw: 250"
                    ]
            case parseFile src of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → ryPowerDraw d `shouldBe` 250
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

        it "parses the shipped data/recipes/basic_food.yaml (#346)" $ do
            r ← Yaml.decodeFileEither "data/recipes/basic_food.yaml"
            case r of
                Left err → expectationFailure (show err)
                Right f  → case ryfRecipes f of
                    [d] → do
                        ryId d `shouldBe` "brew_coffee"
                        ryStation d `shouldBe` "cooking"
                        rySkill d `shouldBe` Just "cooking"
                        ryKnowledge d `shouldBe` Just "basic_cuisine"
                        map ryiItem (ryInputs d)
                            `shouldBe` ["water", "coffee_grounds"]
                        map ryiItem (ryOutputs d) `shouldBe` ["coffee_pot"]
                        ryOutputTemp d `shouldBe` Just 100
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

    describe "Craft.Execute.takeItemsByName" $ do
        it "removes exactly n first-matching instances" $ do
            let inv = [mkInst "steel_bar" 1, mkInst "rations" 2
                      ,mkInst "steel_bar" 3]
            fmap (map iiInstanceId) (takeItemsByName "steel_bar" 1 inv)
                `shouldBe` Just [2, 3]
        it "returns Nothing when short" $
            takeItemsByName "steel_bar" 2 [mkInst "steel_bar" 1]
                `shouldBe` Nothing
        it "n ≤ 0 consumes nothing" $ do
            let inv = [mkInst "steel_bar" 1]
            takeItemsByName "steel_bar" 0 inv `shouldBe` Just inv

    describe "Craft.Execute.consumeIngredients" $ do
        it "consumes the demanded counts and keeps the rest" $ do
            let inv = [mkInst "rations" 9, mkInst "steel_bar" 1
                      ,mkInst "steel_bar" 2, mkInst "steel_bar" 3]
            fmap (map iiInstanceId) (consumeIngredients daggerRecipe inv)
                `shouldBe` Right [9, 3]
        it "fails all-or-nothing when an ingredient is short" $ do
            let inv = [mkInst "steel_bar" 1]
            case consumeIngredients daggerRecipe inv of
                Left reason → reason `shouldSatisfy`
                    T.isInfixOf "steel_bar"
                Right _ → expectationFailure "expected Left"
        it "a fuel line repeating an input demands the sum" $ do
            let short = replicate 2 (mkInst "steel_bar" 0)
                full  = replicate 3 (mkInst "steel_bar" 0)
            consumeIngredients fuelledRecipe short
                `shouldSatisfy` isLeft
            fmap length (consumeIngredients fuelledRecipe full)
                `shouldBe` Right 0

    describe "Craft.Execute.craftQuality" $ do
        it "without a knowledge gate, quality is the skill level" $ do
            craftQuality 0 Nothing   `shouldBe` 0
            craftQuality 55 Nothing  `shouldBe` 55
            craftQuality 100 Nothing `shouldBe` 100
        it "with a knowledge level, blends 70% skill / 30% knowledge" $ do
            let near expect q = abs (q - expect) < 0.001
            craftQuality 90 (Just 80) `shouldSatisfy` near 87
            craftQuality 10 (Just 20) `shouldSatisfy` near 13
            craftQuality 0 (Just 100) `shouldSatisfy` near 30
        it "clamps to [0, 100]" $ do
            craftQuality 150 Nothing    `shouldBe` 100
            craftQuality (-10) Nothing  `shouldBe` 0
            craftQuality 150 (Just 150) `shouldBe` 100
        it "is monotone in both inputs" $ do
            craftQuality 60 (Just 40) > craftQuality 40 (Just 40)
                `shouldBe` True
            craftQuality 40 (Just 60) > craftQuality 40 (Just 40)
                `shouldBe` True
