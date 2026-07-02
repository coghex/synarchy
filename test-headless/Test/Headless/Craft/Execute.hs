{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Recipe catalogue + craft execution tests (#325): the YAML schema
--   (required fields, defaults, fuel/knowledge options) and the pure
--   all-or-nothing consumption core that craft.execute wraps. The
--   engine-integrated path (Lua verb → unit inventory) is gated by
--   tools/craft_probe.py.
module Test.Headless.Craft.Execute (spec) where

import UPrelude
import Test.Hspec
import Data.Either (isLeft)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Craft.Execute (consumeIngredients, takeItemsByName)
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
    }

-- | A fuelled variant whose fuel line repeats an input item.
fuelledRecipe ∷ RecipeDef
fuelledRecipe = daggerRecipe
    { rdId   = "fuelled"
    , rdFuel = Just (RecipeIngredient "steel_bar" 1)
    }

parseFile ∷ BS8.ByteString → Either Yaml.ParseException RecipeYamlFile
parseFile = Yaml.decodeEither'

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
                        map ryiItem (ryOutputs d) `shouldBe` ["steel_dagger"]
                    ds → expectationFailure $
                        "expected exactly one recipe, got " <> show (length ds)

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
