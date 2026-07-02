{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
-- | YAML loader for data/recipes/*.yaml. Mirrors Engine.Asset.YamlInfection.
--   The on-disk schema is documented in data/recipes/basic.yaml.
module Engine.Asset.YamlRecipes
    ( RecipeYamlIngredient(..)
    , RecipeYamlDef(..)
    , RecipeYamlFile(..)
    , loadRecipeYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | One `{ item, count }` line (an input, the fuel, or an output).
--   `count` defaults to 1 so a terse `- item: steel_bar` still loads.
data RecipeYamlIngredient = RecipeYamlIngredient
    { ryiItem  ∷ !Text
    , ryiCount ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON RecipeYamlIngredient where
    parseJSON = withObject "RecipeYamlIngredient" $ \v → RecipeYamlIngredient
        ⊚ v .:  "item"
        ⊛ v .:? "count" .!= 1

-- | YAML shape for one recipe entry. `id`, `station`, `inputs` and
--   `outputs` are required; everything else has a sensible default.
data RecipeYamlDef = RecipeYamlDef
    { ryId        ∷ !Text
    , ryName      ∷ !Text
    , ryStation   ∷ !Text
    , ryInputs    ∷ ![RecipeYamlIngredient]
    , ryFuel      ∷ !(Maybe RecipeYamlIngredient)
    , ryWork      ∷ !Float
    , ryOutputs   ∷ ![RecipeYamlIngredient]
    , ryKnowledge ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic)

instance FromJSON RecipeYamlDef where
    parseJSON = withObject "RecipeYamlDef" $ \v → do
        rid ← v .: "id"
        RecipeYamlDef rid
            ⊚ v .:? "name" .!= rid
            ⊛ v .:  "station"
            ⊛ v .:  "inputs"
            ⊛ v .:? "fuel"
            ⊛ v .:? "work" .!= 0
            ⊛ v .:  "outputs"
            ⊛ v .:? "knowledge"

newtype RecipeYamlFile = RecipeYamlFile
    { ryfRecipes ∷ [RecipeYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON RecipeYamlFile where
    parseJSON = withObject "RecipeYamlFile" $ \v → RecipeYamlFile
        ⊚ v .: "recipes"

loadRecipeYaml ∷ LoggerState → FilePath → IO [RecipeYamlDef]
loadRecipeYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse recipe YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right f → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (ryfRecipes f)))
                <> " recipes from " <> T.pack path
            return (ryfRecipes f)
