{-# LANGUAGE Strict, DeriveGeneric #-}
-- | YAML loader for data/recipes/*.yaml. Mirrors Engine.Asset.YamlInfection.
--   The on-disk schema is documented in data/recipes/basic.yaml.
--   `repair_axis` (#301) marks a recipe as a REPAIR flow (data/recipes/
--   repair.yaml) rather than a craft — see Craft.Types.rdRepairAxis.
module Engine.Asset.YamlRecipes
    ( RecipeYamlIngredient(..)
    , RecipeYamlDef(..)
    , RecipeYamlFile(..)
    , loadRecipeYaml
    , loadRecipeYamlOutcome
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Data.Aeson.Types (Parser)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlListOutcome)

-- | One `{ item, count }` line (an input, the fuel, or an output).
--   `count` defaults to 1 so a terse `- item: steel_bar` still loads,
--   and must be POSITIVE — 'RecipeYamlDef' rejects anything else, since
--   only there is the recipe id available to name in the message.
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
    , rySkill     ∷ !(Maybe Text)
    , ryRepairAxis ∷ !(Maybe Text)
    , ryOutputTemp ∷ !(Maybe Float)
    , ryPowerDraw  ∷ !Float
    } deriving (Show, Eq, Generic)

-- | Reject a zero or negative `count` HERE, where the recipe id is in
--   scope, so the message names the recipe, which line kind it was, the
--   item and the offending value — and so the whole file's load fails
--   rather than a malformed line reaching a consumer.
--
--   Neither the schema (data/recipes/basic.yaml) nor #325 gives zero or
--   a negative any meaning, and both ends absorb one silently with
--   SUCCESS semantics: 'Craft.Execute.takeItemsByName' reports a
--   non-positive demand as satisfied, and the output builder's @max 0@
--   turns one into an empty output line. A mistyped @count: 0@ on an
--   input therefore loaded as a recipe that consumed nothing and still
--   produced its output. Constraining the value where it is AUTHORED is
--   the same boundary #1711/#1716/#1721 hold for their loaders; the
--   defensive downstream clamps stay, and an author disabling a line
--   deletes or comments it out (#1940).
checkCount ∷ Text → Text → RecipeYamlIngredient → Parser ()
checkCount rid kind ing
    | ryiCount ing > 0 = pure ()
    | otherwise = fail $ T.unpack $
        "recipe " <> rid <> ": " <> kind <> " count for \""
        <> ryiItem ing <> "\" must be positive, got "
        <> T.pack (show (ryiCount ing))

instance FromJSON RecipeYamlDef where
    parseJSON = withObject "RecipeYamlDef" $ \v → do
        rid  ← v .: "id"
        axis ← v .:? "repair_axis"
        -- Reject anything but the two known axes HERE, at the only
        -- entry point for repair_axis, so a typo (e.g. "sharpnes")
        -- fails the whole file's load instead of silently becoming a
        -- recipe that repairs the wrong axis (Craft.Types.RepairAxis
        -- is what makes that failure mode impossible downstream).
        case axis of
            Just a | a ≢ "condition" ∧ a ≢ "sharpness" →
                fail (T.unpack ("repair_axis must be \"condition\" or "
                                 <> "\"sharpness\", got " <> a))
            _ → pure ()
        inputs  ← v .:  "inputs"
        fuel    ← v .:? "fuel"
        outputs ← v .:  "outputs"
        -- An EMPTY inputs:/outputs: list stays valid (the two repair
        -- recipes ship `outputs: []`) — emptiness is not a count.
        mapM_ (checkCount rid "input")  inputs
        mapM_ (checkCount rid "fuel")   fuel
        mapM_ (checkCount rid "output") outputs
        RecipeYamlDef rid
            ⊚ v .:? "name" .!= rid
            ⊛ v .:  "station"
            ⊛ pure inputs
            ⊛ pure fuel
            ⊛ v .:? "work" .!= 0
            ⊛ pure outputs
            ⊛ v .:? "knowledge"
            ⊛ v .:? "skill"
            ⊛ pure axis
            ⊛ v .:? "output_temp"
            ⊛ v .:? "power_draw" .!= 0

newtype RecipeYamlFile = RecipeYamlFile
    { ryfRecipes ∷ [RecipeYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON RecipeYamlFile where
    parseJSON = withObject "RecipeYamlFile" $ \v → RecipeYamlFile
        ⊚ v .: "recipes"

-- | 'loadRecipeYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadRecipeYaml'.
loadRecipeYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [RecipeYamlDef])
loadRecipeYamlOutcome logger =
    loadYamlListOutcome logger "recipe" "recipes" ryfRecipes

loadRecipeYaml ∷ LoggerState → FilePath → IO [RecipeYamlDef]
loadRecipeYaml logger path = fromMaybe [] ⊚ loadRecipeYamlOutcome logger path
