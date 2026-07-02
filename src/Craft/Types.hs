{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Data-driven crafting recipe catalogue (#325). A recipe DEF describes
--   one craft: what station kind it runs at, what it consumes (inputs +
--   optional fuel), how much work it takes, and what it produces. Loaded
--   from data/recipes/*.yaml via Engine.Asset.YamlRecipes into the
--   engine-wide RecipeManager (mirrors Infection.Types / Substance.Types).
--
--   Station kind names a work-station OPERATION ("smelt", "forge",
--   "assemble") — buildings advertise the operations they offer via
--   bdOperations (#326) and craft.executeAt matches the two; the
--   AI/bill layer (#329) reads rdWork.
--
--   Field prefix `rd` / `ri` to avoid colliding with other record
--   namespaces.
module Craft.Types
    ( RecipeIngredient(..)
    , RecipeDef(..)
    , recipeDemands
    , RecipeManager(..)
    , emptyRecipeManager
    , lookupRecipe
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- | One (item def name, count) line of a recipe — an input, the fuel,
--   or an output.
data RecipeIngredient = RecipeIngredient
    { riItem  ∷ !Text   -- ^ item def name (Item.Types.idName)
    , riCount ∷ !Int    -- ^ how many; parser defaults omitted counts to 1
    } deriving (Show, Eq)

-- | One recipe from the YAML catalogue.
data RecipeDef = RecipeDef
    { rdId        ∷ !Text                     -- ^ unique key
    , rdName      ∷ !Text                     -- ^ display name
    , rdStation   ∷ !Text                     -- ^ station operation
                                              --   ("smelt", "forge",
                                              --   "assemble", …); matched
                                              --   against bdOperations by
                                              --   craft.executeAt (#326)
    , rdInputs    ∷ ![RecipeIngredient]       -- ^ consumed materials
    , rdFuel      ∷ !(Maybe RecipeIngredient) -- ^ consumed fuel, if any
    , rdWork      ∷ !Float                    -- ^ effort to complete; the
                                              --   craft AI (#329) will burn
                                              --   this down, scaled by skill
    , rdOutputs   ∷ ![RecipeIngredient]       -- ^ produced items
    , rdKnowledge ∷ !(Maybe Text)             -- ^ knowledge the crafter must
                                              --   KNOW (present in
                                              --   uiKnowledge) to execute
    } deriving (Show, Eq)

-- | Everything a craft consumes: inputs plus the fuel line, if any.
--   Consumption folds over this, so a recipe whose fuel repeats an
--   input item simply demands the sum.
recipeDemands ∷ RecipeDef → [RecipeIngredient]
recipeDemands r = rdInputs r ⧺ maybe [] (:[]) (rdFuel r)

-- | Engine-wide registry loaded from data/recipes/.
newtype RecipeManager = RecipeManager
    { rmDefs ∷ HM.HashMap Text RecipeDef
    } deriving (Show, Eq)

emptyRecipeManager ∷ RecipeManager
emptyRecipeManager = RecipeManager HM.empty

lookupRecipe ∷ Text → RecipeManager → Maybe RecipeDef
lookupRecipe n (RecipeManager m) = HM.lookup n m
