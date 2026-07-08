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
    , RepairAxis(..)
    , repairAxisName
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- | One (item def name, count) line of a recipe — an input, the fuel,
--   or an output.
data RecipeIngredient = RecipeIngredient
    { riItem  ∷ !Text   -- ^ item def name (Item.Types.idName)
    , riCount ∷ !Int    -- ^ how many; parser defaults omitted counts to 1
    } deriving (Show, Eq)

-- | Which wear axis a repair recipe (#301) restores. A proper sum
--   type rather than raw Text so an invalid value CANNOT reach
--   RecipeDef — Engine.Asset.YamlRecipes' parser is the only producer
--   of a `Just Text` axis and rejects anything but "condition"/
--   "sharpness" at parse time (a malformed recipe never enters the
--   catalogue), and every RepairAxis consumer pattern-matches both
--   constructors exhaustively (no wildcard fallback), so a future
--   third axis can't silently alias onto the wrong one either.
data RepairAxis = RepairCondition | RepairSharpness
    deriving (Show, Eq)

-- | The YAML/Lua-facing spelling of an axis — round-trips with the
--   `repair_axis` vocabulary ("condition"/"sharpness").
repairAxisName ∷ RepairAxis → Text
repairAxisName RepairCondition = "condition"
repairAxisName RepairSharpness = "sharpness"

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
    , rdSkill     ∷ !(Maybe Text)             -- ^ crafter skill that sets
                                              --   output quality (#343);
                                              --   Nothing = quality rolls
                                              --   from the item def's spec
    , rdRepairAxis ∷ !(Maybe RepairAxis)      -- ^ #301: Just marks this as
                                              --   a REPAIR recipe rather
                                              --   than a craft — it targets
                                              --   an existing item instance
                                              --   (repair.repairAt) instead
                                              --   of producing rdOutputs.
                                              --   Nothing (the default) is
                                              --   an ordinary craft recipe;
                                              --   craft.execute/executeAt
                                              --   refuse recipes with this
                                              --   set.
    , rdOutputTemp ∷ !(Maybe Float)           -- ^ #344/#346: outputs spawn
                                              --   tracked at this °C
                                              --   instead of ambient (e.g.
                                              --   100 for fresh-brewed
                                              --   coffee, a hot bar off
                                              --   the smelter). Nothing =
                                              --   outputs spawn at ambient
                                              --   (the historical default).
    , rdPowerDraw ∷ !Float                    -- ^ #590: watts this recipe
                                              --   demands from its station
                                              --   WHILE being worked. 0
                                              --   (default, every recipe
                                              --   predating #590) = never
                                              --   gated on power, runs at
                                              --   any Built station
                                              --   regardless of network
                                              --   status. Positive = the
                                              --   job-dependent consumer
                                              --   load (replaces the old
                                              --   #361 building-level
                                              --   bdPowerDrain for crafting
                                              --   purposes — see
                                              --   Power.Network's
                                              --   activeCraftConsumersOn).
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
