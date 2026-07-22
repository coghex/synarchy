{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure inventory side of craft execution (#325): verify a recipe's
--   demands against an inventory and consume them all-or-nothing. Kept
--   free of EngineEnv so the headless suite can exercise it directly;
--   the Lua verb (Engine.Scripting.Lua.API.Craft) wraps this in one
--   atomic unit-manager update and appends the rolled outputs.
module Craft.Execute
    ( consumeIngredients
    , takeItemsByName
    , craftQuality
    , applyMentalQuality
    ) where

import UPrelude
import qualified Data.Text as T
import Control.Monad (foldM)
import Craft.Types (RecipeDef(..), RecipeIngredient(..), recipeDemands)
import Item.Types (ItemInstance(..))

-- | Remove @n@ instances of @name@ from the inventory (first-match
--   order, same rule as unit.removeItem). Nothing if fewer than @n@
--   are present — the inventory is returned unchanged only on success,
--   so callers get all-or-nothing semantics. Matches TOP-LEVEL
--   instances only; container contents are not opened.
takeItemsByName ∷ Text → Int → [ItemInstance] → Maybe [ItemInstance]
takeItemsByName name = go
  where
    go n inv
        | n ≤ 0 = Just inv
        | otherwise = case inv of
            [] → Nothing
            (x:xs)
                | iiDefName x ≡ name → go (n - 1) xs
                | otherwise          → (x :) ⊚ go n xs

-- | Output quality for a skill-tagged recipe (#343): deterministic in
--   the crafter, replacing the item-def quality roll. Both arguments
--   are 0–100 levels (crafter's rdSkill level; the rdKnowledge level
--   when the recipe is knowledge-gated — the gate itself has already
--   passed by the time quality is computed). Skill dominates and the
--   knowledge blends in at 30% — knowing the theory lifts a clumsy
--   hand only so far. No botch: low skill just means low quality.
craftQuality ∷ Float → Maybe Float → Float
craftQuality skill mKnow = clamp 0 100 $ case mKnow of
    Nothing → skill
    Just k  → 0.7 * skill + 0.3 * k

-- | The #353 mental-effectiveness quality delta, applied identically
--   atop a skill-tagged recipe's 'craftQuality' output and an untagged
--   recipe's item-def-rolled quality — the one place either base-quality
--   path meets #353. @effectiveness@ (0.75..1.10, from
--   'Combat.Resolution.Common.mentalEffectiveness') scales 1:1 with its
--   departure from the neutral 1.00, clamped to ±10 quality points; the
--   result is float-valued and deliberately unrounded. effectiveness =
--   1.00 leaves @baseQuality@ unchanged.
applyMentalQuality ∷ Float → Float → Float
applyMentalQuality effectiveness baseQuality =
    clamp 0 100 (baseQuality + clamp (-10) 10 (100 * (effectiveness - 1.0)))

-- | Consume everything a recipe demands (inputs + fuel) from an
--   inventory. Right = the inventory after consumption; Left = a
--   human-readable reason naming the first short ingredient. Folding
--   demand-by-demand means a fuel line repeating an input item
--   naturally requires the sum of both counts.
consumeIngredients ∷ RecipeDef → [ItemInstance] → Either Text [ItemInstance]
consumeIngredients recipe inv0 = foldM step inv0 (recipeDemands recipe)
  where
    step inv ing = case takeItemsByName (riItem ing) (riCount ing) inv of
        Nothing   → Left $ "missing " <> T.pack (show (riCount ing))
                         <> "x " <> riItem ing
        Just inv' → Right inv'
