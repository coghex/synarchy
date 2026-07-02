{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure inventory side of craft execution (#325): verify a recipe's
--   demands against an inventory and consume them all-or-nothing. Kept
--   free of EngineEnv so the headless suite can exercise it directly;
--   the Lua verb (Engine.Scripting.Lua.API.Craft) wraps this in one
--   atomic unit-manager update and appends the rolled outputs.
module Craft.Execute
    ( consumeIngredients
    , takeItemsByName
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
