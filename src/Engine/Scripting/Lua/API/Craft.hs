{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level crafting API module (#325/#326/#329). Re-exports every
--   Lua-facing function from the sub-modules so existing call-sites
--   keep working: Craft.Recipe is the read-only catalogue
--   (engine.loadRecipeYaml, craft.get, craft.getNames), Craft.Execute
--   runs one craft (craft.execute / craft.executeAt and the shared
--   station gate validateStation), and Craft.Bill is the standing
--   production-order queue (craft.addBill and friends).
module Engine.Scripting.Lua.API.Craft
    ( -- * Recipe catalogue
      module Engine.Scripting.Lua.API.Craft.Recipe
      -- * Craft execution
    , module Engine.Scripting.Lua.API.Craft.Execute
      -- * Craft bills
    , module Engine.Scripting.Lua.API.Craft.Bill
    ) where

import Engine.Scripting.Lua.API.Craft.Recipe
import Engine.Scripting.Lua.API.Craft.Execute
import Engine.Scripting.Lua.API.Craft.Bill
