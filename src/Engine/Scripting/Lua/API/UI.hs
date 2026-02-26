{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level UI API module.  Re-exports every Lua-facing function
--   from the sub-modules so existing call-sites keep working.
module Engine.Scripting.Lua.API.UI
  ( -- * Page functions
    module Engine.Scripting.Lua.API.UI.Page
    -- * Element creation
  , module Engine.Scripting.Lua.API.UI.Element
    -- * Hierarchy
  , module Engine.Scripting.Lua.API.UI.Hierarchy
    -- * Focus operations
  , module Engine.Scripting.Lua.API.UI.Focus
    -- * Text buffer / cursor operations
  , module Engine.Scripting.Lua.API.UI.TextInput
    -- * Property setters / getters
  , module Engine.Scripting.Lua.API.UI.Property
  ) where

import Engine.Scripting.Lua.API.UI.Page
import Engine.Scripting.Lua.API.UI.Element
import Engine.Scripting.Lua.API.UI.Hierarchy
import Engine.Scripting.Lua.API.UI.Focus
import Engine.Scripting.Lua.API.UI.TextInput
import Engine.Scripting.Lua.API.UI.Property
