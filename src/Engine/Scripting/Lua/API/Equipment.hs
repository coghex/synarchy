{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level equipment API module (#67/#478). Re-exports every
--   Lua-facing function from the sub-modules so existing call-sites
--   keep working: Equipment.Class is the class catalogue
--   (engine.loadEquipmentYaml, equipment.getClass / getClassNames),
--   Equipment.Slot is named-slot equip/unequip, Equipment.Accessory is
--   the ordered accessory list (equip/unequip folding buffs into
--   uiModifiers), and Equipment.Render is the shared render-field
--   pusher used by both the loadout and accessory-list queries.
module Engine.Scripting.Lua.API.Equipment
    ( -- * Equipment class catalogue
      module Engine.Scripting.Lua.API.Equipment.Class
      -- * Slot equip/unequip
    , module Engine.Scripting.Lua.API.Equipment.Slot
      -- * Accessory equip/unequip
    , module Engine.Scripting.Lua.API.Equipment.Accessory
      -- * Render-field pushing
    , module Engine.Scripting.Lua.API.Equipment.Render
    ) where

import Engine.Scripting.Lua.API.Equipment.Class
import Engine.Scripting.Lua.API.Equipment.Slot
import Engine.Scripting.Lua.API.Equipment.Accessory
import Engine.Scripting.Lua.API.Equipment.Render
