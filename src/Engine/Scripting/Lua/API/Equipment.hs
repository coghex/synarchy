{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level equipment API module (#67/#478). Re-exports every
--   Lua-facing function from the sub-modules so existing call-sites
--   keep working: Equipment.Class is the class catalogue
--   (engine.loadEquipmentYaml, equipment.getClass / getClassNames),
--   Equipment.Slot is named-slot equip/unequip, Equipment.Accessory is
--   the ordered accessory list (equip/unequip folding buffs into
--   uiModifiers), and Equipment.Render is the shared render-field
--   pusher used by both the loadout and accessory-list queries. The
--   export list is explicit (not a blanket @module@ re-export) so
--   sub-module-internal helpers (e.g. Equipment.Slot's
--   removeFirstFromInventoryWhere, needed by Equipment.Accessory)
--   don't leak onto this top-level module's public API.
module Engine.Scripting.Lua.API.Equipment
    ( loadEquipmentYamlFn
    , equipmentGetClassFn
    , equipmentGetClassNamesFn
    , equipmentEquipFn
    , equipmentUnequipFn
    , equipmentGetLoadoutFn
    , equipmentEquipAccessoryFn
    , equipmentUnequipAccessoryFn
    , equipmentGetAccessoriesFn
    , pushItemInstance
    ) where

import Engine.Scripting.Lua.API.Equipment.Class
import Engine.Scripting.Lua.API.Equipment.Slot
import Engine.Scripting.Lua.API.Equipment.Accessory
import Engine.Scripting.Lua.API.Equipment.Render
