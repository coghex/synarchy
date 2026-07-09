{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level unit API module. Re-exports every Lua-facing function
--   from the sub-modules so existing call-sites keep working: Yaml is
--   unit-def loading, Spawn is lifecycle/position/pose, Survival is
--   drink/eat/feed, Query is combat-adjacent numeric getters, List is
--   listing/info/texture lookups, Combat is wound/blood/injury, Equipment
--   is item-fill/repair, Inventory is item-level inventory ops, Cargo is
--   building/unit/technomule transfers, Stats is skills/knowledge/stats/
--   modifiers, Selection is cursor selection + anim/facing state, and
--   Medical is bleed/infection/frostbite treatment (#546).
module Engine.Scripting.Lua.API.Units
    ( module Engine.Scripting.Lua.API.Units.Yaml
    , module Engine.Scripting.Lua.API.Units.Spawn
    , module Engine.Scripting.Lua.API.Units.Survival
    , module Engine.Scripting.Lua.API.Units.Query
    , module Engine.Scripting.Lua.API.Units.List
    , module Engine.Scripting.Lua.API.Units.Combat
    , module Engine.Scripting.Lua.API.Units.Equipment
    , module Engine.Scripting.Lua.API.Units.Inventory
    , module Engine.Scripting.Lua.API.Units.Cargo
    , module Engine.Scripting.Lua.API.Units.Stats
    , module Engine.Scripting.Lua.API.Units.Selection
    , module Engine.Scripting.Lua.API.Units.Medical
    ) where

import Engine.Scripting.Lua.API.Units.Yaml
import Engine.Scripting.Lua.API.Units.Spawn
import Engine.Scripting.Lua.API.Units.Survival
import Engine.Scripting.Lua.API.Units.Query
import Engine.Scripting.Lua.API.Units.List
import Engine.Scripting.Lua.API.Units.Combat
import Engine.Scripting.Lua.API.Units.Equipment
import Engine.Scripting.Lua.API.Units.Inventory
import Engine.Scripting.Lua.API.Units.Cargo
import Engine.Scripting.Lua.API.Units.Stats
import Engine.Scripting.Lua.API.Units.Selection
import Engine.Scripting.Lua.API.Units.Medical
