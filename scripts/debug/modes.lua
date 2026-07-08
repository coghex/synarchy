-- Concrete debug-overlay categories (#545): what each button/list shows
-- and which item field arms it. Order here IS the on-screen stacking
-- order — scripts/debug.lua's layout walks this array top to bottom.
--
-- The `exclusiveWith` lists reproduce the original monolith's arm
-- matrix exactly, including its one quirk: structure mode was added
-- last and only wired mutually exclusive against location, so arming
-- spawn/fluid/item/terrain does NOT clear an already-armed structure.
-- That's pre-existing behavior (not something this split changes).
local Mode = require("scripts.debug.mode")

local FLUID_KINDS = { "water", "lava" }

local spawnMode = Mode.new({
    key = "spawn", label = "spawn", armedField = "armedDef",
    exclusiveWith = { "armedFluidType", "armedItemDef", "armedTerrainId",
                       "armedLocation" },
    emptyText = "  (no units defined)",
    fetch = function()
        local defs = unit.listDefs() or {}
        table.sort(defs)
        return defs
    end,
    entryValue = function(defName) return defName end,
    entryText  = function(defName) return defName end,
})

local fluidMode = Mode.new({
    key = "fluid", label = "fluid", armedField = "armedFluidType",
    exclusiveWith = { "armedDef", "armedItemDef", "armedTerrainId",
                       "armedLocation" },
    fetch = function() return FLUID_KINDS end,
    entryValue = function(kind) return kind end,
    entryText  = function(kind) return kind end,
})

local itemMode = Mode.new({
    key = "item", label = "items", armedField = "armedItemDef",
    exclusiveWith = { "armedDef", "armedFluidType", "armedTerrainId",
                       "armedLocation" },
    emptyText = "  (no items defined)",
    fetch = function() return item.listDefs() or {} end,
    entryValue = function(def) return def.name end,
    entryText  = function(def) return def.name end,
})

local terrainMode = Mode.new({
    key = "terrain", label = "terrain", armedField = "armedTerrainId",
    exclusiveWith = { "armedDef", "armedFluidType", "armedItemDef",
                       "armedLocation" },
    emptyText = "  (no materials loaded)",
    fetch = function() return world.listMaterials() or {} end,
    entryValue = function(m) return m.id end,
    entryText  = function(m) return m.name end,
})

local locationMode = Mode.new({
    key = "location", label = "locations", armedField = "armedLocation",
    exclusiveWith = { "armedDef", "armedFluidType", "armedItemDef",
                       "armedTerrainId", "armedStructure" },
    emptyText = "  (no locations defined)",
    fetch = function() return require("scripts.locations").list() or {} end,
    entryValue = function(d) return d.name end,
    entryText  = function(d) return d.name end,
})

local structureMode = Mode.new({
    key = "structure", label = "structures", armedField = "armedStructure",
    exclusiveWith = { "armedDef", "armedFluidType", "armedItemDef",
                       "armedTerrainId", "armedLocation" },
    fetch = function() return require("scripts.structures").kinds or {} end,
    entryValue = function(kind) return kind end,
    entryText  = function(kind) return kind end,
})

return {
    spawnMode, fluidMode, itemMode, terrainMode, locationMode, structureMode,
}
