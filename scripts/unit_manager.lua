-- Unit Manager - loads unit definitions and manages live unit instances.
-- Loaded as an engine script from init.lua with its own tick interval.
local unitLoader = require("scripts.unit_loader")
local buildingLoader = require("scripts.building_loader")
local itemLoader = require("scripts.item_loader")
local equipmentLoader = require("scripts.equipment_loader")
local substanceLoader = require("scripts.substance_loader")

local unitManager = {}

-- Tracks whether definitions have been loaded
unitManager.defsLoaded = false
unitManager.defCount = 0

-- Live unit instances (managed from game scripts via this module)
-- unitId -> { defName, gridX, gridY, gridZ }
unitManager.units = {}

-----------------------------------------------------------
-- Init (called automatically when script is loaded)
-----------------------------------------------------------

function unitManager.init(scriptId)
    engine.logInfo("Unit manager initializing...")

    local count = unitLoader.loadAll("data/units")
    unitManager.defCount = count
    unitManager.defsLoaded = true

    -- Buildings ride along here: same loader pattern, same lifecycle
    -- timing. If buildings get heavy enough to need their own manager
    -- this can be split out, but for now it's just data loading.
    local buildingCount = buildingLoader.loadAll("data/buildings")
    unitManager.buildingDefCount = buildingCount

    -- Items load BEFORE the first unit spawns; unit spawn looks up
    -- starting_inventory item names against the registry, so they must
    -- exist by then. (Boot order: this init runs before any menu is
    -- shown, so we're safe.)
    local itemCount = itemLoader.loadAll("data/items")
    unitManager.itemDefCount = itemCount

    -- Equipment classes — must load before any unit-info v2 panel
    -- queries equipment.getClass(). UnitDef.udEquipmentClass is just a
    -- name reference; the actual class data lives here.
    local equipmentCount = equipmentLoader.loadAll("data/equipment")
    unitManager.equipmentClassCount = equipmentCount

    -- Substances (steel, bronze, leather, …). Items that reference a
    -- material name need this registry populated before any combat or
    -- inventory lookup hits substance.get().
    local substanceCount = substanceLoader.loadAll("data/substances")
    unitManager.substanceCount = substanceCount

    engine.logInfo("Unit manager initialized: " .. count
        .. " unit defs, " .. buildingCount .. " building defs, "
        .. itemCount .. " item defs, "
        .. equipmentCount .. " equipment classes")
end

-----------------------------------------------------------
-- Update (called at tick interval)
-----------------------------------------------------------

function unitManager.update(dt)
    -- Future: animation ticks, AI updates dispatched from here
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function unitManager.shutdown()
    unitManager.units = {}
    engine.logInfo("Unit manager shut down")
end

return unitManager
