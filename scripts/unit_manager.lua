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
    engine.logInfo("Unit manager initializing (YAML loads deferred to startup_loader)")
    -- Unit / building / item / equipment / substance YAMLs are now
    -- loaded by scripts/startup_loader.lua during the boot loading
    -- screen, so this init no longer eagerly parses them. The
    -- catalogs are still populated before any menu is shown
    -- (startup_loader finishes before showMenu("main") runs).
    unitManager.defsLoaded = true
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
