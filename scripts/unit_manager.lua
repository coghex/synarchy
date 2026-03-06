-- Unit Manager - loads unit definitions and manages live unit instances.
-- Loaded as an engine script from init.lua with its own tick interval.
local unitLoader = require("scripts.unit_loader")

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

    engine.logInfo("Unit manager initialized: " .. count .. " definitions loaded")
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
