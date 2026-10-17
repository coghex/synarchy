-- Resource parameters for the pool systems (#2831, S4 of epic #2827;
-- design D-9 "Parameters").
--
-- A Simulated pool system (hydration, nutrition) ticks its pools with the
-- SPECIES' entry in scripts/unit_resource_config.lua when it has one, and
-- otherwise with the system's DEFAULT parameters below. The defaults are
-- the acolyte's mass-proportional values: every rate is a fraction of the
-- pool's own mass-derived maximum, so they scale to any body. Referencing
-- the acolyte entry keeps the two from drifting apart.
--
-- (Stamina is gated the same way since #2834, and exhaustion and sleep
-- pressure since #2835: each ticks only while its system is Simulated, with
-- the species entry or, for a species with none, the acolyte's.)
--
-- A species with neither (and a system that needs parameters) cannot run
-- the system Simulated: scripts/physiology.lua's setMode refuses the
-- transition atomically (it never reaches the engine), so a unit is never
-- left Simulated with nothing to tick it.

local config = require("scripts.unit_resource_config")

local M = {}

-- The pool resources each GATED system ticks, and the system that owns
-- each. A resource named here ticks only while its system is Simulated;
-- every other config entry is unowned and ticks as always. The system id
-- need not match the pool key: the sleep system owns sleep_pressure.
M.POOLS = {
    hydration  = { "hydration" },
    nutrition  = { "hunger", "calories" },
    stamina    = { "stamina" },
    exhaustion = { "exhaustion" },
    sleep      = { "sleep_pressure" },
}

M.SYSTEM_OF = {
    hydration      = "hydration",
    hunger         = "nutrition",
    calories       = "nutrition",
    stamina        = "stamina",
    exhaustion     = "exhaustion",
    sleep_pressure = "sleep",
}

-- The registry's default parameters, per pool resource.
M.DEFAULTS = {
    hydration      = config.acolyte.hydration,
    hunger         = config.acolyte.hunger,
    calories       = config.acolyte.calories,
    stamina        = config.acolyte.stamina,
    exhaustion     = config.acolyte.exhaustion,
    sleep_pressure = config.acolyte.sleep_pressure,
}

-- The system a resource belongs to, or nil for an unowned resource.
function M.system(resourceName) return M.SYSTEM_OF[resourceName] end

-- The parameters a unit definition ticks `resourceName` with: its own
-- entry, else the default; nil when neither exists.
function M.paramsFor(defName, resourceName)
    local species = config[defName]
    local own = species and species[resourceName]
    if own ~= nil then return own end
    return M.DEFAULTS[resourceName]
end

-- Can `system` run Simulated for this definition? True for a system with
-- no pools (thermo, salt carry their own parameters in their modules);
-- otherwise every pool needs parameters. `false, reason` names the pool.
function M.available(defName, system)
    local pools = M.POOLS[system]
    if pools == nil then return true end
    for _, resourceName in ipairs(pools) do
        if M.paramsFor(defName, resourceName) == nil then
            return false, "system '" .. system .. "' has no resource "
                .. "parameters for '" .. resourceName .. "' (neither a "
                .. "species entry for '" .. tostring(defName)
                .. "' nor a registry default)"
        end
    end
    return true
end

return M
