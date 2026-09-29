-- Species-independent fallbacks for unit-AI CONFIG keys (#2753).
--
-- A species config (scripts/unit_ai_tunables.lua, or a satellite
-- script's unitAi.setConfig) is threaded as `params` into every action
-- it runs -- and into the mental-state short-circuit
-- (scripts/unit_ai_mental.lua), which is not an action the species
-- chose. Delirium, and a mental break's wander, panic with no one
-- nearby and lash-out with no eligible target, all stumble through
-- needs.wanderExecute on the unit's OWN config, so any def the AI
-- dispatches can reach the wander sampler whether or not its config
-- ever mentions wandering. nomad_primitive's (unit_ai_encounter.lua)
-- does not, and a delirious ruin occupant used to raise on the nil
-- `params.wander_radius` inside unitAi.update, aborting the rest of
-- that tick's units.
--
-- So each such key has ONE fallback, declared here and installed as the
-- config table's __index: a def's own value always wins, and a def that
-- never wanders need not restate it.
--
-- Not to be confused with scripts/unit_ai_defaults.lua, which fills
-- RUNTIME fields of a unit's aiState row; these are per-def tunables.

local M = {}

M.FALLBACK = {
    -- Tiles. The acolyte's radius -- the humanoid baseline every
    -- mental-state wander leg was written against. Shipped configs set
    -- their own (acolyte 5, technomule 3, bear 8, red squirrel 6).
    wander_radius = 5.0,
}

local fallbackMeta = { __index = M.FALLBACK }

-- Install the fallbacks under `cfg` (in place) and return it. A table
-- that already carries a metatable is left as it is.
function M.apply(cfg)
    if type(cfg) == "table" and getmetatable(cfg) == nil then
        setmetatable(cfg, fallbackMeta)
    end
    return cfg
end

-- Every def of a whole config table (the tunables module's).
function M.applyAll(configs)
    for _, cfg in pairs(configs) do M.apply(cfg) end
    return configs
end

return M
