-- Unit Stats Wrapper
--
-- Phase B of the stat system: derived stats live here, not in Haskell.
-- Game code should call `stats.get(uid, name)` instead of `unit.getStat`
-- directly so YAML attributes AND derived formulas are both reachable
-- under one name.
--
-- Dispatch order in stats.get:
--   1. Attribute from YAML (via unit.getStat)            -- YAML wins.
--   2. Derived formula registered in this module.
--   3. nil
--
-- YAML wins so a per-unit override (e.g. "wolf" with an explicit
-- max_stamina in its YAML) takes precedence over the generic formula.
--
-- Formulas always recompute; no caching. Float math is cheap and
-- recompute keeps the semantics obvious — if endurance changes,
-- max_stamina reflects it on the next read.

local stats = {}

-- Derived formulas. Each entry takes a unit id and returns a number
-- or nil (nil = "this unit type doesn't have the required inputs").
-- Add new formulas here as the game needs them.
local derived = {
    -- Stamina pool size = endurance * 10. Larger endurance means
    -- the unit can exert itself longer before stamina runs out.
    max_stamina = function(uid)
        local e = unit.getStat(uid, "endurance")
        return e and e * 10 or nil
    end,

    -- How much weight a unit can carry before encumbrance kicks in.
    carrying_capacity = function(uid)
        local s = unit.getStat(uid, "strength")
        return s and s * 5 or nil
    end,
}

-- Read a single stat. Attribute first, then derived, then nil.
function stats.get(uid, name)
    local v = unit.getStat(uid, name)
    if v ~= nil then return v end

    local fn = derived[name]
    if fn then return fn(uid) end

    return nil
end

-- Return a table containing every attribute defined on the unit AND
-- every derived stat that has the inputs it needs. Useful for the
-- info panel "show me everything" view.
function stats.getAll(uid)
    local all = unit.getAllStats(uid)
    if not all then return nil end

    for name, fn in pairs(derived) do
        if all[name] == nil then    -- YAML override still wins
            local v = fn(uid)
            if v ~= nil then all[name] = v end
        end
    end
    return all
end

-- Expose the derived names so other systems can introspect (e.g. an
-- info panel that wants to label derived stats differently).
function stats.derivedNames()
    local names = {}
    for name, _ in pairs(derived) do
        names[#names + 1] = name
    end
    return names
end

return stats
