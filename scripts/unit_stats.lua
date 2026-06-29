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
-- YAML wins so a per-unit override (e.g. a unit with an explicit
-- max_stamina in its YAML) takes precedence over the generic formula.
--
-- Formulas always recompute; no caching. Float math is cheap and
-- recompute keeps the semantics obvious — if endurance changes,
-- max_stamina reflects it on the next read.

local stats = {}

-- Derived formulas. Each entry takes a unit id and returns a number
-- or nil (nil = "this unit type doesn't have the required inputs").
--
-- Phase 2 of the survival plan promoted body composition to live
-- mutable stats: body_mass, lean_mass (skeletal muscle), and fat_mass
-- are now seeded into uiStats at spawn (and mutated by Phase 3/4
-- regrowth/catabolism). The engine recomputes the body-driven derived
-- stats (strength, max_hydration, max_hunger, carrying_capacity) and
-- writes them back into uiStats — so those are now attribute reads,
-- not derived formulas. Only stats that depend on live non-body inputs
-- live in this table.
-- Real-time seconds in one game-day at timeScale 1.0 (1 game-minute
-- per real-second). The metabolism formula yields kcal-per-game-day;
-- dividing by this gives the per-real-second drain rate that
-- unit_resources expects.
local DAY_SECONDS = 1440

-- Activity-based metabolic multipliers (rough real-world physical-
-- activity-level ratios for unloaded effort). Mining and combat are NOT
-- first-class UnitActivity states — the sim only tracks idle / walking /
-- collapsed — so they're detected from the resolved animation name (the
-- pickaxe/shovel work anims, the attack_/combat_ overrides). The
-- anim-derived states win over the locomotion activity: a unit standing
-- still swinging a pick reads activity "idle" but is working hard. This
-- one multiplier is shared by the hunger metabolic burn (metabolism_rate)
-- AND the activity-scaled hydration drain, so both react to exertion the
-- same way. All tunable.
local ACT_MULT_MINING  = 2.0
local ACT_MULT_COMBAT  = 2.5
local ACT_MULT_RUNNING = 1.7
local ACT_MULT_WALKING = 1.5
local ACT_MULT_IDLE    = 1.0

local function activityMultiplier(uid)
    local anim = unit.getCurrentAnim and unit.getCurrentAnim(uid)
    if anim and anim ~= "" then
        if anim:find("pickaxe", 1, true) or anim:find("shovel", 1, true) then
            return ACT_MULT_MINING
        end
        if anim:sub(1, 7) == "attack_" or anim:sub(1, 7) == "combat_" then
            return ACT_MULT_COMBAT
        end
    end
    local activity = unit.getActivity(uid) or "idle"
    if activity == "running" then return ACT_MULT_RUNNING end
    if activity == "walking" then return ACT_MULT_WALKING end
    return ACT_MULT_IDLE
end

-- Exposed so unit_resources (hydration drain) shares the same exertion
-- model the hunger burn uses.
stats.activityMultiplier = activityMultiplier

local derived = {
    -- Stamina pool size = endurance * 10. Larger endurance means
    -- the unit can exert itself longer before stamina runs out.
    max_stamina = function(uid)
        local e = unit.getStat(uid, "endurance")
        return e and e * 10 or nil
    end,

    -- Current metabolic burn rate, kcal per real-second. Activity-
    -- AWARE — the walking 1.5× multiplier is folded in here so every
    -- downstream consumer (hunger drain, Phase 4 catabolism deficit,
    -- info-panel display) reads one authoritative "burn rate" rather
    -- than reapplying activity multipliers in three places.
    --
    -- Base formula: BMR_kcal_per_day = 22 · (body − fat) + 4.5 · fat,
    -- scaled by the per-unit metabolism multiplier. Uses the FULL
    -- non-fat mass (lean + organ = body − fat) as the metabolically
    -- active tissue — lean alone is skeletal muscle, which is only
    -- half of non-fat tissue and would give the wrong BMR.
    --
    -- Activity multipliers are shared with the hydration drain via
    -- activityMultiplier() above (mining 2.0×, combat 2.5×, running
    -- 1.7×, walking 1.5×, everything else 1.0×). Mining/combat are
    -- detected from the unit's resolved animation since they aren't
    -- distinct UnitActivity states.
    --
    -- A default acolyte (body=71.3 kg, fat=14.3 kg, metabolism=1.0)
    -- at idle burns ≈ 1318 kcal/day = 0.92 kcal/real-sec — emptying
    -- the ~1426 kcal hunger pool in ~26 game-hours of idle.
    metabolism_rate = function(uid)
        local body = unit.getStat(uid, "body_mass")
        local fat  = unit.getStat(uid, "fat_mass")
        local m    = unit.getStat(uid, "metabolism")
        if not (body and fat and m) then return nil end
        local actMult = activityMultiplier(uid)
        return (22 * (body - fat) + 4.5 * fat) * m * actMult / DAY_SECONDS
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
