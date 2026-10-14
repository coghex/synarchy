-- Physiology consumer facade (#2830, S5 of epic #2827; design D-2, D-7).
--
-- Every GAMEPLAY reader of a registry-owned key (the owned, derived and
-- meter keys of data/physiology/systems.yaml) goes through get / getBase /
-- fraction, and every cross-system CONTRIBUTION to such a key goes through
-- debit / credit. tools/physiology_access_audit.py enforces both.
--
-- INACTIVE: production modes are not active yet (the registry's
-- modesActive flag is false until S4), so every operation here reports
-- TODAY's semantics and produces today's applied writes. The seam exists so
-- S4 can activate the intended behavior in this file and in the engine
-- verbs without migrating another caller:
--   * reads go through unit.readPhysio / unit.readPhysioBase, which answer
--     getStat / getStatBase while inactive and the mode-aware value once
--     active (D-2);
--   * contributions are applied by contributionApplies() below, which is
--     the one place a Nominal system will discard them (D-4, S4).
--
-- Owner-local state updates (a system writing its OWN keys, e.g.
-- salts.tick) stay with the owner; this facade governs reads from, and
-- contributions between, systems.

local stats = require("scripts.unit_stats")

local physiology = {}

-- The owned/derived/meter names of the loaded registry, cached on first use
-- (the registry is loaded once at boot and a different one is refused).
local ownedKeys = nil

local function isOwned(key)
    if ownedKeys == nil then
        local reg = engine.getPhysiologyRegistry and engine.getPhysiologyRegistry()
        if not reg then return false end
        ownedKeys = {}
        for _, sys in pairs(reg.systems) do
            for _, o in ipairs(sys.owns) do ownedKeys[o.key] = true end
            for _, d in ipairs(sys.derived) do ownedKeys[d.key] = true end
            for _, m in ipairs(sys.meters) do ownedKeys[m] = true end
        end
    end
    return ownedKeys[key] == true
end

-- True when `key` belongs to a registry system.
function physiology.isOwned(key) return isOwned(key) end

-- The EFFECTIVE value of `key` and the owning system's mode: `value, mode`.
-- An unowned key reads exactly as stats.get does (the attribute, then the
-- Lua-derived formulas such as max_stamina), with a nil mode. nil for an
-- undefined key, an Unsupported one, or a missing unit.
function physiology.get(uid, key)
    local v, mode = unit.readPhysio(uid, key)
    if v ~= nil then return v, mode end
    if isOwned(key) then return nil, nil end
    return stats.get(uid, key), nil
end

-- The stored BASE of `key` (unit.getStatBase for an unowned key).
function physiology.getBase(uid, key)
    return unit.readPhysioBase(uid, key)
end

-- `get(key) / get(maxKey)`, or nil when either is undefined or the maximum
-- is not positive (a fraction of nothing is undefined). The maximum is read
-- first and the current value only when it is usable, so a unit with no
-- usable maximum never has the current stat read (or lazily rolled).
-- Unclamped: callers clamp.
function physiology.fraction(uid, key, maxKey)
    local mx = physiology.get(uid, maxKey)
    if not mx or mx <= 0 then return nil end
    local cur = physiology.get(uid, key)
    if not cur then return nil end
    return cur / mx
end

-- ---- Contributions ----

-- The one place a contribution may be discarded because the receiving
-- system is Nominal (D-4, S4). Inactive, every contribution applies.
local function contributionApplies(uid, key) return true end

-- Per-key rules for the contributions the migrated callers make. `cap`
-- is the upper bound a credit clamps to (nil = unbounded); `needsCap` makes
-- a credit without a positive bound a no-op; `floor` the lower bound;
-- `absent` the value of a pool the unit has not stored yet.
local function salts() return require("scripts.salts") end

local RULES = {
    hydration = {
        cap      = function(uid) return stats.get(uid, "max_hydration") end,
        needsCap = true,
        absent   = function(uid) return 0 end,
    },
    salt = {
        floor  = 0,
    },
    -- #2761's reserve: starts full, never exceeds its capacity.
    salt_store = {
        cap    = function(uid) return salts().storeCap(uid) end,
        floor  = 0,
        absent = function(uid) return salts().storeCap(uid) end,
    },
}

-- Add `amount` to a pool, clamped to its capacity. Returns the amount
-- applied (the change in the stored value), 0 when nothing was written, or
-- nil, reason for a key with no contribution rule.
function physiology.credit(uid, key, amount)
    local rule = RULES[key]
    if not rule then return nil, "no contribution rule for " .. tostring(key) end
    if not contributionApplies(uid, key) then return 0 end
    local cap = rule.cap and rule.cap(uid)
    if rule.needsCap and (not cap or cap <= 0) then return 0 end
    local old = unit.getStat(uid, key)
    if old == nil then
        if not rule.absent then return 0 end
        old = rule.absent(uid)
    end
    local new = old + amount
    if cap and new > cap then new = cap end
    if rule.floor and new < rule.floor then new = rule.floor end
    unit.setStat(uid, key, new)
    return new - old
end

-- Remove `amount` from a pool, floored at the rule's floor (0). An absent
-- pool is left absent: nothing to debit, returns 0.
function physiology.debit(uid, key, amount)
    local rule = RULES[key]
    if not rule then return nil, "no contribution rule for " .. tostring(key) end
    if not contributionApplies(uid, key) then return 0 end
    local old = unit.getStat(uid, key)
    if old == nil then return 0 end
    local new = old - amount
    local floor = rule.floor or 0
    if new < floor then new = floor end
    unit.setStat(uid, key, new)
    return old - new
end

return physiology
