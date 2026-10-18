-- Physiology facade (#2830 S5 migration, #2831 S4 activation; epic #2827,
-- design D-2, D-4, D-5, D-7).
--
-- Every GAMEPLAY reader of a registry-owned key (the owned, derived and
-- meter keys of data/physiology/systems.yaml) goes through get / getBase /
-- fraction, every cross-system CONTRIBUTION to such a key goes through
-- debit / credit, and every MODE transition goes through setMode /
-- clearMode. tools/physiology_access_audit.py enforces all three.
--
-- ACTIVE (S4): the answers are mode-aware.
--   * reads go through unit.readPhysio / unit.readPhysioBase: a Nominal
--     system reads its healthy values, an Unsupported one reads nil, a
--     Simulated one reads its stored state (D-2);
--   * a contribution to a pool whose system is not Simulated is
--     DISCARDED (D-4): debit and credit apply 0, the receiver's mode
--     being the only thing that decides it (contributionApplies below);
--   * setMode / clearMode run the engine's atomic transition, then the
--     transient-cache resets and the change hooks (only on an EFFECTIVE
--     change) before returning (D-5, D-5a). Nothing is refreshed: the values
--     that read physiology (circulation since #2836, consciousness,
--     concentration and state_of_mind since #2838) are derived on demand,
--     never cached, so there is no cache to bring up to date and a
--     transition or a load publishes no stored value of its own.
--   * writes of the mind's OWN durable keys are atomic batches
--     (physiology.commit, #2838): one validated unit.commitPhysio per
--     writer event, so a reader on another thread sees a complete state.
--
-- STAMINA (#2834, S8a): its pool is the one a second thread spends, so a
-- debit or credit on it is a RELATIVE, atomic commit of an EFFECTIVE delta
-- through unit.commitStamina (never a read-modify-write), a Nominal receiver
-- discards the contribution, and the SD-20 combat-engagement marker is
-- applied here (physiology.applyEngagement, the only caller of the
-- facade-only unit.applyCombatEngagement).
--
-- Owner-local state updates (a system writing its OWN keys, e.g.
-- salts.tick) stay with the owner; this facade governs reads from,
-- contributions between, and mode changes of, systems.

local stats = require("scripts.unit_stats")
local effects = require("scripts.unit_effects")

local physiology = {}

-- Refused batches already logged, once per unit per reason (session-transient,
-- cleared with the engagement diagnostics in resetSessionState).
local reportedRefusal = {}

-- The owned/derived/meter names of the loaded registry and the system each
-- belongs to, cached on first use (the registry is loaded once at boot and a
-- different one is refused).
local keySystem = nil
local systemOrder = nil
local systemOwned = nil     -- system id -> its owned (stored) keys

local function loadRegistry()
    if keySystem ~= nil then return keySystem end
    local reg = engine.getPhysiologyRegistry and engine.getPhysiologyRegistry()
    if not reg or not reg.systems then return nil end
    keySystem = {}
    systemOrder = {}
    systemOwned = {}
    for _, sid in ipairs(reg.systemOrder or {}) do
        systemOrder[#systemOrder + 1] = sid
    end
    for sid, sys in pairs(reg.systems) do
        systemOwned[sid] = {}
        for _, o in ipairs(sys.owns) do
            keySystem[o.key] = sid
            systemOwned[sid][#systemOwned[sid] + 1] = o.key
        end
        for _, d in ipairs(sys.derived) do keySystem[d.key] = sid end
        for _, m in ipairs(sys.meters) do keySystem[m] = sid end
    end
    return keySystem
end

local function isOwned(key)
    local ks = loadRegistry()
    return ks ~= nil and ks[key] ~= nil
end

-- True when `key` belongs to a registry system.
function physiology.isOwned(key) return isOwned(key) end

-- The system id that owns `key`, or nil.
function physiology.systemOf(key)
    local ks = loadRegistry()
    return ks and ks[key] or nil
end

-- The stored keys a system owns (its pools): what a transition rewrites.
function physiology.ownedKeys(system)
    loadRegistry()
    return systemOwned and systemOwned[system] or {}
end

-- The registry's system ids in TICK ORDER (data/physiology/systems.yaml:
-- thermo before salt, then the pool systems).
function physiology.systemOrder()
    loadRegistry()
    return systemOrder or {}
end

-- The unit's resolved mode for one system: "nominal" | "simulated", or nil
-- when the system is Unsupported for it (or unknown, or the unit is gone).
function physiology.mode(uid, system)
    return unit.getPhysioMode(uid, system)
end

-- Every system's resolved mode for one unit, `{ [system] = mode }`, read
-- once so a tick decides all of its gates from one consistent answer.
function physiology.modes(uid)
    local out = {}
    for _, sid in ipairs(physiology.systemOrder()) do
        out[sid] = unit.getPhysioMode(uid, sid)
    end
    return out
end

-- True while the system is Simulated for this unit.
function physiology.isSimulated(uid, system)
    return unit.getPhysioMode(uid, system) == "simulated"
end

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

-- The one place a contribution is discarded because the RECEIVING system
-- is not Simulated (D-4): Nominal state is not stored, so there is
-- nothing to add to or remove from, and an Unsupported system has none.
local function contributionApplies(uid, key)
    local sid = physiology.systemOf(key)
    if sid == nil then return true end
    return unit.getPhysioMode(uid, sid) == "simulated"
end

-- Would a contribution to `key` be applied for this unit (its receiving system
-- is Simulated, or no registry system owns the key)? The same answer credit,
-- debit and creditBatch act on, exposed for a caller that reports what its
-- credit did (consumable.drink's summary).
function physiology.contributionApplies(uid, key)
    return contributionApplies(uid, key)
end

-- Per-key rules for the contributions the migrated callers make. `cap`
-- is the upper bound a credit clamps to (nil = unbounded); `needsCap` makes
-- a credit without a positive bound a no-op; `floor` the lower bound;
-- `absent` the value of a pool the unit has not stored yet.
local function salts() return require("scripts.salts") end

local RULES = {
    -- Stamina is committed engine-side (see debit/credit below).
    stamina = { commit = true },
    hydration = {
        cap      = function(uid) return stats.get(uid, "max_hydration") end,
        needsCap = true,
        absent   = function(uid) return 0 end,
    },
    salt = {
        floor  = 0,
    },
    -- Exhaustion and sleep pressure (#2835). No shipped caller contributes to
    -- either yet, but the seam exists so that one that does is discarded by a
    -- Nominal or Unsupported receiver (D-4) and clamped to the pool's
    -- maximum by a Simulated one. A contribution is an EFFECTIVE delta: the
    -- stored base is solved so the effective value lands on the target under
    -- the pool's retained modifiers (`invert`), never `base + delta`.
    exhaustion = {
        cap      = function(uid) return stats.get(uid, "max_exhaustion") end,
        needsCap = true,
        invert   = true,
    },
    sleep_pressure = {
        cap      = function(uid) return stats.get(uid, "max_sleep_pressure") end,
        needsCap = true,
        invert   = true,
    },
    -- #2761's reserve: starts full, never exceeds its capacity.
    salt_store = {
        cap    = function(uid) return salts().storeCap(uid) end,
        floor  = 0,
        absent = function(uid) return salts().storeCap(uid) end,
    },
}

-- A contribution to stamina: ONE relative commit of an EFFECTIVE delta in the
-- unit-manager update combat's strike cost shares, so it can never refund an
-- intervening spend (S8-F2). Returns the EFFECTIVE change (after - before).
-- A receiver that holds no pool (Nominal, Unsupported) or a dead one takes
-- nothing; anything else refused is a broken invariant and raises.
local function commitStaminaContribution(uid, delta)
    local res, reason = effects.commitStamina(uid, delta)
    if res == nil then
        if reason == "nominal" or reason == "unsupported" or reason == "dead"
           or reason == "no_such_unit" then
            return 0
        end
        error("stamina contribution refused for unit " .. tostring(uid)
              .. ": " .. tostring(reason))
    end
    return res.after - res.before
end

-- Move a Simulated pool's EFFECTIVE value by `amount` (a debit passes a
-- negative one), clamped to [0, cap], and answer the EFFECTIVE change
-- actually made (read back, so binary32 storage and the zero clamp are
-- reported as they landed). The stored base is the one whose effective value
-- is the target under the pool's active modifiers (scripts/lib/stat_base.lua:
-- `t / (1 + sum p) - sum d`): writing `base + amount` would be wrong by the
-- multiplier and the flat part. 0, with nothing written, when no
-- representable base exists (a multiplier that is not positive, or a
-- negative or non-finite base): the same refusals a transition's seed makes.
local function contributeEffective(uid, key, rule, amount)
    local cap = rule.cap(uid)
    if not cap or cap <= 0 then return 0 end
    -- A Simulated pool is always stored; the owned read raises when it is not.
    local old = unit.readPhysio(uid, key)
    if old == nil then return 0 end
    local target = math.min(cap, math.max(0, old + amount))
    local base = require("scripts.lib.stat_base").baseFor(uid, key, target)
    if base == nil then return 0 end
    effects.setStat(uid, key, base)
    local after = unit.readPhysio(uid, key)
    return after ~= nil and (after - old) or 0
end

-- Add `amount` to a pool, clamped to its capacity. Returns the amount
-- applied (the change in the stored value; for the pools a modifier can
-- scale, the change in the EFFECTIVE value), 0 when nothing was written, or
-- nil, reason for a key with no contribution rule.
function physiology.credit(uid, key, amount)
    local rule = RULES[key]
    if not rule then return nil, "no contribution rule for " .. tostring(key) end
    if not contributionApplies(uid, key) then return 0 end
    if rule.commit then return commitStaminaContribution(uid, amount) end
    if rule.invert then return contributeEffective(uid, key, rule, amount) end
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
    effects.setStat(uid, key, new)
    return new - old
end

-- Remove `amount` from a pool, floored at the rule's floor (0). An absent
-- pool is left absent: nothing to debit, returns 0.
function physiology.debit(uid, key, amount)
    local rule = RULES[key]
    if not rule then return nil, "no contribution rule for " .. tostring(key) end
    if not contributionApplies(uid, key) then return 0 end
    if rule.commit then return -commitStaminaContribution(uid, -amount) end
    if rule.invert then return -contributeEffective(uid, key, rule, -amount) end
    local old = unit.getStat(uid, key)
    if old == nil then return 0 end
    local new = old - amount
    local floor = rule.floor or 0
    if new < floor then new = floor end
    effects.setStat(uid, key, new)
    return old - new
end

-- ---- Atomic batches ----

-- Commit ONE validated batch of stored keys (`{ key = value, ... }`, stored
-- bases with unit.setStat's semantics) in a single unit-manager update, or
-- none (#2838, D-5a's deferral). Returns `true`, or `nil, reason` for a
-- refused batch (a missing unit, an empty or malformed batch, a non-finite
-- value, a key the write gate refuses). It NEVER raises: a refusal inside
-- the pcall'd per-unit pass would skip that unit's injuries, failure meters
-- and pools for the pass, so the caller reads the result and publishes no
-- consequence of an event that did not commit.
function physiology.commit(uid, batch)
    local ok, reason = effects.commitPhysio(uid, batch)
    if ok == nil then
        local key = tostring(uid) .. ":" .. tostring(reason)
        if not reportedRefusal[key] then
            reportedRefusal[key] = true
            engine.logWarn("physiology: unit " .. tostring(uid)
                .. " batch refused (" .. tostring(reason) .. ")")
        end
        return nil, reason
    end
    return true
end

-- Credit one SIMULATED system's stored keys together: each item moves its key
-- by `amount` (the EFFECTIVE read plus the amount, clamped to [lo, hi]), and
-- the lot is one atomic batch. A receiver that is not Simulated (Nominal
-- holds no state, Unsupported has none) DISCARDS the whole credit (D-4) and
-- applies nothing. Returns the table `{ [key] = applied change }` (all
-- zero when discarded or refused). `items` is a list of
-- `{ key =, amount =, lo =, hi = }`, all keys of one system.
function physiology.creditBatch(uid, items)
    local applied = {}
    for _, it in ipairs(items) do applied[it.key] = 0 end
    if #items == 0 then return applied end
    if not contributionApplies(uid, items[1].key) then return applied end
    local batch, olds = {}, {}
    for _, it in ipairs(items) do
        local old = physiology.get(uid, it.key)
        if old == nil then return applied end
        local new = old + it.amount
        if it.hi and new > it.hi then new = it.hi end
        if it.lo and new < it.lo then new = it.lo end
        olds[it.key] = old
        batch[it.key] = new
    end
    if not physiology.commit(uid, batch) then return applied end
    for key, new in pairs(batch) do applied[key] = new - olds[key] end
    return applied
end

-- ---- Transitions ----

-- fn(uid, system, fromMode, toMode) per registered hook, run only on an
-- EFFECTIVE mode change (never for a value write or a set equal to the
-- effective mode). Hooks cancel or rebuild whatever a mode change makes
-- stale outside this facade (an AI latch, a debounce, a regrowth author).
local hooks = {}

-- Register a change hook for one system (`nil` = every system). Idempotent
-- per `id`, so a reloaded script replaces its hook instead of stacking.
function physiology.onModeChanged(id, system, fn)
    hooks[id] = { system = system, fn = fn }
end

-- Drop the transient state that is a remainder or a debounce OF the stored
-- values `keys` of one system (#2633): the carry remainder and the alert
-- flag of each key, and, for nutrition, the regrowth authorship of the body
-- composition. Scoped to what actually changed: another system's remainders
-- and debounce are not touched, and a call that changed nothing never gets
-- here.
local function resetTransientState(uid, system, keys)
    local carry  = require("scripts.unit_resource_carry")
    local alerts = require("scripts.unit_resource_alerts")
    for _, key in ipairs(keys) do
        carry.forget(uid, key)
        alerts.forget(uid, key)
    end
    if system == "nutrition" then
        require("scripts.unit_resource_regrowth").forget(uid)
    end
end

-- The resource parameters a Simulated system needs exist (species entry
-- first, then the registry default), or why not.
local function parametersAvailable(uid, system)
    local info = unit.getInfo(uid)
    if not info then return false, "no such unit" end
    return require("scripts.unit_resource_params")
        .available(info.defName, system)
end

-- After the engine accepted a transition: reset what the change invalidated
-- and run the change hooks (only for an EFFECTIVE change). `values` is the
-- caller's table, or nil. A set that changed no mode and wrote no value (an
-- unchanged pin, a clear with nothing to clear) is not an event: it resets
-- nothing and runs no hook.
local function finish(uid, system, changed, from, to, values)
    -- The keys the caller actually wrote. An empty table writes none, so it is
    -- no value write at all.
    local written = nil
    if values ~= nil then
        written = {}
        for key in pairs(values) do written[#written + 1] = key end
        if #written == 0 then written = nil end
    end
    if not changed and written == nil then return end
    resetTransientState(uid, system, changed and physiology.ownedKeys(system) or written)
    if changed then
        for _, h in pairs(hooks) do
            if h.system == nil or h.system == system then
                h.fn(uid, system, from, to)
            end
        end
    end
end

-- ---- Combat engagement (SD-20, #2834) ----

-- Diagnostics already logged for a refused engagement seed, once per unit per
-- refusal reason. Session-transient: unit ids are reused after a load or an
-- Exit to Menu, so unit_resources clears it through its reset hook and its
-- session-teardown registration (resetSessionState).
local reportedEngagement = {}

function physiology.resetSessionState()
    reportedEngagement = {}
    reportedRefusal = {}
end

-- Apply a unit's PENDING combat engagement (combat_engaged == 1): one atomic
-- engine update that, for a unit with no explicit stamina override, seeds the
-- full effective maximum, pins a Simulated override and marks the engagement
-- applied; with an explicit override of either mode it only latches the
-- marker. Returns the engine's outcome: "transitioned" | "latched" |
-- "refusedDead" | "seedUnrepresentable", or nil, reason for a unit with
-- nothing pending. On "transitioned" the change hooks and the refresh run
-- before returning (D-5a), exactly as for setMode. A seed D-5's guards refuse
-- (a retained modifier makes the maximum unrepresentable) leaves the unit
-- pending and Nominal and is logged ONCE per unit and reason; it is retried
-- on every eligible pass and succeeds once the blocker is gone.
function physiology.applyEngagement(uid)
    local outcome, code = effects.applyCombatEngagement(uid)
    if outcome == "transitioned" then
        finish(uid, "stamina", true, "nominal", "simulated", nil)
    elseif outcome == "seedUnrepresentable" then
        local key = tostring(uid) .. ":" .. tostring(code)
        if not reportedEngagement[key] then
            reportedEngagement[key] = true
            engine.logWarn("physiology: unit " .. tostring(uid)
                .. " cannot apply its combat engagement (" .. tostring(code)
                .. "): stamina stays Nominal until the blocking modifier is gone")
        end
    end
    return outcome, code
end

-- Set a system's mode, recording the override (even one equal to the
-- species default) and seeding or scrubbing its state when the EFFECTIVE
-- mode changes. `values` optionally gives EFFECTIVE targets for the
-- Simulated system's owned keys ({ hydration = 20 }). Returns
-- `true, changed` or `nil, reason`; a refusal changes nothing.
function physiology.setMode(uid, system, mode, values)
    if mode == "simulated" then
        local ok, why = parametersAvailable(uid, system)
        if not ok then return nil, why end
    end
    local ok, changed, from, to = effects.setPhysioMode(uid, system, mode, values)
    if not ok then return nil, changed end
    finish(uid, system, changed, from, to, values)
    return true, changed
end

-- Remove the system's override, so the unit follows its species default
-- again. Same return shape and refusal behavior as setMode.
function physiology.clearMode(uid, system)
    local info = unit.getInfo(uid)
    if info and unit.getPhysioDefault(uid, system) == "simulated" then
        local ok, why = parametersAvailable(uid, system)
        if not ok then return nil, why end
    end
    local ok, changed, from, to = effects.clearPhysioMode(uid, system)
    if not ok then return nil, changed end
    finish(uid, system, changed, from, to, nil)
    return true, changed
end

return physiology
