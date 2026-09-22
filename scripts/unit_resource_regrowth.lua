-- Sub-precision body composition for surplus regrowth (#2637).
--
-- fat_mass, lean_mass, body_mass and the calorie store live in uiStats
-- as binary32. applyRegrowth's per-tick tissue deltas are often smaller
-- than half a step at ordinary body sizes, so writing the sum of the
-- stored float and the delta rounded back to the same float every tick:
-- idle regrowth grew fat and spent calories while lean mass and total
-- mass never moved, and walking's fat burn disappeared the same way.
-- Each stat rounded on its own, so body_mass drifted off
-- fat_mass + lean_mass + the implicit organ remainder.
--
-- This module keeps two pieces of transient state per unit, and nothing
-- else:
--
--   * The sub-binary32 residue of the last fat, lean and body write.
--     The next tick starts from stored + residue, so integration is
--     exact in doubles and storage is that value's correctly rounded
--     image. The residue is only ever the last write's rounding; a
--     caller that stops ticking loses at most half an ulp, and a clamp
--     to 0 discards the part the tissue could not absorb instead of
--     banking it as debt.
--   * The organ baseline: body − fat − lean at the moment regrowth
--     takes authorship of this composition, in doubles. It is NOT
--     recomputed from the quantized stats on later ticks. Doing that
--     would fold each tick's rounding back into "organ" and hide the
--     drift this module exists to stop. Historical drift already in
--     the stored stats is the baseline; it is not reconstructed.
--
-- body_mass is not integrated on its own. Every tick writes
-- fat + lean + that baseline, so after quantization the stored total
-- stays within one binary32 step of stored fat + stored lean + organ.
--
-- Authorship is the whole composition or nothing. An external write of
-- fat_mass, lean_mass or body_mass — including one that stores the same
-- float, which a value comparison cannot see — drops the residues and
-- the baseline together. The next tick rebases organ from whatever is
-- stored now and adds only this tick's delta, so an intervening edit
-- is kept and a stale exact total is not written back over it. The
-- write barrier covers Lua unit.setStat; a writer that reaches uiStats
-- without that verb is caught by the stored-value check on the next
-- tick, because the residue is reused only while storage still holds
-- exactly what this module last wrote.
--
-- Calories are not carried here. The store already has an owner
-- (unit_resource_carry); applyRegrowth spends through that so the
-- metabolic drain's residue and this spend share one remainder.
--
-- None of this is serialized. It is meaningless without the stored
-- stats it is a residue of, and a load replaces those wholesale.
-- unit_resources clears it from the save_modules reset hook and from
-- the Exit-to-Menu teardown. The loss is at most half an ulp per stat;
-- the organ baseline is reconstructed from the stored masses on the
-- next regrowth tick, so it does not need its own save component.
-- Entries for a unit that stops being ticked linger, exactly as the
-- resource-carry residues do: they are tiny, and a reused id cannot
-- inherit one after that clear.
local carry = require("scripts.unit_resource_carry")

local M = {}

-- uid → { fat = {stored, residue}, lean = ..., body = ..., organ = n }
local pending = {}

-- Stat names whose write ends composition authorship. Anything else
-- (calories, stamina, a derived stat) must not: the resource tick
-- writes those every cadence, and dropping the baseline there would
-- turn this carry off.
local COMPOSITION = {
    fat_mass = true,
    lean_mass = true,
    body_mass = true,
}

function M.forget(uid, statName)
    if uid == nil then return end
    if statName == nil or COMPOSITION[statName] then
        pending[uid] = nil
    end
end

-- In-place, like unit_resource_carry.resetOnLoad: `pending` is an
-- upvalue of the closures above.
function M.resetOnLoad()
    for k in pairs(pending) do pending[k] = nil end
end

local wrapped = false

-- Idempotent. Safe before the engine API exists: reports false and
-- leaves the wrapper uninstalled so a later call can still install.
function M.installWriteBarrier()
    if type(unit) ~= "table" or type(unit.setStat) ~= "function" then
        return false
    end
    -- Carry wraps the same verb. Install that first so our wrapper
    -- stays outside it: a composition write must drop OUR authorship
    -- and still run carry's own per-stat forget.
    if type(carry.installWriteBarrier) == "function" then
        carry.installWriteBarrier()
    end
    if wrapped then return true end
    local previous = unit.setStat
    unit.setStat = function(uid, statName, ...)
        M.forget(uid, statName)
        return previous(uid, statName, ...)
    end
    wrapped = true
    return true
end

local function exactOf(row, key, current)
    local entry = row and row[key]
    if entry and entry.stored == current then
        return current + entry.residue, true
    end
    return current, false
end

local function writeStat(uid, statName, value)
    unit.setStat(uid, statName, value)
    local stored = unit.getStat(uid, statName)
    if stored == nil then return nil end
    return { stored = stored, residue = value - stored }
end

-- Apply one tick's tissue deltas and write fat, lean and body.
-- `fatDelta` / `leanDelta` are unclamped kilograms. Returns false when
-- the unit has no composition to author (nothing is written).
function M.commitComposition(uid, fatDelta, leanDelta)
    if not M.installWriteBarrier() then return false end
    local body = unit.getStat(uid, "body_mass")
    local lean = unit.getStat(uid, "lean_mass")
    local fat  = unit.getStat(uid, "fat_mass")
    if not (body and lean and fat) then return false end

    local row = pending[uid]
    local fatExact, ownFat = exactOf(row, "fat", fat)
    local leanExact, ownLean = exactOf(row, "lean", lean)
    local bodyExact, ownBody = exactOf(row, "body", body)

    -- Reuse the baseline only while we still authored every part of it.
    -- Otherwise rebase from the exact values we actually hold, which for
    -- a stat we no longer author is just what storage says now.
    local organ
    if row and row.organ ~= nil and ownFat and ownLean and ownBody then
        organ = row.organ
    else
        organ = bodyExact - fatExact - leanExact
    end

    -- Clamp before the residue is taken, so unapplied burn/growth is
    -- not repaid the moment the tissue leaves the bound.
    local fatValue = fatExact + fatDelta
    if fatValue < 0 then fatValue = 0 end
    local leanValue = leanExact + leanDelta
    if leanValue < 0 then leanValue = 0 end
    local bodyValue = fatValue + leanValue + organ
    if bodyValue < 0 then bodyValue = 0 end

    local fatEntry = writeStat(uid, "fat_mass", fatValue)
    local leanEntry = writeStat(uid, "lean_mass", leanValue)
    local bodyEntry = writeStat(uid, "body_mass", bodyValue)
    if not (fatEntry and leanEntry and bodyEntry) then
        M.forget(uid)
        return false
    end
    pending[uid] = {
        fat = fatEntry,
        lean = leanEntry,
        body = bodyEntry,
        organ = organ,
    }
    return true
end

M.installWriteBarrier()

return M
