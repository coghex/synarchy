-- The side effects of the per-unit resource pass (unit_resources), behind one
-- seam (#2710, SCN-10).
--
-- EVERY mutation the pass makes goes through this module: the stat writes
-- (setStat, recomputeBody), the two engine commits (commitStamina,
-- commitPhysio), the physiology mode verbs, frostbite, stance recovery, the
-- equipment drop, the combat engagement, the four pose verbs (kill, collapse,
-- crawl, revive), the death and survival alerts and the unit event feed. In
-- normal play each is exactly the existing verb, called with the same
-- arguments, so the tick is unchanged. A static Hspec check ("Scenario.Units",
-- "the readiness seam") fails if a pass module calls a mutating verb directly.
--
-- A scenario unit's paused readiness runs the SAME pass at zero elapsed time
-- (unit_resources.settleScenarioUnit) as a PURE evaluation of the authored
-- state: M.record(uid, fn) runs fn with a recorder installed for that one
-- unit, and none of that unit's mutations is performed. Stat writes and body
-- recomputes are recorded (writes, last value per key); the engine commits
-- answer from their read-only previews (unit.previewStamina /
-- unit.previewPhysio: the commits' own pure steps, run without publishing);
-- the pose verbs record the verdict in queue order; frostbite, stance
-- recovery, the drop, the engagement and the mode verbs are recorded and
-- refused; alerts and events are not emitted (and an alert's debounce state
-- is untouched, so the first real pass still warns). Every other unit's
-- effects pass through unchanged. The recorder is removed even when fn raises.
local M = {}

local recorder = nil

local function rec(uid)
    if recorder and recorder.uid == uid then return recorder end
    return nil
end

function M.kill(uid)
    local r = rec(uid)
    if r then r.dead = true; return true end
    return unit.kill(uid)
end

function M.collapse(uid)
    local r = rec(uid)
    if r then r.pose = "collapsed"; return true end
    return unit.collapse(uid)
end

function M.crawl(uid)
    local r = rec(uid)
    if r then r.pose = "crawling"; return true end
    return unit.crawl(uid)
end

function M.revive(uid)
    local r = rec(uid)
    if r then r.pose = "standing"; return true end
    return unit.revive(uid)
end

function M.deathAlert(uid, cause, channel)
    local r = rec(uid)
    if r then r.cause = r.cause or cause; return end
    return require("scripts.unit_resource_alerts").emitDeathAlert(uid, cause, channel)
end

function M.survivalAlert(uid, resourceName, value, maxVal, pose, info)
    if rec(uid) then return end
    return require("scripts.unit_resource_alerts")
        .checkSurvivalAlerts(uid, resourceName, value, maxVal, pose, info)
end

function M.unitEvent(kind, msg, uid, gx, gy)
    if rec(uid) then return end
    if gx and gy then return engine.emitEventForUnit(kind, msg, uid, gx, gy) end
    return engine.emitEventForUnit(kind, msg, uid)
end

function M.setStat(uid, key, value)
    local r = rec(uid)
    if r then r.writes[key] = value; return true end
    return unit.setStat(uid, key, value)
end

function M.recomputeBody(uid)
    local r = rec(uid)
    if r then r.recompute = true; return true end
    return unit.recomputeBody(uid)
end

function M.commitStamina(uid, amount)
    if rec(uid) then return unit.previewStamina(uid, amount) end
    return unit.commitStamina(uid, amount)
end

function M.commitPhysio(uid, batch)
    local r = rec(uid)
    if r then
        local ok, reason = unit.previewPhysio(uid, batch)
        if ok then for k, v in pairs(batch) do r.writes[k] = v end end
        return ok, reason
    end
    return unit.commitPhysio(uid, batch)
end

-- The verbs readiness never performs: recorded by name, refused as a
-- no-op the caller already handles.
local function refused(r, verb)
    r.refused[#r.refused + 1] = verb
end

function M.frostbite(uid, part, necrosis)
    local r = rec(uid)
    if r then refused(r, "frostbite"); return nil, "readiness" end
    return unit.frostbite(uid, part, necrosis)
end

function M.recoverStance(uid, amount)
    local r = rec(uid)
    if r then refused(r, "recoverStance"); return 0 end
    return unit.recoverStance(uid, amount)
end

function M.dropEquipmentToGround(uid, slot)
    local r = rec(uid)
    if r then refused(r, "dropEquipmentToGround"); return false end
    return unit.dropEquipmentToGround(uid, slot)
end

function M.applyCombatEngagement(uid)
    local r = rec(uid)
    if r then refused(r, "applyCombatEngagement"); return nil, "readiness" end
    return unit.applyCombatEngagement(uid)
end

function M.setPhysioMode(uid, system, mode, values)
    local r = rec(uid)
    if r then refused(r, "setPhysioMode"); return nil, "readiness" end
    return unit.setPhysioMode(uid, system, mode, values)
end

function M.clearPhysioMode(uid, system)
    local r = rec(uid)
    if r then refused(r, "clearPhysioMode"); return nil, "readiness" end
    return unit.clearPhysioMode(uid, system)
end

-- Run fn with uid's effects recorded. Returns the record:
-- { dead = true|nil, cause = <first death cause>|nil, pose = <last pose>|nil,
--   writes = { key = last value written }, recompute = true|nil,
--   refused = { verb, ... } }.
function M.record(uid, fn)
    local outer = recorder
    recorder = { uid = uid, writes = {}, refused = {} }
    local ok, err = pcall(fn)
    local r = recorder
    recorder = outer
    if not ok then error(err, 0) end
    return r
end

return M
