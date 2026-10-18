-- The side effects of the per-unit resource pass (unit_resources), behind one
-- seam (#2710, SCN-10).
--
-- The pass decides a unit's death, collapse, crawl and revival and tells the
-- player about it. Every one of those effects goes through this module, which
-- in normal play is exactly the existing verb: unit.kill / unit.collapse /
-- unit.crawl / unit.revive (queued engine commands), the death and survival
-- alerts, and the unit event feed.
--
-- A scenario unit's paused readiness runs the SAME pass at zero elapsed time
-- (unit_resources.settleScenarioUnit) and must learn its verdict now rather
-- than queue it or announce it. M.record(uid, fn) runs fn with a recorder
-- installed for that one unit: its verbs are recorded in the order the
-- queue would have applied them, and its alerts and events are not emitted
-- (an alert's debounce state is untouched too, so the first real pass still
-- warns). Every other unit's effects pass through unchanged. The recorder is
-- removed even when fn raises.
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

-- Run fn with uid's effects recorded. Returns the record:
-- { dead = true|nil, cause = <first death cause>|nil, pose = <last pose>|nil }.
function M.record(uid, fn)
    local outer = recorder
    recorder = { uid = uid }
    local ok, err = pcall(fn)
    local r = recorder
    recorder = outer
    if not ok then error(err, 0) end
    return r
end

return M
