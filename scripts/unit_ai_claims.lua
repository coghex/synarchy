-- Transient unit-AI coordination registries (#1329).
--
-- Eleven module-local tables coordinate unit AI transiently: the
-- five coordinate claim registries (dig, chop, construct, till, plant),
-- repairClaims and repairPriority, plus #916's four same-tick encounter
-- episode overlays. None of them is persisted, and
-- until #1329 nothing cleared them when a load replaced the session.
-- Both id allocators REWIND across a load -- World/Load/Publish.hs
-- assigns nextItemInstanceIdRef straight from the save, and
-- unit_resources.lua records the same for umNextId -- so a claim, a
-- claimant uid, or a repair-priority flag written by session A could
-- attach to an unrelated entity in session B. Worse, all six timed
-- tables expire on `now - c.at > timeout`, which never fires while the
-- loaded clock is EARLIER than the session that wrote the claim.
--
-- Two jobs, both small enough to share one module:
--
--   * track(t) enrolls a table in the load reset; resetAll() empties
--     every enrolled table IN PLACE. The owning modules hold theirs as
--     closure upvalues, and unitAi.till.claims / unitAi.plant.claims are
--     public fields, so reassigning would strand every live reader on
--     the old table. Content-data caches are deliberately NOT enrolled
--     (packBuildCache in unit_ai_construct.lua, foodDefCache in
--     unit_ai_needs.lua): they hold YAML-derived facts identical across
--     sessions, so clearing them would only re-read the same files.
--
--   * key(wid, x, y) is the page-qualified coordinate claim key. A bare
--     "x,y" is ONE slot shared by every page's tile at that coordinate.
--     That is not reachable through concurrent ticking -- unit.getAllIds
--     resolves to activeUnits, the active page only -- but it IS
--     reachable across a page SWITCH: these tables are never cleared when
--     the active page changes, and unit.exists is session-global rather
--     than page-scoped, so the departed page's claimant still reads as
--     alive for the whole claim timeout. Inside that window, given the
--     same (x,y) designated on both pages, the newly-active page's
--     designation is unclaimable, and if both pages hold a job there the
--     second claimant overwrites the shared entry -- the first unit's
--     next tick then sees c.uid ~= uid and drops its in-progress
--     construct job, losing accumulated work.
--
-- `wid` is world.getActiveWorldId()'s page id, resolved at each call site
-- rather than stored on the job record. Job records ride the lua.unit_ai
-- save component, so deriving the page transiently leaves that schema
-- (and its save-compat fixtures) untouched -- and it is exact, because
-- every tick path that reads or writes a claim runs for a unit on the
-- ACTIVE page, which is therefore the job's own page. A job left behind
-- on a page the player switched away from is simply never ticked; its
-- claim ages out on the existing timeout, blocking nothing on any other
-- page now that the key names its own.
--
-- The reset hook itself is registered by unit_ai_save.lua under its own
-- id: "unit_ai" already belongs to the persistent component, and
-- saveModules refuses a reset-hook/component id collision.
-----------------------------------------------------------

local M = {}

-- Enrolled transient tables, in registration order.
local tracked = {}

-- Enroll `t` in the load reset and hand it straight back, so an owner
-- can write `local claims = claimsLib.track({})` and keep one binding.
function M.track(t)
    tracked[#tracked + 1] = t
    return t
end

-- Page-qualified claim key. x and y are always numbers here (canonical
-- integer tile coords -- see unit_ai.lua's seam contract), so the ":"
-- separator stays unambiguous however a page id is spelled: everything
-- past the LAST ":" is "<number>,<number>", which no page id can absorb.
function M.key(wid, x, y)
    return tostring(wid) .. ":" .. x .. "," .. y
end

-- Page-qualified IDENTITY claim key (#1854). Chop claims name the
-- exact PLANT being felled rather than the tile it stands on, because
-- two wood-tagged trees can legitimately share one tile and a tile key
-- would let one acolyte's claim block the other's tree. `iid` is a
-- world.getFloraAt().instanceId -- always a number -- so the ":"
-- separator stays unambiguous however a page id is spelled, exactly as
-- M.key's own note explains.
function M.instanceKey(wid, iid)
    return tostring(wid) .. ":#" .. iid
end

-- Empty every enrolled table in place; returns how many entries were
-- dropped, for the caller's load diagnostic.
function M.resetAll()
    local dropped = 0
    for _, t in ipairs(tracked) do
        for k in pairs(t) do
            t[k] = nil
            dropped = dropped + 1
        end
    end
    return dropped
end

-- How many tables are enrolled. Exists for the load-reset regression,
-- which pins the count so a newly added claim registry that forgets to
-- call track() is caught rather than silently surviving loads.
function M.trackedCount()
    return #tracked
end

return M
