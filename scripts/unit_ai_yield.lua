-- Retained ground-yield collection (#2550).
--
-- Auto-harvest (scripts/unit_ai_harvest.lua) and foraging
-- (scripts/unit_ai_needs.lua) both finish a pick by leaving the yields
-- lying on the ground as ordinary items and then draining a recorded
-- gid list one item per tick. That list SURVIVES an interruption --
-- neither phase is cleared by a preemption -- so the worker that comes
-- back to it may be anywhere. Before this module the collecting
-- branches read no coordinates at all and item.pickupGround performs no
-- distance test of its own (src/Engine/Scripting/Lua/API/Items/Ground.hs
-- moves the resolved instance into the inventory unconditionally), so a
-- worker could harvest, leave for a drink, and then pull its old yields
-- in from ten tiles away.
--
-- One implementation serves both callers deliberately: the two branches
-- share a defect, and two private copies of a seam-aware proximity test
-- are two chances to disagree about where the seam is.
--
-- A leaf: this requires unit_ai_locations (which requires nothing),
-- unit_ai_page and unit_ai_stall (likewise), so no cycle reaches back
-- through unit_ai.lua. The walk SPEED is the caller's argument rather
-- than a movement_speed call here, because forage paces a starving
-- unit differently from routine farm-tending.
--
-- Line budget: this file exists at all partly because
-- tools/lua_module_budget.py caps the unit_ai_*.lua family at 500
-- physical lines and scripts/unit_ai_needs.lua had ~24 to spare.

local locations = require("scripts.unit_ai_locations")
local page      = require("scripts.unit_ai_page")
-- The eligible-time stall accounting the commanded-order budget and the
-- player pickup order already charge against (#1291). Reused unchanged:
-- the approach below is the same question ("is this worker getting
-- anywhere?") over the same closest-approach reset.
local stall     = require("scripts.unit_ai_stall")

local M = {}

-- How much closer the worker must get before its collection deadline
-- resets, in tiles. The same figure and the same reason as
-- scripts/unit_ai_pickup.lua's PICKUP_PROGRESS_TILES: comfortably above
-- path jitter, small enough that a real approach keeps refreshing it.
M.PROGRESS_TILES = 0.5

-- The adjacency both callers' own approach branches stop at, in
-- Chebyshev tiles. Named once so the collection test and the walk it
-- guards cannot drift apart from each other or from harvest:229 /
-- needs:415, which spell the same bound inline.
M.ADJACENT_TILES = 1

-- How long a worker may fail to close on a retained yield, in ELIGIBLE
-- seconds, before the collection is abandoned. A module constant rather
-- than a tunable for the same reason unit_ai_pickup.lua's
-- PICKUP_PROGRESS_TILES is one, plus a hard one:
-- scripts/unit_ai_tunables.lua sits at exactly its 500-line
-- tools/lua_module_budget.py cap, and splitting it is another issue's
-- work. The figure is pickup_timeout's, because the question is
-- pickup_timeout's -- "can this worker reach the item it is going for?"
-- -- and callers may still override it per call.
M.COLLECT_TIMEOUT = 30.0

-- Chebyshev TILE distance from (ax, ay) to (bx, by), minimised over b's
-- cylindrical u-images: itself plus one shift each way along (+u, -v) by
-- `step`. Same three-image set as unit_ai_locations' Euclidean
-- `distance`, localizeTileToAnchor and Location.Bounds.seamAliases, so a
-- yield lying directly across the U seam is measured at its real
-- physical separation rather than at the width of the world.
--
-- Chebyshev rather than Euclidean because that is the metric the
-- surrounding approach gates in both callers already use
-- (`math.max(math.abs(...), math.abs(...)) <= 1`): the proximity test
-- and the walk it guards have to share one frame, or a worker can be
-- "arrived" by one and "out of reach" by the other forever.
--
-- Identity when step is 0 -- an arena, a non-wrapping page, or a period
-- the engine could not supply -- where the loop runs exactly once.
function M.chebyshev(ax, ay, bx, by, step)
    local s = 0
    if type(step) == "number" and step > 0 then s = step end
    local lo, hi = 0, 0
    if s > 0 then lo, hi = -1, 1 end
    local best = math.huge
    for k = lo, hi do
        local dx = math.abs(ax - (bx + k * s))
        local dy = math.abs(ay - (by - k * s))
        local d  = math.max(dx, dy)
        if d < best then best = d end
    end
    return best
end

-- One tile of Chebyshev separation, over the same alias set: the
-- adjacency both callers' approach branches already stop at.
--
-- `wrapWidth` is the page's FULL cylindrical u-wrap period in tiles
-- (what world.getWrapWidth returns) and is an EXPLICIT input rather
-- than an engine round-trip, exactly as unit_ai_locations'
-- nearestKnownLocation takes it, so this primitive stays callable from
-- a bare Lua VM. M.periodForUnit below is the uid-keyed wrapper.
function M.adjacent(ax, ay, bx, by, wrapWidth)
    return M.chebyshev(ax, ay, bx, by, locations.aliasStep(wrapWidth))
           <= M.ADJACENT_TILES
end

-- The wrap period of the page the ACTING unit stands on -- never the
-- active or visible one. These AI paths exist to serve an off-active-
-- page actor, and world.localizeTile would resolve its period from the
-- VISIBLE page, which is the wrong frame for exactly that worker.
function M.periodForUnit(uid)
    return locations.wrapPeriodFor(page.ofUnit(uid))
end

-- Decide what this collection tick does with the retained gid list
-- `loot`, WITHOUT consuming anything from it. Returns one of:
--
--   "empty"                -- nothing left; the caller ends the phase
--   "gone",     gid        -- the row does not resolve on this worker's
--                             own page, or the worker itself is gone:
--                             terminal, and the caller ends the phase
--                             exactly as it does today
--   "reach",    gid, row   -- adjacent: the caller may take this exact
--                             instance, applying its own admission
--                             policy first (auto-harvest weighs it,
--                             forage deliberately does not)
--   "approach", gid, row   -- out of reach; the walk has been issued
--                             and the gid stays pending
--   "stalled",  gid, row   -- out of reach for longer than the eligible-
--                             time budget: terminal, so an unreachable
--                             yield ends the phase instead of
--                             oscillating forever
--
-- The gid is PEEKED, never popped: both callers used to table.remove
-- ahead of the pickup, and an approach tick placed after that pop would
-- silently drop the yield. It leaves the list only on the tick that
-- calls item.pickupGround, or on a terminal exit.
--
-- `field` names the per-action approach clock on `s`
-- (s.harvestCollect / s.forageCollect): a small stall record so
-- stall.reset / stall.charge are reused unchanged, dropped on every
-- terminal outcome and on arrival so a later collection starts fresh.
-- Both names are stripped from the lua.unit_ai payload
-- (scripts/unit_ai_save.lua's TRANSIENT_WORK_FIELDS).
--
-- BOUNDING the approach is not optional. A pending collection scores a
-- fixed positive utility that beats idle unconditionally, and
-- scripts/unit_ai.lua's stuck-walk watchdog stops the unit and reports
-- a failure without clearing any action phase -- so an unreachable
-- retained yield would re-decide, re-path and re-fail every cycle
-- forever. The budget is a STALL timer, not a total-trip budget (#920):
-- it resets on every new closest approach, so a worker genuinely
-- walking twenty tiles is never abandoned mid-route.
function M.nextYield(uid, s, field, loot, timeout, speed)
    local gid = loot and loot[#loot]
    if gid == nil then s[field] = nil; return "empty" end

    -- Re-resolved on the WORKER'S OWN page every tick (#1666/#1673),
    -- which is the page item.pickupGround commits through, so the
    -- coordinates measured belong to the exact instance that would
    -- move. Requirement 3: this runs before EVERY pickup, including the
    -- one that resumes after a second interruption.
    local row = item.getGroundForUnit(uid, gid)
    if not row then s[field] = nil; return "gone", gid end
    local info = unit.getInfo(uid)
    if not info then s[field] = nil; return "gone", gid end

    local utx, uty = math.floor(info.gridX), math.floor(info.gridY)
    local tx,  ty  = math.floor(row.x), math.floor(row.y)
    local step = locations.aliasStep(M.periodForUnit(uid))
    local d = M.chebyshev(utx, uty, tx, ty, step)

    if d <= M.ADJACENT_TILES then
        -- Arrived. Stop a unit still under way toward the yield before
        -- it walks past the item it came for -- the same arrival rule
        -- the harvest and till/plant branches take, and the reason
        -- dispatch's `switching or activity == "idle"` can land here
        -- mid-stride at all. Unlike picking, collection charges no
        -- work clock, so the pickup happens on this same tick.
        s[field] = nil
        local activity = unit.getActivity and unit.getActivity(uid)
        if activity == "walking" or activity == "running" then
            unit.stop(uid)
        end
        return "reach", gid, row
    end

    local clock = s[field]
    if not clock then clock = {}; s[field] = clock end
    local now = engine.gameTime()
    -- A new closest approach refunds the whole budget; charging happens
    -- against whatever is left. Reset first, exactly as
    -- unit_ai_pickup.lua's pickupUtility orders the two.
    if not clock.bestDist or d < clock.bestDist - M.PROGRESS_TILES then
        clock.bestDist = d
        stall.reset(clock, now)
    end
    -- Charged as eligible, because this only runs from execute and
    -- execute only runs for the action arbitration put in control. The
    -- intervals that are NOT this action's announce themselves by
    -- dropping stallSeenAt as they happen: unit_ai_harvest's onExit for
    -- an ordinary preemption, unit_ai_stall's suspendOrders for the
    -- collapsed-pose and mid-animation ticks unit_ai.lua swallows
    -- without firing one, and MAX_CHARGED_INTERVAL as the backstop for
    -- a gap no path could announce. One residual: forage registers no
    -- onExit (scripts/unit_ai_actions.lua), so a SHORT interval another
    -- action won from a forager -- longer ones the backstop already
    -- zeroes -- is charged against its collection budget. That is a
    -- deliberate accept, not an oversight: the only consequence is
    -- abandoning an approach marginally sooner, which leaves the yields
    -- lying there for the next decision, and never the other way
    -- around. Giving forage an onExit is an arbitration change this
    -- issue's scope does not reach.
    if stall.charge(clock, true, now) > (timeout or M.COLLECT_TIMEOUT) then
        s[field] = nil
        return "stalled", gid, row
    end
    -- Steer to the RESOLVED row, floored to its tile, so the walk and
    -- the Chebyshev test above share one frame. Never to a cached
    -- target: forage rewrites s.forageTarget on every scoring pass,
    -- with whatever its scan found, which need not be this gid at all.
    unit.moveTo(uid, tx + 0.5, ty + 0.5, speed)
    return "approach", gid, row
end

return M
