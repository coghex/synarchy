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
-- TWO entry points, and which one is which matters. M.tickCollection
-- is the BUDGET, and its callers put it in their UTILITY, because that
-- is the only path that runs on every thought tick; M.nextYield is the
-- per-tick DECISION and belongs in execute. The long argument for that
-- split is at tickCollection itself -- it is the difference between an
-- unreachable yield being abandoned and it re-pathing forever.
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
-- guards cannot drift apart from each other, nor from the `cheb <= 1`
-- the harvest and forage walk-to-target branches spell inline.
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

-- The two collecting phases this module serves, each naming its own
-- state fields and the action name arbitration knows it by. A table
-- rather than six arguments at four call sites, and a CONSTANT one so
-- no call allocates. `clear` is that action's terminal exit spelled
-- out -- the same field set its own execute branch clears, and the same
-- one scripts/unit_ai_ref_schema.lua's `onEmpty` clears when the load
-- scrub empties the list.
M.HARVEST = { action = "auto_harvest", phase = "harvestPhase",
              loot = "harvestLoot", clock = "harvestCollect",
              clear = { "harvestPhase", "harvestLoot" } }
M.FORAGE  = { action = "forage", phase = "foragePhase",
              loot = "forageLoot", clock = "forageCollect",
              clear = { "foragePhase", "forageLoot", "forageTarget" } }

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

-- Chebyshev tile separation between `uid` and the live row `gid` names
-- on ITS OWN page, or nil when either cannot be resolved. Both callers
-- measure through this, so the budget sampled below and the approach
-- issued by nextYield can never disagree about how far away the yield
-- is.
local function yieldDistance(uid, gid)
    local row = item.getGroundForUnit(uid, gid)
    if not row then return nil end
    local info = unit.getInfo(uid)
    if not info then return nil, row end
    return M.chebyshev(math.floor(info.gridX), math.floor(info.gridY),
                       math.floor(row.x), math.floor(row.y),
                       locations.aliasStep(M.periodForUnit(uid))), row
end

-- Sample `spec`'s pending collection against its approach budget, and
-- END the collection when that budget runs out -- clearing exactly the
-- fields the action's own terminal exit clears. Returns whether it did.
-- A no-op when no collection is pending, when the row cannot be
-- resolved (execute ends that case, on its own reading), or when the
-- worker is already adjacent.
--
-- CALLED FROM UTILITY, NOT EXECUTE, and that is the whole point.
-- scripts/unit_ai.lua re-executes an unchanged action only once the
-- unit is idle again, so an execute-side sample is throttled by the
-- very walk it exists to time. Its stuck-walk watchdog returns a
-- going-nowhere unit to idle only after params.stuck_walk_timeout
-- (6 s), which is LONGER than stall.MAX_CHARGED_INTERVAL (5 s) -- and
-- an interval past that bound charges ZERO rather than being clamped,
-- deliberately, because it cannot be one uninterrupted stretch. So
-- every interval an execute-side sample ever saw would be discarded,
-- stalledFor would never advance, and the unreachable yield would
-- re-decide, re-path and re-report a failure forever: the livelock the
-- bound exists to prevent, surviving the bound. Utility runs on every
-- thought tick whatever the unit is doing, which is exactly why
-- scripts/unit_ai_pickup.lua charges pickup_timeout there too.
--
-- Eligibility is read the same way pickupUtility reads it: from
-- s.currentAction BEFORE this tick re-scores, so it names the action
-- that owned the interval which just elapsed rather than the one about
-- to start. An interval another action won charges nothing, which is
-- also what gives foraging correct accounting despite registering no
-- onExit (scripts/unit_ai_actions.lua).
--
-- Ending a collection here is silent: no core.reportFailure, unlike the
-- commanded pickup order this borrows its shape from. That order is a
-- PLAYER instruction whose abandonment the player must hear about;
-- autonomous farm-tending giving up on one unreachable yield is not,
-- and the yields stay on the ground for whoever can reach them.
function M.tickCollection(uid, s, spec, timeout)
    if s[spec.phase] ~= "collecting" then return false end
    local loot = s[spec.loot]
    local gid = loot and loot[#loot]
    if gid == nil then return false end

    local d = yieldDistance(uid, gid)
    if d == nil then return false end
    if d <= M.ADJACENT_TILES then
        -- Underfoot: there is no approach to time.
        s[spec.clock] = nil
        return false
    end

    local clock = s[spec.clock]
    if not clock then clock = {}; s[spec.clock] = clock end
    local now = engine.gameTime()
    -- A new closest approach refunds the whole budget; charging happens
    -- against whatever is left. Reset first, exactly as
    -- unit_ai_pickup.lua's pickupUtility orders the two. A STALL timer,
    -- not a total-trip budget (#920): a worker genuinely walking twenty
    -- tiles keeps refreshing it and is never abandoned mid-route.
    if not clock.bestDist or d < clock.bestDist - M.PROGRESS_TILES then
        clock.bestDist = d
        stall.reset(clock, now)
    end
    if stall.charge(clock, s.currentAction == spec.action, now)
       <= (timeout or M.COLLECT_TIMEOUT) then
        return false
    end
    s[spec.clock] = nil
    for _, f in ipairs(spec.clear) do s[f] = nil end
    return true
end

-- Decide what this collection tick does with `spec`'s retained gid
-- list, WITHOUT consuming anything from it. Returns one of:
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
--
-- The gid is PEEKED, never popped: both callers used to table.remove
-- ahead of the pickup, and an approach tick placed after that pop would
-- silently drop the yield. It leaves the list only on the tick that
-- calls item.pickupGround, or on a terminal exit.
--
-- The approach BUDGET is not this function's: M.tickCollection above
-- owns it, from the utility path, for the reason stated there. This one
-- runs only when the action is actually executing, which is precisely
-- what makes it the wrong place to time a walk from.
function M.nextYield(uid, s, spec, speed)
    local loot = s[spec.loot]
    local gid = loot and loot[#loot]
    if gid == nil then return "empty" end

    -- Re-resolved on the WORKER'S OWN page every tick (#1666/#1673),
    -- which is the page item.pickupGround commits through, so the
    -- coordinates measured belong to the exact instance that would
    -- move. Requirement 3: this runs before EVERY pickup, including the
    -- one that resumes after a second interruption.
    local d, row = yieldDistance(uid, gid)
    if d == nil then return "gone", gid end

    if d <= M.ADJACENT_TILES then
        -- Arrived. Stop a unit still under way toward the yield before
        -- it walks past the item it came for -- the same arrival rule
        -- the harvest and till/plant branches take, and the reason
        -- dispatch's `switching or activity == "idle"` can land here
        -- mid-stride at all. Unlike picking, collection charges no
        -- work clock, so the pickup happens on this same tick.
        local activity = unit.getActivity and unit.getActivity(uid)
        if activity == "walking" or activity == "running" then
            unit.stop(uid)
        end
        return "reach", gid, row
    end

    -- Steer to the RESOLVED row, floored to its tile, so the walk and
    -- the Chebyshev test share one frame. Never to a cached target:
    -- forage rewrites s.forageTarget on every scoring pass, with
    -- whatever its scan found, which need not be this gid at all.
    unit.moveTo(uid, math.floor(row.x) + 0.5, math.floor(row.y) + 0.5, speed)
    return "approach", gid, row
end

return M
