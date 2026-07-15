-- Unit AI materials-sourcing ladder (#538 split from unit_ai.lua).
--
-- Shared "inventory → nearby ground → technomule" fetch helpers used
-- by deliver_to_build_site, construct_job, craft_job, and repair_job.
-- Not on its own an "action" — no utility/execute pair, just the
-- sourcing primitives those actions' phase machines call into.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local distance = core.distance

local mv = require("scripts.movement_speed")

local M = {}

local function inventoryCountOf(uid, matType)
    local inv = unit.getInventory(uid)
    if not inv then return 0 end
    local n = 0
    for _, it in ipairs(inv) do
        if it.defName == matType then n = n + 1 end
    end
    return n
end

-- Item-def weight lookup for the fetch capacity gate. (pickup_ground
-- has its own copy further down — locals are lexically scoped, so
-- this section can't see it.)
local function deliverItemWeight(defName)
    for _, d in ipairs(item.listDefs() or {}) do
        if d.name == defName then return d.weight or 0 end
    end
    return 0
end

-- Nearest technomule, or nil. The colony's construction stock rides
-- on it; deliverers fetch their shortfall from here. No range limit —
-- materials are worth the walk.
local function findTechnomule(fromX, fromY)
    local best, bestD = nil, math.huge
    for _, otherUid in ipairs(unit.getAllIds() or {}) do
        local info = unit.getInfo(otherUid)
        if info and info.defName == "technomule" then
            local d = distance(fromX, fromY, info.gridX, info.gridY)
            if d < bestD then
                best, bestD = { uid = otherUid, gridX = info.gridX,
                                gridY = info.gridY }, d
            end
        end
    end
    return best
end

-- Count ground items of a def within `range` tiles of (fromX, fromY).
-- The middle rung of the sourcing ladder (inventory → ground → mule):
-- loose materials near the site get hauled before the mule is tapped.
local function groundCountOf(fromX, fromY, defName, range)
    local n = 0
    for _, g in ipairs(item.listGround() or {}) do
        if g.defName == defName
           and distance(fromX, fromY, g.x, g.y) <= range then
            n = n + 1
        end
    end
    return n
end

-- Ground stock of a defName across the WHOLE active world, no range
-- limit (#795) — the one authoritative scope an until-stock craft
-- bill's target counts against: ground-only, unbounded, same as
-- crafting_panel.lua's groundStockTally(). A thin unbounded-range call
-- into groundCountOf so the craft AI (unit_ai_craft.lua) and the #330
-- panel compute the identical count from the identical formula.
local function groundStockCountOf(defName)
    return groundCountOf(0, 0, defName, math.huge)
end

-- Is an UNTIL-STOCK craft bill (#795, Craft.Bills.BillMode) already at
-- its target? Such a bill sits idle/condition-satisfied instead of
-- drawing a fresh claim, and becomes claimable again the instant a
-- later rescan sees stock drop back below target. Always false for
-- fixed-count/repeat-forever bills (bill.mode ~= "until"), which have
-- no stock target.
--
-- This live re-check — at claim time (unit_ai_craft.lua's
-- findCraftBill) and again after every completed cycle (craftExecute)
-- — is also what bounds overproduction when two separate bills target
-- the same output: neither can run forever, since each stops within
-- one cycle of the (shared, global) stock actually reaching its own
-- target, without ever discarding a cycle already in flight.
local function untilStockSatisfied(bill)
    if bill.mode ~= "until" then return false end
    return groundStockCountOf(bill.outputItem) >= (bill.target or 0)
end

-- Fetch loop against GROUND items: walk to the nearest instance of a
-- wanted def and pick it up (item.pickupGround preserves the instance),
-- one item per execute tick. `wants` = {defName → count}; entries are
-- removed as they're satisfied or become unavailable (raced pickers,
-- capacity) — the caller reconciles its plan against what actually
-- landed in inventory afterwards. Returns true while still busy.
local function fetchWantsFromGround(uid, wants, params, range)
    local mat = next(wants)
    if not mat then return false end
    local info = unit.getInfo(uid)
    if not info then return false end

    local best, bestD = nil, range or math.huge
    for _, g in ipairs(item.listGround() or {}) do
        if g.defName == mat then
            local d = distance(info.gridX, info.gridY, g.x, g.y)
            if d <= bestD then best, bestD = g, d end
        end
    end
    if not best then
        -- None left in range (someone else collected them).
        wants[mat] = nil
        return next(wants) ~= nil
    end

    if bestD > params.pickup_arrival_tiles then
        unit.moveTo(uid, best.x, best.y, mv.comfort(uid))  -- hauling → comfort
        return true
    end

    unit.stop(uid)
    -- Capacity gate at the moment of pickup, same as pickup_ground.
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    local w       = best.weight or deliverItemWeight(best.defName)
    if carried + w > maxW then
        engine.logWarn("fetch: unit " .. tostring(uid)
            .. " at capacity (" .. string.format("%.1f", carried + w)
            .. " > " .. string.format("%.1f", maxW)
            .. " kg) — leaving ground " .. mat)
        wants[mat] = nil
        return next(wants) ~= nil
    end
    if item.pickupGround(uid, best.id) then
        wants[mat] = wants[mat] - 1
        if wants[mat] <= 0 then wants[mat] = nil end
    end
    -- On a raced pickup (false) the next tick re-scans.
    return next(wants) ~= nil
end

-- Fetch loop against the technomule's stock: walk to the mule, then
-- take everything wanted in one go (unit.transferItemToUnit preserves
-- the instances). Entries are cleared even on shortfall — raced
-- claimants and empty stock resolve by the caller reconciling against
-- inventory. Returns true while still busy (walking).
local function fetchWantsFromMule(uid, wants, info, params)
    if not next(wants) then return false end
    local mule = findTechnomule(info.gridX, info.gridY)
    if not mule then
        for k in pairs(wants) do wants[k] = nil end
        return false
    end

    if distance(info.gridX, info.gridY, mule.gridX, mule.gridY)
       > params.mule_fetch_arrival then
        unit.moveTo(uid, mule.gridX, mule.gridY, mv.comfort(uid))  -- hauling → comfort
        return true
    end

    unit.stop(uid)
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    for matType, count in pairs(wants) do
        for _ = 1, count do
            local w = deliverItemWeight(matType)
            if carried + w > maxW then
                engine.logWarn("fetch: unit " .. tostring(uid)
                    .. " at capacity (" .. string.format("%.1f", carried + w)
                    .. " > " .. string.format("%.1f", maxW)
                    .. " kg) — leaving rest of " .. matType .. " on mule")
                break
            end
            if not unit.transferItemToUnit(mule.uid, uid, matType) then
                break    -- mule stock ran out (raced another claimant)
            end
            carried = carried + w
        end
        wants[matType] = nil
    end
    return false
end

M.inventoryCountOf       = inventoryCountOf
M.deliverItemWeight      = deliverItemWeight
M.findTechnomule         = findTechnomule
M.groundCountOf          = groundCountOf
M.untilStockSatisfied    = untilStockSatisfied
M.fetchWantsFromGround   = fetchWantsFromGround
M.fetchWantsFromMule     = fetchWantsFromMule

return M
