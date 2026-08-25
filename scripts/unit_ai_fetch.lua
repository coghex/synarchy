-- Unit AI materials-sourcing ladder (#538 split from unit_ai.lua).
--
-- Shared "inventory → nearby ground → technomule → cargo storage" fetch
-- helpers used by deliver_to_build_site, construct_job, craft_job, and
-- repair_job. Not on its own an "action" — no utility/execute pair,
-- just the sourcing primitives those actions' phase machines call into.
--
-- The cargo rung (cargoCountOf / fetchWantsFromCargo, plus the shared
-- moveBesideBuilding walk) moved here from unit_ai_craft.lua with
-- #1673: it is the fourth rung of the SAME ladder the other three
-- already live on, and craft_job was its only caller.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local distance = core.distance
local chebToFootprint = core.chebToFootprint

local mv = require("scripts.movement_speed")
local page = require("scripts.unit_ai_page")

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

-- Can this unit CARRY what a whole job still has to fetch (#1326)?
-- `demands` is {defName → count}; only the SHORTFALL is weighed, since
-- copies already in inventory are part of getCarryingWeight and must
-- not be charged twice.
--
-- Every gate below (fetchWantsFromGround / fetchWantsFromMule /
-- unit_ai_craft's fetchWantsFromCargo) refuses a single pickup that
-- would overflow, one item at a time — none of them can see that the
-- job as a WHOLE never fits. Without this prospective check a worker
-- claims such a job, fetches until a gate stops it, fails the
-- post-fetch inventory reconciliation, releases the job still holding
-- the partial load, and re-claims the same job on the next decision
-- tick, forever. Six of the seven shipped smelting recipes are heavier
-- than an average acolyte's free headroom, so this is reachable on the
-- main smelting tier. scripts/unit_ai_repair.lua makes the identical
-- comparison before ITS claim, for the identical reason.
--
-- Equality is eligible: those gates compare with a strict `>`, so a
-- load landing exactly on capacity really does fit and must not be
-- pre-rejected here.
--
-- Silent by contract: this runs inside the candidate scan on every
-- decision tick, so a rejection logs nothing and emits no event — it
-- just leaves the job unclaimed and pending.
local function loadFeasible(uid, demands)
    local needed = 0
    for defName, count in pairs(demands or {}) do
        local short = count - inventoryCountOf(uid, defName)
        if short > 0 then
            needed = needed + deliverItemWeight(defName) * short
        end
    end
    if needed <= 0 then return true end
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    return carried + needed <= maxW
end

-- Nearest technomule, or nil. The colony's construction stock rides
-- on it; deliverers fetch their shortfall from here. No range limit —
-- materials are worth the walk.
--
-- `uid` is the ASKING unit, and the mule must stand on that unit's own
-- page (#1673): unit.getAllIds snapshots the ACTIVE page independently
-- of whatever page the actor was selected from, so an interleaved
-- world.show would otherwise hand a deliverer a mule in another world
-- to walk to and pull items off. An actor whose own page cannot be
-- read selects no mule at all.
local function findTechnomule(uid, fromX, fromY)
    local myPage = page.ofUnit(uid)
    if not myPage then return nil end
    local best, bestD = nil, math.huge
    for _, otherUid in ipairs(unit.getAllIds() or {}) do
        local info = unit.getInfo(otherUid)
        if info and info.defName == "technomule"
           and page.same(myPage, info.page) then
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

-- Count of defName across BUILT storage buildings (cargo holds) on
-- the acting unit's OWN page — the stockpile rung of the sourcing
-- ladder. No range limit, same rationale as the mule: stored materials
-- are worth the walk.
--
-- #1673: building.getActiveIds takes its own ACTIVE-page snapshot, so
-- every row is re-checked against `myPage` before it can be counted;
-- an unknown actor page counts nothing rather than counting the
-- active world's stock.
local function cargoCountOf(defName, myPage)
    if not myPage then return 0 end
    local total = 0
    for _, bid in ipairs(building.getActiveIds() or {}) do
        if bid and building.getActivity(bid) == "built"
           and page.same(myPage, page.ofBuilding(bid)) then
            for _, it in ipairs(building.getStorage(bid) or {}) do
                if it.defName == defName then total = total + 1 end
            end
        end
    end
    return total
end

-- Walk toward the nearest border tile of a building's footprint.
-- Returns true while still walking (Chebyshev > 1 → moveTo issued),
-- false once the unit stands on or beside the footprint. Shared by
-- the craft walking phase and the cargo fetch (same approach as
-- deliver / build_nearby).
local function moveBesideBuilding(uid, info, binfo)
    local tw, th = binfo.tileW or 1, binfo.tileH or 1
    local utx, uty = math.floor(info.gridX), math.floor(info.gridY)
    if chebToFootprint(utx, uty, binfo.gridX, binfo.gridY, tw, th) <= 1 then
        return false
    end
    local bestX, bestY, bestD = nil, nil, math.huge
    for dx = -1, tw do
        for dy = -1, th do
            if dx == -1 or dx == tw or dy == -1 or dy == th then
                local nx = binfo.gridX + dx + 0.5
                local ny = binfo.gridY + dy + 0.5
                local d = distance(info.gridX, info.gridY, nx, ny)
                if d < bestD then bestX, bestY, bestD = nx, ny, d end
            end
        end
    end
    if bestX then
        unit.moveTo(uid, bestX, bestY, mv.comfort(uid))
    end
    return true
end

-- Fetch loop against cargo storage: walk beside the nearest BUILT
-- store holding any wanted def and withdraw everything wanted from it
-- in one visit (unit.withdrawFromCargo preserves the instances;
-- adjacency is this walk, per the API contract). Entries clear even
-- on shortfall, like the mule fetch — raced withdrawals and split
-- stock resolve by the caller reconciling against inventory (a
-- release + re-plan reaches the next store). Returns true while
-- still busy (walking).
--
-- #1673: `info` is the ACTOR's own unit.getInfo table, so info.page is
-- the actor's page; every candidate store must match it. An actor with
-- no readable page reaches no store (page.same is false on nil).
local function fetchWantsFromCargo(uid, wants, info, params)
    if not next(wants) then return false end
    local best, bestD = nil, math.huge
    for _, bid in ipairs(building.getActiveIds() or {}) do
        if bid and building.getActivity(bid) == "built" then
            local has = false
            for _, it in ipairs(building.getStorage(bid) or {}) do
                if wants[it.defName] then has = true; break end
            end
            if has then
                local binfo = building.getInfo(bid)
                if binfo and page.same(info.page, binfo.page) then
                    local d = distance(info.gridX, info.gridY,
                        binfo.gridX + (binfo.tileW or 1) / 2,
                        binfo.gridY + (binfo.tileH or 1) / 2)
                    if d < bestD then
                        binfo.bid = bid
                        best, bestD = binfo, d
                    end
                end
            end
        end
    end
    if not best then
        -- Nothing stored anywhere (someone else withdrew it).
        for k in pairs(wants) do wants[k] = nil end
        return false
    end
    if moveBesideBuilding(uid, info, best) then return true end
    unit.stop(uid)
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    for defName, count in pairs(wants) do
        for _ = 1, count do
            local w = deliverItemWeight(defName)
            if carried + w > maxW then
                engine.logWarn("fetch: unit " .. tostring(uid)
                    .. " at capacity (" .. string.format("%.1f", carried + w)
                    .. " > " .. string.format("%.1f", maxW)
                    .. " kg) — leaving rest of " .. defName .. " in cargo")
                break
            end
            if not unit.withdrawFromCargo(uid, best.bid, defName) then
                break    -- store ran out (raced another claimant)
            end
            carried = carried + w
        end
        wants[defName] = nil
    end
    return false
end

-- Fetch loop against the technomule's stock: walk to the mule, then
-- take everything wanted in one go (unit.transferItemToUnit preserves
-- the instances). Entries are cleared even on shortfall — raced
-- claimants and empty stock resolve by the caller reconciling against
-- inventory. Returns true while still busy (walking).
local function fetchWantsFromMule(uid, wants, info, params)
    if not next(wants) then return false end
    local mule = findTechnomule(uid, info.gridX, info.gridY)
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
M.loadFeasible           = loadFeasible
M.findTechnomule         = findTechnomule
M.groundCountOf          = groundCountOf
M.untilStockSatisfied    = untilStockSatisfied
M.fetchWantsFromGround   = fetchWantsFromGround
M.fetchWantsFromMule     = fetchWantsFromMule
M.fetchWantsFromCargo    = fetchWantsFromCargo
M.cargoCountOf           = cargoCountOf
M.moveBesideBuilding     = moveBesideBuilding

return M
