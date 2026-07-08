-- Unit AI storage + build-assist actions (#538 split from unit_ai.lua).
--
-- store_materials: auto-deposit excess Materials-category items into
-- the nearest built cargo. build_nearby: stand adjacent to an
-- under-construction building so the construction tick in
-- scripts/building_spawn.lua counts this unit toward its worker-rate
-- progress.

local core = require("scripts.unit_ai_core")
local aiState        = core.aiState
local distance        = core.distance
local chebToFootprint = core.chebToFootprint

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local M = {}

-----------------------------------------------------------
-- Action: store_materials
--
-- Auto-deposit excess Materials-category items into the nearest
-- built cargo. Utility scales with how full the unit is — a barely-
-- loaded acolyte holds onto their plates, a 90 %-full one walks
-- them over to the cargo. Only Materials category items go in;
-- food, weapons, and worn gear stay on the unit.
--
-- State on s:
--   storeTarget  = bid (cached during the walk so utility re-scans
--                  cheaply on subsequent ticks)
-----------------------------------------------------------

-- Count items in the inventory whose def category is "Materials".
local function countMaterialsInInventory(uid)
    local inv = unit.getInventory(uid)
    if not inv then return 0 end
    local n = 0
    for _, it in ipairs(inv) do
        if it.category == "Materials" then n = n + 1 end
    end
    return n
end

-- Nearest built building with non-zero storage capacity AND room for
-- at least one more item (current weight < capacity). Returns
-- {bid, gridX, gridY, tileW, tileH, distance} or nil.
local function findStorageTarget(fromX, fromY, maxRange)
    -- Active-world buildings only (#197); see findDeliveryTarget.
    local ids = building.getActiveIds()
    if not ids or #ids == 0 then return nil end
    local best, bestD = nil, maxRange
    for _, bid in ipairs(ids) do
        if bid and building.getActivity(bid) == "built" then
            local cap = building.getStorageCapacity(bid)
            local used = building.getStorageWeight(bid) or 0
            if cap and cap > 0 and used < cap then
                local info = building.getInfo(bid)
                if info then
                    local tw = info.tileW or 1
                    local th = info.tileH or 1
                    local cx = info.gridX + tw / 2
                    local cy = info.gridY + th / 2
                    local d = distance(fromX, fromY, cx, cy)
                    if d <= bestD then
                        best = {
                            bid = bid, gridX = info.gridX, gridY = info.gridY,
                            tileW = tw, tileH = th, distance = d,
                        }
                        bestD = d
                    end
                end
            end
        end
    end
    return best
end

local function storeMaterialsUtility(uid, s, params)
    if countMaterialsInInventory(uid) <= 0 then return -math.huge end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local target = findStorageTarget(info.gridX, info.gridY,
                                     params.store_scan_range)
    if not target then return -math.huge end

    -- Fill fraction. carrying_capacity is a derived stat (body-mass
    -- driven); guard against zero in case eager-stats hasn't fired.
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW = unit.getStat(uid, "carrying_capacity")
    if not maxW or maxW <= 0 then return -math.huge end
    local fill = math.min(1.0, carried / maxW)

    s.storeTarget = target.bid
    return params.store_base_utility * fill * fill * fill
         * roles.weight(s, "store_materials")
end

local function storeMaterialsExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    -- Re-resolve target each tick we execute; cheap and self-heals
    -- if the cached cargo got destroyed or filled up.
    local target = findStorageTarget(info.gridX, info.gridY,
                                     params.store_scan_range)
    if not target then return end
    s.storeTarget = target.bid

    local utx  = math.floor(info.gridX)
    local uty  = math.floor(info.gridY)
    local cheb = chebToFootprint(utx, uty, target.gridX, target.gridY,
                                 target.tileW, target.tileH)

    if cheb > 1 then
        local bestX, bestY, bestD = nil, nil, math.huge
        for dx = -1, target.tileW do
            for dy = -1, target.tileH do
                if dx == -1 or dx == target.tileW
                   or dy == -1 or dy == target.tileH then
                    local nx = target.gridX + dx + 0.5
                    local ny = target.gridY + dy + 0.5
                    local d  = distance(info.gridX, info.gridY, nx, ny)
                    if d < bestD then
                        bestX, bestY, bestD = nx, ny, d
                    end
                end
            end
        end
        if bestX then
            unit.moveTo(uid, bestX, bestY, mv.comfort(uid))  -- hauling → comfort
        end
        return
    end

    -- Arrived. Deposit every Materials item in inventory one at a
    -- time. Stop on the first reject (cargo full) — next tick the
    -- utility may pick a different target if one has room.
    local inv = unit.getInventory(uid) or {}
    -- Build a unique list of material defNames (each one might
    -- have multiple instances; the API pops the first matching).
    local seen = {}
    local mats = {}
    for _, it in ipairs(inv) do
        if it.category == "Materials" and not seen[it.defName] then
            seen[it.defName] = true
            mats[#mats + 1] = it.defName
        end
    end
    for _, defName in ipairs(mats) do
        while true do
            local ok = unit.depositToCargo(uid, target.bid, defName)
            if not ok then break end
        end
    end

    s.storeTarget = nil
end

-----------------------------------------------------------
-- Action: build_nearby
--
-- Fires when an under-construction building (Appearing activity,
-- bdBuildWork > 0) is within build_scan_range. Once adjacent
-- (Chebyshev ≤ 1) the unit stands still; the construction tick in
-- scripts/building_spawn.lua counts adjacent workers and applies
-- the rate formula to biBuildProgress per real second.
--
-- Utility excludes the asking unit from the worker count so the
-- value reflects "is it worth ME joining". With saturation_n = 5
-- the 5th would-be joiner sees util = 0 and prefers wander.
-----------------------------------------------------------

-- Returns {bid, gridX, gridY, tileW, tileH, distance} for the
-- nearest Appearing building with bdBuildWork > 0 within maxRange,
-- or nil. Uses building.getActiveIds() / getActivity / getBuildRequired /
-- getInfo — all cheap on a small N of placed buildings.
local function findNearestUnbuilt(fromX, fromY, maxRange)
    -- Active-world buildings only (#197); see findDeliveryTarget.
    local ids = building.getActiveIds()
    if not ids or #ids == 0 then return nil end
    local best, bestD = nil, maxRange
    for _, bid in ipairs(ids) do
        if bid then
            local activity = building.getActivity(bid)
            local required = building.getBuildRequired(bid)
            if activity == "appearing" and required and required > 0 then
                local info = building.getInfo(bid)
                if info and info.gridX and info.gridY then
                    local tw = info.tileW or 1
                    local th = info.tileH or 1
                    -- Building center for distance ranking.
                    local cx = info.gridX + tw / 2
                    local cy = info.gridY + th / 2
                    local d = distance(fromX, fromY, cx, cy)
                    if d <= bestD then
                        best = {
                            bid    = bid,
                            gridX  = info.gridX,
                            gridY  = info.gridY,
                            tileW  = tw,
                            tileH  = th,
                            distance = d,
                        }
                        bestD = d
                    end
                end
            end
        end
    end
    return best
end

-- Acolytes currently in build_nearby targeting this bid, excluding
-- excludeUid (so utility can ask "what'd the count be if I joined").
local function countBuildersAt(bid, excludeUid)
    local count = 0
    local ids = unit.getAllIds() or {}
    for _, uid in ipairs(ids) do
        if uid ~= excludeUid then
            local s = aiState[uid]
            if s and s.currentAction == "build_nearby"
               and s.buildTarget == bid then
                count = count + 1
            end
        end
    end
    return count
end

local function buildNearbyUtility(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local target = findNearestUnbuilt(info.gridX, info.gridY,
                                      params.build_scan_range)
    if not target then return -math.huge end

    -- Cache for execute. (Execute re-resolves; this just lets
    -- countBuildersAt elsewhere see who's targeting what.)
    s.buildTarget = target.bid

    local workers    = countBuildersAt(target.bid, uid)
    local saturation = params.build_saturation_n
    local fill       = math.max(0, 1.0 - workers / saturation)
    if fill <= 0 then return -math.huge end

    -- Linear distance falloff: 1.0 within arrival range → 0 at
    -- scan range. Anything closer than build_arrival_tiles counts
    -- as adjacent for the purposes of the utility curve.
    local d = target.distance
    local distFactor
    if d <= params.build_arrival_tiles then
        distFactor = 1.0
    else
        local span = params.build_scan_range - params.build_arrival_tiles
        distFactor = math.max(0, 1.0 - (d - params.build_arrival_tiles) / span)
    end

    return params.build_base_utility * fill * distFactor
         * roles.weight(s, "build_nearby")
end

local function buildNearbyExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    local target = findNearestUnbuilt(info.gridX, info.gridY,
                                      params.build_scan_range)
    if not target then return end
    s.buildTarget = target.bid

    local utx  = math.floor(info.gridX)
    local uty  = math.floor(info.gridY)
    local cheb = chebToFootprint(utx, uty,
                                 target.gridX, target.gridY,
                                 target.tileW, target.tileH)

    if cheb <= 1 then
        -- Adjacent (or standing on) — stop and let the construction
        -- tick count us. No "facing the building" logic yet; can
        -- layer that on later when there's a build animation.
        unit.stop(uid)
        return
    end

    -- Walk toward the nearest border tile of the footprint. For a
    -- 1×1 footprint that's the 8 surrounding tiles; for larger
    -- footprints it's the ring around. Pick the one closest to us.
    local bestX, bestY, bestD = nil, nil, math.huge
    for dx = -1, target.tileW do
        for dy = -1, target.tileH do
            -- Skip interior cells; only border counts as approach.
            if dx == -1 or dx == target.tileW
               or dy == -1 or dy == target.tileH then
                local nx = target.gridX + dx + 0.5
                local ny = target.gridY + dy + 0.5
                local d  = distance(info.gridX, info.gridY, nx, ny)
                if d < bestD then
                    bestX, bestY, bestD = nx, ny, d
                end
            end
        end
    end
    if bestX then
        unit.moveTo(uid, bestX, bestY, mv.comfort(uid))  -- going to build site → comfort
    end
end


M.storeMaterialsUtility = storeMaterialsUtility
M.storeMaterialsExecute = storeMaterialsExecute
M.buildNearbyUtility     = buildNearbyUtility
M.buildNearbyExecute     = buildNearbyExecute

return M
