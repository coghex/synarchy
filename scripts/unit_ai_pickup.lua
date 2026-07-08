-- Unit AI ground-item pickup (#538 split from unit_ai.lua).
--
-- Action: pickup_ground. Player-ordered pickup of a ground item
-- (right-click → Pick up; unitAi.commandPickup). Path to the item,
-- then atomically move it into the inventory with the engine pickup
-- animation. If the unit is over carrying capacity when it ARRIVES
-- (it can change en route), refuse and log.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local reportFailure = core.reportFailure
local distance       = core.distance
local ensureState    = core.ensureState

local mv = require("scripts.movement_speed")

local M = {}

local function pickupGroundEntry(gid)
    for _, g in ipairs(item.listGround() or {}) do
        if g.id == gid then return g end
    end
    return nil
end

local function pickupItemWeight(defName)
    for _, d in ipairs(item.listDefs() or {}) do
        if d.name == defName then return d.weight or 0 end
    end
    return 0
end

local function pickupUtility(uid, s, params)
    local order = s.pickupOrder
    if not order then return -math.huge end
    if not pickupGroundEntry(order.gid) then
        -- Item gone (someone else took it / already collected) — normal,
        -- not a failure.
        s.pickupOrder = nil
        return -math.huge
    end
    if engine.gameTime() - order.issuedAt > (params.pickup_timeout or 30) then
        -- Timed out trying to reach a still-present item: a real failure.
        reportFailure(uid, "Couldn't reach item to pick up")
        s.pickupOrder = nil
        return -math.huge
    end
    return params.pickup_utility
end

local function pickupExecute(uid, s, params)
    local order = s.pickupOrder
    if not order then return end
    local g = pickupGroundEntry(order.gid)
    local info = unit.getInfo(uid)
    if not g or not info then
        s.pickupOrder = nil
        return
    end

    local d = distance(info.gridX, info.gridY, g.x, g.y)
    if d > params.pickup_arrival_tiles then
        unit.moveTo(uid, g.x, g.y, mv.comfort(uid))  -- going to pick up → comfort
        return
    end

    unit.stop(uid)
    -- Capacity check at the moment of truth ("walk, then refuse").
    -- Fill counts: a full canteen on the ground weighs its contents
    -- too (1 L = 1 kg, matching getCarryingWeight).
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    -- listGround's weight is the INSTANCE weight incl. fill (gems
    -- vary per find); def-mean + fill is the fallback.
    local w       = g.weight or (pickupItemWeight(g.defName) + (g.fill or 0))
    if carried + w > maxW then
        engine.logWarn("pickup_ground: unit " .. tostring(uid)
            .. " over capacity (" .. string.format("%.1f", carried + w)
            .. " > " .. string.format("%.1f", maxW)
            .. " kg) — leaving " .. g.defName)
        s.pickupOrder = nil
        return
    end

    -- Engine pickup animation + the atomic ground→inventory move.
    unit.pickup(uid)
    item.pickupGround(uid, order.gid)
    s.pickupOrder = nil
end

function unitAi.commandPickup(uid, gid)
    local s = ensureState(uid)
    s.pickupOrder = { gid = gid, issuedAt = engine.gameTime() }
    s.nextActionAt = 0
end


M.pickupUtility = pickupUtility
M.pickupExecute = pickupExecute

return M
