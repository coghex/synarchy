-- Unit AI ground-item pickup (#538 split from unit_ai.lua).
--
-- Action: pickup_ground. Player-ordered pickup of a ground item
-- (right-click → Pick up; unitAi.commandPickup). Path to the item,
-- then atomically move it into the inventory with the engine pickup
-- animation.
--
-- Capacity is gated TWICE, deliberately (#920):
--   * at COMMAND time, so a retrieval that can never succeed is
--     refused before the carrier walks out to a remote ruin — the
--     player finds out while the decision is still theirs to change,
--     instead of after the trip is already wasted;
--   * on ARRIVAL, because the load can change en route (the unit is
--     handed gear, picks something else up, drains its canteen).
-- Both gates measure the same two quantities: the carrier's live
-- unit.getCarryingWeight (loose + equipped + accessory mass, fill and
-- nested container contents included) against the ground instance's
-- live total weight from item.listGround.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local reportFailure = core.reportFailure
local distance       = core.distance
local ensureState    = core.ensureState

local mv = require("scripts.movement_speed")
-- Shared eligible-time stall accounting (#1291) — the same one
-- maintainTask charges a commanded move against.
local stall = require("scripts.unit_ai_stall")

-- How much closer the carrier must get before its pickup deadline
-- resets. Comfortably above path jitter, small enough that a real
-- approach keeps refreshing it every few steps.
local PICKUP_PROGRESS_TILES = 0.5

local M = {}

local function pickupGroundEntry(gid)
    for _, g in ipairs(item.listGround() or {}) do
        if g.id == gid then return g end
    end
    return nil
end

local function pickupItemDef(defName)
    for _, d in ipairs(item.listDefs() or {}) do
        if d.name == defName then return d end
    end
    return nil
end

local function pickupItemWeight(defName)
    local d = pickupItemDef(defName)
    return (d and d.weight) or 0
end

-- Player-facing name of an item def ("Field Radio", not "radio").
local function itemLabel(defName)
    local d = pickupItemDef(defName)
    if d and d.displayName and d.displayName ~= "" then return d.displayName end
    return defName
end

-- Player-facing name of a unit. Mirrors unit_resource_alerts.unitLabel:
-- a personal name (#264) if it has one, else the species label, else a
-- prettified def name.
local function unitLabel(uid)
    local info = unit.getInfo(uid)
    if info and info.name and info.name ~= "" then return info.name end
    if info and info.displayName and info.displayName ~= "" then
        return info.displayName
    end
    local n = (info and info.defName) or "Unit"
    return n:sub(1, 1):upper() .. n:sub(2)
end

-- The live total mass of a ground instance. listGround's `weight` is
-- already the instance weight (empty weight + fill + nested contents;
-- gems and part-full canteens vary per find); def mean + fill is the
-- fallback for a table that predates it.
local function groundWeight(g)
    return g.weight or (pickupItemWeight(g.defName) + (g.fill or 0))
end

-- (overCapacity, wouldCarry, capacity) for `uid` taking `g`.
local function capacityCheck(uid, g)
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    local total   = carried + groundWeight(g)
    return total > maxW, total, maxW
end

-- One message for both gates. reportFailure files it in the event log
-- tagged with this unit, so the player sees WHICH colonist can't take
-- WHICH item — in the global log and in that unit's own Log tab.
local function reportOverCapacity(uid, g, total, maxW)
    reportFailure(uid, string.format(
        "%s can't carry %s — %.1f kg would exceed a %.1f kg capacity",
        unitLabel(uid), itemLabel(g.defName), total, maxW))
end

local function pickupUtility(uid, s, params)
    local order = s.pickupOrder
    if not order then return -math.huge end
    local g = pickupGroundEntry(order.gid)
    if not g then
        -- Item gone (someone else took it / already collected) — normal,
        -- not a failure.
        s.pickupOrder = nil
        return -math.huge
    end
    -- pickup_timeout is a STALL timer, not a total-trip budget (#920).
    -- What it guards against is a target the carrier can't reach; a
    -- recovery from a remote ruin is tens of tiles of perfectly good
    -- walking and used to be abandoned mid-approach purely for taking
    -- longer than the budget. The deadline resets only on a NEW closest
    -- approach, so a unit circling or oscillating never refreshes it —
    -- the same progress rule as unit_ai.lua's stuck-walk watchdog — and
    -- only time the carrier was actually free to walk is charged
    -- against it (#1291, unit_ai_stall.lua) — including the reset,
    -- which is this order's own to make: an interruption that carries
    -- the carrier closer must not refund a budget already spent. This
    -- runs on the thought tick rather than every update the way
    -- maintainTask does; the accounting tolerates both cadences.
    local now = engine.gameTime()
    local eligible = s.currentAction == "pickup_ground"
    local info = eligible and unit.getInfo(uid)
    if info then
        local d = distance(info.gridX, info.gridY, g.x, g.y)
        if not order.bestDist or d < order.bestDist - PICKUP_PROGRESS_TILES then
            order.bestDist = d
            stall.reset(order, now)
        end
    end
    if stall.charge(order, eligible, now)
       > (params.pickup_timeout or 30) then
        -- Stalled out short of a still-present item: a real failure.
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
    -- Capacity check at the moment of truth ("walk, then refuse") — the
    -- load can have changed since the command-time gate below cleared it.
    local over, total, maxW = capacityCheck(uid, g)
    if over then
        engine.logWarn("pickup_ground: unit " .. tostring(uid)
            .. " over capacity (" .. string.format("%.1f", total)
            .. " > " .. string.format("%.1f", maxW)
            .. " kg) — leaving " .. g.defName)
        reportOverCapacity(uid, g, total, maxW)
        s.pickupOrder = nil
        return
    end

    -- Engine pickup animation + the atomic ground→inventory move.
    unit.pickup(uid)
    if item.pickupGround(uid, order.gid) then
        -- Who ended up carrying what, in the player's own log: the text
        -- names the item, and the event's uid files it under the carrier
        -- (which is also what the per-unit Log tab filters on). Without
        -- this a recovered item vanishes into an unnamed inventory.
        engine.emitEventForUnit("unit_event",
            unitLabel(uid) .. " picked up " .. itemLabel(g.defName),
            uid, math.floor(g.x), math.floor(g.y))
    end
    s.pickupOrder = nil
end

-- Player order: send `uid` to fetch ground item `gid`. Returns true if
-- the order was accepted, false if it was refused up front.
--
-- The refusal case is the point (#920): a retrieval that cannot fit is
-- rejected here, before any travel, with a player-visible warning. An
-- item that is already GONE still queues — pickupUtility retires that
-- order quietly on the next tick, which is not a failure worth warning
-- about, and re-deriving that distinction here would duplicate it.
function unitAi.commandPickup(uid, gid)
    local g = pickupGroundEntry(gid)
    if g then
        local over, total, maxW = capacityCheck(uid, g)
        if over then
            reportOverCapacity(uid, g, total, maxW)
            return false
        end
    end
    local s = ensureState(uid)
    s.pickupOrder = { gid = gid, issuedAt = engine.gameTime() }
    s.nextActionAt = 0
    return true
end


M.pickupUtility = pickupUtility
M.pickupExecute = pickupExecute
-- Shared with unit_ai_transfer.lua (#1247): a transfer order files the
-- same kind of unit-and-item warning this one does, so it reuses these
-- rather than growing a second set of labels free to disagree.
M.itemLabel     = itemLabel
M.unitLabel     = unitLabel

return M
