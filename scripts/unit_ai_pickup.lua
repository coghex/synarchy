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
-- live total weight.
--
-- EVERY phase of an order resolves its ground entry on the CARRIER'S
-- OWN page (#1666). Ground-item ids are per-page allocators, so the
-- same number names a different item on every page; item.pickupGround
-- has committed on the carrier's own page since #1208, and this layer
-- used to select and measure the entry from item.listGround -- the
-- ACTIVE page -- so the two halves of one contract could describe two
-- different items. item.getGroundForUnit(uid, gid) is the owning-page
-- read that closes it: one named unit, one id, resolved through the
-- same unitOwningWorldState the commit resolves through. This is the
-- same active-page-query-authorizing-an-owning-page-mutation rule
-- unit_ai_reconcile.lua states for the post-load boundary (#1589).

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local reportFailure = core.reportFailure
local distance       = core.distance
local ensureState    = core.ensureState

local mv = require("scripts.movement_speed")
-- Shared eligible-time stall accounting (#1291) — the same one
-- maintainTask charges a commanded move against.
local stall = require("scripts.unit_ai_stall")
local hold = require("scripts.unit_ai_hold")

-- How much closer the carrier must get before its pickup deadline
-- resets. Comfortably above path jitter, small enough that a real
-- approach keeps refreshing it every few steps.
local PICKUP_PROGRESS_TILES = 0.5

local M = {}

-- The ground entry `gid` names ON `uid`'S OWN PAGE, plus whether that
-- page could be resolved at all (#1666).
--
-- The second value is what keeps "gone" honest. `nil, true` is the
-- page answering that it genuinely holds no such id -- the only thing
-- that may retire an order. `nil, false` is no answer at all (the unit
-- is gone, or its page has no live world), and a caller must hold the
-- order rather than retire it: falling back to the active page is
-- precisely the wrong-entity match this whole path exists to refuse.
local function pickupGroundEntry(uid, gid)
    return item.getGroundForUnit(uid, gid)
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

-- The live total mass of a ground instance. The ground row's `weight` is
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
    local g, resolved = pickupGroundEntry(uid, order.gid)
    if not g then
        if resolved then
            -- The carrier's OWN page says the item is gone (someone
            -- else took it / already collected) — normal, not a
            -- failure.
            s.pickupOrder = nil
        end
        -- Otherwise the page could not be resolved, which is not an
        -- answer: the order is unjudgeable this tick, not finished, so
        -- it is left standing. No eligible time is charged either —
        -- an action that cannot be scored never becomes currentAction,
        -- so the #1291 stall budget is not spent on it.
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
    local g, resolved = pickupGroundEntry(uid, order.gid)
    -- unit.getInfo is a GLOBAL lookup, so an off-active-page carrier
    -- still answers — with coordinates in ITS OWN page's frame, which
    -- is the frame `g` is now measured in too.
    local info = unit.getInfo(uid)
    if not info then
        -- The unit itself is gone; its order is moot.
        s.pickupOrder = nil
        return
    end
    if not g then
        -- Same split as pickupUtility: only the carrier's own page
        -- saying "no such id" retires the order.
        if resolved then s.pickupOrder = nil end
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
-- TWO refusals, and they are deliberately different in kind.
--
-- `gid` does not name a ground item on `uid`'s OWN page (#1666). This
-- is a CALLER error — a public verb handed an id belonging to some
-- other page, or to a unit whose page is not live — not a colonist who
-- cannot lift something. So it says nothing to the player: no
-- over-capacity warning, no event naming a carrier and an item, just a
-- diagnostic line. Nothing is inspected and nothing is written: the
-- capacity gate never runs (there is no instance to weigh), no order is
-- stored, any position hold stays exactly as it was (#1216), and
-- nextActionAt is left alone.
--
-- Capacity (#920): a retrieval that cannot fit is rejected here, before
-- any travel, with a player-visible warning.
function unitAi.commandPickup(uid, gid)
    local g, resolved = pickupGroundEntry(uid, gid)
    if not g then
        engine.logWarn("commandPickup: ground item " .. tostring(gid)
            .. " is not on unit " .. tostring(uid) .. "'s own page"
            .. (resolved and "" or " (no live page for that unit)")
            .. " — order refused")
        return false
    end
    local over, total, maxW = capacityCheck(uid, g)
    if over then
        reportOverCapacity(uid, g, total, maxW)
        return false
    end
    local s = ensureState(uid)
    -- #1216: an ACCEPTED player order supersedes a position hold. Past
    -- the capacity gate above and not before -- a refusal stores no
    -- order, so it must leave the unit holding exactly as it was.
    hold.clear(s)
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
