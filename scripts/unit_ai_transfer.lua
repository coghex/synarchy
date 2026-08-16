-- Unit AI transfer execution: BOTH modes of epic #1013.
--
--   transfer_order    (#1247, UIT-2B) -- Mode B. A durable order walks
--                     its acting unit to the counterpart endpoint and
--                     commits there, exactly once.
--   escort_transfer   (#1250, UIT-3B) -- Mode A. A player-opened
--                     session walks its source unit to the destination
--                     and HOLDS it there while the player moves items
--                     by hand, for as long as the window is open.
--
-- They share this file because they share their whole vocabulary: the
-- same rect-to-rect approach, the same contract-owned reach rule, the
-- same comfort pacing, and the same place in the #306 utility ladder.
-- What differs is what happens on arrival -- one commits and finishes,
-- the other stops and waits for the player.
--
-- Mode B's own record follows.
--
-- Unit AI transfer-order execution (#1247, epic #1013 slice UIT-2B).
--
-- Action: transfer_order. A durable order queued by #1246's store makes
-- its ACTING unit walk to the counterpart endpoint and commit there,
-- exactly once. Modelled on #920's commandPickup (unit_ai_pickup.lua),
-- which is the shape the design names:
--
--   * a COMMAND-TIME gate with a player-visible refusal, so an order
--     that can never fit is rejected before anybody walks anywhere;
--   * an ARRIVAL-TIME re-gate, because the load changes en route;
--   * a STALL timer, not a trip budget -- the deadline resets only on a
--     new closest approach, so a long-but-progressing haul finishes
--     while an unreachable endpoint gives up;
--   * an attributed completion event tagged with the carrier's uid;
--   * comfort walk pacing, the same as pickup: this is a player order,
--     not a sprint.
--
-- WHAT THIS MODULE DOES NOT DO is decide anything about a transfer.
-- Eligibility, proximity, capacity, exact-instance identity, per-item
-- atomicity and the structured refusal vocabulary are all #1000/#1085's
-- (src/Unit/Transfer.hs), reached through the engine verbs; the order
-- lifecycle is #1246's store. This file is the BEHAVIOUR: when to walk,
-- when to give up, and what the player is told.
--
-- The arrival gate is the COMMIT ITSELF, deliberately. Pickup has to
-- re-check capacity in Lua because item.pickupGround has no gate of its
-- own; unit.commitTransferOrder does, atomically, at the instant the
-- item moves -- strictly stronger than anything this file could measure
-- a moment earlier, and re-deriving it here would be a second capacity
-- rule free to disagree with the one that actually decides. So what
-- arrival adds is not another check but the player-visible REPORT: any
-- entry the commit refused comes back `failed`, and reportOutcomes
-- files it with the same unit-and-item warning the command-time gate
-- uses.
--
-- The order store stays authoritative for order state and nothing about
-- an order is mirrored durably into aiState -- which is what makes an
-- in-flight order survive a save/load with no migration and no
-- reconciliation: the carrier's next tick simply re-reads the store and
-- carries on walking. s.transferOrder below is trip bookkeeping only
-- (the stall budget and the closest approach so far), stripped at
-- snapshot time by unit_ai_save.lua.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local reportFailure   = core.reportFailure
local distance        = core.distance
local chebToFootprint = core.chebToFootprint
local ensureState     = core.ensureState

local mv = require("scripts.movement_speed")
-- Shared eligible-time stall accounting (#1291): an interval another
-- action won, or one the AI never ticked through, costs a pending order
-- nothing.
local stall = require("scripts.unit_ai_stall")
-- The player-facing item/unit labels the pickup order already
-- established ("Field Radio", not "radio"; a personal name if the unit
-- has one). Required rather than re-derived so a transfer warning and a
-- pickup warning can never name the same thing two different ways.
local pickup = require("scripts.unit_ai_pickup")
local itemLabel = pickup.itemLabel
local unitLabel = pickup.unitLabel

-- How much closer the carrier must get before its deadline resets.
-- Same value, and the same reasoning, as PICKUP_PROGRESS_TILES:
-- comfortably above path jitter, small enough that a real approach
-- refreshes it every few steps.
local TRANSFER_PROGRESS_TILES = 0.5

-- The in-progress LOCK utility (requirement 3), and the one number that
-- keeps the wander tick and ordinary utility churn from taking the
-- carrier away mid-haul. A queued order is a player order-at-a-distance,
-- so it sits in the player-order band as a peer of pickup (7.5), above
-- follow_command (7.0) and every routine-work lock (<=6.0), while
-- combat / treatment (>=8.0) and dire survival still preempt it.
-- Registered AFTER pickup_ground in every action list, so a unit holding
-- both resolves the tie to pickup by list order; neither clears the
-- other and the loser resumes afterwards.
--
-- A module constant rather than a per-species tunable, following
-- FOLLOW_COMMAND_UTILITY: an order is a player order whatever species
-- carries it, and a species config that forgot the key would score nil.
-- Being a constant also means role weights (#265) cannot reach it —
-- those multiply work-action ENTRY utilities only, never a lock.
local TRANSFER_ORDER_UTILITY = 7.5

-- STALL budget of ELIGIBLE time (unit_ai_stall.lua), NOT a total-trip
-- budget: it restarts on every new closest approach, so it can only
-- expire on a carrier that has stopped getting closer. The same 60 s a
-- commanded move (TASK_TIMEOUT_SEC) allows, and deliberately more than
-- pickup's 30 s, because an order at a distance is the long-haul case by
-- construction.
--
-- Arrival is deliberately NOT a constant here: it is the transfer
-- contract's own Chebyshev <= 1 rect-to-rect rule (approachDist below),
-- so the walk and the commit cannot disagree about "close enough".
local TRANSFER_ORDER_TIMEOUT = 60.0

local M = {}

-- Walk to the nearest tile of the ring just OUTSIDE an endpoint's
-- footprint. Rect-generic because a counterpart may be a unit (a 1x1
-- rect) as easily as a multi-tile building; the three building-only
-- copies in unit_ai_deliver/craft/repair predate this and folding them
-- onto it is not this slice's change.
local function moveBesideRect(uid, info, ax, ay, tw, th)
    local bestX, bestY, bestD = nil, nil, math.huge
    for dx = -1, tw do
        for dy = -1, th do
            if dx == -1 or dx == tw or dy == -1 or dy == th then
                local nx = ax + dx + 0.5
                local ny = ay + dy + 0.5
                local d = distance(info.gridX, info.gridY, nx, ny)
                if d < bestD then bestX, bestY, bestD = nx, ny, d end
            end
        end
    end
    if bestX then
        unit.moveTo(uid, bestX, bestY, mv.comfort(uid))
    end
end

-- The order this unit should be working, or nil.
--
-- Two independent reasons to pass one over, and they are NOT the same
-- thing: a TERMINAL order is finished (it stays in the store because
-- pruning is UIT-5A's, so it must be skipped rather than re-run -- this
-- is what keeps commit exactly-once after the fact), while an order
-- with no approachRole is one whose acting unit is neither endpoint --
-- a building-to-building order (D-10), valid in the store and simply
-- not this executor's to claim.
local function actionableOrder(uid)
    for _, o in ipairs(unit.getTransferOrders(uid) or {}) do
        if not o.terminal and o.approachRole then return o end
    end
    return nil
end

-- The trip record for `order`, restarted whenever the unit moves on to
-- a different order (a fresh order gets the whole budget, never the
-- leftovers of the last one).
local function trip(s, order, now)
    local t = s.transferOrder
    if not t or t.oid ~= order.id then
        t = { oid = order.id }
        s.transferOrder = t
        stall.reset(t, now)
    end
    return t
end

-- Chebyshev distance from the carrier to the counterpart's occupied
-- RECTANGLE -- the same measure src/Unit/Transfer.hs's withinReach
-- applies, so "close enough to walk no further" and "close enough to
-- commit" cannot disagree. Anchor-to-anchor distance would strand the
-- carrier beside the far end of a multi-tile building.
local function approachDist(info, a)
    return chebToFootprint(math.floor(info.gridX), math.floor(info.gridY),
                           a.gridX, a.gridY, a.tileW or 1, a.tileH or 1)
end

-- One player-visible warning for a set of refused entries, naming the
-- carrier and the item -- bounded, because a twelve-item batch that
-- wholly refuses must not file twelve separate warnings. The first
-- refused item is named in full and the rest are counted, which is what
-- a player needs to act ("it wouldn't fit") without a wall of text.
--
-- `reason`/`cause` come straight from the engine's structured
-- vocabulary; the message quotes the cause when there is one, since for
-- an arrival refusal (`became_stale`) the cause IS the explanation.
local function reportOutcomes(uid, outcomes, lead)
    local first, n = nil, 0
    for _, o in ipairs(outcomes or {}) do
        if o.state == "failed" then
            n = n + 1
            if not first then first = o end
        end
    end
    if n == 0 then return 0 end
    local extra = (n > 1) and string.format(" (and %d more)", n - 1) or ""
    reportFailure(uid, string.format("%s %s %s%s -- %s",
        unitLabel(uid), lead, itemLabel(first.defName), extra,
        first.cause or first.reason or "refused"))
    return n
end

-----------------------------------------------------------
-- Utility
-----------------------------------------------------------
-- Returns TRANSFER_ORDER_UTILITY -- constant while an order is
-- actionable, which is what makes it the in-progress lock. See that
-- constant for where it sits in the #306 ladder and why.
--
-- `params` is unused: both numbers are module constants, so every
-- species that registers this action gets the same behaviour whether or
-- not its config block knows the action exists.
local function transferUtility(uid, s)
    local order = actionableOrder(uid)
    if not order then
        s.transferOrder = nil
        s.transferCandidate = nil
        return -math.huge
    end
    s.transferCandidate = order

    -- The counterpart stopped existing during the walk. Retire the
    -- order QUIETLY (requirement 4): a destination that was demolished
    -- or a carrier's partner that died is ordinary attrition, not a
    -- failure worth interrupting the player over -- but the entries
    -- still have to reach a terminal state, with the reason recorded,
    -- or the order sits pending forever. became_stale + source_missing
    -- / receiver_missing, which side depending on which end the unit
    -- was walking TO.
    if not order.approach then
        unit.failTransferOrder(uid, order.id, "became_stale",
            order.approachRole == "source" and "source_missing"
                                            or "receiver_missing")
        s.transferOrder = nil
        s.transferCandidate = nil
        return -math.huge
    end

    local now = engine.gameTime()
    local t = trip(s, order, now)
    -- A STALL timer, not a total-trip budget (#920): a haul across the
    -- map is tens of tiles of perfectly good walking, and the old
    -- from-issue budget shape abandoned exactly those. The deadline
    -- resets only on a NEW closest approach -- so a carrier circling an
    -- unreachable endpoint never refreshes it -- and only time this
    -- action actually held the unit is charged (#1291).
    local eligible = s.currentAction == "transfer_order"
    local info = eligible and unit.getInfo(uid)
    if info then
        local d = approachDist(info, order.approach)
        if not t.bestDist or d < t.bestDist - TRANSFER_PROGRESS_TILES then
            t.bestDist = d
            stall.reset(t, now)
        end
    end
    if stall.charge(t, eligible, now) > TRANSFER_ORDER_TIMEOUT then
        -- Stalled short of an endpoint that is still there: a real
        -- failure, and one the player should hear about. out_of_range
        -- is the honest reason -- the carrier never got in range.
        reportFailure(uid, unitLabel(uid)
            .. " couldn't reach the transfer destination")
        unit.failTransferOrder(uid, order.id, "out_of_range")
        s.transferOrder = nil
        s.transferCandidate = nil
        return -math.huge
    end
    return TRANSFER_ORDER_UTILITY
end

-----------------------------------------------------------
-- Execute
-----------------------------------------------------------
local function transferExecute(uid, s)
    local order = s.transferCandidate or actionableOrder(uid)
    local info = unit.getInfo(uid)
    if not order or not order.approach or not info then
        s.transferOrder = nil
        s.transferCandidate = nil
        return
    end

    -- Movement has started. Idempotent (only queued entries advance),
    -- so re-running it after an interruption costs nothing, and it is
    -- unconditional rather than gated on distance: an order created
    -- while the carrier already stands next to the endpoint must still
    -- pass THROUGH in_transit on its way to ready_to_commit.
    unit.advanceTransferOrder(uid, order.id, "in_transit")

    if approachDist(info, order.approach) > 1 then
        moveBesideRect(uid, info, order.approach.gridX, order.approach.gridY,
                       order.approach.tileW or 1, order.approach.tileH or 1)
        return
    end

    unit.stop(uid)
    -- Arrived, by the contract's own Chebyshev <= 1 rect measure, so
    -- the commit's identical re-check is guaranteed to agree about
    -- range. Everything else it revalidates -- the endpoints still
    -- exist and are eligible, the exact instances are still there, they
    -- still fit -- is the arrival gate, and whatever it refuses comes
    -- back as became_stale carrying the real cause.
    unit.advanceTransferOrder(uid, order.id, "ready_to_commit")
    local result = unit.commitTransferOrder(uid, order.id)
    s.transferOrder = nil
    s.transferCandidate = nil
    if not result then return end

    local moved = {}
    for _, o in ipairs(result.outcomes or {}) do
        if o.state == "completed" then moved[#moved + 1] = o.defName end
    end
    if #moved > 0 then
        -- ONE attributed event for the whole order, naming what landed
        -- and tagged with the carrier -- which is also what files it in
        -- that unit's own Log tab. The pickup precedent, at batch
        -- granularity: an order is one trip, so it reads as one line.
        local extra = (#moved > 1)
            and string.format(" (and %d more)", #moved - 1) or ""
        local where = unit.transferEndpointInfo(order.approach)
        engine.emitEventForUnit("unit_event", string.format(
            "%s transferred %s%s %s %s", unitLabel(uid),
            itemLabel(moved[1]), extra,
            order.approachRole == "source" and "from" or "to",
            (where and where.displayName ~= "" and where.displayName)
                or "storage"),
            uid, math.floor(info.gridX), math.floor(info.gridY))
    end
    -- D-1 / requirement 5: whatever did not make it is reported, so a
    -- partial batch is visibly partial rather than silently short.
    reportOutcomes(uid, result.outcomes, "couldn't transfer")
end

-----------------------------------------------------------
-- Public command
-----------------------------------------------------------
-- Queue a transfer order for `uid` against a request table
-- ({ source = {kind,id}, destination = {kind,id}, items = {...} } --
-- the same shape unit.checkTransfer takes). Returns the order id on
-- acceptance, false on refusal.
--
-- The refusal case is the point (#920's lesson, restated by this
-- issue's requirement 2): an order that cannot fit is rejected HERE,
-- before any travel, with a player-visible warning naming the carrier
-- and the item -- the player finds out while the decision is still
-- theirs to change. "Refused" means no order was stored at all, which
-- the engine reports by withholding orderId: either the whole request
-- was rejected, or every item failed its check so there is nothing to
-- make the trip for.
--
-- A PARTIAL result is an acceptance, not a refusal (D-1): twelve items
-- into room for eight queue eight, the four that did not fit ride along
-- as terminal entries of the same order, and the warning says so.
function unitAi.commandTransferOrder(uid, request)
    local result, err = unit.createTransferOrder(uid, request)
    if not result then
        if err then engine.logWarn("commandTransferOrder: " .. tostring(err)) end
        return false
    end
    if not result.orderId then
        if result.accepted == false then
            reportFailure(uid, string.format("%s can't transfer -- %s",
                unitLabel(uid), tostring(result.reason)))
        else
            reportOutcomes(uid, result.outcomes, "can't transfer")
        end
        return false
    end
    reportOutcomes(uid, result.outcomes, "can't transfer")
    local s = ensureState(uid)
    s.transferOrder = nil
    -- Decide on the next tick rather than at the unit's natural
    -- cadence, the same responsiveness commandMove/commandPickup buy.
    s.nextActionAt = 0
    return result.orderId
end

-----------------------------------------------------------
-- Mode A: the escort hold (#1250)
-----------------------------------------------------------
-- The in-progress LOCK that IS the hold (requirement 5). A Mode A
-- session is the player standing an acolyte somewhere and working its
-- pockets by hand, so it sits at the same 7.5 as a queued order and
-- pickup: above follow_command (7.0) and every routine-work lock
-- (<=6.0), below combat / treatment (>=8.0) and dire survival, which
-- still preempt.
--
-- Constant while a session names this unit, which is the whole point.
-- unit.setFrozen is NOT this (it only pins the render publish while the
-- sim keeps walking) and neither is a completed move order (the unit
-- reverts to wander the moment it arrives) --
-- docs/expedition_survival_calibration.md E3 measured both.
local ESCORT_UTILITY = 7.5

-- The session holding this unit, or nil. Read through package.loaded so
-- a build with the gesture module unloaded simply never scores this
-- action, rather than pulling a UI module into the AI thread.
local function heldSession(uid)
    local session = package.loaded["scripts.transfer_session"]
    if not session or type(session.holdsUnit) ~= "function" then return nil end
    if not session.holdsUnit(uid) then return nil end
    return session
end

-- There is deliberately NO stall timer here. Mode B's exists because an
-- order must reach a terminal state on its own; a session is the
-- player's own window, ends when they close it, and a unit that cannot
-- reach its destination is UIT-5B's failure handling, not this slice's.
--
-- It keeps NO per-unit state either, deliberately: the SESSION is the
-- state, so there is nothing to reconcile, nothing to strip at snapshot
-- time (scripts/unit_ai_save.lua persists an aiState row minus an
-- explicit transient list, so a scratch field here would ride into
-- `lua.unit_ai`), and nothing an interrupted tick can leave behind.
local function escortUtility(uid, _s)
    if not heldSession(uid) then return -math.huge end
    return ESCORT_UTILITY
end

local function escortExecute(uid, _s)
    local session = heldSession(uid)
    if not session then return end
    local active = session.get()
    local info = unit.getInfo(uid)
    if not (active and info) then return end

    -- Already open: stand still. `unit.stop` on a unit that is already
    -- stopped is a no-op, and re-running it is what makes this a hold
    -- rather than a one-shot -- an interruption that walked the unit
    -- away (combat, a mental break) leaves it standing wherever it
    -- ended up once this action wins again, which is the honest
    -- best-effort until UIT-5B owns that case.
    if active.phase ~= session.PHASE_APPROACHING then
        unit.stop(uid)
        return
    end

    local dest = session.destinationNow()
    if not dest then
        -- The destination stopped existing mid-approach. Retire the
        -- session QUIETLY rather than holding a unit against nothing;
        -- the richer player-facing failure handling is UIT-5B's.
        session.close("destination_missing")
        return
    end

    -- The contract's OWN Chebyshev <= 1 rect-to-rect rule, measured
    -- against the endpoint's live footprint, so "close enough to walk
    -- no further" and "close enough to commit" cannot disagree.
    if approachDist(info, dest) > 1 then
        moveBesideRect(uid, info, dest.gridX, dest.gridY,
                       dest.tileW or 1, dest.tileH or 1)
        return
    end
    unit.stop(uid)
    session.markArrived()
end

M.action = {
    name = "transfer_order",
    utility = transferUtility,
    execute = transferExecute,
}
-- Registered AFTER M.action in every action list, so a unit that
-- somehow holds both resolves the equal-utility tie to the queued order
-- by list order; neither clears the other.
--
-- Deliberately NOT forceExecute, exactly like the queued order beside
-- it: re-executing while the unit is actively walking re-issues
-- `unit.moveTo`, which wipes the engine-side `usLocalPath` and leaves
-- the unit barely making progress between AI ticks (see unit_ai.lua's
-- re-execute conditions). So the approach is issued once and the
-- arrival is noticed when the walk ends and the unit is idle again --
-- moveBesideRect aims at the ring tile just OUTSIDE the footprint, so
-- ending that walk IS satisfying the contract's reach rule. Once the
-- session is open the unit stands idle, so this then runs every tick,
-- which is what makes the hold a hold.
M.escortAction = {
    name = "escort_transfer",
    utility = escortUtility,
    execute = escortExecute,
}
M.transferUtility = transferUtility
M.transferExecute = transferExecute
M.escortUtility   = escortUtility
M.escortExecute   = escortExecute

return M
