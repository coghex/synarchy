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
--
-- FAILURE HANDLING (#1253, UIT-5A) is the other half of that, and it
-- lives next door in unit_ai_transfer_outcome.lua: whichever way an
-- order ends, the outcome is surfaced to the player exactly once and the
-- order is then pruned from the store (finishOrder). Four ways out reach
-- it from this file -- a completed or partly-completed commit, a
-- counterpart that stopped existing, a carrier that stalled short of one,
-- and the player's own "Cancel transfer" -- plus a fifth the executor
-- cannot reach at all, the carrier itself dying or being destroyed,
-- which the engine handles in Unit.Transfer.Live.
--
-- One consequence lands HERE rather than there: the quiet retirement
-- #1247 gave a vanished counterpart is now surfaced like every other
-- failure. Pruning removes the only place a dead order's reason could
-- still have been read, so silence would mean the player never hears
-- about it at all.

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
local hold = require("scripts.unit_ai_hold")
-- Outcome surfacing, terminal pruning and the cancel gesture (#1253).
local outcome = require("scripts.unit_ai_transfer_outcome")
local targetPhrase   = outcome.targetPhrase
local finishOrder    = outcome.finishOrder
local reportOutcomes = outcome.reportOutcomes
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

-- The AI action a Mode B order's EXECUTOR must be able to run for the
-- order to mean anything (#2030). Named once here, beside the action it
-- names, and exported so the player ingress that queues an order asks
-- about it by the same string the dispatch loop registers -- rather than
-- three literals free to drift from the registration.
--
-- The Mode A counterpart is transfer_session.ESCORT_ACTION, which names
-- scripts/unit_ai_escort.lua's action the same way for the same reason.
local TRANSFER_ORDER_ACTION = "transfer_order"
M.TRANSFER_ORDER_ACTION = TRANSFER_ORDER_ACTION

-- The per-species action inventory (#1250). A leaf module with no
-- dependencies of its own, so requiring it at the top costs nothing and
-- closes no cycle.
local aiActions = require("scripts.unit_ai_actions")

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
-- thing. A TERMINAL order is finished; since #1253 it is normally
-- pruned on the tick that ended it, so one showing up here at all means
-- a save was taken inside that sub-tick window -- skipping it is what
-- keeps commit exactly-once even then, without re-surfacing an outcome
-- that has already been reported. An order with no approachRole is a
-- different thing entirely: its acting unit is neither endpoint -- a
-- building-to-building order (D-10), valid in the store and simply not
-- this executor's to claim. The cancel gesture deliberately reaches
-- that one (unit_ai_transfer_outcome.pendingOrders); this must not.
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

    -- The counterpart stopped existing during the walk: became_stale +
    -- source_missing / receiver_missing, which side depending on which
    -- end the unit was walking TO.
    --
    -- #1247 retired this one QUIETLY, on the reasoning that a demolished
    -- destination is ordinary attrition rather than a failure worth
    -- interrupting the player over. That held only while the terminal
    -- entries stayed in the store to be read afterwards. #1253 prunes
    -- them, so silence here would mean an order the player queued simply
    -- disappearing with its cargo still aboard and nothing anywhere
    -- saying why -- the one outcome this issue exists to rule out. It is
    -- surfaced like every other failure, and pruned like every other
    -- terminal order.
    if not order.approach then
        unit.failTransferOrder(uid, order.id, "became_stale",
            order.approachRole == "source" and "source_missing"
                                            or "receiver_missing")
        reportFailure(uid, string.format("%s couldn't transfer -- the %s "
            .. "no longer exists", unitLabel(uid), targetPhrase(order)))
        finishOrder(uid, s, order.id)
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
        -- is the honest reason -- the carrier never got in range -- and
        -- requirement 6 forbids inventing an "unreachable" one for it.
        -- The endpoint is still live, so the warning can name it.
        local phrase = targetPhrase(order)
        unit.failTransferOrder(uid, order.id, "out_of_range")
        reportFailure(uid, string.format("%s couldn't reach the %s",
            unitLabel(uid), phrase))
        finishOrder(uid, s, order.id)
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
    -- Snapshot which entries were ALREADY terminal, BEFORE the commit
    -- rewrites the rest. A commit result reports every requested item,
    -- create-time refusals included, and commandTransferOrder warned
    -- about those when the order was queued -- so without this the four
    -- that never fit in a twelve-into-eight batch get a second warning
    -- on arrival for the same refusal. Neither advance above can move an
    -- entry into or out of this set: both only step a pending state on.
    local alreadyReported = outcome.settledIds(order)
    local result = unit.commitTransferOrder(uid, order.id)
    if not result then
        -- The order vanished between this tick's read and the commit --
        -- the only way the verb answers nil for an order it just
        -- resolved. There is no outcome to surface and nothing left to
        -- prune; drop the trip bookkeeping and let the next tick decide.
        s.transferOrder = nil
        s.transferCandidate = nil
        return
    end

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
    -- D-1 / #1247 requirement 5: whatever did not make it is reported,
    -- so a partial batch is visibly partial rather than silently short --
    -- but only what THIS commit refused (see above). An arrival refusal
    -- comes back as became_stale carrying the real precondition as its
    -- cause, and reportOutcomes quotes the cause, which is the part that
    -- explains itself.
    reportOutcomes(uid, result.outcomes, "couldn't transfer", alreadyReported)
    -- Both halves of the outcome have now been surfaced, so the order
    -- has no reader left. Every entry the commit touched is terminal, so
    -- this is the terminal transition (#1253 requirement 5).
    finishOrder(uid, s, order.id)
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
    -- Species capability, ahead of everything (#2030). This is the ONE
    -- boundary every Mode B order is created through, and it is
    -- deliberately reusable by surfaces that never ran a gesture's
    -- omission rule, so the capability is re-checked here rather than
    -- trusted -- the same defence in depth Mode A's `transfer_session.
    -- create` keeps behind its own menu omission.
    --
    -- It must precede `unit.createTransferOrder`, which is the verb that
    -- CREATES AND STORES: a refusal reported after it would leave a
    -- pending order behind for a carrier whose dispatch loop can never
    -- tick the action that reads it -- an order that sits in the store
    -- for ever, which is the bug.
    --
    -- Reported on the player-visible channel rather than the
    -- developer-log one below, because this is the Mode A analogue: the
    -- player right-clicked and chose an entry, so a silent refusal would
    -- leave them watching a gesture vanish with no explanation.
    if not aiActions.unitHas(uid, TRANSFER_ORDER_ACTION) then
        reportFailure(uid, string.format(
            "%s can't carry a transfer order", unitLabel(uid)))
        return false
    end
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
    -- #1216: an accepted player order supersedes a position hold. Below
    -- every refusal branch above, which store no order and must leave a
    -- standing hold alone.
    hold.clear(s)
    s.transferOrder = nil
    -- Decide on the next tick rather than at the unit's natural
    -- cadence, the same responsiveness commandMove/commandPickup buy.
    s.nextActionAt = 0
    return result.orderId
end

M.action = {
    name = TRANSFER_ORDER_ACTION,
    utility = transferUtility,
    execute = transferExecute,
}
-- The rect-to-rect approach and the contract's own reach measure, both
-- of which Mode A's escort shares verbatim. Exported rather than
-- copied: "close enough to walk no further" and "close enough to
-- commit" must be ONE rule for both modes.
M.moveBesideRect = moveBesideRect
M.approachDist   = approachDist

-- Mode A's action, re-exported (#1250). Its body lives in
-- scripts/unit_ai_escort.lua -- this module is at its #538 line budget
-- and the escort is a self-contained concern -- but the REGISTRATION
-- point stays one name here, beside the queued order it is a peer of,
-- so scripts/unit_ai.lua names both from the same require.
--
-- Registered AFTER M.action in every action list, so a unit that
-- somehow holds both resolves the equal-utility tie to the queued order
-- by list order; neither clears the other.
M.escortAction = require("scripts.unit_ai_escort").action

-- Mode A's TARGET-side hold (#1251), re-exported the same way. It is
-- NOT listed beside the two above: scripts/unit_ai.lua prepends it to
-- EVERY species, because a session's destination may be any
-- player-commandable unit while being a session's SOURCE is a
-- per-species capability. See scripts/unit_ai_escort.lua's header for
-- why that asymmetry is the point rather than an oversight.
M.escortHoldAction = require("scripts.unit_ai_escort").holdAction

M.transferUtility = transferUtility
M.transferExecute = transferExecute

return M
