-- Transfer-order OUTCOMES (#1253, epic #1013 slice UIT-5A): how a
-- durable order's ending reaches the player, what happens to the order
-- afterwards, and the player's own way to call one off.
--
-- Split out of unit_ai_transfer.lua, which stays the BEHAVIOUR (when to
-- walk, when to give up) and consumes this. The seam is the one #1253
-- draws: everything here is about an order ENDING, and every exit
-- follows the same two steps in the same order.
--
--   1. SURFACE the outcome, exactly once. Failures go through the
--      existing unit_warning path (reportFailure) naming the carrier and
--      the target; a completion or a cancellation is a unit_event.
--   2. PRUNE the order (finishOrder). The store holds LIVE work only,
--      like craft bills, so nothing terminal rides a save or keeps
--      reporting a demolished endpoint to the integrity sweep.
--
-- Doing it in that order matters, and so does doing the second step
-- UNCONDITIONALLY: engine.emitEvent* drops a line whose category the
-- player's notification settings have muted, so cleanup that waited on
-- the event being recorded would strand a muted failure in the store
-- forever.
--
-- It is also what makes the handling EDGE-TRIGGERED. Pruning at the
-- terminal transition leaves a later tick nothing to re-surface or
-- re-run, so idempotence is structural rather than a flag somebody has
-- to remember to set -- and unit.pruneTransferOrder answers false for an
-- order already gone, so even a repeated call is inert.
--
-- The one exit NOT here is the carrier itself dying or being destroyed:
-- nothing ticks a unit in either state, so that retirement is the
-- engine's (Unit.Transfer.Live.retireTransferOrdersEverywhere, driven
-- from Unit.Thread.Command.Lifecycle's destroy and
-- Unit.Thread.Command.Pose's kill).

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local reportFailure = core.reportFailure
local ensureState   = core.ensureState

-- The player-facing item/unit labels the pickup order already
-- established ("Field Radio", not "radio"; a personal name if the unit
-- has one). Required rather than re-derived so a transfer warning and a
-- pickup warning can never name the same thing two different ways.
local pickup = require("scripts.unit_ai_pickup")
-- The transfer order's action name, from its one owner (#2030), so the
-- "is this unit mid-order?" test below cannot drift from the name the
-- dispatch loop registers it under.
local TRANSFER_ORDER_ACTION =
    require("scripts.unit_ai_actions").TRANSFER_ORDER_ACTION
local itemLabel = pickup.itemLabel
local unitLabel = pickup.unitLabel

local M = {}

-- The player-facing name of an endpoint as it stands RIGHT NOW, or nil
-- when it can no longer be resolved -- which is exactly what a
-- demolished building or a dead partner reads as.
local function endpointName(ep)
    local where = ep and unit.transferEndpointInfo(ep)
    local name = where and where.displayName
    if type(name) == "string" and name ~= "" then return name end
    return nil
end

-- How a warning NAMES the target (requirement 2). The role comes from
-- the order rather than being assumed to be the destination: a Retrieve
-- gesture walks its carrier to the SOURCE, and telling the player their
-- acolyte could not reach the "destination" would point at the wrong end
-- of the trip. The live display name is appended when there still is
-- one, so "couldn't reach" identifies which hold and "no longer exists"
-- honestly does not pretend to.
function M.targetPhrase(order)
    local role = (order.approachRole == "source") and "source" or "destination"
    local name = endpointName(order.approach)
    return name and string.format("transfer %s (%s)", role, name)
                 or ("transfer " .. role)
end

-- Terminal cleanup: drop the order and let go of the trip bookkeeping.
-- Called at the transition that made the order terminal, and always
-- AFTER whatever surfaced its outcome. See the header for why that
-- order, and why this is not conditional on the event being recorded.
function M.finishOrder(uid, s, oid)
    unit.pruneTransferOrder(uid, oid)
    s.transferOrder = nil
    s.transferCandidate = nil
end

-- The instance ids of every entry that is ALREADY terminal.
--
-- Taken from the order as it stood BEFORE a commit, this is exactly the
-- set whose outcome some earlier moment already told the player about --
-- see reportOutcomes. Ids are unique within a batch by construction (the
-- contract refuses a request with a duplicate instance,
-- `ErrDuplicateInstance`), so keying on them is unambiguous and, unlike
-- a positional index, cannot silently mis-pair if the two lists ever
-- stop being built from the same `tbEntries` walk.
function M.settledIds(order)
    local settled = {}
    for _, e in ipairs((order or {}).entries or {}) do
        if e.state == "failed" or e.state == "cancelled"
           or e.state == "completed" then
            settled[e.instanceId] = true
        end
    end
    return settled
end

-- One player-visible warning for a set of refused entries, naming the
-- carrier and the item -- bounded, because a twelve-item batch that
-- wholly refuses must not file twelve separate warnings. The first
-- refused item is named in full and the rest are counted, which is what
-- a player needs to act ("it wouldn't fit") without a wall of text.
--
-- `alreadyReported` (a settledIds set, optional) is what keeps "exactly
-- once" true across the two moments that report on one order. A commit
-- result carries an outcome for EVERY requested item, create-time
-- refusals included -- twelve into room for eight comes back as eight
-- completions and the same four `receiver_full` entries the command-time
-- gate already warned about when the order was queued. Reporting the
-- whole list again at arrival would file that warning twice for one
-- refusal. Each moment reports only what it produced.
--
-- `reason`/`cause` come straight from the engine's structured
-- vocabulary; the message quotes the cause when there is one, since for
-- an arrival refusal (`became_stale`) the cause IS the explanation.
function M.reportOutcomes(uid, outcomes, lead, alreadyReported)
    local first, n = nil, 0
    for _, o in ipairs(outcomes or {}) do
        if o.state == "failed"
           and not (alreadyReported and alreadyReported[o.instanceId]) then
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
-- Explicit cancellation (requirement 1)
-----------------------------------------------------------
-- Every non-terminal order `uid` is the ACTING unit of.
--
-- Deliberately wider than unit_ai_transfer's actionableOrder, which
-- additionally skips an order with no approachRole. That order is D-10's
-- building-to-building pair: valid in the store, carried by this unit,
-- and inert because no executor exists for it yet. #1247 recorded that
-- nothing in that slice could cancel one; this gesture is the something,
-- and leaving it out would make the one order the player can never
-- otherwise be rid of the one the menu refuses to offer.
local function pendingOrders(uid)
    local out = {}
    for _, o in ipairs(unit.getTransferOrders(uid) or {}) do
        if not o.terminal then out[#out + 1] = o end
    end
    return out
end

-- Does `uid` have an order the player could cancel? The context menu's
-- gate: no active order means NO entry (requirement 1), never a disabled
-- one, matching how every other transfer gesture omits what it cannot
-- run (#1249).
function unitAi.hasActiveTransferOrder(uid)
    return #pendingOrders(uid) > 0
end

-- Cancel `uid`'s transfer work outright. Returns true if anything was
-- cancelled. This is what the unit context menu's "Cancel transfer" row
-- invokes.
--
-- ALL of the unit's non-terminal orders, not merely the one it happens
-- to be walking: the entry promises to release the unit, and releasing
-- it from the first of three queued hauls would leave it setting off on
-- the second before the menu had closed.
--
-- Movement stops only when the transfer job is the action actually
-- holding the unit. Cancelling while it eats or fights must not yank it
-- out of that -- the order it is not currently running is still
-- cancelled -- and no OTHER command field is touched, so a commanded
-- move or a pending pickup issued alongside the order survives exactly
-- as it was (unit_ai's command fields are independent by design).
function unitAi.cancelTransferOrder(uid)
    local pending = pendingOrders(uid)
    if #pending == 0 then return false end
    local s = ensureState(uid)
    if s.currentAction == TRANSFER_ORDER_ACTION then unit.stop(uid) end
    local info = unit.getInfo(uid)
    for _, o in ipairs(pending) do
        -- Named BEFORE the cancel, while the endpoint is still whatever
        -- it was when the player pointed at it.
        local phrase = M.targetPhrase(o)
        -- cancelBatch semantics: PENDING entries become cancelled and
        -- terminal ones are left alone, so a cancel landing after a
        -- partial commit records six delivered and six abandoned rather
        -- than overwriting six real deliveries. Nothing moves.
        unit.cancelTransferOrder(uid, o.id)
        -- unit_event, not unit_warning: the player chose this, so it is
        -- a thing that happened, not a thing that went wrong. Attributed
        -- to the carrier, which is what files it in that unit's own Log
        -- tab (requirement 1).
        local msg = string.format("%s's %s was cancelled", unitLabel(uid),
                                  phrase)
        if info and info.gridX then
            engine.emitEventForUnit("unit_event", msg, uid,
                math.floor(info.gridX), math.floor(info.gridY))
        else
            engine.emitEventForUnit("unit_event", msg, uid)
        end
        M.finishOrder(uid, s, o.id)
    end
    -- Re-decide on the next tick rather than at the unit's natural
    -- cadence, the same responsiveness commandTransferOrder buys.
    s.nextActionAt = 0
    return true
end

M.pendingOrders = pendingOrders

return M
