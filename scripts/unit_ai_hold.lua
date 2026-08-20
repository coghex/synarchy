-- Position hold after a completed player move order (#1216).
--
-- Project-owner decision (2026-08-10, SURV-4,
-- docs/expedition_survival_calibration.md): a unit that COMPLETES a
-- player-issued move order stands at the destination instead of
-- drifting off on ambient wander. It stops contributing autonomously
-- until re-commanded -- that is the deliberate trade-off, not a
-- side effect.
--
-- The hold is one field, `s.holdAnchor = { x, y }`, and one candidate
-- action, `hold_position`, scored at exactly the utility
-- follow_command carries (unit_ai_combat.lua's
-- FOLLOW_COMMAND_UTILITY). Scoring it in the SAME band is the whole
-- mechanism, and it is what keeps the #306 ladder intact without
-- restating it: everything that could interrupt the move order that
-- created this hold -- dire self survival (drink/eat/dry-canteen
-- refill), combat (engage/retreat), treatment (treat_ally), a mental
-- break's own pre-scoring short-circuit -- still outranks it, while
-- everything the order already outranked -- ambient wander, work
-- ENTRY and its in-progress locks (<=6.0), the situational goals --
-- still loses. A second number would be a second ladder free to
-- drift from the first.
--
-- The anchor is created in ONE place: unit_ai_stall.lua's maintainTask,
-- on the branch that already distinguishes ARRIVAL from the
-- TASK_TIMEOUT_SEC stall (requirement 5 -- an order that gave up
-- creates no hold). It is written there rather than called out to from
-- there because unit_ai_stall.lua is deliberately dependency-free and
-- this module requires it.
--
-- Who CLEARS it is the other half of the contract, and the rule is
-- "an accepted, explicit player command, and nothing else":
--   * unit_ai_core.lua's commandMove (a player move; the `internal`
--     opt-out keeps scripts/building_spawn.lua's portal walk-out from
--     ever creating or clearing one) and commandAttack when
--     `committed` is set -- an AI's own emergent engage
--     (unit_ai_combat.lua) passes no flag and stays a temporary
--     interrupt;
--   * unit_ai_pickup.lua's commandPickup and unit_ai_transfer.lua's
--     commandTransferOrder, both on their ACCEPTED path only -- a
--     refusal issues no order, so it must leave the hold standing;
--   * scripts/transfer_session.lua's Mode A session, on both units it
--     takes;
--   * unitAi.releaseHold(uid), the scripted/console release.
-- Nothing else clears it, and in particular nothing does so on the
-- unit's own behalf: the whole point is that the unit stays put.

local unitAi = package.loaded["scripts.unit_ai"]

local mv = require("scripts.movement_speed")
-- Eligible-time stall accounting (#1291) plus the arrival radius the
-- anchor is judged by -- the same one maintainTask uses to decide the
-- order arrived, so "arrived" and "still here" can never disagree.
local stall = require("scripts.unit_ai_stall")

local M = {}

-- unit_ai_core's own distance, duplicated for the same reason
-- unit_ai_stall.lua duplicates it: requiring core from here would be a
-- load cycle (core requires this module for its command verbs).
local function distance(ax, ay, bx, by)
    local dx = ax - bx
    local dy = ay - by
    return math.sqrt(dx * dx + dy * dy)
end

-- Exactly unit_ai_combat.lua's FOLLOW_COMMAND_UTILITY. Stated here as
-- its own constant rather than required from there, because that
-- module requires unit_ai_core, which requires this one; the pair is
-- pinned together by Test.Headless.Lua.UnitAiHold instead.
local HOLD_UTILITY = 7.0

-- Drop the hold. Takes the state table (nil-tolerant: a command can
-- reach a unit the AI has never ticked).
function M.clear(s)
    if s then s.holdAnchor = nil end
end

-- The hold's own return budget. An anchor the unit cannot get back to
-- -- terrain edited out from under it while an interrupt had it
-- elsewhere -- must not re-path forever, so the walk home is charged
-- against TASK_TIMEOUT_SEC exactly the way the move order that created
-- it was: ELIGIBLE time only, reset by a new closest approach, and
-- whole again whenever the unit is actually standing on the anchor
-- (nothing is stalled then, and the next displacement deserves a full
-- budget). Charging happens in the utility, the same tick cadence
-- unit_ai_pickup.lua charges its own order at.
local function holdUtility(uid, s)
    local a = s.holdAnchor
    if not a then return -math.huge end
    -- A live commanded task owns the unit outright, so the two never
    -- compete at their shared utility: whatever hold a previous order
    -- left behind waits until this one arrives or gives up. Ties would
    -- otherwise be decided by registry order, which is not a contract.
    if s.commandedTask then return -math.huge end
    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local now = engine.gameTime()
    local d = distance(info.gridX, info.gridY, a.x, a.y)
    if d <= stall.TASK_ARRIVAL_TILES then
        a.bestDist = nil
        stall.reset(a, now)
        return HOLD_UTILITY
    end

    local eligible = s.currentAction == "hold_position"
    if eligible and (not a.bestDist
                     or d < a.bestDist - stall.TASK_PROGRESS_TILES) then
        a.bestDist = d
        stall.reset(a, now)
    end
    if stall.charge(a, eligible, now) > stall.TASK_TIMEOUT_SEC then
        -- Unreachable anchor: release rather than loop. The unit goes
        -- back to ordinary autonomous behaviour, which is the same
        -- answer a stalled move order gets.
        s.holdAnchor = nil
        return -math.huge
    end
    return HOLD_UTILITY
end

local function holdExecute(uid, s)
    local a = s.holdAnchor
    if not a then return end
    local info = unit.getInfo(uid)
    if not info then return end
    if distance(info.gridX, info.gridY, a.x, a.y)
            <= stall.TASK_ARRIVAL_TILES then
        unit.stop(uid)
        return
    end
    -- Walking BACK is the unit tidying up after an interrupt carried it
    -- off, not the player's order being re-issued, so it ambles at
    -- comfort rather than the `ordered` push commandMove asks for.
    unit.moveTo(uid, a.x, a.y, mv.comfort(uid))
end

-- Universal candidate (registered in scripts/unit_ai.lua's
-- UNIVERSAL_ACTIONS): any species that can be handed a move order can
-- be left holding by one.
M.action = {
    name    = "hold_position",
    utility = holdUtility,
    execute = holdExecute,
}

-----------------------------------------------------------
-- Public: scripted / console verbs
--
-- aiState is reached through the singleton rather than
-- unit_ai_core.lua (the load cycle above), and per call rather than
-- captured at module scope, so require order stays irrelevant.
-----------------------------------------------------------

-- | Release `uid`'s position hold without issuing any movement
--   (requirement 3). Returns true if the unit was holding.
function unitAi.releaseHold(uid)
    local all = unitAi.aiState
    local s = all and all[uid]
    if not s or not s.holdAnchor then return false end
    s.holdAnchor = nil
    return true
end

-- | `uid`'s hold anchor as { x = , y = }, or nil when it is not
--   holding. A copy: the live table carries this module's own stall
--   accounting, which is nobody else's to read or write.
function unitAi.getHold(uid)
    local all = unitAi.aiState
    local s = all and all[uid]
    local a = s and s.holdAnchor
    if not a then return nil end
    return { x = a.x, y = a.y }
end

M.HOLD_UTILITY = HOLD_UTILITY
M.holdUtility  = holdUtility
M.holdExecute  = holdExecute

return M
