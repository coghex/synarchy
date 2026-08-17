-- Unit AI escort hold (#1250, epic #1013 slice UIT-3B): Mode A.
--
-- A player-opened transfer session (scripts/transfer_session.lua) walks
-- its source unit to the destination and HOLDS it there while the
-- player moves items by hand, for as long as the window is open.
--
-- Split out of scripts/unit_ai_transfer.lua, which owns Mode B's queued
-- order and is at its #538 line budget. The two still share their whole
-- vocabulary -- the same rect-to-rect approach and the same
-- contract-owned reach rule, required from that module rather than
-- copied, so "close enough to walk no further" and "close enough to
-- commit" cannot become two different rules. What differs is only what
-- happens on arrival: the order commits and finishes, the escort stops
-- and waits for the player.
--
-- The action is REGISTERED through unit_ai_transfer's own
-- `M.escortAction` re-export, so scripts/unit_ai.lua names both modes
-- from one require and the two sit adjacent in every action list.

local M = {}

-- Mode B's module is required LAZILY, and that is load-bearing rather
-- than style: it requires THIS module at its own tail (to re-export the
-- action), so resolving it at module scope here would be a load cycle —
-- `require` publishes a module only after its loader returns, so the
-- re-entry would re-execute it. By the time anything below runs, that
-- module is fully loaded and `require` is a package.loaded lookup.
local function shared()
    return require("scripts.unit_ai_transfer")
end

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
    local transfer = shared()
    if transfer.approachDist(info, dest) > 1 then
        transfer.moveBesideRect(uid, info, dest.gridX, dest.gridY,
                                dest.tileW or 1, dest.tileH or 1)
        return
    end
    unit.stop(uid)
    session.markArrived()
end

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
M.action = {
    name = "escort_transfer",
    utility = escortUtility,
    execute = escortExecute,
}
M.escortUtility = escortUtility
M.escortExecute = escortExecute

return M
