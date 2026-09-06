-- Unit AI escort hold (#1250, epic #1013 slice UIT-3B): Mode A.
--
-- A player-opened transfer session (scripts/transfer_session.lua) walks
-- its source unit to the destination and HOLDS it there while the
-- player moves items by hand, for as long as the window is open.
--
-- #1251 (UIT-4) added the other side. A session whose DESTINATION is a
-- unit holds that unit too, from creation, and this module owns BOTH
-- sides: which side a unit is on is the session's own answer
-- (`roleOf`), and the two actions share one utility constant, so
-- neither end can outscore the other.
--
-- They are two REGISTRATIONS rather than one, and the split is exactly
-- where the two sides differ in who may do them (#1251 review round 1):
--
--   escort_transfer  the SOURCE side. Registered per species, because
--                    its presence is the question
--                    `transfer_session.resolveSource` and `M.create`
--                    ask before making a unit an escort -- a species
--                    that cannot walk a session over must not be
--                    offered as one (#1250).
--   escort_hold      the TARGET side. Prepended to EVERY species by
--                    `unitAi.registerActions`, because a session's
--                    destination may be ANY player-commandable unit and
--                    the endpoint rule is commandability, never a def
--                    list. Scoping this one per species would leave a
--                    legal target -- a debug-spawned bear put in the
--                    player faction -- whose AI never evaluated the
--                    hold, so it would keep walking while an escort
--                    approached where it used to be. Making it
--                    universal is what keeps the hold endpoint-generic
--                    WITHOUT inventing a def-specific refusal.
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
-- Both are REGISTERED through unit_ai_transfer's own `M.escortAction` /
-- `M.escortHoldAction` re-exports, so scripts/unit_ai.lua names every
-- mode from one require and Mode A's source action sits adjacent to
-- Mode B's in each species list.

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
-- sim keeps walking) -- observation E3 in
-- docs/history/expedition_survival_calibration_2026-07.md measured
-- that. Neither is #1216's position hold, which is a different
-- thing that looks similar: it stands a unit on the tile its own
-- COMPLETED move order named, at follow_command's 7.0 rather than this
-- 7.5, so a session still outranks and supersedes it (M.create clears
-- the anchor on both units it takes), and a survival interrupt that
-- outranks 7.0 walks a held unit off its anchor and back again. This
-- lock does neither: it names a unit the SESSION chose, at a number
-- nothing below 8.0 can move.
--
-- ONE constant for BOTH sides of a unit-to-unit session (#1251, UIT-4):
-- the target's hold is the same in-progress lock as the escort's, which
-- is what makes it preempt that unit's autonomous work exactly like any
-- player order, and what keeps a pair from being pulled apart by one end
-- outscoring the other. Two actions, one number.
local ESCORT_UTILITY = 7.5

-- The session holding this unit, IF it holds it on the side this action
-- speaks for; nil otherwise. Read through package.loaded so a build with
-- the gesture module unloaded simply never scores either action, rather
-- than pulling a UI module into the AI thread.
--
-- Asking for the SIDE rather than filtering afterwards is what makes
-- the two actions mutually exclusive by construction: exactly one of
-- them can score for a given unit, so the dispatch loop never has to
-- break a tie between them.
local function heldAs(uid, side)
    local session = package.loaded["scripts.transfer_session"]
    if not session or type(session.roleOf) ~= "function" then return nil end
    if session.roleOf(uid) ~= side then return nil end
    return session
end

-- There is deliberately NO stall timer here, and UIT-5B (#1254) landed
-- without adding one. Mode B's exists because an order must reach a
-- terminal state on its own; a session is the player's own window and
-- ends when they close it, when a player order preempts it, or when
-- transfer_session's own tick finds an endpoint gone, ineligible, dead
-- or unconscious. A destination that is merely UNREACHABLE is none of
-- those: the escort keeps walking and the player can close the window,
-- which is why a timer would be a change to the hold mechanism rather
-- than failure handling.
--
-- It keeps NO per-unit state either, deliberately: the SESSION is the
-- state, so there is nothing to reconcile, nothing to strip at snapshot
-- time (scripts/unit_ai_save.lua persists an aiState row minus an
-- explicit transient list, so a scratch field here would ride into
-- `lua.unit_ai`), and nothing an interrupted tick can leave behind.
local function escortUtility(uid, _s)
    if not heldAs(uid, "source") then return -math.huge end
    return ESCORT_UTILITY
end

local function escortExecute(uid, _s)
    local session = heldAs(uid, "source")
    if not session then return end
    local active = session.get()
    local info = unit.getInfo(uid)
    if not (active and info) then return end

    -- Already open: stand still, this unit's own approach finished.
    -- `unit.stop` on a unit that is already stopped is a no-op, and
    -- re-running it is what makes this a hold rather than a one-shot --
    -- an interruption that walked the unit away (combat, a mental break)
    -- leaves it standing wherever it ended up once this action wins
    -- again. Still the honest best-effort after UIT-5B (#1254), which
    -- covers a held unit that stops being commandable AT ALL and
    -- deliberately not one that was merely pulled off its mark and can
    -- resume.
    if active.phase ~= session.PHASE_APPROACHING then
        unit.stop(uid)
        return
    end

    local dest = session.destinationNow()
    if not dest then
        -- The destination stopped existing mid-approach. Retire the
        -- session QUIETLY rather than holding a unit against nothing.
        -- Since UIT-5B (#1254) transfer_session.update notices this on
        -- its own tick, and a wider set of failures with it, so this is
        -- the AI-side belt rather than the only detector -- both run the
        -- same one teardown, whichever gets there first.
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

-- The TARGET side (#1251). Its entire job is to be where the escort is
-- walking to, from the moment the session exists — standing still
-- through the approach is what gives that walk a fixed destination — so
-- it never approaches anything and has no phase to consult. `unit.stop`
-- every tick for the same reason the source's open phase does it.
--
-- No forceExecute here either: a held target is idle, so this runs every
-- tick anyway, and re-issuing anything mid-walk is exactly what the
-- source side avoids.
local function holdUtility(uid, _s)
    if not heldAs(uid, "target") then return -math.huge end
    return ESCORT_UTILITY
end

local function holdExecute(uid, _s)
    if not heldAs(uid, "target") then return end
    unit.stop(uid)
end

M.holdAction = {
    name = "escort_hold",
    utility = holdUtility,
    execute = holdExecute,
}

M.escortUtility = escortUtility
M.escortExecute = escortExecute
M.holdUtility   = holdUtility
M.holdExecute   = holdExecute

return M
