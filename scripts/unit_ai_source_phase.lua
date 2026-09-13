-- Lifecycle of drink_from_source's phase lock (#2545).
--
-- scripts/unit_ai_water.lua drives the no-canteen drinking sequence
-- through a per-unit phase flag, and drinkFromSourceUtility returns
-- math.huge for as long as that flag is set, so a half-completed
-- descent can't be preempted by an ordinary errand. That lock is the
-- whole hazard: any way the sequence stops making progress strands the
-- unit at infinite priority with nothing able to outscore it, and the
-- sequence's own exit conditions are reached only by executing it.
--
-- Three such ways are reachable in shipped code:
--
--   * A REVIVE. unit_resource_injury.lua stands up any healthy crawling
--     unit, and this sequence dwells in Crawling from the bottom of the
--     descent to the top of the ascent -- so the AI re-issued the
--     descent every thought tick against a revive that undid it at
--     10 Hz, and the drinking phase was never reached at all. That one
--     is closed at the source: the injury tick now exempts an active
--     source phase, the same way #612 exempts the sleep chain. The
--     exemption is only safe BECAUSE the phase is bounded here -- a
--     phase that could never end would convert the reported stranding
--     into a permanent suppression of genuine injury revival.
--   * A PREEMPTION. unit_ai_mental.preempt swaps the running action out
--     on delirium or a mental break and fires only the OUTGOING
--     action's onExit, and the episode's wander then walks the unit off
--     the bank. Phase machines deliberately survive an ordinary switch
--     (scripts/unit_ai.lua), so this one needs an onExit of its own;
--     without it the flag outlived the episode and re-locked at
--     math.huge from wherever the unit had been left.
--   * ANYTHING ELSE that takes the posture or the water away mid-drink:
--     a fall knockdown, a newly disabling injury, a source that dries
--     up. unit_resource_tick.sourceDrinkingEligible (#2541) stops
--     paying hydration immediately in every one of them, so the 95%
--     exit condition the drinking phase waits on becomes unreachable.
--
-- The bound is what makes that last, open-ended class safe without
-- enumerating it. It is deliberately NOT a "re-descend and try again"
-- repair: hydration eligibility is re-read live against an ADJACENT
-- source, so restoring only the POSTURE fixes nothing for a unit that
-- is no longer at the bank -- it would re-enter the same infinite lock
-- with a fresh pose. Abandoning returns the utility to a finite score,
-- which is all ordinary arbitration needs to take the unit back.
--
-- Abandoning is not a cooldown, and deliberately so: the very next
-- thought tick may score drink_from_source highest again and enter a
-- FRESH sequence with a fresh budget. In every reachable case that is
-- the right answer rather than a re-lock in disguise -- the standing
-- entry re-reads the remembered tile's fluid and forgets a source that
-- has dried up (so the utility goes to -inf on the tick after), a unit
-- still on a real bank SHOULD keep trying to drink, and a preempted
-- unit is not scoring actions at all while its episode lasts. What the
-- bound guarantees is that no single attempt can hold infinite priority
-- indefinitely, which is what ordinary arbitration needs.
--
-- The deadline is game time, so it stretches and compresses with the
-- time scale exactly as the sequence it bounds does.

local M = {}

-- Game-seconds one descend/drink/ascend sequence may hold the lock.
-- A healthy sequence is a handful of seconds -- two strided pose
-- transitions each way, and regen_factor_source_drinking (5 L/s) lifts
-- a typical acolyte from the drink threshold to the 95% exit in about
-- two -- so this is an order of magnitude of slack before a sequence
-- reads as "not progressing".
local BUDGET = 60.0

-- True while the sequence owns the math.huge lock.
function M.active(s)
    return s.sourcePhase ~= nil
end

-- Enter the sequence, starting its budget.
function M.begin(s)
    s.sourcePhase   = "descending"
    s.sourcePhaseAt = engine.gameTime()
end

-- Advance to the next leg. The budget covers the WHOLE sequence rather
-- than restarting per leg: a lock that renewed its own deadline at
-- every phase change would not be bounded by it at all, since a stalled
-- sequence can still change phase (a drinking unit that reaches 95%
-- from a canteen bolus enters an ascent it may never be able to climb).
function M.advance(s, phase)
    s.sourcePhase = phase
end

-- Release the lock. Both fields die together: a deadline must never
-- outlive the phase it bounds, and unit_resource_injury.lua's crawling
-- exemption keys on the phase alone, so a phase left set by a cleared
-- deadline would suppress a genuine revive forever.
function M.clear(s)
    s.sourcePhase   = nil
    s.sourcePhaseAt = nil
end

-- Check the budget for one execute tick. Returns true once the sequence
-- has been ABANDONED, which is the caller's signal to run no phase this
-- tick -- re-entering from the standing entry on the same tick would
-- start a fresh budget and make the bound vacuous.
--
-- A restored save arrives with the phase and no deadline
-- (unit_ai_save.lua strips the clock, which cannot outlive the session
-- it was measured in): that re-arms here with a full budget, the same
-- answer every other reloaded wait in the AI gives.
function M.expire(s)
    if s.sourcePhase == nil then return false end
    local now = engine.gameTime()
    if s.sourcePhaseAt == nil then
        s.sourcePhaseAt = now
        return false
    end
    if now - s.sourcePhaseAt < BUDGET then return false end
    M.clear(s)
    return true
end

-- Exposed so a gate can restate the bound instead of hard-coding it.
M.BUDGET = BUDGET

return M
