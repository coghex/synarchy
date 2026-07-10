-- Unit AI: mental short-circuits (#352) — the "can't act purposefully"
-- gates the dispatch loop checks before scoring any action:
--
--   * Delirium (physiological): consciousness in the delirious band
--     (heat stroke / hypoxia / salt imbalance — not yet unconscious,
--     which collapses the unit instead). Moved here verbatim from
--     unit_ai.lua's tickOne; semantics unchanged.
--   * Mental break (psychological, scripts/mental_state.lua): the
--     state-of-mind episode. Behaviour was rolled once at episode
--     entry and held: aimless wander (the same stumble delirium uses)
--     or fleeing — running away from the nearest other unit.
--
-- Both run BEFORE the nextActionAt cadence gate (a broken/delirious
-- unit must not sit frozen waiting for its next decision slot), so
-- keep them cheap and only issue moves when the unit isn't already
-- mid-walk. Internal module (plain `return M`), imported by
-- unit_ai.lua's tickOne — same shape as the other #538 submodules.

local brain  = require("scripts.brain")
local mental = require("scripts.mental_state")
local needs  = require("scripts.unit_ai_needs")
local mv     = require("scripts.movement_speed")

local M = {}

local FLEE_RADIUS      = 15.0  -- only units this close are fled from
local FLEE_TARGET_DIST = 8.0   -- matches retreat's away-tile distance

local function nearestOtherUnit(uid, me)
    local best, bestD2 = nil, FLEE_RADIUS * FLEE_RADIUS
    for _, oid in ipairs(unit.getAllIds() or {}) do
        if oid ~= uid and unit.getPose(oid) ~= "dead" then
            local oi = unit.getInfo(oid)
            if oi then
                local dx, dy = oi.gridX - me.gridX, oi.gridY - me.gridY
                local d2 = dx * dx + dy * dy
                if d2 < bestD2 then best, bestD2 = oi, d2 end
            end
        end
    end
    return best
end

-- Run directly away from the nearest other unit (retreat's away-tile
-- math, unit_ai_combat.lua), with a little angular jitter so a panic
-- run blocked by terrain slides around it on the next re-issue rather
-- than ramming the same wall. Nobody within FLEE_RADIUS → undirected
-- panic (wander).
local function fleeExecute(uid, s, params)
    local me = unit.getInfo(uid)
    if not me then return end
    local other = nearestOtherUnit(uid, me)
    if not other then
        needs.wanderExecute(uid, s, params)
        return
    end
    local dx, dy = me.gridX - other.gridX, me.gridY - other.gridY
    if dx * dx + dy * dy < 0.001 then dx, dy = 1, 0 end
    local angle = math.atan(dy, dx) + (math.random() - 0.5) * 0.8
    unit.moveTo(uid,
                me.gridX + math.cos(angle) * FLEE_TARGET_DIST,
                me.gridY + math.sin(angle) * FLEE_TARGET_DIST,
                mv.ordered(uid))
end

-- Returns true when it handled the tick — the unit can't act
-- purposefully and the dispatch loop must not score actions.
function M.shortCircuit(uid, s, params, activity)
    -- Delirium: the unit stumbles — aimless slow wander, no goals/
    -- work/combat. Only re-issue when not already moving (no spam).
    if brain.isDelirious(uid) then
        if activity ~= "walking" and activity ~= "running" then
            needs.wanderExecute(uid, s, params)
        end
        s.currentAction = "delirious"
        return true
    end

    -- Mental break: same short-circuit, psychologically driven.
    -- Persistent state (claims, phase machines) stays, exactly like
    -- delirium — preempted work resumes once the episode passes.
    if mental.isBreaking(uid) then
        if activity ~= "walking" and activity ~= "running" then
            if mental.breakBehavior(uid) == "flee" then
                fleeExecute(uid, s, params)
            else
                needs.wanderExecute(uid, s, params)
            end
        end
        s.currentAction = "mental_break"
        return true
    end

    return false
end

return M
