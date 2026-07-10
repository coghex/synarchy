-- Unit AI: mental short-circuits (#352) — the "can't act purposefully"
-- gates the dispatch loop checks before scoring any action:
--
--   * Delirium (physiological): consciousness in the delirious band
--     (heat stroke / hypoxia / salt imbalance — not yet unconscious,
--     which collapses the unit instead). Moved here from unit_ai.lua's
--     tickOne, plus one fix: entry now fires the running action's
--     onExit (see preempt below) — the old inline block skipped it.
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

-- Entering a short-circuit state PREEMPTS whatever action was running:
-- fire its onExit exactly like the dispatcher's switch path does, so
-- work phase machines reset their elapsed-time accumulators (construct's
-- building→walking, craft's setBillWorking) — without this, an episode
-- keeps a craft bill drawing power throughout and lands the entire
-- 60–120 s interruption as instant progress on resume. Persistent
-- state (claims, consumed materials) stays, same as any preemption.
local function preempt(uid, s, params, actList, newName)
    if s.currentAction == newName then return end
    if s.currentAction then
        for _, a in ipairs(actList or {}) do
            if a.name == s.currentAction then
                if a.onExit then a.onExit(uid, s, params) end
                break
            end
        end
    end
    s.currentAction   = newName
    s.actionStartedAt = engine.gameTime()
end

-- Returns true when it handled the tick — the unit can't act
-- purposefully and the dispatch loop must not score actions.
function M.shortCircuit(uid, s, params, activity, actList)
    -- Delirium: the unit stumbles — aimless slow wander, no goals/
    -- work/combat. Only re-issue when not already moving (no spam).
    if brain.isDelirious(uid) then
        preempt(uid, s, params, actList, "delirious")
        if activity ~= "walking" and activity ~= "running" then
            needs.wanderExecute(uid, s, params)
        end
        return true
    end

    -- Mental break: same short-circuit, psychologically driven.
    if mental.isBreaking(uid) then
        preempt(uid, s, params, actList, "mental_break")
        if activity ~= "walking" and activity ~= "running" then
            if mental.breakBehavior(uid) == "flee" then
                fleeExecute(uid, s, params)
            else
                needs.wanderExecute(uid, s, params)
            end
        end
        return true
    end

    return false
end

return M
