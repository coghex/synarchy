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
--     entry and held: aimless wander (the same stumble delirium uses),
--     fleeing — running away from the nearest other unit, catatonia —
--     frozen in place, or lash-out — episode-owned combat (below).
--
-- Both run BEFORE the nextActionAt cadence gate (a broken/delirious
-- unit must not sit frozen waiting for its next decision slot), so
-- keep them cheap and only issue moves when the unit isn't already
-- mid-walk. Internal module (plain `return M`), imported by
-- unit_ai.lua's tickOne — same shape as the other #538 submodules.

local brain        = require("scripts.brain")
local mental       = require("scripts.mental_state")
local needs        = require("scripts.unit_ai_needs")
local mv           = require("scripts.movement_speed")
local core         = require("scripts.unit_ai_core")
local combatAttack = require("scripts.unit_ai_combat_attack")

local M = {}

local FLEE_RADIUS      = 15.0  -- only units this close are fled from
local FLEE_TARGET_DIST = 8.0   -- matches retreat's away-tile distance

-- Lash-out (#717) target-policy constants.
local LASHOUT_RANGE           = 8.0   -- chebyshev tiles
local LASHOUT_ATTACKER_WINDOW = 10.0  -- matches unit_ai_combat's ENGAGE_WINDOW_SEC

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

-- Lash-out (#717) target policy: who's fair game. Self, dead/collapsed,
-- and the technomule (the established noncombatant-mechanical-unit
-- exclusion idiom, scripts/unit_ai_fetch.lua) are excluded; faction is
-- NOT checked — a lashing-out unit may attack an ally.
local function eligibleLashoutTarget(uid, me, oid)
    if oid == uid then return false end
    if not unit.exists(oid) then return false end
    local pose = unit.getPose(oid)
    if pose == "dead" or pose == "collapsed" then return false end
    local info = unit.getInfo(oid)
    if not info or info.defName == "technomule" then return false end
    local d = math.max(math.abs(me.gridX - info.gridX),
                        math.abs(me.gridY - info.gridY))
    return d <= LASHOUT_RANGE
end

-- Prefer a recent live attacker (same engage window unit_ai_combat uses)
-- that's still eligible; otherwise the nearest eligible unit. "Nearest"
-- ranks by the SAME Chebyshev metric eligibility itself is gated on
-- (LASHOUT_RANGE above) — ranking by Euclidean distance instead would
-- disagree with it (e.g. an (8,0) candidate reading "nearer" than a
-- (6,6) one, despite the latter being closer under Chebyshev).
local function pickLashoutTarget(uid, me)
    local att = unit.getLastAttacker(uid)
    if att and (engine.gameTime() - (att.at or 0)) <= LASHOUT_ATTACKER_WINDOW
       and eligibleLashoutTarget(uid, me, att.uid) then
        return att.uid
    end
    local best, bestD = nil, math.huge
    for _, oid in ipairs(unit.getAllIds() or {}) do
        if eligibleLashoutTarget(uid, me, oid) then
            local info = unit.getInfo(oid)
            local d = math.max(math.abs(info.gridX - me.gridX),
                                math.abs(info.gridY - me.gridY))
            if d < bestD then best, bestD = oid, d end
        end
    end
    return best
end

-- Lash-out execute: episode-owned combat. The mental short-circuit runs
-- BEFORE candidate scoring (tickOne returns as soon as it sees true), so
-- the ordinary attack_target candidate never gets a chance to fire on
-- its own — drive its swing/pursuit logic directly instead. Target
-- state rides the same s.attackTargetUid / s.activeGoal fields
-- commandAttack uses, so the shared combat plumbing (cooldowns, lunge,
-- anim) just works; episode-end cleanup (in shortCircuit) clears them
-- the same way once the episode ends.
local function lashOutExecute(uid, s, params)
    local me = unit.getInfo(uid)
    if not me then return end

    local hadTarget = s.attackTargetUid ~= nil
    if s.attackTargetUid
       and not eligibleLashoutTarget(uid, me, s.attackTargetUid) then
        s.attackTargetUid = nil
    end
    if not s.attackTargetUid then
        local target = pickLashoutTarget(uid, me)
        if not target then
            -- No one to lash out at: agitated wander, keep looking —
            -- the next tick re-tries pickLashoutTarget. A target lost
            -- THIS tick (dead/collapsed/out of range) already has a
            -- combat pursuit in flight (attackTargetExecute's own
            -- moveTo, unit_ai_combat_attack.lua) — stop it immediately
            -- rather than letting the unit walk out that stale leg
            -- before the activity-gated wander below can fire.
            if hadTarget then unit.stop(uid) end
            local activity = unit.getActivity(uid)
            if activity ~= "walking" and activity ~= "running" then
                needs.wanderExecute(uid, s, params)
            end
            return
        end
        core.setGoal(s, "attack")
        s.attackTargetUid = target
        s.committed        = nil
    end

    combatAttack.attackTargetExecute(uid, s, params)
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
    -- Episode-end cleanup (#717): lash-out's target/goal belong only to
    -- the episode. Tracked via s.mentalLashoutActive (set/cleared below)
    -- rather than inferred from s.currentAction — delirium (just below)
    -- preempts currentAction to "delirious" whenever it overlaps a
    -- break, so a break that ends while the unit is STILL delirious
    -- would otherwise never match "mental_break" and skip this cleanup
    -- entirely, leaking the episode's target/goal into ordinary combat
    -- AI once the unit becomes lucid again. The flag survives that
    -- preemption untouched, so this still fires the tick isBreaking
    -- goes false regardless of what currentAction reads by then.
    if s.mentalLashoutActive and not mental.isBreaking(uid) then
        s.attackTargetUid = nil
        s.committed        = nil
        s.mentalLashoutActive = nil
        core.markGoalAccomplished(s, "attack")
        unit.clearAnimOverride(uid)
        -- Stop any in-flight pursuit (an out-of-range target has one:
        -- attackTargetExecute's own moveTo, unit_ai_combat_attack.lua).
        -- Clearing only the Lua-side state above isn't enough on its
        -- own: if delirium ALSO overlaps this exact tick, the branch
        -- below only re-issues a move when the unit isn't already
        -- walking/running (its own no-spam gate) — so a still-moving
        -- pursuit would otherwise ride out its old leg untouched.
        unit.stop(uid)
    end

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
        local behavior = mental.breakBehavior(uid)

        if not s.mentalLashoutActive and behavior == "lash_out" then
            -- Fresh episode: lash-out picks its own target from scratch
            -- via pickLashoutTarget, never inheriting whatever combat
            -- (or lack of it) was running the moment before the break.
            s.attackTargetUid = nil
            s.committed        = nil
            unit.clearAnimOverride(uid)
        end
        if behavior == "lash_out" then s.mentalLashoutActive = true end

        if behavior == "catatonia" then
            -- Frozen in place: no replacement movement, work, combat,
            -- survival, or order action for the whole episode.
        elseif behavior == "lash_out" then
            lashOutExecute(uid, s, params)
        elseif activity ~= "walking" and activity ~= "running" then
            if behavior == "flee" then
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
