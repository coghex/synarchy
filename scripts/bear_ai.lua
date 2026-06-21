-- Bear AI — species-specific candidates + config.
--
-- Owns bear ambient behavior (idle / wander / alert / rest) and the
-- bear_brown config block. Plugs into the unit_ai framework via
-- unitAi.registerActions / unitAi.setConfig. Combat candidates
-- (engage / attack_target / retreat) come from unit_ai itself —
-- registerActions auto-prepends them so we don't restate them here.
--
-- Future bear species (panda, polar bear, grizzly variants) can
-- either reuse this file's helpers + register their own def name,
-- or live in their own files with their own quirks. The shared
-- bearStanding / ensureBearState / advanceRestPhase helpers are
-- written to be name-agnostic so a panda_ai.lua could re-import
-- them.
--
-- Save/load: per-unit AI state (s.bearPosture / s.restPhase / etc.)
-- lives on the unit_ai aiState table, which already roundtrips via
-- the saveModules registration in unit_ai.init. Nothing extra here.

local unitAi = require("scripts.unit_ai")
local mv = require("scripts.movement_speed")

local bearAi = package.loaded["scripts.bear_ai"] or {}
package.loaded["scripts.bear_ai"] = bearAi

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function randRange(lo, hi)
    return lo + math.random() * (hi - lo)
end

-- A freshly-spawned bear has no s.bearPosture yet; the first time a
-- candidate's utility function runs it sees nil and we treat that as
-- "standing" so the bear can be picked normally. Execute then seeds
-- the field via ensureBearState on the first run.
local function bearStanding(s)
    return s.bearPosture == nil or s.bearPosture == "standing"
end

local function ensureBearState(s)
    s.bearPosture   = s.bearPosture   or "standing"
    s.bearAnchor    = s.bearAnchor    or nil
    s.activityUntil = s.activityUntil or 0
    s.lastRestAt    = s.lastRestAt    or 0
end

-----------------------------------------------------------
-- Candidate: bear_idle
-- Stand still, play idle anim. Cheapest baseline ambient action.
-----------------------------------------------------------

local function bearIdleUtility(uid, s, params)
    if not bearStanding(s) then return -math.huge end
    -- During the rest goal we never want to fall back to standing idle.
    if unitAi.isGoalActive(s, "rest") then return -math.huge end
    return 0.2
end

local function bearIdleExecute(uid, s, params)
    ensureBearState(s)
    unit.setAnim(uid, "idle")
    s.activityUntil = engine.gameTime()
        + randRange(params.idle_dur_min, params.idle_dur_max)
end

-----------------------------------------------------------
-- Candidate: bear_wander
-- Random walk inside wander_radius from anchor. Anchor drifts so
-- bears slowly roam instead of being rooted.
-----------------------------------------------------------

local function bearWanderUtility(uid, s, params)
    if not bearStanding(s) then return -math.huge end
    if unitAi.isGoalActive(s, "rest") then return -math.huge end
    -- Penalty while the previous wander leg is still in flight, so
    -- we don't repeatedly out-score idle and re-issue moveTo.
    if s.activityUntil and engine.gameTime() < s.activityUntil
       and s.currentAction == "bear_wander" then
        return 0.3
    end
    return 0.5
end

local function bearWanderExecute(uid, s, params)
    ensureBearState(s)
    if not s.bearAnchor then
        local px, py = unit.getPos(uid)
        s.bearAnchor = { x = px or 0, y = py or 0 }
    end
    local angle = math.random() * 2 * math.pi
    local r     = math.sqrt(math.random()) * params.wander_radius
    local tx = s.bearAnchor.x + math.cos(angle) * r
    local ty = s.bearAnchor.y + math.sin(angle) * r
    unit.moveTo(uid, tx, ty, mv.meander(uid))  -- aimless wander → slow meander
    s.activityUntil = engine.gameTime()
        + randRange(params.wander_dur_min, params.wander_dur_max)
    if math.random() < params.anchor_drift_chance then
        local px, py = unit.getPos(uid)
        if px then s.bearAnchor = { x = px, y = py } end
    end
end

-----------------------------------------------------------
-- Candidate: bear_alert
-- Stand on hind legs, look around. Mild posture variation; doesn't
-- preempt combat or rest.
-----------------------------------------------------------

local function bearAlertUtility(uid, s, params)
    if not bearStanding(s) and s.bearPosture ~= "stand_on_hind" then
        return -math.huge
    end
    if unitAi.isGoalActive(s, "rest") then return -math.huge end
    -- Stay in alert while the timer hasn't elapsed; otherwise rare.
    if s.bearPosture == "stand_on_hind"
       and engine.gameTime() < (s.activityUntil or 0) then
        return 0.6   -- keep current pose stable
    end
    return 0.15
end

local function bearAlertExecute(uid, s, params)
    ensureBearState(s)
    local now = engine.gameTime()
    if s.bearPosture == "stand_on_hind" then
        if now >= (s.activityUntil or 0) then
            -- Drop back to standing.
            unit.setAnim(uid, "idle")
            s.bearPosture  = "standing"
            s.activityUntil = now + 0.5
        end
        return
    end
    -- Enter alert: stand on hind, hold for alert_dur.
    unit.setAnim(uid, "stand_on_hind")
    s.bearPosture   = "stand_on_hind"
    s.activityUntil = now + params.transition_dur
        + randRange(params.alert_dur_min, params.alert_dur_max)
end

-----------------------------------------------------------
-- Candidate: bear_rest
-- Sit → lie → sleep sub-state machine. Persists across ticks via
-- activeGoal == "rest"; advances one phase per tick when the
-- per-phase timer elapses.
-----------------------------------------------------------

local function bearRestUtility(uid, s, params)
    if unitAi.isGoalActive(s, "rest") then return 2.0 end
    if unitAi.isGoalActive(s, "attack")
       or unitAi.isGoalActive(s, "retreat") then
        return -math.huge
    end
    -- Recovery: combat stole the rest goal mid-cycle (commandAttack's
    -- setGoal overwrites activeGoal). Once the fight ends, the bear is
    -- still sitting/lying/sleeping — every other ambient candidate
    -- requires standing, so without this branch no action can ever
    -- fire again and the bear freezes. Resume the rest machine; it
    -- stands the bear back up through the normal phase exits.
    if s.restPhase or not bearStanding(s) then return 2.0 end
    -- Time-since-last-rest pressure.
    local since = engine.gameTime() - (s.lastRestAt or 0)
    local pressure = since / params.rest_interval - 0.5
    return math.min(1.0, pressure)
end

local function advanceRestPhase(uid, s, params)
    local now = engine.gameTime()

    if not s.restPhase then
        unit.setAnim(uid, "standing_to_sitting")
        s.restPhase      = "to_sit"
        s.restPhaseUntil = now + params.transition_dur
        return
    end

    if now < (s.restPhaseUntil or 0) then return end

    if s.restPhase == "to_sit" then
        unit.setAnim(uid, "sitting_idle")
        s.bearPosture    = "sitting"
        s.restPhase      = "sit_idle"
        s.restPhaseUntil = now + randRange(params.sit_dur_min,
                                            params.sit_dur_max)
    elseif s.restPhase == "sit_idle" then
        if math.random() < 0.6 then
            unit.setAnim(uid, "sitting_to_lying_down")
            s.restPhase      = "to_lie"
            s.restPhaseUntil = now + params.transition_dur
        else
            unit.setAnim(uid, "idle")
            s.bearPosture    = "standing"
            s.restPhase      = nil
            s.restPhaseUntil = nil
            s.lastRestAt     = now
            unitAi.markGoalAccomplished(s, "rest")
        end
    elseif s.restPhase == "to_lie" then
        unit.setAnim(uid, "lying_down_idle")
        s.bearPosture    = "lying"
        s.restPhase      = "lie_idle"
        s.restPhaseUntil = now + randRange(params.lie_dur_min,
                                            params.lie_dur_max)
    elseif s.restPhase == "lie_idle" then
        if math.random() < 0.6 then
            unit.setAnim(uid, "lying_down_to_sleeping")
            s.restPhase      = "to_sleep"
            s.restPhaseUntil = now + params.transition_dur
        else
            unit.setAnim(uid, "sitting_idle")
            s.bearPosture    = "sitting"
            s.restPhase      = "sit_idle"
            s.restPhaseUntil = now + randRange(params.sit_dur_min,
                                                params.sit_dur_max)
        end
    elseif s.restPhase == "to_sleep" then
        unit.setAnim(uid, "sleeping_idle")
        s.bearPosture    = "sleeping"
        s.restPhase      = "sleep_idle"
        s.restPhaseUntil = now + randRange(params.sleep_dur_min,
                                            params.sleep_dur_max)
    elseif s.restPhase == "sleep_idle" then
        unit.setAnim(uid, "lying_down_idle")
        s.bearPosture    = "lying"
        s.restPhase      = "lie_idle"
        s.restPhaseUntil = now + randRange(5, 15)
    end
end

local function bearRestExecute(uid, s, params)
    ensureBearState(s)
    if not unitAi.isGoalActive(s, "rest") then
        unitAi.setGoal(s, "rest")
    end
    advanceRestPhase(uid, s, params)
end

-----------------------------------------------------------
-- Registration
-----------------------------------------------------------

unitAi.setConfig("bear_brown", {
    thought_interval = 1.0,
    thought_jitter   = 0.3,
    -- Combat re-evaluation cadence. See unit_ai.scheduleNext —
    -- charging units need sub-second range checks or they walk
    -- straight through their target.
    combat_thought_interval = 0.1,
    -- Wander geometry. Movement speeds come from the comfort/ordered/
    -- sprint regime (scripts/movement_speed.lua).
    wander_radius = 8.0,
    anchor_drift_chance = 0.25,
    -- Activity durations (seconds; randomised in [min, max]).
    idle_dur_min   = 3,  idle_dur_max   = 8,
    wander_dur_min = 5,  wander_dur_max = 14,
    alert_dur_min  = 3,  alert_dur_max  = 7,
    sit_dur_min    = 8,  sit_dur_max    = 20,
    lie_dur_min    = 15, lie_dur_max    = 35,
    sleep_dur_min  = 30, sleep_dur_max  = 90,
    -- One-shot transition durations; should track the animation
    -- length (bear posture anims run ~1.0s).
    transition_dur = 1.0,
    -- Rest-cycle pacing. Score grows over time-since-last-rest;
    -- ties wander at rest_interval, hits 1.0 at 2 × rest_interval.
    rest_interval = 120.0,
})

unitAi.registerActions("bear_brown", {
    -- universal combat (retreat / engage / attack_target) is
    -- auto-prepended by unitAi.registerActions; we only list the
    -- bear-specific ambient candidates here.
    { name = "bear_rest",   utility = bearRestUtility,
      execute = bearRestExecute,
      -- Rest is a multi-tick state machine; advance even while
      -- engine activity is "idle".
      forceExecute = true },
    { name = "bear_alert",  utility = bearAlertUtility,
      execute = bearAlertExecute,
      -- Alert holds a hind-stand pose; exit transition needs to
      -- fire when the timer elapses, not just on a switch.
      forceExecute = true },
    { name = "bear_wander", utility = bearWanderUtility,
      execute = bearWanderExecute },
    { name = "bear_idle",   utility = bearIdleUtility,
      execute = bearIdleExecute },
})

-----------------------------------------------------------
-- Shutdown shim
-----------------------------------------------------------

-- Kept so any old `require("scripts.bear_ai")` from before the
-- framework refactor still gets a module table with a no-op
-- shutdown. Real lifecycle now lives in unit_ai.
function bearAi.shutdown() end

return bearAi
