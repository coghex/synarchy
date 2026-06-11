-- Gray wolf AI — species-specific candidates + config.
--
-- Owns wolf ambient behavior (idle / wander / howl / rest) and the
-- gray_wolf config block. Plugs into the unit_ai framework via
-- unitAi.registerActions / unitAi.setConfig. Combat candidates
-- (engage / attack_target / retreat) come from unit_ai itself —
-- registerActions auto-prepends them so we don't restate them here.
--
-- Differences from bear_ai: wolves have no stand_on_hind chain, so
-- the "alert" slot is a howl (the roar anim) instead; the rest
-- state machine is the same sit → lie → sleep chain (the wolf
-- yaml ships all three transitions). Wolves also roam wider and
-- pursue faster — see the config block.
--
-- Save/load: per-unit AI state (s.wolfPosture / s.restPhase / etc.)
-- lives on the unit_ai aiState table, which already roundtrips via
-- the saveModules registration in unit_ai.init. Nothing extra here.

local unitAi = require("scripts.unit_ai")

local wolfAi = package.loaded["scripts.gray_wolf_ai"] or {}
package.loaded["scripts.gray_wolf_ai"] = wolfAi

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function randRange(lo, hi)
    return lo + math.random() * (hi - lo)
end

-- A freshly-spawned wolf has no s.wolfPosture yet; the first time a
-- candidate's utility function runs it sees nil and we treat that as
-- "standing" so the wolf can be picked normally. Execute then seeds
-- the field via ensureWolfState on the first run.
local function wolfStanding(s)
    return s.wolfPosture == nil or s.wolfPosture == "standing"
end

local function ensureWolfState(s)
    s.wolfPosture   = s.wolfPosture   or "standing"
    s.wolfAnchor    = s.wolfAnchor    or nil
    s.activityUntil = s.activityUntil or 0
    s.lastRestAt    = s.lastRestAt    or 0
end

-----------------------------------------------------------
-- Candidate: wolf_idle
-- Stand still, play idle anim. Cheapest baseline ambient action.
-----------------------------------------------------------

local function wolfIdleUtility(uid, s, params)
    if not wolfStanding(s) then return -math.huge end
    -- During the rest goal we never want to fall back to standing idle.
    if unitAi.isGoalActive(s, "rest") then return -math.huge end
    return 0.2
end

local function wolfIdleExecute(uid, s, params)
    ensureWolfState(s)
    unit.setAnim(uid, "idle")
    s.activityUntil = engine.gameTime()
        + randRange(params.idle_dur_min, params.idle_dur_max)
end

-----------------------------------------------------------
-- Candidate: wolf_wander
-- Random walk inside wander_radius from anchor. Anchor drifts so
-- wolves roam instead of being rooted — drift is more aggressive
-- than the bear's, wolves cover ground.
-----------------------------------------------------------

local function wolfWanderUtility(uid, s, params)
    if not wolfStanding(s) then return -math.huge end
    if unitAi.isGoalActive(s, "rest") then return -math.huge end
    -- Penalty while the previous wander leg is still in flight, so
    -- we don't repeatedly out-score idle and re-issue moveTo.
    if s.activityUntil and engine.gameTime() < s.activityUntil
       and s.currentAction == "wolf_wander" then
        return 0.3
    end
    return 0.5
end

local function wolfWanderExecute(uid, s, params)
    ensureWolfState(s)
    if not s.wolfAnchor then
        local px, py = unit.getPos(uid)
        s.wolfAnchor = { x = px or 0, y = py or 0 }
    end
    local angle = math.random() * 2 * math.pi
    local r     = math.sqrt(math.random()) * params.wander_radius
    local tx = s.wolfAnchor.x + math.cos(angle) * r
    local ty = s.wolfAnchor.y + math.sin(angle) * r
    unit.moveTo(uid, tx, ty, params.wander_speed)
    s.activityUntil = engine.gameTime()
        + randRange(params.wander_dur_min, params.wander_dur_max)
    if math.random() < params.anchor_drift_chance then
        local px, py = unit.getPos(uid)
        if px then s.wolfAnchor = { x = px, y = py } end
    end
end

-----------------------------------------------------------
-- Candidate: wolf_howl
-- One-shot roar anim, then back to idle. Fills the bear_alert slot
-- (posture flavor) without a posture chain — the howl is just a
-- standing one-shot held for its anim length.
-----------------------------------------------------------

local function wolfHowlUtility(uid, s, params)
    if not wolfStanding(s) and s.wolfPosture ~= "howling" then
        return -math.huge
    end
    if unitAi.isGoalActive(s, "rest") then return -math.huge end
    -- Stay in the howl while the timer hasn't elapsed; otherwise rare.
    if s.wolfPosture == "howling"
       and engine.gameTime() < (s.activityUntil or 0) then
        return 0.6   -- keep the one-shot stable until it finishes
    end
    return 0.1
end

local function wolfHowlExecute(uid, s, params)
    ensureWolfState(s)
    local now = engine.gameTime()
    if s.wolfPosture == "howling" then
        if now >= (s.activityUntil or 0) then
            -- Howl finished, drop back to standing.
            unit.setAnim(uid, "idle")
            s.wolfPosture   = "standing"
            s.activityUntil = now + 0.5
        end
        return
    end
    -- Start the howl: roar is a 1.0s one-shot; hold a touch longer.
    unit.setAnim(uid, "roar")
    s.wolfPosture   = "howling"
    s.activityUntil = now + params.howl_dur
end

-----------------------------------------------------------
-- Candidate: wolf_rest
-- Sit → lie → sleep sub-state machine. Persists across ticks via
-- activeGoal == "rest"; advances one phase per tick when the
-- per-phase timer elapses. Same chain as the bear — the wolf yaml
-- ships all three transition anims.
-----------------------------------------------------------

local function wolfRestUtility(uid, s, params)
    if unitAi.isGoalActive(s, "rest") then return 2.0 end
    if unitAi.isGoalActive(s, "attack")
       or unitAi.isGoalActive(s, "retreat") then
        return -math.huge
    end
    -- Recovery: combat stole the rest goal mid-cycle (commandAttack's
    -- setGoal overwrites activeGoal). Once the fight ends, the wolf is
    -- still sitting/lying/sleeping — every other ambient candidate
    -- requires standing, so without this branch no action can ever
    -- fire again and the wolf freezes. Resume the rest machine; it
    -- stands the wolf back up through the normal phase exits.
    if s.restPhase or not wolfStanding(s) then return 2.0 end
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
        s.wolfPosture    = "sitting"
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
            s.wolfPosture    = "standing"
            s.restPhase      = nil
            s.restPhaseUntil = nil
            s.lastRestAt     = now
            unitAi.markGoalAccomplished(s, "rest")
        end
    elseif s.restPhase == "to_lie" then
        unit.setAnim(uid, "lying_down_idle")
        s.wolfPosture    = "lying"
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
            s.wolfPosture    = "sitting"
            s.restPhase      = "sit_idle"
            s.restPhaseUntil = now + randRange(params.sit_dur_min,
                                                params.sit_dur_max)
        end
    elseif s.restPhase == "to_sleep" then
        unit.setAnim(uid, "sleeping_idle")
        s.wolfPosture    = "sleeping"
        s.restPhase      = "sleep_idle"
        s.restPhaseUntil = now + randRange(params.sleep_dur_min,
                                            params.sleep_dur_max)
    elseif s.restPhase == "sleep_idle" then
        unit.setAnim(uid, "lying_down_idle")
        s.wolfPosture    = "lying"
        s.restPhase      = "lie_idle"
        s.restPhaseUntil = now + randRange(5, 15)
    end
end

local function wolfRestExecute(uid, s, params)
    ensureWolfState(s)
    if not unitAi.isGoalActive(s, "rest") then
        unitAi.setGoal(s, "rest")
    end
    advanceRestPhase(uid, s, params)
end

-----------------------------------------------------------
-- Registration
-----------------------------------------------------------

unitAi.setConfig("gray_wolf", {
    thought_interval = 1.0,
    thought_jitter   = 0.3,
    -- Combat re-evaluation cadence. See unit_ai.scheduleNext —
    -- charging units need sub-second range checks or they walk
    -- straight through their target.
    combat_thought_interval = 0.1,
    -- Wander geometry. Wider than the bear (8) — wolves range.
    wander_radius = 14.0,
    wander_speed  = 1.0,    -- LEGACY (still used by wolfWanderExecute)
    anchor_drift_chance = 0.4,
    -- Combat speed fractions. command_frac is also what attack
    -- pursuit uses; retreat is full sprint. Wolf max_speed is 6.5,
    -- so command 0.8 ≈ 5.2 tiles/sec — the chase is the weapon.
    speed_frac_wander  = 0.15,  -- ~1.0 tiles/sec at max_speed 6.5
    speed_frac_command = 0.8,
    speed_frac_retreat = 1.0,
    -- Activity durations (seconds; randomised in [min, max]).
    idle_dur_min   = 2,  idle_dur_max   = 6,
    wander_dur_min = 5,  wander_dur_max = 14,
    sit_dur_min    = 6,  sit_dur_max    = 15,
    lie_dur_min    = 12, lie_dur_max    = 30,
    sleep_dur_min  = 25, sleep_dur_max  = 80,
    -- Howl hold (roar anim runs 1.0s; hold a beat after).
    howl_dur = 1.5,
    -- One-shot transition durations; should track the animation
    -- length (wolf posture anims run ~1.0s, same as the bear's).
    transition_dur = 1.0,
    -- Rest-cycle pacing. Score grows over time-since-last-rest;
    -- ties wander at rest_interval, hits 1.0 at 2 × rest_interval.
    rest_interval = 150.0,
    -- LEGACY: kept for any straggler reading params.command_speed.
    -- New code uses speed_frac_* above × unit.getMaxSpeed.
    command_speed = 3.0,
})

unitAi.registerActions("gray_wolf", {
    -- universal combat (retreat / engage / attack_target) is
    -- auto-prepended by unitAi.registerActions; we only list the
    -- wolf-specific ambient candidates here.
    { name = "wolf_rest",   utility = wolfRestUtility,
      execute = wolfRestExecute,
      -- Rest is a multi-tick state machine; advance even while
      -- engine activity is "idle".
      forceExecute = true },
    { name = "wolf_howl",   utility = wolfHowlUtility,
      execute = wolfHowlExecute,
      -- Howl is a held one-shot; the exit transition needs to
      -- fire when the timer elapses, not just on a switch.
      forceExecute = true },
    { name = "wolf_wander", utility = wolfWanderUtility,
      execute = wolfWanderExecute },
    { name = "wolf_idle",   utility = wolfIdleUtility,
      execute = wolfIdleExecute },
})

return wolfAi
