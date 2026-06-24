-- Red squirrel AI — species-specific candidates + config.
--
-- Background-fluff prey animal. Much simpler than bear_ai: no rest
-- machine, no posture-driven combat stances. The squirrel wanders,
-- occasionally rears up to look around (alert), and — its defining
-- behaviour — flees from anything bigger than itself.
--
-- Combat candidates (engage / attack_target / retreat) are still
-- auto-prepended by unitAi.registerActions, so a squirrel that is
-- *commanded* to attack (unitAi.commandAttack — e.g. a scripted
-- "swarm the player" event) will fight. The flee candidate stands
-- down whenever an attack goal is active, so a command overrides the
-- natural skittishness.
--
-- Save/load: per-unit AI state (s.sqPosture / s.activityUntil / …)
-- lives on the unit_ai aiState table, which already roundtrips via
-- the saveModules registration in unit_ai.init. Nothing extra here.

local unitAi = require("scripts.unit_ai")
local mv = require("scripts.movement_speed")

local squirrelAi = package.loaded["scripts.red_squirrel_ai"] or {}
package.loaded["scripts.red_squirrel_ai"] = squirrelAi

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function randRange(lo, hi)
    return lo + math.random() * (hi - lo)
end

-- nil posture (fresh spawn) counts as standing so the squirrel can be
-- picked normally; ensureSqState seeds the field on first execute.
local function sqStanding(s)
    return s.sqPosture == nil or s.sqPosture == "standing"
end

local function ensureSqState(s)
    s.sqPosture     = s.sqPosture     or "standing"
    s.sqAnchor      = s.sqAnchor      or nil
    s.activityUntil = s.activityUntil or 0
end

-- Nearest unit (any species except another red squirrel) within
-- `radius` tiles, or nil. Squirrels flee acolytes, bears, the
-- technomule — anything that isn't one of them. Dead units don't
-- scare anyone. O(N) over the live unit set; fine at fluff counts.
local function nearestThreat(uid, radius)
    local px, py = unit.getPos(uid)
    if not px then return nil end
    local best, bestD2 = nil, radius * radius
    for _, oid in ipairs(unit.getAllIds()) do
        if oid ~= uid then
            local info = unit.getInfo(oid)
            if info and info.defName ~= "red_squirrel"
               and unit.getPose(oid) ~= "dead" then
                local ox, oy = unit.getPos(oid)
                if ox then
                    local d2 = (ox - px) ^ 2 + (oy - py) ^ 2
                    if d2 < bestD2 then best, bestD2 = oid, d2 end
                end
            end
        end
    end
    if best then return best, math.sqrt(bestD2) end
    return nil
end

-----------------------------------------------------------
-- Candidate: squirrel_flee
-- Sprint directly away from the nearest threat. Stands down while an
-- attack goal is active so a scripted swarm command takes precedence.
-----------------------------------------------------------

local function squirrelFleeUtility(uid, s, params)
    if unitAi.isGoalActive(s, "attack") then return -math.huge end
    local threat = nearestThreat(uid, params.flee_radius)
    if threat then
        s.fleeFrom = threat
        return 3.0   -- beats every ambient action; flight is priority one
    end
    s.fleeFrom = nil
    return -math.huge
end

local function squirrelFleeExecute(uid, s, params)
    ensureSqState(s)
    -- A flee breaks any posture; drop back to all fours to run.
    s.sqPosture = "standing"
    local threat = s.fleeFrom
    if not threat or not unit.exists(threat) then return end
    local px, py = unit.getPos(uid)
    local ox, oy = unit.getPos(threat)
    if not px or not ox then return end
    local dx, dy = px - ox, py - oy
    local len = math.sqrt(dx * dx + dy * dy)
    if len < 0.001 then
        -- Threat is right on top of us; pick a random escape heading.
        local a = math.random() * 2 * math.pi
        dx, dy = math.cos(a), math.sin(a)
        len = 1.0
    end
    local tx = px + (dx / len) * params.flee_distance
    local ty = py + (dy / len) * params.flee_distance
    unit.moveTo(uid, tx, ty, mv.sprint(uid))   -- panic → full sprint
end

-----------------------------------------------------------
-- Candidate: squirrel_alert
-- Rear up on hind legs and look around. Pure flavour; never fires
-- while a threat is near (flee owns that) and exits on a timer.
-----------------------------------------------------------

local function squirrelAlertUtility(uid, s, params)
    if not sqStanding(s) and s.sqPosture ~= "alert" then
        return -math.huge
    end
    if unitAi.isGoalActive(s, "attack") then return -math.huge end
    -- Hold the pose stable until its timer elapses.
    if s.sqPosture == "alert"
       and engine.gameTime() < (s.activityUntil or 0) then
        return 0.6
    end
    return 0.25
end

local function squirrelAlertExecute(uid, s, params)
    ensureSqState(s)
    local now = engine.gameTime()
    if s.sqPosture == "alert" then
        if now >= (s.activityUntil or 0) then
            unit.setAnim(uid, "idle")
            s.sqPosture     = "standing"
            s.activityUntil = now + 0.3
        end
        return
    end
    unit.setAnim(uid, "alert_idle")
    s.sqPosture     = "alert"
    s.activityUntil = now + randRange(params.alert_dur_min,
                                      params.alert_dur_max)
end

-----------------------------------------------------------
-- Candidate: squirrel_wander
-- Random walk inside wander_radius from a slowly-drifting anchor.
-----------------------------------------------------------

local function squirrelWanderUtility(uid, s, params)
    if not sqStanding(s) then return -math.huge end
    if unitAi.isGoalActive(s, "attack") then return -math.huge end
    if s.activityUntil and engine.gameTime() < s.activityUntil
       and s.currentAction == "squirrel_wander" then
        return 0.3
    end
    return 0.5
end

local function squirrelWanderExecute(uid, s, params)
    ensureSqState(s)
    if not s.sqAnchor then
        local px, py = unit.getPos(uid)
        s.sqAnchor = { x = px or 0, y = py or 0 }
    end
    local angle = math.random() * 2 * math.pi
    local r     = math.sqrt(math.random()) * params.wander_radius
    local tx = s.sqAnchor.x + math.cos(angle) * r
    local ty = s.sqAnchor.y + math.sin(angle) * r
    unit.moveTo(uid, tx, ty, mv.meander(uid))  -- aimless → slow meander
    s.activityUntil = engine.gameTime()
        + randRange(params.wander_dur_min, params.wander_dur_max)
    if math.random() < params.anchor_drift_chance then
        local px, py = unit.getPos(uid)
        if px then s.sqAnchor = { x = px, y = py } end
    end
end

-----------------------------------------------------------
-- Candidate: squirrel_idle
-- Cheapest baseline. Stand still, play idle.
-----------------------------------------------------------

local function squirrelIdleUtility(uid, s, params)
    if not sqStanding(s) then return -math.huge end
    if unitAi.isGoalActive(s, "attack") then return -math.huge end
    return 0.2
end

local function squirrelIdleExecute(uid, s, params)
    ensureSqState(s)
    unit.setAnim(uid, "idle")
    s.activityUntil = engine.gameTime()
        + randRange(params.idle_dur_min, params.idle_dur_max)
end

-----------------------------------------------------------
-- Registration
-----------------------------------------------------------

unitAi.setConfig("red_squirrel", {
    thought_interval = 0.6,
    thought_jitter   = 0.3,
    -- Flight needs tight re-evaluation so a sprinting squirrel keeps
    -- re-pathing away from a chasing threat.
    combat_thought_interval = 0.15,
    -- Wander geometry.
    wander_radius       = 6.0,
    anchor_drift_chance = 0.3,
    -- Flight: scare radius + how far to bolt per leg.
    flee_radius   = 6.0,
    flee_distance = 8.0,
    -- Activity durations (seconds; randomised in [min, max]).
    idle_dur_min   = 2, idle_dur_max   = 5,
    wander_dur_min = 3, wander_dur_max = 8,
    alert_dur_min  = 2, alert_dur_max  = 5,
    transition_dur = 0.5,
})

unitAi.registerActions("red_squirrel", {
    -- universal combat (retreat / engage / attack_target) is
    -- auto-prepended; we list only the squirrel-specific ambient
    -- candidates here.
    { name = "squirrel_flee",   utility = squirrelFleeUtility,
      execute = squirrelFleeExecute },
    { name = "squirrel_alert",  utility = squirrelAlertUtility,
      execute = squirrelAlertExecute,
      -- Holds a pose; exit transition must fire on timer elapse.
      forceExecute = true },
    { name = "squirrel_wander", utility = squirrelWanderUtility,
      execute = squirrelWanderExecute },
    { name = "squirrel_idle",   utility = squirrelIdleUtility,
      execute = squirrelIdleExecute },
})

-- Kept for parity with bear_ai's shutdown shim; real lifecycle lives
-- in unit_ai.
function squirrelAi.shutdown() end

return squirrelAi
