-- Unit AI
--
-- Utility-AI based per-unit decision loop. Each unit type defines a
-- config table with a thought interval, jitter, and a list of actions.
-- An action is a {utility, execute} pair: `utility(uid, aiState)`
-- returns a number; the highest score wins and `execute` is called.
--
-- Decision cadence is per-unit: each unit holds a `nextActionAt`
-- timestamp set on every decision (interval + ± jitter). The module's
-- update(dt) tick iterates all units but only acts on those whose
-- nextActionAt has elapsed — this naturally distributes load across
-- ticks without a global "act every N ticks" gate.
--
-- Player commands flow through `unit_ai.commandMove(uid, tx, ty, speed)`
-- rather than `unit.moveTo` directly. The wrapper stores the task on
-- aiState so the follow_command action can evaluate it as a candidate
-- — letting higher-utility needs (later: thirst, hunger) interrupt and
-- the command resume once the need is satisfied.
--
-- Self-registers in package.loaded so engine.loadScript (which uses
-- dofile and creates a fresh chunk) and require return the same
-- instance — otherwise the engine would tick one module's aiState
-- while commandMove from init.lua writes to a different module's
-- aiState. Same pattern as scripts/debug.lua and unit_drag_select.lua.

local unitAi = package.loaded["scripts.unit_ai"] or {}
package.loaded["scripts.unit_ai"] = unitAi

-----------------------------------------------------------
-- Tunables per unit def
-----------------------------------------------------------

-- Wander utility formula:
--   base + stamina_weight * (stamina / max_stamina)
--                         - time_penalty * (now - actionStartedAt)
-- Clamped to -inf if stamina < min_wander_stamina_fraction * max.
--
-- follow_command utility is constant 1.0 when a task is pending;
-- placed deliberately above wander's ~0.8 fresh utility so player
-- commands win in the absence of dire needs.
local config = {
    acolyte = {
        thought_interval = 1.0,    -- seconds between decisions
        thought_jitter   = 0.5,    -- ± fraction of interval
        wander_radius    = 5.0,    -- tiles
        wander_speed     = 1.5,    -- tiles/sec for wander moves
        base_wander_utility          = 0.5,
        wander_stamina_weight        = 0.3,
        wander_time_penalty          = 0.1,    -- per second in session
        wander_min_stamina_fraction  = 0.2,
        command_speed                = 2.0,    -- default speed for player commands
    },
}

-----------------------------------------------------------
-- Per-unit AI state, keyed by uid. Hung off the module table so it
-- survives reloads via the package.loaded singleton above.
-----------------------------------------------------------
-- aiState[uid] = {
--   currentAction   = "idle" | "wander" | "follow_command",
--   actionStartedAt = <posix>,
--   nextActionAt    = <posix>,
--   commandedTask   = { x, y, speed, startedAt } | nil,
-- }
unitAi.aiState = unitAi.aiState or {}
local aiState = unitAi.aiState

-- Constants for arrival detection on commanded tasks.
local TASK_ARRIVAL_TILES = 0.6
local TASK_TIMEOUT_SEC   = 60.0

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function ensureState(uid)
    local s = aiState[uid]
    if not s then
        s = {
            currentAction   = "idle",
            actionStartedAt = os.time(),
            nextActionAt    = 0,      -- decide on first sight
            commandedTask   = nil,
        }
        aiState[uid] = s
    end
    return s
end

-- Schedule the next decision: interval + ±jitter, in seconds.
local function scheduleNext(s, params)
    local jitter = (math.random() * 2 - 1) * params.thought_jitter
    s.nextActionAt = os.time() + params.thought_interval * (1 + jitter)
end

local function distance(ax, ay, bx, by)
    local dx = ax - bx
    local dy = ay - by
    return math.sqrt(dx * dx + dy * dy)
end

-----------------------------------------------------------
-- Action: idle
-----------------------------------------------------------
local function idleUtility(uid, s, params)
    return 0
end

local function idleExecute(uid, s, params)
    -- Clear any leftover movement target.
    unit.stop(uid)
end

-----------------------------------------------------------
-- Action: wander
-----------------------------------------------------------
local function wanderUtility(uid, s, params)
    local stamina = unit.getStat(uid, "stamina")
    local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
    if not stamina or not maxStam or maxStam <= 0 then return -math.huge end

    local fraction = stamina / maxStam
    if fraction < params.wander_min_stamina_fraction then
        return -math.huge
    end

    local timeInSession = 0
    if s.currentAction == "wander" then
        timeInSession = os.time() - s.actionStartedAt
    end

    return params.base_wander_utility
         + params.wander_stamina_weight * fraction
         - params.wander_time_penalty * timeInSession
end

local function wanderExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    local angle = math.random() * 2 * math.pi
    local dist  = math.random() * params.wander_radius
    local tx = info.gridX + math.cos(angle) * dist
    local ty = info.gridY + math.sin(angle) * dist

    unit.moveTo(uid, tx, ty, params.wander_speed)
end

-----------------------------------------------------------
-- Action: follow_command
-----------------------------------------------------------
local function followCommandUtility(uid, s, params)
    if not s.commandedTask then return -math.huge end
    return 1.0
end

local function followCommandExecute(uid, s, params)
    local task = s.commandedTask
    if not task then return end
    unit.moveTo(uid, task.x, task.y, task.speed or params.command_speed)
end

-----------------------------------------------------------
-- Action registry per unit type
-----------------------------------------------------------
local actions = {
    acolyte = {
        { name = "idle",           utility = idleUtility,
          execute = idleExecute },
        { name = "wander",         utility = wanderUtility,
          execute = wanderExecute },
        { name = "follow_command", utility = followCommandUtility,
          execute = followCommandExecute },
    },
}

-----------------------------------------------------------
-- Task arrival / timeout housekeeping
-----------------------------------------------------------
local function maintainTask(uid, s)
    local task = s.commandedTask
    if not task then return end

    local info = unit.getInfo(uid)
    if not info then
        -- Unit gone; drop the task.
        s.commandedTask = nil
        return
    end

    -- Arrival check.
    if distance(info.gridX, info.gridY, task.x, task.y)
       <= TASK_ARRIVAL_TILES then
        s.commandedTask = nil
        return
    end

    -- Timeout check (handles unreachable destinations).
    if os.time() - task.startedAt > TASK_TIMEOUT_SEC then
        s.commandedTask = nil
    end
end

-----------------------------------------------------------
-- Decide + execute for one unit
-----------------------------------------------------------
local function tickOne(uid, defName)
    local params  = config[defName]
    local actList = actions[defName]
    if not params or not actList then return end

    -- Short-circuit for transient states: collapsed/reviving units
    -- can't act, and we don't want to clobber their anim by issuing
    -- new commands.
    local activity = unit.getActivity(uid)
    if activity == "collapsed" or activity == "reviving" then return end

    local s = ensureState(uid)
    maintainTask(uid, s)

    if os.time() < s.nextActionAt then return end

    -- Score every action; pick the highest. Ties → first in list.
    local bestAction, bestScore = nil, -math.huge
    for _, a in ipairs(actList) do
        local u = a.utility(uid, s, params)
        if u > bestScore then
            bestScore  = u
            bestAction = a
        end
    end

    if bestAction then
        local switching = bestAction.name ~= s.currentAction
        if switching then
            s.currentAction   = bestAction.name
            s.actionStartedAt = os.time()
        end
        -- Re-execute conditions:
        --   * On a switch: always (need to set up the new action).
        --   * On the same action: only if the unit is currently idle
        --     — meaning its previous walk arrived or failed. We do
        --     NOT want to re-issue moveTo while it's actively walking
        --     because that wipes `usLocalPath` engine-side and the
        --     unit barely makes progress between AI ticks.
        if switching or activity == "idle" then
            bestAction.execute(uid, s, params)
        end
    end

    scheduleNext(s, params)
end

-----------------------------------------------------------
-- Public: player-issued move command
-----------------------------------------------------------
-- Player code (e.g. right-click handler) calls this instead of
-- `unit.moveTo` so the AI can treat the command as a candidate
-- action and resume it after higher-priority interrupts.
function unitAi.commandMove(uid, tx, ty, speed)
    local s = ensureState(uid)
    s.commandedTask = {
        x         = tx,
        y         = ty,
        speed     = speed,
        startedAt = os.time(),
    }
    -- Force a fresh decision on the next tick rather than waiting
    -- for the unit's natural cadence — feels more responsive.
    s.nextActionAt = 0
end

-----------------------------------------------------------
-- Debug: expose state for the console
-----------------------------------------------------------
function unitAi.getState(uid)
    return aiState[uid]
end

-----------------------------------------------------------
-- Init / Update / Shutdown
-----------------------------------------------------------
function unitAi.init(scriptId)
    engine.logInfo("Unit AI initializing...")
end

function unitAi.update(dt)
    local ids = unit.getAllIds()
    if not ids or #ids == 0 then return end

    for _, uid in ipairs(ids) do
        local info = unit.getInfo(uid)
        if info and info.defName then
            tickOne(uid, info.defName)
        end
    end
end

function unitAi.shutdown()
    -- Empty the singleton state in-place so all references see it
    -- (reassigning the local would orphan the package.loaded copy).
    for k in pairs(aiState) do aiState[k] = nil end
    engine.logInfo("Unit AI shut down")
end

return unitAi
