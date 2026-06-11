-- Offline regression harness for the bear/wolf rest-freeze fix.
-- Stubs the engine API surface that unit_ai/bear_ai touch, then
-- recreates the freeze scenario: a bear mid-rest (lying, restPhase
-- set) whose activeGoal was stolen by combat and then cleared.
-- PASS = the AI picks bear_rest and walks the state machine back to
-- standing. FAIL = no candidate fires (the pre-fix freeze).

package.path = "./?.lua;" .. package.path

local now = 1000.0
local units = { [1] = {
    defName = "bear_brown", gridX = 0, gridY = 0,
    pose = "lying", activity = "idle",
    stats = { stamina = 10, endurance = 1.0 },
} }

local lastAnim = nil

engine = {
    gameTime = function() return now end,
    realTime = function() return now end,
    isPaused = function() return false end,
    logInfo = function() end, logDebug = function() end,
    logWarn = function() end, logError = function() end,
}
unit = {
    getAllIds = function() return { 1 } end,
    getInfo = function(uid) return units[uid] end,
    exists = function(uid) return units[uid] ~= nil end,
    getPose = function(uid) return units[uid] and units[uid].pose end,
    getActivity = function(uid) return units[uid] and units[uid].activity end,
    getStat = function(uid, name) return units[uid] and units[uid].stats[name] end,
    getVisibleTiles = function() return nil end,
    getLastAttacker = function() return nil end,
    getMaxSpeed = function() return 6.0 end,
    getPos = function(uid) return units[uid].gridX, units[uid].gridY end,
    setAnim = function(_, name) lastAnim = name end,
    moveTo = function() end,
    stop = function() end,
}
world = { getFluidAt = function() return nil end }
building = { list = function() return "No buildings placed" end }
combat = {}

-- Replace require with a dofile-based loader: LuaJIT's native require
-- marks in-progress modules with a sentinel, which breaks the
-- package.loaded self-registration pattern these modules use for
-- circular deps. This loader matches the engine's semantics (dofile +
-- package.loaded cache).
function require(name)
    local cached = package.loaded[name]
    if type(cached) == "table" then return cached end
    local chunk = assert(loadfile(name:gsub("%.", "/") .. ".lua"))
    local ret = chunk()
    if package.loaded[name] == nil then package.loaded[name] = ret end
    return package.loaded[name]
end

local unitAi = require("scripts.unit_ai")

-- Recreate the post-combat freeze state: rest was in progress, combat
-- stole activeGoal (setGoal overwrote "rest"), the fight ended and
-- cleared it. restPhase + posture survive.
local s = {
    currentAction   = "attack_target",
    actionStartedAt = now,
    nextActionAt    = 0,
    goalStatus      = { rest = "in_progress", attack = "accomplished" },
    activeGoal      = nil,
    bearPosture     = "lying",
    restPhase       = "lie_idle",
    restPhaseUntil  = now - 1,   -- phase timer already elapsed
    lastRestAt      = 0,
    activityUntil   = 0,
}
unitAi.aiState[1] = s

-- Drive ticks. Each tick advances game time past nextActionAt and
-- past any restPhaseUntil the machine sets, so phases step through.
-- math.random < 0.6 branches can bounce lie→sit→lie, so give it a
-- generous budget; standing is an absorbing state for this test.
math.randomseed(12345)
local stood = false
for i = 1, 200 do
    now = now + 60.0
    s.restPhaseUntil = s.restPhaseUntil and math.min(s.restPhaseUntil, now - 1)
    unitAi.update(0.1)
    if s.bearPosture == "standing" and not s.restPhase then
        stood = true
        break
    end
end

if not stood then
    print("FAIL: bear never recovered (posture=" .. tostring(s.bearPosture)
        .. " restPhase=" .. tostring(s.restPhase)
        .. " currentAction=" .. tostring(s.currentAction) .. ")")
    os.exit(1)
end
assert(s.goalStatus.rest == "accomplished", "rest goal not closed out")
print("PASS: bear resumed rest machine and stood back up (anim=" ..
    tostring(lastAnim) .. ")")

-- Same scenario for the wolf.
units[1] = {
    defName = "gray_wolf", gridX = 0, gridY = 0,
    pose = "lying", activity = "idle",
    stats = { stamina = 10, endurance = 1.0 },
}
local sw = {
    currentAction   = "attack_target",
    actionStartedAt = now,
    nextActionAt    = 0,
    goalStatus      = { rest = "in_progress", attack = "accomplished" },
    activeGoal      = nil,
    wolfPosture     = "sleeping",
    restPhase       = "sleep_idle",
    restPhaseUntil  = now - 1,
    lastRestAt      = 0,
    activityUntil   = 0,
}
unitAi.aiState[1] = sw
local wolfStood = false
for i = 1, 200 do
    now = now + 60.0
    sw.restPhaseUntil = sw.restPhaseUntil and math.min(sw.restPhaseUntil, now - 1)
    unitAi.update(0.1)
    if sw.wolfPosture == "standing" and not sw.restPhase then
        wolfStood = true
        break
    end
end
if not wolfStood then
    print("FAIL: wolf never recovered (posture=" .. tostring(sw.wolfPosture)
        .. " restPhase=" .. tostring(sw.restPhase) .. ")")
    os.exit(1)
end
print("PASS: wolf resumed rest machine and stood back up")
