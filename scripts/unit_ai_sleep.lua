-- Unit AI sleep goal (circadian rhythm epic #479 / #611 / #612).
--
-- Combines the long multi-day sleep_pressure resource (#611) with the
-- short dusk-centered circadian urge (scripts/circadian.lua) into a
-- single "go to sleep" action. Walking to a spot is an ordinary,
-- interruptible candidate like wander; once the unit commits to lying
-- down it locks in (math.huge) through sleeping and waking, mirroring
-- unit_ai_water.lua's drink_from_source three-phase lock.
--
-- Lying down is a multi-hop pose chain (#612): the acolyte only has
-- art for standing<->crouching<->crawling (existing) and a new
-- crawling<->sleeping link, so falling asleep plays standing -> crouching
-- -> crawling -> sleeping one hop per AI tick (waking reverses it) —
-- there is no engine-side auto-chaining (see Unit.Thread.Command.Pose),
-- so this module drives every hop itself, exactly like drinkFromSource
-- drives its stand<->crouch<->crawl descent/ascent.
--
-- Wake conditions (v1, per design): sleep_pressure back near full, OR
-- the first dawn crossing since falling asleep — whichever comes
-- first. Interruption by anything else (sound, an attack, another
-- acolyte) is explicitly deferred; the only additional hook is the
-- public wake API below, so other systems can force a wake.

local core       = require("scripts.unit_ai_core")
local mv         = require("scripts.movement_speed")
local circadian  = require("scripts.circadian")
local exhaustion = require("scripts.exhaustion")

local unitAi = package.loaded["scripts.unit_ai"]

local M = {}

local STRIDE_LIE_DOWN = 2
local STRIDE_WAKE     = 2

-- Treat "close enough to full" as full — avoids sitting locked forever
-- chasing the last 2% while regen asymptotically approaches maxVal.
local WAKE_PRESSURE_FRAC = 0.98

local DAWN_ANGLE = 0.25

-- True on the AI tick that sees sunAngle cross DAWN_ANGLE from below,
-- since the unit started sleeping (s.sleepLastSunAngle is reset when
-- sleep begins). A unit that falls asleep already past dawn (forced by
-- exhaustion at noon) only wakes at the NEXT crossing, not immediately.
local function dawnHasArrived(uid, s)
    local info = unit.getInfo(uid)
    if not info then return false end
    local angle = world.getSunAngleAt(math.floor(info.gridX), math.floor(info.gridY))
    if not angle then return false end
    local prev = s.sleepLastSunAngle
    s.sleepLastSunAngle = angle
    return prev ~= nil and prev < DAWN_ANGLE and angle >= DAWN_ANGLE
end

-- 8 compass directions, matching unit_ai_water.lua's SEARCH_DIRECTIONS
-- rosette exactly (diagonals pre-normalised so every waypoint in a ring
-- sits at the same physical distance from the origin).
local SEARCH_DIRECTIONS = {
    {  1,        0,        },  -- E
    {  0.707107, 0.707107  },  -- SE
    {  0,        1         },  -- S
    { -0.707107, 0.707107  },  -- SW
    { -1,        0         },  -- W
    { -0.707107, -0.707107 },  -- NW
    {  0,        -1        },  -- N
    {  0.707107, -0.707107 },  -- NE
}

-- "Any flat open tile" (v1 design): flat (world.getSlopeAt == 0, per
-- CLAUDE.md's slope bitmask) and dry. Not the dedicated threat/hazard
-- safety filtering the v1 design explicitly deferred — just enough to
-- stop the AI from settling on a slope or the middle of a lake.
local function isValidSleepTile(gx, gy)
    return world.getSlopeAt(gx, gy) == 0 and not world.getFluidAt(gx, gy)
end

-- Rosette-style widening search for a sleep spot, geometrically the
-- same pattern as unit_ai_water.lua's search_for_water (8 compass
-- points per ring, rings expanding outward by spacing) — but unlike
-- water, flatness/fluid are directly queryable from wherever the unit
-- currently stands, so this SAMPLES candidate tiles instead of
-- physically walking+FOV-scanning to each one. The whole rosette is
-- rotated by a random angleOffset each call (mirrors
-- unit_ai_water.lua's per-session searchAngleOffset jitter) — without
-- it this is a pure function of (origin, radius, spacing), so a retry
-- after a failed/timed-out pick would deterministically re-find the
-- exact same dead candidate instead of exploring different ground.
-- Returns nil if no ring/direction combination is valid anywhere in
-- the search — the caller retries with a fresh rotation on its next
-- tick rather than settling for an invalid fallback.
local function pickSleepSpot(originX, originY, radius, spacing)
    local rings = math.max(1, math.floor(radius / spacing))
    local angleOffset = math.random() * 2 * math.pi
    local cosA, sinA  = math.cos(angleOffset), math.sin(angleOffset)
    for ring = 1, rings do
        for _, d in ipairs(SEARCH_DIRECTIONS) do
            local rx = d[1] * cosA - d[2] * sinA
            local ry = d[1] * sinA + d[2] * cosA
            local x  = originX + rx * ring * spacing
            local y  = originY + ry * ring * spacing
            if isValidSleepTile(math.floor(x), math.floor(y)) then
                return x, y
            end
        end
    end
    return nil
end

local function shouldWake(uid, s)
    if s.sleepWakeRequested then
        s.sleepWakeRequested = nil
        return true
    end
    local sp    = unit.getStat(uid, "sleep_pressure")
    local maxSp = require("scripts.unit_stats").get(uid, "max_sleep_pressure")
    if sp and maxSp and maxSp > 0 and sp / maxSp >= WAKE_PRESSURE_FRAC then
        return true
    end
    return dawnHasArrived(uid, s)
end

-----------------------------------------------------------
-- Action: go_to_sleep
-----------------------------------------------------------
local function sleepUtility(uid, s, params)
    -- Locked in once the lie-down/sleep/wake sequence has started —
    -- nothing may preempt it (matches drinkFromSourceUtility).
    if s.sleepPhase then return math.huge end

    local sp    = unit.getStat(uid, "sleep_pressure")
    local maxSp = require("scripts.unit_stats").get(uid, "max_sleep_pressure")
    if not sp or not maxSp or maxSp <= 0 then return -math.huge end

    local deficit = 1 - sp / maxSp
    if deficit < params.sleep_min_deficit then return -math.huge end

    local urge = circadian.getCircadianUrge(uid) or 0
    -- exhaustion.fraction is "restedness" (1=fresh, 0=fatigued) — invert
    -- to a deficit so it stacks with sleep_pressure's deficit the same
    -- way. Short-horizon (regens with ordinary rest, per exhaustion.lua),
    -- so it's a minor nudge on top of the sleep_pressure/urge baseline,
    -- not an independent trigger — exhaustion.lua's own header calls out
    -- feeding it into this utility as #612's job.
    local exhaustionDeficit = 1 - (exhaustion.fraction(uid) or 1.0)
    return params.sleep_base_weight
         + params.sleep_deficit_weight * deficit
         + params.sleep_urge_weight * urge
         + params.sleep_exhaustion_weight * exhaustionDeficit
end

local function sleepExecute(uid, s, params)
    local pose = unit.getPose(uid) or "standing"

    -- Lying down: one pose-step per AI tick (standing -> crouching ->
    -- crawling -> sleeping). Each transitionTo call is a no-op while
    -- the previous hop's animation is still playing (tickOne itself
    -- won't re-enter until activity returns to idle), so re-issuing
    -- it every tick is safe.
    if s.sleepPhase == "lying_down" then
        if     pose == "standing"  then unit.transitionTo(uid, "crouching", STRIDE_LIE_DOWN)
        elseif pose == "crouching" then unit.transitionTo(uid, "crawling", STRIDE_LIE_DOWN)
        elseif pose == "crawling"  then unit.transitionTo(uid, "sleeping", STRIDE_LIE_DOWN)
        elseif pose == "sleeping"  then
            s.sleepPhase         = "sleeping"
            s.sleepLastSunAngle  = nil   -- baseline for dawn-crossing detection
        end
        return
    end

    -- Sleeping: held pose, sleep_pressure regens (unit_resource_tick's
    -- regen_factor_sleeping). Check wake conditions every tick.
    if s.sleepPhase == "sleeping" then
        if shouldWake(uid, s) then
            s.sleepPhase = "waking"
            unit.transitionTo(uid, "crawling", STRIDE_WAKE)
        end
        return
    end

    -- Waking: reverse the lie-down chain.
    if s.sleepPhase == "waking" then
        if     pose == "sleeping"  then unit.transitionTo(uid, "crawling", STRIDE_WAKE)
        elseif pose == "crawling"  then unit.transitionTo(uid, "crouching", STRIDE_WAKE)
        elseif pose == "crouching" then unit.transitionTo(uid, "standing", STRIDE_WAKE)
        elseif pose == "standing"  then s.sleepPhase = nil
        end
        return
    end

    -- No phase yet: pick a nearby spot once per session (mirrors
    -- unit_ai_water's search-session anchoring) and walk to it.
    local info = unit.getInfo(uid)
    if not info then return end
    if not s.sleepSpot or s.sleepSession ~= s.actionStartedAt then
        local x, y = pickSleepSpot(info.gridX, info.gridY,
                                   params.sleep_spot_radius,
                                   params.sleep_spot_ring_spacing)
        if not x then return end  -- nothing valid this rotation; retry
                                   -- next tick with a fresh one
        s.sleepSpot          = { x = x, y = y }
        s.sleepSession       = s.actionStartedAt
        s.sleepSpotPickedAt  = engine.gameTime()
    end

    local d = core.distance(info.gridX, info.gridY, s.sleepSpot.x, s.sleepSpot.y)
    if d <= params.sleep_spot_arrival_tiles then
        s.sleepPhase = "lying_down"
        unit.transitionTo(uid, "crouching", STRIDE_LIE_DOWN)
    elseif engine.gameTime() - (s.sleepSpotPickedAt or engine.gameTime())
           > params.sleep_spot_max_wait then
        -- Unreachable (e.g. the stuck-walk watchdog gave up on it) —
        -- drop it so the next tick picks a different spot instead of
        -- retrying the same dead target forever.
        s.sleepSpot = nil
    else
        unit.moveTo(uid, s.sleepSpot.x, s.sleepSpot.y, mv.meander(uid))
    end
end

M.sleepUtility = sleepUtility
M.sleepExecute = sleepExecute

-- Public wake API (#612 v1 priority): force-wakes a sleeping unit on
-- its next AI tick. No-op if the unit isn't tracked or isn't actually
-- asleep (mid lying-down/waking already resolves on its own). This is
-- the ONLY interrupt hook in v1 — sound/threat/social wake triggers
-- are deferred; other systems that need to wake a unit call this.
function unitAi.wakeUnit(uid)
    local s = unitAi.getState(uid)
    if s and s.sleepPhase then
        s.sleepWakeRequested = true
    end
end

return M
