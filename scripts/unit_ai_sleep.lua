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

local core     = require("scripts.unit_ai_core")
local mv       = require("scripts.movement_speed")
local circadian = require("scripts.circadian")

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
    return params.sleep_base_weight
         + params.sleep_deficit_weight * deficit
         + params.sleep_urge_weight * urge
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
    -- unit_ai_water's search-session anchoring) and walk to it. "Any
    -- flat open tile" — no dedicated safety filtering (v1 decision);
    -- normal pathing already routes around impassable terrain.
    local info = unit.getInfo(uid)
    if not info then return end
    if not s.sleepSpot or s.sleepSession ~= s.actionStartedAt then
        local angle = math.random() * 2 * math.pi
        local dist  = math.random() * params.sleep_spot_radius
        s.sleepSpot    = { x = info.gridX + math.cos(angle) * dist,
                           y = info.gridY + math.sin(angle) * dist }
        s.sleepSession = s.actionStartedAt
    end

    local d = core.distance(info.gridX, info.gridY, s.sleepSpot.x, s.sleepSpot.y)
    if d <= params.sleep_spot_arrival_tiles then
        s.sleepPhase = "lying_down"
        unit.transitionTo(uid, "crouching", STRIDE_LIE_DOWN)
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
