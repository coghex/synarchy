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
-- the first crossing of the unit's OWN time-of-day wake boundary since
-- falling asleep — whichever comes first. That boundary is derived
-- per species from the same circadian phase the sleep urge uses (#1945:
-- half a day past scripts/circadian.lua's circadian_center, so a
-- dusk-centered sleeper still wakes at dawn while the dawn-centered
-- bear wakes at dusk instead of on its own urge peak) — it is NOT the
-- universal dawn this module originally hard-coded. Interruption by
-- anything else (sound, an attack, another acolyte) is explicitly
-- deferred; the only additional hook is the public wake API below, so
-- other systems can force a wake.

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

-- Half a day past the sleeper's own circadian peak — the antipode of
-- its sleep-urge curve on world.getSunAngleAt's circular 0..1 domain
-- (0 = midnight, 0.25 = dawn, 0.5 = noon, 0.75 = dusk). This module used
-- to hard-code 0.25 for EVERY species; the dawn-centered bear_brown
-- (#613) therefore had its wake boundary sitting exactly on its own
-- sleep-urge peak, so it was driven to bed in the very window it was
-- woken in (#1945). Deriving the boundary from the phase keeps the
-- dusk-centered default at 0.25 exactly (0.75 + 0.5 = 1.25 -> 0.25) and
-- moves the bear's to 0.75.
--
-- Half a day is the derivation rather than the urge window's trailing
-- edge (center + circadian_width) because that edge would put the
-- acolyte's boundary at 0.875, changing shipped behavior, and rather
-- than a new per-def wake-angle field because every def would then have
-- to restate what its phase already says.
local WAKE_PHASE_OFFSET = 0.5

-- The unit's own time-of-day wake boundary. The center comes from
-- scripts/circadian.lua's shapeFor — the single source of truth for
-- per-def phase, including its DEFAULT_CENTER = 0.75 fallback for a def
-- that configures none — so this module deliberately keeps no default
-- of its own and an unconfigured def keeps the historical 0.25.
local function wakeAngleFor(defName)
    local center = circadian.shapeFor(defName)
    return (center + WAKE_PHASE_OFFSET) % 1.0
end

-- The unit's OWN longitude-local sun angle right now, with the info
-- record it came from (callers that need defName would otherwise fetch
-- it twice). The single reader of the sun in this module: the baseline
-- seeded when sleep begins and every later crossing sample are then the
-- same measurement of the same quantity by construction, taken at the
-- unit's own position through the same longitude-aware call. nil when
-- the unit has gone away or its column has no climate sample yet.
local function localSunAngleFor(uid)
    local info = unit.getInfo(uid)
    if not info then return nil, nil end
    local angle = world.getSunAngleAt(math.floor(info.gridX), math.floor(info.gridY))
    if not angle then return nil, nil end
    return angle, info
end

-- True on the AI tick that sees the sun sweep across this unit's own
-- wake boundary, since it started sleeping (s.sleepLastSunAngle is
-- seeded with the angle AT the moment the sleeping phase begins, so the
-- very first sleeping-phase check already has an earlier sample to
-- cross from — see sleepExecute). Written as containment in the
-- half-open forward arc (prev, angle] rather than `prev < B and angle >=
-- B`, so it stays correct on the circular domain: a boundary of 0.0, or
-- any crossing that spans the 1.0 -> 0.0 midnight wrap, is invisible to
-- a bare comparison. A unit that falls asleep already past its boundary
-- (forced by exhaustion at noon) starts from a baseline just PAST it, so
-- the forward arc back round to the boundary is nearly a whole day: it
-- only wakes at the NEXT crossing, not immediately.
local function wakeBoundaryCrossed(uid, s)
    local angle, info = localSunAngleFor(uid)
    if not angle then return false end
    local prev = s.sleepLastSunAngle
    s.sleepLastSunAngle = angle
    if prev == nil then return false end
    local boundary   = wakeAngleFor(info.defName)
    local travelled  = (angle - prev) % 1.0
    local toBoundary = (boundary - prev) % 1.0
    return toBoundary > 0 and toBoundary <= travelled
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
    return wakeBoundaryCrossed(uid, s)
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
            s.sleepPhase = "sleeping"
            -- Seed the wake-boundary baseline with the angle at the
            -- moment sleep actually begins (#1939). This used to store
            -- nil, which is the ABSENCE of a baseline rather than one:
            -- wakeBoundaryCrossed needs an earlier sample, so the first
            -- sleeping-phase check only recorded one and any boundary
            -- swept in the gap since this transition was lost — and
            -- unrecoverably, because every later sample that day then
            -- sits past the boundary. That gap is a real 0.5-1.5 s
            -- (thought_interval + thought_jitter, scheduled after the
            -- action runs), not two checks within one tick. nil survives
            -- only as the genuinely-unreadable case, which keeps the old
            -- wait-one-more-tick behaviour.
            s.sleepLastSunAngle = (localSunAngleFor(uid))
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
-- Exposed for tools/circadian_species_probe.py, which asserts the
-- per-species boundary (and the unconfigured-def default) through the
-- same lookup the sleeping unit itself uses.
M.wakeAngleFor = wakeAngleFor

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
