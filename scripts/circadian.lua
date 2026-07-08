-- Circadian urge (circadian rhythm epic #479 / #611).
--
-- A transient, time-of-day-driven pull toward sleep — derived LIVE from
-- world.getSunAngleAt(gx, gy) (the longitude-aware day/night phase,
-- #483) every call, never stored as a resource. Spikes for a few hours
-- around dusk, flat (0) at midnight and at noon. Distinct from sleep
-- pressure (scripts/unit_resource_config.lua's "sleep_pressure" entries
-- — the LONG, multi-day debt resource that only real sleep clears):
-- this is the SHORT, cyclic pull. Nothing consumes either signal yet —
-- the AI goal that combines them is #612's job.
--
-- world.getSunAngleAt's convention (World.Time.Types.worldTimeToSunAngle):
-- 0.0 = midnight, 0.25 = dawn, 0.5 = noon, 0.75 = dusk, wrapping back to
-- 1.0 = midnight.

local resourceConfig = require("scripts.unit_resource_config")

local M = {}

-- Defaults when a def has no explicit circadian_center/circadian_width
-- (or no sleep_pressure config at all): a 6-game-hour-wide bump (±3h)
-- centered on dusk. See unit_resource_config.lua's acolyte.sleep_pressure
-- entry for the full derivation — #613 overrides these per species by
-- editing that same per-def table.
local DEFAULT_CENTER = 0.75
local DEFAULT_WIDTH  = 0.125

-- Shortest distance between two points on the 0..1 sun-angle circle —
-- handles the midnight wraparound (e.g. angle 0.02 is close to 0.98,
-- not 0.96 apart).
local function circularDistance(a, b)
    local d = math.abs(a - b) % 1.0
    return math.min(d, 1.0 - d)
end

-- Raised-cosine bump: 1.0 at the center, tapering smoothly to exactly 0
-- at +/- width, and staying exactly 0 beyond it — the "exactly 0" (not
-- just small) is what guarantees flat at midnight/noon as long as they
-- fall outside the window, matching the issue's "flat (near-zero) at
-- midnight and at noon" requirement precisely rather than approximately.
local function bump(sunAngle, center, width)
    if not sunAngle or not width or width <= 0 then return 0.0 end
    local d = circularDistance(sunAngle, center)
    if d >= width then return 0.0 end
    return 0.5 * (1.0 + math.cos(math.pi * d / width))
end

M.bump = bump

-- Per-def bump shape, falling back to the module defaults when the def
-- has no sleep_pressure config (or no circadian_center/width fields in
-- it) — mirrors exhaustion.lua's "sensible default when data is
-- missing" convention.
local function shapeFor(defName)
    local cfg = resourceConfig[defName] and resourceConfig[defName].sleep_pressure
    local center = (cfg and cfg.circadian_center) or DEFAULT_CENTER
    local width  = (cfg and cfg.circadian_width)  or DEFAULT_WIDTH
    return center, width
end

M.shapeFor = shapeFor

-- unit.getCircadianUrge(uid) — 0..1, or nil if the unit doesn't exist or
-- has no resolvable position (issue #611 requirement: skip gracefully,
-- never error). Named to match the issue's own example call shape.
function M.getCircadianUrge(uid)
    local info = unit.getInfo(uid)
    if not info then return nil end
    local sunAngle = world.getSunAngleAt(math.floor(info.gridX), math.floor(info.gridY))
    if not sunAngle then return nil end
    local center, width = shapeFor(info.defName)
    return bump(sunAngle, center, width)
end

return M
