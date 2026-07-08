-- Exhaustion (circadian epic #479 / #610).
--
-- Short-horizon physical fatigue, separate from stamina (per-action
-- exertion, regens fast) and from the future sleep-pressure meter (#611,
-- a multi-day debt only real sleep clears). The resource itself ticks
-- generically through unit_resources.lua's config/tickResource machinery
-- (same shape as stamina/hydration — see the "exhaustion" config entries
-- there). This module holds the one passive near-empty effect this issue
-- adds: a movement-speed penalty, mirroring injuries.speedMultiplier /
-- salts.speedMultiplier exactly (composed into movement_speed.lua's
-- M.sprint alongside them).
--
-- No AI-goal wiring here — a fatigued unit slows down but doesn't collapse
-- or seek rest on its own. Feeding exhaustion into a "go to sleep" utility
-- is #612's job, once sleep pressure + circadian urge (#611) exist to
-- combine with it.

local stats = require("scripts.unit_stats")

local M = {}

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- Fraction 0..1. Defaults to 1.0 (fully rested) when the resource hasn't
-- been initialized yet (unit_resources seeds it to max on first tick) or
-- the unit type has no exhaustion config at all. max_exhaustion is a pure
-- Lua-derived stat (unit_stats.lua), never written into engine uiStats, so
-- it must be read via stats.get — raw unit.getStat would always miss it.
local function fraction(uid)
    local cur = unit.getStat(uid, "exhaustion")
    local mx  = stats.get(uid, "max_exhaustion")
    if cur and mx and mx > 0 then return clamp(cur / mx, 0, 1) end
    return 1.0
end

M.fraction = fraction

-- Movement multiplier ramps down as exhaustion nears empty, like salts'
-- cramp band. Full effect only below FATIGUE_START; floor at
-- FATIGUE_MIN_MULT so a fatigued unit still moves, just slowly.
local FATIGUE_START    = 0.35
local FATIGUE_MIN_MULT = 0.55

function M.speedMultiplier(uid)
    local f = fraction(uid)
    if f >= FATIGUE_START then return 1.0 end
    local t = clamp((FATIGUE_START - f) / FATIGUE_START, 0, 1)
    return 1.0 - (1.0 - FATIGUE_MIN_MULT) * t
end

return M
