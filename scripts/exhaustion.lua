-- Exhaustion (circadian epic #479 / #610).
--
-- Short-horizon physical fatigue, separate from stamina (per-action
-- exertion, regens fast) and from the sleep-pressure resource (#611, the
-- multi-day debt only real sleep clears). The resource itself ticks
-- generically through unit_resources.lua's config/tickResource machinery
-- (same shape as stamina/hydration — see the "exhaustion" config entries
-- there). This module holds the one passive near-empty effect this issue
-- adds: a movement-speed penalty, mirroring injuries.speedMultiplier /
-- salts.speedMultiplier exactly (composed into movement_speed.lua's
-- M.bandMultiplier alongside them, so it reaches EVERY gait — sprint,
-- the derived comfort/ordered, and the ambient meander — rather than
-- only the ones that happen to derive from sprint, #1948).
--
-- Exhaustion has two effects. The passive one is the movement-speed
-- penalty above, held here. The active one lives elsewhere:
-- scripts/unit_ai_sleep.lua's go_to_sleep utility (#612) inverts
-- M.fraction into a fatigue deficit and adds it at
-- sleep_exhaustion_weight, alongside the sleep_pressure deficit and the
-- live circadian urge. That weight is the smallest of the three, so a
-- fatigued unit still never collapses, and exhaustion on its own never
-- carries the utility over a standing order — it only nudges a unit
-- already leaning toward sleep.

local stats = require("scripts.unit_stats")

local M = {}

local clamp = require("scripts.lib.numeric").clamp

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
