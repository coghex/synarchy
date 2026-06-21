-- Shared movement-speed model.
--
-- One place for the comfort / ordered / sprint speeds so the two
-- consumers can't drift apart:
--   * unit_resources.lua — drains stamina as (speed/comfort)², making
--     comfort the stamina-neutral cruise.
--   * unit_ai.lua — maps the unit's regime (ambient / ordered / alarmed)
--     to a commanded speed passed to unit.moveTo.
--
-- Speeds are in tiles/sec. `max_speed` (unit YAML) is the reference top
-- speed at agility 1.0; actual sprint scales linearly with agility.

local M = {}

local injuries = require("scripts.injuries")

local function clamp(x, lo, hi)
    return math.max(lo, math.min(hi, x))
end

-- Top (sprint) speed: max_speed × agility. Agility 1.0 → max_speed,
-- 2.0 → double (a 20 km/h human, an exceptional 40 km/h one). Agility is
-- clamped defensively so absurd stat data can't produce absurd speeds.
-- A leg/foot fracture multiplies the whole speed band down (a limp) via
-- injuries.speedMultiplier, so comfort/ordered/meander — all derived
-- from sprint — slow together. A fully disabling break keeps the unit
-- collapsed entirely (unit_resources), so this only ever limps a unit
-- that's still on its feet.
function M.sprint(uid)
    local maxsp = unit.getMaxSpeed(uid) or 0
    local agi   = unit.getStat(uid, "agility") or 1.0
    return maxsp * clamp(agi, 0.3, 3.0) * injuries.speedMultiplier(uid)
end

-- Comfort (stamina-neutral cruise): a fraction of sprint set by endurance,
-- so a fit unit cruises closer to its top speed. Always strictly below
-- sprint, so a unit can never "cruise faster than it can sprint".
function M.comfort(uid)
    local endur = unit.getStat(uid, "endurance") or 1.0
    local frac  = clamp(0.45 * math.sqrt(math.max(0, endur)), 0.2, 0.9)
    return M.sprint(uid) * frac
end

-- Ordered (player command / following): a slight, sustainable push above
-- comfort — a small stamina deficit the unit can hold for a while —
-- capped just under sprint.
function M.ordered(uid)
    return math.min(M.comfort(uid) * 1.15, M.sprint(uid) * 0.95)
end

-- Meander: the slow amble of a unit with NO goal (ambient wander). Well
-- below comfort, so the unit also recovers stamina while drifting. A low
-- fraction of max_speed — NOT agility-scaled, because ambling is leisurely
-- regardless of how fast the unit *could* run — and capped under half
-- comfort so even a low-comfort unit still meanders slower than it cruises.
-- This is what keeps animals from sprinting around with no purpose.
local MEANDER_FRAC = 0.25
function M.meander(uid)
    local maxsp = unit.getMaxSpeed(uid) or 0
    return math.min(maxsp * MEANDER_FRAC, M.comfort(uid) * 0.5)
end

return M
