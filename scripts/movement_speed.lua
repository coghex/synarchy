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

local injuries   = require("scripts.injuries")
local salts      = require("scripts.salts")
local exhaustion = require("scripts.exhaustion")

local function clamp(x, lo, hi)
    return math.max(lo, math.min(hi, x))
end

-- Encumbrance speed band, keyed on the carried-weight / capacity ratio.
--   FREE_FRAC  — load up to this fraction of capacity is "free" (a unit
--                carrying a tool and a canteen isn't visibly slowed).
--   IN_CAP_K   — gentle penalty slope from FREE_FRAC up to full capacity.
--   OVER_K     — a much steeper slope once OVER capacity, so an overloaded
--                unit slows hard. The hard pickup gate still holds (a unit
--                only ends up over capacity from worn gear / a forced load),
--                so this just makes that state visibly costly to move in.
--   FLOOR      — never slower than this fraction of the base band, so a
--                pinned-down unit still inches along rather than freezing.
local ENC_FREE_FRAC = 0.25
local ENC_IN_CAP_K  = 0.40
local ENC_OVER_K    = 1.20
local ENC_FLOOR     = 0.20

-- Encumbrance multiplier on the whole speed band (mirrors
-- injuries.speedMultiplier / salts.speedMultiplier). A light load is ~1.0;
-- the penalty grows with the carried/capacity ratio and is EASED by the
-- endurance stat — a fit unit (or a pack animal like the technomule, with
-- both huge capacity and high endurance) shrugs off load that would crawl
-- a weak one. Returns 1.0 when capacity data is missing so callers stay
-- safe for defs without a carry stat.
function M.encumbranceMultiplier(uid)
    local cap = unit.getStat(uid, "carrying_capacity")
    if not cap or cap <= 0 then return 1.0 end
    local carried = unit.getCarryingWeight(uid) or 0
    local r = carried / cap
    -- Endurance 1.0 is nominal (acolyte); clamp so absurd stat data can't
    -- zero the penalty or blow it up. Higher endurance divides the penalty.
    local endur = clamp(unit.getStat(uid, "endurance") or 1.0, 0.3, 3.0)
    -- Penalty accrues only above the free allowance: a gentle term within
    -- capacity, plus a steep extra term once over it.
    local penalty = 0.0
    local inCap = math.min(r, 1.0) - ENC_FREE_FRAC
    if inCap > 0 then penalty = penalty + ENC_IN_CAP_K * inCap end
    if r > 1.0  then penalty = penalty + ENC_OVER_K * (r - 1.0) end
    return clamp(1.0 - penalty / endur, ENC_FLOOR, 1.0)
end

-- Top (sprint) speed: max_speed × agility. Agility 1.0 → max_speed,
-- 2.0 → double (a 20 km/h human, an exceptional 40 km/h one). Agility is
-- clamped defensively so absurd stat data can't produce absurd speeds.
-- A leg/foot fracture multiplies the whole speed band down (a limp) via
-- injuries.speedMultiplier, so comfort/ordered/meander — all derived
-- from sprint — slow together. A fully disabling break keeps the unit
-- collapsed entirely (unit_resources), so this only ever limps a unit
-- that's still on its feet. Carried load slows the band the same way via
-- encumbranceMultiplier — heavier = slower, eased by endurance.
function M.sprint(uid)
    local maxsp = unit.getMaxSpeed(uid) or 0
    local agi   = unit.getStat(uid, "agility") or 1.0
    -- Salt cramps (hyponatremia) and low exhaustion (circadian epic #479 /
    -- #610) both slow the whole speed band too, like a limp.
    return maxsp * clamp(agi, 0.3, 3.0)
         * injuries.speedMultiplier(uid) * salts.speedMultiplier(uid)
         * exhaustion.speedMultiplier(uid)
         * M.encumbranceMultiplier(uid)
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
--
-- Encumbrance DOES slow the amble (a loaded unit physically plods), so it
-- multiplies the raw max_speed cap too. The comfort term already carries
-- encumbrance via sprint, but on an agile unit the max_speed cap binds and
-- would otherwise hide load from wander until it grew heavy — so apply the
-- multiplier to both terms and the amble always responds to load.
local MEANDER_FRAC = 0.25
function M.meander(uid)
    local maxsp = unit.getMaxSpeed(uid) or 0
    return math.min(maxsp * MEANDER_FRAC * M.encumbranceMultiplier(uid),
                    M.comfort(uid) * 0.5)
end

return M
