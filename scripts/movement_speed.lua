-- Shared movement-speed model.
--
-- One place for the comfort / ordered / sprint speeds so the two
-- consumers can't drift apart:
--   * unit_resources.lua — drains stamina as (speed/comfort)², making
--     comfort the stamina-neutral cruise.
--   * the unit_ai_*.lua action modules — map the unit's regime
--     (ambient / ordered / alarmed) to a commanded speed passed to
--     unit.moveTo.
--
-- Speeds are in tiles/sec. `max_speed` (unit YAML) is the reference top
-- speed at agility 1.0; actual sprint scales linearly with agility.

local M = {}

local injuries   = require("scripts.injuries")
local salts      = require("scripts.salts")
local exhaustion = require("scripts.exhaustion")
local starvation = require("scripts.starvation")

local clamp = require("scripts.lib.numeric").clamp

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

-- Every WHOLE-BAND modifier, as one product (#1948). These are the
-- physiological / load conditions that slow a unit no matter which gait
-- it is using, so they belong to the band itself rather than to any one
-- speed:
--   * a limp from a leg/foot fracture (injuries.speedMultiplier). A
--     fully disabling break keeps the unit collapsed entirely
--     (unit_resources), so this only ever limps a unit still on its feet.
--   * salt cramps / hyponatremia (salts.speedMultiplier).
--   * near-empty exhaustion (circadian epic #479 / #610).
--   * a low calorie-store fraction — the hungry band (#806).
--   * carried load, eased by endurance (M.encumbranceMultiplier).
-- Every one of them returns a neutral 1.0 when the unit has no such
-- state (no wound, no live salt/exhaustion/calorie pool, no carry stat),
-- so a def without those resources gets exactly its unmodified band.
--
-- Kept as ONE named function so every gait multiplies by the SAME set:
-- sprint, and the derived comfort / ordered, take it through M.sprint,
-- and meander's raw max_speed branch takes it directly. That is what
-- makes it impossible for a modifier to reach some gaits and not others.
function M.bandMultiplier(uid)
    return injuries.speedMultiplier(uid) * salts.speedMultiplier(uid)
         * exhaustion.speedMultiplier(uid)
         * starvation.speedMultiplier(uid)
         * M.encumbranceMultiplier(uid)
end

-- Top (sprint) speed: max_speed × agility. Agility 1.0 → max_speed,
-- 2.0 → double (a 20 km/h human, an exceptional 40 km/h one). Agility is
-- clamped defensively so absurd stat data can't produce absurd speeds.
-- The whole-band modifiers above scale it down, so comfort/ordered —
-- both derived from sprint — slow together with it.
function M.sprint(uid)
    local maxsp = unit.getMaxSpeed(uid) or 0
    local agi   = unit.getStat(uid, "agility") or 1.0
    return maxsp * clamp(agi, 0.3, 3.0) * M.bandMultiplier(uid)
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

-- Uphill exertion (#375). The engine reports the signed slope grade the
-- unit is walking (getInfo.moveGrade: 1.0 = straight up a ramp's fall
-- line, negative = downhill). Climbing multiplies the EFFECTIVE EFFORT
-- the speed-drain model (unit_resource_tick.lua) reads in place of raw
-- speed: effort = speed × (1 + K·grade), so holding a commanded pace up
-- a full grade burns like moving (1+K)× faster. Shared here, not
-- private to the drain tick, because a speed SELECTOR needs the same
-- formula in reverse (#999's adaptive-pacing recovery pace): a fixed
-- fraction of comfort as a raw speed only actually undercuts comfort on
-- flat ground — on a graded slope its effective effort can exceed
-- comfort again, so recover mode would keep draining instead of
-- recovering on a sustained climb. Downhill/flat leave both directions
-- of the formula untouched (grade ≤ 0 clamps to 0).
local UPHILL_EXERTION_PER_GRADE = 0.5
M.UPHILL_EXERTION_PER_GRADE = UPHILL_EXERTION_PER_GRADE

-- The raw speed whose EFFECTIVE effort (per the formula above) equals
-- `targetEffort` on the given grade — the inverse of the drain tick's
-- effort calculation. At grade 0 this is just `targetEffort` itself.
function M.speedForEffort(targetEffort, grade)
    local g = math.max(0, grade or 0)
    return targetEffort / (1 + UPHILL_EXERTION_PER_GRADE * g)
end

-- Meander: the slow amble of a unit with NO goal (ambient wander). Well
-- below comfort, so the unit also recovers stamina while drifting. A low
-- fraction of max_speed — NOT agility-scaled, because ambling is leisurely
-- regardless of how fast the unit *could* run — and capped at half
-- comfort so even a low-comfort unit never meanders faster than half
-- its cruise.
-- This is what keeps animals from sprinting around with no purpose.
--
-- The whole-band modifiers DO slow the amble (a limping, cramping,
-- exhausted, hungry or loaded unit physically plods), so they multiply
-- the raw max_speed term too — not just the comfort term, which already
-- carries them via sprint. Applying them to only one branch hides them
-- behind whichever branch of the `min` wins: on an agile unit the raw
-- cap binds, so a modifier confined to the comfort term never reaches
-- the amble at all (#1948 — the bug #305 had already fixed for
-- encumbrance alone). Scaling BOTH terms by the same M.bandMultiplier
-- makes the choice of branch irrelevant: the amble always responds.
--
-- Note this cannot make a wandering unit burn stamina. The drain tick
-- reads speed/comfort, and that ratio is now INVARIANT under the band
-- modifiers (both terms carry the same factor, so it cancels) instead
-- of creeping up as the band shrank.
local MEANDER_FRAC = 0.25
function M.meander(uid)
    local maxsp = unit.getMaxSpeed(uid) or 0
    return math.min(maxsp * MEANDER_FRAC * M.bandMultiplier(uid),
                    M.comfort(uid) * 0.5)
end

return M
