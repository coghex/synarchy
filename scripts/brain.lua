-- Brain / consciousness — the cognitive consequences of physiological
-- derangement. The sixth homeostasis layer.
--
-- A single derived CONSCIOUSNESS value (0..1, 1 = fully alert) = the WORST of
-- the body's derangements: core-temperature extremes (hypothermic confusion /
-- heat-stroke delirium), low blood oxygen (drowsy → out), and salt imbalance
-- (hypo/hypernatremic confusion). All three drivers are already stats
-- (core_temp, blood_oxygen, salt_conc), so this just reads + combines them.
--
-- Bands → behaviour:
--   ≥ 0.70  alert      — normal
--   ≥ 0.40  confused   — addled (Status condition; light)
--   ≥ 0.15  delirious  — can't act purposefully → stumbles/wanders (unit_ai)
--   <  0.15 unconscious → collapses (unit_resources), recovers at ≥ 0.40
--
-- This is what makes a unit PASS OUT before a temperature/hypoxia/salt death
-- meter finishes — the "cooks in the desert, passes out, then dies" arc.

local brain = {}

-- ---- Bands ----
local CONFUSED_BELOW    = 0.70
local DELIRIOUS_BELOW   = 0.40
local UNCONSCIOUS_BELOW = 0.15   -- collapse trigger
local RISE_AT           = 0.40   -- revive gate (hysteresis vs collapse)

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- Instantaneous consciousness from the physiological drivers (no integration —
-- the underlying stats already drift). min() = your worst problem knocks you out.
local function compute(uid)
    local core = unit.getStat(uid, "core_temp")   or 37.0
    local o2   = unit.getStat(uid, "blood_oxygen") or 1.0
    local conc = unit.getStat(uid, "salt_conc")    or 1.0

    -- Core temp: confusion when hypothermic (<34°C) or hyperthermic (>39.5°C).
    local tf
    if core < 34.0 then tf = clamp((core - 30.0) / 4.0, 0, 1)        -- 34→1, 30→0
    elseif core > 39.5 then tf = clamp((42.0 - core) / 2.5, 0, 1)    -- 39.5→1, 42→0
    else tf = 1.0 end

    -- Blood oxygen: <0.8 impairs, <0.5 unconscious.
    local of = clamp((o2 - 0.5) / 0.3, 0, 1)                         -- 0.8→1, 0.5→0

    -- Salt: deviation from ideal in either direction.
    local dev = math.abs(conc - 1.0)
    local sf  = clamp(1.0 - (dev - 0.25) / 0.35, 0, 1)               -- dev≤0.25→1, 0.6→0

    return math.min(tf, of, sf)
end

-- Recompute + store the consciousness stat (called each physiology tick).
function brain.tick(uid)
    unit.setStat(uid, "consciousness", compute(uid))
end

function brain.consciousness(uid)
    return unit.getStat(uid, "consciousness") or 1.0
end

function brain.isUnconscious(uid)  return brain.consciousness(uid) < UNCONSCIOUS_BELOW end
function brain.canRise(uid)        return brain.consciousness(uid) >= RISE_AT end
function brain.isDelirious(uid)
    local c = brain.consciousness(uid)
    return c < DELIRIOUS_BELOW and c >= UNCONSCIOUS_BELOW
end
function brain.isConfused(uid)
    local c = brain.consciousness(uid)
    return c < CONFUSED_BELOW and c >= DELIRIOUS_BELOW
end

function brain.state(uid)
    local c = brain.consciousness(uid)
    if c < UNCONSCIOUS_BELOW then return "unconscious"
    elseif c < DELIRIOUS_BELOW then return "delirious"
    elseif c < CONFUSED_BELOW then return "confused"
    else return "alert" end
end

return brain
