-- Circulation — how well the unit perfuses blood to its tissues (0..1, 1 =
-- perfect). A DERIVED measure recomputed each tick from cardiovascular state,
-- not an integrated meter. It's the second layer of the homeostasis sim (after
-- core body temperature) and feeds a growing list of effects:
--   * thermo: cold vasoconstriction (poor circulation conserves core heat but
--     starves the extremities — the setup for frostbite).
--   * frostbite (next phase): poor circulation + cold → extremities freeze.
--   * (future) blood-oxygen delivery, healing rate, consciousness.
--
-- Factors (multiplicative): blood volume (shock), fitness (constitution + body
-- composition — the obese/frail circulate worse), sickness (sepsis → septic
-- shock), and cold vasoconstriction. "Old" has no stat yet → constitution
-- stands in for frailty; add an age input here when it exists.

local circulation = {}

-- ---- Tunables ----
local FAT_PENALTY_START = 0.20   -- body-fat fraction above this hurts circulation
local FAT_PENALTY_K     = 0.7
local VASO_START = 36.0          -- core temp below which vasoconstriction kicks in
local VASO_K     = 0.06          -- circulation lost per °C below VASO_START
local VASO_MIN   = 0.45          -- floor on the vasoconstriction factor
local SEPSIS_PENALTY = 0.5       -- septic shock: × (1 − this·sepsis)
local HR_FACTOR_MIN  = 0.5       -- floor on the bradycardia perfusion penalty
local CIRC_MIN   = 0.05

local clamp = require("scripts.lib.numeric").clamp

-- Compute circulation 0..1 for a unit, given its current core temperature
-- (passed in so this stays independent of the thermo module).
function circulation.compute(uid, coreTemp)
    -- Blood volume: low volume = poor perfusion (haemorrhagic shock).
    local b = unit.getBlood(uid)
    local bloodFrac = (b and b.max and b.max > 0) and (b.current / b.max) or 1.0
    local bloodFactor = clamp(bloodFrac * 1.1, 0.1, 1.0)

    -- Fitness: innate (constitution) × body composition (obesity hurts).
    local con   = unit.getStat(uid, "constitution") or 1.0
    local fatM  = unit.getStat(uid, "fat_mass") or 0.0
    local bodyM = unit.getStat(uid, "body_mass") or 70.0
    local fatFrac = (bodyM > 0) and (fatM / bodyM) or 0.2
    local fitness = clamp(
        (0.5 + 0.5 * con)
        * (1 - FAT_PENALTY_K * math.max(0, fatFrac - FAT_PENALTY_START)),
        0.3, 1.1)

    -- Sickness: systemic infection (sepsis) drives septic shock.
    local sepsis = unit.getStat(uid, "sepsis") or 0
    local sickness = clamp(1 - SEPSIS_PENALTY * sepsis, 0.4, 1.0)

    -- Cold vasoconstriction: the body shunts blood away from the skin/limbs to
    -- protect the core — which is exactly why extremities frostbite first.
    local core = coreTemp or 37.0
    local vaso = clamp(1 - VASO_K * math.max(0, VASO_START - core), VASO_MIN, 1.0)

    -- Heart rate (cardio.lua): bradycardia (a cold/dying heart) perfuses
    -- poorly; a resting+ heart is fine (no bonus for tachycardia).
    local hr = unit.getStat(uid, "heart_rate") or 70
    local hrFactor = clamp(hr / 70, HR_FACTOR_MIN, 1.0)

    return clamp(bloodFactor * fitness * sickness * vaso * hrFactor, CIRC_MIN, 1.0)
end

-- Recompute and store the unit's circulation stat from its EFFECTIVE core
-- temperature (#2831, D-5a): a Simulated thermo's stored core, a Nominal
-- one's healthy 37.0, 37.0 for an Unsupported one. Always on, whatever the
-- thermo mode: blood volume, fitness and sepsis are injury physiology, and
-- only the cold-vasoconstriction factor reads the core. Runs after cardio
-- and before the Simulated systems in every tick, and again at dt 0 after
-- any physiology transition (scripts/physiology.lua refresh).
function circulation.update(uid)
    local physiology = require("scripts.physiology")
    local core = physiology.get(uid, "core_temp") or 37.0
    local circ = circulation.compute(uid, core)
    unit.setStat(uid, "circulation", circ)
    return circ
end

-- Last-computed circulation for a unit (set as the stat "circulation" by
-- circulation.update each cycle). Defaults to 1.0 if never computed.
function circulation.get(uid)
    return unit.getStat(uid, "circulation") or 1.0
end

return circulation
