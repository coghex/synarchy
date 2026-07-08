-- Failure meters for scripts/unit_resources.lua: delayed-death
-- pathways. A physiological system fills a 0→1 meter from its driving
-- injuries and KILLS at 1.0, reporting its cause — but it RECOVERS if
-- the injury is treated below threshold, so a dying unit has a
-- survival window. (Blood loss is the existing analog, via the blood
-- resource hitting 0.) First meter: HYPOXIA / suffocation.

local cardio   = require("scripts.cardio")
local injuries = require("scripts.injuries")
local thermo   = require("scripts.thermo")
local salts    = require("scripts.salts")
local alerts   = require("scripts.unit_resource_alerts")

local M = {}

-- Each meter: a 0→1 physiological bar driven by an injury function
-- (injuries.lua), filling toward death over `fillSec` game-seconds at full
-- failure and recovering at `recover`/s once the driving injury is treated
-- below `deadband`. Killing reports the cause (refined from the actual
-- wounds, falling back to the meter's own label). Death is delayed, so a
-- catastrophically-wounded unit has a survival window for sci-fi treatment.
--   stat     — the unit stat the meter lives in (saved with the unit)
--   driver   — injuries.lua fn (uid → 0..1 failure fraction)
--   fillSec  — game-seconds at full failure to reach death
--   deadband — failure below this still self-compensates → no fill
--   recover  — meter drained per second when below deadband
--   cause    — fallback death-cause label (woundless / systemic)
local FAILURE_METERS = {
    -- HYPOXIA: low blood oxygen → suffocation. Now driven by cardio (blood_oxygen
    -- = lungs × perfusion), MAX'd with the direct lung path — so the combat
    -- lung-damage death is preserved AND circulatory hypoxia (massive blood
    -- loss, severe cold-bradycardia) can suffocate a unit too.
    { stat = "hypoxia", driver = cardio.hypoxiaFailure,
      fillSec = 180, deadband = 0.5,  recover = 0.08, cause = "suffocation" },
    -- NEURO: catastrophic brain trauma → nervous-system shutdown (fast).
    { stat = "neuro",   driver = injuries.neuroFailure,
      fillSec = 45,  deadband = 0.55, recover = 0.05, cause = "brain death" },
    -- SHOCK: massive aggregate acute trauma → cardiac arrest from shock.
    { stat = "shock",   driver = injuries.shockFailure,
      fillSec = 90,  deadband = 0.5,  recover = 0.06, cause = "shock" },
    -- ORGAN: untreated visceral trauma festering over "hours" (sepsis /
    -- hepatic encephalopathy / renal failure) — the slow walk-away death.
    { stat = "organ",   driver = injuries.organFailure,
      fillSec = 600, deadband = 0.45, recover = 0.04, cause = "organ failure" },
    -- SEPSIS: untreated/dirty open wounds get infected (woundInfection,
    -- grown in Combat.Wounds) and the infection spreads systemically — the
    -- slow walk-away death the medical kit's antiseptic (prevention) and
    -- antibiotics (cure) exist to stop. Slow fill (a survival window).
    { stat = "sepsis",  driver = injuries.sepsisFailure,
      fillSec = 480, deadband = 0.45, recover = 0.05, cause = "sepsis" },
    -- HYPOTHERMIA: core body temperature dropped into the danger zone (the
    -- arctic-acolyte death). core_temp is advanced by thermo.tick each tick;
    -- this meter integrates the time spent freezing, so a unit can be rescued
    -- by warming it before the meter tops out.
    { stat = "hypothermia",  driver = thermo.hypothermiaFailure,
      fillSec = 90, deadband = 0.0, recover = 0.08, cause = "hypothermia" },
    -- HYPERTHERMIA: core body temperature climbed into the danger zone (the
    -- desert-acolyte death — heat stroke). Same survival-window shape.
    { stat = "hyperthermia", driver = thermo.hyperthermiaFailure,
      fillSec = 60, deadband = 0.0, recover = 0.10, cause = "heat stroke" },
    -- SALT IMBALANCE: sodium concentration far from the ideal — hyponatremia
    -- (sweated out + drank only water) or hypernatremia (dehydrated/over-salted)
    -- → seizure/collapse. Eating (salt) / rebalancing water pulls it back.
    { stat = "salt_imbalance", driver = salts.imbalanceFailure,
      fillSec = 120, deadband = 0.0, recover = 0.08, cause = "electrolyte imbalance" },
}

-- Returns true if a meter killed the unit this tick. Meters tick in order;
-- the first to top out kills, with its (wound-refined) cause.
function M.tickFailureMeters(uid, dt)
    for _, m in ipairs(FAILURE_METERS) do
        local v    = unit.getStat(uid, m.stat) or 0
        local fail = m.driver(uid) or 0
        local deficit = (fail <= m.deadband) and 0
                        or (fail - m.deadband) / (1 - m.deadband)
        if deficit > 0 then
            v = v + deficit * (1 / m.fillSec) * dt
        else
            v = v - m.recover * dt                 -- compensates once treated
        end
        v = math.max(0, math.min(1, v))
        unit.setStat(uid, m.stat, v)
        if v >= 1.0 then
            alerts.emitDeathAlert(uid, injuries.deathCause(uid) or m.cause, "injury")
            unit.kill(uid)
            return true
        end
    end
    return false
end

-- Per-meter readout for the info panel: current 0..1 value, signed
-- per-(game)second rate (+ filling toward death, − recovering once the
-- driving injury is treated), the driving failure fraction, and the death
-- cause label. Mirrors the tickFailureMeters math exactly so the tooltip
-- numbers match what the meter is actually doing.
function M.meterInfo(uid)
    local out = {}
    for _, m in ipairs(FAILURE_METERS) do
        local v       = unit.getStat(uid, m.stat) or 0
        local fail    = m.driver(uid) or 0
        local deficit = (fail <= m.deadband) and 0
                        or (fail - m.deadband) / (1 - m.deadband)
        local rate
        if deficit > 0 then rate = deficit * (1 / m.fillSec)   -- filling
        else                rate = -m.recover end               -- recovering
        out[m.stat] = { value = v, rate = rate, fail = fail, cause = m.cause }
    end
    return out
end

return M
