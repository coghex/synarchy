-- Cardiovascular: heart rate + blood oxygen.
--
-- The fifth homeostasis layer. Heart rate RESPONDS to stress and DRIVES
-- perfusion; blood oxygen is what perfusion + the lungs actually deliver, and
-- it generalises the existing hypoxia (suffocation) meter beyond lung wounds.
--
--   heart_rate (bpm) drifts toward a target set by: activity, heat/fever
--     (tachycardia), blood loss (compensatory tachycardia), low O2
--     (compensatory), and cold (BRADYcardia — a cold heart slows). It feeds
--     circulation (circulation.lua reads it: bradycardia → poor perfusion).
--   blood_oxygen (0..1) drifts toward lungCapacity × delivery, where delivery
--     = circulation × heart-rate perfusion. Lung damage, massive blood loss, or
--     severe cold-bradycardia all pull it down → hypoxia → suffocation death.
--
-- This is what makes severe hypothermia *also* a respiratory death (the heart
-- slows → poor delivery → hypoxia), and what makes the hypoxia meter fire from
-- circulatory collapse, not just a punctured lung.
--
-- MODES (#2836, S9). Cardio is a registry system (data/physiology/systems.yaml)
-- that owns heart_rate and blood_oxygen. Only a SIMULATED cardio ticks
-- (scripts/unit_resources.lua gates it, ahead of every other system): a
-- Nominal one reads the resting heart rate and a fully oxygenated blood, an
-- Unsupported one (no body) has neither. Circulation is no longer stored or
-- lagged: it is derived on demand by the shared Haskell rule, so the oxygen
-- delivery below reads it with THIS tick's heart rate.
--
-- Everything here is in EFFECTIVE values. The integrators write the stored
-- BASE that makes the effective value land on the result under the key's
-- retained modifiers (scripts/lib/stat_base.lua), the same rule the pool
-- engine and the facade's contributions use.

local injuries = require("scripts.injuries")
local physiology = require("scripts.physiology")
local statBase = require("scripts.lib.stat_base")

local cardio = {}

-- ---- Tunables ----
-- The registry's RESTING_HR constant (data/physiology/systems.yaml), which
-- heart_rate's healthy value and circulation's heart-rate factor divide by;
-- Test.Headless.Unit.Physiology fails if this copy drifts.
cardio.RESTING_HR = 70
local MIN_HR = 30 ; local MAX_HR = 200
local WALK_HR = 25 ; local RUN_HR = 55          -- activity bumps
local HEAT_HR_K     = 12     -- +bpm per °C of core above 38
local BLOODLOSS_HR_K = 70    -- +bpm × (1 − bloodFrac) (compensatory tachycardia)
local COLD_HR_K     = 7      -- −bpm per °C of core below 35 (bradycardia)
local HYPOXIA_HR_K  = 80     -- +bpm × (0.9 − o2) (compensatory)
local HR_DRIFT      = 0.30   -- the heart responds over a few seconds

local O2_DRIFT   = 0.15      -- blood oxygen equilibrates a bit slower
local O2_DELIVERY_FLOOR = 0.60  -- only severe perfusion collapse tanks O2
local O2_DANGER  = 0.70 ; local O2_FATAL = 0.40   -- hypoxia danger band

local MOVING_RUN = { running = true, sprinting = true }

local clamp = require("scripts.lib.numeric").clamp
local effects = require("scripts.unit_effects")

-- Store `target` as the EFFECTIVE value of an owned key: the base that lands
-- on it under the key's retained modifiers. Nothing is written when no such
-- base is representable (a multiplier that is not positive, a base below
-- zero), exactly as a transition's seed refuses.
local function writeEffective(uid, key, target)
    local base = statBase.baseFor(uid, key, target)
    if base ~= nil then effects.setStat(uid, key, base) end
end

-- Advance heart rate then blood oxygen one tick. Runs BEFORE every other
-- system's tick. The oxygen delivery reads the derived circulation after the
-- heart rate has been integrated, so it sees this tick's heart rate and
-- inputs (the old one-tick lag on the stored circulation is gone).
--
-- Only called for a Simulated cardio, whose owned keys the spawn / transition
-- seeding stores: physiology.get raises a missing one (an invariant
-- violation), never starting from the resting value.
function cardio.tick(uid, dt)
    local hr = physiology.get(uid, "heart_rate")
    local o2 = physiology.get(uid, "blood_oxygen")
    if hr == nil or o2 == nil then return end   -- Unsupported, or the unit is gone

    local core = physiology.get(uid, "core_temp") or 37.0
    local blood = unit.readBloodView(uid)
    local bloodFrac = blood and blood.fraction or 1.0

    -- Heart-rate target.
    local target = cardio.RESTING_HR
    local act = unit.getActivity(uid) or "idle"
    if MOVING_RUN[act] then target = target + RUN_HR
    elseif act == "walking" then target = target + WALK_HR end
    if core > 38 then target = target + (core - 38) * HEAT_HR_K end
    if core < 35 then target = target - (35 - core) * COLD_HR_K end   -- bradycardia
    if bloodFrac < 1 then target = target + (1 - bloodFrac) * BLOODLOSS_HR_K end
    if o2 < 0.9 then target = target + (0.9 - o2) * HYPOXIA_HR_K end
    target = clamp(target, MIN_HR, MAX_HR)

    hr = hr + (target - hr) * clamp(HR_DRIFT * dt, 0, 1)
    writeEffective(uid, "heart_rate", hr)
    -- What the heart rate actually reads now (a write that was not
    -- representable under a retained modifier left it where it was).
    hr = physiology.get(uid, "heart_rate") or hr

    -- Blood oxygen = lung capacity × delivery (perfusion). Lung damage tanks it
    -- directly; poor circulation / bradycardia only bite at the extremes
    -- (delivery floored so mild poor circ doesn't suffocate anyone).
    local lungCap = 1 - clamp(injuries.pulmonaryFailure(uid), 0, 1)
    local circ    = physiology.get(uid, "circulation")
    if circ == nil then return end
    local hrPerf  = clamp(hr / cardio.RESTING_HR, 0.4, 1.0)   -- brady → poor delivery
    local delivery = O2_DELIVERY_FLOOR
                   + (1 - O2_DELIVERY_FLOOR) * clamp(circ * hrPerf, 0, 1)
    local o2target = clamp(lungCap * delivery, 0, 1)
    o2 = o2 + (o2target - o2) * clamp(O2_DRIFT * dt, 0, 1)
    writeEffective(uid, "blood_oxygen", clamp(o2, 0, 1))
end

-- A Nominal cardio reads the resting rate and a fully oxygenated blood (the
-- registry's healthy values); an Unsupported one has neither, and every
-- consumer takes the same substitute rather than a nil.
function cardio.heartRate(uid) return physiology.get(uid, "heart_rate") or cardio.RESTING_HR end
function cardio.bloodOxygen(uid) return physiology.get(uid, "blood_oxygen") or 1.0 end

-- 0..1 hypoxia danger fraction (drives the existing hypoxia/suffocation meter).
-- MAX of the blood-oxygen path (NEW: circulatory hypoxia) and the direct lung
-- path (pulmonaryFailure, UNCHANGED) — so the combat lung-damage death is
-- preserved exactly and circulatory hypoxia is added on top. A Nominal or
-- Unsupported cardio reads blood oxygen 1.0, so the oxygen path is zero and the
-- lung path alone drives the meter: the hypoxia meter is injury physiology, not
-- a cardio effect, and a punctured lung still suffocates.
function cardio.hypoxiaFailure(uid)
    local o2 = cardio.bloodOxygen(uid)
    local o2fail = clamp((O2_DANGER - o2) / (O2_DANGER - O2_FATAL), 0, 1)
    return math.max(o2fail, clamp(injuries.pulmonaryFailure(uid), 0, 1))
end

return cardio
