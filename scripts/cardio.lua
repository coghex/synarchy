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
-- circulatory collapse, not just a punctured lung. All Lua; stats only.

local injuries = require("scripts.injuries")

local cardio = {}

-- ---- Tunables ----
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

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- Advance heart rate then blood oxygen one tick. Runs BEFORE thermo.tick so
-- circulation reads this tick's heart rate; O2 uses last tick's circulation
-- (a one-tick lag, invisible at these drift rates).
function cardio.tick(uid, dt)
    local core = unit.getStat(uid, "core_temp") or 37.0
    local b    = unit.getBlood(uid)
    local bloodFrac = (b and b.max and b.max > 0) and clamp(b.current / b.max, 0, 1) or 1.0
    local o2   = unit.getStat(uid, "blood_oxygen") or 1.0

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

    local hr = unit.getStat(uid, "heart_rate") or cardio.RESTING_HR
    hr = hr + (target - hr) * clamp(HR_DRIFT * dt, 0, 1)
    unit.setStat(uid, "heart_rate", hr)

    -- Blood oxygen = lung capacity × delivery (perfusion). Lung damage tanks it
    -- directly; poor circulation / bradycardia only bite at the extremes
    -- (delivery floored so mild poor circ doesn't suffocate anyone).
    local lungCap = 1 - clamp(injuries.pulmonaryFailure(uid), 0, 1)
    local circ    = unit.getStat(uid, "circulation") or 1.0
    local hrPerf  = clamp(hr / cardio.RESTING_HR, 0.4, 1.0)   -- brady → poor delivery
    local delivery = O2_DELIVERY_FLOOR
                   + (1 - O2_DELIVERY_FLOOR) * clamp(circ * hrPerf, 0, 1)
    local o2target = clamp(lungCap * delivery, 0, 1)
    o2 = o2 + (o2target - o2) * clamp(O2_DRIFT * dt, 0, 1)
    unit.setStat(uid, "blood_oxygen", clamp(o2, 0, 1))
end

function cardio.heartRate(uid) return unit.getStat(uid, "heart_rate") or cardio.RESTING_HR end
function cardio.bloodOxygen(uid) return unit.getStat(uid, "blood_oxygen") or 1.0 end

-- 0..1 hypoxia danger fraction (drives the existing hypoxia/suffocation meter).
-- MAX of the blood-oxygen path (NEW: circulatory hypoxia) and the direct lung
-- path (pulmonaryFailure, UNCHANGED) — so the combat lung-damage death is
-- preserved exactly and circulatory hypoxia is added on top.
function cardio.hypoxiaFailure(uid)
    local o2 = cardio.bloodOxygen(uid)
    local o2fail = clamp((O2_DANGER - o2) / (O2_DANGER - O2_FATAL), 0, 1)
    return math.max(o2fail, clamp(injuries.pulmonaryFailure(uid), 0, 1))
end

return cardio
