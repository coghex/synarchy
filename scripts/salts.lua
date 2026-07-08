-- Salts / electrolytes (sodium balance).
--
-- The fourth homeostasis layer. Unlike hunger/thirst (fill-the-bar resources),
-- salt is a BALANCE: both too-low (hyponatremia) and too-high (hypernatremia)
-- are dangerous. The thing that matters is CONCENTRATION = salt ÷ body-water,
-- and body-water already exists (hydration) — so concentration is DERIVED, not
-- a second tracked compartment.
--
--   * Sweat drains salt (in thermo.tick) → depletion.
--   * Drinking plain water raises hydration, not salt → DILUTION (hyponatremia).
--   * Eating food adds salt (unit_ai_needs.lua eatExecute) → the player-controllable input.
--   * The kidneys EXCRETE excess salt (so hypernatremia self-corrects) but can't
--     CREATE it — so a sweated-out unit must EAT to recover. That asymmetry is
--     what makes food matter in the heat.
--
-- Effects this phase: cramps (movement penalty), impaired sweating (heat
-- tie-in), and a balance death meter. Confusion/fatigue is deferred to the
-- brain-effects phase, which will read salt_conc.

local salts = {}

-- ---- Tunables ----
-- max_salt scale is arbitrary (only the salt÷hydration RATIO matters). Tie it
-- to body mass so bigger bodies hold more.
local SALT_PER_KG     = 1.0
-- Baseline excretion as a FRACTION of max_salt per second (food replaces it).
-- Proportional, not absolute, so depletion TIME is mass-invariant — a mouse
-- and a dragon both run dry in the same game-hours. Calibrated to the old
-- absolute 0.004 salt/s at the human max_salt of ~70 (0.004/70).
local BASELINE_DRAIN_FRAC = 0.004 / 70
local KIDNEY_EXCRETE  = 0.05    -- excrete excess salt when concentration > 1.05
local MEAL_SALT_FRAC  = 0.30    -- a meal restores this fraction of max_salt

-- Concentration bands (1.0 = ideal). conc = saltFrac ÷ hydrationFrac.
local HYPO_START  = 0.75 ; local HYPO_FATAL  = 0.45   -- diluted / depleted
local HYPER_START = 1.35 ; local HYPER_FATAL = 1.75   -- dehydrated / over-salted

-- Cramps (from hyponatremia): movement multiplier ramps from 1.0 down.
local CRAMP_START   = 0.80
local CRAMP_FULL    = 0.50
local CRAMP_MIN_MULT = 0.40    -- worst-case movement speed ×

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

function salts.maxSalt(uid)
    return (unit.getStat(uid, "body_mass") or 70.0) * SALT_PER_KG
end

local function hydrationFrac(uid)
    local h  = unit.getStat(uid, "hydration")
    local hm = unit.getStat(uid, "max_hydration")
    if h and hm and hm > 0 then return clamp(h / hm, 0.05, 1.0) end
    return 1.0
end

local function concOf(uid, salt, mx)
    local saltFrac = (mx > 0) and (salt / mx) or 1.0
    return saltFrac / hydrationFrac(uid)
end

-- Advance salt one tick: baseline excretion + kidney excretion of excess.
-- Stores the salt pool ("salt") and the derived concentration ("salt_conc").
function salts.tick(uid, dt)
    local mx = salts.maxSalt(uid)
    local s  = unit.getStat(uid, "salt")
    if not s then s = mx end   -- first tick: spawn full
    s = s - BASELINE_DRAIN_FRAC * mx * dt
    -- Kidneys dump excess salt (hypernatremia self-corrects); they can't add
    -- salt, so hyponatremia only recovers by EATING.
    local conc = concOf(uid, s, mx)
    if conc > 1.05 then
        s = s - KIDNEY_EXCRETE * (conc - 1.05) * dt * mx
    end
    s = clamp(s, 0, mx)
    unit.setStat(uid, "salt", s)
    unit.setStat(uid, "salt_conc", concOf(uid, s, mx))
end

function salts.concentration(uid)
    return unit.getStat(uid, "salt_conc") or 1.0
end

-- Add a fraction of max_salt to the pool (the eat action calls this).
function salts.addSalt(uid, frac)
    local mx = salts.maxSalt(uid)
    local s  = unit.getStat(uid, "salt") or mx
    unit.setStat(uid, "salt", clamp(s + frac * mx, 0, mx))
end

function salts.mealSalt(uid) salts.addSalt(uid, MEAL_SALT_FRAC) end

-- Cramp movement multiplier (hyponatremia → muscle cramps slow the unit).
function salts.speedMultiplier(uid)
    local c = salts.concentration(uid)
    if c >= CRAMP_START then return 1.0 end
    local t = clamp((CRAMP_START - c) / (CRAMP_START - CRAMP_FULL), 0, 1)
    return 1.0 - (1.0 - CRAMP_MIN_MULT) * t
end

-- Sweating needs salt — depletion impairs it (so a salt-poor unit overheats
-- sooner). Only hyponatremia impairs sweat; thermo multiplies sweat by this.
function salts.sweatFactor(uid)
    local c = salts.concentration(uid)
    if c >= 0.80 then return 1.0 end
    return clamp(0.3 + 0.7 * (c / 0.80), 0.3, 1.0)
end

-- 0..1 imbalance danger fraction (drives the salt-imbalance failure meter),
-- worst of hypo- and hyper-natremia.
function salts.imbalanceFailure(uid)
    local c = salts.concentration(uid)
    local hypo  = clamp((HYPO_START - c) / (HYPO_START - HYPO_FATAL), 0, 1)
    local hyper = clamp((c - HYPER_START) / (HYPER_FATAL - HYPER_START), 0, 1)
    return math.max(hypo, hyper)
end

-- Short label for the Status panel.
function salts.stateLabel(uid)
    local c = salts.concentration(uid)
    if c <= HYPO_FATAL + 0.1  then return "Severe hyponatremia"
    elseif c < HYPO_START     then return "Low salt"
    elseif c > HYPER_FATAL - 0.1 then return "Severe hypernatremia"
    elseif c > HYPER_START    then return "High salt"
    else return "Balanced" end
end

return salts
