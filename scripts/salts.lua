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
--   * Eating food adds salt (unit_ai_needs.lua eatExecute) → the player-controllable
--     input. The meal's salt goes into a RESERVE (salt_store, #2761), the way a
--     meal's calories go into the stomach (#93), and the reserve tops the blood
--     pool back up toward its ideal level at a bounded rate between meals.
--   * The kidneys EXCRETE excess salt (so hypernatremia self-corrects) but can't
--     CREATE it — so a sweated-out unit must EAT to recover. That asymmetry is
--     what makes food matter in the heat.
--
-- Effects this phase: cramps (movement penalty), impaired sweating (heat
-- tie-in), and a balance death meter. Confusion/fatigue is deferred to the
-- brain-effects phase, which will read salt_conc.
--
-- Modes (#2831, S4). Only a SIMULATED salt system ticks and stores `salt` and
-- `salt_store`. A Nominal one stores nothing and reads healthy (full pool, full
-- reserve, concentration 1.0). `salt_conc` is never stored in any mode: it is
-- computed on demand from the pool and the body-water fraction
-- (physiology.get), by the same rule the kidneys use inside the tick below.

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
local MEAL_SALT_FRAC  = 0.30    -- a meal deposits this fraction of max_salt

-- Virtual meals (#2831, SD-4). A Nominal NUTRITION system never eats, so
-- nothing would ever refill a Simulated salt system's reserve, and the pool
-- would run dry in the heat. Once per salt tick, a unit in exactly that
-- combination is credited ONE meal through the real reserve path when its pool
-- has fallen below the fraction of max_salt it normally holds between meals
-- AND the reserve is nearly spent. The reserve clause only suppresses repeats:
-- baseline loss, kidney excretion and sweat continue unchanged.
local VIRTUAL_MEAL_POOL_FRAC    = 0.85  -- pool / max_salt below this ...
local VIRTUAL_MEAL_RESERVE_FRAC = 0.30  -- ... and reserve / max_salt below this

-- Salt reserve (#2761). A meal's salt is deposited here, not straight into the
-- pool (which cannot hold more than max_salt, so a meal could only top it up
-- and the gap to the next meal drained it by half). While the pool is below its
-- IDEAL level (concentration 1.0) the reserve refills it at a bounded rate.
-- Both constants are fractions of max_salt, so a mouse and a dragon behave
-- alike (as for BASELINE_DRAIN_FRAC).
--
-- Calibration, against the reference heat (39.6 C, humidity 0.26; the acolyte
-- in Test.Headless.Unit.SaltReserve, which is what the figures below were
-- measured on). Its core settles at 38.3-38.4 C, where thermo.lua sweats
-- 1.5 x (core - 37.5) x (1 - humidity) = 0.89-0.99 and drains SWEAT_SALT_K
-- (0.10) of salt per unit of sweat per second: 0.0013-0.0014 of a 70 kg
-- body's max_salt per second, 0.0014-0.0015 with the baseline drain.
-- Eating starts when the stomach falls below eat_max_fraction (0.25) of
-- max_hunger, which in that heat is every 500 engine seconds, and the
-- acolyte's two rations then deposit 2 x MEAL_SALT_FRAC = 0.60. A 500 s gap
-- costs 0.70-0.75, so one meal covers 80-85 % of the gap that follows it
-- and the reserve carries the remainder.
--   * STORE_CAP_FRAC must hold one gap's loss (0.75) PLUS a full meal's
--     deposit (0.60) without discarding any of it, i.e. at least 1.35, or
--     the first meals, which arrive while the reserve is still mostly full,
--     overflow and the salt is lost. 2.0 leaves a margin for hotter cores.
--     Over the 4320 engine seconds (3 game days) the acceptance run uses,
--     the reserve then bottoms out at ~0.67 of max_salt and the pool never
--     leaves its ideal level. A fresh unit starts with the reserve full.
--   * STORE_RELEASE_FRAC is a bounded rate, above the loss rate it must
--     keep up with (0.0015) so the pool tracks its ideal level while the
--     reserve lasts: 0.002 of max_salt per second.
-- Without food the full reserve lasts ~2000 s in that heat, against ~525 s
-- with no reserve, so a unit with water and no food still reaches
-- hyponatremia, only later.
local STORE_CAP_FRAC     = 2.0     -- reserve capacity, fraction of max_salt
local STORE_RELEASE_FRAC = 0.002   -- max release into the pool, max_salt / s

-- Concentration bands (1.0 = ideal). conc = saltFrac ÷ hydrationFrac.
local HYPO_START  = 0.75 ; local HYPO_FATAL  = 0.45   -- diluted / depleted
local HYPER_START = 1.35 ; local HYPER_FATAL = 1.75   -- dehydrated / over-salted

-- Cramps (from hyponatremia): movement multiplier ramps from 1.0 down.
local CRAMP_START   = 0.80
local CRAMP_FULL    = 0.50
local CRAMP_MIN_MULT = 0.40    -- worst-case movement speed ×

local clamp = require("scripts.lib.numeric").clamp
local physiology = require("scripts.physiology")
local effects = require("scripts.unit_effects")

function salts.maxSalt(uid)
    return (unit.getStat(uid, "body_mass") or 70.0) * SALT_PER_KG
end

local function hydrationFrac(uid)
    local f = physiology.fraction(uid, "hydration", "max_hydration")
    if f then return clamp(f, 0.05, 1.0) end
    return 1.0
end

local function concOf(uid, salt, mx)
    local saltFrac = (mx > 0) and (salt / mx) or 1.0
    return saltFrac / hydrationFrac(uid)
end

function salts.storeCap(uid)
    return salts.maxSalt(uid) * STORE_CAP_FRAC
end

-- Advance salt one tick: baseline excretion, release from the reserve, and
-- kidney excretion of excess. Stores the salt pool ("salt") and the reserve
-- ("salt_store"); the concentration is derived, never stored. Called only for
-- a Simulated salt system, whose keys are present by the seeding rule: a
-- missing one is an invariant violation physiology.get raises, not a cue to
-- start full.
function salts.tick(uid, dt)
    local mx = salts.maxSalt(uid)
    local s  = physiology.get(uid, "salt")
    if not s then return end                    -- Unsupported: nothing to tick
    local store = physiology.get(uid, "salt_store")
    s = s - BASELINE_DRAIN_FRAC * mx * dt
    -- The reserve refills the pool toward ideal concentration (1.0), never
    -- past it: no more than the reserve holds, the rate limit, or the deficit.
    -- A dehydrated unit's ideal pool is smaller (concentration is salt over
    -- water), so it releases nothing and cannot be pushed hypernatremic.
    local deficit = hydrationFrac(uid) * mx - s
    if deficit > 0 and store > 0 then
        local give = math.min(store, STORE_RELEASE_FRAC * mx * dt, deficit)
        s     = s + give
        store = store - give
    end
    -- Kidneys dump excess salt (hypernatremia self-corrects); they can't add
    -- salt, so hyponatremia only recovers by EATING.
    local conc = concOf(uid, s, mx)
    if conc > 1.05 then
        s = s - KIDNEY_EXCRETE * (conc - 1.05) * dt * mx
    end
    s = clamp(s, 0, mx)
    store = clamp(store, 0, salts.storeCap(uid))
    -- The virtual meal is decided on the values this tick just produced,
    -- before they are stored, and credited AFTER they are, so it lands on the
    -- reserve exactly as a real meal does (capped at storeCap) and releases
    -- from the NEXT tick.
    local virtualMeal = physiology.mode(uid, "nutrition") == "nominal"
        and s / mx < VIRTUAL_MEAL_POOL_FRAC
        and store < VIRTUAL_MEAL_RESERVE_FRAC * mx
    effects.setStat(uid, "salt", s)
    effects.setStat(uid, "salt_store", store)
    if virtualMeal then salts.mealSalt(uid) end
end

function salts.concentration(uid)
    return physiology.get(uid, "salt_conc") or 1.0
end

-- Deposit a fraction of max_salt into the reserve (the eat action calls this
-- once per item eaten). Capped at the reserve's capacity; the overflow is
-- discarded.
function salts.deposit(uid, frac)
    physiology.credit(uid, "salt_store", frac * salts.maxSalt(uid))
end

function salts.mealSalt(uid) salts.deposit(uid, MEAL_SALT_FRAC) end

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
