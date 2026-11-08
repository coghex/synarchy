-- Thermoregulation / core body temperature.
--
-- The spine of the homeostasis sim: a lumped heat-balance model that drifts a
-- unit's core temperature toward the equilibrium set by its environment, the
-- body's metabolic heat, insulation (fat), and active thermoregulation
-- (shivering when cold, sweating when hot — sweating gated by humidity). At
-- the extremes the hypothermia / hyperthermia failure meters (driven from the
-- functions here) kill the unit, with a survival window so it can be rescued
-- by getting it to better conditions.
--
-- core_temp lives as a unit STAT ("core_temp", °C) while the thermo system is
-- SIMULATED, so it saves with the unit and shows in the Status panel; a
-- Nominal thermo stores nothing and reads the baseline (#2831). Ticked from
-- unit_resources.update, only for a Simulated thermo.
--
-- Future layers (circulation, heart rate, blood oxygen, salts, brain effects,
-- frostbite, fever feedback) read/extend core_temp — see the treatment-arc
-- design notes. This module is deliberately just the thermal foundation.

local circulation = require("scripts.circulation")
local salts       = require("scripts.salts")
local injuries    = require("scripts.injuries")
local physiology  = require("scripts.physiology")

local thermo = {}

thermo.BASELINE = 37.0   -- homeostatic core temperature (°C)

-- ---- Tunables (calibrated for "slowly freeze in the arctic / cook in a
-- 200°F desert over ~10-20 min"; all freely tweakable) ----
local BASAL_HEAT    = 0.9    -- metabolic heat flux at rest (× metabolism stat)
local ACTIVITY_HEAT = 0.6    -- extra heat while moving
-- Starvation cuts heat generation: a body running on empty can't sustain
-- full metabolic output, so it cools faster (low calories accelerate
-- hypothermia). Below HUNGER_HEAT_FLOOR_FRAC of the calorie STORE the
-- metabolic heat output ramps down linearly toward HUNGER_HEAT_MIN at an
-- empty store. Units with no calorie store (wildlife) are unaffected.
local HUNGER_HEAT_FLOOR_FRAC = 0.5
local HUNGER_HEAT_MIN        = 0.6
local LOSS_COEFF    = 0.10   -- heat lost per °C of body↔air gradient, / insulation
local INSUL_BASE    = 1.0    -- baseline insulation (naked; thermoneutral ≈28°C)
local FAT_INSUL_K   = 2.0    -- + body-fat fraction × this (blubber insulates)
local CLOTHING_INSUL_K = 0.5 -- × the worn gear's summed insulation. Tuned so a
                             -- full kit (~1.95) is comfortable at temperate but
                             -- DEEP cold (~−50°C) still overwhelms it → freezes.
local THERMAL_INERTIA = 0.003  -- body thermal mass: how slowly core_temp moves
-- Heat capacity ∝ body mass, so core_temp responds at a rate ∝ 1/mass: a tiny
-- creature swings fast (freezes / overheats in seconds), a giant swings slowly.
-- THERMAL_INERTIA is calibrated at the human reference mass; scale by
-- HUMAN_MASS/body_mass for everyone else. The equilibrium core temp is
-- UNCHANGED by this (it's set by the production/loss balance, not the rate) —
-- only how fast the body reaches it. MAX_DTEMP_PER_TICK is a stability clamp so
-- the explicit integrator can't overshoot for very-low-mass (very-fast) bodies.
local HUMAN_MASS = 70.0
local MAX_DTEMP_PER_TICK = 1.5  -- °C; anti-overshoot guard, never bites at human scale
local THERMONEUTRAL = BASAL_HEAT / LOSS_COEFF  -- gradient at rest equilibrium (≈9°C)

local SHIVER_START = 36.5    -- below this, shivering kicks in (adds heat, costs stamina)
local SHIVER_K     = 1.2
local SHIVER_MAX   = 2.5
local SWEAT_START  = 37.5    -- above this, sweating kicks in (sheds heat, costs hydration)
local SWEAT_K      = 1.5
local SWEAT_MAX    = 4.0

-- Danger bands (°C). Failure fraction ramps 0→1 across each.
local HYPO_START  = 35.0  ; local HYPO_FATAL  = 28.0
local HYPER_START = 39.0  ; local HYPER_FATAL = 42.0

-- Minimal physiological costs of fighting temperature.
local SHIVER_STAMINA_K  = 0.04   -- stamina/s drained per unit of shivering
local SWEAT_HYDRATION_K = 0.05   -- hydration/s drained per unit of sweating (at 70 kg)
local SWEAT_SALT_K      = 0.10   -- salt/s drained per unit of sweating (at 70 kg)

-- Poor circulation conserves core heat (vasoconstriction keeps warm blood off
-- the cold skin) — so heat LOSS scales with circulation, down to this floor.
local CIRC_LOSS_FLOOR = 0.5

-- Frostbite: below FROST_AMBIENT (°C), poorly-perfused extremities freeze →
-- necrosis (the engine rots them off / lets them get infected). Drive scales
-- with how far below freezing AND how poor circulation is — so a
-- bleeding/obese/sick/already-cold (vasoconstricted) unit frostbites far
-- sooner than a healthy one.
local FROST_AMBIENT       = 0.0
local FROST_AMBIENT_SCALE = 25.0   -- −25°C below the threshold = full cold drive
local FROST_CIRC_REF      = 1.05   -- (REF − circulation): perfect circ still frosts slowly
local FROST_RATE          = 0.015  -- extremity necrosis/s at full drive
local FROST_PARTS = { "l_hand", "r_hand", "l_foot", "r_foot" }

-- Fever: infection RAISES THE THERMOREGULATORY SETPOINT (the body shivers up to
-- a higher target and won't sweat-cool below it), so the fever holds at any
-- ambient — not just a heat bump that a cold room would absorb. High core temp
-- in turn SLOWS infection growth (Combat.Wounds), a self-regulating loop. A
-- moderate infection is a fever (~38-39); a severe one pushes toward the
-- hyperthermia danger zone (40+), so a raging fever CAN kill.
local FEVER_OFFSET_K   = 2.0   -- °C of setpoint rise per unit of infection load
local FEVER_MAX_OFFSET = 3.5   -- ceiling (fever core ≈ 40.5°C)

local MOVING = { walking = true, running = true, sprinting = true }

local clamp = require("scripts.lib.numeric").clamp
local effects = require("scripts.unit_effects")

-- Sweat debit mass relationship (#2825). The two SWEAT_*_K constants are the
-- absolute debits at the HUMAN_MASS reference; every other body pays them
-- scaled by body_mass / HUMAN_MASS. That is the same proportional rule as the
-- baseline salt excretion (salts.lua) and baseline hydration drain
-- (unit_resource_config.lua): the pools are sized by mass (max_salt = mass,
-- max_hydration = 0.6 × mass), so the FRACTION of each pool a unit of sweat
-- removes per second — and so the depletion TIME — is the same for a 0.3 kg
-- squirrel, a 70 kg human and a 170 kg bear. Exactly 1 at 70 kg, so 70 kg
-- calibrations keep their meaning. A missing body_mass reads as 70 kg (as
-- everywhere in this file); zero, negative or non-finite mass scales to 0,
-- i.e. no sweat debit, rather than a negative or infinite one.
function thermo.sweatMassScale(bodyM)
    if not bodyM or bodyM ~= bodyM or bodyM <= 0 or bodyM == math.huge then
        return 0
    end
    return bodyM / HUMAN_MASS
end

-- ---- The heat balance (#2960) ----
--
-- One tick's heat balance is split in three so the engine's own rule can be
-- READ without advancing anything:
--
--   thermo.heatInputs(uid, info)  gathers a unit's current state (reads only);
--   thermo.heatBalance(inputs)    is the rule, a PURE function of that table;
--   thermo.tick(uid, info, dt)    = gather -> rule -> apply (the only writer).
--
-- `thermo.readHeatBalance(uid)` is the read verb: gather + rule for a unit's
-- current state, with no mutation (no setStat, debit, frostbite, stat roll or
-- cache). The physiology probe reads it to judge a unit against what the rule
-- predicts from that unit's own state. thermo.tick calls the same two
-- functions, so the expectation and the simulation cannot drift apart.
--
-- The gather reads unowned stats through a READER: the tick's reader
-- (thermo.TICK) uses unit.getStat, which lazily rolls and caches a templated
-- stat that has not been rolled yet, as the tick always did (that is how a
-- unit's first pass initializes it); the read verb's reader (thermo.OBSERVE)
-- uses unit.peekStat, which never rolls, advances the stat RNG or caches, and
-- so reports a unit with an unrolled input as not observable instead of
-- changing it. The physiology access audit holds the observe side to that.

local function feverOffsetOf(uid)
    return math.min(FEVER_MAX_OFFSET,
                    FEVER_OFFSET_K * injuries.infectionLoad(uid))
end

-- The tick's reader: the stat getters roll an unrolled templated stat and
-- cache it, which is the tick's own initialization of a unit and so exactly
-- what it always did. NEVER used by the read verb.
function thermo.tickStat(uid, name)
    return unit.getStat(uid, name)
end

function thermo.tickCalFrac(uid)
    return physiology.fraction(uid, "calories", "max_calories")
end

-- The read verb's reader: it observes and changes nothing. A stat the unit
-- holds is returned as the getter would return it; an unrolled one answers
-- nil and WHY ("unrolled"), and a stat the unit does not define answers nil,
-- "undefined" (the gather then takes its defaults, as the tick does).
function thermo.observeStat(uid, name)
    return unit.peekStat(uid, name)
end

-- The calorie STORE fraction from the owned pool (a pure registry read) and
-- the peeked maximum, in physiology.fraction's order: nil without a usable
-- maximum or a pool; or nil, "unrolled" when the maximum is not rolled yet.
function thermo.observeCalFrac(uid)
    local mx, why = unit.peekStat(uid, "max_calories")
    if mx == nil and why == "unrolled" then return nil, why end
    if not mx or mx <= 0 then return nil end
    local cur = physiology.get(uid, "calories")
    if not cur then return nil end
    return cur / mx
end

thermo.TICK    = { stat = thermo.tickStat,    calFrac = thermo.tickCalFrac }
thermo.OBSERVE = { stat = thermo.observeStat, calFrac = thermo.observeCalFrac }

-- Gather the state one tick's heat balance reads, through `reader`
-- (thermo.TICK or thermo.OBSERVE). Returns nil when there is nothing to
-- balance (no grid position, no active world climate, or no stored core
-- temperature), and nil, "unrolled:<stat>" when the OBSERVE reader meets an
-- input the unit has not rolled yet. `info` is unit.getInfo(uid) (for
-- gridX/gridY).
--
-- The sweat factor is read only for a unit past its sweat onset, exactly as
-- the tick always did, so a unit that is not sweating never reads the salt
-- concentration.
function thermo.heatInputs(uid, info, reader)
    local gx, gy = info.gridX, info.gridY
    if not gx or not gy then return nil end
    local clim = world.getClimateAt(gx, gy)
    if not clim then return nil end   -- no active world → leave core_temp alone

    -- Only a SIMULATED thermo ticks (scripts/unit_resources.lua gates it), so
    -- its stored core is present by the seeding rule (#2831): a missing one
    -- is an invariant violation that physiology.get raises, never a cue to
    -- start from the baseline.
    local core = physiology.get(uid, "core_temp")
    if not core then return nil end

    -- Circulation, derived on demand (#2836, S9) from the unit's current
    -- inputs, so it is coherent with this tick's blood, stats and heart rate.
    -- A Nominal cardio still computes it with every injury factor live (SD-21)
    -- and a unit this runs for always has a body, so the nil case is only a
    -- guard.
    local circ = circulation.get(uid) or 1.0

    -- Ambient is ELEVATION-CORRECTED: world.getAmbientAt applies the worldgen
    -- altitude lapse rate (the same one the ice system uses), so a unit on an
    -- ice-capped peak actually feels the cold instead of the valley mean (#308).
    -- Falls back to the raw regional mean if the helper is unavailable.
    local ambient  = world.getAmbientAt and world.getAmbientAt(gx, gy)
    if not ambient then ambient = clim.temp or 20.0 end
    local humidity = clamp(clim.humidity or 0.5, 0, 1)
    -- Debug/test override: set thermo.debugAmbient (°C) / debugHumidity to
    -- simulate an arctic / desert environment in the flat arena (whose climate
    -- is a fixed ~0°C). nil in normal play.
    if thermo.debugAmbient then ambient = thermo.debugAmbient end
    if thermo.debugHumidity then humidity = thermo.debugHumidity end

    local unrolled = nil
    local function stat(name)
        local v, why = reader.stat(uid, name)
        if v == nil and why == "unrolled" and not unrolled then unrolled = name end
        return v
    end
    local metab = stat("metabolism") or 1.0
    local fatM  = stat("fat_mass") or 0.0
    local bodyM = stat("body_mass") or 70.0
    -- Insulation inputs: body fat and WORN CLOTHING (unit.getInsulation sums
    -- the equipped/accessory gear's insulation). Clothing is the difference
    -- between freezing naked and being comfortable — dress for the climate.
    local clothing = (unit.getInsulation and unit.getInsulation(uid)) or 0

    -- Starvation input: a low calorie STORE (not the stomach meter — a unit
    -- with an empty stomach but a fed store keeps full output) throttles
    -- metabolic heat. nil for a unit with no calorie store.
    local calFrac, calWhy = reader.calFrac(uid)
    if calWhy == "unrolled" and not unrolled then unrolled = "max_calories" end
    if unrolled then return nil, "unrolled:" .. unrolled end

    local feverOffset = feverOffsetOf(uid)
    local sweatFactor = nil
    if core > SWEAT_START + feverOffset then
        sweatFactor = salts.sweatFactor(uid)
    end

    return {
        core        = core,
        circ        = circ,
        ambient     = ambient,
        humidity    = humidity,
        metab       = metab,
        fatM        = fatM,
        bodyM       = bodyM,
        clothing    = clothing,
        calFrac     = calFrac,
        moving      = MOVING[unit.getActivity(uid) or "idle"] == true,
        feverOffset = feverOffset,
        sweatFactor = sweatFactor,
    }
end

-- THE RULE. A pure function of a heat-inputs table (see heatInputs): no
-- reads of the world or of any unit, no writes, deterministic. Returns the
-- terms of one tick's balance:
--
--   production, shiver, loss, sweat   heat fluxes (shiver is INCLUDED in
--                                     production, and reported for its cost)
--   net                               production - loss - sweat
--   inertia                           °C per flux-second (∝ HUMAN_MASS/mass)
--   rate                              net * inertia: the UNCLAMPED core-temp
--                                     rate in °C/s the unit is on right now
--   insul, calFactor, fatFrac         the intermediate values the terms use
--
-- A tick of `dt` seconds moves the core by thermo.coreDelta(balance, dt).
function thermo.heatBalance(inputs)
    local core, circ = inputs.core, inputs.circ
    local bodyM = inputs.bodyM
    local fatFrac = (bodyM > 0) and (inputs.fatM / bodyM) or 0.2
    -- Insulation: baseline + body fat + WORN CLOTHING.
    local insul = INSUL_BASE + fatFrac * FAT_INSUL_K
                + inputs.clothing * CLOTHING_INSUL_K

    -- Starvation factor: below HUNGER_HEAT_FLOOR_FRAC of the calorie store
    -- the metabolic heat output ramps down linearly toward HUNGER_HEAT_MIN.
    local calFactor = 1.0
    local frac = inputs.calFrac
    if frac and frac < HUNGER_HEAT_FLOOR_FRAC then
        calFactor = HUNGER_HEAT_MIN + (1 - HUNGER_HEAT_MIN)
                                      * (frac / HUNGER_HEAT_FLOOR_FRAC)
    end

    -- Metabolic heat production (basal + activity + shivering). All three
    -- scale with the starvation factor: basal/activity burn is throttled
    -- when calories run low, and shivering — sustained muscle work that
    -- needs metabolic fuel — likewise fails on an empty stomach (a real
    -- sign of severe hypoglycemia), so a starving body in the cold can't
    -- defend its temperature and chills far faster.
    local production = BASAL_HEAT * inputs.metab * calFactor
    if inputs.moving then production = production + ACTIVITY_HEAT * calFactor end

    -- Fever raises the setpoint: the body shivers up to (and won't sweat below)
    -- SHIVER_START/SWEAT_START + this offset.
    local shiverStart = SHIVER_START + inputs.feverOffset
    local sweatStart  = SWEAT_START  + inputs.feverOffset

    local shiver = 0
    if core < shiverStart then
        shiver = math.min(SHIVER_MAX, SHIVER_K * (shiverStart - core)) * calFactor
        production = production + shiver
    end

    -- Conductive/convective loss to the air (negative = the environment is
    -- HEATING the body, when ambient > core). Scaled by circulation:
    -- vasoconstriction (poor circulation) keeps warm blood off the skin, so a
    -- cold unit sheds heat more slowly — protecting the core at the expense of
    -- the extremities (the frostbite tradeoff).
    local circLoss = CIRC_LOSS_FLOOR + (1 - CIRC_LOSS_FLOOR) * circ
    local loss = (core - inputs.ambient) * LOSS_COEFF / insul * circLoss

    -- Evaporative cooling (sweat). Useless in high humidity — which is what
    -- makes a hot, humid environment lethal at lower temperatures than a dry one.
    -- ALSO needs salt: a salt-depleted unit can't sweat effectively, so it
    -- overheats sooner (the salt↔heat tie-in).
    local sweat = 0
    if core > sweatStart then
        sweat = math.min(SWEAT_MAX, SWEAT_K * (core - sweatStart)
                                    * (1 - inputs.humidity))
        sweat = sweat * (inputs.sweatFactor or 1.0)
    end

    -- Heat capacity ∝ mass → rate ∝ 1/mass (small bodies swing fast).
    local inertia = THERMAL_INERTIA * (HUMAN_MASS / math.max(bodyM, 0.001))
    local net = production - loss - sweat
    return {
        production = production, shiver = shiver, loss = loss, sweat = sweat,
        net = net, inertia = inertia, rate = net * inertia,
        insul = insul, calFactor = calFactor, fatFrac = fatFrac,
    }
end

-- The core-temperature change one tick of `dt` seconds makes from a balance:
-- the stability-clamped step (MAX_DTEMP_PER_TICK never bites at human scale).
function thermo.coreDelta(balance, dt)
    return clamp(balance.net * dt * balance.inertia,
                 -MAX_DTEMP_PER_TICK, MAX_DTEMP_PER_TICK)
end

-- READ VERB (#2960): a unit's current heat balance, with no side effects, or
-- nil when there is nothing to balance. Returns the balance table and the
-- inputs table it was evaluated from; nil, "unrolled:<stat>" for a unit that
-- holds an input the read would have to roll to know (a read never rolls).
-- Honors the same climate selection and debug overrides the tick does,
-- because it gathers through the same function.
function thermo.readHeatBalance(uid)
    local info = unit.getInfo(uid)
    if not info then return nil end
    local inputs, why = thermo.heatInputs(uid, info, thermo.OBSERVE)
    if not inputs then return nil, why end
    return thermo.heatBalance(inputs), inputs
end

-- Advance a unit's core temperature one tick. `info` is unit.getInfo(uid)
-- (for gridX/gridY). No-op if there's no world climate to sample.
--
-- Returns a receipt of the step it executed, or nil when it executed none:
-- { dt = seconds, delta = the unclamped net * dt * inertia, applied = the
-- stability-clamped change coreDelta moved the core by }. The scheduler
-- passes a fixed script tick as `dt` and drops missed passes, so the number
-- of executed steps is not derivable from wall-clock time; a caller that must
-- account executed simulated time (the physiology probe's ledger, #2960) wraps
-- this function and sums the receipts. The production caller ignores it.
function thermo.tick(uid, info, dt)
    local inputs = thermo.heatInputs(uid, info, thermo.TICK)
    if not inputs then return nil end
    local bal = thermo.heatBalance(inputs)

    local applied = thermo.coreDelta(bal, dt)
    local core = clamp(inputs.core + applied, 15.0, 45.0)
    effects.setStat(uid, "core_temp", core)

    -- Minimal costs of thermoregulating.
    local shiver, sweat = bal.shiver, bal.sweat
    if shiver > 0 then
        -- A relative, atomic debit (#2834, S8-F2): the old read-modify-write
        -- republished an absolute value and refunded any strike combat
        -- committed in between. A Nominal stamina receiver discards it.
        physiology.debit(uid, "stamina", SHIVER_STAMINA_K * shiver * dt)
    end
    if sweat > 0 then
        -- Debits scale with body mass (thermo.sweatMassScale): the same
        -- sweat removes the same FRACTION of any body's pools.
        local massScale = thermo.sweatMassScale(inputs.bodyM)
        physiology.debit(uid, "hydration", SWEAT_HYDRATION_K * massScale * sweat * dt)
        -- Sweat loses salt too. With water replaced by drinking but salt only
        -- by eating, heavy sweating + plain water → hyponatremia.
        physiology.debit(uid, "salt", SWEAT_SALT_K * massScale * sweat * dt)
    end

    -- Frostbite: freeze the extremities when it's cold and circulation is poor.
    -- The engine (unit.frostbite → woundNecrosis) handles the rest: rot-off,
    -- infection susceptibility, display.
    local ambient, circ = inputs.ambient, inputs.circ
    if ambient < FROST_AMBIENT then
        local drive = clamp((FROST_AMBIENT - ambient) / FROST_AMBIENT_SCALE, 0, 1)
                    * clamp(FROST_CIRC_REF - circ, 0, 1)
        if drive > 0.001 then
            local nec = FROST_RATE * drive * dt
            for _, part in ipairs(FROST_PARTS) do
                effects.frostbite(uid, part, nec)
            end
        end
    end
    return { dt = dt, delta = bal.net * dt * bal.inertia, applied = applied }
end

function thermo.coreTemp(uid)
    local c = unit.getStat(uid, "core_temp")
    if not c or c <= 0 then return thermo.BASELINE end
    return c
end

-- 0..1 hypothermia danger fraction (drives the hypothermia failure meter).
function thermo.hypothermiaFailure(uid)
    return clamp((HYPO_START - thermo.coreTemp(uid)) / (HYPO_START - HYPO_FATAL), 0, 1)
end

-- 0..1 hyperthermia danger fraction (drives the hyperthermia failure meter).
function thermo.hyperthermiaFailure(uid)
    return clamp((thermo.coreTemp(uid) - HYPER_START) / (HYPER_FATAL - HYPER_START), 0, 1)
end

-- A short status word for the current thermal state (for the Status panel).
function thermo.stateLabel(uid)
    local c = thermo.coreTemp(uid)
    if c <= HYPO_FATAL + 2 then return "Freezing"
    elseif c < HYPO_START   then return "Hypothermic"
    elseif c < SHIVER_START then return "Cold"
    elseif c > HYPER_FATAL - 2 then return "Overheating"
    elseif c > HYPER_START  then return "Heat stroke"
    elseif c > SWEAT_START + 0.5 then return "Hot"
    else return "Normal" end
end

return thermo
