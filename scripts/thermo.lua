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
-- core_temp lives as a unit STAT ("core_temp", °C), so it saves with the unit
-- and shows in the Status panel. Ticked from unit_resources.update.
--
-- Future layers (circulation, heart rate, blood oxygen, salts, brain effects,
-- frostbite, fever feedback) read/extend core_temp — see the treatment-arc
-- design notes. This module is deliberately just the thermal foundation.

local circulation = require("scripts.circulation")
local salts       = require("scripts.salts")
local injuries    = require("scripts.injuries")

local thermo = {}

thermo.BASELINE = 37.0   -- homeostatic core temperature (°C)

-- ---- Tunables (calibrated for "slowly freeze in the arctic / cook in a
-- 200°F desert over ~10-20 min"; all freely tweakable) ----
local BASAL_HEAT    = 0.9    -- metabolic heat flux at rest (× metabolism stat)
local ACTIVITY_HEAT = 0.6    -- extra heat while moving
-- Starvation cuts heat generation: a body running on empty can't sustain
-- full metabolic output, so it cools faster (low calories accelerate
-- hypothermia). Below HUNGER_HEAT_FLOOR_FRAC of the hunger pool the
-- metabolic heat output ramps down linearly toward HUNGER_HEAT_MIN at an
-- empty stomach. Units with no hunger pool (wildlife) are unaffected.
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
local SWEAT_HYDRATION_K = 0.05   -- hydration/s drained per unit of sweating
local SWEAT_SALT_K      = 0.10   -- salt drained per unit of sweating

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

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- Advance a unit's core temperature one tick. `info` is unit.getInfo(uid)
-- (for gridX/gridY). No-op if there's no world climate to sample.
function thermo.tick(uid, info, dt)
    local gx, gy = info.gridX, info.gridY
    if not gx or not gy then return end
    local clim = world.getClimateAt(gx, gy)
    if not clim then return end   -- no active world → leave core_temp alone

    local core = unit.getStat(uid, "core_temp")
    if not core or core <= 0 then core = thermo.BASELINE end

    -- Circulation, recomputed each tick (vasoconstriction uses this core). Set
    -- always — other systems (frostbite, display) read the stat.
    local circ = circulation.compute(uid, core)
    unit.setStat(uid, "circulation", circ)

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

    local metab = unit.getStat(uid, "metabolism") or 1.0
    local fatM  = unit.getStat(uid, "fat_mass") or 0.0
    local bodyM = unit.getStat(uid, "body_mass") or 70.0
    local fatFrac = (bodyM > 0) and (fatM / bodyM) or 0.2
    -- Insulation: baseline + body fat + WORN CLOTHING (unit.getInsulation sums
    -- the equipped/accessory gear's insulation). Clothing is the difference
    -- between freezing naked and being comfortable — dress for the climate.
    local clothing = (unit.getInsulation and unit.getInsulation(uid)) or 0
    local insul = INSUL_BASE + fatFrac * FAT_INSUL_K + clothing * CLOTHING_INSUL_K

    -- Starvation factor: low calories throttle metabolic heat output.
    local calFactor = 1.0
    local maxHun = unit.getStat(uid, "max_hunger")
    local curHun = unit.getStat(uid, "hunger")
    if maxHun and maxHun > 0 and curHun then
        local frac = curHun / maxHun
        if frac < HUNGER_HEAT_FLOOR_FRAC then
            calFactor = HUNGER_HEAT_MIN + (1 - HUNGER_HEAT_MIN)
                                          * (frac / HUNGER_HEAT_FLOOR_FRAC)
        end
    end

    -- Metabolic heat production (basal + activity + shivering). All three
    -- scale with the starvation factor: basal/activity burn is throttled
    -- when calories run low, and shivering — sustained muscle work that
    -- needs metabolic fuel — likewise fails on an empty stomach (a real
    -- sign of severe hypoglycemia), so a starving body in the cold can't
    -- defend its temperature and chills far faster.
    local activity = unit.getActivity(uid) or "idle"
    local production = BASAL_HEAT * metab * calFactor
    if MOVING[activity] then production = production + ACTIVITY_HEAT * calFactor end

    -- Fever raises the setpoint: the body shivers up to (and won't sweat below)
    -- SHIVER_START/SWEAT_START + this offset.
    local feverOffset = math.min(FEVER_MAX_OFFSET,
                                 FEVER_OFFSET_K * injuries.infectionLoad(uid))
    local shiverStart = SHIVER_START + feverOffset
    local sweatStart  = SWEAT_START  + feverOffset

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
    local loss = (core - ambient) * LOSS_COEFF / insul * circLoss

    -- Evaporative cooling (sweat). Useless in high humidity — which is what
    -- makes a hot, humid environment lethal at lower temperatures than a dry one.
    -- ALSO needs salt: a salt-depleted unit can't sweat effectively, so it
    -- overheats sooner (the salt↔heat tie-in).
    local sweat = 0
    if core > sweatStart then
        sweat = math.min(SWEAT_MAX, SWEAT_K * (core - sweatStart) * (1 - humidity))
        sweat = sweat * salts.sweatFactor(uid)
    end

    -- Heat capacity ∝ mass → rate ∝ 1/mass (small bodies swing fast).
    local inertia = THERMAL_INERTIA * (HUMAN_MASS / math.max(bodyM, 0.001))
    local dTemp = (production - loss - sweat) * dt * inertia
    dTemp = clamp(dTemp, -MAX_DTEMP_PER_TICK, MAX_DTEMP_PER_TICK)
    core = clamp(core + dTemp, 15.0, 45.0)
    unit.setStat(uid, "core_temp", core)

    -- Minimal costs of thermoregulating.
    if shiver > 0 then
        local st = unit.getStat(uid, "stamina")
        if st then unit.setStat(uid, "stamina",
            math.max(0, st - SHIVER_STAMINA_K * shiver * dt)) end
    end
    if sweat > 0 then
        local hy = unit.getStat(uid, "hydration")
        if hy then unit.setStat(uid, "hydration",
            math.max(0, hy - SWEAT_HYDRATION_K * sweat * dt)) end
        -- Sweat loses salt too. With water replaced by drinking but salt only
        -- by eating, heavy sweating + plain water → hyponatremia.
        local s = unit.getStat(uid, "salt")
        if s then unit.setStat(uid, "salt",
            math.max(0, s - SWEAT_SALT_K * sweat * dt)) end
    end

    -- Frostbite: freeze the extremities when it's cold and circulation is poor.
    -- The engine (unit.frostbite → woundNecrosis) handles the rest: rot-off,
    -- infection susceptibility, display.
    if ambient < FROST_AMBIENT then
        local drive = clamp((FROST_AMBIENT - ambient) / FROST_AMBIENT_SCALE, 0, 1)
                    * clamp(FROST_CIRC_REF - circ, 0, 1)
        if drive > 0.001 then
            local nec = FROST_RATE * drive * dt
            for _, part in ipairs(FROST_PARTS) do
                unit.frostbite(uid, part, nec)
            end
        end
    end
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
