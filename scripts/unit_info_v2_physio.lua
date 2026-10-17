-- Unit info v2: mode-aware physiology display (#2832, S6 of epic #2827;
-- design D-7, D-8).
--
-- The Status panel's rows and conditions for the registry's systems
-- (thermo, salt, hydration, nutrition, stamina, sleep, cardio, blood;
-- exhaustion has no row) follow the unit's RESOLVED mode:
--
--   * Simulated  -- unchanged: today's values, units, formats, colours.
--   * Nominal    -- the row shows its healthy reading (a pool shows the
--                   word "Nominal"), in the neutral value colour, with the
--                   value tooltip "Not simulated for this unit: always
--                   healthy.". Conditions and failure meters the system
--                   owns never appear.
--   * Unsupported (bodyless unit) -- the row, and everything the system
--                   owns, is hidden.
--   * A DERIVED row whose Nominal answer is still live (circulation, SD-21) --
--                   shown with its real value and its usual colour bands
--                   under a Nominal system: blood loss, sepsis, fitness and
--                   cold still act, so "always healthy" would be false. Only
--                   an Unsupported system hides it.
--   * Unowned keys (blood, pain, sleep, ...) -- untouched
--                   pass-through; this module never reads them.
--
-- Every read here goes through scripts.physiology (the mode resolver and
-- the read verbs), so rendering inserts no owned value, changes no mode
-- and advances no RNG: a Nominal system has nothing stored to read, and a
-- screenshot can never be made healthy by writing state. The panel's
-- shape signature folds `signature(uid)` in so a mode change rebuilds the
-- row set and tooltips (unit_info_v2_stats.lua).
--
-- Split out of unit_info_v2_status.lua, which is at its line budget.

local physiology = require("scripts.physiology")
local statDefs   = require("scripts.unit_info_v2_stat_defs")
local L          = require("scripts.unit_info_v2_layout")

local M = {}

M.NOMINAL_TEXT = "Nominal"
M.NOMINAL_HINT = "Not simulated for this unit: always healthy."

-- Status-panel row key -> the registry key whose system owns the row. A
-- display alias is not a registry key: body_temp shows core_temp (thermo)
-- and the salt row shows the derived salt_conc (salt). A row absent here
-- is unowned and passes through.
local ROW_KEY = {
    body_temp = "core_temp",
    salt      = "salt_conc",
    hydration = "hydration",
    hunger    = "hunger",
    calories  = "calories",
    stamina   = "stamina",
    -- The sleep system's pool (#2835): the system id is `sleep`, the row and
    -- the registry key are `sleep_pressure`.
    sleep_pressure = "sleep_pressure",
    -- The cardio system (#2836): its two owned integrators and the derived
    -- circulation it computes.
    heart_rate     = "heart_rate",
    blood_oxygen   = "blood_oxygen",
    circulation    = "circulation",
    -- The blood system's volume (#2837, S10): the row key and the registry key
    -- are both `blood`. Its value lives in the dedicated uiBlood field, read
    -- through physiology.get, never a stats key.
    blood          = "blood",
}
M.ROW_KEY = ROW_KEY

-- Rows whose Nominal answer is a LIVE derived value, not a healthy constant
-- (the registry's `healthy: live`): they keep their value, colour and tooltip
-- under a Nominal system and are hidden only when it is Unsupported.
local LIVE_ROW = { circulation = true }

-- "simulated" | "nominal" | "unsupported" for a registry-owned key, nil
-- for an unowned one (no system owns it, so there is no mode to apply --
-- not the same as a nil mode, which is an Unsupported system).
function M.keyState(uid, key)
    local system = physiology.systemOf(key)
    if not system then return nil end
    return physiology.mode(uid, system) or "unsupported"
end

-- True when the key's system lets its conditions and meters show: the key
-- is unowned, or its system is Simulated.
function M.active(uid, key)
    local s = M.keyState(uid, key)
    return s == nil or s == "simulated"
end

-- One token naming every system's resolved mode, folded into the Status
-- panel's shape signature: changing a mode, the unit's support, or a
-- default changes it; an ordinary value change does not.
function M.signature(uid)
    local parts = {}
    for _, sid in ipairs(physiology.systemOrder()) do
        parts[#parts + 1] = sid .. "=" .. (physiology.mode(uid, sid) or "-")
    end
    return table.concat(parts, ",")
end

-- Healthy readings for the scalar rows, in the Simulated formatters' units,
-- led by the word Nominal so the mode is stated, not only implied by the
-- colour and the hover ("Nominal 37.0°C", "Nominal 100%").
local function healthyTemp(uid)
    return M.NOMINAL_TEXT .. string.format(" %.1f\194\176C",
        physiology.get(uid, "core_temp") or 37.0)
end
local function healthySalt(uid)
    return M.NOMINAL_TEXT .. string.format(" %d%%",
        math.floor((physiology.get(uid, "salt_conc") or 1.0) * 100 + 0.5))
end
local function poolText() return M.NOMINAL_TEXT end
local function healthyHeartRate(uid)
    return M.NOMINAL_TEXT .. string.format(" %d bpm",
        math.floor((physiology.get(uid, "heart_rate") or 70) + 0.5))
end
local function healthyBloodOxygen(uid)
    return M.NOMINAL_TEXT .. string.format(" %d%%",
        math.floor((physiology.get(uid, "blood_oxygen") or 1.0) * 100 + 0.5))
end

local NOMINAL_TEXT_OF = {
    body_temp = healthyTemp,
    salt      = healthySalt,
    hydration = poolText,
    hunger    = poolText,
    calories  = poolText,
    stamina   = poolText,
    sleep_pressure = poolText,
    heart_rate   = healthyHeartRate,
    blood_oxygen = healthyBloodOxygen,
    blood        = poolText,
}

-- A Nominal row whose tooltip carries more than the Nominal hint: the blood
-- row keeps the LIVE wound bleed rate, because Nominal blood supplies a full
-- volume but never stops a wound bleeding (#2837). Returns extra hint text.
local NOMINAL_EXTRA = {
    blood = function(uid)
        local b = unit.getBlood(uid)
        local rate = b and b.bleedRate or 0
        if rate <= 0 then return "Not bleeding." end
        local pct = (b.max and b.max > 0) and (rate / b.max * 100) or 0
        return string.format("Bleeding: %.0f ml/s (%.2f%%/s), but the volume is not lost.",
            rate * 1000, pct)
    end,
}

-- The label and its healthy reading are two words, so a Nominal scalar row
-- is set in the condition font: at the stat font the pair is wider than the
-- panel's value column and would run over the icon. Rows are chosen at build
-- time and a mode change rebuilds them, so the font follows the mode.
local SCALAR_ROW = { body_temp = true, salt = true, heart_rate = true,
                    blood_oxygen = true }

-- Wrap one Status row definition so its value, colour and value tooltip
-- follow the mode. An unowned row is returned as is. `uid` is the unit the
-- panel is being built for.
function M.row(def, uid)
    local regKey = ROW_KEY[def.key]
    if not regKey then return def end
    local value, colorFn, tooltipFn = def.value, def.colorFn, def.valueTooltip
    local sd   = statDefs.STAT_DEFS[def.key]
    local name = sd and sd.name or def.key
    local out = {}
    for k, v in pairs(def) do out[k] = v end
    if SCALAR_ROW[def.key] and M.keyState(uid, regKey) == "nominal" then
        out.opts = { fontSize = L.CONDITION_FONT_SIZE }
    end
    if LIVE_ROW[def.key] then
        -- A live derived value: hidden only when the system is Unsupported.
        out.value = function(uid)
            if M.keyState(uid, regKey) == "unsupported" then return nil end
            return value(uid)
        end
        return out
    end
    out.value = function(uid)
        local s = M.keyState(uid, regKey)
        if s == "unsupported" then return nil end
        if s == "nominal" then return NOMINAL_TEXT_OF[def.key](uid) end
        return value(uid)
    end
    out.colorFn = function(uid)
        if M.keyState(uid, regKey) == "nominal" then return L.CONTENT_VAL_COLOR end
        return colorFn and colorFn(uid) or nil
    end
    out.valueTooltip = function(uid)
        if M.keyState(uid, regKey) == "nominal" then
            local extra = NOMINAL_EXTRA[def.key]
            return { text = name,
                     hint = extra and (M.NOMINAL_HINT .. "\n" .. extra(uid)) or M.NOMINAL_HINT }
        end
        return tooltipFn and tooltipFn(uid) or nil
    end
    return out
end

-- Failure-meter conditions (delayed-death pathways): the rising % so the
-- player sees the clock, with the real numbers in the tooltip. Bucketed to
-- 10% in the row name so the panel rebuilds in steps, not every tick. A
-- meter a Nominal or Unsupported system owns never shows (thermo owns
-- hypothermia/hyperthermia, salt owns salt_imbalance); the wound-driven
-- meters (hypoxia, neuro, shock, organ, sepsis) are unowned and follow
-- their existing rule.
local METER_CONDITIONS = {
    { stat = "hypoxia", label = "Suffocating",   icon = "hydration",
      title = "Cell hypoxia", fatal = "suffocation",
      desc  = "Lungs/airway can't oxygenate the blood." },
    { stat = "neuro",   label = "Brain failing", icon = "nerve_injury",
      title = "Neural shutdown", fatal = "brain death",
      desc  = "Catastrophic brain trauma is shutting the nervous system down." },
    { stat = "shock",   label = "In shock",      icon = "blood",
      title = "Systemic shock", fatal = "cardiac arrest",
      desc  = "The body's whole-system collapse under massive trauma." },
    { stat = "organ",   label = "Organ failure", icon = "festered_injury",
      title = "Organ failure", fatal = "sepsis",
      desc  = "Untreated visceral trauma festering (sepsis / encephalopathy)." },
    { stat = "sepsis",  label = "Septic",        icon = "festered_injury",
      title = "Sepsis", fatal = "sepsis",
      desc  = "Infection from untreated wounds has spread to the blood. "
              .. "Antibiotics can still cure it." },
    { stat = "hypothermia", label = "Hypothermic", icon = "frostbite",
      title = "Hypothermia", fatal = "hypothermia",
      desc  = "Core body temperature is dangerously low. Get the unit "
              .. "somewhere warm." },
    { stat = "hyperthermia", label = "Overheating", icon = "frostbite",
      title = "Hyperthermia", fatal = "heat stroke",
      desc  = "Core body temperature is dangerously high. Get the unit "
              .. "out of the heat and rehydrated." },
    { stat = "salt_imbalance", label = "Electrolyte crisis", icon = "salt",
      title = "Electrolyte imbalance", fatal = "electrolyte imbalance",
      desc  = "Blood sodium is dangerously off balance (hypo- or "
              .. "hypernatremia). Food restores salt; water rebalances it." },
}

-- The visible meter conditions, in the table's order, as { name, icon, hint }.
function M.meterConditions(uid)
    local out = {}
    local meterInfo = require("scripts.unit_resources").meterInfo(uid)
    for _, mc in ipairs(METER_CONDITIONS) do
        local v = M.active(uid, mc.stat) and physiology.get(uid, mc.stat) or nil
        if v and v > 0.02 then
            local mi   = meterInfo[mc.stat] or {}
            local rate = mi.rate or 0
            local rateLine
            if rate > 0 then
                rateLine = string.format(
                    "Rising %.2f%%/s → fatal (%s) at 100%%.", rate * 100, mc.fatal)
            else
                rateLine = string.format(
                    "Recovering %.2f%%/s (injury treated).", -rate * 100)
            end
            out[#out + 1] = {
                name = string.format("%s (%d%%)", mc.label, math.floor(v * 10) * 10),
                icon = mc.icon,
                hint = string.format("%s: %.0f%%\n%s\n%s",
                    mc.title, (mi.value or v) * 100, rateLine, mc.desc),
            }
        end
    end
    return out
end

return M
