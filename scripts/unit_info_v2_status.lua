-- Unit info v2 Status sub-tab (#542 split from unit_info_v2.lua).
--
-- The unit's whole current state in one scrollable list — vitals
-- (most-likely-to-kill first), then daily-need resources, then the
-- carry load, then any active CONDITIONS (knocked down, collapsed,
-- bleeding…) and finally the full INJURY list (icon + name +
-- severity). This is the one place to see why a unit can or can't
-- act. unitConditions is also used by unit_info_v2_stats.lua's
-- panelShapeSig to decide when the panel needs a rebuild.

local injuries   = require("scripts.injuries")
local stats      = require("scripts.unit_stats")
local thermo     = require("scripts.thermo")
local statDefs   = require("scripts.unit_info_v2_stat_defs")
local panelEngine = require("scripts.unit_info_v2_panel_engine")
local L          = require("scripts.unit_info_v2_layout")

local M = {}

local fmtCurMax = statDefs.fmtCurMax
local lowFrac    = statDefs.lowFrac

-- Status formatters: blood + pain aren't stats so they bypass
-- fmtCurMax. Blood is "cur / max L"; pain is the raw accumulator.
local function fmtBlood(uid)
    local b = unit.getBlood(uid)
    if not b then return "?" end
    return string.format("%.1f / %.1f L", b.current, b.max)
end

local function fmtPain(uid)
    local p = unit.getPain(uid)
    if not p then return "?" end
    return string.format("%.2f", p)
end

local function fmtBodyTemp(uid)
    return string.format("%.1f\194\176C", thermo.coreTemp(uid))
end
-- Green near 37, amber in the cold/hot bands, red in the danger zone.
local function bodyTempColor(uid)
    local c = thermo.coreTemp(uid)
    if c <= 35.0 or c >= 39.0 then return { 1.0, 0.30, 0.30, 1.0 } end
    if c <= 36.0 or c >= 38.0 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end

local function fmtCirculation(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "circulation") or 1.0) * 100 + 0.5))
end
local function circulationColor(uid)
    local c = unit.getStat(uid, "circulation") or 1.0
    if c <= 0.4 then return { 1.0, 0.30, 0.30, 1.0 } end
    if c <= 0.65 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end

-- Salt concentration as % of ideal (100% = balanced); both directions bad.
local function fmtSalt(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "salt_conc") or 1.0) * 100 + 0.5))
end
local function saltColor(uid)
    local c = unit.getStat(uid, "salt_conc") or 1.0
    if c <= 0.55 or c >= 1.6 then return { 1.0, 0.30, 0.30, 1.0 } end
    if c <= 0.8  or c >= 1.35 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end

local function fmtHeartRate(uid)
    return string.format("%d bpm", math.floor((unit.getStat(uid, "heart_rate") or 70) + 0.5))
end
local function heartRateColor(uid)
    local hr = unit.getStat(uid, "heart_rate") or 70
    if hr <= 40 or hr >= 160 then return { 1.0, 0.30, 0.30, 1.0 } end
    if hr <= 50 or hr >= 120 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end
local function fmtBloodOxygen(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "blood_oxygen") or 1.0) * 100 + 0.5))
end
local function bloodOxygenColor(uid)
    local o = unit.getStat(uid, "blood_oxygen") or 1.0
    if o <= 0.6 then return { 1.0, 0.30, 0.30, 1.0 } end
    if o <= 0.85 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end
local function fmtConsciousness(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "consciousness") or 1.0) * 100 + 0.5))
end
local function consciousnessColor(uid)
    local c = unit.getStat(uid, "consciousness") or 1.0
    if c < 0.4 then return { 1.0, 0.30, 0.30, 1.0 } end   -- delirious/out
    if c < 0.7 then return { 1.0, 0.62, 0.25, 1.0 } end   -- confused
    return { 0.75, 0.85, 0.70, 1.0 }
end

-- Conditions: the unit's transient STATES (as opposed to injuries, which
-- are wounds). Returns a worst-first list of { name, icon } — what's
-- keeping the unit down or threatening it — derived from pose + the
-- knockedDown flag + vitals + wounds. Shown as rows in the Status tab
-- alongside the vitals and the injury list, so the panel always answers
-- "why can / can't this unit act?".
local function unitConditions(uid)
    local out = {}
    local pose = unit.getPose(uid)
    local info = unit.getInfo(uid)

    if pose == "dead" then
        out[#out + 1] = { name = "Dead", icon = "death",
            hint = "This unit is dead." }
        return out   -- nothing else matters
    end

    if pose == "collapsed" then
        if info and info.knockedDown then
            out[#out + 1] = { name = "Knocked down", icon = "pain",
                hint = "Stunned by a fall. Recovers on its own after a moment." }
        else
            -- Survival collapse: name the gate keeping it down (order
            -- matches checkRevive: blood, then hydration, then stamina).
            local blood = unit.getBlood(uid)
            if blood and blood.max > 0 and blood.current / blood.max < 0.5 then
                out[#out + 1] = { name = "Unconscious (blood loss)", icon = "blood",
                    hint = "Out cold from blood loss. Wakes once blood recovers above 50%." }
            elseif lowFrac(uid, "hydration", "max_hydration", 0.5) then
                out[#out + 1] = { name = "Collapsed (dehydrated)", icon = "hydration",
                    hint = "Collapsed from thirst. Rises once hydration recovers above 50%." }
            elseif lowFrac(uid, "stamina", "max_stamina", 0.5) then
                out[#out + 1] = { name = "Collapsed (exhausted)", icon = "stamina",
                    hint = "Collapsed from exhaustion. Rises once stamina recovers above 50%." }
            else
                out[#out + 1] = { name = "Collapsed", icon = "pain",
                    hint = "On the ground, unable to act." }
            end
        end
    end

    -- Mental state from consciousness (brain.lua): confused / delirious. Out-
    -- cold shows as the collapse condition above. Driven by temp extremes, low
    -- blood oxygen, or salt imbalance.
    local cns = stats.get(uid, "consciousness") or 1.0
    if cns < 0.15 then
        -- unconscious — already shown via the collapsed condition.
    elseif cns < 0.40 then
        out[#out + 1] = { name = "Delirious", icon = "confusion",
            hint = "Addled and unable to act purposefully — stumbling around. "
                   .. "From temperature extremes, low oxygen, or salt imbalance." }
    elseif cns < 0.70 then
        out[#out + 1] = { name = "Confused", icon = "confusion",
            hint = "Mentally clouded. Worsens toward delirium and collapse." }
    end

    -- Failure meters (delayed-death pathways) — show the rising % so the
    -- player sees the clock and can treat in time. The tooltip carries the
    -- real numbers: current %, the per-second fill/recover rate, and what it
    -- kills you with. Bucketed to 10% in the row name so the panel rebuilds
    -- in steps, not every tick.
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
    local meterInfo = require("scripts.unit_resources").meterInfo(uid)
    for _, mc in ipairs(METER_CONDITIONS) do
        local v = stats.get(uid, mc.stat)
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

    -- Bleeding: an open (cutting) wound that is actually seeping. Two
    -- gates, matching what bleedRateFor actually squares for live blood
    -- loss: (1) EFFECTIVE severity (severityEffective = max of acute trauma
    -- and the necrosis floor) so an open necrotic wound with little acute
    -- trauma left still counts; (2) the live seep fraction (1 − clot) ×
    -- bandage, so a clotted or dressed-shut wound doesn't, however severe.
    -- Together they avoid both the closed-wound false positive and the
    -- necrotic-wound false negative.
    local ws = unit.getWounds(uid)
    if type(ws) == "table" then
        for _, w in ipairs(ws) do
            local effSev = w.severityEffective or w.severity or 0
            local seep = (1 - (w.clot or 0)) * (w.bandage or 1)
            if (w.kind == "slash" or w.kind == "stab")
               and effSev >= 0.2 and seep > 0.02 then
                out[#out + 1] = { name = "Bleeding", icon = "blood",
                    hint = "Losing blood from open wounds.\n"
                        .. "Bleeds out at 0 blood; revives once it recovers." }
                break
            end
        end
    end

    -- Standing daily-need warnings.
    if pose ~= "collapsed" then
        if lowFrac(uid, "hydration", "max_hydration", 0.25) then
            out[#out + 1] = { name = "Dehydrated", icon = "hydration",
                hint = "Water below 25%. Collapses near empty; find a water source." }
        end
        local cal = stats.get(uid, "calories")
        if cal and cal <= 0 then
            out[#out + 1] = { name = "Starving", icon = "hunger",
                hint = "Out of energy — burning fat then muscle reserves to survive." }
        end
    end

    return out
end
M.unitConditions = unitConditions

-- Carry row: carried weight / effective capacity. The value tooltip
-- surfaces the capacity breakdown when modifiers are active — the
-- technomule's "cybernetic enhancements +50%" shows here.
local function fmtCarry(uid)
    local cap = unit.getStat(uid, "carrying_capacity")
    if not cap then return nil end
    local carried = unit.getCarryingWeight(uid) or 0
    return string.format("%.1f / %.1f kg", carried, cap)
end

-- Danger colour-coding for the vital numbers: amber as they enter the
-- warning band, red at critical. Returns white when safe (so the refresh
-- restores it as values recover).
local VITAL_CRIT_COLOR = { 1.0, 0.30, 0.30, 1.0 }
local VITAL_WARN_COLOR = { 1.0, 0.62, 0.25, 1.0 }
local function dangerColor(frac, crit, warn)
    if frac == nil  then return L.CONTENT_VAL_COLOR end
    if frac <= crit then return VITAL_CRIT_COLOR  end
    if frac <= warn then return VITAL_WARN_COLOR  end
    return L.CONTENT_VAL_COLOR
end
local function bloodColorFn(uid)
    local b = unit.getBlood(uid)
    if not (b and b.max and b.max > 0) then return L.CONTENT_VAL_COLOR end
    return dangerColor(b.current / b.max, 0.30, 0.50)
end
local function painColorFn(uid)
    local p = unit.getPain(uid) or 0
    if p >= 0.70 then return VITAL_CRIT_COLOR end
    if p >= 0.40 then return VITAL_WARN_COLOR end
    return L.CONTENT_VAL_COLOR
end
local function fracColorFn(cur, maxName, crit, warn)
    return function(uid)
        local c = stats.get(uid, cur)
        local m = stats.get(uid, maxName)
        if not (c and m and m > 0) then return L.CONTENT_VAL_COLOR end
        return dangerColor(c / m, crit, warn)
    end
end
local function sleepPressureColorFn(uid)
    local c, m = stats.get(uid, "sleep_pressure"), stats.get(uid, "max_sleep_pressure"); return (c and m and m > 0) and dangerColor(1 - c / m, 0.10, 0.30) or L.CONTENT_VAL_COLOR end

-- Blood value tooltip: the live BLEED rate in ml/s (with %/s of total blood
-- in parens). engine.getBlood exposes bleedRate (L/s) summed over wounds.
local function bloodValueTooltip(uid)
    local b = unit.getBlood(uid)
    if not b then return nil end
    local rate = b.bleedRate or 0
    if rate <= 0 then
        return { text = "Blood", hint = "Not bleeding." }
    end
    local pctS = (b.max and b.max > 0) and (rate / b.max * 100) or 0
    return { text = "Blood",
             hint = string.format("Bleeding: %.0f ml/s (%.2f%%/s)",
                 rate * 1000, pctS) }
end

-- Status panel: the unit's whole current state in one scrollable list —
-- vitals (most-likely-to-kill first), then daily-need resources, then
-- the carry load, then any active CONDITIONS (knocked down, collapsed,
-- bleeding…) and finally the full INJURY list (icon + name + severity).
-- Conditions + injuries are dynamic rows appended to the fixed vitals,
-- each rendered icon-left / text-right by the shared row builder (its
-- `key` doubles as the icon basename, so an injury's catalog icon shows
-- without a STAT_DEFS entry). This is the one place to see why a unit
-- can or can't act.
function M.buildStatusPanel(rect, uid)
    local rows = {
        { key = "blood",     value = fmtBlood, colorFn = bloodColorFn,
          valueTooltip = bloodValueTooltip },
        { key = "pain",      value = fmtPain,  colorFn = painColorFn },
        { key = "body_temp", value = fmtBodyTemp, colorFn = bodyTempColor },
        { key = "heart_rate", value = fmtHeartRate, colorFn = heartRateColor },
        { key = "blood_oxygen", value = fmtBloodOxygen, colorFn = bloodOxygenColor },
        { key = "consciousness", value = fmtConsciousness, colorFn = consciousnessColor },
        { key = "circulation", value = fmtCirculation, colorFn = circulationColor },
        { key = "salt",      value = fmtSalt, colorFn = saltColor },
        { key = "stamina",   value = function(u) return fmtCurMax(u, "stamina",   "max_stamina")   end,
          colorFn = fracColorFn("stamina",   "max_stamina",   0.10, 0.30) },
        { key = "hunger",    value = function(u) return fmtCurMax(u, "hunger",    "max_hunger")    end,
          colorFn = fracColorFn("hunger",    "max_hunger",    0.05, 0.25) },
        { key = "calories",  value = function(u) return fmtCurMax(u, "calories",  "max_calories")  end,
          colorFn = fracColorFn("calories",  "max_calories",  0.05, 0.25) },
        { key = "hydration", value = function(u) return fmtCurMax(u, "hydration", "max_hydration") end,
          colorFn = fracColorFn("hydration", "max_hydration", 0.10, 0.25) },
        { key = "sleep_pressure", value = function(u) return fmtCurMax(u, "sleep_pressure", "max_sleep_pressure") end,
          colorFn = sleepPressureColorFn },   -- inverted: HIGH means needs sleep
        { key          = "carrying_capacity",
          value        = fmtCarry,
          valueTooltip = statDefs.carryValueTooltip },
    }

    -- Conditions (states): icon + small amber name. Captured per-build;
    -- the panel rebuilds when the condition/injury set changes (panelShapeSig).
    local CONDITION_COLOR = { 1.0, 0.62, 0.25, 1.0 }
    for _, c in ipairs(unitConditions(uid)) do
        local name = c.name
        local tt   = { text = name, hint = c.hint or "Current condition." }
        rows[#rows + 1] = {
            key          = c.icon,
            value        = function() return name end,
            opts         = { fontSize = L.CONDITION_FONT_SIZE,
                             color = CONDITION_COLOR,
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Injuries (wounds): icon + small name, COLOUR-coded by severity, with
    -- the severity word + effects in the tooltip. Identical wounds STACK
    -- with a (xN) multiplier — consistent with item stacking — so a flurry
    -- of small cuts reads "Cut (x6)" instead of six rows. Wounds of
    -- different severity carry different tiered names, so they don't merge;
    -- only truly-identical (mostly minor) ones do. Worst in a group drives
    -- the colour.
    local groups, order = {}, {}
    for _, inj in ipairs(injuries.list(uid)) do   -- list is worst-first
        local g = groups[inj.name]
        if not g then
            g = { icon = inj.icon, count = 0, worst = inj }
            groups[inj.name] = g
            order[#order + 1] = inj.name
        end
        g.count = g.count + 1
        if (inj.severity or 0) > (g.worst.severity or 0) then g.worst = inj end
    end
    for _, nm in ipairs(order) do
        local g    = groups[nm]
        local inj  = g.worst
        local disp = (g.count > 1) and (nm .. " (x" .. g.count .. ")") or nm
        local pct     = string.format("%d%%",
                            math.floor((inj.severity or 0) * 100 + 0.5))
        local sevWord = injuries.severityLabel(inj.severity)
                            :gsub("^%l", string.upper)
        local loc     = injuries.locationName(inj.part):gsub("^%l", string.upper)
        if g.count > 1 then loc = loc .. " + " .. (g.count - 1) .. " more" end
        local hint = "Location: " .. loc
                     .. "\nSeverity: " .. sevWord .. " (" .. pct .. ")"
                     .. (inj.kind == "severed" and "\nPermanent."
                                                or  "\nHeals over time.")
        local effects = injuries.effects(inj.kind, inj.part, inj.severity,
                                         inj.infection)
        if #effects > 0 then
            hint = hint .. "\n\nEffects:\n• " .. table.concat(effects, "\n• ")
        end
        -- Dressing: a bandaged wound seeps only a fraction of its
        -- natural bleed (0% = sealed). Report it so the player sees
        -- how good the first-aid was.
        if inj.bandaged then
            local seepPct = math.floor((inj.seep or 0) * 100 + 0.5)
            local quality = (seepPct <= 0) and "Bleeding stopped"
                            or string.format("Bleeding cut to %d%%", seepPct)
            local kind = (inj.dressing == "tourniquet")
                         and "Makeshift tourniquet" or "Dressing"
            hint = hint .. "\n\n" .. kind .. ": " .. quality
        end
        -- Clotting progress (0 = open, 1.0 = clotted shut). Skip for
        -- kinds that don't externally bleed.
        local clotKind = inj.kind ~= "concussion" and inj.kind ~= "fracture"
        if clotKind then
            local clotPct = math.floor((inj.clot or 0) * 100 + 0.5)
            local clotLine = (clotPct >= 100) and "Clotted"
                             or string.format("Clotting: %d%%", clotPct)
            hint = hint .. "\nWound: " .. clotLine
        end
        local tt = { text = disp, hint = hint }
        rows[#rows + 1] = {
            key          = g.icon,
            kind         = "injury",
            value        = function() return disp end,
            opts         = { fontSize = L.CONDITION_FONT_SIZE,
                             color = injuries.severityColor(inj.severity),
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Infections: their own section, below injuries. An infected wound shows
    -- the specific bug (e.g. "Gas gangrene (left thigh)") + its level, colour-
    -- coded by how bad it is. Cured by antibiotics; cleared by the immune
    -- response over time (which then leaves immunity, below the scars).
    for _, inf in ipairs(injuries.infectionList(uid)) do
        local pct = math.floor((inf.level or 0) * 100 + 0.5)
        local col = (inf.level >= 0.7) and { 1.00, 0.35, 0.35, 1.0 }   -- red
                 or (inf.level >= 0.4) and { 1.00, 0.55, 0.20, 1.0 }   -- orange
                 or { 0.95, 0.80, 0.45, 1.0 }                          -- amber
        local hint = inf.name .. "\nInfection level: " .. pct .. "%"
                     .. ((inf.category == "bacterial")
                         and "\nTreatable with antibiotics."
                         or  "\nThe immune system must fight this off.")
        local disp = inf.name .. " " .. pct .. "%"
        local tt = { text = inf.name, hint = hint }
        rows[#rows + 1] = {
            key          = inf.icon,
            kind         = "infection",
            value        = function() return disp end,
            opts         = { fontSize = L.CONDITION_FONT_SIZE, color = col,
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Scars: permanent marks from healed severe wounds. Dim grey,
    -- below the active injuries, descriptive only.
    for _, sc in ipairs(injuries.scarList(uid)) do
        local loc = injuries.locationName(sc.part):gsub("^%l", string.upper)
        local hint = "Location: " .. loc
                     .. "\nA permanent scar from a healed "
                     .. (sc.kind or "wound") .. "."
        local tt = { text = sc.name, hint = hint }
        rows[#rows + 1] = {
            key          = sc.icon,
            kind         = "injury",
            value        = function() return sc.name end,
            opts         = { fontSize = L.CONDITION_FONT_SIZE,
                             color = { 0.55, 0.55, 0.55, 1.0 },
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Immunities: acquired resistance from surviving infections. Bottom of
    -- the panel, dim blue-green, descriptive. Fades very slowly.
    for _, im in ipairs(unit.getImmunities(uid) or {}) do
        local pct = math.floor((im.level or 0) * 100 + 0.5)
        local disp = "Immunity (" .. im.name .. ") " .. pct .. "%"
        local hint = "Acquired immunity to " .. im.name
                     .. "\nStrength: " .. pct .. "%"
                     .. "\nResists re-infection; fades slowly over time."
        local tt = { text = disp, hint = hint }
        rows[#rows + 1] = {
            key          = im.icon,   -- "immunity"
            value        = function() return disp end,
            opts         = { fontSize = L.CONDITION_FONT_SIZE,
                             color = { 0.55, 0.80, 0.80, 1.0 },
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    return panelEngine.buildIconStatPanel(rect, uid, rows, "status")
end

return M
