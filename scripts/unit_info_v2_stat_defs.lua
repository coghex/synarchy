-- Unit info v2 stat metadata + live-value formatting (#542 split from
-- unit_info_v2.lua).
--
-- Per-stat name, icon basename, and tooltip description (STAT_DEFS),
-- plus the formatters that turn a live stat + its active modifiers
-- into the "base (+bonus)" row text and the modifier-breakdown value
-- tooltip. Shared by the Physical/Mental panels (statRow) and the
-- Status panel (carryValueTooltip) — see unit_info_v2_panels.lua and
-- unit_info_v2_status.lua.

local M = {}

-- Format a number for stat display: "?" when nil, 2 decimal places.
function M.fmtNum(x)
    if x == nil then return "?" end
    return string.format("%.2f", x)
end

-- Format a "cur / max" pair: "0 / 10" with stat lookup. Returns "?"
-- when either value is missing. Uses the unit_stats wrapper so derived
-- stats (e.g. max_stamina = endurance * 10) resolve through the same
-- call as raw attributes.
function M.fmtCurMax(uid, curName, maxName)
    local stats = require("scripts.unit_stats")
    local cur = stats.get(uid, curName)
    local mx  = stats.get(uid, maxName)
    if cur == nil or mx == nil then return "?" end
    return string.format("%.1f / %.1f", cur, mx)
end

-- True when a stat's current/max ratio is below `frac`. Shared by the
-- Status panel's collapse-condition detection and its daily-need
-- warning rows.
function M.lowFrac(uid, cur, maxName, frac)
    local stats = require("scripts.unit_stats")
    local c = stats.get(uid, cur)
    local m = stats.get(uid, maxName)
    return c and m and m > 0 and (c / m < frac)
end

-----------------------------------------------------------
-- Stat metadata
--
-- Per-stat name, icon basename (file in assets/textures/icons/),
-- and a one-line description shown as the tooltip hint. Hover the icon
-- to see name + description; the value text on the right is the live
-- number. Add new stats by appending here — the panels look entries
-- up by key, so the same key flows into icon/tooltip/value lookups.
-----------------------------------------------------------

local STAT_DEFS = {
    -- Status (current-state resources + derived feedback)
    stamina      = { icon = "stamina",      name = "Stamina",
        desc = "Drives sustained physical effort. Drops with action, regenerates with rest." },
    hunger       = { icon = "hunger",       name = "Hunger",
        desc = "Stomach fullness (kcal of undigested food). Fills when the unit eats; drains as digestion feeds the calorie store. Low hunger triggers auto-eating." },
    calories     = { icon = "hunger",       name = "Calories",
        desc = "Energy store (kcal available to spend). Fed by digestion, drained by metabolism and activity. At 0 the body catabolizes fat, then muscle — starvation." },
    hydration    = { icon = "hydration",    name = "Hydration",
        desc = "Need for water. Drops faster than hunger; critical in hot climates." },
    blood        = { icon = "blood",        name = "Blood",
        desc = "Blood volume in litres. Drained by bleeding wounds; below 30% triggers unconsciousness, ≤0 means death." },
    pain         = { icon = "pain",         name = "Pain",
        desc = "Accumulated pain from wounds (severity weighted by attack kind). High pain penalises hit chance and evasion." },
    body_temp    = { icon = "body_temp",    name = "Body Temp",
        desc = "Core body temperature (°C). Driven by the environment, the body's heat production, and insulation. Far from 37°C means hypothermia (cold) or heat stroke (hot)." },
    circulation  = { icon = "circulation",  name = "Circulation",
        desc = "How well the body perfuses blood to its tissues. Lowered by blood loss, obesity, frailty, sickness, and cold (vasoconstriction). Poor circulation freezes the extremities first." },
    salt         = { icon = "salt",         name = "Salt",
        desc = "Electrolyte (sodium) balance, as % of ideal. Sweat and water dilute it (cramps, hyponatremia); dehydration concentrates it (hypernatremia). Food restores salt — the kidneys can't make it. Both extremes are dangerous." },
    heart_rate   = { icon = "heart_rate",   name = "Heart Rate",
        desc = "Beats per minute. Rises with exertion, heat, blood loss, and low oxygen (the body compensating); cold slows it (bradycardia). Drives circulation." },
    blood_oxygen = { icon = "blood_oxygen", name = "Blood O₂",
        desc = "Blood oxygen saturation. Lung damage, massive blood loss, or a cold-slowed heart starve the blood of oxygen → hypoxia → suffocation." },
    consciousness = { icon = "consciousness", name = "Consciousness",
        desc = "Mental clarity, set by the worst of: temperature extremes, low blood oxygen, salt imbalance. Falling → confused → delirious (stumbles) → unconscious (collapses)." },
    carrying_capacity = { icon = "weight",  name = "Carry Load",
        desc = "Carried weight vs carrying capacity (from muscle mass and strength). Includes worn gear and container contents; over-capacity units refuse to pick anything else up." },

    -- Physical stats (rolled at spawn)
    strength     = { icon = "strength",     name = "Strength",
        desc = "Affects melee damage, carry capacity, and heavy-tool work speed." },
    endurance    = { icon = "endurance",    name = "Endurance",
        desc = "Sets the ceiling for stamina and slows its drain under load." },
    reflexes     = { icon = "reflexes",     name = "Reflexes",
        desc = "Affects dodge, parry, and the reaction window for ranged attacks." },
    constitution = { icon = "constitution", name = "Constitution",
        desc = "Slows bleeding from wounds and accelerates natural healing." },
    metabolism   = { icon = "metabolism",   name = "Metabolism",
        desc = "How quickly the unit burns calories. High metabolism eats more but recovers faster." },
    toughness    = { icon = "toughness",    name = "Toughness",
        desc = "Flat damage reduction on all incoming hits. Caps at 50% at toughness 10." },
    dexterity    = { icon = "dexterity",    name = "Dexterity",
        desc = "Fine motor control. Drives aim on offense and parry on defense." },
    agility      = { icon = "agility",      name = "Agility",
        desc = "Whole-body motion. Drives dodging incoming attacks." },
    height       = { icon = "height",       name = "Height",
        desc = "Affects reach, line of sight, and the cap for skeletal lean mass." },
    weight       = { icon = "weight",       name = "Weight",
        desc = "Total body mass. Heavier units move slower and apply more force in melee." },

    -- Mental stats
    perception   = { icon = "perception",   name = "Perception",
        desc = "Sight range, hearing, and chance to spot hidden things. Also helps spot incoming attacks." },
    intelligence = { icon = "intelligence", name = "Intelligence",
        desc = "Tactical decision-making. Smarter units pick vital low-resistance targets; dumber ones swing more randomly." },

    -- Skills (weapon classes + balance). All grow with XP.
    balance      = { icon = "balance",      name = "Balance",
        desc = "Footing on uneven terrain. Reduces falls, slips, and stagger from impacts; contributes to dodge." },
    dodge        = { icon = "agility",      name = "Dodge",
        desc = "Active evasion — slipping a blow you SEE coming. Only works against attacks you're aware of (in view, facing them); useless against an ambush from behind. Chance scales with agility and this skill; telegraphed lunges are easier to dodge." },
    dagger       = { icon = "dagger",       name = "Dagger",
        desc = "Skill with daggers and other short blades. Improves hit chance and damage with that weapon class." },
    unarmed      = { icon = "unarmed",      name = "Unarmed",
        desc = "Skill at fighting without a weapon — fists, claws, fangs. Used by all natural-weapon creatures (bears) plus unarmed humanoids." },
    -- Leap/grapple skills (combat: lunging + clinging). LEARNED proficiency;
    -- effective capability blends with the STATS noted — the skill/stat split,
    -- so a strong, agile novice still out-leaps a frail expert. Icons reuse
    -- agility/strength until dedicated art exists.
    jumping      = { icon = "agility",      name = "Jumping",
        desc = "Technique for leaping — gap-closing lunges that turn body momentum into a strike. Effective leap range and speed scale with the agility and strength stats." },
    grappling    = { icon = "strength",     name = "Grappling",
        desc = "Technique for grabbing and clinging — latching a grasping part (jaws, claws, hands) onto an enemy and holding through their thrashing. Scales with strength and dexterity." },
}
M.STAT_DEFS = STAT_DEFS

-----------------------------------------------------------
-- Live modifier-aware formatting
-----------------------------------------------------------

-- Sum a stat's active modifiers into (deltaSum, percentSum) and the
-- effective total: (base + deltaSum) * (1 + percentSum). Mirrors the
-- engine's effectiveStat composition so the panel never disagrees
-- with unit.getStat.
local function statModifierTotals(uid, statName, base)
    local mods = unit.getModifiers(uid, statName) or {}
    local delta, percent = 0, 0
    for _, m in ipairs(mods) do
        delta   = delta   + (m.delta or 0)
        percent = percent + (m.percent or 0)
    end
    return mods, delta, percent, (base + delta) * (1 + percent)
end

-- Format a stat as "base (+bonus)" when modifiers are active for that
-- stat, else just "base". Bonus = effective - base, so percentage
-- modifiers show up too. Reads live each tick so accessory
-- equip/unequip updates the display without a panel rebuild.
local function fmtStatLive(uid, statName)
    local base = unit.getStatBase(uid, statName)
    if base == nil then return nil end
    local _, _, _, effective = statModifierTotals(uid, statName, base)
    local bonus = effective - base
    if math.abs(bonus) < 0.005 then
        return string.format("%.2f", base)
    end
    return string.format("%.2f (%+.2f)", base, bonus)
end

-- Build the icon tooltip for a stat row — name + base description.
-- Modifier sources don't go here; the player can find them on the
-- value tooltip (statValueTooltip).
local function statHoverTooltip(_uid, statKey)
    local def = STAT_DEFS[statKey]
    if not def then return nil end
    return { text = def.name, hint = def.desc }
end

-- Build the value tooltip for a stat row — title is the *effective*
-- total (base + sum of bonuses), hint multilines each active modifier
-- ("Technogoggles + 0.85"). The number shown in the row is the base
-- with the bonus in parens, so the tooltip title adds new info
-- (the total) rather than just repeating what's already on screen.
-- One hint line per modifier: "cybernetic enhancements +50%" for
-- percentage mods, "Technogoggles +0.85" for flat deltas, both parts
-- for mixed mods.
local function modifierLines(mods)
    local lines = {}
    for _, m in ipairs(mods) do
        local pct = m.percent or 0
        if math.abs(pct) >= 0.0005 then
            if math.abs(m.delta or 0) >= 0.005 then
                lines[#lines + 1] = string.format("%s %+.2f %+d%%",
                    m.source or "?", m.delta, pct * 100)
            else
                lines[#lines + 1] = string.format("%s %+d%%",
                    m.source or "?", pct * 100)
            end
        else
            lines[#lines + 1] = string.format("%s %+.2f",
                m.source or "?", m.delta or 0)
        end
    end
    return lines
end

local function statValueTooltip(uid, statKey)
    local base = unit.getStatBase(uid, statKey)
    if base == nil then return nil end
    local mods, _, _, effective = statModifierTotals(uid, statKey, base)
    if #mods == 0 then
        -- Nothing to explain; suppress the tooltip entirely so a
        -- bonus-less stat doesn't show an empty popup.
        return nil
    end
    return {
        text = string.format("%.2f", effective),
        hint = table.concat(modifierLines(mods), "\n"),
    }
end

-- Carry-load value tooltip: the panel row already shows the effective
-- total, so the tooltip shows where it came from — body-derived base
-- with the modifier bonus in absolute terms: "167.0 (+83.5)".
function M.carryValueTooltip(uid)
    local base = unit.getStatBase(uid, "carrying_capacity")
    if base == nil then return nil end
    local mods, _, _, effective =
        statModifierTotals(uid, "carrying_capacity", base)
    if #mods == 0 then return nil end
    return {
        text = string.format("%.1f (%+.1f)", base, effective - base),
        hint = table.concat(modifierLines(mods), "\n"),
    }
end

-- Row spec helper for engine-side stats. Icon tooltip = stat name +
-- description (no modifier sources). Value tooltip = base value as
-- title + modifier source list as hint — appears only when there are
-- active modifiers, so unmodified stats hover plainly.
function M.statRow(key)
    return {
        key          = key,
        value        = function(u) return fmtStatLive(u, key) end,
        tooltip      = function(u) return statHoverTooltip(u, key) end,
        valueTooltip = function(u) return statValueTooltip(u, key) end,
    }
end

return M
