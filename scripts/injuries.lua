-- Injury catalog + helpers.
--
-- The ENGINE stamps wounds as { part, kind, severity } where kind is a
-- broad category ("fracture", "concussion", "slash", "stab", "blunt").
-- This module is the data-side layer that turns a (kind, part) pair into
-- a human name + icon basename for the UI, and answers the gameplay
-- questions injuries drive: how badly is the unit's locomotion impaired,
-- is it incapacitated, and have its injuries become lethal.
--
-- It's pure/stateless — every function reads live wound data via
-- unit.getWounds(uid). Extend INJURY_NAMES / icons freely; an unknown
-- (kind, part) falls back to a sensible generic.

local M = {}

-- DESIGN — heart-only vitality (do NOT add brain/neck/spine here):
-- instant death keys off the engine's per-part `bpVital` flag, which
-- `unit.getWounds` ships as `w.vital`. For the acolyte ONLY the heart is
-- vital — destroying the brain, severing the neck, etc. is SURVIVED for a
-- few seconds (the treatment-window conceit) and then kills via the delayed
-- failure meters (neuro/shock/suffocation/organ in unit_resources.lua),
-- not instantly. This table is a heart-only FALLBACK for the rare caller
-- whose wound carries no `w.vital` (the engine normally always sets it, so
-- it's effectively belt-and-braces). Other body plans (a robot) may flag
-- several parts vital — that lives in their YAML, not here.
local VITAL_PARTS = {
    heart = true,
}

-- Severity-tiered flavour names, keyed "kind|token". Each entry is a
-- list of { minSeverity, name } from HIGHEST minSeverity down; M.name
-- picks the first whose minSeverity ≤ the wound's severity. This is how
-- the same wound kind escalates by force: a head fracture reads "Skull
-- fracture" → "Shattered skull" → "Crushed skull" (fatal) as severity
-- climbs. Tokens strip the l_/r_ side prefix (l_leg → leg).
local INJURY_TIERS = {
    -- head/neck SUBPARTS (allocated targets)
    ["concussion|brain"] = {
        { 1.0, "Fatal brain trauma" }, { 0.6, "Severe concussion" },
        { 0.0, "Concussion" } },
    ["internal|brain"] = {
        { 1.0, "Fatal brain trauma" }, { 0.5, "Severe brain trauma" },
        { 0.0, "Brain trauma" } },
    ["fracture|skull"] = {
        { 1.0, "Crushed skull" }, { 0.8, "Shattered skull" },
        { 0.0, "Skull fracture" } },
    ["fracture|jaw"] = {
        { 0.8, "Shattered jaw" }, { 0.0, "Broken jaw" } },
    ["fracture|windpipe"] = {
        { 0.6, "Crushed windpipe" }, { 0.0, "Bruised windpipe" } },
    ["arterial|carotid"] = {
        { 0.6, "Severed carotid artery" }, { 0.0, "Nicked carotid artery" } },
    ["fracture|cervical_spine"] = {
        { 0.8, "Broken neck" }, { 0.0, "Fractured vertebra" } },
    ["concussion|cervical_spine"] = {
        { 0.6, "Severe spinal cord damage" }, { 0.0, "Spinal cord damage" } },
    ["internal|cervical_spine"] = {
        { 0.6, "Severe spinal cord damage" }, { 0.0, "Spinal cord damage" } },
    -- head/neck MACRO fallbacks (a weak hit that didn't reach a subpart)
    ["concussion|head"] = {
        { 1.0, "Fatal brain trauma" }, { 0.6, "Severe concussion" },
        { 0.0, "Concussion" } },
    ["fracture|head"] = {
        { 1.0, "Crushed skull" }, { 0.8, "Shattered skull" },
        { 0.0, "Skull fracture" } },
    ["fracture|neck"] = {
        { 0.8, "Broken neck" }, { 0.0, "Cervical fracture" } },
    ["arterial|"] = {
        { 0.6, "Arterial bleeding" }, { 0.0, "Nicked artery" } },
    ["fracture|torso"] = {
        { 1.0, "Crushed ribcage" }, { 0.6, "Broken ribs" },
        { 0.0, "Cracked ribs" } },
    ["fracture|leg"] = {
        { 0.8, "Shattered leg" }, { 0.0, "Fractured fibula" } },
    ["fracture|foot"] = {
        { 0.8, "Shattered ankle" }, { 0.0, "Fractured ankle" } },
    ["fracture|hand"] = {
        { 0.8, "Shattered wrist" }, { 0.0, "Fractured wrist" } },
    ["fracture|arm"] = {
        { 0.8, "Shattered arm" }, { 0.0, "Fractured arm" } },
    ["internal|"] = {
        { 1.0, "Massive internal trauma" }, { 0.6, "Severe internal bleeding" },
        { 0.0, "Internal bleeding" } },
    -- limb SUB-LIMB subparts (keyed by the subpart id token: thigh/shin/…)
    ["fracture|thigh"]   = { { 0.8, "Shattered femur" },   { 0.0, "Fractured femur" } },
    ["fracture|shin"]    = { { 0.8, "Shattered shin" },    { 0.0, "Fractured tibia" } },
    ["fracture|knee"]    = { { 0.0, "Fractured kneecap" } },
    ["fracture|bicep"]   = { { 0.8, "Shattered humerus" }, { 0.0, "Fractured humerus" } },
    ["fracture|forearm"] = { { 0.8, "Shattered forearm" }, { 0.0, "Fractured forearm" } },
    ["fracture|palm"]    = { { 0.0, "Fractured hand" } },
    ["fracture|sole"]    = { { 0.8, "Shattered foot" },    { 0.0, "Fractured foot" } },
    ["fracture|thumb"]  = { { 0.0, "Broken thumb" } },
    ["fracture|index"]  = { { 0.0, "Broken finger" } },
    ["fracture|middle"] = { { 0.0, "Broken finger" } },
    ["fracture|ring"]   = { { 0.0, "Broken finger" } },
    ["fracture|pinky"]  = { { 0.0, "Broken finger" } },
    ["fracture|bigtoe"]    = { { 0.0, "Broken toe" } },
    ["fracture|indextoe"]  = { { 0.0, "Broken toe" } },
    ["fracture|middletoe"] = { { 0.0, "Broken toe" } },
    ["fracture|ringtoe"]   = { { 0.0, "Broken toe" } },
    ["fracture|pinkytoe"]  = { { 0.0, "Broken toe" } },
    -- torso bone SUBPARTS
    ["fracture|ribcage"]  = { { 1.0, "Crushed ribcage" }, { 0.6, "Broken ribs" }, { 0.0, "Cracked ribs" } },
    ["fracture|sternum"]  = { { 0.0, "Broken sternum" } },
    ["fracture|pelvis"]   = { { 0.8, "Shattered pelvis" }, { 0.0, "Fractured pelvis" } },
    ["fracture|thoracic_spine"] = { { 0.8, "Broken back" }, { 0.0, "Fractured vertebra" } },
    ["fracture|lumbar_spine"]   = { { 0.8, "Broken back" }, { 0.0, "Fractured vertebra" } },
    ["concussion|thoracic_spine"] = { { 0.0, "Spinal cord damage" } },
    ["concussion|lumbar_spine"]   = { { 0.0, "Spinal cord damage" } },
    -- torso ORGAN SUBPARTS (internal trauma, tiered)
    ["internal|heart"]      = { { 1.0, "Heart destroyed" }, { 0.0, "Heart trauma" } },
    ["internal|lungs"]      = { { 0.6, "Collapsed lung" }, { 0.0, "Punctured lung" } },
    ["internal|liver"]      = { { 0.6, "Severe liver trauma" }, { 0.0, "Liver trauma" } },
    ["internal|stomach"]    = { { 0.0, "Ruptured stomach" } },
    ["internal|intestines"] = { { 0.6, "Severe gut trauma" }, { 0.0, "Gut wound" } },
    ["internal|kidneys"]    = { { 0.0, "Kidney trauma" } },
    ["arterial|aorta"]      = { { 0.6, "Severed aorta" }, { 0.0, "Nicked aorta" } },
    ["severed|"] = { { 0.0, "Severed" } },   -- name() appends the part
    ["slash|"] = { { 0.5, "Deep laceration" }, { 0.0, "Laceration" } },
    ["stab|"]  = { { 0.5, "Deep puncture" },   { 0.0, "Puncture wound" } },
    ["blunt|"] = { { 0.5, "Deep contusion" },  { 0.0, "Bruise" } },
}

-- Icon basename (assets/textures/icons/<name>.png). Per-kind default,
-- refined per body region by INJURY_ICON below. Missing icons degrade to
-- a dim text label in the panel, so art can land later.
local KIND_ICON = {
    fracture   = "broken_bone",
    concussion = "concussion",
    internal   = "internal_bleeding",
    severed    = "severed",
    slash      = "cut_injury",
    stab       = "puncture_injury",
    blunt      = "bruise",
    frostbite  = "frostbite",
}

-- Region-specific icon overrides, keyed like INJURY_TIERS ("kind|token").
-- Lets an ankle/wrist break show a joint icon and a broken neck a spinal
-- icon, reusing the existing injury-icon set. Falls back to KIND_ICON.
local INJURY_ICON = {
    ["fracture|foot"] = "joint_injury",    -- ankle
    ["fracture|hand"] = "joint_injury",    -- wrist
    ["fracture|neck"] = "spinal_injury",
}

local function tokenOf(part)
    -- Strip a leading side prefix ("l_leg" -> "leg") and return the bare
    -- token used for name lookup.
    part = part or ""
    return (part:gsub("^[lr]_", ""))
end

-- Human-readable injury LOCATION: side + token, with the awkward tokens
-- spelled out. "l_thigh" → "left thigh", "r_index" → "right index finger".
local LOCATION_NICE = {
    index = "index finger", middle = "middle finger",
    ring = "ring finger",  pinky  = "little finger",
    bigtoe = "big toe", indextoe = "second toe", middletoe = "middle toe",
    ringtoe = "fourth toe", pinkytoe = "little toe",
    cervical_spine = "neck", thoracic_spine = "upper spine",
    lumbar_spine = "lower spine",
    fore_leg = "foreleg", hind_leg = "hind leg",
}
function M.locationName(part)
    part = part or ""
    local p2, side = part:sub(1, 2), ""
    if     p2 == "l_" then side = "left "
    elseif p2 == "r_" then side = "right " end
    local tok = tokenOf(part)
    return side .. (LOCATION_NICE[tok] or tok)
end

-- Pick the tiered name for (kind, token) at this severity.
local function tieredName(kind, tok, sev)
    local tiers = INJURY_TIERS[kind .. "|" .. tok] or INJURY_TIERS[kind .. "|"]
    if not tiers then return nil end
    for _, tier in ipairs(tiers) do
        if (sev or 0) >= tier[1] then return tier[2] end
    end
    return tiers[#tiers][2]
end

-- Display name for a wound (kind, part, severity). Severed names append
-- the human-readable part ("Severed hand"); everything else is tiered.
function M.name(kind, part, sev)
    kind = kind or "blunt"
    local tok = tokenOf(part)
    local nm = tieredName(kind, tok, sev)
    if kind == "severed" then
        return "Severed " .. tok:gsub("_", " ")
    end
    return nm or (kind:sub(1,1):upper() .. kind:sub(2) .. " (" .. tok .. ")")
end

function M.icon(kind, part)
    kind = kind or "blunt"
    local tok = tokenOf(part)
    return INJURY_ICON[kind .. "|" .. tok]
        or KIND_ICON[kind]
        or "pain"   -- last-resort existing icon (unknown kind)
end

-- Severity qualifier word.
function M.severityLabel(sev)
    sev = sev or 0
    if sev >= 1.0  then return "critical" end
    if sev >= 0.6  then return "severe"   end
    if sev >= 0.3  then return "moderate" end
    return "minor"
end

-- Severity → row text colour (so the panel COLOUR-codes severity instead
-- of spelling out "moderate"/"critical"; the word moves to the tooltip).
function M.severityColor(sev)
    sev = sev or 0
    if sev >= 1.0 then return { 1.00, 0.30, 0.30, 1.0 } end   -- critical: red
    if sev >= 0.6 then return { 1.00, 0.55, 0.20, 1.0 } end   -- severe:   orange
    if sev >= 0.3 then return { 1.00, 0.85, 0.30, 1.0 } end   -- moderate: yellow
    return { 0.75, 0.85, 0.70, 1.0 }                           -- minor:    pale green
end

-- The functional consequences of a wound, as short tooltip bullet lines.
-- Derived from kind × body region × severity — mirrors the gameplay rules
-- in speedMultiplier / isIncapacitated / Combat.Wounds / the failure meters.
function M.effects(kind, part, sev, infection)
    sev = sev or 0
    local tok = tokenOf(part)
    local e   = {}
    local inf = infection or 0
    if inf >= 0.4 then e[#e + 1] = "Festering — risk of sepsis"
    elseif inf >= 0.1 then e[#e + 1] = "Infected" end
    local legLike = tok:find("leg") or tok:find("foot") or tok:find("thigh")
        or tok:find("shin") or tok:find("knee") or tok:find("sole")
        or tok:find("toe")
    if (kind == "fracture" or kind == "severed") and legLike then
        if sev >= 0.85 then e[#e + 1] = "Cannot walk — crawls"
        else                e[#e + 1] = "Slows movement" end
    end
    if kind == "concussion" then
        if sev >= 0.9      then e[#e + 1] = "Risk of brain death"
        elseif sev >= 0.35 then e[#e + 1] = "Can lose consciousness" end
    end
    if kind == "arterial" then e[#e + 1] = "Heavy bleeding" end
    if kind == "slash" or kind == "stab" then e[#e + 1] = "Bleeding" end
    if kind == "internal" then
        e[#e + 1] = "Internal bleeding"
        if tok == "lungs" then
            e[#e + 1] = "Impairs breathing — risk of suffocation"
        elseif tok == "brain" and sev >= 0.55 then
            e[#e + 1] = "Risk of brain death"
        elseif (tok == "liver" or tok == "kidneys" or tok == "stomach"
                or tok == "intestines") and sev >= 0.5 then
            e[#e + 1] = "Risk of organ failure"
        end
    end
    if kind == "fracture" and (tok == "windpipe" or tok == "throat") then
        e[#e + 1] = "Impairs breathing — risk of suffocation"
    end
    if kind == "severed" then
        e[#e + 1] = "Permanent loss"
        if tok == "hand" or tok == "arm" or tok == "forearm"
           or tok == "palm" then
            e[#e + 1] = "Drops held weapon"
        end
    end
    return e
end

-- A panel-ready description list, worst-first:
--   { { name, icon, severity, label, kind, part, bandaged, seep } , ... }
--
-- `w.bandage` (from unit.getWounds) is the fraction of the wound's
-- natural bleed that still seeps after a dressing — 1.0 untreated,
-- < 1.0 bandaged, 0.0 fully sealed. A treated wound reads
-- "Laceration (bandaged)" and its name carries the suffix, so a
-- bandaged and an untreated wound of the same kind stay on separate
-- rows in the Status panel.
function M.list(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return {} end
    local out = {}
    for _, w in ipairs(ws) do
        local base     = M.name(w.kind, w.part, w.severity)
        local bandaged = (w.bandage or 1) < 1
        local nec      = w.necrosis or 0
        local rowIcon  = M.icon(w.kind, w.part)
        -- Frostbite carries its OWN rot here (frostbite necrosis isn't an
        -- infection — that's why it's shown in the injuries section, not the
        -- infections one). Tier the name + swap to the rot icon as it dies.
        if w.kind == "frostbite" then
            if     nec >= 0.85 then base = "Gangrenous frostbite"; rowIcon = "rot_injury"
            elseif nec >= 0.4  then base = "Frostbite (rotting)";  rowIcon = "rot_injury"
            else                    base = "Frostbite" end
        end
        -- Dressing label: a makeshift tourniquet reads distinctly from a
        -- proper bandage (it's the crude no-supplies fallback).
        local suffix = ""
        if w.dressing == "tourniquet" then suffix = " (tourniquet)"
        elseif bandaged then suffix = " (bandaged)" end
        -- Infections are listed in their OWN Status section now (see
        -- M.infectionList) — the injury row no longer carries an "(infected)"
        -- suffix. The infection data is still attached for any consumer.
        local inf    = w.infection or 0
        out[#out + 1] = {
            name      = base .. suffix,
            icon      = rowIcon,
            severity  = w.severity or 0,
            label     = M.severityLabel(w.severity or 0),
            kind      = w.kind,
            part      = w.part,
            bandaged  = bandaged,
            dressing  = w.dressing or "",
            seep      = w.bandage or 1,
            clot      = w.clot or 0,
            necrosis  = nec,
            infection = inf,
            clean     = w.clean or false,
            infectionType = w.infectionType or "",
            infectionName = w.infectionName or "",
            infectionIcon = w.infectionIcon or "",
            infectionCategory = w.infectionCategory or "",
        }
    end
    table.sort(out, function(a, b) return a.severity > b.severity end)
    return out
end

-- Infections currently active on a unit, as their own panel-ready list
-- (worst-first): { name, icon, level, part, kind, category }. A wound is
-- listed once its infection passes a visibility floor. If the specific bug
-- isn't typed yet (no world climate), it falls back to a tiered descriptor.
local INFECTION_VIS = 0.05
function M.infectionList(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return {} end
    local out = {}
    for _, w in ipairs(ws) do
        local inf = w.infection or 0
        local nec = w.necrosis or 0
        -- List a row for an active infection, OR for infection-caused dead
        -- tissue that persists after a cure (infectionType set). Frostbite rot
        -- (kind "frostbite", no infectionType) is shown in the INJURIES section
        -- instead, so it isn't double-listed here.
        local infectionRot = nec >= 0.05 and (w.infectionType or "") ~= ""
        if inf >= INFECTION_VIS or infectionRot then
            local nm = w.infectionName
            if not nm or nm == "" then
                if     inf >= 0.7 then nm = "Suppurating infection"
                elseif inf >= 0.4 then nm = "Festering infection"
                else                   nm = "Infection" end
            end
            -- Necrosis overrides the icon (rot) and tags the name; otherwise a
            -- high infection reads as festering. Icons: rot_injury (dead
            -- tissue) > festered_injury (festering) > the bug's own icon.
            local icon, tag
            if nec >= 0.5 then
                icon, tag = "rot_injury", " — gangrenous"
            elseif nec >= 0.05 then
                icon, tag = "rot_injury", " — rotting"
            elseif inf >= 0.4 then
                icon, tag = "festered_injury", ""
            else
                icon = w.infectionIcon
                if not icon or icon == "" then icon = "bacterial_infection" end
                tag = ""
            end
            out[#out + 1] = {
                name     = nm .. tag .. " (" .. M.locationName(w.part) .. ")",
                icon     = icon,
                level    = math.max(inf, nec),
                infection = inf,
                necrosis = nec,
                part     = w.part,
                kind     = w.kind,
                category = w.infectionCategory or "",
            }
        end
    end
    table.sort(out, function(a, b) return a.level > b.level end)
    return out
end

-- Scar display name by the wound kind that left it.
local SCAR_NAMES = {
    slash    = "Scar",
    stab     = "Puncture scar",
    blunt    = "Healed contusion",
    fracture = "Healed fracture",
    arterial = "Scar",
    internal = "Healed trauma",
    burn     = "Burn scar",
}
function M.scarName(kind) return SCAR_NAMES[kind] or "Scar" end

-- Panel-ready list of a unit's permanent scars:
--   { { name, icon, kind, part, severity } , ... }
function M.scarList(uid)
    local sc = unit.getScars(uid)
    if type(sc) ~= "table" then return {} end
    local out = {}
    for _, s in ipairs(sc) do
        out[#out + 1] = {
            name     = M.scarName(s.kind),
            icon     = "scar",   -- dedicated scar icon (assets/textures/icons/injury/scar.png)
            kind     = s.kind,
            part     = s.part,
            severity = s.severity or 0,
        }
    end
    return out
end

-- Is this wound on a locomotion limb (leg or foot)? Uses the macro-part
-- rollup so a fractured femur / tibia / ankle subpart still counts as a
-- leg injury for the limp/incapacitation logic.
local function isLegWound(w)
    local m = (w.macro or w.part) or ""
    -- Match the macro rollup (l_leg/r_foot) AND the leg/foot subpart
    -- tokens directly, so detection works even if `macro` is absent.
    return m:find("leg") ~= nil or m:find("foot") ~= nil
        or m:find("thigh") ~= nil or m:find("shin") ~= nil
        or m:find("knee") ~= nil or m:find("sole") ~= nil
        or m:find("toe") ~= nil
end

-- Worst leg/foot disabling severity (fractures AND severed parts). A
-- severed foot/leg is fully disabling (severity 1.0). Drives the limp
-- speed penalty; 0 when the unit can walk freely.
function M.legFractureSeverity(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local worst = 0
    for _, w in ipairs(ws) do
        if (w.kind == "fracture" or w.kind == "severed") and isLegWound(w) then
            -- EFFECTIVE severity (max of acute trauma and necrosis floor),
            -- matching the engine's injurySpeedMult — a healed-but-necrotic
            -- leg wound keeps impairing instead of freeing the unit to walk.
            worst = math.max(worst, w.severityEffective or w.severity or 0)
        end
    end
    return worst
end

-- Active concussion severity (head injury that knocks a unit out).
function M.concussionSeverity(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local worst = 0
    for _, w in ipairs(ws) do
        if w.kind == "concussion" then
            worst = math.max(worst, w.severity or 0)
        end
    end
    return worst
end

-- Locomotion speed multiplier from leg/foot fractures: a clean unit
-- moves at 1.0; a bad break drags it down to a hobble. Below the
-- incapacitation threshold the unit can still limp; at/above it
-- M.isIncapacitated keeps it down entirely.
function M.speedMultiplier(uid)
    local s = M.legFractureSeverity(uid)
    if s <= 0 then return 1.0 end
    -- 0 -> 1.0, 0.6 -> ~0.4; floor 0.25.
    return math.max(0.25, 1.0 - s)
end

-- Concussion locomotor band — hysteresis so a unit hovering at the
-- knockout severity doesn't flap collapsed↔crawling tick-to-tick. Collapse
-- the instant the concussion reaches CONCUSSION_OUT; once down, stay down
-- until it heals back below the lower CONCUSSION_RISE (mirrors the
-- consciousness UNCONSCIOUS_BELOW / RISE_AT band in brain.lua).
local CONCUSSION_OUT  = 0.35   -- knockout (collapse) trigger
local CONCUSSION_RISE = 0.25   -- must drop below this before rising

-- Unconscious: a real concussion knocks the unit out cold (Collapsed —
-- it can't even crawl). Kept separate from cannotWalk so a lucid unit
-- with shattered legs crawls instead of lying collapsed.
function M.isUnconscious(uid)
    return M.concussionSeverity(uid) >= CONCUSSION_OUT
end

-- Rise gate for the concussion path: an already-collapsed unit may only
-- leave collapse once its concussion has healed below the rise band. The
-- gap between CONCUSSION_OUT and CONCUSSION_RISE is the hysteresis that
-- stops a wound-tick wobble around 0.35 from re-posing the unit.
function M.concussionCanRise(uid)
    return M.concussionSeverity(uid) < CONCUSSION_RISE
end

-- Pure locomotor collapse decision WITH hysteresis on the collapse↔crawl
-- boundary (#304). Inputs are pre-evaluated booleans so this stays
-- engine-free and unit-testable:
--   knockedOut — bare knockout trigger (concussion ≥ OUT, or consciousness
--                < UNCONSCIOUS_BELOW): enough to drop a unit ON ITS FEET.
--   canRise    — BOTH boundary inputs have cleared their rise band
--                (consciousness ≥ RISE_AT AND concussion < CONCUSSION_RISE):
--                enough for an ALREADY-collapsed unit to come back up.
-- A unit that is not yet collapsed collapses on the bare trigger; once
-- collapsed it stays down until canRise. Without this asymmetry both
-- directions pivot on the same threshold and a unit jittering across it
-- flaps collapsed↔crawling every tick.
function M.collapseWithHysteresis(pose, knockedOut, canRise)
    if pose == "collapsed" then return not canRise end
    return knockedOut
end

-- Can't walk: the legs/feet are too broken to stand on (two badly-broken
-- locomotion parts, or one shattered/severed one). A CONSCIOUS unit in
-- this state crawls; an unconscious one collapses.
function M.cannotWalk(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return false end
    local badLegs, worstLeg = 0, 0
    for _, w in ipairs(ws) do
        if (w.kind == "fracture" or w.kind == "severed") and isLegWound(w) then
            -- EFFECTIVE severity (acute trauma or necrosis floor) so the
            -- crawl decision stays in lockstep with the engine's movement
            -- penalty as a leg wound heals but rots.
            local sev = w.severityEffective or w.severity or 0
            worstLeg = math.max(worstLeg, sev)
            if sev >= 0.5 then badLegs = badLegs + 1 end
        end
    end
    return worstLeg >= 0.85 or badLegs >= 2
end

-- Severe enough to be off its feet (collapsed OR crawling): unconscious,
-- or legs too broken to walk. Callers that must distinguish the two use
-- isUnconscious / cannotWalk directly.
function M.isIncapacitated(uid)
    return M.isUnconscious(uid) or M.cannotWalk(uid)
end

-- If the unit has a lethal injury — a SINGLE wound on a VITAL body part
-- at severity >= 1.0 (the structure is destroyed: brain pulped, neck
-- broken through, ribcage crushed) — return a cause string for the death
-- alert; else nil. This is how a fall kills: it inflicts injuries, and a
-- catastrophic one crosses the lethal line. (Accumulating several
-- MODERATE wounds doesn't instakill — you can survive a fracture plus a
-- bruise; bleeding out from many wounds is handled separately by the
-- blood system.)
-- Death-cause scoring. Each plausible cause carries an IMMEDIACY score;
-- if a unit dies, the highest-scoring cause among its CURRENTLY-ACTIVE
-- injuries is reported (a chest impale that nicks the heart reads "a
-- ruptured heart", not "blood loss", even though it was also bleeding).
-- Deriving from live wounds == the push/compare/heal model: a healed
-- injury drops out, an empty set falls back to blood loss, the max wins.
-- EDIT the scores / phrasing to taste.
--   { match = function(w) -> bool, cause = "...", score = N }
-- evaluated top-to-bottom; a wound contributes its FIRST matching entry.
local function tok(p) return tokenOf(p) end
local DEATH_CAUSES = {
    { k="severed",   part="head",  cause="decapitation",            score=95 },
    { k="internal",  part="heart", sev=0.8, cause="a ruptured heart", score=100 },
    { k="concussion", part="brain", sev=0.9, cause="a pulverized brain", score=90 },
    { k="internal",  part="brain", sev=0.9, cause="catastrophic brain trauma", score=90 },
    { k="concussion", part="brain", sev=0.4, cause="a severe head injury", score=72 },
    { k="fracture",  part="skull", sev=0.9, cause="a crushed skull",     score=88 },
    { k="fracture",  part="skull", sev=0.5, cause="severe head trauma",  score=66 },
    { k="fracture",  part="cervical_spine", sev=0.8, cause="a broken neck", score=85 },
    { k="internal",  part="lungs",    sev=0.6, cause="suffocation",     score=70 },
    { k="fracture",  part="windpipe", sev=0.8, cause="a crushed windpipe", score=74 },
    { k="severed",   part="windpipe",          cause="suffocation",     score=74 },
    { k="internal",  part="liver", sev=0.6, cause="a ruptured liver",   score=60 },
    { k="internal",  part="kidneys",sev=0.6, cause="a ruptured kidney", score=60 },
    { k="internal",  part="stomach",   sev=0.6, cause="internal bleeding", score=55 },
    { k="internal",  part="intestines",sev=0.5, cause="internal bleeding", score=55 },
    { k="arterial",  cause="blood loss",                           score=50 },
    { k="internal",  cause="internal bleeding",                    score=52 },
}
-- Fallback when nothing specific scores (the unit just bled out).
local DEATH_FALLBACK = "blood loss"

-- 0..1 pulmonary failure — severe lung damage and/or a blocked or
-- severed airway. Drives the hypoxia failure-meter (suffocation). One
-- functioning lung oxygenates, so partial lung damage stays low; total
-- loss / crushed windpipe / severed neck reads ~1.
function M.pulmonaryFailure(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local f = 0
    for _, w in ipairs(ws) do
        local p, sev = w.part or "", (w.severity or 0)
        if w.kind == "internal" and p == "lungs" then
            f = math.max(f, sev)                 -- collapsed/punctured lung
        elseif w.kind == "fracture" and p == "windpipe" then
            f = math.max(f, sev)                 -- crushed airway
        elseif w.kind == "severed"
               and (p == "neck" or p == "windpipe" or p == "head") then
            f = math.max(f, 1.0)                 -- airway severed
        end
    end
    return math.min(1, f)
end

-- 0..1 NEURO failure — catastrophic brain trauma drives a nervous-system
-- shutdown (the heart still beats, sci-fi conceit, but the brain is dying).
-- A pulverized brain reads ~1; a moderate concussion barely registers.
function M.neuroFailure(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local f = 0
    for _, w in ipairs(ws) do
        local p, sev = w.part or "", (w.severity or 0)
        if (w.kind == "concussion" or w.kind == "internal") and p == "brain" then
            f = math.max(f, sev)
        elseif w.kind == "severed" and (p == "head" or p == "brain") then
            f = math.max(f, 1.0)                 -- decapitation
        end
    end
    return math.min(1, f)
end

-- 0..1 traumatic SHOCK — the body's whole-system response to massive acute
-- injury (cardiac arrest from shock). Driven by the AGGREGATE of severe
-- wounds, not any single one: one broken arm is survivable, but many
-- severe wounds at once tips into shock. Arterial wounds weigh heaviest.
function M.shockFailure(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local load = 0
    for _, w in ipairs(ws) do
        local sev = w.severity or 0
        if sev >= 0.4 then
            local wgt = (w.kind == "arterial") and 1.5
                     or (w.kind == "internal") and 1.0
                     or 0.7                       -- fractures / blunt / etc.
            load = load + (sev - 0.3) * wgt
        end
    end
    -- Saturating: ~1.8 of accumulated load → near-certain shock.
    return 1 - math.exp(-load / 1.2)
end

-- 0..1 ORGAN failure — untreated abdominal/visceral trauma festering over
-- "hours" (sepsis, hepatic encephalopathy from a ruptured liver, renal
-- failure). The slow pathway: a unit walks away from the fight and dies
-- later if never treated.
local VISCERA = { liver = true, kidneys = true, stomach = true,
                  intestines = true, spleen = true }
function M.organFailure(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local f = 0
    for _, w in ipairs(ws) do
        local p, sev = w.part or "", (w.severity or 0)
        if w.kind == "internal" and VISCERA[p] then
            f = math.max(f, sev)
        end
    end
    return math.min(1, f)
end

-- 0..1 SEPSIS — systemic infection from untreated/dirty wounds spreading
-- to the bloodstream. Driven by the AGGREGATE of each wound's infection
-- level (woundInfection, grown deterministically in Combat.Wounds on open,
-- un-disinfected wounds). Saturating: several festering wounds tip into
-- sepsis faster than one. Antibiotics (unit.treatInfection) drive each
-- wound's infection down → this falls back below the meter deadband and the
-- unit recovers. Antiseptic at dressing-time prevents it ever starting.
function M.sepsisFailure(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local load = 0
    for _, w in ipairs(ws) do
        local inf = w.infection or 0
        if inf > 0.2 then load = load + (inf - 0.2) end
    end
    -- ~0.8 of accumulated load (≈ one fully-festering wound) → ~0.8 fail.
    return 1 - math.exp(-load / 0.5)
end

-- Total active infection across all wounds (sum of woundInfection). Drives the
-- fever response in thermo.lua (the body raises its temperature to fight it).
function M.infectionLoad(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return 0 end
    local load = 0
    for _, w in ipairs(ws) do load = load + (w.infection or 0) end
    return load
end

-- The most plausible / immediate cause of death for a (possibly dead)
-- unit, from its active wounds. Returns a phrase ("a ruptured heart").
function M.deathCause(uid)
    local ws = unit.getWounds(uid)
    local best, bestScore = nil, -1
    if type(ws) == "table" then
        -- Gangrene: heavy dead tissue outranks most causes (a rotting unit
        -- that dies died of gangrene, whatever meter actually topped out).
        for _, w in ipairs(ws) do
            if (w.necrosis or 0) >= 0.6 and 80 > bestScore then
                best, bestScore = "gangrene", 80
            end
        end
        for _, w in ipairs(ws) do
            local wk, wp, wsev = w.kind, tok(w.part), (w.severity or 0)
            for _, c in ipairs(DEATH_CAUSES) do
                if c.k == wk
                   and (c.part == nil or c.part == wp)
                   and wsev >= (c.sev or 0)
                   and c.score > bestScore then
                    best, bestScore = c.cause, c.score
                    break   -- first matching entry for this wound
                end
            end
        end
    end
    -- nil when no wound scores (a woundless death — starvation, thirst):
    -- the caller falls back to the explicit cause it was given.
    return best
end

function M.lethalCause(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return nil end
    for _, w in ipairs(ws) do
        -- `vital` is set engine-side per (sub)part (brain, spine, heart,
        -- or a vital macro-part on a simple body). Falls back to the
        -- VITAL_PARTS name table if the flag is absent (older callers).
        local vital = w.vital
        if vital == nil then vital = VITAL_PARTS[w.part or ""] end
        if vital and (w.severity or 0) >= 1.0 then
            return M.name(w.kind, w.part, w.severity)
        end
    end
    return nil
end

return M
