-- Combat-log injury narration.
--
-- Turns a hit's per-layer injury detail (from the engine, in the event
-- payload) into a cold, clinical Dwarf-Fortress-style sentence:
--
--   "Acolyte slashes Acolyte in the left hand with a steel dagger,
--    slicing off the index finger, lacerating the ring finger's skin,
--    fat, and muscle, and cutting the little finger's skin."
--
-- ALL wording lives in the editable tables at the top — tweak freely.
-- The sentence-builder groups a subpart's soft-tissue layers under one
-- verb, lists same-verb injuries together, and filters injuries implied
-- by a sever (a sliced-off finger doesn't also report its cut layers).

local M = {}

-- ── Severity tiers (1..4). T1<0.25 · T2<0.5 · T3<0.85 · T4≥0.85 ──────────
local function tier(sev)
    if     sev >= 0.85 then return 4
    elseif sev >= 0.50 then return 3
    elseif sev >= 0.25 then return 2
    else                    return 1 end
end

-- ── EDITABLE VOCAB ──────────────────────────────────────────────────────

-- Main sentence verb, by mechanism × overall-hit tier (the swing itself).
local MAIN_VERB = {
    slash = { "grazes", "cuts", "slashes", "hacks at" },
    stab  = { "jabs", "stabs", "thrusts at", "impales" },
    blunt = { "strikes", "clubs", "bashes", "smashes" },
}

-- Per-injury clause gerund, by tissue family × tier.
local GERUND = {
    soft_slash    = { "grazing", "cutting", "lacerating", "gashing open" },
    soft_stab     = { "nicking", "piercing", "stabbing", "impaling" },
    soft_blunt    = { "bruising", "battering", "mangling", "pulping" },
    bone          = { "fracturing", "breaking", "shattering", "crushing" },
    cartilage     = { "bruising", "cracking", "crushing", "crushing" },
    organ_tear    = { "nicking", "tearing", "rupturing", "bursting" },
    organ_rupture = { "bruising", "tearing", "rupturing", "destroying" },
    nerve_blunt   = { "bruising", "concussing", "battering", "pulverizing" },
    nerve_cut     = { "nicking", "tearing", "mangling", "destroying" },
    artery        = { "nicking", "opening", "severing", "severing" },
}

-- Sever phrasing (a whole structure removed), by mechanism.
local SEVER_VERB = { slash = "slicing off", stab = "tearing off",
                     blunt = "tearing off", default = "severing" }

-- Hollow organs tear/perforate; solid/sac organs rupture. Keyed by the
-- organ layer name — include plural ids (the acolyte authors "lungs",
-- "kidneys") so they don't fall through to the "tear" default.
local ORGAN_BEHAVIOR = {
    intestines = "tear", colon = "tear", stomach = "tear",
    bladder = "tear", lung = "tear", lungs = "tear",
    liver = "rupture", spleen = "rupture",
    kidney = "rupture", kidneys = "rupture", heart = "rupture",
}

-- Soft-tissue layer names — grouped under the subpart possessive
-- ("the forearm's skin, fat, and muscle").
local SOFT = { skin = true, fat = true, muscle = true, scalp = true }

-- Severity at/above which a cut/blunt to a structural layer SEVERS the
-- whole subpart (and its softer layers are then implied, not reported).
local SEVER_SEVERITY = 1.0

-- ── helpers ─────────────────────────────────────────────────────────────

local function familyOf(material, mech, name)
    if material == "bone" then return "bone"
    elseif material == "cartilage" then return "cartilage"
    elseif material == "artery" then return "artery"
    elseif material == "nerve" then
        return (mech == "blunt") and "nerve_blunt" or "nerve_cut"
    elseif material == "organ" then
        local b = ORGAN_BEHAVIOR[name] or "tear"
        return (b == "rupture") and "organ_rupture" or "organ_tear"
    else  -- flesh/skin/etc.
        if mech == "slash" then return "soft_slash"
        elseif mech == "stab" then return "soft_stab"
        else return "soft_blunt" end
    end
end

local function gerundFor(material, mech, name, sev)
    local fam = familyOf(material, mech, name)
    local ladder = GERUND[fam]
    return ladder and ladder[tier(sev)] or "wounding"
end

-- Join a list with commas + "and": {a} → "a"; {a,b} → "a and b";
-- {a,b,c} → "a, b, and c".
local function listJoin(items)
    local n = #items
    if n == 0 then return "" end
    if n == 1 then return items[1] end
    if n == 2 then return items[1] .. " and " .. items[2] end
    return table.concat(items, ", ", 1, n - 1) .. ", and " .. items[n]
end

-- Parse the engine's detail string "sub:layer:material:sevPct|..." into
-- a list of { sub, layer, material, sev }.
local function parseDetail(s)
    local out = {}
    if not s or s == "" then return out end
    for entry in s:gmatch("[^|]+") do
        local sub, layer, mat, pct = entry:match("([^:]*):([^:]*):([^:]*):([^:]*)")
        if sub then
            out[#out + 1] = { sub = sub, layer = layer, material = mat,
                              sev = (tonumber(pct) or 0) / 100 }
        end
    end
    return out
end

-- ── the sentence-builder ────────────────────────────────────────────────

-- Build the list of outcome clauses from the per-layer detail.
function M.clauses(detail, mech)
    -- 1. Detect severs: a structural (bone/cartilage) layer at/above the
    --    sever threshold under a cut/blunt removes the whole subpart; its
    --    other layers are then IMPLIED (suppressed).
    local severed = {}     -- sub -> true
    for _, d in ipairs(detail) do
        if (d.material == "bone" or d.material == "cartilage")
           and d.sev >= SEVER_SEVERITY then
            severed[d.sub] = true
        end
    end

    -- 2. A slash swath crosses several subparts' soft tissue at DIFFERENT
    --    depths (the near finger gashed, the far one grazed). Report each
    --    soft layer ONCE, at its deepest occurrence — otherwise the line
    --    reads "cutting the skin ... and grazing the skin". Pre-compute the
    --    worst severity per soft-tissue layer name.
    local softWorst = {}
    for _, d in ipairs(detail) do
        if not severed[d.sub] and SOFT[d.layer] then
            if not softWorst[d.layer] or d.sev > softWorst[d.layer] then
                softWorst[d.layer] = d.sev
            end
        end
    end

    -- 3. Group by gerund. Soft tissue (skin/fat/muscle/scalp) gets NO
    --    possessive — the limb is already named in the main sentence — and
    --    same-verb soft layers collapse into one "the X, Y, and Z". Named
    --    structures (bones, organs, vessels) stay individual ("the radius
    --    and the ulna" — same-part bones kept separate by design).
    local order, groups = {}, {}
    local softDone = {}
    local function ensure(g)
        if not groups[g] then
            groups[g] = { soft = {}, named = {} }
            order[#order + 1] = g
        end
        return groups[g]
    end
    for _, d in ipairs(detail) do
        if not severed[d.sub] then
            if SOFT[d.layer] then
                if not softDone[d.layer] then
                    softDone[d.layer] = true
                    -- gerund keyed on the layer's WORST severity, not this row's
                    local g = gerundFor(d.material, mech, d.layer, softWorst[d.layer])
                    local grp = ensure(g)
                    grp.soft[#grp.soft + 1] = d.layer
                end
            else
                local g = gerundFor(d.material, mech, d.layer, d.sev)
                local grp = ensure(g)
                grp.named[#grp.named + 1] = d.layer
            end
        end
    end

    local clauses = {}
    -- Sever clauses first (the dramatic ones).
    local severList = {}
    for sub, _ in pairs(severed) do severList[#severList + 1] = "the " .. sub end
    if #severList > 0 then
        local v = SEVER_VERB[mech] or SEVER_VERB.default
        clauses[#clauses + 1] = v .. " " .. listJoin(severList)
    end
    -- Then the layer clauses, in encounter order.
    for _, g in ipairs(order) do
        local grp = groups[g]
        local nouns = {}
        if #grp.soft > 0 then
            nouns[#nouns + 1] = "the " .. listJoin(grp.soft)
        end
        for _, nm in ipairs(grp.named) do
            nouns[#nouns + 1] = "the " .. nm
        end
        clauses[#clauses + 1] = g .. " " .. listJoin(nouns)
    end
    return clauses
end

-- Natural body-part weapons take a possessive ("its claws"), manufactured
-- ones an article ("a steel dagger").
local NATURAL_WEAPONS = {
    fists = true, claws = true, fangs = true, paw = true, paws = true,
    teeth = true, bite = true, talons = true, beak = true, horns = true,
    hooves = true, tail = true,
}
-- "Steel Dagger" → "a steel dagger"; "claws" → "its claws".
local function weaponPhrase(w)
    if not w or w == "" then return "a weapon" end
    local lw = w:lower()
    if NATURAL_WEAPONS[lw] then return "its " .. lw end
    local art = lw:match("^[aeiou]") and "an " or "a "
    return art .. lw
end

-- Full hit line. atkName/tgtName already display-formatted by the caller.
function M.hitLine(atkName, tgtName, payload)
    local mech   = payload.kind or "blunt"
    local limb   = payload.limb or payload.part or "body"
    local weapon = weaponPhrase(payload.weapon)
    local detail = parseDetail(payload.detail)

    -- Main verb tier = worst layer severity in the blow.
    local worst = tonumber(payload.severity) or 0
    for _, d in ipairs(detail) do if d.sev > worst then worst = d.sev end end
    local mv = MAIN_VERB[mech] or MAIN_VERB.blunt
    local verb = mv[tier(worst)] or mv[1]

    local head = string.format("%s %s %s in the %s with %s",
        atkName, verb, tgtName, limb, weapon)
    local cl = M.clauses(detail, mech)
    if #cl == 0 then return head .. "." end
    return head .. ", " .. listJoin(cl) .. "."
end

return M
