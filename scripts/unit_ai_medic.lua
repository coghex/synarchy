-- Unit AI medic auto-treatment (#538 split from unit_ai.lua).
--
-- Action: treat_ally  (Phase D — medic auto-treat)
--
-- A unit that KNOWS bleed-control bandages a bleeding ally. Capability
-- = bleed_control knowledge × intelligence (the same product the treat
-- action and the Knowledge-tab tooltip use). The squad self-organises:
--   * the BEST available medic claims a patient and rushes;
--   * a LESSER medic only steps in when the best is tied up in combat
--     AND nobody else has already claimed that patient.
-- Flow mirrors deliver_to_build_site: claim → fetch the first-aid kit
-- off the technomule → carry it to the patient → unit.treatBleeding
-- (drawing from the kit now in the medic's own inventory), repeating
-- until the patient stops bleeding or the kit runs dry.
--
-- State on s:
--   treatClaim   = { patient = uid }   -- lock-in, visible to others
--   treatPending = patient table       -- utility → execute handoff
--
-- Non-external kinds (concussion / fracture / internal) aren't
-- bandageable, so they don't make a unit a patient.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local aiState        = core.aiState
local distance        = core.distance
local reportFailure   = core.reportFailure

local mv = require("scripts.movement_speed")

local M = {}

local TREAT_SKIP_KINDS = {
    concussion = true, fracture = true, internal = true,
}

-- Medic capability: bleed-control knowledge × intelligence. 0 = the
-- unit doesn't know how (or is too dim to apply it).
local function medicCapability(uid)
    local lvl = unit.getKnowledge(uid, "bleed_control")
    if not lvl or lvl <= 0 then return 0 end
    return lvl * (unit.getStat(uid, "intelligence") or 1.0)
end

-- A conscious, living unit can administer aid; a collapsed or dead one
-- can't (and isn't counted as a candidate medic).
local function canActAsMedic(uid)
    local pose = unit.getPose(uid)
    return pose ~= nil and pose ~= "dead" and pose ~= "collapsed"
end

-- Does this unit have a wound worth dressing? — an external bleeder
-- still seeping above the "good enough" threshold AND not already
-- mostly self-clotted (a wound that's clotting on its own doesn't need
-- a bandage wasted on it).
local CLOT_ENOUGH = 0.85
-- An infected wound past this level wants antibiotics (the cure). Applies
-- to ANY wound kind (even the skip-kinds: a closed fracture can still
-- fester), so it's checked outside the bleeder gate.
local INFECT_TREAT_MIN = 0.15
local function needsTreatment(uid, minSeep)
    for _, w in ipairs(unit.getWounds(uid) or {}) do
        if not TREAT_SKIP_KINDS[w.kind] and (w.bandage or 1) > minSeep
           and (w.clot or 0) < CLOT_ENOUGH then
            return true
        end
        if (w.infection or 0) >= INFECT_TREAT_MIN then
            return true   -- needs antibiotics
        end
    end
    return false
end

-- Does the patient have an infected wound worth antibiotics?
local function hasInfection(uid)
    for _, w in ipairs(unit.getWounds(uid) or {}) do
        if (w.infection or 0) >= INFECT_TREAT_MIN then return true end
    end
    return false
end

-- A medic treats its own squad: same faction (a "debug" medic — staged
-- in the debug overlay — also treats player units so test fights can be
-- patched up).
local function isAlly(uid, medicFaction)
    local f = unit.getFaction(uid)
    return f == medicFaction
        or (medicFaction == "debug" and f == "player")
        or (medicFaction == "player" and f == "debug")
end

-- A best-medic who's fighting can't break off — that's what frees a
-- lesser medic to step in.
local function medicBusyInCombat(uid)
    local st = uid and aiState[uid]
    local act = st and st.currentAction
    return act == "retreat" or act == "engage" or act == "attack_target"
end

-- Is `uid` free to take on THIS patient right now? A medic in combat
-- can't break off, and one already committed to a DIFFERENT patient is
-- spoken for — either way it's unavailable, which is what lets a free
-- lesser medic step in. (A medic already claiming THIS patient is still
-- "available" for it — that's the one re-confirming its own claim.)
local function medicAvailable(uid, patientUid)
    if medicBusyInCombat(uid) then return false end
    local st = aiState[uid]
    if st and st.treatClaim and st.treatClaim.patient ~= patientUid then
        return false
    end
    return true
end

-- The best AVAILABLE medic for a patient, scored by capability with a
-- gentle distance discount (a much-nearer competent medic beats a
-- marginally-better distant one, so we don't summon a skilled medic from
-- across the map past a free one standing next to the patient). Excludes
-- the patient itself, the dead/collapsed, NON-allies, medics in combat,
-- and medics already committed to a different patient. Returns the uid,
-- or nil if nobody can help. `params` supplies treat_scan_range.
local function bestMedicFor(patientUid, params)
    local pinfo = unit.getInfo(patientUid)
    local range = (params and params.treat_scan_range) or 60.0
    local bestUid, bestScore = nil, 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        if uid ~= patientUid and canActAsMedic(uid)
           and isAlly(patientUid, unit.getFaction(uid))
           and medicAvailable(uid, patientUid) then
            local cap = medicCapability(uid)
            if cap > 0 then
                local minfo = unit.getInfo(uid)
                local d = (pinfo and minfo)
                    and distance(pinfo.gridX, pinfo.gridY,
                                 minfo.gridX, minfo.gridY) or 0
                local score = cap * (1 - 0.5 * math.min(1, d / range))
                if score > bestScore then
                    bestUid, bestScore = uid, score
                end
            end
        end
    end
    return bestUid
end

-- Any LIVE, AVAILABLE unit (≠ excludeUid) already claiming this patient?
-- A claimer that's been pulled into combat can't honor its claim while
-- fighting, so it does NOT hold the slot — a free medic must be able to
-- step in (this mirrors medicAvailable/bestMedicFor, which already skip
-- combat-busy medics; without the same skip here the patient would be
-- pinned to the interrupted medic and ignored by everyone else, #306).
-- The claim itself persists (treat_ally is not cleared on preempt, like
-- every other action's locked state) so the fighter resumes this patient
-- once combat ends; if a lesser medic finished it first, treatExecute
-- sees no remaining need and drops the redundant claim.
local function patientClaimed(patientUid, excludeUid)
    for otherUid, st in pairs(aiState) do
        if otherUid ~= excludeUid and st.treatClaim
           and st.treatClaim.patient == patientUid then
            if unit.getInfo(otherUid) and not medicBusyInCombat(otherUid) then
                return true
            end
        end
    end
    return false
end

-- Nearest treatable, currently-unclaimed bleeding ally, or nil.
local function findPatient(uid, info, params)
    local myFaction = unit.getFaction(uid)
    local best, bestD = nil, params.treat_scan_range
    for _, pid in ipairs(unit.getAllIds() or {}) do
        if pid ~= uid and isAlly(pid, myFaction)
           and needsTreatment(pid, params.treat_min_seep)
           and not patientClaimed(pid, uid) then
            local pinfo = unit.getInfo(pid)
            if pinfo and unit.getPose(pid) ~= "dead" then
                local d = distance(info.gridX, info.gridY,
                                   pinfo.gridX, pinfo.gridY)
                if d <= bestD then
                    best = { uid = pid, distance = d }
                    bestD = d
                end
            end
        end
    end
    return best
end

local function treatAllyUtility(uid, s, params)
    -- Locked in once claimed; survives across ticks so the
    -- fetch-and-treat sequence isn't yanked by ambient utility. Finite
    -- so dire survival / combat can still preempt (claim persists).
    if s.treatClaim then return params.treat_lock_utility end

    if medicCapability(uid) <= 0 then return -math.huge end
    if not canActAsMedic(uid) then return -math.huge end
    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local patient = findPatient(uid, info, params)
    if not patient then return -math.huge end

    -- Squad ranking: only the best AVAILABLE allied medic takes the
    -- patient. bestMedicFor already excludes medics in combat or
    -- committed to another patient (and non-allies), so a free lesser
    -- medic automatically steps in when the best is tied up — and two
    -- bleeding allies get two different medics instead of serialising.
    if bestMedicFor(patient.uid, params) ~= uid then
        return -math.huge
    end

    s.treatPending = patient
    return params.treat_base_utility
end

-- A usable kit the unit already carries (a container holding ≥1
-- bandage): returns its defName, or nil.
local function ownKitDefName(uid)
    for _, it in ipairs(unit.getInventory(uid) or {}) do
        if it.kind == "container" then
            for _, r in ipairs(unit.getItemContents(uid, it.defName) or {}) do
                if r.defName == "bandage" and (r.count or 0) > 0 then
                    return it.defName
                end
            end
        end
    end
    return nil
end

-- Nearest unit carrying a usable kit (the technomule), to fetch from.
local function findKitHolder(fromX, fromY)
    local best, bestD = nil, math.huge
    for _, uid in ipairs(unit.getAllIds() or {}) do
        local kit = ownKitDefName(uid)
        if kit then
            local info = unit.getInfo(uid)
            if info then
                local d = distance(fromX, fromY, info.gridX, info.gridY)
                if d < bestD then
                    best = { uid = uid, gridX = info.gridX,
                             gridY = info.gridY, kit = kit }
                    bestD = d
                end
            end
        end
    end
    return best
end

local function treatExecute(uid, s, params)
    -- Lock in the claim on first call so other medics' utility checks
    -- see the reservation.
    if not s.treatClaim then
        local p = s.treatPending
        if not p then return end
        s.treatClaim   = { patient = p.uid }
        s.treatPending = nil
    end
    local patient = s.treatClaim.patient

    local info = unit.getInfo(uid)
    if not info then s.treatClaim = nil; return end

    -- Patient vanished / died / fully dressed → release.
    if not unit.getInfo(patient) or unit.getPose(patient) == "dead"
       or not needsTreatment(patient, params.treat_min_seep) then
        s.treatClaim = nil
        return
    end

    -- Phase 1: make sure I'm carrying a kit with bandages; if not,
    -- fetch one off the nearest kit-holder (the technomule). (The
    -- no-kit-anywhere fallback — a makeshift tourniquet — is a later
    -- chunk; for now, release so the unit re-evaluates.)
    -- Phase 1: secure supplies. If I'm not carrying a kit, fetch one
    -- off the nearest holder (the technomule). If there's NO kit
    -- anywhere, don't give up — rush to the patient and improvise a
    -- makeshift tourniquet there (the treatBleeding fallback). Better a
    -- crude stopgap than letting them bleed.
    if not ownKitDefName(uid) then
        local holder = findKitHolder(info.gridX, info.gridY)
        if holder then
            if distance(info.gridX, info.gridY, holder.gridX, holder.gridY)
               > params.mule_fetch_arrival then
                unit.moveTo(uid, holder.gridX, holder.gridY, mv.ordered(uid))
                return
            end
            unit.stop(uid)
            unit.transferItemToUnit(holder.uid, uid, holder.kit)
            return   -- re-evaluate next tick now that I hold the kit
        end
        -- no kit reachable → fall through to the patient (tourniquet)
    end

    -- Phase 2: rush to the patient. Target a tile ~1 away (toward me),
    -- not the patient's own tile — a collapsed patient OCCUPIES its
    -- tile, and pathing onto a blocked tile fails outright, leaving the
    -- medic frozen. treat_arrival (1.5) still lets us dress the wound
    -- from the neighbouring tile. (Same "approach the obstacle, don't
    -- stand on it" rule the deliver action uses for build sites.)
    local pinfo = unit.getInfo(patient)
    local d = distance(info.gridX, info.gridY, pinfo.gridX, pinfo.gridY)
    if d > params.treat_arrival then
        local dx, dy = info.gridX - pinfo.gridX, info.gridY - pinfo.gridY
        local len = math.max(0.001, math.sqrt(dx * dx + dy * dy))
        local tx = pinfo.gridX + (dx / len)
        local ty = pinfo.gridY + (dy / len)
        unit.moveTo(uid, tx, ty, mv.sprint(uid))
        return
    end

    -- Phase 3: arrived — dress the worst bleeder. treatBleeding draws
    -- from my own kit (default owner = me). Re-fires on subsequent idle
    -- ticks (lock keeps treat_ally selected) until the patient stops
    -- bleeding or the kit runs dry; a hard failure drops the claim.
    unit.stop(uid)
    local res = unit.treatBleeding(uid, patient)
    if res and not res.ok and res.message ~= "no bleeding wound to treat" then
        -- Surface the failed treatment (red, coalesced per patient). A
        -- patient with only an infected (non-bleeding) wound legitimately
        -- has "no bleeding wound" — that's not a failure, it's the cue to
        -- give antibiotics below, so don't report it.
        reportFailure(patient, "Treatment failed: "
            .. (res.message or "unknown"))
        s.treatClaim = nil
    end
    -- CURE: administer antibiotics to an infected wound (treatBleeding's
    -- antiseptic step only PREVENTS infection on a fresh dressing; an
    -- already-infected wound needs the antibiotics cure). Requires the
    -- INFECTION-CONTROL knowledge; re-fires until the infection is knocked
    -- down or the kit's pills run out.
    if hasInfection(patient) and unit.getKnowledge(uid, "infection_control") then
        local ir = unit.treatInfection(uid, patient)
        if ir and not ir.ok then
            reportFailure(patient, "Infection untreated: "
                .. (ir.message or "unknown"))
        end
    end
end

M.treatAllyUtility = treatAllyUtility
M.treatExecute     = treatExecute

return M
