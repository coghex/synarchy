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
local page = require("scripts.unit_ai_page")
-- Exact-instance medical supply discovery, shared with the context menu.
local supply = require("scripts.medical_supply")

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

-- A medic treats its own side. Which factions count as "its own side"
-- is the shared ALLY relation (#912), not a rule medic logic gets to
-- state for itself: the player↔debug pairing that used to be spelled
-- out here (so a debug medic staged in the overlay can patch up player
-- units after a test fight) is now declared in the faction model, where
-- swarm rallying reads the same answer.
local function isAlly(uid, medicFaction)
    return faction.areAllies(unit.getFaction(uid), medicFaction)
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
    -- #2297: a patient whose own projection cannot be read has no page
    -- to rank against, so nobody is ranked for it -- fail closed, the
    -- same rule findKitHolder already applies to its actor.
    if not pinfo then return nil end
    local range = (params and params.treat_scan_range) or 60.0
    local bestUid, bestScore = nil, 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        if uid ~= patientUid and canActAsMedic(uid)
           and isAlly(patientUid, unit.getFaction(uid))
           and medicAvailable(uid, patientUid) then
            local minfo = unit.getInfo(uid)
            -- #2297: rank only medics standing on the PATIENT's own
            -- page. unit.getAllIds reads the ACTIVE page, which is not
            -- necessarily either one's, and the distance discount below
            -- is meaningless between two worlds.
            if minfo and page.same(pinfo.page, minfo.page) then
                local cap = medicCapability(uid)
                if cap > 0 then
                    local d = distance(pinfo.gridX, pinfo.gridY,
                                       minfo.gridX, minfo.gridY)
                    local score = cap * (1 - 0.5 * math.min(1, d / range))
                    if score > bestScore then
                        bestUid, bestScore = uid, score
                    end
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
-- #2297: a claimer on ANOTHER page does not hold the slot either, and
-- for a sharper reason than the combat one above. aiState is global
-- across every page, but the AI only ticks the loaded one, so an
-- off-page claimer never reaches treatExecute and never releases its
-- claim -- it would pin the patient forever against a medic standing
-- right beside them. Unknown on either side reads as "not this
-- claimer's", which fails toward letting the on-page medic work.
local function patientClaimed(patientUid, excludeUid)
    local pinfo = unit.getInfo(patientUid)
    for otherUid, st in pairs(aiState) do
        if otherUid ~= excludeUid and st.treatClaim
           and st.treatClaim.patient == patientUid then
            local oinfo = unit.getInfo(otherUid)
            if oinfo and page.same(pinfo and pinfo.page, oinfo.page)
               and not medicBusyInCombat(otherUid) then
                return true
            end
        end
    end
    return false
end

-- Nearest treatable, currently-unclaimed bleeding ally, or nil.
local function findPatient(uid, info, params)
    local myFaction = unit.getFaction(uid)
    -- #2297: page-qualified against the scanning medic, same rule and
    -- same reason as findKitHolder below. `info` is the medic's own
    -- projection, and page.same is false whenever either side is
    -- unknown, so an unreadable actor selects nobody.
    local myPage = info and info.page
    local best, bestD = nil, params.treat_scan_range
    for _, pid in ipairs(unit.getAllIds() or {}) do
        if pid ~= uid and isAlly(pid, myFaction)
           and needsTreatment(pid, params.treat_min_seep)
           and not patientClaimed(pid, uid) then
            local pinfo = unit.getInfo(pid)
            if pinfo and page.same(myPage, pinfo.page)
               and unit.getPose(pid) ~= "dead" then
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
-- bandage), as { defName, instanceId }, or nil. Exact identity (#2302):
-- the scan asks each inventory row about ITS OWN container, so a
-- stocked kit behind an empty same-definition sibling is found -- and
-- the fetch below can name the instance discovery chose instead of
-- popping whichever same-defName item the holder happens to reach
-- first. Shared with the context menu so the greyed row and the
-- treatment commit answer the same question.
local ownKit = supply.bandageKit

-- Nearest unit carrying a usable kit (the technomule), to fetch from.
-- Page-qualified against the asking medic (#1673), same rule and same
-- reason as fetch.findTechnomule: unit.getAllIds reads the ACTIVE page,
-- which is not necessarily the medic's own.
local function findKitHolder(medicUid, fromX, fromY)
    local myPage = page.ofUnit(medicUid)
    if not myPage then return nil end
    local best, bestD = nil, math.huge
    for _, uid in ipairs(unit.getAllIds() or {}) do
        local kit = ownKit(uid)
        if kit then
            local info = unit.getInfo(uid)
            if info and page.same(myPage, info.page) then
                local d = distance(fromX, fromY, info.gridX, info.gridY)
                if d < bestD then
                    best = { uid = uid, gridX = info.gridX,
                             gridY = info.gridY, kit = kit.defName,
                             kitInstance = kit.instanceId }
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

    -- Patient vanished / died / fully dressed → release. #2297 adds
    -- the page to that list, and checks it HERE — before the kit fetch
    -- and before the walk — because a stored claim outlives the tick
    -- that made it: the active page can move underneath it, and an
    -- off-page claim would otherwise transfer a kit and steer a sprint
    -- toward another world's coordinates.
    local pinfo = unit.getInfo(patient)
    if not pinfo or unit.getPose(patient) == "dead"
       or not page.same(info.page, pinfo.page)
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
    if not ownKit(uid) then
        local holder = findKitHolder(uid, info.gridX, info.gridY)
        if holder then
            if distance(info.gridX, info.gridY, holder.gridX, holder.gridY)
               > params.mule_fetch_arrival then
                unit.moveTo(uid, holder.gridX, holder.gridY, mv.ordered(uid))
                return
            end
            unit.stop(uid)
            -- The exact instance discovery picked: the by-definition
            -- form could pop a DIFFERENT same-defName kit the holder is
            -- also carrying -- an empty one, in the very case that made
            -- the stocked sibling worth fetching (#2302).
            unit.transferItemToUnit(holder.uid, uid, holder.kit,
                                    holder.kitInstance)
            return   -- re-evaluate next tick now that I hold the kit
        end
        -- no kit reachable → fall through to the patient (tourniquet)
    end

    -- Phase 2: rush to the patient. Target a tile ~1 away (toward me),
    -- not the patient's own tile — a collapsed patient OCCUPIES its
    -- tile, and pathing onto a blocked tile fails outright, leaving the
    -- medic frozen. unit.treatmentRange() -- the engine's own reach,
    -- the same one treatBleeding refuses beyond (#2297) -- still lets
    -- us dress the wound from the neighbouring tile. (Same "approach
    -- the obstacle, don't stand on it" rule the deliver action uses
    -- for build sites.)
    local d = distance(info.gridX, info.gridY, pinfo.gridX, pinfo.gridY)
    if d > unit.treatmentRange() then
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
-- Exported for the #1673 page-pairing gate, which asserts directly on
-- what a kit-holder scan selects rather than reconstructing a whole
-- wounded-patient scenario to observe it second-hand.
M.findKitHolder    = findKitHolder

return M
