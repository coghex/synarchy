-- Autonomous combat-decision policy seam (#2521, FTS-5 of #2496).
--
-- One place answers "may this unit start, or keep, an autonomous fight
-- with that target?" for every acquisition and continuation path: the
-- ruin-encounter guards (unit_ai_encounter.lua), the generic
-- incoming_hit engage (unit_ai_combat.lua), attack_target continuation
-- and its mid-fight retaliation swap (unit_ai_combat_attack.lua).
--
-- Two independent permissions, nothing else:
--
--   * HOSTILITY -- unit.relationFromTo(uid, target) == "hostile", the
--     engine's directed relation authority. Never gated by the cutoffs
--     below: a hostile recent attacker stays acquirable under each
--     path's existing window and eligibility rules.
--   * LOCAL SELF-DEFENSE -- the target is this unit's own last attacker
--     (unit.getLastAttacker, the existing local damage awareness), the
--     hit is at most LOCAL_WINDOW_SEC old (exactly ten qualifies), and
--     it is STRICTLY later than every applicable cutoff: the
--     termination of this unit's previous attack goal, and a fixture-
--     supplied diplomacy-clear time. Each new hit refreshes the window;
--     visibility never does. It mutates no relation, tag or team state,
--     and it grants nobody but the struck unit permission.
--
-- Continuation additionally accepts a fixture-supplied grandfathered
-- AUTHORIZATION, but only for the exact (unit, target, goal occurrence)
-- it names: a new goal of the same name against the same target is a new
-- occurrence, and a mid-fight retarget voids it for the rest of the
-- occurrence. Unrestricted combat and the attack permission
-- (unit.canAttack) are deliberately NOT acquisition rules.
--
-- GOVERNED GOALS are classified by EXCLUSION: every attack goal except a
-- committed player/debug order and a mental lash-out, which keep their
-- own termination rules. The classification belongs to the goal
-- occurrence (see open() below), not to the unit's mutable flags. So a
-- non-hostile self-defense goal restored from a save ends at its first
-- decision -- last-attacker fields reset on load and nothing here
-- survives it -- which is intentional: old damage cannot keep a fight
-- alive across a load.
--
-- TRANSIENT BOOKKEEPING ONLY, KEYED BY UID. The cutoff and the live
-- goal's occurrence (which carries its own id) are keyed by unit id,
-- never by AI-state table identity, and never stored in aiState or saved.
-- Both tables are enrolled with unit_ai_claims and emptied by its reset
-- on every successful load and on session teardown, so a rewound uid
-- inherits nothing. After a successful load, each restored attack goal
-- gets a FRESH occurrence, classified from its durable saved fields
-- (M.restoreOccurrences); it carries no cutoff, basis or authorization
-- from the saving session.
--
-- LOAD ROLLBACK. A load that fails after unit_ai has applied is unwound
-- by saveModules re-applying the OLD session's rows, with
-- apply(data, nil, "rollback"). unit_ai_save's forward apply first takes
-- an in-memory copy of this registry (M.stashForLoad), and its rollback
-- apply puts that copy back (M.restoreStash), so the restored session
-- keeps its exemptions, cutoffs and occurrences -- including when the
-- load reset had already emptied and re-seeded the registry before a
-- later reset hook failed. The stash is never saved; the next forward
-- apply overwrites it and session teardown clears it (M.dropStash).
-- Neither table nor the stash grants a permission by itself.
--
-- FIXTURE INPUTS. Runtime diplomacy and grandfathered authorization
-- belong to FTS-6; until then they reach this seam only through
-- M.fixtureInputs, whose production default returns nothing.

local claims = require("scripts.unit_ai_claims")

local M = {}

M.LOCAL_WINDOW_SEC = 10

-- uid -> game time at which this unit's last governed attack goal ended.
local cutoffs = claims.track({})
-- uid -> { id, target, provenance, exempt, basis, retargeted } for the
-- live attack goal.
local occurrences = claims.track({})
local nextOccurrence = 0
-- The load-rollback copy: { cutoffs = {...}, occurrences = {...} } or nil.
local stash = nil

local function now()
    return (engine and engine.gameTime and engine.gameTime()) or 0
end

-- The uid -> AI-state map goal changes are resolved against: core's
-- aiState in production (bound by unit_ai_core), a fixture's own map in
-- a harness that stubs core. unit_ai_core's setGoal/markGoalAccomplished
-- hand over only the row, so goalChanged looks its uid up here.
local states = {}
function M.bindStates(t) states = t end

local function uidOf(s)
    for uid, row in pairs(states) do
        if row == s then return uid end
    end
    return nil
end

-- Production default: no live diplomacy and no grandfathered
-- authorization exist. A fixture may replace this with a function
-- returning { diplomacyClearAt = <time>,
--             authorization = { uid = , target = , occurrence = } }.
function M.fixtureInputs(_uid)
    return nil
end

function M.isHostile(uid, target)
    return unit.relationFromTo(uid, target) == "hostile"
end

-- EXEMPTION IS A PROPERTY OF THE GOAL OCCURRENCE, fixed once when the
-- occurrence opens and never read from mutable state afterwards. The
-- opener passes the goal's PROVENANCE explicitly: unit_ai_core's
-- commandAttack ("committed" for a player/debug order, "autonomous"
-- otherwise) and unit_ai_mental's lash-out start ("lash_out"). Ending,
-- cutoff, continuation and swap decisions read only that stored
-- classification -- never s.committed or s.mentalLashoutActive, which
-- retreat, lash-out cleanup and commandAttack all rewrite before the goal
-- they describe changes. An attack goal with no occurrence (no opener
-- passed through setGoal) is GOVERNED: classification fails closed.
local EXEMPT_PROVENANCE = { committed = true, lash_out = true }

local function open(uid, s, provenance)
    nextOccurrence = nextOccurrence + 1
    occurrences[uid] = { id = nextOccurrence, target = s.attackTargetUid,
                         provenance = provenance or "autonomous",
                         exempt = EXEMPT_PROVENANCE[provenance] == true }
end

-- Is this unit's live attack goal exempt? Only its occurrence answers.
function M.exempt(uid)
    local occ = occurrences[uid]
    return occ ~= nil and occ.exempt
end

-- Called by unit_ai_core's setGoal (before the goal changes) and by its
-- markGoalAccomplished (with nil, when the ending goal is active): an
-- ending or replaced GOVERNED attack goal records the cutoff, and a new
-- attack goal opens a fresh occurrence with the provenance its opener
-- passed. A committed order or a lash-out therefore never stamps a
-- cutoff, and a governed goal always does, whatever replaces it. A row
-- that belongs to no bound unit has nothing to record.
function M.goalChanged(s, newGoal, provenance)
    if s.activeGoal ~= "attack" and newGoal ~= "attack" then return end
    local uid = uidOf(s)
    if uid == nil then return end
    if s.activeGoal == "attack" then
        if not M.exempt(uid) then cutoffs[uid] = now() end
        occurrences[uid] = nil
    end
    if newGoal == "attack" then open(uid, s, provenance) end
end

-- After a successful load, once the load reset has emptied the tables
-- and the restored aiState is in place (unit_ai_save's reset hook), and
-- so before any cleanup can rewrite them: rebuild every restored attack
-- goal's occurrence from its DURABLE saved fields -- mentalLashoutActive
-- names a lash-out, committed a player/debug order. A fresh occurrence id
-- inherits nothing from the saving session: no cutoff, basis or
-- authorization. Returns the count.
local function savedProvenance(s)
    if s.mentalLashoutActive then return "lash_out" end
    if s.committed then return "committed" end
    return "autonomous"
end

function M.restoreOccurrences(rows)
    local n = 0
    for uid, s in pairs(rows) do
        if type(s) == "table" and s.activeGoal == "attack" then
            open(uid, s, savedProvenance(s)); n = n + 1
        end
    end
    return n
end

local function copy(t)
    local out = {}
    for k, v in pairs(t) do
        if type(v) == "table" then
            local row = {}
            for f, x in pairs(v) do row[f] = x end
            v = row
        end
        out[k] = v
    end
    return out
end

local function replace(live, from)
    for k in pairs(live) do live[k] = nil end
    for k, v in pairs(from) do live[k] = v end
end

-- Load rollback (see the header). stashForLoad copies the outgoing
-- session's registry; restoreStash puts that copy back over whatever
-- the abandoned load left there; dropStash forgets it.
function M.stashForLoad()
    stash = { cutoffs = copy(cutoffs), occurrences = copy(occurrences) }
end

function M.restoreStash()
    if stash == nil then return false end
    replace(cutoffs, copy(stash.cutoffs))
    replace(occurrences, copy(stash.occurrences))
    return true
end

function M.dropStash()
    stash = nil
end

-- Which permission acquired the live goal ("hostile" / "self_defense").
function M.noteBasis(uid, basis)
    local occ = occurrences[uid]
    if occ then occ.basis = basis end
end

-- The live goal's occurrence id, for fixtures building an authorization.
function M.occurrence(uid)
    local occ = occurrences[uid]
    return occ and occ.id or nil
end

function M.cutoff(uid)
    return cutoffs[uid]
end

-- A mid-fight swap moved the goal to another target: no authorization
-- may follow it, and none may revive on a swap back.
function M.retargeted(uid)
    local occ = occurrences[uid]
    if occ then occ.retargeted = true end
end

-- Does `att` (a unit.getLastAttacker record) let `uid` act in local
-- self-defense against `target` right now?
function M.selfDefenseFresh(uid, target, att)
    if not att or att.uid ~= target or att.uid == uid then return false end
    local at = att.at or 0
    if now() - at > M.LOCAL_WINDOW_SEC then return false end
    local cut = cutoffs[uid]
    if cut and at <= cut then return false end
    local inputs = M.fixtureInputs(uid)
    local clear = inputs and inputs.diplomacyClearAt
    if clear and at <= clear then return false end
    return true
end

-- Acquisition: "hostile", "self_defense", or nil. Pass `att` only on an
-- attacker path; a visibility path passes nil and can acquire through
-- hostility alone.
function M.acquireBasis(uid, target, att)
    if M.isHostile(uid, target) then return "hostile" end
    if att and M.selfDefenseFresh(uid, target, att) then
        return "self_defense"
    end
    return nil
end

local function authorized(uid, target)
    local inputs = M.fixtureInputs(uid)
    local a = inputs and inputs.authorization
    local occ = occurrences[uid]
    return a ~= nil and occ ~= nil and not occ.retargeted
        and a.uid == uid and a.target == target and occ.target == target
        and a.occurrence == occ.id
end

-- Continuation of `uid`'s live attack goal (`s` is its AI state, read
-- for the target only). Returns ok, reason.
function M.mayContinue(uid, s)
    if M.exempt(uid) then return true end
    local target = s.attackTargetUid
    if not target then return true end
    if M.isHostile(uid, target) then return true end
    if M.selfDefenseFresh(uid, target, unit.getLastAttacker(uid)) then
        return true
    end
    if authorized(uid, target) then return true end
    local occ = occurrences[uid]
    if occ and occ.basis == "self_defense" then
        return false, "self-defense ended"
    end
    return false, "faction relation is no longer hostile"
end

return M
