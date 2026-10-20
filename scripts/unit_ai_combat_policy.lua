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
-- committed player/debug order (s.committed) and a mental lash-out
-- (s.mentalLashoutActive), which keep their own termination rules. So a
-- non-hostile self-defense goal restored from a save ends at its first
-- decision -- last-attacker fields reset on load and nothing here
-- survives it -- which is intentional: old damage cannot keep a fight
-- alive across a load.
--
-- TRANSIENT BOOKKEEPING ONLY. The cutoff and the current goal
-- occurrence are keyed by the unit's AI-state TABLE (weak keys), never
-- stored in aiState and never saved, so a rewound uid after a load can
-- inherit nothing; both tables are enrolled with unit_ai_claims and
-- emptied by its reset on every successful load and session teardown.
-- Neither grants a permission by itself.
--
-- FIXTURE INPUTS. Runtime diplomacy and grandfathered authorization
-- belong to FTS-6; until then they reach this seam only through
-- M.fixtureInputs, whose production default returns nothing.

local claims = require("scripts.unit_ai_claims")

local M = {}

M.LOCAL_WINDOW_SEC = 10

local weak = { __mode = "k" }
-- s -> game time at which this unit's last attack goal ended.
local cutoffs = claims.track(setmetatable({}, weak))
-- s -> { id, target, basis, retargeted } for the live attack goal.
local occurrences = claims.track(setmetatable({}, weak))
local nextOccurrence = 0

local function now()
    return (engine and engine.gameTime and engine.gameTime()) or 0
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

-- Called by unit_ai_core's setGoal (before the goal changes) and by its
-- markGoalAccomplished (with nil, when the ending goal is active): an
-- active attack goal ending or being replaced records the cutoff, and a
-- new attack goal opens a fresh occurrence.
function M.goalChanged(s, newGoal)
    if s.activeGoal == "attack" then
        cutoffs[s] = now()
        occurrences[s] = nil
    end
    if newGoal == "attack" then
        nextOccurrence = nextOccurrence + 1
        occurrences[s] = { id = nextOccurrence, target = s.attackTargetUid }
    end
end

-- Which permission acquired the live goal ("hostile" / "self_defense").
function M.noteBasis(s, basis)
    local occ = occurrences[s]
    if occ then occ.basis = basis end
end

-- The live goal's occurrence id, for fixtures building an authorization.
function M.occurrence(s)
    local occ = occurrences[s]
    return occ and occ.id or nil
end

function M.cutoff(s)
    return cutoffs[s]
end

-- A mid-fight swap moved the goal to another target: no authorization
-- may follow it, and none may revive on a swap back.
function M.retargeted(s)
    local occ = occurrences[s]
    if occ then occ.retargeted = true end
end

function M.governs(s)
    return not (s.committed or s.mentalLashoutActive)
end

-- Does `att` (a unit.getLastAttacker record) let `uid` act in local
-- self-defense against `target` right now?
function M.selfDefenseFresh(uid, s, target, att)
    if not att or att.uid ~= target or att.uid == uid then return false end
    local at = att.at or 0
    if now() - at > M.LOCAL_WINDOW_SEC then return false end
    local cut = cutoffs[s]
    if cut and at <= cut then return false end
    local inputs = M.fixtureInputs(uid)
    local clear = inputs and inputs.diplomacyClearAt
    if clear and at <= clear then return false end
    return true
end

-- Acquisition: "hostile", "self_defense", or nil. Pass `att` only on an
-- attacker path; a visibility path passes nil and can acquire through
-- hostility alone.
function M.acquireBasis(uid, s, target, att)
    if M.isHostile(uid, target) then return "hostile" end
    if att and M.selfDefenseFresh(uid, s, target, att) then
        return "self_defense"
    end
    return nil
end

local function authorized(uid, s, target)
    local inputs = M.fixtureInputs(uid)
    local a = inputs and inputs.authorization
    local occ = occurrences[s]
    return a ~= nil and occ ~= nil and not occ.retargeted
        and a.uid == uid and a.target == target and occ.target == target
        and a.occurrence == occ.id
end

-- Continuation of the live attack goal. Returns ok, reason.
function M.mayContinue(uid, s)
    if not M.governs(s) then return true end
    local target = s.attackTargetUid
    if not target then return true end
    if M.isHostile(uid, target) then return true end
    if M.selfDefenseFresh(uid, s, target, unit.getLastAttacker(uid)) then
        return true
    end
    if authorized(uid, s, target) then return true end
    local occ = occurrences[s]
    if occ and occ.basis == "self_defense" then
        return false, "self-defense ended"
    end
    return false, "faction relation is no longer hostile"
end

return M
