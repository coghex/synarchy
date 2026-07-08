-- Unit AI combat priority ladder (#538 split from unit_ai.lua).
--
-- Who to fight, when to flee, and when to break off. follow_command's
-- utility/execute live here too (the priority-ladder comment applies
-- to both) since they're the baseline retreat/engage compare against.
-- Swing mechanics (anim, lunge, cooldown math consumers) live in
-- unit_ai_combat_attack.lua, which requires this file for the attack-
-- mode helpers (staminaPct/chooseAttackMode/computeAttackCooldown).

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local isGoalActive        = core.isGoalActive
local setGoal             = core.setGoal
local markGoalAccomplished = core.markGoalAccomplished
local ensureState         = core.ensureState

local mv = require("scripts.movement_speed")

local M = {}

-----------------------------------------------------------
-- Action: follow_command
-----------------------------------------------------------
-- An explicit player move order outranks routine autonomous behaviour:
-- above ambient wander, routine work (build/deliver/store/dig ≤6.0), and
-- the situational goals (find_water/notify) — so a right-click reliably
-- redirects a unit that is merely wandering, working, or scouting.
--
-- It is OUTRANKED by (the #306 ladder, re-derived against this 7.0
-- baseline — NOT the historical 1.0):
--   * dire SELF survival — drink (thirst·15, crosses 7.0 at thirst≈0.47)
--     and eat (≥7.5 whenever it fires), and a dry-canteen refill (peaks
--     7.5 near-empty): a unit tends to its own body first, then resumes;
--   * combat — engage/retreat (8.0+): a unit defends itself or flees a
--     hopeless fight rather than walking to a commanded tile;
--   * treatment — treat_ally (8.0): a medic finishes saving a life.
-- Peer to it: an explicit ground-item pickup (7.5) — also a player order,
-- nudged just above so it wins the move-vs-pickup tie.
-- commandedTask persists until maintainTask clears it on arrival/timeout,
-- so the unit resumes the move once the higher-priority action finishes.
local FOLLOW_COMMAND_UTILITY = 7.0

local function followCommandUtility(uid, s, params)
    if not s.commandedTask then return -math.huge end
    return FOLLOW_COMMAND_UTILITY
end

local function followCommandExecute(uid, s, params)
    local task = s.commandedTask
    if not task then return end
    -- Player-ordered move: a slight sustainable push above comfort
    -- (ordered regime), unless the command specified an explicit speed.
    unit.moveTo(uid, task.x, task.y, task.speed or mv.ordered(uid))
end


-----------------------------------------------------------
-- Action: retreat
--
-- Universal "this fight is futile, run" candidate. Outscores
-- attack_target so a wounded outmatched unit breaks contact.
--
-- Triggers when:
--   1. We're on an attack goal AND
--   2. We've been hit by our attack target (per uiLastAttackerUid) AND
--   3. Group combat effectiveness (us + anyone else engaging the same
--      target) is less than 1/1.5 of the threat's own effectiveness.
--
-- Once triggered, sets activeGoal = "retreat" + s.retreatThreatUid;
-- attack_target then naturally yields (its utility checks activeGoal
-- == "attack"). The candidate keeps re-executing while in retreat
-- to re-path away from a chasing threat.
--
-- Termination: threat dies, threat despawns, or we're more than
-- RETREAT_SAFE_DIST chebyshev tiles away. Then we clear the goal
-- and ambient candidates take back over.
-----------------------------------------------------------
local RETREAT_FUTILITY_RATIO = 1.5
-- 2e: a unit under a player/scripted ORDER is much braver — it weighs the
-- group by the same ratio but tolerates being outmatched far further before
-- breaking. SOFT override (the user's choice): a truly hopeless fight (beyond
-- this) still routs it; an order isn't a fight-to-the-last-cell compulsion.
local RETREAT_FUTILITY_RATIO_COMMITTED = 4.0
local RETREAT_SAFE_DIST      = 12.0    -- chebyshev tiles
local RETREAT_TARGET_DIST    = 8.0     -- how far to pick a new retreat tile


local function selfWoundedByTarget(uid, s)
    if not s.attackTargetUid then return false end
    local att = unit.getLastAttacker(uid)
    if not att then return false end
    return att.uid == s.attackTargetUid
end



-- True iff this unit's threat (per attack goal) significantly
-- outclasses everyone currently engaging it. Threshold defaults to
-- 1.5×.
local function futilityCheck(uid, s)
    local threatUid = s.attackTargetUid
    if not threatUid then return false, 0 end
    if not unit.exists(threatUid) then return false, 0 end
    if unit.getPose(threatUid) == "dead" then return false, 0 end
    local threatEff = unitAi.combatEffectiveness(threatUid)
    local groupEff  = unitAi.groupEffectivenessVs(threatUid)
    if groupEff <= 0 then return false, 0 end
    local ratio = threatEff / groupEff
    -- A unit under orders (player/scripted commandAttack) holds far longer.
    local cap = s.committed and RETREAT_FUTILITY_RATIO_COMMITTED
                            or  RETREAT_FUTILITY_RATIO
    return ratio > cap, ratio
end

local function retreatUtility(uid, s, params)
    -- Carry-through: as long as the unit is in retreat goal, keep
    -- the candidate dominant — 8.0 matches the engage floor, above a
    -- player move order (7.0) so fleeing for your life isn't cancelled
    -- by a stale move command (#306).
    if isGoalActive(s, "retreat") then return 8.0 end
    if not isGoalActive(s, "attack") then return -math.huge end
    if not selfWoundedByTarget(uid, s) then return -math.huge end
    local futile, ratio = futilityCheck(uid, s)
    if not futile then return -math.huge end
    -- Urgency grows with ratio from the combat floor (8.0). ratio=1.5
    -- → 8.0; ratio=3.0 → 11.0 — a hopeless fight outranks even dire
    -- needs so the unit commits to escaping.
    return 8.0 + (ratio - RETREAT_FUTILITY_RATIO) * 2.0
end

local function retreatExecute(uid, s, params)
    -- First entry into retreat: transfer the threat ref + switch goal.
    -- Clear any combat-anim override left over from attack_target so
    -- the engine's state-driven walking anim plays during the flight.
    if not isGoalActive(s, "retreat") then
        s.retreatThreatUid = s.attackTargetUid
        s.attackTargetUid  = nil
        s.committed        = nil   -- broke despite the order; drop commitment
        markGoalAccomplished(s, "attack")
        setGoal(s, "retreat")
        unit.clearAnimOverride(uid)
        engine.logDebug("retreat: " .. tostring(uid)
            .. " breaks from " .. tostring(s.retreatThreatUid))
    end

    local threat = s.retreatThreatUid
    if not threat or not unit.exists(threat)
       or unit.getPose(threat) == "dead" then
        -- Threat gone; we're safe.
        s.retreatThreatUid = nil
        markGoalAccomplished(s, "retreat")
        return
    end

    local me  = unit.getInfo(uid)
    local you = unit.getInfo(threat)
    if not me or not you then return end

    local dx = me.gridX - you.gridX
    local dy = me.gridY - you.gridY
    local dist = math.max(math.abs(dx), math.abs(dy))
    if dist > RETREAT_SAFE_DIST then
        engine.logDebug("retreat: " .. tostring(uid)
            .. " reached safe distance from " .. tostring(threat))
        s.retreatThreatUid = nil
        markGoalAccomplished(s, "retreat")
        return
    end

    -- Pick a tile in the direction away from the threat.
    local mag = math.sqrt(dx * dx + dy * dy)
    if mag < 0.001 then
        -- Co-located: pick an arbitrary direction.
        dx, dy = 1, 0
        mag = 1
    end
    local tx = me.gridX + (dx / mag) * RETREAT_TARGET_DIST
    local ty = me.gridY + (dy / mag) * RETREAT_TARGET_DIST

    -- Re-issue moveTo only when the unit is idle, OR the destination
    -- has drifted by > 0.5 tiles (threat moved): same heuristic as
    -- attack_target's pursuit moves. Avoids stomping usLocalPath.
    local last = s.retreatLastMoveTo
    local needRepath = unit.getActivity(uid) == "idle"
        or not last
        or math.abs(last.x - tx) > 0.5
        or math.abs(last.y - ty) > 0.5
    if needRepath then
        -- Fleeing → sprint: max_speed × agility, "as fast as they can".
        unit.moveTo(uid, tx, ty, mv.sprint(uid))
        s.retreatLastMoveTo = { x = tx, y = ty }
    end
end


-----------------------------------------------------------
-- Action: engage
--
-- Universal "should I pick a fight, and with whom?" decision layer.
-- THREAT_SOURCES is a table of independent threat detectors; each
-- returns a {uid, score} pair or nil. engage picks the highest-
-- scoring threat and triggers `commandAttack` — actual fighting
-- runs through the regular `attack_target` candidate from there.
--
-- Phase 2.2 ships with a single source (incoming_hit, replacing
-- the old standalone "retaliate" candidate). Phase 2.3+ adds:
--   * hostile_in_sight — visible unit of opposing faction.
--   * prey_hunger — hungry predator + viable prey visible.
--   * defend_ally — friendly being attacked nearby.
-- Each new source is a table entry; the picker stays the same.
--
-- Score is the natural utility-comparison number: 8.0 for fresh
-- incoming-hit retaliation, above a commanded move (follow_command
-- 7.0) but below dire-need candidates (thirst / hunger scale past it)
-- so a starving bear still drinks before fighting (#306).
-----------------------------------------------------------
local ENGAGE_WINDOW_SEC = 10.0
-- How recently a melee hit must have landed for the in-combat target swap
-- to round on the new attacker (shorter than the engage window so the bear
-- chases a fled target rather than ping-ponging to whoever poked it once).
local RETALIATE_WINDOW_SEC = 3.0

local THREAT_SOURCES = {
    -- Recently took damage from someone who is still alive. The
    -- 10s window resets on each new hit (Combat.Resolution stamps
    -- uiLastAttackerAt on every wound), so sustained combat keeps
    -- us engaged; a fled attacker stops being relevant after 10s.
    {
        name = "incoming_hit",
        score = function(uid, s, params)
            local att = unit.getLastAttacker(uid)
            if not att then return nil end
            if engine.gameTime() - (att.at or 0)
               > ENGAGE_WINDOW_SEC then return nil end
            if not unit.exists(att.uid) then return nil end
            if unit.getPose(att.uid) == "dead" then return nil end
            -- 8.0 sits above a player move order (follow_command 7.0)
            -- and well above the goal candidates (find_water/notify,
            -- ≤6): a unit defends itself when struck rather than
            -- walking off to a commanded tile or resuming a routine
            -- search. Dire SELF needs still beat us (drinking when
            -- near-empty / eating when starving scale past 8.0), the
            -- intended scale being: literally-dying > combat/treatment
            -- > player-issued moves > general goals > ambient (#306).
            return { uid = att.uid, score = 8.0 }
        end,
    },
    -- Future sources go here, e.g.:
    -- { name = "hostile_in_sight", score = function(uid, s, params)
    --       … FOV scan, faction filter, distance-weighted score … end },
}

-- Shared by utility + execute so they never disagree about which
-- target won. Returns (uid, score) or (nil, -math.huge).
local function pickThreat(uid, s, params)
    local bestUid, bestScore = nil, -math.huge
    for _, src in ipairs(THREAT_SOURCES) do
        local t = src.score(uid, s, params)
        if t and t.score > bestScore then
            bestUid, bestScore = t.uid, t.score
        end
    end
    return bestUid, bestScore
end

local function engageUtility(uid, s, params)
    -- Already on an attack goal: defer to attack_target. Engage
    -- only fires the *initial* target-selection; mid-fight target
    -- swaps land in a later slice.
    if isGoalActive(s, "attack") then return -math.huge end
    -- Don't re-engage the same threat we're actively fleeing from.
    -- retreat clears its goal once we're safe; until then we stay
    -- in flight even if the threat keeps landing hits.
    if isGoalActive(s, "retreat") then return -math.huge end
    local _, score = pickThreat(uid, s, params)
    return score
end

local function engageExecute(uid, s, params)
    local target, _ = pickThreat(uid, s, params)
    if not target then return end
    -- Reuse commandAttack so the goal + state are set identically
    -- to a player-issued order. attack_target wins on the next tick.
    unitAi.commandAttack(uid, target)
    engine.logDebug("engage: " .. tostring(uid)
        .. " engages " .. tostring(target))
end

-----------------------------------------------------------
-- Heavy / quick attack mode + dynamic recovery time.
--
-- Two attack modes, picked per swing. Heavy commits the body forward
-- and applies linear strength; quick is a controlled motion that
-- applies sqrt(strength). The damage gap between them therefore comes
-- entirely from the strength stat — a high-str unit gains a lot from
-- going heavy, a low-str unit gains nothing and never picks it. No
-- per-mode damage multiplier; the engine's Combat.Resolution makes
-- the differential stat-driven.
--
-- Recovery time (cooldown between swings) is computed live each tick
-- from:
--   * base attack_cooldown declared on the weapon / natural_weapon
--   * mode multiplier (heavy = 1.6× the quick base)
--   * weight factor: heavy weapons take longer to recover from,
--     scaled by the wielder's strength
--   * stamina factor: exhausted units recover slower
--   * injury factor: wounds on the weapon-arm slow you sharply
--   * stat factor: agility × dexterity make you faster
--
-- All inputs are stats the engine already tracks. No species-specific
-- knobs — a healthy acolyte and a healthy bear land at similar swings/
-- second despite very different stat profiles, because each species'
-- strengths and weaknesses cancel out through the formula.
-----------------------------------------------------------

-- Heavy mode is worth choosing when stamina is high AND the wielder
-- can actually deliver more damage (strength > 1.0; at str=1 heavy and
-- quick deal identical damage, so paying the extra stamina is a loss).
-- A wound on the weapon arm at severity ≥ 0.5 makes heavy unusable —
-- you can't put the body in to a swing if the arm holding the weapon
-- is torn up.
-- Stamina pct, robust to species that haven't been wired into
-- unit_resources.lua yet. A unit with no stamina stat at all is
-- treated as healthy (100%) — combat assumes "stamina works" so
-- the absence of config doesn't permanently lock the unit into
-- quick-mode. The unit_resources tick handles the drain regardless.
local function staminaPct(uid)
    local s  = unit.getStat(uid, "stamina")
    local ms = require("scripts.unit_stats").get(uid, "max_stamina")
    if s and ms and ms > 0 then
        return math.max(0, math.min(1, s / ms))
    end
    return 1.0
end

local function chooseAttackMode(uid)
    local pct = staminaPct(uid)
    if pct < 0.5 then return "quick" end

    local str = unit.getStat(uid, "strength") or 1.0
    if str <= 1.0 then return "quick" end

    local armPart = unit.getWeaponWieldedFrom(uid)
    if armPart then
        local armWound = unit.getWoundSeverityOn(uid, armPart) or 0
        if armWound >= 0.5 then return "quick" end
    end

    -- Soft utility curve: heavy preference rises from 0 at 50% stamina
    -- to 1.0 at 100% stamina. No flicker at any threshold.
    local heavyPref = (pct - 0.5) / 0.5
    if heavyPref > 0.5 then return "heavy" end
    return "quick"
end

local function computeAttackCooldown(uid, mode)
    local base = unit.getAttackCooldown(uid) or 1.5
    local modeMult = (mode == "heavy") and 1.6 or 1.0

    -- Weight factor: only weapons heavier than the reference (1 kg)
    -- add cost, and the cost scales inversely with strength. A
    -- powerful wielder isn't slowed by a heavy weapon they can muscle.
    local weight   = unit.getEquippedWeaponWeight(uid) or 1.0
    local str      = unit.getStat(uid, "strength") or 1.0
    local weightF  = 1 + math.max(0, weight - 1.0) / (5 * str)

    -- Stamina factor: 1.0 at full, 1.5 at empty. Exhausted units pay
    -- 50% longer recoveries. staminaPct returns 1.0 for species
    -- without resource config, so this is a no-op until the species
    -- is wired in — the formula still works for combat-only checks.
    local pct   = staminaPct(uid)
    local stamF = 1 + (1 - pct) * 0.5

    -- Injury factor: a severity-0.5 wound on the weapon arm doubles
    -- recovery. Severity-1.0 (vital arm, unit usually dead) triples.
    local armPart  = unit.getWeaponWieldedFrom(uid)
    local armWound = armPart
                     and (unit.getWoundSeverityOn(uid, armPart) or 0)
                     or 0
    local injuryF  = 1 + 2 * armWound

    -- Stat factor: agility and dexterity together govern raw motor
    -- speed. 1/sqrt(agi×dex) puts a 1.0/1.0 unit at the baseline and
    -- a 2.0/2.0 superhero at half the recovery.
    local agi = unit.getStat(uid, "agility")   or 1.0
    local dex = unit.getStat(uid, "dexterity") or 1.0
    local statF = 1 / math.sqrt(math.max(0.05, agi * dex))

    return base * modeMult * weightF * stamF * injuryF * statF
end


M.followCommandUtility   = followCommandUtility
M.followCommandExecute   = followCommandExecute
M.retreatUtility         = retreatUtility
M.retreatExecute         = retreatExecute
M.engageUtility          = engageUtility
M.engageExecute          = engageExecute
M.staminaPct             = staminaPct
M.chooseAttackMode       = chooseAttackMode
M.computeAttackCooldown  = computeAttackCooldown

return M
