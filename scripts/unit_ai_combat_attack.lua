-- Unit AI attack-target combat swings (#538 split from unit_ai.lua).
--
-- Combat animation naming/injury detection plus the attack_target
-- action: closing distance, the lunge sequence for short-reach
-- attackers, swing timing, and mode selection. Requires
-- unit_ai_combat.lua for the attack-mode helpers (stamina/mode/
-- cooldown) shared with the lunge decision.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local isGoalActive         = core.isGoalActive
local markGoalAccomplished = core.markGoalAccomplished

local mv = require("scripts.movement_speed")

-- NB: named combatCore, not `combat` — the latter is the engine's
-- global combat API (combat.attack, called below by this file's own
-- swing logic), and a local of the same name would shadow it.
local combatCore = require("scripts.unit_ai_combat")
local staminaPct            = combatCore.staminaPct
local chooseAttackMode      = combatCore.chooseAttackMode
local computeAttackCooldown = combatCore.computeAttackCooldown

local M = {}

-----------------------------------------------------------
-- Combat animation helpers
--
-- The engine's publishToRender resolves animation names from a
-- (pose, activity) state-key lookup, which has no slot for combat
-- attack swings or weapon-class variants. We bypass that by writing
-- to uiAnimOverride directly via unit.setAnimOverride; the engine
-- preserves the override until clearAnimOverride flips it off.
--
-- Animation name conventions vary per species:
--   * acolyte: "<injured_>BASE<_RH_dagger | _unarmed>" — combat
--              anims carry the weapon-class suffix.
--   * bear:    "<injured_>BASE" — bears have no weapon variants
--              because their natural weapon (claws/fangs) is the
--              only one they ever fight with.
-- Add to COMBAT_ANIM_SUFFIX when new species ship with their own
-- combat anim sets.
-----------------------------------------------------------

local COMBAT_ANIM_SUFFIX = {
    acolyte = {
        dagger  = "_RH_dagger",
        unarmed = "_unarmed",
    },
    bear_brown = {
        unarmed = "",   -- bear anim files have no class suffix
    },
}

-- "Injured" for animation purposes = cumulative EFFECTIVE wound severity
-- > 1.0 (sum across all active wounds). A single bad slash or several
-- moderate ones flip the unit to the limp/struggling combat anim;
-- a couple of light scratches don't. Tunable via INJURED_THRESHOLD.
-- Sum the engine's effective severity (severityEffective = max(acute,
-- necrosis), from woundEffSeverity) so this stays in lockstep with the
-- engine-side injured-anim flag (Unit.Thread.publishToRender sums
-- woundEffSeverity); a rotting wound counts even once the cut has closed.
local INJURED_THRESHOLD = 1.0
local function isInjured(uid)
    local wounds = unit.getWounds(uid)
    if not wounds then return false end
    local total = 0
    for _, w in ipairs(wounds) do
        total = total + (w.severityEffective or w.severity or 0)
        if total > INJURED_THRESHOLD then return true end
    end
    return false
end

-- Compose a combat anim name from the base ("attack_quick" /
-- "attack_heavy" / "combat_idle" / "combat_hit_react") plus the
-- unit's def-specific weapon-class suffix plus an injured prefix
-- when the unit is wounded. Returns nil if the species has no
-- entry in COMBAT_ANIM_SUFFIX (which means we just don't override
-- and let the engine's state-driven anim play).
local function combatAnimName(uid, base)
    local info = unit.getInfo(uid)
    if not info or not info.defName then return nil end
    local suffixes = COMBAT_ANIM_SUFFIX[info.defName]
    if not suffixes then return nil end
    local class = unit.getWeaponClass(uid) or "unarmed"
    local suffix = suffixes[class]
    if not suffix then return nil end
    local injured = isInjured(uid) and "injured_" or ""
    return injured .. base .. suffix
end

-----------------------------------------------------------
-- Action: attack_target
--
-- Combat candidate. Set via unitAi.commandAttack(uid, targetUid).
-- Goal-driven so dire-need candidates (thirst, hunger) preempt
-- by outscoring this candidate's 1.0.
--
-- State carried on `s`:
--   s.activeGoal       = "attack" (via setGoal)
--   s.attackTargetUid  = uid of the target
--   s.attackLastSwingAt = gameTime of last fired swing (cooldown gate)
--   s.attackLastMode   = "heavy" | "quick" — for cooldown math, since
--                        the recovery from the PREVIOUS swing is what
--                        gates the next one.
--
-- Each tick, until the target is dead or gone:
--   * Target missing / dead → clear goal.
--   * In range AND cooldown elapsed → pick mode, fire combat.attack
--     with it, stamp attackLastSwingAt + attackLastMode.
--   * In range AND on cooldown → stand still and wait.
--   * Out of range → re-pathfind toward target's CURRENT tile so
--     moving targets get tracked. tickOne gates re-issuing moveTo
--     on activity == idle, so we don't wipe usLocalPath while the
--     unit is mid-walk.
-----------------------------------------------------------
local function attackTargetUtility(uid, s, params)
    if not isGoalActive(s, "attack") then return -math.huge end
    if not s.attackTargetUid then return -math.huge end
    -- In the combat band (8.0), same as engage/retreat. commandAttack
    -- sets the attack goal but leaves any pending commandedTask intact,
    -- so the pursuit MUST out-rank follow_command (7.0) — otherwise a
    -- stale move order would yank the unit straight back off the fight
    -- the tick after engage hands over (#306). Dire SELF survival
    -- (drink/eat scaling past 8) still pre-empts and resumes, and the
    -- move resumes once the attack goal ends (target dead/gone/fled).
    return 8.0
end

-- Helper: pop the attack-target's anim override safely. Used when
-- the goal terminates (target dead, gone, mid-fight switch) so we
-- don't leave the unit frozen in a combat-idle stance forever.
local function clearAttackAnim(uid)
    unit.clearAnimOverride(uid)
end

-- Only very-short-reach attackers lunge (slice 2a: the squirrel, reach
-- ~0.11). Normal melee units (reach ≥ this) just close and swing. The
-- general "skilled/unintelligent fighters also lunge" gating is later (2e).
-- Abort a lunge that never resolves (e.g. interrupted mid-air) after this.
local LUNGE_TIMEOUT_SEC = 3.0

-- ----- Lunge decision (2e: rarity, split by intelligence + skill) -----
-- A lunge is a deliberate, committed move — not something a unit does every
-- time it's out of reach. WHO lunges, and how readily, depends on the mind:
--   * Unintelligent creatures (intelligence < LUNGE_INSTINCT_INTEL — squirrels,
--     bears, mules) lunge on INSTINCT. For a short-reach predator/prey it's
--     often the ONLY way to reach a tall target, so the propensity is high;
--     the leap's REACH is still naturally bounded by the jumping skill (the
--     engine's getJumpReach), so a clumsy animal simply can't leap far.
--   * Intelligent fighters (acolytes) treat the lunge as a trained TECHNIQUE,
--     gated by the `jumping` skill — a novice (skill 10) almost never lunges;
--     a skilled one occasionally does to close a gap. Otherwise they walk in
--     and fight normally.
-- Either way it costs commitment, so it's gated on stamina.
local LUNGE_INSTINCT_INTEL    = 0.7   -- below = instinct regime (animals)
local LUNGE_INSTINCT_P        = 0.85  -- animal propensity when out of reach
local LUNGE_TECH_MAX_P        = 0.5   -- ceiling for a trained lunger
local LUNGE_TECH_SKILL_K      = 0.6   -- jumping/100 × this = technique chance
local LUNGE_MIN_STAMINA_FRAC  = 0.25  -- too winded to commit below this

-- Decide whether an out-of-reach unit commits to a lunge THIS attempt.
-- Rolled only when already eligible (out of reach, off cooldown), so the
-- attack cooldown spaces the rolls — a "no" just means the unit pursues on
-- foot this cycle and may roll again next time it's off cooldown.
local function shouldLunge(uid, s)
    -- Stamina gate — a leap is a big spend.
    local stam = unit.getStat(uid, "stamina")
    if stam then
        local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
        if maxStam and maxStam > 0 and stam / maxStam < LUNGE_MIN_STAMINA_FRAC then
            return false
        end
    end
    local intel = unit.getStat(uid, "intelligence") or 1.0
    local p
    if intel < LUNGE_INSTINCT_INTEL then
        p = LUNGE_INSTINCT_P                       -- instinct: readily
    else
        local jumping = unit.getSkill(uid, "jumping") or 0.0
        p = math.min(LUNGE_TECH_MAX_P, (jumping / 100.0) * LUNGE_TECH_SKILL_K)
    end
    return math.random() < p
end

-- The lunge: a short-reach unit leaps to land ADJACENT to a target it
-- can't otherwise reach, then strikes on arrival with a reach BONUS equal
-- to the leap's strike-reach — so the engine's height-gated part picker
-- can select the now-reachable high parts (neck/throat). Multi-tick:
-- issue the leap, wait for the airborne→land transition, then strike.
-- Returns true if it handled this tick (caller skips normal attack/pursue).
local function tryLunge(uid, s, target, me, you, chebyshev)
    local range = unit.getAttackRange(uid) or 1.0
    local now = engine.gameTime()

    -- Phase 2: airborne — wait until we've actually left the ground and
    -- come back down, then deliver the strike.
    if s.lungePhase == "air" then
        if now - (s.lungeStartAt or now) > LUNGE_TIMEOUT_SEC then
            s.lungePhase = nil; return false        -- bail; resume normal logic
        end
        local pose = unit.getPose(uid)
        if pose == "falling" then s.lungeSawAir = true end
        if s.lungeSawAir and pose == "standing"
           and unit.getActivity(uid) ~= "transitioning" then
            if unit.exists(target) and unit.getPose(target) ~= "dead" then
                unit.setAnimOverride(uid, "attack_quick")
                -- reach bonus lets the strike hit a high part; impact speed
                -- folds the leap's full-body momentum into the damage.
                combat.attack(uid, target, s.lungeMode or "quick",
                              s.lungeReach or 0, s.lungeImpactSpeed or 0)
                s.attackSwingUntil  = now + (unit.getAnimDuration(uid, "attack_quick") or 0.4)
                s.attackLastSwingAt = now
                s.attackLastMode    = s.lungeMode or "quick"
            end
            s.lungePhase = nil; s.lungeSawAir = nil
            s.lungeTarget = nil; s.lungeReach = nil; s.lungeImpactSpeed = nil
        end
        return true   -- consume the tick while leaping / landing
    end

    -- Phase 1: decide to leap. Must be out of melee reach, off cooldown,
    -- and the mind/skill check (2e) must elect to commit — otherwise fall
    -- through to normal pursue (walk closer and fight).
    if chebyshev <= range then return false end
    local last = s.attackLastSwingAt or 0
    if now - last < computeAttackCooldown(uid, s.attackLastMode or "quick") then
        return false
    end
    if not shouldLunge(uid, s) then return false end
    local jr = unit.getJumpReach(uid)
    if not jr or not jr.dist or jr.dist <= 0 then return false end

    -- Land one tile from the target, on our side. getInfo gridX/Y are
    -- CONTINUOUS positions, so floor to integer TILE coords (unit.jump
    -- needs integers, or its tointeger silently rejects the command).
    local mtx, mty = math.floor(me.gridX),  math.floor(me.gridY)
    local ttx, tty = math.floor(you.gridX), math.floor(you.gridY)
    local sgx = (mtx > ttx and 1) or (mtx < ttx and -1) or 0
    local sgy = (mty > tty and 1) or (mty < tty and -1) or 0
    local landX, landY = ttx + sgx, tty + sgy
    local ldx, ldy = landX - mtx, landY - mty
    local d = math.sqrt(ldx * ldx + ldy * ldy)

    if d < 0.5 then
        -- Already adjacent: a vertical pounce in place — full strike-reach,
        -- no horizontal leap (which the engine would refuse at d≈0).
        unit.setAnimOverride(uid, "attack_quick")
        combat.attack(uid, target, "quick", jr.height or 0,
                      unit.lungeImpactSpeed(uid, 0))
        s.attackSwingUntil  = now + (unit.getAnimDuration(uid, "attack_quick") or 0.4)
        s.attackLastSwingAt = now
        s.attackLastMode    = "quick"
        return true
    end
    if d > jr.dist then return false end

    -- Strike-reach envelope at this leap distance.
    local frac  = d / jr.dist
    local reach = (jr.height or 0) * (1 - frac * frac)
    if unit.jump(uid, landX, landY) then
        s.lungePhase        = "air"
        s.lungeSawAir       = false
        s.lungeStartAt      = now
        s.lungeTarget       = target
        s.lungeMode         = "quick"
        s.lungeReach        = reach
        s.lungeImpactSpeed  = unit.lungeImpactSpeed(uid, d)
    end
    return true
end

local function attackTargetExecute(uid, s, params)
    local target = s.attackTargetUid
    if not target then
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end
    -- Target existence + alive check. Phase 2 will layer in
    -- observed-status memory so a unit who hasn't seen the target
    -- doesn't blindly path toward it.
    if not unit.exists(target) then
        engine.logDebug("attack: target " .. tostring(target)
                        .. " gone, clearing goal")
        s.attackTargetUid = nil
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end
    if unit.getPose(target) == "dead" then
        engine.logDebug("attack: target " .. tostring(target)
                        .. " is dead, clearing goal")
        s.attackTargetUid = nil
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end

    -- Mid-fight RETALIATION: if someone other than the current target just
    -- hit us and they're within melee reach, turn on them. A predator
    -- chasing a fleeing victim will round on whoever's stabbing its flank
    -- instead of tunnel-visioning the runner. (The initial-engage path
    -- already handles first contact; this is the in-combat target swap.)
    do
        local att = unit.getLastAttacker(uid)
        -- A collapsed attacker is disqualified alongside dead — equally
        -- incapable of being a live threat worth swapping onto (#717:
        -- this also closes the gap where lash-out's own collapsed-
        -- target exclusion could otherwise be bypassed via this shared
        -- retaliation path, since attack_target is the same execute
        -- function lash-out drives).
        local attPose = att and unit.getPose(att.uid)
        if att and att.uid ~= target and unit.exists(att.uid)
           and attPose ~= "dead" and attPose ~= "collapsed"
           and (engine.gameTime() - (att.at or 0)) <= RETALIATE_WINDOW_SEC then
            local m = unit.getInfo(uid)
            local a = unit.getInfo(att.uid)
            if m and a then
                local d = math.max(math.abs(m.gridX - a.gridX),
                                   math.abs(m.gridY - a.gridY))
                if d <= (unit.getAttackRange(uid) or 1.0) + 0.5 then
                    s.attackTargetUid = att.uid
                    target = att.uid
                    clearAttackAnim(uid)
                end
            end
        end
    end

    local me  = unit.getInfo(uid)
    local you = unit.getInfo(target)
    if not me or not you then return end

    local dx = math.abs(me.gridX - you.gridX)
    local dy = math.abs(me.gridY - you.gridY)
    local chebyshev = (dx > dy) and dx or dy
    local range    = unit.getAttackRange(uid) or 1.0

    -- Short-reach units (the squirrel) lunge instead of futilely closing to
    -- a melee range they can never reach. Handles the whole leap→land→strike
    -- sequence over several ticks; if it acted, skip the normal path.
    if tryLunge(uid, s, target, me, you, chebyshev) then return end

    if chebyshev <= range then
        -- In range. If we were mid-walk, stop so the next AI tick
        -- sees activity == "idle" and we can settle into the
        -- cooldown loop. unit.stop is idempotent — fine to spam.
        if unit.getActivity(uid) == "walking" then
            unit.stop(uid)
        end
        -- Cooldown gate: recovery from the PREVIOUS swing is what
        -- governs when the next one can fire. So we read the
        -- cooldown using last swing's mode, not the upcoming one.
        local now  = engine.gameTime()
        local last = s.attackLastSwingAt or 0
        local prevMode = s.attackLastMode or "quick"
        local cooldown = computeAttackCooldown(uid, prevMode)
        -- Stance gate: you can't throw a swing you're not set for.
        -- Costs mirror Combat.Resolution (heavy 0.5, quick 0.25). If
        -- not set for heavy, downgrade to quick; if not even set for
        -- quick, hold the guard and recover (no swing this tick).
        local stance = unit.getStat(uid, "stance") or 1.0
        if now - last >= cooldown and stance >= 0.25 then
            -- Pick mode by current stamina + wounds + strength.
            -- Damage differential comes from the engine's strength
            -- application (sqrt(str) for quick, str for heavy); we
            -- just decide which swing to throw.
            local mode = chooseAttackMode(uid)
            if mode == "heavy" and stance < 0.5 then mode = "quick" end
            local base = (mode == "heavy") and "attack_heavy"
                                             or "attack_quick"
            local anim = combatAnimName(uid, base)
            if anim then
                unit.setAnimOverride(uid, anim)
                -- Hold the swing override for the swing animation's real
                -- length; otherwise the very next AI tick (still mid-
                -- cooldown) overwrites it with combat_idle before a
                -- single frame shows. 0.5s fallback if the duration is
                -- unknown.
                local dur = unit.getAnimDuration(uid, anim) or 0.5
                s.attackSwingUntil = now + dur
            end
            combat.attack(uid, target, mode)
            s.attackLastSwingAt = now
            s.attackLastMode    = mode
            engine.logDebug("attack: " .. tostring(uid)
                .. " " .. mode .. " at " .. tostring(target)
                .. " (cd=" .. string.format("%.2f", cooldown)
                .. "s, stance=" .. string.format("%.2f", stance)
                .. ", anim=" .. tostring(anim) .. ")")
        else
            -- Mid-cooldown — show the combat-idle stance instead of
            -- falling back to regular idle. But NOT while the last
            -- swing's animation is still playing (attackSwingUntil),
            -- or we'd cut the swing short. setAnimOverride is cheap to
            -- call every tick; engine treats same-anim writes as a
            -- no-op for playback timing.
            if now >= (s.attackSwingUntil or 0) then
                local anim = combatAnimName(uid, "combat_idle")
                if anim then unit.setAnimOverride(uid, anim) end
            end
        end
    else
        -- Out of range — clear the override so the engine's state-
        -- driven walking anim plays while we close on the target.
        unit.clearAnimOverride(uid)
        local last = s.attackLastMoveTo
        local dxLast = last and math.abs(last.x - you.gridX) or math.huge
        local dyLast = last and math.abs(last.y - you.gridY) or math.huge
        if unit.getActivity(uid) == "idle"
           or dxLast > 0.5 or dyLast > 0.5 then
            -- Close on the enemy at a STAMINA-AWARE pace. Sprint only while
            -- we have the wind for it; cruise when winded. Charging an empty
            -- tank just collapses us two tiles short — only fleeing for our
            -- lives (retreat) runs to exhaustion.
            local sp      = mv.sprint(uid)
            local stam    = unit.getStat(uid, "stamina")
            local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
            if stam and maxStam and maxStam > 0 then
                local frac = stam / maxStam
                if frac < 0.30 then sp = mv.comfort(uid)      -- winded: cruise
                elseif frac < 0.55 then sp = mv.ordered(uid)  -- tiring: push
                end
            end
            unit.moveTo(uid, you.gridX, you.gridY, sp)
            s.attackLastMoveTo = { x = you.gridX, y = you.gridY }
        end
    end
end


M.attackTargetUtility = attackTargetUtility
M.attackTargetExecute = attackTargetExecute

return M
