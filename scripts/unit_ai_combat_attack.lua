-- Unit AI attack-target combat swings (#538 split from unit_ai.lua).
--
-- Combat animation naming/injury detection plus the attack_target
-- action: closing distance, swing timing, and mode selection. Requires
-- unit_ai_combat.lua for the attack-mode helpers (stamina/mode/
-- cooldown). The lunge sequence for short-reach attackers is DISPATCHED
-- from this action but owned by unit_ai_combat_lunge.lua (#1713) --
-- it needs a hook in unit_ai.lua's tickOne that no execute can provide.

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
-- The mid-fight retaliation swap below compares against this window; it
-- is DEFINED in unit_ai_combat.lua (one definition, imported here rather
-- than duplicated). Before #1483 the split left the name unbound here,
-- so the comparison read a nil global and raised.
local RETALIATE_WINDOW_SEC  = combatCore.RETALIATE_WINDOW_SEC

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
    nomad_primitive = {
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
    -- In the combat band (8.0), same as engage. A HUMAN move issued while
    -- this goal is live scores 9.0 in unit_ai_combat.lua and interrupts us,
    -- except while a committed swing finishes or when an in-range actor has
    -- at least 4× the target's effectiveness. Scripted/internal moves retain
    -- the ordinary 7.0 ladder. Retreat keeps its independent, potentially
    -- higher utility. This is #916's replacement for the stale #306 claim
    -- that every attack must always outrank every pending move.
    return 8.0
end

-- Helper: pop the attack-target's anim override safely. Used when
-- the goal terminates (target dead, gone, mid-fight switch) so we
-- don't leave the unit frozen in a combat-idle stance forever.
local function clearAttackAnim(uid)
    unit.clearAnimOverride(uid)
end

-- The lunge — a short-reach attacker's leap→land→strike sequence — lives
-- in scripts/unit_ai_combat_lunge.lua (#1713). It spans several ticks and
-- spends most of them AIRBORNE, where the dispatcher suppresses execution
-- entirely, so its bookkeeping needs an observer on unit_ai.lua's
-- short-circuit paths as well as this execute; that module owns both
-- halves and the seven-field cleanup they share.
local lunge = require("scripts.unit_ai_combat_lunge")


local function attackTargetExecute(uid, s, params)
    local target = s.attackTargetUid
    -- Abandoning the attack goal ends any lunge with it (#1713): these
    -- three returns are terminal for the whole engagement, and the phase-2
    -- gate below -- the only other place that would notice -- is never
    -- reached again once the goal is gone. Leaving the bookkeeping behind
    -- would strand the persisted lungeTarget reference.
    if not target then
        lunge.clear(s)
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
        lunge.clear(s)
        s.attackTargetUid = nil
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end
    if unit.getPose(target) == "dead" then
        engine.logDebug("attack: target " .. tostring(target)
                        .. " is dead, clearing goal")
        lunge.clear(s)
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
        -- incapable of being a live threat worth swapping onto — and
        -- the technomule is never a valid combat target at all (#717:
        -- this also closes the gap where lash-out's own collapsed/
        -- technomule exclusions could otherwise be bypassed via this
        -- shared retaliation path, since attack_target is the same
        -- execute function lash-out drives).
        local attPose = att and unit.getPose(att.uid)
        local attInfo = att and unit.getInfo(att.uid)
        -- #916 ruin encounters own target acquisition completely: their
        -- exact hostile/same-page/visible rule selected this target, so the
        -- generic retaliation path must not swap to an arbitrary recent
        -- attacker that never passed it.
        if not s.ruinEncounterCombat
           and att and att.uid ~= target and unit.exists(att.uid)
           and attPose ~= "dead" and attPose ~= "collapsed"
           and attInfo and attInfo.defName ~= "technomule"
           and (engine.gameTime() - (att.at or 0)) <= RETALIATE_WINDOW_SEC then
            local m = unit.getInfo(uid)
            if m then
                local d = math.max(math.abs(m.gridX - attInfo.gridX),
                                   math.abs(m.gridY - attInfo.gridY))
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
    if lunge.tryLunge(uid, s, target, me, you, chebyshev) then return end

    -- ADVISORY, NOT AUTHORITATIVE (#2328). This range gate and the
    -- stance gate below are the ADMISSION and animation drivers: they
    -- decide whether to throw a swing this tick and which one. They do
    -- not decide whether it lands. `combat.attack` only enqueues, and
    -- the combat worker drains the queue on its own tick, so the world
    -- can move underneath a queued strike. Combat.Resolution.Admission
    -- re-checks page, this same horizontal reach, and this same stance
    -- against the LIVE units immediately before the strike commits, and
    -- refuses it outright otherwise. Keep the two in step: both sides
    -- measure Chebyshev separation against `unit.getAttackRange` and
    -- both spend the same per-mode stance costs.
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
