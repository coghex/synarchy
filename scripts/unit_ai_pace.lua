-- Sustained-travel adaptive pacing (#999, split out of unit_ai_combat.lua
-- to stay under its 500-line budget). `ordered` (comfort×1.15) is a small,
-- deliberate stamina DEFICIT — sustainable only briefly, not indefinitely
-- — so a long uninterrupted follow_command move used to hold it until the
-- unit collapsed, then resume it again on revival: run, faint, run, faint.
-- Requirement: stamina must feed back into the commanded pace without
-- ever reissuing unit.moveTo mid-walk (that resets usLocalPath and stalls
-- pathing, #999 review) — so this uses unit.setMoveSpeed, which retargets
-- an in-flight move's speed only.
--
-- Required by unit_ai_combat.lua (follow_command's execute + the
-- attack-mode helpers, which also need staminaPct) and by
-- unit_ai_combat_attack.lua transitively through it.

local mv = require("scripts.movement_speed")

local M = {}

-- Stamina pct, robust to species that haven't been wired into
-- unit_resources.lua yet. A unit with no stamina stat at all is treated
-- as healthy (100%) — both combat and adaptive pacing assume "stamina
-- works" so the absence of config doesn't permanently lock a unit into
-- quick-mode / a draining push. Covers the technomule (no stamina stat
-- at all, by design) and a freshly-spawned acolyte before its first
-- resource-init tick.
local function staminaPct(uid)
    local s  = unit.getStat(uid, "stamina")
    local ms = require("scripts.unit_stats").get(uid, "max_stamina")
    if s and ms and ms > 0 then
        return math.max(0, math.min(1, s / ms))
    end
    return 1.0
end

-- Two-state hysteresis (push/recover) rather than a continuously
-- recomputed speed: a smoothly-varying speed would still cross the
-- walk/run gait threshold back and forth as stamina hovers near a
-- boundary. Different down/up thresholds (0.55 down, 0.8 up) give a
-- comfortable margin so the switch doesn't chatter and each phase holds
-- long enough to matter:
--   * PACE_DOWNSHIFT_FRAC sits well above collapse_threshold (0.1), so a
--     healthy unit backs off long before it would ever collapse.
--   * It also sits just above stamina's revive_threshold (0.5), so a
--     unit that just woke from a collapse (~0.5) resumes in "recover"
--     rather than immediately lurching back to a draining push.
--   * PACE_RECOVER_FRAC is strictly below 1.0× comfort (comfort itself
--     is the stamina-neutral speed by construction, see
--     scripts/movement_speed.lua) so a recovering unit's stamina
--     actually climbs instead of merely not-falling — while still
--     comfortably faster than ambient meander.
--
-- Recover pace is GRADE-AWARE (mv.speedForEffort), not a flat fraction
-- of comfort applied to raw speed: sustained uphill travel (#375) taxes
-- EFFECTIVE effort (speed × (1 + K·grade)), not raw speed, so a fixed
-- 0.75×comfort recovery speed would still net-drain on a steep enough
-- ascent — recover mode would hold, never actually recover, and a long
-- enough climb would cycle collapse/revive same as the original bug.
-- Targeting effort (not speed) keeps the recovery drain ratio constant
-- at PACE_RECOVER_FRAC regardless of terrain.
local PACE_DOWNSHIFT_FRAC = 0.55
local PACE_RESUME_FRAC    = 0.8
local PACE_RECOVER_FRAC   = 0.75

-- The initial pace for a fresh (or just-resumed) commandedTask: push
-- unless the unit is already below the downshift line, so a unit
-- commanded while already tired doesn't lurch to full ordered speed for
-- one tick only to downshift again immediately.
local function initialPaceMode(uid)
    return (staminaPct(uid) < PACE_DOWNSHIFT_FRAC) and "recover" or "push"
end

local function currentGrade(uid)
    local info = unit.getInfo(uid)
    return (info and info.moveGrade) or 0
end

local function paceSpeed(uid, mode)
    if mode == "recover" then
        return mv.speedForEffort(mv.comfort(uid) * PACE_RECOVER_FRAC,
                                  currentGrade(uid))
    end
    return mv.ordered(uid)
end

-- Per-tick pacing feedback for an ACTIVE follow_command move. Called
-- unconditionally every AI tick (unit_ai.lua's tickOne, alongside
-- core.maintainTask) rather than through the switch/idle execute gate,
-- since that gate deliberately only re-runs an action's execute on a
-- switch or an idle re-decision — exactly the reissue this must avoid.
-- Only ever calls unit.setMoveSpeed, never unit.moveTo, so the unit's
-- in-flight local path is never disturbed.
local function followCommandPaceTick(uid, s)
    if s.currentAction ~= "follow_command" then return end
    local task = s.commandedTask
    if not task or task.speed then return end

    local pct  = staminaPct(uid)
    local mode = task.paceMode or "push"
    if mode == "push" and pct < PACE_DOWNSHIFT_FRAC then
        mode = "recover"
    elseif mode == "recover" and pct > PACE_RESUME_FRAC then
        mode = "push"
    end
    local switchedMode = mode ~= task.paceMode
    task.paceMode = mode

    -- Push is a flat, terrain-independent speed (mv.ordered) — only
    -- worth retargeting on an actual mode switch. Recover is
    -- grade-aware (paceSpeed → mv.speedForEffort), so a sustained climb
    -- with changing grade (steeper/shallower segments) must keep
    -- retargeting every tick to stay undercutting comfort throughout,
    -- not just at the moment it dropped into recover.
    if switchedMode or mode == "recover" then
        unit.setMoveSpeed(uid, paceSpeed(uid, mode))
    end
end

M.staminaPct           = staminaPct
M.initialPaceMode      = initialPaceMode
M.paceSpeed            = paceSpeed
M.followCommandPaceTick = followCommandPaceTick

return M
