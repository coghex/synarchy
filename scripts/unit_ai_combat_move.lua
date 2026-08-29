-- Player move-order arbitration while combat is active (#916).

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local pace = require("scripts.unit_ai_pace")
local stall = require("scripts.unit_ai_stall")

local M = {}

local FOLLOW_COMMAND_UTILITY = 7.0
local COMBAT_MOVE_UTILITY = 9.0
local COMBAT_MOVE_PROOF_SEC = 2.0
local COMBAT_MOVE_PROGRESS_TILES = 0.1

local function activeCombatTarget(s)
    if not core.isGoalActive(s, "attack") or not s.attackTargetUid then
        return nil
    end
    if not unit.exists(s.attackTargetUid)
       or unit.getPose(s.attackTargetUid) == "dead" then return nil end
    return s.attackTargetUid
end

local function highConfidenceFinish(uid, target)
    local me, you = unit.getInfo(uid), unit.getInfo(target)
    if not me or not you or me.page ~= you.page then return false end
    local dist = math.max(math.abs(me.gridX - you.gridX),
                          math.abs(me.gridY - you.gridY))
    if dist > (unit.getAttackRange(uid) or 1.0) then return false end
    return unitAi.combatEffectiveness(uid)
        >= 4 * unitAi.combatEffectiveness(target)
end

local function resetProof(task)
    task.combatMoveControlAt = nil
    task.combatMoveStartX = nil
    task.combatMoveStartY = nil
end

local function followCommandUtility(uid, s)
    local task = s.commandedTask
    if not task then return -math.huge end
    local target = task.player and activeCombatTarget(s) or nil
    if not target then
        resetProof(task)
        return FOLLOW_COMMAND_UTILITY
    end
    -- A swing or movement transition already committed to its animation
    -- finishes. A lunge is an attack transition even while the unit activity
    -- is airborne rather than literally `transitioning`. Retreat is a
    -- different escape already in progress and therefore keeps its own score.
    if engine.gameTime() < (s.attackSwingUntil or 0)
       or s.lungePhase ~= nil
       or unit.getActivity(uid) == "transitioning"
       or core.isGoalActive(s, "retreat")
       or highConfidenceFinish(uid, target) then
        resetProof(task)
        return FOLLOW_COMMAND_UTILITY
    end
    if task.combatMoveControlAt then
        local info = unit.getInfo(uid)
        if info and engine.gameTime() - task.combatMoveControlAt
           >= COMBAT_MOVE_PROOF_SEC then
            local moved = math.sqrt((info.gridX - task.combatMoveStartX) ^ 2
                                  + (info.gridY - task.combatMoveStartY) ^ 2)
            if moved < COMBAT_MOVE_PROGRESS_TILES then
                unit.stop(uid)
                core.reportFailure(uid,
                    "Couldn't break away from combat — move order abandoned")
                s.commandedTask = nil
                return -math.huge
            end
            resetProof(task)
        end
    end
    return COMBAT_MOVE_UTILITY
end

local function followCommandExecute(uid, s)
    local task = s.commandedTask
    if not task then return end
    stall.noteWalk(uid, task)
    if task.player and activeCombatTarget(s) and not task.combatMoveControlAt then
        local info = unit.getInfo(uid)
        if info then
            -- The committed transition has finished by the time utility lets
            -- us enter. Release combat_idle / attack overrides so state-driven
            -- walking animates the withdrawal rather than sliding away.
            unit.clearAnimOverride(uid)
            task.combatMoveControlAt = engine.gameTime()
            task.combatMoveStartX = info.gridX
            task.combatMoveStartY = info.gridY
        end
    end
    if task.speed then
        task.paceMode = nil
        unit.moveTo(uid, task.x, task.y, task.speed)
        return
    end
    task.paceMode = pace.initialPaceMode(uid)
    unit.moveTo(uid, task.x, task.y, pace.paceSpeed(uid, task.paceMode))
end

M.FOLLOW_COMMAND_UTILITY = FOLLOW_COMMAND_UTILITY
M.followCommandUtility = followCommandUtility
M.followCommandExecute = followCommandExecute

return M
