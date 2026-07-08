-- Unit AI ally notification (#538 split from unit_ai.lua).
--
--
-- Fires when activeGoal == "notify_allies" (set by scanForWater's
-- discovery branch, or by being notified in person). Two paths:
--
--  * Radio branch (unit carries a "radio" item): stand still for
--    notify_broadcast_seconds, then push knownWaterSources to every
--    other radio-bearing acolyte, marking their find_water goal
--    accomplished. The broadcaster's own notify_allies then completes.
--    In v1, every acolyte spawns with a radio, so this branch
--    informs the whole group in one hop.
--
--  * Walk branch (no radio): coordinated rank-based split. Each tick
--    we enumerate all current walk-notifiers sorted by uid, find this
--    unit's rank, and target uninformed[rank]. Two notifiers always
--    pick two different targets; the cascade doubles each round
--    (1 → 2 → 4 → ...) until no uninformed remain. When this unit
--    transfers, notifyPhase is reset rather than the goal completed —
--    we re-evaluate next tick and pick a new target if any remain.
--    Completion happens when uninformed is empty, or when rank
--    exceeds #uninformed (more walkers than work).
--
-- Phase state on s:
--   notifyPhase           = "walking" | "transferring" | "broadcasting" | nil
--   notifyPhaseStartedAt  = game-time when phase began (for timed phases)
--   notifyTarget          = uid (walk variant only)
-----------------------------------------------------------

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local aiState              = core.aiState
local isGoalActive         = core.isGoalActive
local setGoal              = core.setGoal
local markGoalAccomplished = core.markGoalAccomplished
local addWaterSource       = core.addWaterSource
local hasKnownWaterSource  = core.hasKnownWaterSource
local distance             = core.distance

local mv = require("scripts.movement_speed")

local M = {}

local function hasRadio(uid)
    local inv = unit.getInventory(uid)
    if not inv then return false end
    for _, it in ipairs(inv) do
        if it.defName == "radio" then return true end
    end
    return false
end

local function isAcolyteUid(uid)
    local info = unit.getInfo(uid)
    return info ~= nil and info.defName == "acolyte"
end

-- Acolytes whose AI state has been initialized but who don't yet have
-- a known water source. Sorted by uid so every observer agrees on
-- the same ordering. excludeUid lets the caller drop themselves out
-- (a walker shouldn't target itself).
local function getUninformedAcolytes(excludeUid)
    local result = {}
    local ids = unit.getAllIds()
    if not ids then return result end
    for _, uid in ipairs(ids) do
        if uid ~= excludeUid and isAcolyteUid(uid) then
            local s = aiState[uid]
            if s and not hasKnownWaterSource(s) then
                table.insert(result, uid)
            end
        end
    end
    table.sort(result)
    return result
end

-- Acolytes currently in the walk-notify pool: notify_allies active,
-- no radio. Sorted by uid for the rank split.
local function getWalkNotifiers()
    local result = {}
    local ids = unit.getAllIds()
    if not ids then return result end
    for _, uid in ipairs(ids) do
        if isAcolyteUid(uid) then
            local s = aiState[uid]
            if s and isGoalActive(s, "notify_allies") and not hasRadio(uid) then
                table.insert(result, uid)
            end
        end
    end
    table.sort(result)
    return result
end

-- Transfer-of-knowledge effect. Dedup-merges sources into the
-- target's memory and advances their goal chain (find_water →
-- notify_allies). Idempotent on already-informed targets.
local function transferSourcesTo(sources, targetUid)
    local targetS = aiState[targetUid]
    if not targetS then return end
    for _, src in ipairs(sources) do
        addWaterSource(targetS, src.x, src.y)
    end
    if isGoalActive(targetS, "find_water") then
        markGoalAccomplished(targetS, "find_water")
        setGoal(targetS, "notify_allies")
    elseif not targetS.goalStatus
        or targetS.goalStatus["find_water"] ~= "accomplished" then
        -- They were past find_water somehow (no goal, or on a later
        -- one we don't yet have). Record the accomplishment without
        -- queueing notify_allies on top.
        targetS.goalStatus = targetS.goalStatus or {}
        targetS.goalStatus["find_water"] = "accomplished"
    end
end

local function notifyAlliesUtility(uid, s, params)
    if not isGoalActive(s, "notify_allies") then return -math.huge end
    -- Lock in once a phase is active: a half-done broadcast or walk
    -- shouldn't be yanked by ambient WANDER drift. The lock equals the
    -- goal floor (goal_notify_weight = 4.0): above wander (~0.8) so idle
    -- drift can't interrupt it, but below follow_command (7.0) and
    -- combat/treatment (≥8.0) — notify is a routine, non-critical goal,
    -- so a player order, a fight, or a medic's patient all supersede it
    -- (#306). It resumes once the higher-priority action finishes.
    if s.notifyPhase then return params.goal_notify_weight end
    return params.goal_notify_weight
end

local function notifyAlliesExecute(uid, s, params)
    -- Radio branch ----------------------------------------------------
    if hasRadio(uid) then
        if not s.notifyPhase then
            s.notifyPhase = "broadcasting"
            s.notifyPhaseStartedAt = engine.gameTime()
            unit.stop(uid)
            return
        end
        if s.notifyPhase == "broadcasting" then
            local elapsed = engine.gameTime() - s.notifyPhaseStartedAt
            if elapsed < params.notify_broadcast_seconds then return end
            -- Broadcast: every other radio-bearing acolyte gets the
            -- full source list, find_water flips to accomplished.
            -- Recipients on radio do NOT pick up notify_allies — the
            -- broadcast already saturated the radio-bearer pool. If
            -- any non-radio acolytes exist, they'll need walk-notify;
            -- the broadcaster delegates that responsibility implicitly
            -- because each non-radio recipient (none in v1, since
            -- everyone has a radio) would receive notify_allies via
            -- transferSourcesTo's find_water→notify_allies chain.
            local mySources = s.knownWaterSources or {}
            local ids = unit.getAllIds() or {}
            for _, other in ipairs(ids) do
                if other ~= uid and isAcolyteUid(other) and hasRadio(other) then
                    local otherS = aiState[other]
                    if otherS then
                        for _, src in ipairs(mySources) do
                            addWaterSource(otherS, src.x, src.y)
                        end
                        if isGoalActive(otherS, "find_water") then
                            markGoalAccomplished(otherS, "find_water")
                        else
                            otherS.goalStatus = otherS.goalStatus or {}
                            otherS.goalStatus["find_water"] = "accomplished"
                        end
                    end
                end
            end
            markGoalAccomplished(s, "notify_allies")
            s.notifyPhase = nil
            s.notifyPhaseStartedAt = nil
            return
        end
        return   -- defensive: unknown phase, no-op
    end

    -- Walk branch -----------------------------------------------------
    local walkers    = getWalkNotifiers()
    local uninformed = getUninformedAcolytes(uid)

    if #uninformed == 0 then
        markGoalAccomplished(s, "notify_allies")
        s.notifyPhase = nil
        s.notifyTarget = nil
        s.notifyPhaseStartedAt = nil
        return
    end

    -- Find this unit's rank in the walker pool (stable across all
    -- walkers because both lists are uid-sorted).
    local rank = nil
    for i, w in ipairs(walkers) do
        if w == uid then rank = i; break end
    end
    if not rank then
        -- Goal active but we're not in the walker pool — shouldn't
        -- happen. Self-heal by completing.
        markGoalAccomplished(s, "notify_allies")
        s.notifyPhase = nil
        return
    end

    -- More walkers than uninformed: ranks past #uninformed have no
    -- target. Bow out cleanly.
    if rank > #uninformed then
        markGoalAccomplished(s, "notify_allies")
        s.notifyPhase = nil
        s.notifyTarget = nil
        return
    end

    local targetUid = uninformed[rank]
    s.notifyTarget = targetUid

    local targetInfo = unit.getInfo(targetUid)
    if not targetInfo then
        -- Target despawned mid-walk. Reset phase, re-pick next tick.
        s.notifyPhase = nil
        s.notifyTarget = nil
        return
    end

    local info = unit.getInfo(uid)
    if not info then return end
    local d = distance(info.gridX, info.gridY,
                       targetInfo.gridX, targetInfo.gridY)

    if d > params.notify_arrival_tiles then
        s.notifyPhase = "walking"
        unit.moveTo(uid, targetInfo.gridX, targetInfo.gridY,
                    mv.comfort(uid))  -- routine errand → comfort
        return
    end

    -- Arrived. Stand for notify_transfer_seconds, then hand off.
    if s.notifyPhase ~= "transferring" then
        s.notifyPhase = "transferring"
        s.notifyPhaseStartedAt = engine.gameTime()
        unit.stop(uid)
        return
    end
    local elapsed = engine.gameTime() - s.notifyPhaseStartedAt
    if elapsed < params.notify_transfer_seconds then return end

    transferSourcesTo(s.knownWaterSources or {}, targetUid)
    -- Phase clears; goal STAYS active. Next tick re-enters this
    -- function, recomputes uninformed (now -1), picks a new target
    -- if any remain. This is the parallel-doubling cascade: A informs
    -- B, then both A and B look for further uninformed targets next
    -- round.
    s.notifyPhase = nil
    s.notifyTarget = nil
    s.notifyPhaseStartedAt = nil
end

M.notifyAlliesUtility = notifyAlliesUtility
M.notifyAlliesExecute = notifyAlliesExecute

return M
