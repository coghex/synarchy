-- Unit AI chop-designation jobs (#538 split from unit_ai.lua).
--
-- Action: chop_designation (#97)
--
-- Player-directed tree felling: claim the nearest chop-designated
-- tree, walk to it, and swing until it falls — world.harvestFlora
-- then spawns the wood logs as ground items and starts the (long)
-- regrowth timer, and the designation is removed. Structure mirrors
-- dig_designation: module-local claims keyed by tile so two acolytes
-- never fell the same tree, expiring on timeout or claimant death;
-- finite lock-in so dire needs still preempt; walking → equipping →
-- chopping phases with the same anim-override discipline. Felling
-- progress lives HERE (s.chopProgress), not in the designation — an
-- interrupted fell restarts, there is no mid-chop visual to persist.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local grantWorkXP = core.grantWorkXP

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local M = {}

local chopClaims = {}   -- "x,y" → { uid = ..., at = gameTime }

local function chopKey(x, y) return x .. "," .. y end

local function chopClaimedByOther(key, uid, now, timeout)
    local c = chopClaims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        chopClaims[key] = nil
        return false
    end
    return true
end

-- Best chopping speed among carried tools; bare hands as the floor.
local function bestChopSpeed(uid, params)
    local speed = params.chop_bare_speed
    local inv = unit.getInventory(uid)
    if inv then
        for _, it in ipairs(inv) do
            for _, tool in pairs(params.chop_tools) do
                if tool.defs[it.defName] and tool.speed > speed then
                    speed = tool.speed
                end
            end
        end
    end
    return speed
end

local function releaseChopJob(s, uid)
    if s.chopJob then
        local key = chopKey(s.chopJob.x, s.chopJob.y)
        local c = chopClaims[key]
        if c and c.uid == uid then chopClaims[key] = nil end
    end
    s.chopJob = nil
    s.chopPhase = nil
    s.chopProgress = nil
end

-- The designation vanished (tree felled — possibly by us — or player
-- cancel). BOTH the utility check and the execute loop can be first
-- to notice, so completion lives in one helper.
local function chopComplete(uid, s)
    unit.clearAnimOverride(uid)
    releaseChopJob(s, uid)
end

-- Colony wood stock (issue #97's chopUrgency): logs on the ground
-- plus logs this unit carries. #96's shared proximity-scan sourcing
-- can replace this once the build AI lands; until then ground+own
-- inventory is the observable stockpile.
local function woodStockFactor(uid, params)
    local stock = 0
    for _, g in ipairs(item.listGround() or {}) do
        if g.defName == "wood_log" then stock = stock + 1 end
    end
    local inv = unit.getInventory(uid)
    if inv then
        for _, it in ipairs(inv) do
            if it.defName == "wood_log" then stock = stock + 1 end
        end
    end
    local scarcity = 1 - math.min(1, stock / params.chop_stock_target)
    return params.chop_stock_floor
         + (1 - params.chop_stock_floor) * scarcity
end

local function chopUtility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in, released the moment the designation
    -- disappears (this check runs BEFORE execute each tick).
    if s.chopJob then
        local d = chop.getDesignationAt(wid, s.chopJob.x, s.chopJob.y)
        if d then return params.chop_lock_utility end
        chopComplete(uid, s)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local gx, gy, dist =
        chop.nearestDesignation(wid, info.gridX, info.gridY)
    if not gx then return -math.huge end
    if dist > params.chop_scan_range then return -math.huge end

    local now = engine.gameTime()
    if chopClaimedByOther(chopKey(gx, gy), uid, now,
                          params.chop_claim_timeout) then
        return -math.huge
    end

    -- Stash the scored candidate so execute doesn't re-scan.
    s.chopCandidate = { x = gx, y = gy }

    local distFactor = math.max(0, 1 - dist / params.chop_scan_range)
    return params.chop_base_utility * distFactor
         * woodStockFactor(uid, params)
         * roles.weight(s, "chop_designation")
end

local function chopExecute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim a fresh job and head for the tree.
    if not s.chopJob then
        local cand = s.chopCandidate
        if not cand then return end
        local key = chopKey(cand.x, cand.y)
        if chopClaimedByOther(key, uid, now, params.chop_claim_timeout) then
            return
        end
        if not chop.getDesignationAt(wid, cand.x, cand.y) then return end
        -- A just-felled tree's designation removal is a queued world
        -- command; the regrowth-timer check keeps us from re-claiming
        -- the tile in that window (a regrowing stump is not choppable).
        -- NOT fl.harvestable: that flag is the bare-forage signal,
        -- gated on the #332 growth window — a designated sprout or
        -- standing-dead tree must stay choppable.
        local fl = world.getFloraAt(cand.x, cand.y)
        if fl and (fl.regrowthRemaining or 0) > 0 then return end
        chopClaims[key] = { uid = uid, at = now }
        s.chopCandidate = nil
        s.chopJob = { x = cand.x, y = cand.y }
        s.chopProgress = 0
        s.chopEquipped = false
        s.chopPhase = "walking"
        unit.moveTo(uid, cand.x + 0.5, cand.y + 0.5, mv.comfort(uid))
        return
    end

    local job = s.chopJob
    -- Keep the claim fresh while we hold the job.
    chopClaims[chopKey(job.x, job.y)] = { uid = uid, at = now }

    if s.chopPhase == "walking" then
        local utx = math.floor(info.gridX)
        local uty = math.floor(info.gridY)
        local cheb = math.max(math.abs(utx - job.x), math.abs(uty - job.y))
        if cheb <= 1 then
            unit.stop(uid)
            if not s.chopEquipped then
                -- setAnimOverride wins over the engine's state-driven
                -- anim resolution (same as dig).
                unit.setAnimOverride(uid, params.chop_equip_anim)
                s.chopPhase = "equipping"
                s.chopEquipUntil = now + params.chop_equip_seconds
            else
                unit.setAnimOverride(uid, params.chop_work_anim)
                s.chopPhase = "chopping"
                s.lastChopAt = now
            end
        else
            -- Execute only fires when idle, so this re-issue means
            -- the previous walk arrived short or failed.
            unit.moveTo(uid, job.x + 0.5, job.y + 0.5, mv.comfort(uid))
        end
        return
    end

    if s.chopPhase == "equipping" then
        if now >= (s.chopEquipUntil or 0) then
            s.chopEquipped = true
            unit.setAnimOverride(uid, params.chop_work_anim)
            s.chopPhase = "chopping"
            s.lastChopAt = now
        end
        return
    end

    if s.chopPhase == "chopping" then
        if not chop.getDesignationAt(wid, job.x, job.y) then
            -- Player cancelled (or raced) out from under us.
            chopComplete(uid, s)
            return
        end
        -- Idempotent: re-asserts the work anim after preemption.
        unit.setAnimOverride(uid, params.chop_work_anim)
        local dt = math.min(now - (s.lastChopAt or now), 2.0)
        s.lastChopAt = now
        -- Muscle swings the axe; an axe bites deeper than bare hands.
        -- Woodcutting skill rides along like mining does for dig:
        -- level 50 ≈ baseline, level 0 half rate (#265). Legacy-save
        -- units without the key fell at the yaml novice base.
        local strength = unit.getStat(uid, "strength") or 1.0
        local speed = bestChopSpeed(uid, params)
        local wcSkill = unit.getSkill(uid, "woodcutting") or 25.0
        s.chopProgress = (s.chopProgress or 0)
                       + params.chop_rate * speed * strength
                       * (0.5 + wcSkill / 100.0) * dt
        if s.chopProgress >= 1.0 then
            -- Felled. The "wood" tag scopes the harvest so a shared
            -- tile can't trade its berry bush for the tree; the yield
            -- spawns as ground items and the regrowth timer starts. A
            -- nil result (species died / raced) still completes.
            world.harvestFlora(job.x, job.y, "wood")
            chop.cancelDesignation(job.x, job.y)
            grantWorkXP(uid, "woodcutting", params.chop_xp_per_fell or 0)
            chopComplete(uid, s)
        end
        return
    end
end

-- Preemption (thirst, combat, player order): drop the tool VISUAL
-- only — claim, job, and progress survive so the fell resumes
-- afterwards, re-entered through the walking phase.
local function chopOnExit(uid, s, params)
    unit.clearAnimOverride(uid)
    if s.chopPhase == "chopping" or s.chopPhase == "equipping" then
        s.chopPhase = "walking"
    end
end


M.chopUtility = chopUtility
M.chopExecute = chopExecute
M.chopOnExit  = chopOnExit

return M
