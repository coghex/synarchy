-- Unit AI chop-designation jobs (#538 split from unit_ai.lua).
--
-- Action: chop_designation (#97)
--
-- Player-directed tree felling: claim the nearest chop-designated
-- tree, walk to it, and swing until it falls —
-- world.harvestFloraInstance then spawns the wood logs as ground items
-- and starts that tree's (long) regrowth timer, and its designation is
-- removed. Structure mirrors dig_designation: module-local claims keyed
-- by page + FLORA INSTANCE since #1854 (two wood-tagged trees can share
-- one tile, and a tile key would let one acolyte's claim block the
-- other's tree) so two acolytes never fell the same tree, expiring on
-- timeout, on claimant death, or when a load replaces the session
-- (#1329);
-- finite lock-in so dire needs still preempt; walking → equipping →
-- chopping phases with the same anim-override discipline. Felling
-- progress lives HERE (s.chopProgress), not in the designation — an
-- interrupted fell restarts, there is no mid-chop visual to persist.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local grantWorkXP = core.grantWorkXP

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")
-- Page-qualified claim keys + the load reset that empties this
-- registry when a save replaces the session (#1329).
local claimsLib = require("scripts.unit_ai_claims")

local M = {}

local chopClaims = claimsLib.track({})  -- page key → { uid, at = gameTime }

-- (wid, floraInstanceId) since #1854 -- see the header.
local chopKey = claimsLib.instanceKey

-- This plant's own regrowth timer, or nil when the tile no longer holds
-- it (chunk evicted, or the plant is gone). #1854 made the timer
-- per-instance, so a berry bush picked on the same tile no longer reads
-- as "this tree is a regrowing stump".
local function instanceRegrowth(x, y, iid)
    for _, e in ipairs(world.getFloraGrowthAt(x, y) or {}) do
        if e.instanceId == iid then return e.regrowthRemaining or 0 end
    end
    return nil
end

local function chopClaimedByOther(key, uid, now, timeout)
    local c = chopClaims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        chopClaims[key] = nil
        return false
    end
    return true
end

-- The job's target instance. `iid` is DELIBERATELY not persisted
-- (stripped in unit_ai_save.lua, on the constructJob.build precedent):
-- a durable FloraInstanceId carried as a bare number is a reference
-- kind unit_ai_save_refs.lua does not declare and the integrity graph
-- could not check. The designation authority is durable, so a job
-- restored from a save re-resolves its target from the tile it saved,
-- and the whole re-key stays inside lua.unit_ai's existing schema.
--
-- Re-resolution is CLAIM-AWARE, and adopting a plant CLAIMS it in the
-- same step. A tile can carry several designated plants, and two
-- acolytes can restore jobs on it: resolving both to the same (say,
-- lowest-id) designation would have them fell one tree together while
-- the other's designation was orphaned, and the loser's claim silently
-- overwritten. Walking the tile's designations in the engine's own
-- deterministic order and taking the first one nobody else holds gives
-- them one tree each; claiming as we adopt is what stops two units
-- ticking in the same frame from both adopting it.
local function jobInstance(wid, s, uid, params, now)
    local job = s.chopJob
    if not job then return nil end
    if job.iid then return job.iid end
    for _, d in ipairs(chop.getDesignationsAt(wid, job.x, job.y) or {}) do
        local key = chopKey(wid, d.instanceId)
        if not chopClaimedByOther(key, uid, now, params.chop_claim_timeout) then
            chopClaims[key] = { uid = uid, at = now }
            job.iid = d.instanceId
            return job.iid
        end
    end
    return nil
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

local function releaseChopJob(wid, s, uid)
    if s.chopJob and s.chopJob.iid then
        local key = chopKey(wid, s.chopJob.iid)
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
local function chopComplete(wid, uid, s)
    unit.clearAnimOverride(uid)
    releaseChopJob(wid, s, uid)
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

    -- Active job: finite lock-in, released the moment THIS TREE's
    -- designation disappears (this check runs BEFORE execute each
    -- tick). #1854: asked per instance, so a second designated tree on
    -- the same tile no longer keeps a unit locked onto a job whose own
    -- target the player cancelled.
    if s.chopJob then
        local iid = jobInstance(wid, s, uid, params, engine.gameTime())
        if iid and chop.getDesignationForInstance(wid, iid) then
            return params.chop_lock_utility
        end
        chopComplete(wid, uid, s)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    -- #1854: the fourth return is the winning PLANT's stable id.
    local gx, gy, dist, iid =
        chop.nearestDesignation(wid, info.gridX, info.gridY)
    if not gx then return -math.huge end
    if dist > params.chop_scan_range then return -math.huge end

    local now = engine.gameTime()
    if chopClaimedByOther(chopKey(wid, iid), uid, now,
                          params.chop_claim_timeout) then
        return -math.huge
    end

    -- Stash the scored candidate so execute doesn't re-scan.
    s.chopCandidate = { x = gx, y = gy, iid = iid }

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
        -- A candidate with no instance id addresses no plant (#1854):
        -- nothing to claim, nothing to fell.
        if not cand or not cand.iid then return end
        local key = chopKey(wid, cand.iid)
        if chopClaimedByOther(key, uid, now, params.chop_claim_timeout) then
            return
        end
        if not chop.getDesignationForInstance(wid, cand.iid) then return end
        -- A just-felled tree's designation removal is a queued world
        -- command; the regrowth-timer check keeps us from re-claiming
        -- THIS TREE in that window (a regrowing stump is not choppable).
        -- NOT the harvestable flag: that is the bare-forage signal,
        -- gated on the #332 growth window — a designated sprout or
        -- standing-dead tree must stay choppable.
        local regrowth = instanceRegrowth(cand.x, cand.y, cand.iid)
        if regrowth and regrowth > 0 then return end
        chopClaims[key] = { uid = uid, at = now }
        s.chopCandidate = nil
        s.chopJob = { x = cand.x, y = cand.y, iid = cand.iid }
        s.chopProgress = 0
        s.chopEquipped = false
        s.chopPhase = "walking"
        unit.moveTo(uid, cand.x + 0.5, cand.y + 0.5, mv.comfort(uid))
        return
    end

    local job = s.chopJob
    local iid = jobInstance(wid, s, uid, params, now)
    -- A job restored from a save whose tile no longer resolves to a
    -- designated plant this unit may hold has nothing left to fell.
    if not iid then
        chopComplete(wid, uid, s)
        return
    end
    local key = chopKey(wid, iid)
    -- Never refresh a claim that is not ours. A held job normally owns
    -- its target, but a restored one adopted it above and could have
    -- raced; overwriting here would take another acolyte's tree out from
    -- under it mid-swing.
    if chopClaimedByOther(key, uid, now, params.chop_claim_timeout) then
        chopComplete(wid, uid, s)
        return
    end
    -- Keep the claim fresh while we hold the job.
    chopClaims[key] = { uid = uid, at = now }

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
        if not chop.getDesignationForInstance(wid, iid) then
            -- Player cancelled (or raced) out from under us.
            chopComplete(wid, uid, s)
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
            -- Felled. #1854: the EXACT instance is harvested and its
            -- own designation cancelled, so a second designated tree on
            -- the same tile keeps both its designation and its
            -- untouched regrowth state. The "wood" tag still scopes the
            -- harvest so a raced species change can't trade the tree
            -- for a berry bush. A nil result (plant gone / raced) still
            -- completes.
            world.harvestFloraInstance(job.x, job.y, iid, "wood")
            chop.cancelDesignation(job.x, job.y, iid)
            grantWorkXP(uid, "woodcutting", params.chop_xp_per_fell or 0)
            chopComplete(wid, uid, s)
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
