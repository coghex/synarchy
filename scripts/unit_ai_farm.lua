-- Unit AI farming actions (#538 split from unit_ai.lua).
--
-- till_designation (#333), plant_designation (#336), and auto_harvest
-- (#336) — the farming epic's tile-work actions. Each is grouped
-- under unitAi.till / unitAi.plant / unitAi.harvest (plain field
-- assignments on the shared singleton, established by #333) rather
-- than top-level locals returned from this module; kept exactly as-is
-- across this split so the existing public shape (e.g. any debug-
-- console or future caller poking unitAi.till.claims) doesn't move.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local grantWorkXP = core.grantWorkXP

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

-----------------------------------------------------------
-- Action: till_designation (#333)
--
-- Player-directed tilling: claim the nearest till-designated tile,
-- walk to it, and work until done — world.setVegAt then flips the
-- tile's ground cover to the tilled-soil id and the designation is
-- removed. Structure mirrors chop_designation: module-local claims
-- keyed by tile so two acolytes never till the same tile, expiring on
-- timeout or claimant death; finite lock-in so dire needs still
-- preempt; walking → equipping → tilling phases with the same
-- anim-override discipline. Till progress lives HERE (s.tillProgress),
-- not in the designation — an interrupted till restarts, there is no
-- mid-till visual to persist.
--
-- Grouped under unitAi.till (an established convention, #333) rather
-- than a top-level local per helper — kept as-is by this split.
-----------------------------------------------------------
unitAi.till = { claims = {} }   -- claims: "x,y" → { uid = ..., at = gameTime }
-- Tilled-soil vegetation id — must match World.Vegetation's
-- vegTilledSoil. No Lua-side veg-id registry exists yet, so this is a
-- plain mirrored constant.
unitAi.till.VEG_ID = 77

function unitAi.till.key(x, y) return x .. "," .. y end

function unitAi.till.claimedByOther(key, uid, now, timeout)
    local c = unitAi.till.claims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        unitAi.till.claims[key] = nil
        return false
    end
    return true
end

function unitAi.till.releaseJob(s, uid)
    if s.tillJob then
        local key = unitAi.till.key(s.tillJob.x, s.tillJob.y)
        local c = unitAi.till.claims[key]
        if c and c.uid == uid then unitAi.till.claims[key] = nil end
    end
    s.tillJob = nil
    s.tillPhase = nil
    s.tillProgress = nil
end

-- The designation vanished (tile tilled — possibly by us — or player
-- cancel). BOTH the utility check and the execute loop can be first
-- to notice, so completion lives in one helper.
function unitAi.till.complete(uid, s)
    unit.clearAnimOverride(uid)
    unitAi.till.releaseJob(s, uid)
end

function unitAi.till.utility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in, released the moment the designation
    -- disappears (this check runs BEFORE execute each tick).
    if s.tillJob then
        local d = till.getDesignationAt(wid, s.tillJob.x, s.tillJob.y)
        if d then return params.till_lock_utility end
        unitAi.till.complete(uid, s)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local gx, gy, dist =
        till.nearestDesignation(wid, info.gridX, info.gridY)
    if not gx then return -math.huge end
    if dist > params.till_scan_range then return -math.huge end

    local now = engine.gameTime()
    if unitAi.till.claimedByOther(unitAi.till.key(gx, gy), uid, now,
                                  params.till_claim_timeout) then
        return -math.huge
    end

    -- Stash the scored candidate so execute doesn't re-scan.
    s.tillCandidate = { x = gx, y = gy }

    local distFactor = math.max(0, 1 - dist / params.till_scan_range)
    return params.till_base_utility * distFactor
         * roles.weight(s, "till_designation")
end

function unitAi.till.execute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim a fresh job and head for the tile.
    if not s.tillJob then
        local cand = s.tillCandidate
        if not cand then return end
        local key = unitAi.till.key(cand.x, cand.y)
        if unitAi.till.claimedByOther(key, uid, now, params.till_claim_timeout) then
            return
        end
        local d = till.getDesignationAt(wid, cand.x, cand.y)
        if not d then return end
        unitAi.till.claims[key] = { uid = uid, at = now }
        s.tillCandidate = nil
        s.tillJob = { x = cand.x, y = cand.y, z = d.z }
        s.tillProgress = 0
        s.tillEquipped = false
        s.tillPhase = "walking"
        unit.moveTo(uid, cand.x + 0.5, cand.y + 0.5, mv.comfort(uid))
        return
    end

    local job = s.tillJob
    -- Keep the claim fresh while we hold the job.
    unitAi.till.claims[unitAi.till.key(job.x, job.y)] = { uid = uid, at = now }

    if s.tillPhase == "walking" then
        local utx = math.floor(info.gridX)
        local uty = math.floor(info.gridY)
        local cheb = math.max(math.abs(utx - job.x), math.abs(uty - job.y))
        if cheb <= 1 then
            unit.stop(uid)
            if not s.tillEquipped then
                -- setAnimOverride wins over the engine's state-driven
                -- anim resolution (same as dig/chop).
                unit.setAnimOverride(uid, params.till_equip_anim)
                s.tillPhase = "equipping"
                s.tillEquipUntil = now + params.till_equip_seconds
            else
                unit.setAnimOverride(uid, params.till_work_anim)
                s.tillPhase = "tilling"
                s.lastTillAt = now
            end
        else
            -- Execute only fires when idle, so this re-issue means
            -- the previous walk arrived short or failed.
            unit.moveTo(uid, job.x + 0.5, job.y + 0.5, mv.comfort(uid))
        end
        return
    end

    if s.tillPhase == "equipping" then
        if now >= (s.tillEquipUntil or 0) then
            s.tillEquipped = true
            unit.setAnimOverride(uid, params.till_work_anim)
            s.tillPhase = "tilling"
            s.lastTillAt = now
        end
        return
    end

    if s.tillPhase == "tilling" then
        if not till.getDesignationAt(wid, job.x, job.y) then
            -- Player cancelled (or raced) out from under us.
            unitAi.till.complete(uid, s)
            return
        end
        -- Idempotent: re-asserts the work anim after preemption.
        unit.setAnimOverride(uid, params.till_work_anim)
        local dt = math.min(now - (s.lastTillAt or now), 2.0)
        s.lastTillAt = now
        -- Farming skill (#265/#336) rides along like mining/woodcutting
        -- do for dig/chop: level 50 ≈ baseline, level 0 half rate.
        -- Legacy-save units without the key till at the yaml novice base.
        local strength = unit.getStat(uid, "strength") or 1.0
        local fSkill = unit.getSkill(uid, "farming") or 25.0
        s.tillProgress = (s.tillProgress or 0)
                       + params.till_rate * strength
                       * (0.5 + fSkill / 100.0) * dt
        if s.tillProgress >= 1.0 then
            -- Tilled: flip the tile's ground cover, then drop the
            -- designation (mirrors chop's harvestFlora → cancelDesignation
            -- order — the world edit lands before the marker clears).
            world.setVegAt(wid, job.x, job.y, job.z, unitAi.till.VEG_ID)
            till.cancelDesignation(job.x, job.y)
            grantWorkXP(uid, "farming", params.till_xp_per_till or 0)
            unitAi.till.complete(uid, s)
        end
        return
    end
end

-- Preemption (thirst, combat, player order): drop the tool VISUAL
-- only — claim, job, and progress survive so the till resumes
-- afterwards, re-entered through the walking phase.
function unitAi.till.onExit(uid, s, params)
    unit.clearAnimOverride(uid)
    if s.tillPhase == "tilling" or s.tillPhase == "equipping" then
        s.tillPhase = "walking"
    end
end


-- Action: plant_designation (#336)
--
-- Player-directed planting: claim the nearest plant-designated tile
-- (#335), walk to it, and work until done — dispatches to
-- world.plantCropAt (groundcover crops, a CropPlot) or
-- world.plantRowCropAt (row crops, a FloraInstance) by the
-- designation's category, then the designation is removed. Structure
-- mirrors unitAi.till exactly: module-local claims keyed by tile,
-- finite lock-in, walking → equipping → planting phases with the same
-- anim-override discipline. Planting progress lives HERE
-- (s.plantProgress), not in the designation — an interrupted plant
-- restarts, there is no mid-plant visual to persist.
--
-- Grouped under unitAi.plant (the unitAi.till convention, #333) —
-- kept as-is by this split.
-----------------------------------------------------------
unitAi.plant = { claims = {} }   -- claims: "x,y" → { uid = ..., at = gameTime }

function unitAi.plant.key(x, y) return x .. "," .. y end

function unitAi.plant.claimedByOther(key, uid, now, timeout)
    local c = unitAi.plant.claims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        unitAi.plant.claims[key] = nil
        return false
    end
    return true
end

function unitAi.plant.releaseJob(s, uid)
    if s.plantJob then
        local key = unitAi.plant.key(s.plantJob.x, s.plantJob.y)
        local c = unitAi.plant.claims[key]
        if c and c.uid == uid then unitAi.plant.claims[key] = nil end
    end
    s.plantJob = nil
    s.plantPhase = nil
    s.plantProgress = nil
end

-- The designation vanished (tile planted — possibly by us — or player
-- cancel). BOTH the utility check and the execute loop can be first
-- to notice, so completion lives in one helper.
function unitAi.plant.complete(uid, s)
    unit.clearAnimOverride(uid)
    unitAi.plant.releaseJob(s, uid)
end

function unitAi.plant.utility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in, released the moment the designation
    -- disappears OR changes crop (this check runs BEFORE execute each
    -- tick). plant.designate replaces in place (HM.insert) rather than
    -- refusing a re-designate, so a player can swap the crop on this
    -- tile while we're mid-job; matching only on d's existence would
    -- plant the STALE crop we originally claimed and then cancel the
    -- player's newer designation out from under them.
    if s.plantJob then
        local d = plant.getDesignationAt(wid, s.plantJob.x, s.plantJob.y)
        if d and d.crop == s.plantJob.crop then return params.plant_lock_utility end
        unitAi.plant.complete(uid, s)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local gx, gy, dist =
        plant.nearestDesignation(wid, info.gridX, info.gridY)
    if not gx then return -math.huge end
    if dist > params.plant_scan_range then return -math.huge end

    local now = engine.gameTime()
    if unitAi.plant.claimedByOther(unitAi.plant.key(gx, gy), uid, now,
                                   params.plant_claim_timeout) then
        return -math.huge
    end

    -- Stash the scored candidate so execute doesn't re-scan.
    s.plantCandidate = { x = gx, y = gy }

    local distFactor = math.max(0, 1 - dist / params.plant_scan_range)
    return params.plant_base_utility * distFactor
         * roles.weight(s, "plant_designation")
end

function unitAi.plant.execute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim a fresh job and head for the tile.
    if not s.plantJob then
        local cand = s.plantCandidate
        if not cand then return end
        local key = unitAi.plant.key(cand.x, cand.y)
        if unitAi.plant.claimedByOther(key, uid, now, params.plant_claim_timeout) then
            return
        end
        local d = plant.getDesignationAt(wid, cand.x, cand.y)
        if not d then return end
        unitAi.plant.claims[key] = { uid = uid, at = now }
        s.plantCandidate = nil
        s.plantJob = { x = cand.x, y = cand.y, z = d.z,
                       crop = d.crop, category = d.category }
        s.plantProgress = 0
        s.plantEquipped = false
        s.plantPhase = "walking"
        unit.moveTo(uid, cand.x + 0.5, cand.y + 0.5, mv.comfort(uid))
        return
    end

    local job = s.plantJob
    -- Keep the claim fresh while we hold the job.
    unitAi.plant.claims[unitAi.plant.key(job.x, job.y)] = { uid = uid, at = now }

    if s.plantPhase == "walking" then
        local utx = math.floor(info.gridX)
        local uty = math.floor(info.gridY)
        local cheb = math.max(math.abs(utx - job.x), math.abs(uty - job.y))
        if cheb <= 1 then
            unit.stop(uid)
            if not s.plantEquipped then
                -- setAnimOverride wins over the engine's state-driven
                -- anim resolution (same as dig/chop/till).
                unit.setAnimOverride(uid, params.plant_equip_anim)
                s.plantPhase = "equipping"
                s.plantEquipUntil = now + params.plant_equip_seconds
            else
                unit.setAnimOverride(uid, params.plant_work_anim)
                s.plantPhase = "planting"
                s.lastPlantAt = now
            end
        else
            -- Execute only fires when idle, so this re-issue means
            -- the previous walk arrived short or failed.
            unit.moveTo(uid, job.x + 0.5, job.y + 0.5, mv.comfort(uid))
        end
        return
    end

    if s.plantPhase == "equipping" then
        if now >= (s.plantEquipUntil or 0) then
            s.plantEquipped = true
            unit.setAnimOverride(uid, params.plant_work_anim)
            s.plantPhase = "planting"
            s.lastPlantAt = now
        end
        return
    end

    if s.plantPhase == "planting" then
        local d = plant.getDesignationAt(wid, job.x, job.y)
        if not d or d.crop ~= job.crop then
            -- Player cancelled, or re-designated this tile with a
            -- different crop, out from under us — drop the stale job
            -- rather than plant it and cancel the newer designation.
            unitAi.plant.complete(uid, s)
            return
        end
        -- Idempotent: re-asserts the work anim after preemption.
        unit.setAnimOverride(uid, params.plant_work_anim)
        local dt = math.min(now - (s.lastPlantAt or now), 2.0)
        s.lastPlantAt = now
        -- Farming skill (#265) rides along like mining/woodcutting do
        -- for dig/chop: level 50 ≈ baseline, level 0 half rate.
        local strength = unit.getStat(uid, "strength") or 1.0
        local fSkill = unit.getSkill(uid, "farming") or 25.0
        s.plantProgress = (s.plantProgress or 0)
                        + params.plant_rate * strength
                        * (0.5 + fSkill / 100.0) * dt
        if s.plantProgress >= 1.0 then
            -- Planted: dispatch by category (row crops are a
            -- FloraInstance, groundcover crops a CropPlot), then drop
            -- the designation (mirrors till's world-edit-before-marker-
            -- clears order).
            if job.category == "groundcover_crop" then
                world.plantCropAt(job.x, job.y, job.crop)
            else
                world.plantRowCropAt(wid, job.x, job.y, job.crop)
            end
            plant.cancelDesignation(job.x, job.y)
            grantWorkXP(uid, "farming", params.plant_xp_per_plant or 0)
            unitAi.plant.complete(uid, s)
        end
        return
    end
end

-- Preemption (thirst, combat, player order): drop the tool VISUAL
-- only — claim, job, and progress survive so the plant resumes
-- afterwards, re-entered through the walking phase.
function unitAi.plant.onExit(uid, s, params)
    unit.clearAnimOverride(uid)
    if s.plantPhase == "planting" or s.plantPhase == "equipping" then
        s.plantPhase = "walking"
    end
end


-- Action: auto_harvest (#336)
--
-- Skill-gated colony farm-tending: pick up any ripe harvestable flora
-- in range — planted crops AND wild flora alike (world.
-- findHarvestableFlora / world.harvestFlora, #94, don't distinguish the
-- two; #334's crop species carry worldGen.density 0.0, so any crop
-- instance found here was deliberately planted, never a wild spawn).
-- Instant harvest, mirroring forage's shape rather than till/plant's
-- progress accumulator — picking ripe fruit is quick. NOT hunger-gated
-- like forage: this is routine farm-tending work, weighted by the
-- farming skill/role (#265) instead of scaled by need.
--
-- Grouped under unitAi.harvest (the unitAi.till convention, #333) —
-- kept as-is by this split.
-----------------------------------------------------------
unitAi.harvest = {}

function unitAi.harvest.utility(uid, s, params)
    if not world.findHarvestableFlora then return -math.huge end
    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local ux = math.floor(info.gridX)
    local uy = math.floor(info.gridY)
    local spot = world.findHarvestableFlora(ux, uy, params.harvest_scan_range)
    if not spot then
        s.harvestTarget = nil
        return -math.huge
    end
    s.harvestTarget = { x = spot.gx, y = spot.gy }
    local distFactor = math.max(0, 1 - spot.dist / params.harvest_scan_range)
    return params.harvest_base_utility * distFactor
         * roles.weight(s, "auto_harvest")
end

function unitAi.harvest.execute(uid, s, params)
    -- Collecting: pull the harvested yield off the ground, one item
    -- per tick (mirrors forageExecute's collecting phase).
    if s.harvestPhase == "collecting" then
        local loot = s.harvestLoot or {}
        local nextGid = table.remove(loot)
        if not nextGid or not item.pickupGround(uid, nextGid) then
            s.harvestPhase = nil
            s.harvestLoot  = nil
        end
        return
    end

    local tgt = s.harvestTarget
    if not tgt then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local cheb = math.max(math.abs(utx - tgt.x), math.abs(uty - tgt.y))

    if cheb <= 1 then
        local yields = world.harvestFlora(tgt.x, tgt.y)
        if yields and #yields > 0 then
            unit.pickup(uid)   -- bend-down anim over the plant
            local gids = {}
            for _, yi in ipairs(yields) do gids[#gids + 1] = yi.gid end
            s.harvestLoot  = gids
            s.harvestPhase = "collecting"
            grantWorkXP(uid, "farming", params.harvest_xp_per_harvest or 0)
        end
        -- Raced / regrowing after all, or a completed harvest either
        -- way: forget the target; the next decision re-finds.
        s.harvestTarget = nil
        return
    end

    unit.moveTo(uid, tgt.x + 0.5, tgt.y + 0.5, mv.comfort(uid))
end

