-- Unit AI equipment repair (#538 split from unit_ai.lua).
--
-- Action: repair_job (#302). AI-autonomous equipment maintenance: an
-- acolyte notices its own (or the technomule's spare) gear has
-- degraded past a threshold and carries it to the right station
-- (#301: furnace for condition, workbench for sharpness) to restore
-- it via repair.repairAt. No engine-side spatial designation — a
-- repair target is an item INSTANCE (inventory/equipment/accessories,
-- own or the technomule's), so claims are keyed by instanceId
-- (repairClaims) rather than tile, playing the same race-guard role
-- a designation's status does for dig/chop/construct. Ground-item
-- targeting is out of scope (item.listGround() exposes no instanceId/
-- condition/sharpness).
--
-- State on s: repairJob = { instanceId, defName, axis, recipeId,
-- consumable, consumableCount, groundWant, muleWant, groundDone,
-- onMule, itemFetched, bid }; repairPhase = "fetch_item" |
-- "fetch_consumable" | "walking" | "repairing".
-----------------------------------------------------------

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local distance        = core.distance
local chebToFootprint = core.chebToFootprint
local reportFailure   = core.reportFailure
local grantWorkXP     = core.grantWorkXP

local config = require("scripts.unit_ai_tunables")

local fetch = require("scripts.unit_ai_fetch")
local findTechnomule       = fetch.findTechnomule
local deliverItemWeight    = fetch.deliverItemWeight
local inventoryCountOf     = fetch.inventoryCountOf
local fetchWantsFromGround = fetch.fetchWantsFromGround
local fetchWantsFromMule   = fetch.fetchWantsFromMule

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local repairUtility, repairExecute, repairOnExit

do
local repairClaims = {}   -- instanceId → { uid, at }

-- Player-facing "prioritize repair" flag (#303 UI). Purely a
-- candidate-selection preference: a flagged instance always wins
-- scanHeldItems's pick among a unit's OWN repair candidates over an
-- unflagged one, severity ties broken as before. Deliberately NOT
-- folded into the severity/utility math in repairUtility below —
-- repair's entry utility is tuned so its ceiling (broken armor: 2.5 ×
-- 1.2 base × 1.4 role weight = 4.2) sits under every other action's
-- 6.0 in-progress lock (see the comment on the repair config above);
-- boosting it here would risk a prioritized repair preempting some
-- other unit's already-claimed job. Cleared automatically once the
-- item is actually repaired (or found already full) — see the
-- "repairing" phase below.
local repairPriority = {}   -- instanceId → true

function unitAi.setRepairPriority(instanceId, flag)
    if not instanceId then return end
    if flag then repairPriority[instanceId] = true
    else repairPriority[instanceId] = nil end
end

function unitAi.isRepairPriority(instanceId)
    return instanceId ~= nil and repairPriority[instanceId] == true
end

-- uid of whoever currently holds this item's repair claim, or nil.
-- Doesn't distinguish claim phase (fetch/walk/repair) — the UI only
-- needs "somebody's on it".
function unitAi.getRepairClaimant(instanceId)
    local c = repairClaims[instanceId]
    return c and c.uid or nil
end

local function repairClaimedByOther(iid, uid, now, timeout)
    local c = repairClaims[iid]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        repairClaims[iid] = nil
        return false
    end
    return true
end

local function releaseRepairJob(s, uid)
    if s.repairJob then
        local c = repairClaims[s.repairJob.instanceId]
        if c and c.uid == uid then repairClaims[s.repairJob.instanceId] = nil end
    end
    s.repairJob = nil
    s.repairPhase = nil
end

-- Abort the current job. If the target item was already fetched off
-- the mule (job.itemFetched, set once fetch_item's transfer lands), it
-- is now sitting in THIS unit's own inventory — return it before
-- releasing, or it silently disappears from the mule's stock into
-- whichever acolyte was mid-job when something else failed later
-- (missing consumable, a destroyed station, a last-second repair
-- failure, ...). Every abort past fetch_item must go through this
-- instead of a bare releaseRepairJob.
local function abortRepairJob(uid, s, info)
    local job = s.repairJob
    if job and job.itemFetched and info then
        local mule = findTechnomule(info.gridX, info.gridY)
        if mule then
            -- Targeted by instanceId: without it, a defName-only
            -- transfer could pop a DIFFERENT axe_steel this unit
            -- happens to also be carrying (its own starting gear),
            -- sending that back instead and leaving the actually-
            -- fetched (possibly still-degraded) instance stranded here.
            unit.transferItemToUnit(uid, mule.uid, job.defName, job.instanceId)
        end
    end
    releaseRepairJob(s, uid)
end

-- How urgent is repairing this one item, and which axis? Condition is
-- checked before sharpness — a broken/low-condition item is
-- combat-catastrophic (zero armor protection, or a crippled weapon)
-- while low sharpness only reduces penetration, so it's repaired
-- first; the AI picks up a remaining sharpness need on a later tick.
-- Returns severity, axis — or nil if the item doesn't need repair.
local function repairSeverity(it, params)
    if it.condition ~= nil and it.condition < params.repair_condition_threshold then
        if it.condition <= 0 then
            local band = (it.kind == "armor")
                and params.repair_severity_broken_armor
                or params.repair_severity_broken_weapon
            return band, "condition"
        end
        local x = 1 - (it.condition / params.repair_condition_threshold)
        return x * x, "condition"
    end
    if it.sharpness and it.sharpness < params.repair_sharpness_threshold then
        local x = 1 - (it.sharpness / params.repair_sharpness_threshold)
        return x * x, "sharpness"
    end
    return nil, nil
end

-- Whether `it` is currently something the autonomous repair AI would
-- pick up on its own — i.e. repairSeverity would return a candidate for
-- it (#303 review: without this check, the UI could offer/show
-- "priority" on an item above both thresholds, which the AI would then
-- never actually act on). repair_job is only registered for acolytes
-- today, so acolyte's thresholds are the only ones that matter; this
-- reads them directly rather than requiring a callers to know which
-- unit owns the item.
function unitAi.itemNeedsRepair(it)
    local params = config.acolyte
    if not params or not it then return false end
    return repairSeverity(it, params) ~= nil
end

-- Best repair candidate among ownerUid's inventory + equipped gear +
-- accessories. Skips anything already claimed by another live unit.
-- A player-prioritized item (unitAi.setRepairPriority) always beats a
-- non-prioritized one regardless of severity; among same-priority
-- candidates the more severe one wins, as before.
local function scanHeldItems(ownerUid, actingUid, onMule, now, params)
    local best, bestSev, bestPri = nil, 0, false
    local function consider(it)
        if repairClaimedByOther(it.instanceId, actingUid, now,
                                params.repair_claim_timeout) then
            return
        end
        local sev, axis = repairSeverity(it, params)
        if not sev then return end
        local pri = repairPriority[it.instanceId] == true
        local better = (pri and not bestPri)
                     or (pri == bestPri and sev > bestSev)
        if better then
            best, bestSev, bestPri = {
                instanceId = it.instanceId, defName = it.defName,
                axis = axis, severity = sev, onMule = onMule,
            }, sev, pri
        end
    end
    for _, it in ipairs(unit.getInventory(ownerUid) or {}) do consider(it) end
    for _, it in pairs(equipment.getLoadout(ownerUid) or {}) do consider(it) end
    for _, it in ipairs(equipment.getAccessories(ownerUid) or {}) do consider(it) end
    return best
end

-- Own gear first (no fetch needed); only look to the mule's spare
-- stock when the unit itself carries nothing worth repairing.
local function findRepairCandidate(uid, info, params)
    local now = engine.gameTime()
    local best = scanHeldItems(uid, uid, false, now, params)
    if best then return best end
    local mule = findTechnomule(info.gridX, info.gridY)
    if not mule then return nil end
    return scanHeldItems(mule.uid, uid, true, now, params)
end

function repairUtility(uid, s, params)
    if s.repairJob then return params.repair_lock_utility end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local cand = findRepairCandidate(uid, info, params)
    if not cand then return -math.huge end

    -- Only claim if a station for this axis is actually reachable —
    -- mirrors dig's tool gate / construct's materials-available check.
    -- building.findStation ranks candidates by Chebyshev distance from
    -- the given (gx, gy), but its Lua.tointeger argument parsing only
    -- accepts whole numbers — a raw unit position like 16.5 silently
    -- fails to parse, and it falls back to "no distance info" (lowest
    -- building id wins, ignoring proximity entirely). Floor first.
    local recipeId = "repair_" .. cand.axis
    if not building.findStation(recipeId, math.floor(info.gridX),
                                math.floor(info.gridY)) then
        return -math.huge
    end

    local recipe = repair.get(recipeId)
    local input = recipe and recipe.inputs and recipe.inputs[1]
    if not input then return -math.huge end

    -- Capacity feasibility: a mule-sourced item adds its own weight on
    -- pickup, and the recipe's consumable adds more on top — a unit
    -- whose remaining headroom can't cover both would otherwise claim,
    -- fail to fetch, and immediately re-claim the SAME candidate (still
    -- degraded, now sitting in its own inventory) forever — a repeated
    -- "unit_warning" pause storm (config/notifications.yaml pauses on
    -- that category) instead of a clean "can't do this job right now" bail.
    local needed = 0
    if cand.onMule then
        needed = needed + deliverItemWeight(cand.defName)
    end
    if inventoryCountOf(uid, input.item) < (input.count or 1) then
        needed = needed + deliverItemWeight(input.item) * (input.count or 1)
    end
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW = unit.getStat(uid, "carrying_capacity") or math.huge
    if carried + needed > maxW then return -math.huge end

    cand.recipeId = recipeId
    cand.consumable = input.item
    cand.consumableCount = input.count or 1
    s.repairCandidate = cand
    return params.repair_base_utility * cand.severity
         * roles.weight(s, "repair_job")
end

function repairExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then abortRepairJob(uid, s, info); return end
    local now = engine.gameTime()

    -- Claim a fresh job from the scored candidate.
    if not s.repairJob then
        local cand = s.repairCandidate
        if not cand then return end
        s.repairCandidate = nil
        if repairClaimedByOther(cand.instanceId, uid, now,
                                params.repair_claim_timeout) then
            return
        end
        repairClaims[cand.instanceId] = { uid = uid, at = now }
        s.repairJob = {
            instanceId = cand.instanceId, defName = cand.defName,
            axis = cand.axis, recipeId = cand.recipeId,
            consumable = cand.consumable, consumableCount = cand.consumableCount,
            onMule = cand.onMule,
        }
        s.repairPhase = cand.onMule and "fetch_item" or "fetch_consumable"
        -- Cancel any in-flight moveTo the PREVIOUS action left running
        -- (e.g. a wander/search-spiral step): the switch-or-idle
        -- dispatch gate only re-fires execute() while activity=="idle",
        -- so a stale walking activity would strand this phase machine
        -- forever (the stuck-walk watchdog above exists for the same
        -- class of bug). Later phases re-issue their own moveTo.
        unit.stop(uid)
        return
    end

    local job = s.repairJob
    -- Keep the claim fresh while the job is held.
    repairClaims[job.instanceId] = { uid = uid, at = now }

    if s.repairPhase == "fetch_item" then
        local mule = findTechnomule(info.gridX, info.gridY)
        if not mule then releaseRepairJob(s, uid); return end
        if distance(info.gridX, info.gridY, mule.gridX, mule.gridY)
           > params.mule_fetch_arrival then
            unit.moveTo(uid, mule.gridX, mule.gridY, mv.comfort(uid))
            return
        end
        unit.stop(uid)
        -- Targeted by instanceId: the mule may carry more than one
        -- axe_steel, and a defName-only transfer could grab the wrong
        -- copy. transferItemToUnit only succeeds if this EXACT flagged
        -- instance is still on the mule (a raced claimant taking the
        -- specific instance first fails cleanly here, same as any other
        -- instance no longer being found).
        if not unit.transferItemToUnit(mule.uid, uid, job.defName, job.instanceId) then
            releaseRepairJob(s, uid)   -- raced — someone else took it; never fetched
            return
        end
        -- The flagged instance is now in our own inventory — any abort
        -- from here on must return it (abortRepairJob).
        job.itemFetched = true
        s.repairPhase = "fetch_consumable"
        return
    end

    if s.repairPhase == "fetch_consumable" then
        if inventoryCountOf(uid, job.consumable) >= job.consumableCount then
            s.repairPhase = "walking"
            return
        end
        -- Ground (rung 2), then the mule (rung 3) — tried against
        -- SEPARATE want-tables, one per rung. fetchWantsFromGround/Mule
        -- assume the caller pre-splits ground vs mule portions the way
        -- deliverExecute's claim.fromGround/claim.fromMule do; sharing
        -- ONE table between both calls is wrong — a ground miss clears
        -- the entry entirely (`wants[mat] = nil`), so a shared table
        -- would short-circuit the mule fallback on every ground miss.
        if not job.groundDone then
            job.groundWant = job.groundWant
                or { [job.consumable] = job.consumableCount }
            if fetchWantsFromGround(uid, job.groundWant, params,
                                    params.repair_scan_range) then
                return
            end
            job.groundDone = true
            if inventoryCountOf(uid, job.consumable) >= job.consumableCount then
                s.repairPhase = "walking"
                return
            end
        end
        job.muleWant = job.muleWant or { [job.consumable] = job.consumableCount }
        if fetchWantsFromMule(uid, job.muleWant, info, params) then
            return
        end
        if inventoryCountOf(uid, job.consumable) < job.consumableCount then
            -- No lignite_chunk/whetstone anywhere reachable. Give up —
            -- re-evaluate next tick (camping the job gains nothing).
            reportFailure(uid, "No " .. job.consumable
                .. " available to repair " .. job.defName)
            abortRepairJob(uid, s, info)
            return
        end
        s.repairPhase = "walking"
        return
    end

    if s.repairPhase == "walking" then
        if not job.bid then
            job.bid = building.findStation(job.recipeId, math.floor(info.gridX),
                                           math.floor(info.gridY))
            if not job.bid then abortRepairJob(uid, s, info); return end
        end
        local binfo = building.getInfo(job.bid)
        if not binfo then abortRepairJob(uid, s, info); return end
        local utx, uty = math.floor(info.gridX), math.floor(info.gridY)
        local tw, th = binfo.tileW or 1, binfo.tileH or 1
        local cheb = chebToFootprint(utx, uty, binfo.gridX, binfo.gridY, tw, th)
        if cheb <= 1 then
            unit.stop(uid)
            s.repairPhase = "repairing"
            return
        end
        local bestX, bestY, bestD = nil, nil, math.huge
        for dx = -1, tw do
            for dy = -1, th do
                if dx == -1 or dx == tw or dy == -1 or dy == th then
                    local nx, ny = binfo.gridX + dx + 0.5, binfo.gridY + dy + 0.5
                    local d = distance(info.gridX, info.gridY, nx, ny)
                    if d < bestD then bestX, bestY, bestD = nx, ny, d end
                end
            end
        end
        if bestX then unit.moveTo(uid, bestX, bestY, mv.comfort(uid)) end
        return
    end

    if s.repairPhase == "repairing" then
        local r, err = repair.repairAt(uid, job.recipeId, job.instanceId, job.bid)
        if not r then
            if err and err:find("already at full") then
                -- Nothing left to do on this axis; stop advertising it
                -- as prioritized (#303) rather than leaving a stale flag.
                repairPriority[job.instanceId] = nil
            else
                reportFailure(uid, "Repair failed: " .. tostring(err))
            end
            abortRepairJob(uid, s, info)
            return
        end
        grantWorkXP(uid, "smithing", params.repair_xp_per_repair or 0)
        repairPriority[job.instanceId] = nil   -- restored (#303)
        -- Spare gear fetched off the mule goes back once restored.
        -- abortRepairJob's "return the fetched item" step (keyed on
        -- job.itemFetched, targeted by instanceId) handles this the
        -- same way a mid-job abort does.
        abortRepairJob(uid, s, info)
    end
end

-- Preemption (thirst, combat, order): only the final approach needs
-- resetting — mid-fetch phases re-evaluate fresh every tick anyway.
function repairOnExit(uid, s, params)
    if s.repairPhase == "repairing" then
        s.repairPhase = "walking"
    end
end

end


return { utility = repairUtility, execute = repairExecute, onExit = repairOnExit }
