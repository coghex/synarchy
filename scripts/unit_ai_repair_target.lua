-- Repair TARGET selection (#1737, split out of scripts/unit_ai_repair.lua
-- to stay under the #538 line budget).
--
-- One question, three rungs: which item should this worker repair next?
-- The ladder is ABSOLUTE and ordered by how much work reaching the item
-- costs -- own held gear (already in hand), then a degraded instance
-- lying on the ground within repair_scan_range, then the nearest
-- technomule's spare stock. Player priority (#303) and severity rank
-- candidates WITHIN whichever rung answers first; they never promote a
-- lower rung over a higher one.
--
-- The ground rung is #1737's addition. It reads the same row fields the
-- held-item scan reads, because item.listGround / item.getGroundForUnit
-- now expose instanceId, sharpness and kind alongside condition (one
-- shared pushGroundRow, so the two readers cannot disagree). That is
-- what lets ONE repairSeverity score a held row and a ground row alike,
-- including the broken-armour band, rather than a ground-only variant
-- that would silently only ever find condition-axis targets.
--
-- Enumeration is #1673's pattern, verbatim from unit_ai_fetch.lua:
-- item.listGround is ACTIVE-page scoped while item.pickupGround commits
-- on the CARRIER's page, so every enumerated id is re-resolved through
-- item.getGroundForUnit(uid, gid) and every predicate below reads the
-- RESOLVED row. An id that does not resolve yields no candidate, and a
-- worker with no live page yields none.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local distance = core.distance

local config = require("scripts.unit_ai_tunables")

local fetch = require("scripts.unit_ai_fetch")
local findTechnomule    = fetch.findTechnomule
local deliverItemWeight = fetch.deliverItemWeight

local mv = require("scripts.movement_speed")

local M = {}

-- How urgent is repairing this one item, and which axis? Condition is
-- checked before sharpness -- a broken/low-condition item is
-- combat-catastrophic (zero armor protection, or a crippled weapon)
-- while low sharpness only reduces penetration, so it's repaired
-- first; the AI picks up a remaining sharpness need on a later tick.
-- Returns severity, axis -- or nil if the item doesn't need repair.
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
M.severity = repairSeverity

-- Whether `it` is currently something the autonomous repair AI would
-- pick up on its own -- i.e. repairSeverity would return a candidate for
-- it (#303 review: without this check, the UI could offer/show
-- "priority" on an item above both thresholds, which the AI would then
-- never actually act on). repair_job is only registered for acolytes
-- today, so acolyte's thresholds are the only ones that matter; this
-- reads them directly rather than requiring a callers to know which
-- unit owns the item.
--
-- Since #1737 a ground row satisfies this the same way a held row does:
-- both carry condition, sharpness and kind from the same builder.
function unitAi.itemNeedsRepair(it)
    local params = config.acolyte
    if not params or not it then return false end
    return repairSeverity(it, params) ~= nil
end

-- Rank one candidate against the best so far under the ONE ordering
-- every rung shares: a player-prioritized instance beats an
-- unprioritized one outright, and among same-priority candidates the
-- more severe wins. Returns the new best triple.
local function rank(cand, sev, pri, best, bestSev, bestPri)
    local better = (pri and not bestPri)
                or (pri == bestPri and sev > bestSev)
    if better then return cand, sev, pri end
    return best, bestSev, bestPri
end

-- Best repair candidate among ownerUid's inventory + equipped gear +
-- accessories. Skips anything already claimed by another live unit.
local function scanHeldItems(ownerUid, actingUid, onMule, now, params, ctx)
    local best, bestSev, bestPri = nil, 0, false
    local function consider(it)
        if ctx.claimedByOther(it.instanceId, actingUid, now,
                              params.repair_claim_timeout) then
            return
        end
        local sev, axis = repairSeverity(it, params)
        if not sev then return end
        local cand = {
            instanceId = it.instanceId, defName = it.defName,
            axis = axis, severity = sev, onMule = onMule,
        }
        best, bestSev, bestPri = rank(cand, sev, ctx.isPriority(it.instanceId),
                                      best, bestSev, bestPri)
    end
    for _, it in ipairs(unit.getInventory(ownerUid) or {}) do consider(it) end
    for _, it in pairs(equipment.getLoadout(ownerUid) or {}) do consider(it) end
    for _, it in ipairs(equipment.getAccessories(ownerUid) or {}) do consider(it) end
    return best
end
M.scanHeldItems = scanHeldItems

-- Best repair candidate among GROUND instances within
-- params.repair_scan_range of the acting unit, on that unit's OWN page.
--
-- `gid` and `weight` are carried on the candidate because both are
-- needed before the item is ever touched: the gid names the row the
-- fetch phase re-resolves and picks up, and the weight is the row's
-- LIVE total mass (empty case + fill + nested contents), which is what
-- the capacity preflight must weigh -- a degraded kit's static def
-- weight would under-count everything it still holds.
local function scanGround(actingUid, info, now, params, ctx)
    local best, bestSev, bestPri = nil, 0, false
    local range = params.repair_scan_range or math.huge
    for _, g in ipairs(item.listGround() or {}) do
        local owned = item.getGroundForUnit(actingUid, g.id)
        if owned and owned.instanceId ~= nil
           and distance(info.gridX, info.gridY, owned.x, owned.y) <= range
           and not ctx.claimedByOther(owned.instanceId, actingUid, now,
                                      params.repair_claim_timeout) then
            local sev, axis = repairSeverity(owned, params)
            if sev then
                local cand = {
                    instanceId = owned.instanceId, defName = owned.defName,
                    axis = axis, severity = sev, onMule = false,
                    onGround = true, gid = g.id, weight = owned.weight,
                }
                best, bestSev, bestPri =
                    rank(cand, sev, ctx.isPriority(owned.instanceId),
                         best, bestSev, bestPri)
            end
        end
    end
    return best
end
M.scanGround = scanGround

-- The ladder itself (#302 scope, completed by #1737): own gear first
-- (nothing to fetch), then the ground (a walk), then the mule's spare
-- stock (a walk plus a hand-off). Each rung is consulted only when
-- every rung above it answered nothing, so priority and severity order
-- candidates within a rung and never across rungs.
function M.findCandidate(uid, info, params, ctx)
    local now = engine.gameTime()
    local best = scanHeldItems(uid, uid, false, now, params, ctx)
    if best then return best end
    best = scanGround(uid, info, now, params, ctx)
    if best then return best end
    local mule = findTechnomule(uid, info.gridX, info.gridY)
    if not mule then return nil end
    return scanHeldItems(mule.uid, uid, true, now, params, ctx)
end

-------------------------------------------------------------------
-- Taking a ground target, and putting it back
-------------------------------------------------------------------

-- One tick of the ground rung's approach-and-pickup, for a job whose
-- target is still lying on the ground. Returns:
--
--   "busy"  — a walk was issued (or the arrival gate refused nothing
--             yet); the caller returns and re-enters next tick;
--   "taken" — the EXACT instance is now in uid's inventory;
--   "lost"  — raced, gone, out of reach or too heavy. The job never
--             fetched anything, so the caller releases it outright.
--
-- Every field the decision reads comes from the row re-resolved on THIS
-- unit's own page (#1673): item.listGround chose the gid off the ACTIVE
-- page and item.pickupGround will commit on the carrier's, so an
-- unresolved id is not an answer and must not fall back to the listing.
function M.takeGroundTarget(uid, job, info, params)
    local row = item.getGroundForUnit(uid, job.groundGid)
    -- A gid is a per-page ALLOCATOR slot, so a raced pickup followed by
    -- a fresh drop can reuse it for a different item. The claim is on
    -- the INSTANCE, so the instance is what has to still be there.
    if not row or row.instanceId ~= job.instanceId then return "lost" end
    if distance(info.gridX, info.gridY, row.x, row.y)
       > params.pickup_arrival_tiles then
        unit.moveTo(uid, row.x, row.y, mv.comfort(uid))
        return "busy"
    end
    unit.stop(uid)
    -- Capacity again, on the resolved row's live weight: the load this
    -- worker carries changes en route (it may have picked the
    -- consumable up, or been handed something), so the preflight in
    -- repairUtility is necessary but not sufficient. Logged rather than
    -- reported to the player, matching fetchWantsFromGround's own
    -- capacity bail -- the same preflight re-weighs this candidate next
    -- tick and simply will not re-offer it, so this is not the re-claim
    -- storm reportFailure exists to announce.
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW = unit.getStat(uid, "carrying_capacity") or math.huge
    local w = row.weight or deliverItemWeight(row.defName)
    if carried + w > maxW then
        engine.logWarn("repair: unit " .. tostring(uid)
            .. " at capacity (" .. string.format("%.1f", carried + w)
            .. " > " .. string.format("%.1f", maxW)
            .. " kg) -- leaving ground " .. tostring(row.defName))
        return "lost"
    end
    -- Instance-preserving (#1737 requirement 6): the quality, fill,
    -- condition and sharpness repaired are the ones that lay on the
    -- ground. A false return is a raced target, never a reason to
    -- substitute another instance of the same def.
    if not item.pickupGround(uid, job.groundGid) then return "lost" end
    return "taken"
end

-- Put a ground-sourced target back down on the worker's own tile,
-- resolved on the worker's own page (#1208). Returns whether the drop
-- actually landed: a false here means the exact instance is STILL held,
-- and the caller must keep the job alive rather than release it, or the
-- target is stranded in an acolyte's inventory with nothing left that
-- knows it owes a drop.
function M.returnGroundTarget(uid, job)
    return unit.dropItemById(uid, job.instanceId) == true
end

return M
