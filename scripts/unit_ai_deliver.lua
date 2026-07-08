-- Unit AI delivery to build sites (#538 split from unit_ai.lua).
--
-- Fires when (a) an Appearing building within scan range still has
-- unmet material need and (b) this unit carries at least one of
-- those materials. On first selection, the unit "claims" a delivery
-- plan onto aiState[uid].deliveryClaim — a map {[type] = count} —
-- so other acolytes computing utility see reduced remaining need and
-- only the minimum number of them commit. Lock-in: utility returns a
-- finite 6.0 while a claim is held so the walk-and-deliver sequence
-- isn't pre-empted by routine wander/follow_command, while dire needs
-- (drink/eat, ~10 when the meter is nearly empty) still interrupt;
-- the claim persists through the interruption and delivery resumes.
--
-- Reservations from non-existent units are ignored (stale-claim
-- self-heal) so a dying acolyte's lock doesn't strand materials.
-- Sources the shortfall via unit_ai_fetch's ladder (inventory →
-- nearby ground → technomule).

local core = require("scripts.unit_ai_core")
local aiState        = core.aiState
local distance        = core.distance
local chebToFootprint = core.chebToFootprint

local fetch = require("scripts.unit_ai_fetch")
local inventoryCountOf     = fetch.inventoryCountOf
local fetchWantsFromGround = fetch.fetchWantsFromGround
local fetchWantsFromMule   = fetch.fetchWantsFromMule

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local M = {}


-- How many units of matType still need a deliverer, considering both
-- engine-side delivered counts AND other live acolytes' active
-- claims. excludeUid lets the caller drop their own claim from the
-- sum so they can re-evaluate without double-counting.
local function remainingUnclaimedNeed(bid, matType, need, delivered, excludeUid)
    local claimed = 0
    for otherUid, st in pairs(aiState) do
        if otherUid ~= excludeUid and st.deliveryClaim
           and st.deliveryClaim.bid == bid then
            if unit.getInfo(otherUid) then    -- skip stale claims
                claimed = claimed + (st.deliveryClaim.materials[matType] or 0)
            end
        end
    end
    return math.max(0, need - (delivered[matType] or 0) - claimed)
end

local function findDeliveryTarget(uid, fromX, fromY, params)
    -- Active-world buildings only — building.list() is global across every
    -- live world page, so off-world buildings used to leak in as delivery
    -- targets (#197). building.getActiveIds() is page-scoped, matching the
    -- active-world unit.getAllIds the actors come from.
    local ids = building.getActiveIds()
    if not ids or #ids == 0 then return nil end
    local best, bestD = nil, params.deliver_scan_range
    for _, bid in ipairs(ids) do
        if bid and building.getActivity(bid) == "appearing"
           and not building.areMaterialsSatisfied(bid) then
            local need      = building.getMaterialNeed(bid) or {}
            local delivered = building.getMaterialDelivered(bid) or {}
            -- My potential contribution per material: own inventory
            -- first, then nearby GROUND items, then the technomule's
            -- stock for the rest of the shortfall (#96 sourcing ladder).
            -- (Concurrent claimants can plan on the same ground item or
            -- mule stock — the pickup/transfer fails gracefully for the
            -- loser and the next utility pass re-claims what's still
            -- needed, so the race resolves itself.)
            local mule = findTechnomule(fromX, fromY)
            local claim, fromGround, fromMule = {}, {}, {}
            local anyClaim, anyFromGround, anyFromMule = false, false, false
            for matType, count in pairs(need) do
                local remaining = remainingUnclaimedNeed(bid, matType,
                                       count, delivered, uid)
                if remaining > 0 then
                    local have = inventoryCountOf(uid, matType)
                    local groundHave = groundCountOf(fromX, fromY, matType,
                                           params.deliver_scan_range)
                    local muleHave = mule
                        and inventoryCountOf(mule.uid, matType) or 0
                    local bring = math.min(have + groundHave + muleHave,
                                           remaining)
                    if bring > 0 then
                        claim[matType] = bring
                        anyClaim = true
                        local short = bring - have
                        if short > 0 then
                            local fromG = math.min(groundHave, short)
                            if fromG > 0 then
                                fromGround[matType] = fromG
                                anyFromGround = true
                            end
                            if short - fromG > 0 then
                                fromMule[matType] = short - fromG
                                anyFromMule = true
                            end
                        end
                    end
                end
            end
            if anyClaim then
                local info = building.getInfo(bid)
                if info then
                    local tw = info.tileW or 1
                    local th = info.tileH or 1
                    local cx = info.gridX + tw / 2
                    local cy = info.gridY + th / 2
                    local d  = distance(fromX, fromY, cx, cy)
                    if d <= bestD then
                        best = {
                            bid = bid, gridX = info.gridX, gridY = info.gridY,
                            tileW = tw, tileH = th, distance = d,
                            claim = claim,
                            fromGround = anyFromGround and fromGround or nil,
                            fromMule = anyFromMule and fromMule or nil,
                        }
                        bestD = d
                    end
                end
            end
        end
    end
    return best
end

local function deliverUtility(uid, s, params)
    -- Locked in once claim is set. The lock survives across ticks so
    -- the walk-and-deliver sequence isn't yanked by ambient utility.
    -- Finite (see header comment) so dire drink/eat still preempt.
    if s.deliveryClaim then return 6.0 end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local target = findDeliveryTarget(uid, info.gridX, info.gridY, params)
    if not target then return -math.huge end

    s.deliveryPendingTarget = target
    -- Role weight (#265) on the entry only — the 6.0 claim lock above
    -- stays flat. Max weighted entry = 4.0 · 1.4 = 5.6 < 6.0.
    return params.deliver_utility * roles.weight(s, "deliver_to_build_site")
end

local function deliverExecute(uid, s, params)
    -- Lock in claim on first call so subsequent ticks (and other
    -- acolytes' utility checks) see the reservation.
    if not s.deliveryClaim then
        local target = s.deliveryPendingTarget
        if not target then return end
        s.deliveryClaim = { bid = target.bid, materials = target.claim,
                            fromGround = target.fromGround,
                            fromMule = target.fromMule }
        s.deliveryPendingTarget = nil
    end

    local info = unit.getInfo(uid)
    if not info then return end

    -- Source the shortfall before heading to the build site: own
    -- inventory first, then nearby ground items, then the technomule
    -- (#96 sourcing ladder).
    local claim = s.deliveryClaim
    if claim.fromGround then
        if fetchWantsFromGround(uid, claim.fromGround, params,
                                params.deliver_scan_range) then
            return
        end
        claim.fromGround = nil
    end
    if claim.fromMule then
        if fetchWantsFromMule(uid, claim.fromMule, info, params) then
            return
        end
        claim.fromMule = nil
    end

    -- Reconcile the plan against what actually landed in inventory —
    -- fetch sources can come up short (raced pickers, emptied mule,
    -- capacity). Clamping the claim frees the difference for other
    -- acolytes' remaining-need math immediately. Idempotent once the
    -- fetch phases are done, so re-running it every tick is fine.
    for matType, count in pairs(claim.materials) do
        local have = inventoryCountOf(uid, matType)
        claim.materials[matType] = have > 0 and math.min(count, have) or nil
    end

    -- Fetch may have emptied the claim entirely (sources gone / raced).
    if not next(claim.materials) then
        s.deliveryClaim = nil
        return
    end

    local binfo = building.getInfo(s.deliveryClaim.bid)
    if not binfo then
        -- Building destroyed between claim and arrival. Release.
        s.deliveryClaim = nil
        return
    end

    local utx  = math.floor(info.gridX)
    local uty  = math.floor(info.gridY)
    local cheb = chebToFootprint(utx, uty, binfo.gridX, binfo.gridY,
                                 binfo.tileW or 1, binfo.tileH or 1)

    if cheb > 1 then
        -- Walk to the nearest border tile. Same approach as build_nearby.
        local bestX, bestY, bestD = nil, nil, math.huge
        local tw = binfo.tileW or 1
        local th = binfo.tileH or 1
        for dx = -1, tw do
            for dy = -1, th do
                if dx == -1 or dx == tw or dy == -1 or dy == th then
                    local nx = binfo.gridX + dx + 0.5
                    local ny = binfo.gridY + dy + 0.5
                    local d  = distance(info.gridX, info.gridY, nx, ny)
                    if d < bestD then
                        bestX, bestY, bestD = nx, ny, d
                    end
                end
            end
        end
        if bestX then
            unit.moveTo(uid, bestX, bestY, mv.comfort(uid))  -- hauling → comfort
        end
        return
    end

    -- Arrived. Hand over each claimed material, one ItemInstance at
    -- a time so per-item state (condition on motors/processors) is
    -- preserved through to the building's biMaterialsDelivered list.
    for matType, count in pairs(s.deliveryClaim.materials) do
        for _ = 1, count do
            if not unit.transferItemToBuilding(uid, s.deliveryClaim.bid,
                                               matType) then
                break    -- inventory ran out or building gone
            end
        end
    end

    s.deliveryClaim = nil
end


M.deliverUtility = deliverUtility
M.deliverExecute = deliverExecute

return M
