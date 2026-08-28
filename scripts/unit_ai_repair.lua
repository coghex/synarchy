-- Unit AI equipment repair (#538 split from unit_ai.lua).
--
-- Action: repair_job (#302). AI-autonomous equipment maintenance: an
-- acolyte notices that gear it can reach has degraded past a threshold
-- and carries it to the right station (#301: furnace for condition,
-- workbench for sharpness) to restore it via repair.repairAt. No
-- engine-side spatial designation — a repair target is an item
-- INSTANCE, so claims are keyed by instanceId (repairClaims) rather
-- than tile, playing the same race-guard role a designation's status
-- does for dig/chop/construct.
--
-- The SOURCING LADDER is own held gear → a ground instance → the
-- technomule's spare stock, and scripts/unit_ai_repair_target.lua owns
-- every rung of it. #1737 completed the middle one: the ground scan
-- reads instanceId, kind, condition and sharpness off the SAME
-- pushGroundRow the consumable fetch already read x/weight from, so a
-- ground target is scored, claimed, prioritized and race-guarded by
-- exactly the machinery a held target is. (The stale "ground-item
-- targeting is out of scope" note this header carried is gone with it.)
--
-- A ground target is TAKEN with item.pickupGround, which preserves the
-- exact instance, and RETURNED with unit.dropItemById on the worker's
-- own tile when the job ends for any reason — completed, aborted, or
-- dropped by the post-load reconcile. `fromGround` remembers that
-- obligation across a save (lua.unit_ai v7), which is why it is durable
-- rather than derived from the phase.
--
-- State on s: repairJob = { instanceId, defName, axis, recipeId,
-- consumable, consumableCount, groundWant, muleWant, groundDone,
-- onMule, fromGround, groundGid, itemFetched, bid }; repairPhase =
-- "fetch_ground" | "fetch_item" | "fetch_consumable" | "walking" |
-- "repairing" | "returning".
-----------------------------------------------------------

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local distance        = core.distance
local chebToFootprint = core.chebToFootprint
local reportFailure   = core.reportFailure
local grantWorkXP     = core.grantWorkXP

local fetch = require("scripts.unit_ai_fetch")
local findTechnomule       = fetch.findTechnomule
local deliverItemWeight    = fetch.deliverItemWeight
local inventoryCountOf     = fetch.inventoryCountOf
local fetchWantsFromGround = fetch.fetchWantsFromGround
local fetchWantsFromMule   = fetch.fetchWantsFromMule

-- Candidate selection — the whole own/ground/mule ladder — lives in its
-- own module (#1737, #538 line budget). It reads the claim and priority
-- tables below through an explicit ctx rather than owning them: they are
-- session coordination state this module releases and refreshes on every
-- tick, and splitting the registry away from the code that mutates it
-- would put the two out of reach of one another.
local targets = require("scripts.unit_ai_repair_target")

local mv = require("scripts.movement_speed")
local page = require("scripts.unit_ai_page")
local roles = require("scripts.unit_roles")
-- The #1329 load reset: both tables below are transient session
-- coordination state, emptied in place whenever a load replaces the
-- session (item-instance ids rewind, so a stale entry would otherwise
-- attach to an unrelated item).
local claimsLib = require("scripts.unit_ai_claims")

local repairUtility, repairExecute, repairOnExit, repairAbort

do
local repairClaims = claimsLib.track({})   -- instanceId → { uid, at }

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
--
-- TRANSIENT, deliberately (#1329 requirement 6): a load empties this
-- along with the claims. Persisting it as durable player intent would
-- need a typed reference kind, a lua.unit_ai version bump and a
-- save-compat fixture; without one, an inherited flag would land on
-- whatever unrelated item reuses the id after the allocator rewinds.
-- A player who had prioritized an item re-flags it after loading.
local repairPriority = claimsLib.track({})   -- instanceId → true

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
    -- #1737: a ground-sourced target goes back to the GROUND, on this
    -- worker's own tile and own page, never to a mule. A failed drop
    -- (no live page) leaves it still held, so the job is parked in
    -- "returning" and retried instead of released -- ending it here
    -- would strand the instance in an inventory nothing now tracks.
    if job and job.itemFetched and job.fromGround then
        if not targets.returnGroundTarget(uid, job) then
            s.repairPhase = "returning"
            return
        end
    elseif job and job.itemFetched and info then
        local mule = findTechnomule(uid, info.gridX, info.gridY)
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
-- Published for the post-load reconcile (#1589), which must drop a
-- repairJob whose item instance or station no longer exists. That is an
-- abort like any other -- it has the same fetched item to hand back and
-- the same claim to release -- so it goes through this path rather than
-- assigning s.repairJob = nil. Callers pass `info` only when the mule
-- search below is safe for that unit; see scripts/unit_ai.lua's
-- reconcileDropHooks.
repairAbort = abortRepairJob

-- How unit_ai_repair_target.lua's scans read this module's two session
-- registries. Built once; both closures are pure lookups.
local targetCtx = {
    claimedByOther = function(iid, uid, now, timeout)
        return repairClaimedByOther(iid, uid, now, timeout)
    end,
    isPriority = function(iid) return repairPriority[iid] == true end,
}

function repairUtility(uid, s, params)
    if s.repairJob then return params.repair_lock_utility end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local cand = targets.findCandidate(uid, info, params, targetCtx)
    if not cand then return -math.huge end

    -- Only claim if a station for this axis is actually reachable —
    -- mirrors dig's tool gate / construct's materials-available check.
    -- building.findStation ranks candidates by Chebyshev distance from
    -- the given (gx, gy), but its Lua.tointeger argument parsing only
    -- accepts whole numbers — a raw unit position like 16.5 silently
    -- fails to parse, and it falls back to "no distance info" (lowest
    -- building id wins, ignoring proximity entirely). Floor first.
    local recipeId = "repair_" .. cand.axis
    -- #1673: findStation ranks over the ACTIVE page, which is not
    -- necessarily this unit's, so the station it names is checked
    -- against the ACTOR's own page before the job may be scored at all
    -- — otherwise a fresh job starts against a foreign station and its
    -- fetch phases walk and move items before the walking phase finally
    -- rejects it. The id is RETAINED on the candidate rather than
    -- re-resolved in execute: a second findStation call would take its
    -- own, independent active-page snapshot and could answer
    -- differently from the one this gate approved.
    local stationBid = building.findStation(recipeId, math.floor(info.gridX),
                                            math.floor(info.gridY))
    if not stationBid then return -math.huge end
    local sinfo = building.getInfo(stationBid)
    if not sinfo or not page.same(info.page, sinfo.page) then
        return -math.huge
    end
    cand.bid = stationBid

    local recipe = repair.get(recipeId)
    local input = recipe and recipe.inputs and recipe.inputs[1]
    if not input then return -math.huge end

    -- Capacity feasibility: a mule-sourced item adds its own weight on
    -- pickup, and the recipe's consumable adds more on top — a unit
    -- whose remaining headroom can't cover both would otherwise claim,
    -- fail to fetch, and immediately re-claim the SAME candidate (still
    -- degraded, now sitting in its own inventory) forever — a repeated
    -- "unit_warning" pause storm (config/notifications.local.yaml pauses
    -- on that category) instead of a clean "can't do this job right now" bail.
    local needed = 0
    if cand.onMule then
        needed = needed + deliverItemWeight(cand.defName)
    elseif cand.onGround then
        -- The resolved row's LIVE total mass (empty case + fill +
        -- nested contents), never the static def weight: #1737
        -- requirement 6 keeps fill and nested contents intact across
        -- the pickup, so a half-full canteen or a stocked kit weighs
        -- what it actually weighs. Same number the arrival-time gate
        -- re-reads, so a candidate that passes here and is still
        -- carriable on arrival cannot be refused by a different rule.
        needed = needed + (cand.weight or deliverItemWeight(cand.defName))
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
            -- #1737: DURABLE provenance, not a phase-derived guess. A
            -- save can land anywhere between the pickup and the repair,
            -- and afterwards only the job itself can say "this instance
            -- came off the ground and owes a drop, not a hand-back to a
            -- mule". groundGid is the page-local id the fetch phase
            -- re-resolves, cleared the moment the pickup lands.
            fromGround = cand.onGround or nil,
            groundGid = cand.onGround and cand.gid or nil,
            -- #1673: the station repairUtility already vetted against
            -- this unit's page. Carried in so the guard below covers
            -- the job from its first tick, fetch phases included.
            bid = cand.bid,
        }
        s.repairPhase = (cand.onGround and "fetch_ground")
                     or (cand.onMule and "fetch_item")
                     or "fetch_consumable"
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

    -- #1737: the job is over and only the ground-sourced target's drop
    -- is left. Reached when abortRepairJob's drop failed (no live page),
    -- and deliberately AHEAD of the station revalidation below, since
    -- the usual reason we are here is that the station just went away.
    if s.repairPhase == "returning" then
        if unit.dropItemById(uid, job.instanceId) then
            releaseRepairJob(s, uid)
        end
        return
    end

    -- #1673: job.bid is a PERSISTED building reference, so a save
    -- written before this check (or a page switch mid-job) can name a
    -- station on another world. Revalidate it ahead of EVERY phase --
    -- fetch_item and fetch_consumable below issue their own moveTo /
    -- transferItemToUnit / pickupGround calls, so the walking-phase
    -- check alone is too late, exactly as it was for deliverExecute
    -- and craftExecute. A job that has not resolved a station yet is
    -- untouched: job.bid stays nil until the walking phase calls
    -- building.findStation, and that fresh selection is validated
    -- there.
    if job.bid then
        local binfo = building.getInfo(job.bid)
        if not binfo or not page.same(info.page, binfo.page) then
            abortRepairJob(uid, s, info); return
        end
    end

    if s.repairPhase == "fetch_ground" then
        -- The ground rung itself -- re-resolve, walk, weigh, take --
        -- belongs to unit_ai_repair_target.lua; this is the phase
        -- machine reacting to its verdict.
        local verdict = targets.takeGroundTarget(uid, job, info, params)
        if verdict == "taken" then
            job.itemFetched = true
            job.groundGid = nil
            s.repairPhase = "fetch_consumable"
        elseif verdict == "lost" then
            releaseRepairJob(s, uid)   -- never fetched
        end
        return
    end

    if s.repairPhase == "fetch_item" then
        local mule = findTechnomule(uid, info.gridX, info.gridY)
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
        -- #1673: job.bid is a PERSISTED building reference and
        -- building.findStation resolves against the ACTIVE page, not
        -- necessarily this unit's. Revalidate before the walk.
        if not binfo or not page.same(info.page, binfo.page) then
            abortRepairJob(uid, s, info); return
        end
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


return { utility = repairUtility, execute = repairExecute,
         onExit = repairOnExit, abort = repairAbort }
