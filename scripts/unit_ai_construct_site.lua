-- Unit AI construction SITE mechanics (#1844 split from
-- scripts/unit_ai_construct.lua, which the #538 500-line budget caps).
--
-- Everything here answers a question about the job's TILE rather than
-- about the worker: what it costs, where to stand, which structure slot
-- it fills, how the piece is finally placed, and what a cancellation
-- returns to the ground. The orchestration -- claims, utility, the phase
-- machine -- stays in unit_ai_construct.lua.

local fetch = require("scripts.unit_ai_fetch")
local inventoryCountOf = fetch.inventoryCountOf
local groundCountOf    = fetch.groundCountOf
local findTechnomule   = fetch.findTechnomule
local loadFeasible     = fetch.loadFeasible

local M = {}

-- Structure-pack build costs. The REGISTERED catalogue (#1842/#1844) is
-- the authority: it is what construction.payMaterials actually charges
-- and what a refund receipt records, so planning a fetch against
-- anything else would plan for a cost the engine will not take. The pack
-- YAML stays a fallback for the headless fixtures that load this module
-- against a bare Lua backend with no structure.* namespace.
--
-- Cached per (pack, kind), not per pack, because the engine answers per
-- kind. `false` means "no build metadata": those designations are
-- skipped, exactly as a pack with no build: block always was.
local packBuildCache = {}
function M.packBuildInfo(pack, kind)
    local key = tostring(pack) .. "/" .. tostring(kind)
    local c = packBuildCache[key]
    if c ~= nil then
        if c == false then return nil end
        return c
    end
    local info = nil
    if structure and structure.packBuildCost then
        info = structure.packBuildCost(pack, kind)
    else
        local y = engine.loadYaml("data/structure_packs/" .. pack .. ".yaml")
        local bl = y and y.build
        info = bl and bl[kind] or nil
    end
    packBuildCache[key] = info or false
    return info
end

-- Stand position: nearest neighbouring tile's centre — beside the job
-- tile, never on it (a wall materialising around the builder is wrong).
function M.constructStandPos(job, px, py)
    local bestX, bestY, bestD = nil, nil, math.huge
    for _, o in ipairs({ {1, 0}, {-1, 0}, {0, 1}, {0, -1} }) do
        local nx = job.x + o[1] + 0.5
        local ny = job.y + o[2] + 0.5
        local d = (nx - px) ^ 2 + (ny - py) ^ 2
        if d < bestD then bestX, bestY, bestD = nx, ny, d end
    end
    return bestX, bestY
end

-- The structure.hasAt slot this job places into — mirrors
-- placeStructurePiece's kind/edge → slot derivation (#805) so the
-- pre-payment occupancy check below targets the EXACT slot.
function M.jobSlot(job)
    if job.kind == "floor" then return "floor"
    elseif job.kind == "ceiling" then return "ceiling"
    elseif job.kind == "wall" then return "wall_" .. (job.edge or "ne")
    elseif job.kind == "post" then return "post_" .. (job.edge or "n")
    elseif job.kind == "wire" then return "wire"
    end
    return nil
end

-- Place via the structures module. Every kind can fail mid-job (its
-- target chunk unloads, not just a post's vanished floor, #799) — log
-- rather than strand the job; false lets the caller apply the refund.
function M.placeStructurePiece(job)
    local structures = require("scripts.structures")
    local ok
    if job.kind == "floor" then
        ok = structures.floor(job.x, job.y)
    elseif job.kind == "ceiling" then
        ok = structures.ceiling(job.x, job.y)
    elseif job.kind == "wall" then
        ok = structures.wall(job.x, job.y, job.edge or "ne")
    elseif job.kind == "post" then
        -- No corner in the designation (the tool's hover pick does);
        -- default "n" until the tool grows a corner picker.
        ok = structures.post(job.x, job.y, job.edge or "n")
    elseif job.kind == "wire" then
        ok = require("scripts.wire").place(job.x, job.y)
    else
        ok = false
    end
    if not ok then
        engine.logWarn("construct: " .. tostring(job.kind) .. " at " .. job.x
            .. "," .. job.y .. " failed to place mid-job — skipping placement")
    end
    return ok
end

-- Refund a cancelled structure job's ALREADY-PAID materials to the
-- ground from its durable RECEIPT, and from nothing else (#1844).
--
-- #799 refunded the CURRENT pack cost instead, which cannot reproduce
-- what was actually spent once a pack's build: costs change or the pack
-- goes away: the job would be refunded the new cost, or nothing at all.
-- The receipt records the exact multiset that left an inventory for THIS
-- attempt, so it is the only thing consulted here. Its presence is also
-- the paid state, so a job with no receipt refunds nothing by
-- construction rather than by a second flag agreeing with a first.
--
-- `job` is a POPPED designation table (construction.
-- cancelDesignationForRefund), which is what makes the refund happen
-- exactly once: only the caller whose atomic delete won is handed one.
function M.refundStructureMaterials(job)
    if job.category ~= "structure" then return end
    for _, entry in ipairs(job.receipt or {}) do
        for _ = 1, (entry.count or 0) do
            item.spawnGround(entry.name, job.x + 0.5, job.y + 0.5)
        end
    end
end

-- Can this unit source every material the piece needs (inventory +
-- ground + mule) AND carry the shortfall (#1326)? Races lose gracefully
-- at fetch time; this is only the "worth claiming" filter.
function M.materialsAvailable(uid, fromX, fromY, mats, params)
    if not loadFeasible(uid, mats) then return false end
    for matType, need in pairs(mats or {}) do
        local have = inventoryCountOf(uid, matType)
        if have < need then
            local ground = groundCountOf(uid, fromX, fromY, matType,
                                         params.construct_scan_range)
            if have + ground < need then
                local mule = findTechnomule(uid, fromX, fromY)
                local muleHave = mule and inventoryCountOf(mule.uid, matType) or 0
                if have + ground + muleHave < need then return false end
            end
        end
    end
    return true
end

-- The SHARED resolver's verdict for one exact attempt (#1844
-- requirement 10), or nil when the job is gone / is a building.
--
-- The worker asks this before it claims, before it pays and before it
-- places. A designation admitted minutes ago is not evidence it is still
-- buildable, and the world-side invalidator does not fire for every
-- reason at every moment: nothing sweeps on the tick this unit happens
-- to arrive. Asking the resolver is what makes the worker's view the
-- SAME view admission had, instead of the three ad-hoc checks it used to
-- make (a pack lookup, a floorZAt, a hasAt).
--
-- Absent verb (the bare-Lua headless fixtures) degrades to "valid": the
-- engine-side guards on payment and placement still refuse anything the
-- resolver would have, so this can loosen scheduling but never
-- correctness.
function M.planOutcome(wid, job)
    if job.category == "building" then return nil end
    if not (construction and construction.resolvePlan) then return "valid" end
    return construction.resolvePlan(wid, job.x, job.y, job.attempt)
end

-- Stale-claim sweep over the scanned job list: a "claimed" job whose
-- claimant died is released immediately; one unrefreshed past the
-- timeout (stuck worker, adopted orphan from a save/reload) releases
-- when the clock runs out. Any scanning acolyte runs this.
function M.sweepClaims(constructClaims, constructKey, wid, jobs, now, timeout)
    for _, job in ipairs(jobs) do
        -- "placing" is swept exactly like "claimed" (#1844). The
        -- hand-off is one Lua callback wide, so a job still in it a
        -- timeout later is a claimant that died or errored mid-placement
        -- -- and the engine refuses to CANCEL a placing designation, so
        -- without this the tile would be stranded for good.
        if job.status == "claimed" or job.status == "placing" then
            local key = constructKey(wid, job.x, job.y)
            local c = constructClaims[key]
            if not c then
                -- Orphan (loaded save / script reload): adopt with an
                -- anonymous timer so it frees up if nobody owns it.
                -- Adopted with the attempt it is a claim ON, so a later
                -- invalidation for a DIFFERENT attempt cannot erase it.
                constructClaims[key] = { uid = nil, at = now,
                                         attempt = job.attempt }
            elseif (c.uid and not unit.exists(c.uid))
                   or (now - c.at > timeout) then
                constructClaims[key] = nil
                construction.setJobStatus(wid, job.x, job.y, "pending",
                                          job.attempt)
            end
        end
    end
end

-- Stake a BUILDING blueprint, and hold the designation until the
-- building it stakes is really on screen (#1845).
--
-- `building.spawn` returns as soon as the insertion is QUEUED: it lands
-- on the building queue, which the UNIT thread drains, while
-- `setJobStatus("complete")` removes the designation on the WORLD
-- queue. The two are applied independently, so reporting the completion
-- straight after the spawn can delete the designation ghost a frame or
-- more before the staked building exists to draw its own, and the site
-- blinks empty. The designation ghost and the staked pre-delivery ghost
-- are the same 60 % picture of the same definition at the same anchor
-- (the renderer yields the designation once both exist), so simply
-- holding the designation until the instance is observable makes the
-- whole hand-off invisible.
--
-- The wait normally ends on the very next tick. The deadline is for the
-- one case where the queued spawn is dropped outright — its page torn
-- down, its definition unregistered — and there an empty site is the
-- honest picture, because nothing was built.
--
-- Returns "working" while the unit still owes this job something
-- (walking, or waiting on the stake), and "done" or "gone" when the
-- caller should release it.
function M.stakeBuilding(wid, job, uid, info, now, params)
    local core = require("scripts.unit_ai_core")
    local mv = require("scripts.movement_speed")
    if not job.stakedBid then
        if core.distance(info.gridX, info.gridY,
                         job.x + 0.5, job.y + 0.5) > 2.2 then
            unit.moveTo(uid, job.x + 0.5, job.y + 1.5, mv.comfort(uid))
            return "working"
        end
        unit.stop(uid)
        job.stakedBid = building.spawn(job.building, job.x, job.y)
        job.stakedAt = now
        if not job.stakedBid then
            -- Placement invalid (terrain changed, overlap) — retrying
            -- can't succeed, so cancel the blueprint and say so.
            core.reportFailure(uid, "Can't build here — blueprint cancelled")
            construction.cancelDesignation(job.x, job.y, job.attempt)
            return "gone"
        end
    end
    -- `building.getInfo` reads the same manager the renderer draws from,
    -- so "observable here" is exactly "drawable there".
    local visible = building.getInfo and building.getInfo(job.stakedBid)
    local timeout = params.construct_stake_visible_timeout or 5.0
    if not visible and (now - (job.stakedAt or now)) <= timeout then
        return "working"
    end
    construction.setJobStatus(wid, job.x, job.y, "complete", job.attempt)
    return "done"
end

-- Finish one job: requirement 10's third re-check, requirement 18's
-- exact-attempt placement hand-off, the placement itself, and the
-- completion-or-refund that follows it (#1844).
--
-- Returns true when the piece was placed — the caller grants the XP on
-- that, since XP is for work PERFORMED — and false when the attempt was
-- gone or the site had stopped being buildable. EITHER WAY the job is
-- over and the caller releases it.
function M.finishPlacement(wid, job, uid)
    local reportFailure = require("scripts.unit_ai_core").reportFailure
    -- The third of requirement 10's three re-checks, BEFORE the
    -- hand-off: a site that has gone invalid is cancelled, and its
    -- receipt refunded, rather than built.
    local finalPlan = M.planOutcome(wid, job)
    -- …except that unloaded terrain is NOT a refusal, here or anywhere
    -- else in this arc. A chunk evicting between the building-phase
    -- check and this one says nothing about the site; cancelling on it
    -- would delete a valid paid job over a condition that resolves
    -- itself. The job is DEFERRED instead: the designation stands, and
    -- the caller hands the tile back to the scan pool.
    if finalPlan == "unresolved-terrain" then return "deferred" end
    if finalPlan and finalPlan ~= "valid" then
        local removed = construction.cancelDesignationForRefund(
            wid, job.x, job.y, job.attempt)
        if removed then M.refundStructureMaterials(removed) end
        reportFailure(uid,
            "Construction site changed — materials returned to the ground")
        return "gone"
    end
    -- Requirement 18: take the exact-attempt hand-off BEFORE placing
    -- anything. The piece becomes visible to every structure query the
    -- moment it is staged, so without this the world-side invalidator
    -- could read this worker's own success as an external conflict,
    -- cancel the job and refund materials that were correctly spent. A
    -- false answer means the attempt is gone: place nothing.
    if not construction.beginPlacement(wid, job.x, job.y, job.attempt) then
        return "gone"
    end
    -- structure.place returning true means STAGED AND QUEUED, not
    -- committed -- the world thread still declines a queued placement
    -- whose target chunk evicted in between. The page's stage watermark
    -- read either side of the run gives that placement's COMMIT WINDOW,
    -- which the completion carries so the world thread can withhold it
    -- (and refund the receipt) when the placement never landed. Exactly
    -- what scripts/locations.lua does for its own stamp (#2051).
    local fromTok = structure.stageWatermark and structure.stageWatermark(wid)
    local placed = M.placeStructurePiece(job)
    local toTok = structure.stageWatermark and structure.stageWatermark(wid)
    if placed then
        construction.setJobStatus(wid, job.x, job.y, "complete", job.attempt,
                                  fromTok, toTok)
    else
        -- Placement failed outright: nothing was staged, so this claimant
        -- ABORTS the hand-off it took rather than cancelling. An ordinary
        -- cancel is refused while a designation is `placing` (it would
        -- refund a receipt while a queued placement still lands), and
        -- this is the one caller that knows there is no such placement.
        -- Still an atomic exact-attempt pop, so the receipt is refunded
        -- exactly once.
        local removed = construction.abortPlacement(
            wid, job.x, job.y, job.attempt)
        if removed then M.refundStructureMaterials(removed) end
        reportFailure(uid,
            "Construction site changed — materials returned to the ground")
    end
    return placed and "placed" or "gone"
end

-- Externally interrupt a live claimant on (wid,gx,gy) — e.g. a
-- cancelled paid job (#799): s.constructJob is a LOCAL copy that keeps
-- ticking regardless of the engine-side designation, and
-- constructUtility only notices on that unit's own next decision tick
-- — too late to stop it placing the piece. This clears the claim
-- immediately instead. `wid` is the CANCELLED job's own page.
--
-- #1844 also reaches it from the WORLD side, through unit_ai.lua's
-- onConstructInvalidated broadcast: an invalidated designation whose
-- claim registry entry survived would keep the tile reserved until the
-- claimant's next decision tick or its claim timeout, blocking a
-- successor designated there immediately.
function M.abandonClaim(constructClaims, constructKey, wid, gx, gy, attempt)
    local key = constructKey(wid, gx, gy)
    local c = constructClaims[key]
    -- #1844: the registry entry is removed only when it belongs to the
    -- ATTEMPT being abandoned. A delayed invalidation for a retired
    -- attempt arriving after someone has claimed a SUCCESSOR at the same
    -- tile must not erase the successor's claim -- the tile would then
    -- look unclaimed to the next scanner while the successor's own
    -- `claimed` status was still queued, and the stale-claim sweep would
    -- read it as an orphan. The claim carries its attempt for exactly
    -- this comparison; an entry that predates one (an adopted orphan) is
    -- matched by coordinate as before.
    local mine = c ~= nil
        and (attempt == nil or c.attempt == nil or c.attempt == attempt)
    if mine then constructClaims[key] = nil end
    if not (c and c.uid) then return end
    local s = require("scripts.unit_ai").getState(c.uid)
    local job = s and s.constructJob
    -- The same rule for the claimant's own job copy: a worker that has
    -- since claimed a successor at this tile keeps it, because the
    -- coordinate alone can no longer tell the two apart.
    if job and job.x == gx and job.y == gy
       and (attempt == nil or job.attempt == attempt) then
        s.constructJob = nil
        s.constructCandidate = nil
    end
end

return M
