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
        if job.status == "claimed" then
            local key = constructKey(wid, job.x, job.y)
            local c = constructClaims[key]
            if not c then
                -- Orphan (loaded save / script reload): adopt with an
                -- anonymous timer so it frees up if nobody owns it.
                constructClaims[key] = { uid = nil, at = now }
            elseif (c.uid and not unit.exists(c.uid))
                   or (now - c.at > timeout) then
                constructClaims[key] = nil
                construction.setJobStatus(wid, job.x, job.y, "pending",
                                          job.attempt)
            end
        end
    end
end

return M
