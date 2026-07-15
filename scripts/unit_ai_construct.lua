-- Unit AI construction jobs (#538 split from unit_ai.lua).
--
--
-- Executes construction designations (#95). Two job categories:
--   * "building": walk to the blueprint and STAKE it — building.spawn
--     places the Appearing ghost, the designation completes, and the
--     existing deliver_to_build_site + build_nearby machinery takes
--     over (materials gate + worker-rate progress, unchanged).
--   * "structure": the full job — source the piece's materials
--     (inventory → ground → mule, same ladder as delivery), walk
--     beside the tile, pour work into the designation
--     (construction.addJobProgress; the blueprint ghost solidifies as
--     progress accrues), and at 1.0 place the piece via
--     scripts/structures.lua and mark the job complete.
--
-- Claims: one worker per tile. The synchronous guard is a module-local
-- registry (constructClaims, same shape as digClaims); the engine-side
-- designation status ("claimed") is the durable/observable layer —
-- getPendingJobs carries it, and the sweep below releases a dead or
-- expired claimant's job back to "pending" so another acolyte picks it
-- up. Claims from a save (or a Lua reload) arrive with no registry
-- entry; they're adopted with an anonymous timer and released the same
-- way if nobody refreshes them.
--
-- Material races between construct jobs (and against deliveries) are
-- NOT reserved cross-unit: the fetch fails gracefully for the loser,
-- the post-fetch inventory check releases the job back to pending, and
-- the next scan re-plans — same self-heal as the mule-stock race.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local distance        = core.distance
local chebToFootprint = core.chebToFootprint
local reportFailure   = core.reportFailure
local grantWorkXP     = core.grantWorkXP

local fetch = require("scripts.unit_ai_fetch")
local inventoryCountOf     = fetch.inventoryCountOf
local groundCountOf        = fetch.groundCountOf
local findTechnomule       = fetch.findTechnomule
local fetchWantsFromGround = fetch.fetchWantsFromGround
local fetchWantsFromMule   = fetch.fetchWantsFromMule

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local M = {}

local constructClaims = {}   -- "x,y" → { uid = ..., at = gameTime }

local function constructKey(x, y) return x .. "," .. y end

local function constructClaimedByOther(key, uid, now, timeout)
    local c = constructClaims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or (c.uid and not unit.exists(c.uid)) then
        constructClaims[key] = nil
        return false
    end
    return true
end

-- Structure-pack build costs, lazily loaded from the pack YAML's
-- build: block (keyed by piece KIND — wall edges share one entry).
-- false = pack has no build block (debug/stamp-only pack): its
-- designations are skipped, nothing to cost the job with.
local packBuildCache = {}
local function packBuildInfo(pack, kind)
    local p = packBuildCache[pack]
    if p == nil then
        local y = engine.loadYaml("data/structure_packs/" .. pack .. ".yaml")
        p = (y and y.build) or false
        packBuildCache[pack] = p
    end
    if not p then return nil end
    return p[kind]
end

-- Release the unit's hold on its construct job. toPending flips the
-- engine-side status back so another worker can take the tile;
-- omitted = the designation is already gone (completed / cancelled).
local function releaseConstructJob(s, uid, toPending)
    local job = s.constructJob
    if job then
        local key = constructKey(job.x, job.y)
        local c = constructClaims[key]
        if c and c.uid == uid then constructClaims[key] = nil end
        if toPending then
            local wid = world.getActiveWorldId()
            if wid then
                construction.setJobStatus(wid, job.x, job.y, "pending")
            end
        end
    end
    s.constructJob = nil
    s.constructCandidate = nil
end

-- Stale-claim sweep over the scanned job list: a "claimed" job whose
-- claimant died is released immediately; one whose claim went
-- unrefreshed past the timeout (stuck worker, adopted orphan from a
-- save/reload) is released when the clock runs out. Any scanning
-- acolyte runs this, so release doesn't depend on the claimant.
local function sweepConstructClaims(wid, jobs, now, timeout)
    for _, job in ipairs(jobs) do
        if job.status == "claimed" then
            local key = constructKey(job.x, job.y)
            local c = constructClaims[key]
            if not c then
                -- Orphan (loaded save / script reload): adopt with an
                -- anonymous timer so it frees up if nobody owns it.
                constructClaims[key] = { uid = nil, at = now }
            elseif (c.uid and not unit.exists(c.uid))
                   or (now - c.at > timeout) then
                constructClaims[key] = nil
                construction.setJobStatus(wid, job.x, job.y, "pending")
            end
        end
    end
end

-- Can this unit source every material the piece needs right now?
-- (inventory + nearby ground + mule stock). Races lose gracefully at
-- fetch time; this is only the "worth claiming" filter.
local function constructMaterialsAvailable(uid, fromX, fromY, mats, params)
    for matType, need in pairs(mats or {}) do
        local have = inventoryCountOf(uid, matType)
        if have < need then
            local ground = groundCountOf(fromX, fromY, matType,
                                         params.construct_scan_range)
            if have + ground < need then
                local mule = findTechnomule(fromX, fromY)
                local muleHave = mule
                    and inventoryCountOf(mule.uid, matType) or 0
                if have + ground + muleHave < need then return false end
            end
        end
    end
    return true
end

-- Nearest viable pending job within construct_scan_range, or nil.
-- Also runs the stale-claim sweep (the scan already paid for the job
-- list). Buildings are always viable (staking needs no materials);
-- structure jobs need a costed pack, a floor under a post, and
-- sourceable materials.
local function findConstructJob(uid, fromX, fromY, params)
    local wid = world.getActiveWorldId()
    if not wid then return nil end
    local ccx = math.floor(fromX / 16)   -- chunkSize
    local ccy = math.floor(fromY / 16)
    local r = params.construct_scan_chunks
    local jobs = construction.getPendingJobs(ccx - r, ccy - r,
                                             ccx + r, ccy + r)
    if not jobs or #jobs == 0 then return nil end
    local now = engine.gameTime()
    sweepConstructClaims(wid, jobs, now, params.construct_claim_timeout)

    local best, bestD = nil, params.construct_scan_range
    for _, job in ipairs(jobs) do
        if job.status == "pending"
           and not constructClaimedByOther(constructKey(job.x, job.y),
                                           uid, now,
                                           params.construct_claim_timeout) then
            local viable, build = false, nil
            if job.category == "building" then
                viable = true
            else
                build = packBuildInfo(job.pack, job.kind)
                -- A durably-paid job (#799) needs no materials from THIS
                -- worker, so only an unpaid job is gated on sourceability.
                if build
                   and (job.kind ~= "post"
                        or structure.floorZAt(job.x, job.y))
                   and (job.paid or constructMaterialsAvailable(uid, fromX, fromY,
                           build.materials, params)) then
                    viable = true
                end
            end
            if viable then
                local d = distance(fromX, fromY, job.x + 0.5, job.y + 0.5)
                if d <= bestD then
                    job.dist = d
                    job.build = build
                    best, bestD = job, d
                end
            end
        end
    end
    return best
end

local function constructUtility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in (dire needs still preempt; the claim
    -- and phase machine survive the interruption). Dropped when the
    -- designation vanishes (player cancelled it, or a sweep handed the
    -- tile to someone else and THEY finished it).
    if s.constructJob then
        local job = construction.getDesignationAt(wid, s.constructJob.x,
                                                  s.constructJob.y)
        if job then return params.construct_lock_utility end
        releaseConstructJob(s, uid)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local cand = findConstructJob(uid, info.gridX, info.gridY, params)
    if not cand then return -math.huge end

    s.constructCandidate = cand
    local distFactor = math.max(0, 1 - cand.dist / params.construct_scan_range)
    return params.construct_base_utility * distFactor
         * roles.weight(s, "construct_job")
end

-- Stand position: centre of the neighbouring tile nearest the unit —
-- beside the job tile, never on it (the piece appears in the air cell
-- on top; a wall materialising around the builder's ankles is wrong).
local function constructStandPos(job, px, py)
    local bestX, bestY, bestD = nil, nil, math.huge
    for _, o in ipairs({ {1, 0}, {-1, 0}, {0, 1}, {0, -1} }) do
        local nx = job.x + o[1] + 0.5
        local ny = job.y + o[2] + 0.5
        local d = (nx - px) ^ 2 + (ny - py) ^ 2
        if d < bestD then bestX, bestY, bestD = nx, ny, d end
    end
    return bestX, bestY
end

-- The structure.hasAt slot this job will place into — mirrors
-- placeStructurePiece's own kind/edge → slot derivation (#805) so the
-- pre-payment occupancy revalidation below checks the EXACT slot the
-- worker is about to write, not just "is this tile occupied at all".
local function jobSlot(job)
    if job.kind == "floor" then return "floor"
    elseif job.kind == "ceiling" then return "ceiling"
    elseif job.kind == "wall" then return "wall_" .. (job.edge or "ne")
    elseif job.kind == "post" then return "post_" .. (job.edge or "n")
    elseif job.kind == "wire" then return "wire"
    end
    return nil
end

-- Place the finished piece via the structures module (same programmatic
-- builders locations.lua uses; they read the active world's terrain).
-- Posts designated without a floor are filtered at scan time, but the
-- floor can vanish mid-job — placement then fails and we log rather than
-- strand the job (the designation still completes); returns false so the
-- caller can apply the #799 no-silent-loss material policy.
local function placeStructurePiece(job)
    local structures = require("scripts.structures")
    if job.kind == "floor" then
        structures.floor(job.x, job.y)
    elseif job.kind == "ceiling" then
        structures.ceiling(job.x, job.y)
    elseif job.kind == "wall" then
        structures.wall(job.x, job.y, job.edge or "ne")
    elseif job.kind == "post" then
        -- Designations carry no corner (the tool's hover pick does);
        -- default to "n" until the tool grows a corner picker.
        if not structures.post(job.x, job.y, job.edge or "n") then
            engine.logWarn("construct: post at " .. job.x .. "," .. job.y
                .. " lost its floor mid-job — skipping placement")
            return false
        end
    elseif job.kind == "wire" then
        require("scripts.wire").place(job.x, job.y)
    end
    return true
end

-- Refund a structure designation's ALREADY-PAID materials to the ground
-- (#799 no-silent-loss policy): the FULL pack cost, not this call's own
-- (possibly empty, if resumed) job.need delta — 'paid' means the full
-- amount already left SOME unit's inventory. No-op for a building.
local function refundStructureMaterials(job)
    if job.category ~= "structure" then return end
    local build = job.build or packBuildInfo(job.pack, job.kind)
    if not build then return end
    for matType, need in pairs(build.materials or {}) do
        for _ = 1, need do
            item.spawnGround(matType, job.x + 0.5, job.y + 0.5)
        end
    end
end
M.refundStructureMaterials = refundStructureMaterials

local function constructExecute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim the scanned candidate: local registry (the synchronous
    -- guard) + engine status (the durable/observable layer).
    if not s.constructJob then
        local cand = s.constructCandidate
        if not cand then return end
        local key = constructKey(cand.x, cand.y)
        if constructClaimedByOther(key, uid, now,
                                   params.construct_claim_timeout) then
            s.constructCandidate = nil
            return
        end
        constructClaims[key] = { uid = uid, at = now }
        construction.setJobStatus(wid, cand.x, cand.y, "claimed")
        s.constructCandidate = nil
        cand.phase = "fetch"
        -- Fetch shortfalls, planned once at claim time (inventory →
        -- ground → mule). Reconciled against real inventory afterwards.
        if cand.category ~= "building" then
            cand.need = {}
            cand.fromGround, cand.fromMule = {}, {}
            -- A durably-paid job (#799) needs nothing fetched — leave
            -- cand.need empty so the walking phase's payment loop no-ops.
            if not cand.paid then
                local mule = findTechnomule(info.gridX, info.gridY)
                for matType, need in pairs(cand.build.materials or {}) do
                    cand.need[matType] = need
                    local have = inventoryCountOf(uid, matType)
                    local short = need - have
                    if short > 0 then
                        local ground = math.min(short,
                            groundCountOf(info.gridX, info.gridY, matType,
                                          params.construct_scan_range))
                        if ground > 0 then cand.fromGround[matType] = ground end
                        if short - ground > 0 and mule then
                            cand.fromMule[matType] = short - ground
                        end
                    end
                end
            end
            cand.work = cand.build.build_work or 1.0
        end
        s.constructJob = cand
        return
    end

    local job = s.constructJob
    local key = constructKey(job.x, job.y)
    -- A live claim by someone ELSE means ours expired while we were
    -- preempted and the tile was legally re-claimed — walk away.
    local c = constructClaims[key]
    if c and c.uid ~= uid and c.uid and unit.exists(c.uid) then
        s.constructJob = nil
        return
    end
    constructClaims[key] = { uid = uid, at = now }   -- keep the claim fresh

    -- Building blueprint: walk up and stake it, then hand off to the
    -- delivery + build_nearby machinery.
    if job.category == "building" then
        local d = distance(info.gridX, info.gridY, job.x + 0.5, job.y + 0.5)
        if d > 2.2 then
            unit.moveTo(uid, job.x + 0.5, job.y + 1.5, mv.comfort(uid))
            return
        end
        unit.stop(uid)
        local bid = building.spawn(job.building, job.x, job.y)
        if bid then
            construction.setJobStatus(wid, job.x, job.y, "complete")
            releaseConstructJob(s, uid)
        else
            -- Placement invalid (terrain changed, overlap) — retrying
            -- can't succeed, so cancel the blueprint and say so.
            reportFailure(uid, "Can't build here — blueprint cancelled")
            construction.cancelDesignation(job.x, job.y)
            releaseConstructJob(s, uid)
        end
        return
    end

    -- Structure piece, phase 1: source materials.
    if job.phase == "fetch" then
        if fetchWantsFromGround(uid, job.fromGround, params,
                                params.construct_scan_range) then
            return
        end
        if fetchWantsFromMule(uid, job.fromMule, info, params) then
            return
        end
        for matType, need in pairs(job.need) do
            if inventoryCountOf(uid, matType) < need then
                -- Sources came up short (raced / capacity) — release
                -- the tile for someone who can cover it.
                releaseConstructJob(s, uid, true)
                return
            end
        end
        job.phase = "walking"
    end

    -- Phase 2: stand beside the tile. Materials are consumed once, on
    -- arrival — the moment construction starts.
    if job.phase == "walking" then
        local sx, sy = constructStandPos(job, info.gridX, info.gridY)
        if distance(info.gridX, info.gridY, sx, sy)
           <= params.construct_arrival_tiles then
            unit.stop(uid)
            if not job.consumed then
                -- Revalidate the requested slot immediately before the
                -- irreversible material payment (#805): a race can fill
                -- it between claim and arrival (another worker's piece,
                -- or a re-designation). Cancel rather than pay and
                -- overwrite — no material lost, no stuck designation.
                local slot = jobSlot(job)
                if slot and structure.hasAt(job.x, job.y, slot) then
                    reportFailure(uid,
                        "Construction site is already built")
                    debug.recordOutcome{
                        kind = "construction.designate", outcome = "rejected",
                        where = { x = job.x, y = job.y },
                        reason = "requested structure slot filled before material payment",
                    }
                    construction.cancelDesignation(job.x, job.y)
                    releaseConstructJob(s, uid)
                    return
                end
                for matType, need in pairs(job.need) do
                    for _ = 1, need do
                        if not unit.removeItem(uid, matType) then
                            reportFailure(uid,
                                "Construction materials went missing")
                            releaseConstructJob(s, uid, true)
                            return
                        end
                    end
                end
                job.consumed = true
                -- Durably mark the designation paid (#799) — a no-op
                -- re-affirmation when already paid, but for a fresh
                -- payment this is what a replacement worker (or this
                -- unit after a save/load) sees as "already spent".
                construction.setMaterialsPaid(wid, job.x, job.y, true)
            end
            job.phase = "building"
            s.lastConstructAt = now
        else
            unit.moveTo(uid, sx, sy, mv.comfort(uid))
        end
        return
    end

    -- Phase 3: pour work in. progress rides the designation (persisted,
    -- drives the ghost's alpha ramp); the local copy just avoids a
    -- read-back race with the async command queue.
    if job.phase == "building" then
        local elapsed = now - (s.lastConstructAt or now)
        s.lastConstructAt = now
        if elapsed > 0 then
            -- Construction skill scales the pour rate the same way
            -- mining scales dig and woodcutting scales chop: level 50
            -- ≈ baseline, level 0 half rate (#265).
            local conSkill = unit.getSkill(uid, "construction") or 25.0
            local delta = params.construct_rate * (0.5 + conSkill / 100.0)
                        * elapsed / job.work
            job.progress = (job.progress or 0) + delta
            construction.addJobProgress(wid, job.x, job.y, delta)
        end
        if (job.progress or 0) >= 1.0 then
            if not placeStructurePiece(job) then
                -- #799: the paid material can't go down but was already
                -- spent — return it to the ground, not the void.
                refundStructureMaterials(job)
                reportFailure(uid,
                    "Construction site changed — materials returned to the ground")
            end
            construction.setJobStatus(wid, job.x, job.y, "complete")
            grantWorkXP(uid, "construction",
                        params.construct_xp_per_piece or 0)
            releaseConstructJob(s, uid)
        end
        return
    end
end

-- Preempted mid-job (thirst, combat, player order): re-enter through
-- the walking phase so the elapsed-time accumulator restarts — without
-- this, a 60 s drink would land as 60 s of instant build progress.
-- Consumed materials stay consumed (job.consumed guards the re-entry).
local function constructOnExit(uid, s, params)
    local job = s.constructJob
    if job and job.phase == "building" then
        job.phase = "walking"
    end
end


M.constructUtility = constructUtility
M.constructExecute = constructExecute
M.constructOnExit  = constructOnExit

return M
