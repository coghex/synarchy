-- Unit AI construction jobs (#538 split from unit_ai.lua).
--
-- Executes construction designations (#95). Two job categories:
--   * "building": walk to the blueprint and STAKE it — building.spawn
--     places the Appearing ghost, the designation completes, and the
--     existing deliver_to_build_site + build_nearby machinery takes
--     over (materials gate + worker-rate progress, unchanged).
--   * "structure": the full job — source the piece's materials
--     (inventory → ground → mule, same ladder as delivery), walk beside
--     the tile, pour work into the designation (construction.
--     addJobProgress; the ghost solidifies as progress accrues), and at
--     1.0 place the piece via scripts/structures.lua, job complete.
--
-- Claims: one worker per PAGE + tile via a module-local registry
-- (constructClaims, same shape and same #1329 page key as digClaims).
-- Since #1844 a job ALSO carries the exact designation ATTEMPT it
-- claimed, and every lifecycle call names it: a coordinate alone can no
-- longer tell one attempt from a successor designated at the same tile,
-- so a delayed status/progress/payment/completion from a job that has
-- gone is a no-op rather than a silent mutation of someone else's.
-- the engine-side "claimed" status is the durable/observable layer —
-- getPendingJobs carries it, and the sweep below releases a dead or
-- expired claimant back to "pending". Claims from a save/Lua reload
-- arrive with no registry entry (#1329 empties the registry on EVERY
-- load, so a restored "claimed" job is always an orphan here); adopted
-- with an anonymous timer, released the same way if nobody refreshes.
--
-- Material races (construct jobs vs. deliveries) are NOT reserved
-- cross-unit: the fetch fails gracefully for the loser, the post-fetch
-- inventory check releases the job to pending, and the next scan
-- re-plans — same self-heal as the mule-stock race.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local distance        = core.distance
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
local claimsLib = require("scripts.unit_ai_claims")
local site = require("scripts.unit_ai_construct_site")

local M = {}

-- Re-exported so callers keep ONE entry point for the construction
-- module: scripts/build_tool.lua's cancel path refunds through this,
-- and detaches the claimant through the wrapper below. The claim
-- registry itself stays module-private, so the public verb takes only
-- what a caller can know.
M.refundStructureMaterials = site.refundStructureMaterials

local constructClaims = claimsLib.track({})  -- key → { uid, at, attempt }
local constructKey    = claimsLib.key        -- (wid, x, y)

local function constructClaimedByOther(key, uid, now, timeout)
    local c = constructClaims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or (c.uid and not unit.exists(c.uid)) then
        constructClaims[key] = nil
        return false
    end
    return true
end

-- Release the unit's hold on its job. toPending flips engine-side status
-- back so another worker can take the tile; omitted = already gone.
local function releaseConstructJob(wid, s, uid, toPending)
    local job = s.constructJob
    if job then
        local key = constructKey(wid, job.x, job.y)
        local c = constructClaims[key]
        if c and c.uid == uid then constructClaims[key] = nil end
        if toPending then
            -- #1844: every lifecycle call names the exact attempt this
            -- unit observed, so releasing a job that has already been
            -- cancelled and replaced cannot reset the SUCCESSOR's status.
            construction.setJobStatus(wid, job.x, job.y, "pending",
                                      job.attempt)
        end
    end
    s.constructJob = nil
    s.constructCandidate = nil
end


-- Work XP owed to a claimant whose placement is queued but not yet
-- confirmed (#1844), keyed the way claims are. One entry per tile: a job
-- reaches it once, and both engine answers consume it.
local pendingXp = {}

-- Detach whoever was working one exact attempt (scripts/build_tool.lua's
-- player cancel, and the engine broadcast below). The registry is
-- private to this module, so the public form takes only the tile and the
-- attempt.
function M.abandonClaim(wid, gx, gy, attempt)
    site.abandonClaim(constructClaims, constructKey, wid, gx, gy, attempt)
end

-- The engine ACCEPTED that attempt's placement: the piece really landed,
-- so the work is paid for now rather than optimistically at queue time.
local function settlePendingXp(wid, gx, gy, attempt, grant)
    local key = constructKey(wid, gx, gy)
    local owed = pendingXp[key]
    if not (owed and owed.attempt == attempt) then return end
    pendingXp[key] = nil
    if grant and owed.amount > 0 and unit.exists(owed.uid) then
        grantWorkXP(owed.uid, "construction", owed.amount)
    end
end

-- The world-side half of that, as an engine BROADCAST callback (#1844).
-- Attached to the shared singleton rather than declared in
-- unit_ai.lua, which is at its #538 line budget: broadcastToModules
-- dispatches over loaded SCRIPTS, and the singleton's table is the one
-- scripts/unit_ai.lua exposes. Same arrangement unit_ai_core.lua uses
-- for getState/getRole.
local unitAi = package.loaded["scripts.unit_ai"]
if unitAi then
    function unitAi.onConstructInvalidated(pageId, gx, gy, attempt)
        -- A withdrawn attempt is never paid for: `false` discards the
        -- claim rather than granting it.
        settlePendingXp(pageId, gx, gy, attempt, false)
        M.abandonClaim(pageId, gx, gy, attempt)
    end

    -- The other half: the world thread accepted this attempt's queued
    -- placement, so the piece really landed and the work is paid for.
    --
    -- The claim is released here too, and for the same reason the
    -- invalidation releases one: a scanner passing during the hand-off
    -- sees a `placing` job and the stale-claim sweep ADOPTS it with an
    -- anonymous entry. The claimant's own release cannot clear that one
    -- -- it is not its uid -- so without this it would sit on the tile
    -- until its timeout, refusing a successor designated there.
    function unitAi.onConstructCompleted(pageId, gx, gy, attempt)
        settlePendingXp(pageId, gx, gy, attempt, true)
        M.abandonClaim(pageId, gx, gy, attempt)
    end
end

-- Nearest viable pending job within construct_scan_range, or nil. Also
-- runs the stale-claim sweep (the scan already paid for the job list).
-- Buildings are always viable (staking needs no materials); structure
-- jobs need a costed pack, a floor under a post, and sourceable materials.
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
    site.sweepClaims(constructClaims, constructKey, wid, jobs, now,
                     params.construct_claim_timeout)

    local best, bestD = nil, params.construct_scan_range
    for _, job in ipairs(jobs) do
        if job.status == "pending"
           and not constructClaimedByOther(constructKey(wid, job.x, job.y),
                                           uid, now,
                                           params.construct_claim_timeout) then
            local viable, build = false, nil
            if job.category == "building" then
                viable = true
            else
                build = site.packBuildInfo(job.pack, job.kind)
                -- #1844: the resolver decides buildability, not a
                -- hand-rolled pack/floor pair. Material SCARCITY is a
                -- scheduling concern and deliberately still ours.
                -- A durably-paid job (#799) needs no sourceability check.
                if build
                   and site.planOutcome(wid, job) == "valid"
                   and (job.paid or site.materialsAvailable(uid, fromX, fromY,
                           build.materials, params)) then
                    viable = true
                end
            end
            if viable then
                local d = distance(fromX, fromY, job.lx + 0.5, job.ly + 0.5)
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
        -- #1844: the tile carrying A designation is no longer proof it
        -- carries OURS. A cancelled job replaced by a successor at the
        -- same tile must drop here, or this unit would keep pouring work
        -- into someone else's attempt.
        if job and job.attempt == s.constructJob.attempt then
            return params.construct_lock_utility
        end
        releaseConstructJob(wid, s, uid)
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
        local key = constructKey(wid, cand.x, cand.y)
        if constructClaimedByOther(key, uid, now,
                                   params.construct_claim_timeout) then
            s.constructCandidate = nil
            return
        end
        constructClaims[key] = { uid = uid, at = now,
                                 attempt = cand.attempt }
        construction.setJobStatus(wid, cand.x, cand.y, "claimed",
                                  cand.attempt)
        s.constructCandidate = nil
        cand.phase = "fetch"
        -- Fetch shortfalls, planned once at claim time (inventory →
        -- ground → mule). Reconciled against real inventory afterwards.
        if cand.category ~= "building" then
            cand.need = {}
            cand.fromGround, cand.fromMule = {}, {}
            -- A durably-paid job (#799) needs nothing fetched — cand.need
            -- stays empty so the walking phase's payment loop no-ops.
            if not cand.paid then
                local mule = findTechnomule(uid, info.gridX, info.gridY)
                for matType, need in pairs(cand.build.materials or {}) do
                    cand.need[matType] = need
                    local have = inventoryCountOf(uid, matType)
                    local short = need - have
                    if short > 0 then
                        local ground = math.min(short,
                            groundCountOf(uid, info.gridX, info.gridY, matType,
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
    local key = constructKey(wid, job.x, job.y)
    -- A live claim by someone ELSE means ours expired while we were
    -- preempted and the tile was legally re-claimed — walk away.
    local c = constructClaims[key]
    if c and c.uid ~= uid and c.uid and unit.exists(c.uid) then
        s.constructJob = nil
        return
    end
    -- Keep the claim fresh, carrying the attempt it is a claim ON.
    constructClaims[key] = { uid = uid, at = now, attempt = job.attempt }

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
            construction.setJobStatus(wid, job.x, job.y, "complete",
                                      job.attempt)
            releaseConstructJob(wid, s, uid)
        else
            -- Placement invalid (terrain changed, overlap) — retrying
            -- can't succeed, so cancel the blueprint and say so.
            reportFailure(uid, "Can't build here — blueprint cancelled")
            construction.cancelDesignation(job.x, job.y, job.attempt)
            releaseConstructJob(wid, s, uid)
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
                releaseConstructJob(wid, s, uid, true)
                return
            end
        end
        job.phase = "walking"
    end

    -- Phase 2: stand beside the tile. Materials are consumed once, on
    -- arrival — the moment construction starts.
    if job.phase == "walking" then
        local sx, sy = site.constructStandPos(job, info.gridX, info.gridY)
        if distance(info.gridX, info.gridY, sx, sy)
           <= params.construct_arrival_tiles then
            unit.stop(uid)
            if not job.consumed then
                -- Re-resolve the site immediately before the
                -- irreversible material payment (#805, #1844): another
                -- worker's piece can fill the slot between claim and
                -- arrival, terrain can move under it, and the pack's art
                -- or build metadata can go away. Nothing has left the
                -- inventory yet, so a refusal here costs nothing.
                local plan = site.planOutcome(wid, job)
                if plan == "unresolved-terrain" then
                    -- Not a refusal: the site's chunk is simply not
                    -- resident, so nobody can judge it. Release to
                    -- pending and let a later scan pick it up.
                    releaseConstructJob(wid, s, uid, true)
                    return
                elseif plan and plan ~= "valid" then
                    reportFailure(uid, "Construction site is no longer buildable")
                    debug.recordOutcome{
                        kind = "construction.designate", outcome = "rejected",
                        where = { x = job.x, y = job.y },
                        reason = "resolver refused the site before material "
                                 .. "payment: " .. tostring(plan),
                    }
                    construction.cancelDesignation(job.x, job.y, job.attempt)
                    releaseConstructJob(wid, s, uid)
                    return
                end
                -- The designation as it is RIGHT NOW, not as it was at
                -- scan time. This is also the exact-attempt check: a
                -- successor at the same tile is a different job and this
                -- unit has no claim on it.
                local live = construction.getDesignationAt(wid, job.x, job.y)
                if not live or live.attempt ~= job.attempt then
                    releaseConstructJob(wid, s, uid)
                    return
                end
                if live.paid then
                    -- A predecessor already paid for THIS attempt (it
                    -- died, or this is a resumed job after a save/load).
                    -- Receipt presence is the paid state, so there is
                    -- nothing to charge and nothing to record.
                    job.consumed = true
                elseif construction.payMaterials(wid, job.x, job.y,
                                                 job.attempt, uid) then
                    -- ONE lossless step (#1844): the engine removed the
                    -- exact material instances AND wrote the durable
                    -- receipt, or it removed nothing at all. The old
                    -- shape (remove here, then setMaterialsPaid) left a
                    -- window in which a cancellation refunded nothing
                    -- for a cost the inventory had already lost, and
                    -- that window is wider now that a WORLD-thread
                    -- invalidator can cancel too. The cost comes from
                    -- the registered catalogue rather than from
                    -- job.need, so a receipt can only ever record what
                    -- the engine really took.
                    job.consumed = true
                else
                    -- This unit cannot cover the cost, or the attempt
                    -- went away between the read above and the charge.
                    -- Either way: not this unit, not now. Release the
                    -- tile to pending so someone who can, does.
                    releaseConstructJob(wid, s, uid, true)
                    return
                end
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
        -- Requirement 10: an unresolved-terrain site cannot be
        -- PROGRESSED either — its chunk evicting mid-build must stop the
        -- pour rather than run it up to 1.0 and be refused at placement.
        --
        -- SKIPPING the tick, not releasing the job: "the chunk is not
        -- resident" is not a refusal anywhere else in this arc, and
        -- handing the tile back to the scan pool every tick while a
        -- worker stands on it would churn the claim over a condition
        -- that resolves itself. The claim stays fresh above, so nothing
        -- expires; the pour simply resumes when the terrain does.
        if site.planOutcome(wid, job) == "unresolved-terrain" then
            s.lastConstructAt = now
            return
        end
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
            construction.addJobProgress(wid, job.x, job.y, delta,
                                       job.attempt)
        end
        if (job.progress or 0) >= 1.0 then
            -- The hand-off is the job TILE's business (requirement 10's
            -- last re-check, requirement 18's claim, the commit window,
            -- the piece, the refund) and lives in the site module; this
            -- one is at its #538 budget. Either way the job is over.
            --
            -- XP is NOT granted here: structure.place returns once the
            -- piece is staged and QUEUED, so a true answer does not yet
            -- mean the world thread accepted it. Only the engine's
            -- onConstructCompleted broadcast says that; a declined
            -- placement sends onConstructInvalidated instead, and the
            -- claim recorded below is what either one settles.
            local outcome = site.finishPlacement(wid, job, uid)
            if outcome == "deferred" then
                -- The site's chunk is simply not resident, which is
                -- never a refusal here or anywhere else in this arc: the
                -- designation stands, so hand the tile back to the scan
                -- pool rather than leaving it claimed by nobody.
                releaseConstructJob(wid, s, uid, true)
                return
            end
            if outcome == "placed" then
                pendingXp[constructKey(wid, job.x, job.y)] =
                    { attempt = job.attempt, uid = uid,
                      amount = params.construct_xp_per_piece or 0 }
            end
            releaseConstructJob(wid, s, uid)
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
