-- Unit AI dig-designation jobs (#538 split from unit_ai.lua).
--
-- Action: dig_designation
--
-- "If it has nothing better to do": claim the nearest designated
-- tile, walk to its nearest corner, pull out the best tool for the
-- material, and dig. Corner progress drains digger-side first
-- world-side (world.digTile), so the tile slopes toward the unit
-- until it drops a z-level and the designation completes.
--
-- Claims live in a module-local table keyed by tile so two acolytes
-- never work the same tile (same in-flight-claim shape as build
-- sites); claims expire on timeout or when the claimant dies.
-----------------------------------------------------------

local core = require("scripts.unit_ai_core")
local distance = core.distance

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")

local M = {}

local digClaims = {}   -- "x,y" → { uid = ..., at = gameTime }

local function digKey(x, y) return x .. "," .. y end

local function digClaimedByOther(key, uid, now, timeout)
    local c = digClaims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        digClaims[key] = nil
        return false
    end
    return true
end

-- Best carried tool for a material: returns toolName, speed (0 when
-- the unit has no digging tool — no tool, no dig).
local function bestDigTool(uid, params, pickSpeed, shovelSpeed)
    local inv = unit.getInventory(uid)
    if not inv then return nil, 0 end
    local hasShovel, hasPick = false, false
    for _, it in ipairs(inv) do
        if params.dig_tools.shovel.defs[it.defName] then hasShovel = true end
        if params.dig_tools.pick.defs[it.defName]   then hasPick   = true end
    end
    local tool, speed = nil, 0
    if hasShovel and (shovelSpeed or 0) > speed then
        tool, speed = "shovel", shovelSpeed
    end
    if hasPick and (pickSpeed or 0) > speed then
        tool, speed = "pick", pickSpeed
    end
    return tool, speed
end

local function releaseDigJob(s, uid)
    if s.digJob then
        local key = digKey(s.digJob.x, s.digJob.y)
        local c = digClaims[key]
        if c and c.uid == uid then digClaims[key] = nil end
    end
    s.digJob = nil
    s.digPhase = nil
end

-- The designation vanished while we held the job — the tile
-- completed (or was undesignated, rare). BOTH the utility check and
-- the execute loop can be first to notice, depending on tick
-- ordering, so completion lives in one helper: XP if we were
-- actually working it, drop the tool visual, release the claim.
local function digComplete(uid, s, params)
    if s.digPhase == "digging" or s.digPhase == "equipping" then
        unit.addXP(uid, "mining", params.dig_xp_per_tile or 0)
    end
    unit.clearAnimOverride(uid)
    releaseDigJob(s, uid)
end

local function digUtility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in so dire needs (thirst, combat)
    -- still preempt. Released the moment the tile completes — this
    -- check runs BEFORE execute each tick, so completion is usually
    -- detected here (digComplete grants the XP + clears visuals).
    if s.digJob then
        local z = world.getMineDesignationAt(wid, s.digJob.x, s.digJob.y)
        if z then return params.dig_lock_utility end
        digComplete(uid, s, params)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local gx, gy, dist =
        world.nearestMineDesignation(wid, info.gridX, info.gridY)
    if not gx then return -math.huge end
    if dist > params.dig_scan_range then return -math.huge end

    local now = engine.gameTime()
    if digClaimedByOther(digKey(gx, gy), uid, now,
                         params.dig_claim_timeout) then
        return -math.huge
    end

    local _, pickSpeed, shovelSpeed, spoilBlocked =
        world.getDigInfoAt(wid, gx, gy)
    if not pickSpeed then return -math.huge end   -- chunk not loaded
    -- No room for the spoil around this tile (boxed in by water,
    -- cliffs, or other designations): the engine would refuse every
    -- dig tick, so don't take the job at all.
    if spoilBlocked then return -math.huge end
    local tool, speed = bestDigTool(uid, params, pickSpeed, shovelSpeed)
    if not tool or speed <= 0 then return -math.huge end

    -- Stash the scored candidate so execute doesn't re-scan.
    s.digCandidate = { x = gx, y = gy, tool = tool }

    local distFactor = math.max(0, 1 - dist / params.dig_scan_range)
    return params.dig_base_utility * math.min(speed, 1.0) * distFactor
         * roles.weight(s, "dig_designation")
end

-- Corner geometry (grid space, matches mdCorners order NW,NE,SE,SW):
-- corner i of tile (x, y) sits at (x, y) + DIG_CORNERS[i].
local DIG_CORNERS = { {0, 0}, {1, 0}, {1, 1}, {0, 1} }

local function digCornerPos(job, i)
    return job.x + DIG_CORNERS[i][1], job.y + DIG_CORNERS[i][2]
end

-- Stand position for corner i: a quarter tile OUTSIDE the corner,
-- diagonal from the tile center — beside the excavation, never on it.
-- The special corner "top" stands on the tile itself (used when no
-- corner is reachable at the dig level).
local function digStandPos(job, i)
    if i == "top" then
        return job.x + 0.5, job.y + 0.5
    end
    local cx, cy = digCornerPos(job, i)
    local ox, oy = cx - (job.x + 0.5), cy - (job.y + 0.5)
    return cx + ox * 0.5, cy + oy * 0.5
end

-- A corner is reachable when the tile the digger would stand on
-- (just outside the corner) is dry ground at the SAME z as the dig
-- tile. No digging across z levels: a corner whose stand tile is a
-- cliff above (or a hole / water below) is off limits.
local function digCornerReachable(job, i)
    local sx, sy = digStandPos(job, i)
    local tx, ty = math.floor(sx), math.floor(sy)
    if world.getFluidAt(tx, ty) then return false end
    return world.getSurfaceAt(tx, ty) == job.z
end

-- Nearest reachable corner with material left (corners is
-- {c1,c2,c3,c4}). Falls back to "top" — stand on the designated tile
-- itself — when material remains but no corner can be reached at the
-- dig level. nil = nothing left to dig.
local function digNextCorner(job, corners, px, py)
    local best, bi = math.huge, nil
    local anyMaterial = false
    for i = 1, 4 do
        if corners[i] > 0 then
            anyMaterial = true
            if digCornerReachable(job, i) then
                local cx, cy = digCornerPos(job, i)
                local d = (cx - px) ^ 2 + (cy - py) ^ 2
                if d < best then best, bi = d, i end
            end
        end
    end
    if bi then return bi end
    if anyMaterial then return "top" end
    return nil
end

local function digExecute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim a fresh job and head for its nearest undug corner.
    if not s.digJob then
        local cand = s.digCandidate
        if not cand then return end
        local key = digKey(cand.x, cand.y)
        if digClaimedByOther(key, uid, now, params.dig_claim_timeout) then
            return
        end
        local z, c1, c2, c3, c4 =
            world.getMineDesignationAt(wid, cand.x, cand.y)
        if not z then return end
        cand.z = z
        local ci = digNextCorner(cand, { c1, c2, c3, c4 },
                                 info.gridX, info.gridY)
        if not ci then return end
        digClaims[key] = { uid = uid, at = now }
        s.digCandidate = nil
        s.digJob = cand
        s.digEquipped = false
        s.digPhase = "walking"
        cand.corner = ci
        local sx, sy = digStandPos(cand, ci)
        unit.moveTo(uid, sx, sy, mv.comfort(uid))  -- approaching dig site → comfort
        return
    end

    local job = s.digJob
    -- Keep the claim fresh while we hold the job.
    digClaims[digKey(job.x, job.y)] = { uid = uid, at = now }
    local toolCfg = params.dig_tools[job.tool]

    if s.digPhase == "walking" then
        local sx, sy = digStandPos(job, job.corner)
        local d = distance(info.gridX, info.gridY, sx, sy)
        if d <= params.dig_arrival_tiles then
            unit.stop(uid)
            if not s.digEquipped then
                -- First corner of this job: pull the tool out.
                -- setAnimOverride wins over the engine's state-driven
                -- anim resolution (plain setAnim gets clobbered every
                -- sim tick by publishToRender).
                unit.setAnimOverride(uid, toolCfg.equip_anim)
                s.digPhase = "equipping"
                s.digEquipUntil = now + params.dig_equip_seconds
            else
                unit.setAnimOverride(uid, toolCfg.work_anim)
                s.digPhase = "digging"
                s.lastDigAt = now
            end
        else
            -- Execute only fires when idle, so this re-issue means
            -- the previous walk arrived short or failed.
            unit.moveTo(uid, sx, sy, mv.comfort(uid))  -- approaching dig site → comfort
        end
        return
    end

    if s.digPhase == "equipping" then
        if now >= (s.digEquipUntil or 0) then
            s.digEquipped = true
            unit.setAnimOverride(uid, toolCfg.work_anim)
            s.digPhase = "digging"
            s.lastDigAt = now
        end
        return
    end

    if s.digPhase == "digging" then
        local z, c1, c2, c3, c4 =
            world.getMineDesignationAt(wid, job.x, job.y)
        if not z then
            -- Tile completed (or undesignated out from under us).
            digComplete(uid, s, params)
            return
        end
        local corners = { c1, c2, c3, c4 }
        -- Current corner exhausted → walk around the tile to the
        -- next reachable one (DF-style: the unit digs each corner
        -- from beside it, and the slope follows it around). From
        -- "top" we stay put — everything is in reach from up there.
        if job.corner ~= "top" and corners[job.corner] <= 0 then
            local ci = digNextCorner(job, corners,
                                     info.gridX, info.gridY)
            if ci and ci ~= job.corner then
                job.corner = ci
                unit.clearAnimOverride(uid)
                s.digPhase = "walking"
                local sx, sy = digStandPos(job, ci)
                unit.moveTo(uid, sx, sy, mv.comfort(uid))  -- approaching dig site → comfort
                return
            end
            -- No other corner left: the residue finishes from here.
        end
        -- Material can change as digging exposes new strata; re-read
        -- the speed each swing. spoilBlocked can flip mid-dig too
        -- (other diggers filled the surrounding piles) — abandon the
        -- job rather than swing at a refusing engine forever.
        local _, pickSpeed, shovelSpeed, spoilBlocked =
            world.getDigInfoAt(wid, job.x, job.y)
        local speed = (job.tool == "pick") and (pickSpeed or 0)
                                            or (shovelSpeed or 0)
        if speed <= 0 or spoilBlocked then
            unit.clearAnimOverride(uid)
            releaseDigJob(s, uid)
            return
        end
        -- Idempotent: re-asserts the work anim after preemption.
        unit.setAnimOverride(uid, toolCfg.work_anim)
        local dt = math.min(now - (s.lastDigAt or now), 2.0)
        s.lastDigAt = now
        -- Personal factor: muscle moves material, technique wastes
        -- less of the swing. strength ~1.0 baseline; mining skill 50
        -- = 1.0×, 0 = 0.5×, 100 = 1.5×.
        local strength = unit.getStat(uid, "strength") or 1.0
        local mining   = unit.getSkill(uid, "mining") or 0.0
        local unitFactor = strength * (0.5 + mining / 100.0)
        -- Mining skill rides along: the engine's chunk-yield
        -- accumulator scales by the CURRENT digger's skill, so a
        -- handoff mid-dig switches to the new digger's rate.
        -- Perception scales the gem-find roll when the tile
        -- completes — sharp-eyed miners spot more glints.
        local percep = unit.getStat(uid, "perception") or 1.0
        world.digTile(wid, job.x, job.y, info.gridX, info.gridY,
                      params.dig_rate * speed * unitFactor * dt,
                      mining, percep)
        return
    end
end

-- Called by the dispatch loop when another action preempts an active
-- dig (thirst, combat, player order). Drops the tool VISUAL only —
-- the claim and job survive so the dig resumes afterwards, re-entered
-- through the walking phase.
local function digOnExit(uid, s, params)
    unit.clearAnimOverride(uid)
    if s.digPhase == "digging" or s.digPhase == "equipping" then
        s.digPhase = "walking"
    end
end


M.digUtility = digUtility
M.digExecute = digExecute
M.digOnExit  = digOnExit

return M
