-- Building Spawn Sequencer
--
-- Watches each placed building and, once it transitions to the "built"
-- state, spawns a configured starting roster of units one at a time.
-- The next unit doesn't spawn until the previous one has cleared the
-- spawn tile (or been destroyed). Keeps units from piling up on top
-- of each other when a portal opens with a starting party.
--
-- Per-def config:
--   unit_type      = string, name of the unit def to spawn.
--   count          = integer, total units to spawn.
--   spawn_offset   = {x, y}, where the unit appears relative to the
--                    building's anchor tile (in tile units).
--   walk_to_offset = {x, y}, where the unit is then commanded to walk
--                    via unit_ai.commandMove — beats the AI's wander
--                    so they clear the spawn tile predictably.
--   clear_radius   = how far the previous unit must move from its
--                    spawn point before the next one is allowed.
--
-- Per-building runtime state lives on the module table so reloads /
-- package.loaded singleton-ness preserves it through script changes:
--   buildingState[bid] = {
--     lastUid     = uid | nil,
--     lastSpawnX  = x,
--     lastSpawnY  = y,
--     lastSpawnedAt = posix,
--   }
--
-- The countdown ("how many units left to spawn from this building")
-- lives on the engine side as biSpawnRemaining so it survives chunk
-- eviction + save/load without a Lua serializer. Lua only owns the
-- per-spawn sequencing state, which is transient (re-derivable from
-- "is a fresh-spawned unit still on the spawn tile?").

local buildingSpawn = package.loaded["scripts.building_spawn"] or {}
package.loaded["scripts.building_spawn"] = buildingSpawn

local config = {
    acolyte_portal = {
        unit_type      = "acolyte",
        count          = 5,
        spawn_offset   = { x = 0.5, y = 0.5 },
        -- Items handed out to each spawned unit immediately after
        -- spawn, via unit.addItem. Empty now — the acolyte def's
        -- starting_inventory carries the canteen + rations baseline so
        -- the unit-info v2 inventory panel can see them on debug-
        -- spawned units too. Add portal-specific extras here if any
        -- ever appear (e.g. a sigil only the portal grants).
        starting_items = {},
        -- Walk two tiles south so the unit clears the spawn tile
        -- before its commandedTask resolves (the AI's "arrived"
        -- threshold is 0.6 tiles, so a 1-tile walk barely makes it
        -- off the portal). 2 tiles gives breathing room.
        walk_to_offset = { x = 0.5, y = 2.5 },
        -- Minimum seconds between spawns. Acts on top of the
        -- previous-unit-cleared check: even if the previous acolyte
        -- has walked off the spawn tile, we hold the next spawn for
        -- this long so the procession paces out evenly.
        spawn_interval = 2.0,
        -- Fallback timeout: if a spawned unit gets terrain-stuck and
        -- never clears its tile, don't gridlock the spawn forever.
        -- After this many seconds, spawn the next unit anyway.
        stuck_timeout  = 8.0,
    },
}

buildingSpawn.state = buildingSpawn.state or {}
local state = buildingSpawn.state

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- True if the previous spawned unit has cleared the actual spawn
-- tile, OR has been stuck on it long enough that we give up waiting.
-- Cleared = destroyed OR the unit's tile-floor differs from the
-- spawn tile-floor in at least one axis. The timeout handles the
-- edge case of a terrain-stuck unit; in normal terrain the unit
-- walks away quickly via its commandedTask.
local function previousUnitCleared(s, params)
    if not s.lastUid then return true end
    local info = unit.getInfo(s.lastUid)
    if not info then return true end   -- unit destroyed
    if math.floor(info.gridX) ~= math.floor(s.lastSpawnX)
       or math.floor(info.gridY) ~= math.floor(s.lastSpawnY) then
        return true
    end
    -- Stuck-timeout fallback. Without this a single bad-terrain spawn
    -- locks the portal forever.
    if s.lastSpawnedAt and (engine.gameTime() - s.lastSpawnedAt) > params.stuck_timeout then
        return true
    end
    return false
end

-- Ensure a state entry exists for this building, and seed the
-- engine-side biSpawnRemaining countdown the first time we see this
-- bid. Engine sets biSpawnRemaining = -1 (sentinel) at spawn; Lua
-- seeds it to params.count on first sight. Loaded-from-save buildings
-- already have a real count (which may be 0 if the spawn finished
-- before save) — we leave those alone.
local function ensureState(bid, params)
    local s = state[bid]
    if not s then
        local engineRem = building.getSpawnRemaining(bid)
        if engineRem == nil or engineRem < 0 then
            -- Fresh placement, never seeded. Apply config.
            building.setSpawnRemaining(bid, params.count)
        end
        -- engineRem == 0 means "previously depleted by spawns" — load
        -- branch, don't re-seed. engineRem > 0 means "in progress" —
        -- also load branch.
        s = {
            lastUid    = nil,
            lastSpawnX = 0,
            lastSpawnY = 0,
        }
        state[bid] = s
    end
    return s
end

-----------------------------------------------------------
-- Per-building tick
-----------------------------------------------------------

local function tickOne(bid, info)
    local params = config[info.defName]
    if not params then return end

    -- Wait until the portal is done appearing — spawning during the
    -- "appearing" anim would have the unit pop out of an animating
    -- texture, which looks broken.
    if building.getActivity(bid) ~= "built" then return end

    local s = ensureState(bid, params)
    local remaining = building.getSpawnRemaining(bid) or 0
    if remaining <= 0 then return end
    if not previousUnitCleared(s, params) then return end
    -- Inter-spawn cooldown. Even if the previous acolyte cleared the
    -- spawn tile quickly, hold the next one until spawn_interval has
    -- elapsed so the line emerges at a steady cadence.
    if s.lastSpawnedAt
       and (engine.gameTime() - s.lastSpawnedAt) < (params.spawn_interval or 0) then
        return
    end

    -- Spawn at the portal tile's center plus offset. The anchor is
    -- the building's bottom-left tile (integer coords); offset is
    -- in tile-fractional units.
    local spawnX = info.gridX + params.spawn_offset.x
    local spawnY = info.gridY + params.spawn_offset.y
    local walkX  = info.gridX + params.walk_to_offset.x
    local walkY  = info.gridY + params.walk_to_offset.y

    local newUid = unit.spawn(params.unit_type, spawnX, spawnY)
    if not newUid then
        engine.logWarn("BuildingSpawn: unit.spawn failed at "
            .. spawnX .. "," .. spawnY)
        return
    end

    -- Hand out per-building starting items. Unit YAML carries no
    -- inventory now, so this is what differentiates a portal-spawned
    -- unit from a debug-spawned one.
    if params.starting_items then
        for _, entry in ipairs(params.starting_items) do
            unit.addItem(newUid, entry.def, entry.fill or 0)
        end
    end

    -- Route through the AI so the walk-out is a commanded task (beats
    -- the autonomous wander utility; survives if the AI tick fires
    -- before the unit reaches its destination).
    local unitAi = require("scripts.unit_ai")
    unitAi.commandMove(newUid, walkX, walkY, 2.0)

    local newRemaining = building.consumeSpawn(bid) or 0
    s.lastUid       = newUid
    s.lastSpawnX    = spawnX
    s.lastSpawnY    = spawnY
    s.lastSpawnedAt = engine.gameTime()

    engine.logInfo(string.format(
        "BuildingSpawn: portal=%d spawned %s id=%d at (%.2f, %.2f) -> walk to (%.2f, %.2f), remaining=%d",
        bid, params.unit_type, newUid, spawnX, spawnY, walkX, walkY, newRemaining))
end

-----------------------------------------------------------
-- Iteration: scan all buildings. building.list() returns a string,
-- so we parse it. A real Lua-array API for buildings would be cleaner
-- — add `building.getAllIds()` when there's a second consumer.
-----------------------------------------------------------

local function getAllBuildingIds()
    -- Quick & dirty parse of building.list()'s "id=N defName ..." lines.
    -- Replace with a proper API call when one exists.
    local s = building.list()
    if not s or s == "No buildings placed" then return {} end
    local ids = {}
    for id in s:gmatch("id=(%d+)") do
        table.insert(ids, tonumber(id))
    end
    return ids
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------

function buildingSpawn.init(scriptId)
    engine.logInfo("Building spawn sequencer initializing...")
    -- Save hook for per-building transient sequencer state
    -- (lastUid/lastSpawnX/Y/lastSpawnedAt). The roster countdown
    -- itself lives on the BuildingInstance and persists via the
    -- building snapshot — this hook only covers the spawn-rate
    -- gating data.
    local saveLib  = require("scripts.lib.serialize")
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("building_spawn",
        function() return saveLib.serialize(state) end,
        function(blob)
            local restored = saveLib.deserialize(blob) or {}
            for k in pairs(state) do state[k] = nil end
            for k, v in pairs(restored) do state[k] = v end
        end)
end

-- Construction progress curve. Solo workers build at 1× the base
-- rate (R(1)=1 worker-second per real second); coordination bonus
-- ramps as n² up to 3 workers, then tapers logarithmically so the
-- 5th+ helper still adds something but with diminishing returns.
--   R(1) = 1,  R(2) = 4,  R(3) = 9
--   R(4) ≈ 12, R(5) ≈ 13.75, R(6) = 15, R(8) ≈ 16.75, R(10) = 18
local function workerRate(n)
    if n <= 0 then return 0 end
    if n <= 3 then return n * n end
    return 9 + 3 * (math.log(n - 2) / math.log(2))
end

-- Tick the construction progress for one Appearing building. Only
-- fires if the def has bdBuildWork > 0 (legacy time-based defs like
-- the portal are untouched). Counts acolytes physically adjacent
-- with currentAction == "build_nearby", scales by R(n) * dt, and
-- bumps biBuildProgress via the engine API. The engine's
-- currentActivity check flips the building to Built once
-- biBuildProgress ≥ bdBuildWork — no explicit "complete" needed.
local function constructionTickOne(bid, dt)
    if building.getActivity(bid) ~= "appearing" then return end
    local required = building.getBuildRequired(bid)
    if not required or required <= 0 then return end

    -- Materials gate: don't advance progress until every required
    -- material has been fully delivered. While unsatisfied the
    -- building renders as a ghost; once satisfied it flips to the
    -- first construction frame and progresses normally.
    if not building.areMaterialsSatisfied(bid) then return end

    local unitAi = require("scripts.unit_ai")
    local workers = unitAi.countAdjacentBuilders(bid)
    if workers <= 0 then return end

    local delta = workerRate(workers) * dt
    if delta > 0 then
        building.addBuildProgress(bid, delta)
    end
end

function buildingSpawn.update(dt)
    if require("scripts.pause").isPaused() then return end
    local ids = getAllBuildingIds()
    if #ids == 0 then return end
    for _, bid in ipairs(ids) do
        local info = building.getInfo(bid)
        if info and info.defName then
            tickOne(bid, info)
            constructionTickOne(bid, dt)
        end
    end
end

function buildingSpawn.shutdown()
    for k in pairs(state) do state[k] = nil end
    engine.logInfo("Building spawn sequencer shut down")
end

return buildingSpawn
