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
--     remaining   = N,
--     lastUid     = uid | nil,
--     lastSpawnX  = x,
--     lastSpawnY  = y,
--   }

local buildingSpawn = package.loaded["scripts.building_spawn"] or {}
package.loaded["scripts.building_spawn"] = buildingSpawn

local config = {
    acolyte_portal = {
        unit_type      = "acolyte",
        count          = 3,
        spawn_offset   = { x = 0.5, y = 0.5 },
        -- Items handed out to each spawned unit immediately after
        -- spawn, via unit.addItem. The unit YAML carries no
        -- starting_inventory; debug-spawned units come out bare,
        -- portal-spawned units get whatever's listed here.
        starting_items = {
            { def = "canteen_steel_2l", fill = 2.0 },
            -- Two rations per acolyte — enough emergency food to last
            -- ~10 game-hours of idle BMR (500 kcal / 0.92 kcal/sec ≈
            -- 9 minutes real time at timeScale 1).
            { def = "rations" },
            { def = "rations" },
        },
        -- Walk two tiles south so the unit clears the spawn tile
        -- before its commandedTask resolves (the AI's "arrived"
        -- threshold is 0.6 tiles, so a 1-tile walk barely makes it
        -- off the portal). 2 tiles gives breathing room.
        walk_to_offset = { x = 0.5, y = 2.5 },
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
    if s.lastSpawnedAt and (os.time() - s.lastSpawnedAt) > params.stuck_timeout then
        return true
    end
    return false
end

-- Ensure a state entry exists for this building. New entries start at
-- the configured roster size; the count decrements with each spawn.
local function ensureState(bid, params)
    local s = state[bid]
    if not s then
        s = {
            remaining  = params.count,
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
    if s.remaining <= 0 then return end
    if not previousUnitCleared(s, params) then return end

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

    s.remaining     = s.remaining - 1
    s.lastUid       = newUid
    s.lastSpawnX    = spawnX
    s.lastSpawnY    = spawnY
    s.lastSpawnedAt = os.time()

    engine.logInfo(string.format(
        "BuildingSpawn: portal=%d spawned %s id=%d at (%.2f, %.2f) -> walk to (%.2f, %.2f), remaining=%d",
        bid, params.unit_type, newUid, spawnX, spawnY, walkX, walkY, s.remaining))
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
end

function buildingSpawn.update(dt)
    if require("scripts.pause").isPaused() then return end
    local ids = getAllBuildingIds()
    if #ids == 0 then return end
    for _, bid in ipairs(ids) do
        local info = building.getInfo(bid)
        if info and info.defName then
            tickOne(bid, info)
        end
    end
end

function buildingSpawn.shutdown()
    for k in pairs(state) do state[k] = nil end
    engine.logInfo("Building spawn sequencer shut down")
end

return buildingSpawn
