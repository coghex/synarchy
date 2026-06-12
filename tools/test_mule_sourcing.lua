-- Offline regression harness for the technomule arc:
--   1. building_spawn roster order — five acolytes out of the portal,
--      then the technomule, derived purely from the engine-side
--      remaining countdown (survives save/load).
--   2. deliver_to_build_site mule sourcing — an acolyte with no
--      materials claims a delivery against the technomule's stock,
--      walks to the mule, takes the shortfall via
--      unit.transferItemToUnit, then delivers to the site.
--   3. capacity gate during the fetch — an acolyte that can only
--      carry part of the shortfall takes what fits and shrinks its
--      claim so the rest frees up for other deliverers.
--
-- Run: luajit tools/test_mule_sourcing.lua

package.path = "./?.lua;" .. package.path

local now = 1000.0

-----------------------------------------------------------
-- World state the stubs serve
-----------------------------------------------------------

local units = {}      -- uid -> {defName, gridX, gridY, stats, inventory}
local buildings = {}  -- bid -> {defName, activity, gridX, gridY, tileW, tileH,
                      --         need, delivered, buildRequired}
local transferLog = {}

local ITEM_DEFS = {
    { name = "steel_plate",     weight = 1.2 },
    { name = "steel_bar",       weight = 0.5 },
    { name = "steel_hardware",  weight = 0.2 },
    { name = "electric_motor",  weight = 2.5 },
    { name = "processing_unit", weight = 0.4 },
}
local function itemWeight(defName)
    for _, d in ipairs(ITEM_DEFS) do
        if d.name == defName then return d.weight end
    end
    return 0
end

local function invCount(u, defName)
    local n = 0
    for _, it in ipairs(u.inventory) do
        if it.defName == defName then n = n + 1 end
    end
    return n
end

local function invWeight(u)
    local s = 0
    for _, it in ipairs(u.inventory) do s = s + itemWeight(it.defName) end
    return s
end

-----------------------------------------------------------
-- Engine API stubs
-----------------------------------------------------------

engine = {
    gameTime = function() return now end,
    realTime = function() return now end,
    isPaused = function() return false end,
    logInfo = function() end, logDebug = function() end,
    logWarn = function() end, logError = function() end,
}

unit = {
    getAllIds = function()
        local ids = {}
        for uid in pairs(units) do ids[#ids + 1] = uid end
        table.sort(ids)
        return ids
    end,
    getInfo = function(uid) return units[uid] end,
    exists = function(uid) return units[uid] ~= nil end,
    getPose = function(uid) return units[uid] and "standing" end,
    getActivity = function(uid) return units[uid] and "idle" end,
    getStat = function(uid, name)
        local u = units[uid]
        return u and u.stats and u.stats[name]
    end,
    getInventory = function(uid)
        local u = units[uid]
        return u and u.inventory
    end,
    getCarryingWeight = function(uid)
        local u = units[uid]
        return u and invWeight(u) or 0
    end,
    getVisibleTiles = function() return nil end,
    getLastAttacker = function() return nil end,
    getMaxSpeed = function() return 1.4 end,
    getPos = function(uid)
        local u = units[uid]
        return u.gridX, u.gridY
    end,
    setAnim = function() end,
    setAnimOverride = function() end,
    clearAnimOverride = function() end,
    -- Teleport-on-moveTo keeps the walk phases single-tick so the
    -- harness doesn't have to simulate movement.
    moveTo = function(uid, x, y)
        local u = units[uid]
        if u then u.gridX, u.gridY = x, y end
    end,
    stop = function() end,
    transferItemToUnit = function(fromUid, toUid, defName)
        local uF, uT = units[fromUid], units[toUid]
        if not uF or not uT or fromUid == toUid then return false end
        for i, it in ipairs(uF.inventory) do
            if it.defName == defName then
                table.remove(uF.inventory, i)
                table.insert(uT.inventory, it)
                transferLog[#transferLog + 1] =
                    { from = fromUid, to = toUid, def = defName }
                return true
            end
        end
        return false
    end,
    transferItemToBuilding = function(uid, bid, defName)
        local u, b = units[uid], buildings[bid]
        if not u or not b then return false end
        for i, it in ipairs(u.inventory) do
            if it.defName == defName then
                table.remove(u.inventory, i)
                b.delivered[defName] = (b.delivered[defName] or 0) + 1
                return true
            end
        end
        return false
    end,
}

building = {
    list = function()
        local lines = {}
        for bid, b in pairs(buildings) do
            lines[#lines + 1] = "id=" .. bid .. " " .. b.defName
        end
        if #lines == 0 then return "No buildings placed" end
        return table.concat(lines, "\n")
    end,
    getInfo = function(bid) return buildings[bid] end,
    getActivity = function(bid)
        local b = buildings[bid]
        return b and b.activity
    end,
    getMaterialNeed = function(bid)
        local b = buildings[bid]
        return b and b.need
    end,
    getMaterialDelivered = function(bid)
        local b = buildings[bid]
        return b and b.delivered
    end,
    areMaterialsSatisfied = function(bid)
        local b = buildings[bid]
        if not b then return true end
        for mat, n in pairs(b.need) do
            if (b.delivered[mat] or 0) < n then return false end
        end
        return true
    end,
    getBuildRequired = function() return 0 end,
    getStorageCapacity = function() return 0 end,
    getStorageWeight = function() return 0 end,
    getSpawnRemaining = function(bid)
        local b = buildings[bid]
        return b and b.spawnRemaining
    end,
    setSpawnRemaining = function(bid, n)
        local b = buildings[bid]
        if b then b.spawnRemaining = n end
    end,
    consumeSpawn = function(bid)
        local b = buildings[bid]
        if not b then return 0 end
        b.spawnRemaining = math.max(0, (b.spawnRemaining or 0) - 1)
        return b.spawnRemaining
    end,
    addBuildProgress = function() end,
}

item = {
    listDefs = function() return ITEM_DEFS end,
    listGround = function() return {} end,
}

world = {
    getFluidAt = function() return nil end,
    getActiveWorldId = function() return nil end,
    getDigInfoAt = function() return nil end,
    getMineDesignationAt = function() return nil end,
    getSurfaceAt = function() return nil end,
    nearestMineDesignation = function() return nil end,
    digTile = function() return false end,
}
combat = {}

-- Engine-style require (dofile + package.loaded; LuaJIT's native
-- require breaks the self-registration pattern these modules use).
function require(name)
    local cached = package.loaded[name]
    if type(cached) == "table" then return cached end
    local chunk = assert(loadfile(name:gsub("%.", "/") .. ".lua"))
    local ret = chunk()
    if package.loaded[name] == nil then package.loaded[name] = ret end
    return package.loaded[name]
end

local unitAi = require("scripts.unit_ai")

local function tick(n)
    for _ = 1, (n or 1) do
        now = now + 2.0
        unitAi.update(0.1)
    end
end

-----------------------------------------------------------
-- Test 1: portal roster order (building_spawn)
-----------------------------------------------------------

do
    local spawned = {}
    local nextUid = 100
    unit.spawn = function(defName, x, y, _z, _faction)
        nextUid = nextUid + 1
        units[nextUid] = { defName = defName, gridX = x, gridY = y,
                           stats = {}, inventory = {} }
        spawned[#spawned + 1] = defName
        return nextUid
    end
    unit.addItem = function() end

    buildings[1] = {
        defName = "acolyte_portal", activity = "built",
        gridX = 0, gridY = 0, tileW = 2, tileH = 2,
        need = {}, delivered = {},
        spawnRemaining = -1,   -- engine sentinel: never seeded
    }

    local buildingSpawn = require("scripts.building_spawn")
    -- Drive enough ticks for all six spawns. Each spawned unit
    -- teleports on moveTo (stub), clearing the spawn tile instantly;
    -- the 2 s spawn_interval is covered by now advancing 2 s per tick.
    for _ = 1, 40 do
        now = now + 2.0
        buildingSpawn.update(0.1)
    end

    assert(#spawned == 6,
        "expected 6 portal spawns, got " .. #spawned)
    for i = 1, 5 do
        assert(spawned[i] == "acolyte",
            "spawn " .. i .. " should be acolyte, got " .. spawned[i])
    end
    assert(spawned[6] == "technomule",
        "spawn 6 should be technomule, got " .. spawned[6])
    assert(buildings[1].spawnRemaining == 0, "countdown not depleted")
    print("PASS: portal roster spawns 5 acolytes then the technomule")

    -- Clean up roster units; the next tests build their own cast.
    for uid in pairs(units) do units[uid] = nil end
    buildings[1] = nil
end

-----------------------------------------------------------
-- Test 2: deliver_to_build_site sources from the mule
-----------------------------------------------------------

local function freshDeliveryScenario(acolyteCapacity)
    for uid in pairs(units) do units[uid] = nil end
    for bid in pairs(buildings) do buildings[bid] = nil end
    for k in pairs(unitAi.aiState) do unitAi.aiState[k] = nil end
    for i = #transferLog, 1, -1 do transferLog[i] = nil end

    units[1] = { defName = "acolyte", gridX = 5, gridY = 5,
                 stats = { carrying_capacity = acolyteCapacity },
                 inventory = {} }
    -- Close out the water-goal chain (find_water utility 5.0 beats
    -- deliver 4.0 and would starve the test otherwise).
    unitAi.aiState[1] = {
        nextActionAt    = 0,
        actionStartedAt = 0,
        goalStatus = { find_water = "accomplished",
                       notify_allies = "accomplished" },
    }
    units[2] = { defName = "technomule", gridX = 20, gridY = 5,
                 stats = { carrying_capacity = 250 },
                 inventory = {} }
    for _ = 1, 10 do
        table.insert(units[2].inventory, { defName = "steel_plate" })
    end
    buildings[1] = {
        defName = "cargo_hold_S", activity = "appearing",
        gridX = 8, gridY = 8, tileW = 2, tileH = 2,
        need = { steel_plate = 4 }, delivered = {},
        spawnRemaining = 0,
    }
end

do
    freshDeliveryScenario(25.0)
    -- A few ticks: claim → walk to mule (teleport) → take 4 plates →
    -- walk to site (teleport) → hand over.
    tick(8)

    local b = buildings[1]
    assert((b.delivered.steel_plate or 0) == 4,
        "expected 4 plates delivered, got "
        .. tostring(b.delivered.steel_plate))
    assert(#transferLog == 4, "expected 4 mule transfers, got "
        .. #transferLog)
    for _, t in ipairs(transferLog) do
        assert(t.from == 2 and t.to == 1 and t.def == "steel_plate",
            "unexpected transfer " .. t.from .. "->" .. t.to)
    end
    assert(invCount(units[2], "steel_plate") == 6,
        "mule should have 6 plates left")
    assert(building.areMaterialsSatisfied(1), "site not satisfied")
    print("PASS: acolyte fetched 4 plates from the mule and delivered")
end

-----------------------------------------------------------
-- Test 3: capacity gate during the fetch
-----------------------------------------------------------

do
    -- Capacity 2.5 kg fits two 1.2 kg plates per trip, not four. The
    -- gate caps the load each fetch and the shrunk claim re-opens the
    -- remaining need, so the acolyte ends up making two trips and the
    -- site still gets all 4 — without the unit ever exceeding its
    -- capacity in between.
    freshDeliveryScenario(2.5)
    local maxCarried = 0
    local baseTransfer = unit.transferItemToUnit
    unit.transferItemToUnit = function(fromUid, toUid, defName)
        local ok = baseTransfer(fromUid, toUid, defName)
        if ok then
            maxCarried = math.max(maxCarried, invWeight(units[toUid]))
        end
        return ok
    end
    tick(16)
    unit.transferItemToUnit = baseTransfer

    local b = buildings[1]
    assert((b.delivered.steel_plate or 0) == 4,
        "expected all 4 plates delivered across trips, got "
        .. tostring(b.delivered.steel_plate))
    assert(#transferLog == 4, "expected 4 mule transfers, got "
        .. #transferLog)
    assert(maxCarried <= 2.5 + 1e-6,
        "capacity gate breached: carried " .. maxCarried .. " kg")
    assert(invCount(units[2], "steel_plate") == 6,
        "mule should have 6 plates left")
    print("PASS: capacity gate caps each trip; two trips deliver all 4")
end

print("mule sourcing harness: all tests passed")
