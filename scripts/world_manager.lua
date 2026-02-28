-- World Manager - coordinates world state and rendering
local floraCatalog = require("scripts.flora_catalog")
local worldManager = {}

worldManager.currentWorld = nil
worldManager.active = false

-----------------------------------------------------------
-- Internal: send all textures to a world
-----------------------------------------------------------

local function sendStructuralTextures(worldId, st)
    if not st then return end
    if st.ocean        then world.setTexture(worldId, "ocean",     st.ocean)        end
    if st.glacier      then world.setTexture(worldId, "glacier",   st.glacier)      end
    if st.lava         then world.setTexture(worldId, "lava",      st.lava)         end
    if st.blankTexture then world.setTexture(worldId, "blank",     st.blankTexture) end
    if st.noTexture    then world.setTexture(worldId, "notexture", st.noTexture)    end
    if st.isoFaceMap   then world.setTexture(worldId, "iso_facemap", st.isoFaceMap) end
    if st.noFaceMap    then world.setTexture(worldId, "nofacemap",   st.noFaceMap)  end
    -- Terrain slope facemaps
    local slopeNames = {
        "n", "e", "ne", "s", "ns", "es", "nes",
        "w", "nw", "ew", "new", "sw", "nsw", "esw", "nesw"
    }
    local slopeFields = {
        "isoSlopeFaceMapN", "isoSlopeFaceMapE", "isoSlopeFaceMapNE",
        "isoSlopeFaceMapS", "isoSlopeFaceMapNS", "isoSlopeFaceMapES",
        "isoSlopeFaceMapNES", "isoSlopeFaceMapW", "isoSlopeFaceMapNW",
        "isoSlopeFaceMapEW", "isoSlopeFaceMapNEW", "isoSlopeFaceMapSW",
        "isoSlopeFaceMapNSW", "isoSlopeFaceMapESW", "isoSlopeFaceMapNESW"
    }
    for i, field in ipairs(slopeFields) do
        if st[field] then
            world.setTexture(worldId, "iso_slope_facemap_" .. slopeNames[i], st[field])
        end
    end
    -- Vegetation facemaps
    if st.vegFaceMap then world.setTexture(worldId, "veg_facemap", st.vegFaceMap) end
    local vegSlopeFields = {
        "vegSlopeFaceMapN", "vegSlopeFaceMapE", "vegSlopeFaceMapNE",
        "vegSlopeFaceMapS", "vegSlopeFaceMapNS", "vegSlopeFaceMapES",
        "vegSlopeFaceMapNES", "vegSlopeFaceMapW", "vegSlopeFaceMapNW",
        "vegSlopeFaceMapEW", "vegSlopeFaceMapNEW", "vegSlopeFaceMapSW",
        "vegSlopeFaceMapNSW", "vegSlopeFaceMapESW", "vegSlopeFaceMapNESW"
    }
    for i, field in ipairs(vegSlopeFields) do
        if st[field] then
            world.setTexture(worldId, "veg_slope_facemap_" .. slopeNames[i], st[field])
        end
    end
end

local function sendMaterialTextures(worldId, materials)
    if not materials then return end
    for matId, handles in pairs(materials) do
        if handles.tile then
            world.setTexture(worldId, "mat_tile_" .. matId, handles.tile)
        end
        if handles.zoom then
            world.setTexture(worldId, "mat_zoom_" .. matId, handles.zoom)
        end
        if handles.bg then
            world.setTexture(worldId, "mat_bg_" .. matId, handles.bg)
        end
    end
end

local function sendVegTextures(worldId, vegTextures)
    if not vegTextures then return end
    for vegId, handle in pairs(vegTextures) do
        world.setTexture(worldId, "veg_tile_" .. vegId, handle)
    end
end

-----------------------------------------------------------
-- World Management
-----------------------------------------------------------

function worldManager.createWorld(params)
    local worldId    = params.worldId or "main_world"
    local seed       = params.seed or 42
    local worldSize  = params.worldSize or 128
    local plateCount = params.plateCount or 10

    engine.logInfo("Creating world: " .. worldId
        .. " (seed=" .. seed .. ", size=" .. worldSize .. " chunks)")

    -- Send init command with seed and world size
    -- This queues the WorldInit command; the world state will exist
    -- once the world thread processes it.
    world.init(worldId, seed, worldSize, plateCount)

    -- Send all textures
    sendStructuralTextures(worldId, params.structural)
    sendMaterialTextures(worldId, params.materials)
    sendVegTextures(worldId, params.vegTextures)

    -- Register flora species into the catalog.
    -- world.init() created the world state (with its empty catalog IORef),
    -- and flora.register() writes directly to it via atomicModifyIORef'.
    -- The world thread will snapshot the catalog into WorldGenParams
    -- before generating any chunks.
    floraCatalog.init()

    worldManager.currentWorld = worldId

    engine.logInfo("World created: " .. worldId)
    return worldId
end

function worldManager.showWorld(worldId)
    worldId = worldId or worldManager.currentWorld

    if not worldId then
        engine.logWarn("No world to show")
        return
    end

    engine.logInfo("Showing world: " .. worldId)
    world.show(worldId)
    worldManager.active = true
end

function worldManager.hideWorld(worldId)
    worldId = worldId or worldManager.currentWorld

    if not worldId then
        return
    end

    engine.logInfo("Hiding world: " .. worldId)
    world.hide(worldId)
end

function worldManager.destroyWorld(worldId)
    worldId = worldId or worldManager.currentWorld

    if not worldId then
        return
    end

    engine.logInfo("Destroying world: " .. worldId)

    worldManager.hideWorld(worldId)

    -- Tell Haskell to remove this world from wmWorlds
    world.destroy(worldId)

    if worldManager.currentWorld == worldId then
        worldManager.currentWorld = nil
    end

    -- CRITICAL: reset active flag so a new world can be created
    worldManager.active = false
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function worldManager.update(dt)
    if not worldManager.active then return end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function worldManager.isActive()
    return worldManager.active
end

function worldManager.getCurrentWorld()
    return worldManager.currentWorld
end

return worldManager
