-- World Manager - coordinates world state and rendering
local worldManager = {}

worldManager.currentWorld = nil
worldManager.active = false

-----------------------------------------------------------
-- World Management
-----------------------------------------------------------

function worldManager.createWorld(params)
    local worldId = params.worldId or "main_world"
    local seed = params.seed or 42
    local worldSize = params.worldSize or 64
    
    engine.logInfo("Creating world: " .. worldId 
        .. " (seed=" .. seed .. ", size=" .. worldSize .. " chunks)")
    
    -- Send init command with seed and world size
    world.init(worldId, seed, worldSize)
    
    -- Set all material textures
    if params.graniteTexture then
        world.setTexture(worldId, "granite", params.graniteTexture)
    end
    if params.dioriteTexture then
        world.setTexture(worldId, "diorite", params.dioriteTexture)
    end
    if params.gabbroTexture then
        world.setTexture(worldId, "gabbro", params.gabbroTexture)
    end
    if params.noTexture then
        world.setTexture(worldId, "notexture", params.noTexture)
    end
    if params.isoFaceMap then
        world.setTexture(worldId, "iso_facemap", params.isoFaceMap)
    end
    if params.noFaceMap then
        world.setTexture(worldId, "nofacemap", params.noFaceMap)
    end
    if params.zoomGranite then
        world.setTexture(worldId, "zoom_granite", params.zoomGranite)
    end
    if params.zoomDiorite then
        world.setTexture(worldId, "zoom_diorite", params.zoomDiorite)
    end
    if params.zoomGabbro then
        world.setTexture(worldId, "zoom_gabbro", params.zoomGabbro)
    end
    if params.zoomOcean then
        world.setTexture(worldId, "zoom_ocean", params.zoomOcean)
    end
    
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
    worldManager.active = false
end

function worldManager.destroyWorld(worldId)
    worldId = worldId or worldManager.currentWorld
    
    if not worldId then
        return
    end
    
    worldManager.hideWorld(worldId)
    
    if worldManager.currentWorld == worldId then
        worldManager.currentWorld = nil
    end
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
