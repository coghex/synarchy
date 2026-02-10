-- World Manager - coordinates world state and rendering
local worldManager = {}

worldManager.currentWorld = nil
worldManager.active = false

-----------------------------------------------------------
-- World Management
-----------------------------------------------------------

function worldManager.createWorld(params)
    local worldId = params.worldId or "main_world"
    local grassTexture = params.grassTexture
    local seed = params.seed or 42
    local worldSize = params.worldSize or 64
    
    engine.logInfo("Creating world: " .. worldId 
        .. " (seed=" .. seed .. ", size=" .. worldSize .. " chunks)")
    
    -- Send init command with seed and world size
    world.init(worldId, seed, worldSize)
    
    if grassTexture then
        world.setTexture(worldId, "grass", grassTexture)
        engine.logInfo("Set grass texture: " .. tostring(grassTexture))
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
    -- Future: send destroy command to Haskell
    
    if worldManager.currentWorld == worldId then
        worldManager.currentWorld = nil
    end
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function worldManager.update(dt)
    if not worldManager.active then return end
    
    -- Future: tick world simulation
    -- world.tick(dt)
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
