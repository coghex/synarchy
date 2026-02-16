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
    local worldSize = params.worldSize or 128
    local plateCount = params.plateCount or 10
    
    engine.logInfo("Creating world: " .. worldId 
        .. " (seed=" .. seed .. ", size=" .. worldSize .. " chunks)")
    
    -- Send init command with seed and world size
    world.init(worldId, seed, worldSize, plateCount)
    
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
    if params.oceanTexture then
        world.setTexture(worldId, "ocean", params.oceanTexture)
    end
    if params.glacierTexture then
        world.setTexture(worldId, "glacier", params.glacierTexture)
    end
    if params.lavaTexture then
        world.setTexture(worldId, "lava", params.lavaTexture)
    end
    if params.blankTexture then
        world.setTexture(worldId, "blank", params.blankTexture)
    end
    if params.noTexture then
        world.setTexture(worldId, "notexture", params.noTexture)
    end
    if params.isoFaceMap then
        world.setTexture(worldId, "iso_facemap", params.isoFaceMap)
    end
    if params.isoSlopeFaceMapNW then
        world.setTexture(worldId, "iso_slope_facemask_nw", params.isoSlopeFaceMapNW)
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
    if params.zoomGlacier then
        world.setTexture(worldId, "zoom_glacier", params.zoomGlacier)
    end
    if params.zoomLava then
        world.setTexture(worldId, "zoom_lava", params.zoomLava)
    end
    if params.bgGranite then
        world.setTexture(worldId, "bg_granite", params.bgGranite)
    end
    if params.bgDiorite then
        world.setTexture(worldId, "bg_diorite", params.bgDiorite)
    end
    if params.bgGabbro then
        world.setTexture(worldId, "bg_gabbro", params.bgGabbro)
    end
    if params.bgOcean then
        world.setTexture(worldId, "bg_ocean", params.bgOcean)
    end
    if params.bgGlacier then
        world.setTexture(worldId, "bg_glacier", params.bgGlacier)
    end
    if params.basaltTexture then
        world.setTexture(worldId, "basalt", params.basaltTexture)
    end
    if params.obsidianTexture then
        world.setTexture(worldId, "obsidian", params.obsidianTexture)
    end
    if params.sandstoneTexture then
        world.setTexture(worldId, "sandstone", params.sandstoneTexture)
    end
    if params.limestoneTexture then
        world.setTexture(worldId, "limestone", params.limestoneTexture)
    end
    if params.shaleTexture then
        world.setTexture(worldId, "shale", params.shaleTexture)
    end
    if params.impactiteTexture then
        world.setTexture(worldId, "impactite", params.impactiteTexture)
    end
    if params.ironTexture then
        world.setTexture(worldId, "iron", params.ironTexture)
    end
    if params.olivineTexture then
        world.setTexture(worldId, "olivine", params.olivineTexture)
    end
    if params.pyroxeneTexture then
        world.setTexture(worldId, "pyroxene", params.pyroxeneTexture)
    end
    if params.feldsparTexture then
        world.setTexture(worldId, "feldspar", params.feldsparTexture)
    end
    if params.zoomBasalt then
        world.setTexture(worldId, "zoom_basalt", params.zoomBasalt)
    end
    if params.zoomObsidian then
        world.setTexture(worldId, "zoom_obsidian", params.zoomObsidian)
    end
    if params.zoomImpactite then
        world.setTexture(worldId, "zoom_impactite", params.zoomImpactite)
    end
    if params.bgBasalt then
        world.setTexture(worldId, "bg_basalt", params.bgBasalt)
    end
    if params.bgImpactite then
        world.setTexture(worldId, "bg_impactite", params.bgImpactite)
    end
    if params.bgObsidian then
        world.setTexture(worldId, "bg_obsidian", params.bgObsidian)
    end
    if params.bgLava then
        world.setTexture(worldId, "bg_lava", params.bgLava)
    end
    -- Sedimentary + mineral zoom/bg textures
    if params.zoomSandstone then
        world.setTexture(worldId, "zoom_sandstone", params.zoomSandstone)
    end
    if params.zoomLimestone then
        world.setTexture(worldId, "zoom_limestone", params.zoomLimestone)
    end
    if params.zoomShale then
        world.setTexture(worldId, "zoom_shale", params.zoomShale)
    end
    if params.zoomIron then
        world.setTexture(worldId, "zoom_iron", params.zoomIron)
    end
    if params.zoomOlivine then
        world.setTexture(worldId, "zoom_olivine", params.zoomOlivine)
    end
    if params.zoomPyroxene then
        world.setTexture(worldId, "zoom_pyroxene", params.zoomPyroxene)
    end
    if params.zoomFeldspar then
        world.setTexture(worldId, "zoom_feldspar", params.zoomFeldspar)
    end
    if params.bgSandstone then
        world.setTexture(worldId, "bg_sandstone", params.bgSandstone)
    end
    if params.bgLimestone then
        world.setTexture(worldId, "bg_limestone", params.bgLimestone)
    end
    if params.bgShale then
        world.setTexture(worldId, "bg_shale", params.bgShale)
    end
    if params.bgIron then
        world.setTexture(worldId, "bg_iron", params.bgIron)
    end
    if params.bgOlivine then
        world.setTexture(worldId, "bg_olivine", params.bgOlivine)
    end
    if params.bgPyroxene then
        world.setTexture(worldId, "bg_pyroxene", params.bgPyroxene)
    end
    if params.bgFeldspar then
        world.setTexture(worldId, "bg_feldspar", params.bgFeldspar)
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
