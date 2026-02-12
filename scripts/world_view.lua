-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0

-- Texture handles
worldView.textures = {
    granite = -1,
    diorite = -1,
    gabbro = -1,
    glacier = -1,
    lava = -1,
    noTexture = -1,
    blankTexture = -1,
    isoFaceMap = -1,
    noFaceMap = -1,
    zoomGranite = -1,
    zoomDiorite = -1,
    zoomGabbro = -1,
    zoomOcean = -1,
    zoomGlacier = -1,
    zoomLava = -1,
    bgGranite = -1,
    bgDiorite = -1,
    bgGabbro = -1,
    bgOcean = -1,
    bgGlacier = -1,
    bgLava = -1,
    -- New materials
    basalt = -1,
    obsidian = -1,
    sandstone = -1,
    limestone = -1,
    shale = -1,
    impactite = -1,
    iron = -1,
    olivine = -1,
    pyroxene = -1,
    feldspar = -1,
    -- Zoom variants
    zoomBasalt = -1,
    zoomObsidian = -1,
    zoomImpactite = -1,
    -- Background variants
    bgBasalt = -1,
    bgImpactite = -1,
}
worldView.texturesNeeded = 0
worldView.texturesLoadedCount = 0

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function worldView.init(width, height)
    -- Track which textures have loaded
    worldView.texturesNeeded = getTableSize(worldView.textures)
    worldView.texturesLoadedCount = 0

    worldView.fbW = width
    worldView.fbH = height
    
    -- Load all world textures
    worldView.textures.granite    = engine.loadTexture("assets/textures/world/granite/granite.png")
    worldView.textures.diorite    = engine.loadTexture("assets/textures/world/diorite/diorite.png")
    worldView.textures.gabbro     = engine.loadTexture("assets/textures/world/gabbro/gabbro.png")
    worldView.textures.glacier    = engine.loadTexture("assets/textures/world/glacier/glacier.png")
    worldView.textures.lava       = engine.loadTexture("assets/textures/world/lava/lava.png")
    worldView.textures.blankTexture = engine.loadTexture("assets/textures/world/blanktexture.png")
    worldView.textures.noTexture  = engine.loadTexture("assets/textures/world/notexture.png")
    worldView.textures.isoFaceMap = engine.loadTexture("assets/textures/world/facemap/isoface.png")
    worldView.textures.noFaceMap  = engine.loadTexture("assets/textures/world/facemap/noface.png")
    worldView.textures.zoomGranite  = engine.loadTexture("assets/textures/world/zoommap/granite_chunk.png")
    worldView.textures.zoomDiorite  = engine.loadTexture("assets/textures/world/zoommap/diorite_chunk.png")
    worldView.textures.zoomGabbro   = engine.loadTexture("assets/textures/world/zoommap/gabbro_chunk.png")
    worldView.textures.zoomOcean    = engine.loadTexture("assets/textures/world/zoommap/ocean_chunk.png")
    worldView.textures.zoomGlacier  = engine.loadTexture("assets/textures/world/zoommap/glacier_chunk.png")
    worldView.textures.zoomLava     = engine.loadTexture("assets/textures/world/zoommap/lava_chunk.png")
    worldView.textures.bgGranite    = engine.loadTexture("assets/textures/world/zoommap/granite_chunk_background.png")
    worldView.textures.bgDiorite    = engine.loadTexture("assets/textures/world/zoommap/diorite_chunk_background.png")
    worldView.textures.bgGabbro     = engine.loadTexture("assets/textures/world/zoommap/gabbro_chunk_background.png")
    worldView.textures.bgOcean      = engine.loadTexture("assets/textures/world/zoommap/ocean_chunk_background.png")
    worldView.textures.bgGlacier    = engine.loadTexture("assets/textures/world/zoommap/glacier_chunk_background.png")
    worldView.textures.bgLava       = engine.loadTexture("assets/textures/world/zoommap/lava_chunk_background.png")
    worldView.textures.basalt       = engine.loadTexture("assets/textures/world/basalt/basalt.png")
    worldView.textures.obsidian     = engine.loadTexture("assets/textures/world/obsidian/obsidian.png")
    worldView.textures.sandstone    = engine.loadTexture("assets/textures/world/sandstone/sandstone.png")
    worldView.textures.limestone    = engine.loadTexture("assets/textures/world/limestone/limestone.png")
    worldView.textures.shale        = engine.loadTexture("assets/textures/world/shale/shale.png")
    worldView.textures.impactite    = engine.loadTexture("assets/textures/world/impactite/impactite.png")
    worldView.textures.iron         = engine.loadTexture("assets/textures/world/iron/iron.png")
    worldView.textures.olivine      = engine.loadTexture("assets/textures/world/olivine/olivine.png")
    worldView.textures.pyroxene     = engine.loadTexture("assets/textures/world/pyroxene/pyroxene.png")
    worldView.textures.feldspar     = engine.loadTexture("assets/textures/world/feldspar/feldspar.png")
    worldView.textures.zoomBasalt     = engine.loadTexture("assets/textures/world/zoommap/basalt_chunk.png")
    worldView.textures.zoomObsidian   = engine.loadTexture("assets/textures/world/zoommap/obsidian_chunk.png")
    worldView.textures.zoomImpactite  = engine.loadTexture("assets/textures/world/zoommap/impactite_chunk.png")
    worldView.textures.bgBasalt       = engine.loadTexture("assets/textures/world/zoommap/basalt_chunk_background.png")
    worldView.textures.bgImpactite    = engine.loadTexture("assets/textures/world/zoommap/impactite_chunk_background.png")
    
    engine.logInfo("World view initialized, loading " .. worldView.texturesNeeded .. " textures")
end

-----------------------------------------------------------
-- Asset Loading Callback
-----------------------------------------------------------

function worldView.onAssetLoaded(assetType, handle, path)
    if assetType ~= "texture" then return end
    
    local matched = false
    for name, texHandle in pairs(worldView.textures) do
        if handle == texHandle then
            engine.logDebug("World texture loaded: " .. name .. " handle=" .. tostring(handle))
            matched = true
            break
        end
    end
    
    if not matched then return end
    
    worldView.texturesLoadedCount = worldView.texturesLoadedCount + 1
    engine.logInfo("World textures: " .. worldView.texturesLoadedCount 
        .. "/" .. worldView.texturesNeeded .. " loaded")
    
    -- Only create world once ALL textures are loaded
    if worldView.texturesLoadedCount >= worldView.texturesNeeded then
        if worldView.visible then
            engine.logInfo("All world textures loaded, creating world...")
            worldView.createWorld()
        end
    end
end

-----------------------------------------------------------
-- Create World
-----------------------------------------------------------

function worldView.createWorld()
    if worldView.texturesLoadedCount < worldView.texturesNeeded then
        engine.logWarn("Cannot create world, textures not loaded yet ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
        return
    end

    if worldManager.isActive() then
        engine.logWarn("World already active, skipping creation")
        return
    end
    
    worldManager.createWorld({ 
        worldId = "main_world",
        graniteTexture = worldView.textures.granite,
        dioriteTexture = worldView.textures.diorite,
        gabbroTexture  = worldView.textures.gabbro,
        glacierTexture = worldView.textures.glacier,
        lavaTexture = worldView.textures.lava,
        blankTexture   = worldView.textures.blankTexture,
        noTexture      = worldView.textures.noTexture,
        isoFaceMap     = worldView.textures.isoFaceMap,
        noFaceMap      = worldView.textures.noFaceMap,
        zoomGranite    = worldView.textures.zoomGranite,
        zoomDiorite    = worldView.textures.zoomDiorite,
        zoomGabbro     = worldView.textures.zoomGabbro,
        zoomOcean      = worldView.textures.zoomOcean,
        zoomGlacier    = worldView.textures.zoomGlacier,
        zoomLava       = worldView.textures.zoomLava,
        bgGranite      = worldView.textures.bgGranite,
        bgDiorite      = worldView.textures.bgDiorite,
        bgGabbro       = worldView.textures.bgGabbro,
        bgOcean        = worldView.textures.bgOcean,
        bgGlacier      = worldView.textures.bgGlacier,
        basaltTexture    = worldView.textures.basalt,
        obsidianTexture  = worldView.textures.obsidian,
        sandstoneTexture = worldView.textures.sandstone,
        limestoneTexture = worldView.textures.limestone,
        shaleTexture     = worldView.textures.shale,
        impactiteTexture = worldView.textures.impactite,
        ironTexture      = worldView.textures.iron,
        olivineTexture   = worldView.textures.olivine,
        pyroxeneTexture  = worldView.textures.pyroxene,
        feldsparTexture  = worldView.textures.feldspar,
        zoomBasalt       = worldView.textures.zoomBasalt,
        zoomObsidian     = worldView.textures.zoomObsidian,
        zoomImpactite    = worldView.textures.zoomImpactite,
        bgBasalt         = worldView.textures.bgBasalt,
        bgImpactite      = worldView.textures.bgImpactite,
        bgLava           = worldView.textures.bgLava
    })
    worldManager.showWorld()
end

-----------------------------------------------------------
-- Create UI
-----------------------------------------------------------

function worldView.createUI()
    if worldView.page then
        UI.deletePage(worldView.page)
    end
    
    worldView.page = UI.newPage("world_view_hud", "hud")
    
    engine.logInfo("World view UI created")
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function worldView.show()
    if not worldView.page then
        worldView.createUI()
    end
    
    worldView.visible = true
    UI.showPage(worldView.page)
    
    if worldView.texturesLoadedCount >= worldView.texturesNeeded then
        worldView.createWorld()
    else
        engine.logInfo("World view shown, waiting for textures... ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
    end
    
    engine.logInfo("World view shown")
end

function worldView.hide()
    worldManager.hideWorld()
    
    worldView.visible = false
    if worldView.page then
        UI.hidePage(worldView.page)
    end
    
    engine.logInfo("World view hidden")
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function worldView.update(dt)
    if not worldView.visible then return end
    worldManager.update(dt)
end

-----------------------------------------------------------
-- Resize
-----------------------------------------------------------

function worldView.onFramebufferResize(width, height)
    worldView.fbW = width
    worldView.fbH = height
    if worldView.visible then
        worldView.createUI()
    end
end

-----------------------------------------------------------
-- Z-Slice Control (shift+scroll)
-----------------------------------------------------------

function worldView.onZSliceScroll(dx, dy)
    if not worldView.visible then return end
    
    local current = camera.getZSlice()
    if dy > 0 then
        camera.setZSlice(current + 1)
        engine.logDebug("Z-slice: " .. tostring(current + 1))
    elseif dy < 0 then
        camera.setZSlice(current - 1)
        engine.logDebug("Z-slice: " .. tostring(current - 1))
    end
end

-----------------------------------------------------------
-- Camera Zoom (normal scroll, no shift, no UI focus)
-----------------------------------------------------------

function worldView.onScroll(dx, dy)
    if not worldView.visible then return end
    
    local current = camera.getZoom()
    local zoomSpeed = 0.1
    if dy > 0 then
        camera.setZoom(math.max(0.1, current - zoomSpeed))
    elseif dy < 0 then
        camera.setZoom(current + zoomSpeed)
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function worldView.shutdown()
    if worldView.page then
        UI.deletePage(worldView.page)
    end
    
    worldManager.destroyWorld()
end

-----------------------------------------------------------
-- Utility
-----------------------------------------------------------

function getTableSize(table)
    local count = 0
    for key, value in pairs(table) do
        count = count + 1
    end
    return count
end

return worldView
