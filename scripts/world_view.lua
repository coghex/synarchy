-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0

-- Texture handles
worldView.textures = {
    granite = nil,
    diorite = nil,
    gabbro = nil,
    noTexture = nil,
    isoFaceMap = nil,
    noFaceMap = nil,
}

-- Track which textures have loaded
worldView.texturesNeeded = 6
worldView.texturesLoadedCount = 0

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function worldView.init(width, height)
    worldView.fbW = width
    worldView.fbH = height
    
    -- Load all world textures
    worldView.textures.granite    = engine.loadTexture("assets/textures/world/granite/granite.png")
    worldView.textures.diorite    = engine.loadTexture("assets/textures/world/diorite/diorite.png")
    worldView.textures.gabbro     = engine.loadTexture("assets/textures/world/gabbro/gabbro.png")
    worldView.textures.noTexture  = engine.loadTexture("assets/textures/world/notexture.png")
    worldView.textures.isoFaceMap = engine.loadTexture("assets/textures/world/facemap/isoface.png")
    worldView.textures.noFaceMap  = engine.loadTexture("assets/textures/world/facemap/noface.png")
    
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
        noTexture      = worldView.textures.noTexture,
        isoFaceMap     = worldView.textures.isoFaceMap,
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
        engine.logInfo("Z-slice: " .. tostring(current + 1))
    elseif dy < 0 then
        camera.setZSlice(current - 1)
        engine.logInfo("Z-slice: " .. tostring(current - 1))
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

return worldView
