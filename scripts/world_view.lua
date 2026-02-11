-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0
worldView.graniteTexture = nil
worldView.isoFaceMap = nil
worldView.texturesLoaded = false
worldView.faceMapLoaded = false

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function worldView.init(width, height)
    worldView.fbW = width
    worldView.fbH = height
    
    -- Load world textures
    worldView.graniteTexture = engine.loadTexture("assets/textures/world/granite/granite.png")
    worldView.isoFaceMap = engine.loadTexture("assets/textures/world/facemap/isoface.png")
    
end

-----------------------------------------------------------
-- Asset Loading Callback
-----------------------------------------------------------

function worldView.onAssetLoaded(assetType, handle, path)
    engine.logDebug("Asset loaded callback: " .. assetType .. " handle=" .. tostring(handle) .. " path=" .. path)
    
    if assetType == "texture" then
        if handle == worldView.graniteTexture then
            worldView.texturesLoaded = true
        elseif handle == worldView.isoFaceMap then
            worldView.faceMapLoaded = true
        else
            engine.logDebug("Texture loaded but not granite: " .. tostring(handle))
        end
        
        -- Only create world once BOTH textures are loaded
        if worldView.texturesLoaded and worldView.faceMapLoaded then
            if worldView.visible then
                engine.logInfo("World visible, all textures loaded, creating world...")
                worldView.createWorld()
            end
        end
    end
end

-----------------------------------------------------------
-- Create World
-----------------------------------------------------------

function worldView.createWorld()
    if not worldView.texturesLoaded or not worldView.faceMapLoaded then
        engine.logWarn("Cannot create world, textures not loaded yet")
        return
    end

    if worldManager.isActive() then
        engine.logWarn("World already active, skipping creation")
        return
    end
    
    worldManager.createWorld({ 
        worldId = "main_world",
        graniteTexture = worldView.graniteTexture,
        isoFaceMap = worldView.isoFaceMap,
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
    
    if worldView.texturesLoaded and worldView.faceMapLoaded then
        worldView.createWorld()
    else
        engine.logInfo("World view shown, waiting for textures...")
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
    
    -- Camera panning is handled in Haskell (Engine.Loop.Camera)
    
    -- Update world state
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
