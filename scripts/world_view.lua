-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0
worldView.grassTexture = nil
worldView.texturesLoaded = false

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function worldView.init(width, height)
    worldView.fbW = width
    worldView.fbH = height
    
    -- Load world textures
    worldView.grassTexture = engine.loadTexture("assets/textures/grass.png")
    
    engine.logInfo("World view initialized, grass texture handle: " .. tostring(worldView.grassTexture))
end

-----------------------------------------------------------
-- Asset Loading Callback
-----------------------------------------------------------

function worldView.onAssetLoaded(assetType, handle, path)
    engine.logDebug("Asset loaded callback: " .. assetType .. " handle=" .. tostring(handle) .. " path=" .. path)
    
    if assetType == "texture" then
        if handle == worldView.grassTexture then
            worldView.texturesLoaded = true
            engine.logDebug("World textures loaded - GRASS TEXTURE READY")
            
            if worldView.visible then
                engine.logInfo("World visible, creating world with loaded texture...")
                worldView.createWorld()
            end
        else
            engine.logDebug("Texture loaded but not grass texture: " .. tostring(handle) .. " vs " .. tostring(worldView.grassTexture))
        end
    end
end

-----------------------------------------------------------
-- Create World
-----------------------------------------------------------

function worldView.createWorld()
    engine.logInfo("createWorld called, texturesLoaded=" .. tostring(worldView.texturesLoaded) .. " grassTexture=" .. tostring(worldView.grassTexture))
    
    if not worldView.texturesLoaded then
        engine.logWarn("Cannot create world, textures not loaded yet")
        return
    end

    if worldManager.isActive() then
        engine.logWarn("World already active, skipping creation")
        return
    end
    
    worldManager.createWorld({ 
        worldId = "main_world",
        grassTexture = worldView.grassTexture
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
    
    if worldView.texturesLoaded then
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
-- Shutdown
-----------------------------------------------------------

function worldView.shutdown()
    if worldView.page then
        UI.deletePage(worldView.page)
    end
    
    worldManager.destroyWorld()
end

return worldView
