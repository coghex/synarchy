-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0

-----------------------------------------------------------
-- Material Table
--
-- Single source of truth for material ID → texture paths.
-- To add a new material, add one row here. No Haskell changes.
--
-- Each entry:
--   id   = MaterialId (Word8) matching Haskell's Material.hs
--   name = human-readable name (for logging)
--   tile = path to isometric tile texture
--   zoom = path to zoom map chunk texture
--   bg   = path to zoom map background texture
-----------------------------------------------------------

worldView.materialDefs = {
    { id = 1,   name = "granite",    tile = "assets/textures/world/granite/granite.png",
                                     zoom = "assets/textures/world/zoommap/granite_chunk.png",
                                     bg   = "assets/textures/world/zoommap/granite_chunk_background.png" },
    { id = 2,   name = "diorite",    tile = "assets/textures/world/diorite/diorite.png",
                                     zoom = "assets/textures/world/zoommap/diorite_chunk.png",
                                     bg   = "assets/textures/world/zoommap/diorite_chunk_background.png" },
    { id = 3,   name = "gabbro",     tile = "assets/textures/world/gabbro/gabbro.png",
                                     zoom = "assets/textures/world/zoommap/gabbro_chunk.png",
                                     bg   = "assets/textures/world/zoommap/gabbro_chunk_background.png" },
    { id = 4,   name = "basalt",     tile = "assets/textures/world/basalt/basalt.png",
                                     zoom = "assets/textures/world/zoommap/basalt_chunk.png",
                                     bg   = "assets/textures/world/zoommap/basalt_chunk_background.png" },
    { id = 5,   name = "obsidian",   tile = "assets/textures/world/obsidian/obsidian.png",
                                     zoom = "assets/textures/world/zoommap/obsidian_chunk.png",
                                     bg   = "assets/textures/world/zoommap/obsidian_chunk_background.png" },
    { id = 10,  name = "sandstone",  tile = "assets/textures/world/sandstone/sandstone.png",
                                     zoom = "assets/textures/world/zoommap/sandstone_chunk.png",
                                     bg   = "assets/textures/world/zoommap/sandstone_chunk_background.png" },
    { id = 11,  name = "limestone",  tile = "assets/textures/world/limestone/limestone.png",
                                     zoom = "assets/textures/world/zoommap/limestone_chunk.png",
                                     bg   = "assets/textures/world/zoommap/limestone_chunk_background.png" },
    { id = 12,  name = "shale",      tile = "assets/textures/world/shale/shale.png",
                                     zoom = "assets/textures/world/zoommap/shale_chunk.png",
                                     bg   = "assets/textures/world/zoommap/shale_chunk_background.png" },
    { id = 20,  name = "impactite",  tile = "assets/textures/world/impactite/impactite.png",
                                     zoom = "assets/textures/world/zoommap/impactite_chunk.png",
                                     bg   = "assets/textures/world/zoommap/impactite_chunk_background.png" },
    { id = 30,  name = "iron",       tile = "assets/textures/world/iron/iron.png",
                                     zoom = "assets/textures/world/zoommap/iron_chunk.png",
                                     bg   = "assets/textures/world/zoommap/iron_chunk_background.png" },
    { id = 31,  name = "olivine",    tile = "assets/textures/world/olivine/olivine.png",
                                     zoom = "assets/textures/world/zoommap/olivine_chunk.png",
                                     bg   = "assets/textures/world/zoommap/olivine_chunk_background.png" },
    { id = 32,  name = "pyroxene",   tile = "assets/textures/world/pyroxene/pyroxene.png",
                                     zoom = "assets/textures/world/zoommap/pyroxene_chunk.png",
                                     bg   = "assets/textures/world/zoommap/pyroxene_chunk_background.png" },
    { id = 33,  name = "feldspar",   tile = "assets/textures/world/feldspar/feldspar.png",
                                     zoom = "assets/textures/world/zoommap/feldspar_chunk.png",
                                     bg   = "assets/textures/world/zoommap/feldspar_chunk_background.png" },
    { id = 100, name = "lava",       tile = "assets/textures/world/lava/lava.png",
                                     zoom = "assets/textures/world/zoommap/lava_chunk.png",
                                     bg   = "assets/textures/world/zoommap/lava_chunk_background.png" },
}

-----------------------------------------------------------
-- Texture Storage
--
-- Structural textures: named fields (small fixed set)
-- Material textures:   materialTextures[id] = { tile=h, zoom=h, bg=h }
-----------------------------------------------------------

worldView.structuralTextures = {
    ocean          = -1,
    glacier        = -1,
    lava           = -1,
    noTexture      = -1,
    blankTexture   = -1,
    isoFaceMap     = -1,
    isoSlopeFaceMapN    = -1,
    isoSlopeFaceMapE    = -1,
    isoSlopeFaceMapNE   = -1,
    isoSlopeFaceMapS    = -1,
    isoSlopeFaceMapNS   = -1,
    isoSlopeFaceMapES   = -1,
    isoSlopeFaceMapNES  = -1,
    isoSlopeFaceMapW    = -1,
    isoSlopeFaceMapNW   = -1,
    isoSlopeFaceMapEW   = -1,
    isoSlopeFaceMapNEW  = -1,
    isoSlopeFaceMapSW   = -1,
    isoSlopeFaceMapNSW  = -1,
    isoSlopeFaceMapESW  = -1,
    isoSlopeFaceMapNESW = -1,
    noFaceMap      = -1,
}

-- materialTextures[matId] = { tile = handle, zoom = handle, bg = handle }
worldView.materialTextures = {}

-- All loaded handles for asset-loaded tracking
worldView.allHandles = {}

worldView.texturesNeeded = 0
worldView.texturesLoadedCount = 0

-- Whether we've been asked to generate (from create_world_menu)
-- but are still waiting for textures to load
worldView.pendingGeneration = false

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function worldView.init(width, height)
    worldView.fbW = width
    worldView.fbH = height
    worldView.allHandles = {}

    -- Count how many textures we need to load
    local count = 0

    -- Load structural textures
    local st = worldView.structuralTextures
    st.ocean          = engine.loadTexture("assets/textures/world/ocean/ocean.png")
    st.glacier        = engine.loadTexture("assets/textures/world/glacier/glacier.png")
    st.lava           = engine.loadTexture("assets/textures/world/lava/lava.png")
    st.noTexture      = engine.loadTexture("assets/textures/world/notexture.png")
    st.blankTexture   = engine.loadTexture("assets/textures/world/blanktexture.png")
    st.isoFaceMap     = engine.loadTexture("assets/textures/world/facemap/isoface.png")
    st.isoSlopeFaceMapN    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_n.png")
    st.isoSlopeFaceMapE    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_e.png")
    st.isoSlopeFaceMapNE   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_ne.png")
    st.isoSlopeFaceMapS    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_s.png")
    st.isoSlopeFaceMapNS   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_ns.png")
    st.isoSlopeFaceMapES   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_es.png")
    st.isoSlopeFaceMapNES  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nes.png")
    st.isoSlopeFaceMapW    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_w.png")
    st.isoSlopeFaceMapNW   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nw.png")
    st.isoSlopeFaceMapEW   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_ew.png")
    st.isoSlopeFaceMapNEW  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_new.png")
    st.isoSlopeFaceMapSW   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_sw.png")
    st.isoSlopeFaceMapNSW  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nsw.png")
    st.isoSlopeFaceMapESW  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_esw.png")
    st.isoSlopeFaceMapNESW = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nesw.png")
    st.noFaceMap      = engine.loadTexture("assets/textures/world/facemap/noface.png")

    for _, handle in pairs(st) do
        worldView.allHandles[handle] = true
        count = count + 1
    end

    -- Load material textures from the table
    worldView.materialTextures = {}
    for _, def in ipairs(worldView.materialDefs) do
        local tileH = engine.loadTexture(def.tile)
        local zoomH = engine.loadTexture(def.zoom)
        local bgH   = engine.loadTexture(def.bg)
        worldView.materialTextures[def.id] = {
            tile = tileH,
            zoom = zoomH,
            bg   = bgH,
        }
        worldView.allHandles[tileH] = true
        worldView.allHandles[zoomH] = true
        worldView.allHandles[bgH]   = true
        count = count + 3
    end

    worldView.texturesNeeded = count
    worldView.texturesLoadedCount = 0

    engine.logInfo("World view initialized, loading " .. count .. " textures")
end

-----------------------------------------------------------
-- Asset Loading Callback
-----------------------------------------------------------

function worldView.onAssetLoaded(assetType, handle, path)
    if assetType ~= "texture" then return end

    if not worldView.allHandles[handle] then return end

    worldView.texturesLoadedCount = worldView.texturesLoadedCount + 1
    engine.logDebug("World texture loaded: handle=" .. tostring(handle)
        .. " (" .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")

    -- Check if all textures are now loaded
    if worldView.texturesLoadedCount >= worldView.texturesNeeded then
        if worldView.visible then
            engine.logInfo("All world textures loaded, creating world...")
            worldView.createWorld()
        end
        if worldView.pendingGeneration then
            worldView.pendingGeneration = false
            engine.logInfo("Pending generation triggered, creating world...")
            worldView.createWorld()
        end
    end
end

-----------------------------------------------------------
-- Start Generation (called from create_world_menu)
-----------------------------------------------------------

function worldView.startGeneration()
    if worldView.texturesLoadedCount >= worldView.texturesNeeded then
        worldView.createWorld()
    else
        worldView.pendingGeneration = true
        engine.logInfo("World generation deferred, waiting for textures... ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
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

    local wp = worldView.worldParams or {}
    local seed = wp.seed or 42
    local worldSize = wp.worldSize or 128
    local plateCount = wp.plateCount or 10

    worldManager.createWorld({
        worldId    = "main_world",
        seed       = seed,
        worldSize  = worldSize,
        plateCount = plateCount,
        structural = worldView.structuralTextures,
        materials  = worldView.materialTextures,
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

    if worldManager.getCurrentWorld() then
        worldManager.showWorld()
    elseif worldView.texturesLoadedCount >= worldView.texturesNeeded then
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
-- Camera Zoom (normal scroll, no shift, no UI focus)
-----------------------------------------------------------

local zoomImpulseScale = 0.4

function worldView.onScroll(dx, dy)
    if not worldView.visible then return end

    local current = camera.getZoomVelocity()
    local zoom = camera.getZoom()
    local impulse = zoomImpulseScale * zoom
    if dy > 0 then
        camera.setZoomVelocity(current - impulse)
    elseif dy < 0 then
        camera.setZoomVelocity(current + impulse)
    end
end

-----------------------------------------------------------
-- Z-Slice Control (shift+scroll)
-----------------------------------------------------------

function worldView.onZSliceScroll(dx, dy)
    if not worldView.visible then return end

    camera.setZTracking(false)

    local current = camera.getZSlice()
    if dy > 0 then
        camera.setZSlice(current + 1)
    elseif dy < 0 then
        camera.setZSlice(current - 1)
    end
end

-----------------------------------------------------------
-- Key Input
-----------------------------------------------------------

function worldView.onKeyDown(key)
    if not worldView.visible then return end

    if key == "Q" then
        camera.rotateCCW()
        engine.logDebug("Camera rotated CCW, facing=" .. tostring(camera.getFacing()))
    elseif key == "E" then
        camera.rotateCW()
        engine.logDebug("Camera rotated CW, facing=" .. tostring(camera.getFacing()))
    elseif key == "Home" then
        camera.setZTracking(true)
        engine.logDebug("Z-slice tracking re-enabled")
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

function worldView.saveGame(saveName)
    local worldId = worldManager.getCurrentWorld()
    if worldId then
        engine.saveWorld(worldId, saveName or "quicksave")
        engine.logInfo("Game saved: " .. (saveName or "quicksave"))
    end
end

-----------------------------------------------------------
-- Send textures to an existing world (for loaded saves)
-----------------------------------------------------------

function worldView.sendTexturesToWorld(worldId)
    if worldView.texturesLoadedCount < worldView.texturesNeeded then
        engine.logWarn("Cannot send textures, not all loaded yet ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
        return false
    end

    engine.logInfo("Sending textures to loaded world: " .. worldId)

    -- Structural
    local st = worldView.structuralTextures
    world.setTexture(worldId, "ocean",     st.ocean)
    world.setTexture(worldId, "glacier",   st.glacier)
    world.setTexture(worldId, "lava",      st.lava)
    world.setTexture(worldId, "blank",     st.blankTexture)
    world.setTexture(worldId, "notexture", st.noTexture)
    world.setTexture(worldId, "iso_facemap",           st.isoFaceMap)
    world.setTexture(worldId, "iso_slope_facemap_n",   st.isoSlopeFaceMapN)
    world.setTexture(worldId, "iso_slope_facemap_e",   st.isoSlopeFaceMapE)
    world.setTexture(worldId, "iso_slope_facemap_ne",  st.isoSlopeFaceMapNE)
    world.setTexture(worldId, "iso_slope_facemap_s",   st.isoSlopeFaceMapS)
    world.setTexture(worldId, "iso_slope_facemap_ns",  st.isoSlopeFaceMapNS)
    world.setTexture(worldId, "iso_slope_facemap_es",  st.isoSlopeFaceMapES)
    world.setTexture(worldId, "iso_slope_facemap_nes", st.isoSlopeFaceMapNES)
    world.setTexture(worldId, "iso_slope_facemap_w",   st.isoSlopeFaceMapW)
    world.setTexture(worldId, "iso_slope_facemap_nw",  st.isoSlopeFaceMapNW)
    world.setTexture(worldId, "iso_slope_facemap_ew",  st.isoSlopeFaceMapEW)
    world.setTexture(worldId, "iso_slope_facemap_new", st.isoSlopeFaceMapNEW)
    world.setTexture(worldId, "iso_slope_facemap_sw",  st.isoSlopeFaceMapSW)
    world.setTexture(worldId, "iso_slope_facemap_nsw", st.isoSlopeFaceMapNSW)
    world.setTexture(worldId, "iso_slope_facemap_esw", st.isoSlopeFaceMapESW)
    world.setTexture(worldId, "iso_slope_facemap_nesw",st.isoSlopeFaceMapNESW)
    world.setTexture(worldId, "nofacemap",             st.noFaceMap)

    -- Materials (loop over the loaded handles)
    for matId, handles in pairs(worldView.materialTextures) do
        world.setTexture(worldId, "mat_tile_" .. matId, handles.tile)
        world.setTexture(worldId, "mat_zoom_" .. matId, handles.zoom)
        world.setTexture(worldId, "mat_bg_"   .. matId, handles.bg)
    end

    engine.logInfo("All textures sent to world: " .. worldId)
    return true
end

return worldView
