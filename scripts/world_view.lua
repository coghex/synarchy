-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0

-----------------------------------------------------------
-- Vegetation Table
--
-- Each entry maps a vegId range to a texture.
-- Variants 0-3 within each type use the same base
-- texture with different UV offsets or separate files.
-----------------------------------------------------------

worldView.vegDefs = {
    -- Sparse grass (4 variants)
    { idStart = 1,  idEnd = 4,  name = "sparse_grass",
      tiles = {
          "assets/textures/world/veg/sparse_grass_1.png",
          "assets/textures/world/veg/sparse_grass_2.png",
          "assets/textures/world/veg/sparse_grass_3.png",
          "assets/textures/world/veg/sparse_grass_4.png",
      }},
    -- Medium grass
    { idStart = 5,  idEnd = 8,  name = "medium_grass",
      tiles = {
          "assets/textures/world/veg/medium_grass_1.png",
          "assets/textures/world/veg/medium_grass_2.png",
          "assets/textures/world/veg/medium_grass_3.png",
          "assets/textures/world/veg/medium_grass_4.png",
      }},
    -- Dense grass
    { idStart = 9,  idEnd = 12, name = "dense_grass",
      tiles = {
          "assets/textures/world/veg/dense_grass_1.png",
          "assets/textures/world/veg/dense_grass_2.png",
          "assets/textures/world/veg/dense_grass_3.png",
          "assets/textures/world/veg/dense_grass_4.png",
      }},
    -- Tall grass (prairie)
    { idStart = 13, idEnd = 16, name = "tall_grass",
      tiles = {
          "assets/textures/world/veg/tall_grass_1.png",
          "assets/textures/world/veg/tall_grass_2.png",
          "assets/textures/world/veg/tall_grass_3.png",
          "assets/textures/world/veg/tall_grass_4.png",
      }},
    -- Thin moss
    { idStart = 17, idEnd = 20, name = "thin_moss",
      tiles = {
          "assets/textures/world/veg/thin_moss_1.png",
          "assets/textures/world/veg/thin_moss_2.png",
          "assets/textures/world/veg/thin_moss_3.png",
          "assets/textures/world/veg/thin_moss_4.png",
      }},
    -- Thick moss
    { idStart = 21, idEnd = 24, name = "thick_moss",
      tiles = {
          "assets/textures/world/veg/thick_moss_1.png",
          "assets/textures/world/veg/thick_moss_2.png",
          "assets/textures/world/veg/thick_moss_3.png",
          "assets/textures/world/veg/thick_moss_4.png",
      }},
    -- Light ivy
    { idStart = 25, idEnd = 28, name = "light_ivy",
      tiles = {
          "assets/textures/world/veg/light_ivy_1.png",
          "assets/textures/world/veg/light_ivy_2.png",
          "assets/textures/world/veg/light_ivy_3.png",
          "assets/textures/world/veg/light_ivy_4.png",
      }},
    -- Heavy ivy
    { idStart = 29, idEnd = 32, name = "heavy_ivy",
      tiles = {
          "assets/textures/world/veg/heavy_ivy_1.png",
          "assets/textures/world/veg/heavy_ivy_2.png",
          "assets/textures/world/veg/heavy_ivy_3.png",
          "assets/textures/world/veg/heavy_ivy_4.png",
      }},
    -- Lichen / tundra
    { idStart = 33, idEnd = 36, name = "lichen",
      tiles = {
          "assets/textures/world/veg/lichen_1.png",
          "assets/textures/world/veg/lichen_2.png",
          "assets/textures/world/veg/lichen_3.png",
          "assets/textures/world/veg/lichen_4.png",
      }},
    -- Desert scrub
    { idStart = 37, idEnd = 40, name = "desert_scrub",
      tiles = {
          "assets/textures/world/veg/desert_scrub_1.png",
          "assets/textures/world/veg/desert_scrub_2.png",
          "assets/textures/world/veg/desert_scrub_3.png",
          "assets/textures/world/veg/desert_scrub_4.png",
      }},
    -- Marsh grass
    { idStart = 41, idEnd = 44, name = "marsh_grass",
      tiles = {
          "assets/textures/world/veg/marsh_grass_1.png",
          "assets/textures/world/veg/marsh_grass_2.png",
          "assets/textures/world/veg/marsh_grass_3.png",
          "assets/textures/world/veg/marsh_grass_4.png",
      }},
    -- Dead grass
    { idStart = 45, idEnd = 48, name = "dead_grass",
      tiles = {
          "assets/textures/world/veg/dead_grass_1.png",
          "assets/textures/world/veg/dead_grass_2.png",
          "assets/textures/world/veg/dead_grass_3.png",
          "assets/textures/world/veg/dead_grass_4.png",
      }},
    -- Fallen leaves
    { idStart = 49, idEnd = 52, name = "fallen_leaves",
      tiles = {
          "assets/textures/world/veg/fallen_leaves_1.png",
          "assets/textures/world/veg/fallen_leaves_2.png",
          "assets/textures/world/veg/fallen_leaves_3.png",
          "assets/textures/world/veg/fallen_leaves_4.png",
      }},
    -- Pine needles
    { idStart = 53, idEnd = 56, name = "pine_needles",
      tiles = {
          "assets/textures/world/veg/pine_needles_1.png",
          "assets/textures/world/veg/pine_needles_2.png",
          "assets/textures/world/veg/pine_needles_3.png",
          "assets/textures/world/veg/pine_needles_4.png",
      }},
    -- Mushroom patch
    { idStart = 57, idEnd = 60, name = "mushroom_patch",
      tiles = {
          "assets/textures/world/veg/mushroom_patch_1.png",
          "assets/textures/world/veg/mushroom_patch_2.png",
          "assets/textures/world/veg/mushroom_patch_3.png",
          "assets/textures/world/veg/mushroom_patch_4.png",
      }},
    -- Wildflowers
    { idStart = 61, idEnd = 64, name = "wildflowers",
      tiles = {
          "assets/textures/world/veg/wildflowers_1.png",
          "assets/textures/world/veg/wildflowers_2.png",
          "assets/textures/world/veg/wildflowers_3.png",
          "assets/textures/world/veg/wildflowers_4.png",
      }},
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
    -- Vegetation facemaps (top face only, no cube sides)
    vegFaceMap              = -1,
    vegSlopeFaceMapN        = -1,
    vegSlopeFaceMapE        = -1,
    vegSlopeFaceMapNE       = -1,
    vegSlopeFaceMapS        = -1,
    vegSlopeFaceMapNS       = -1,
    vegSlopeFaceMapES       = -1,
    vegSlopeFaceMapNES      = -1,
    vegSlopeFaceMapW        = -1,
    vegSlopeFaceMapNW       = -1,
    vegSlopeFaceMapEW       = -1,
    vegSlopeFaceMapNEW      = -1,
    vegSlopeFaceMapSW       = -1,
    vegSlopeFaceMapNSW      = -1,
    vegSlopeFaceMapESW      = -1,
    vegSlopeFaceMapNESW     = -1,
    noFaceMap      = -1,
}

-- Material textures are now loaded from YAML via engine.loadMaterialYaml().
-- The Haskell side parses data/materials/*.yaml, loads every texture,
-- and registers handles by both name ("mat_tile_loam") and numeric id
-- ("mat_tile_56") in the engine's TextureNameRegistry.
--
-- worldView.materialTextureCount is set after the call returns.
worldView.materialTextureCount = 0

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
    st.ocean          = engine.loadTexture("assets/textures/world/zoommap/ocean_chunk.png")
    st.glacier        = engine.loadTexture("assets/textures/world/zoommap/glacier_chunk.png")
    st.lava           = engine.loadTexture("assets/textures/world/zoommap/lava_chunk.png")
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
    -- Vegetation facemaps (top face only — identical slope shapes
    -- but bottom 16 rows are fully transparent)
    st.vegFaceMap              = engine.loadTexture("assets/textures/world/facemap/vegface.png")
    st.vegSlopeFaceMapN        = engine.loadTexture("assets/textures/world/facemap/vegface_slope_n.png")
    st.vegSlopeFaceMapE        = engine.loadTexture("assets/textures/world/facemap/vegface_slope_e.png")
    st.vegSlopeFaceMapNE       = engine.loadTexture("assets/textures/world/facemap/vegface_slope_ne.png")
    st.vegSlopeFaceMapS        = engine.loadTexture("assets/textures/world/facemap/vegface_slope_s.png")
    st.vegSlopeFaceMapNS       = engine.loadTexture("assets/textures/world/facemap/vegface_slope_ns.png")
    st.vegSlopeFaceMapES       = engine.loadTexture("assets/textures/world/facemap/vegface_slope_es.png")
    st.vegSlopeFaceMapNES      = engine.loadTexture("assets/textures/world/facemap/vegface_slope_nes.png")
    st.vegSlopeFaceMapW        = engine.loadTexture("assets/textures/world/facemap/vegface_slope_w.png")
    st.vegSlopeFaceMapNW       = engine.loadTexture("assets/textures/world/facemap/vegface_slope_nw.png")
    st.vegSlopeFaceMapEW       = engine.loadTexture("assets/textures/world/facemap/vegface_slope_ew.png")
    st.vegSlopeFaceMapNEW      = engine.loadTexture("assets/textures/world/facemap/vegface_slope_new.png")
    st.vegSlopeFaceMapSW       = engine.loadTexture("assets/textures/world/facemap/vegface_slope_sw.png")
    st.vegSlopeFaceMapNSW      = engine.loadTexture("assets/textures/world/facemap/vegface_slope_nsw.png")
    st.vegSlopeFaceMapESW      = engine.loadTexture("assets/textures/world/facemap/vegface_slope_esw.png")
    st.vegSlopeFaceMapNESW     = engine.loadTexture("assets/textures/world/facemap/vegface_slope_nesw.png")

    for _, handle in pairs(st) do
        worldView.allHandles[handle] = true
        count = count + 1
    end

    -- Load material textures from the table
    worldView.materialTextures = {}
    -- Load material textures from YAML (parsed + loaded on Haskell side)
    local matCount = engine.loadMaterialYaml("data/materials")
    worldView.materialTextureCount = matCount
    count = count + matCount

    engine.logInfo("Queued " .. matCount .. " material textures from YAML")

    -- Load vegetation textures
    worldView.vegTextures = {}
    for _, def in ipairs(worldView.vegDefs) do
        for i, path in ipairs(def.tiles) do
            local vegId = def.idStart + (i - 1)
            local h = engine.loadTexture(path)
            worldView.vegTextures[vegId] = h
            worldView.allHandles[h] = true
            count = count + 1
        end
    end

    worldView.seenHandles = {}
    worldView.texturesNeeded = count
    worldView.texturesLoadedCount = 0

    engine.logInfo("World view initialized, loading " .. count .. " textures")
end

-----------------------------------------------------------
-- Asset Loading Callback
-----------------------------------------------------------

function worldView.onAssetLoaded(assetType, handle, path)
    if assetType ~= "texture" then return end
    if worldView.seenHandles and worldView.seenHandles[handle] then return end  -- dedup

    -- Accept if it's a tracked structural/veg handle OR any material handle
    -- (material handles aren't in allHandles since YAML loaded them)
    if not worldView.allHandles[handle] then
        -- Not a structural/veg handle. Could be a material handle or
        -- something unrelated. Only count if we still need more textures.
        if worldView.texturesLoadedCount >= worldView.texturesNeeded then
            return  -- already done, ignore stray loads
        end
    end

    worldView.seenHandles[handle] = true
    worldView.texturesLoadedCount = worldView.texturesLoadedCount + 1
    engine.logDebug("World texture loaded: handle=" .. tostring(handle)
        .. " (" .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")

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
        vegTextures = worldView.vegTextures,
        -- materials no longer passed here; sent via sendMaterialTextures
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
    -- Vegetation facemaps
    world.setTexture(worldId, "veg_facemap",              st.vegFaceMap)
    world.setTexture(worldId, "veg_slope_facemap_n",      st.vegSlopeFaceMapN)
    world.setTexture(worldId, "veg_slope_facemap_e",      st.vegSlopeFaceMapE)
    world.setTexture(worldId, "veg_slope_facemap_ne",     st.vegSlopeFaceMapNE)
    world.setTexture(worldId, "veg_slope_facemap_s",      st.vegSlopeFaceMapS)
    world.setTexture(worldId, "veg_slope_facemap_ns",     st.vegSlopeFaceMapNS)
    world.setTexture(worldId, "veg_slope_facemap_es",     st.vegSlopeFaceMapES)
    world.setTexture(worldId, "veg_slope_facemap_nes",    st.vegSlopeFaceMapNES)
    world.setTexture(worldId, "veg_slope_facemap_w",      st.vegSlopeFaceMapW)
    world.setTexture(worldId, "veg_slope_facemap_nw",     st.vegSlopeFaceMapNW)
    world.setTexture(worldId, "veg_slope_facemap_ew",     st.vegSlopeFaceMapEW)
    world.setTexture(worldId, "veg_slope_facemap_new",    st.vegSlopeFaceMapNEW)
    world.setTexture(worldId, "veg_slope_facemap_sw",     st.vegSlopeFaceMapSW)
    world.setTexture(worldId, "veg_slope_facemap_nsw",    st.vegSlopeFaceMapNSW)
    world.setTexture(worldId, "veg_slope_facemap_esw",    st.vegSlopeFaceMapESW)
    world.setTexture(worldId, "veg_slope_facemap_nesw",   st.vegSlopeFaceMapNESW)

    -- Vegetation tiles
    for vegId, handle in pairs(worldView.vegTextures) do
        world.setTexture(worldId, "veg_tile_" .. vegId, handle)
    end

    -- Materials: look up from registry by numeric ID
    local matIds = {
        1, 2, 3, 6, 7, 8,
        10, 11, 12, 13, 14, 15, 16,
        20, 21, 22, 23, 24, 25,
        30, 31, 32, 33, 34, 35,
        40, 41, 42, 43, 44, 45,
        50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
        60, 61, 62, 63, 64, 65, 66, 67,
        70, 71, 72,
        80, 81, 82, 83, 84, 85, 86,
        90, 91,
        100, 101, 102, 103,
        110, 111, 112, 113,
        250, 251, 255,
    }
    for _, matId in ipairs(matIds) do
        local tileH = engine.getTextureHandle("mat_tile_" .. matId)
        local zoomH = engine.getTextureHandle("mat_zoom_" .. matId)
        local bgH   = engine.getTextureHandle("mat_bg_"   .. matId)
        if tileH and tileH >= 0 then world.setTexture(worldId, "mat_tile_" .. matId, tileH) end
        if zoomH and zoomH >= 0 then world.setTexture(worldId, "mat_zoom_" .. matId, zoomH) end
        if bgH   and bgH   >= 0 then world.setTexture(worldId, "mat_bg_"   .. matId, bgH)   end
    end

    engine.logInfo("All textures sent to world: " .. worldId)
    return true
end

return worldView
