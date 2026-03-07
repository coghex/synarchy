-- Test Arena - flat loam surface for testing units, animations, etc.
-- Accessed via shell: testArena_open()
-- Or via main menu: "Test Arena" button
local worldView = require("scripts.world_view")

local testArena = {}

testArena.page = nil
testArena.visible = false
testArena.showMenuCallback = nil
testArena.arenaWorldId = "test_arena"
testArena.created = false
testArena.fbW = 0
testArena.fbH = 0
testArena.vegetationTextureCount = 0

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function testArena.setShowMenuCallback(cb)
    testArena.showMenuCallback = cb
end

-----------------------------------------------------------
-- Init (called once from ui_manager.checkReady)
-----------------------------------------------------------

function testArena.init(fbW, fbH)
    testArena.fbW = fbW
    testArena.fbH = fbH

    local count = 0
    -- Load vegetation from YAML
    local vegetationLoader = require("scripts.vegetation_loader")
    local vegCount = vegetationLoader.loadAll("data/vegetation")
    testArena.vegetationTextureCount = vegCount
    count = count + vegCount
end

-----------------------------------------------------------
-- Create UI page
-----------------------------------------------------------

function testArena.createUI()
    if testArena.page then
        UI.deletePage(testArena.page)
    end
    testArena.page = UI.newPage("test_arena", "hud")
end

-----------------------------------------------------------
-- Send textures to the arena world
--
-- We need at minimum:
--   - notexture, blank (structural fallbacks)
--   - iso_facemap, nofacemap (facemap rendering)
--   - mat_tile_56 (loam tile texture)
--
-- We also send slope facemaps so the render pipeline
-- doesn't hit TextureHandle 0 for any lookup.
-----------------------------------------------------------

function testArena.sendTextures(worldId)
    local st = worldView.structuralTextures

    local function validHandle(h)
        return h and h ~= -1
    end

    -- If worldView hasn't loaded textures yet, load the essentials ourselves
    local noTex    = validHandle(st.noTexture) and st.noTexture
                     or engine.loadTexture("assets/textures/world/notexture.png")
    local blankTex = validHandle(st.blankTexture) and st.blankTexture
                     or engine.loadTexture("assets/textures/world/blanktexture.png")
    local isoFM    = validHandle(st.isoFaceMap) and st.isoFaceMap
                     or engine.loadTexture("assets/textures/world/facemap/isoface.png")
    local vegFM    = validHandle(st.vegFaceMap) and st.vegFaceMap
                     or engine.loadTexture("assets/textures/world/facemap/vegface.png")
    local noFM     = validHandle(st.noFaceMap) and st.noFaceMap
                     or engine.loadTexture("assets/textures/world/facemap/noface.png")

    world.setTexture(worldId, "notexture",   noTex)
    world.setTexture(worldId, "blank",       blankTex)
    world.setTexture(worldId, "iso_facemap", isoFM)
    world.setTexture(worldId, "veg_facemap", vegFM)
    world.setTexture(worldId, "nofacemap",   noFM)

    -- Slope facemaps (send whatever worldView has, skip if not loaded)
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
        if validHandle(st[field]) then
            world.setTexture(worldId, "iso_slope_facemap_" .. slopeNames[i], st[field])
        end
    end

    -- Loam tile texture (material ID 56) — look up from registry
    local loamHandle = engine.getTextureHandle("mat_tile_56")
    if loamHandle and loamHandle >= 0 then
        world.setTexture(worldId, "mat_tile_56", loamHandle)
    else
        -- Fallback: load directly if YAML hasn't been processed yet
        loamHandle = engine.loadTexture("assets/textures/world/loam/loam.png")
        world.setTexture(worldId, "mat_tile_56", loamHandle)
    end
    for vegId = 1, 64 do
        local h = engine.getTextureHandle("veg_tile_" .. vegId)
        if h and h >= 0 then
            world.setTexture(worldId, "veg_tile_" .. vegId, h)
        end
    end
end

-----------------------------------------------------------
-- Create the arena world
-----------------------------------------------------------

function testArena.createArenaWorld()
    if testArena.created then return end

    engine.logInfo("Creating test arena world...")

    -- Queue the WorldInitArena command on the world thread.
    -- This creates a WorldState with 25 flat loam chunks
    -- and marks it LoadDone immediately.
    world.initArena(testArena.arenaWorldId)

    -- Send textures so the tiles render correctly.
    -- These are queued as WorldSetTexture commands and will
    -- be processed after WorldInitArena creates the WorldState.
    testArena.sendTextures(testArena.arenaWorldId)

    -- Queue the "done" sentinel — the world thread processes these
    -- sequentially, so by the time it reaches this command, all
    -- textures above have been written into the WorldState.
    world.initArenaDone(testArena.arenaWorldId)

    testArena.created = true
    engine.logInfo("Test arena world created")
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function testArena.show()
    if not testArena.page then
        testArena.createUI()
    end

    testArena.createArenaWorld()

    -- Show the page but mark as "waiting for textures"
    testArena.visible = true
    testArena.ready = false
    UI.showPage(testArena.page)
    -- World won't be in wmVisible yet,
    -- so nothing renders — just blank + HUD
end

function testArena.hide()
    testArena.visible = false
    if testArena.page then UI.hidePage(testArena.page) end
    if testArena.created then
        world.hide(testArena.arenaWorldId)
    end
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function testArena.update(dt)
    if not testArena.visible then return end
    -- Future: unit manager ticks, animation updates, etc.
end

-----------------------------------------------------------
-- Input forwarding (reuses world_view's camera controls)
-----------------------------------------------------------

function testArena.onScroll(dx, dy)
    if not testArena.visible then return end
    -- Forward to the same zoom logic world_view uses
    local zoom = camera.getZoom()
    local newZoom = zoom - dy * 0.05
    newZoom = math.max(0.1, math.min(1.5, newZoom))
    camera.setZoom(newZoom)
end

function testArena.onZSliceScroll(dx, dy)
    if not testArena.visible then return end
    local zSlice = camera.getZSlice()
    local newZ = zSlice + math.floor(dy)
    camera.setZSlice(newZ)
end

function testArena.onKeyDown(key)
    if not testArena.visible then return end
    -- Escape returns to main menu
    if key == "Escape" then
        if testArena.showMenuCallback then
            testArena.showMenuCallback("main")
        end
        return
    end
    -- Camera rotation (same as world_view)
    if key == "Q" then
        camera.rotateCCW()
    elseif key == "E" then
        camera.rotateCW()
    end
end

function testArena.onKeyUp(key)
    -- nothing for now
end

-----------------------------------------------------------
-- Asset loaded callback (forwarded from ui_manager)
-----------------------------------------------------------

function testArena.onAssetLoaded(assetType, handle, path)
    -- nothing for now
end

-----------------------------------------------------------
-- Framebuffer resize
-----------------------------------------------------------

function testArena.onFramebufferResize(width, height)
    testArena.fbW = width
    testArena.fbH = height
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function testArena.shutdown()
    if testArena.created then
        world.destroy(testArena.arenaWorldId)
        testArena.created = false
    end
    if testArena.page then
        UI.deletePage(testArena.page)
        testArena.page = nil
    end
end

return testArena
