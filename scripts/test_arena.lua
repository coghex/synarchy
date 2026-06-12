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

-- Signature mirrors loading_screen.init / pause_menu.init shape:
-- (boxTex, font, titleFont, fbW, fbH). The texture / font args are
-- stored even though the arena doesn't currently render menu chrome
-- — keeps the surface consistent so a future overlay can pull them
-- from the module without re-plumbing ui_manager.
function testArena.init(boxTex, font, titleFont, fbW, fbH)
    testArena.boxTexSet = boxTex
    testArena.menuFont  = font
    testArena.titleFont = titleFont
    testArena.fbW       = fbW
    testArena.fbH       = fbH

    -- Materials and vegetation YAMLs are loaded by
    -- scripts/startup_loader.lua during the boot loading screen, so
    -- this init no longer re-runs them. Re-running them would
    -- duplicate registry entries (the YAML loaders are not idempotent).
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

    -- Granite tile texture (material ID 1) — revealed when the player
    -- digs through the top 4 loam tiles.
    local graniteHandle = engine.getTextureHandle("mat_tile_1")
    if graniteHandle and graniteHandle >= 0 then
        world.setTexture(worldId, "mat_tile_1", graniteHandle)
    else
        graniteHandle = engine.loadTexture("assets/textures/world/granite/granite.png")
        world.setTexture(worldId, "mat_tile_1", graniteHandle)
    end

    -- Fluid surface textures. The freshwater render path
    -- (Lake/River, via freshwaterTileToQuad) and the ocean path both
    -- look up `mat_tile_255` (matOcean). Lava (lavaTileToQuad) looks
    -- up `mat_tile_100` (matLava). Without these the WorldSetFluidTile
    -- debug tool succeeds but produces no visible water surface — the
    -- renderer falls back to wtNoTexture and the quad blends into the
    -- terrain below.
    local oceanHandle = engine.getTextureHandle("mat_tile_255")
    if oceanHandle and oceanHandle >= 0 then
        world.setTexture(worldId, "mat_tile_255", oceanHandle)
    else
        oceanHandle = engine.loadTexture("assets/textures/world/ocean/ocean.png")
        world.setTexture(worldId, "mat_tile_255", oceanHandle)
    end

    local lavaHandle = engine.getTextureHandle("mat_tile_100")
    if lavaHandle and lavaHandle >= 0 then
        world.setTexture(worldId, "mat_tile_100", lavaHandle)
    else
        lavaHandle = engine.loadTexture("assets/textures/world/lava/lava.png")
        world.setTexture(worldId, "mat_tile_100", lavaHandle)
    end
    -- 17 types × 4 variants = IDs 1..68 (keep in sync with
    -- world_view.sendTexturesToWorld / world_manager.sendVegTextures).
    for vegId = 1, 68 do
        local h = engine.getTextureHandle("veg_tile_" .. vegId)
        if h and h >= 0 then
            world.setTexture(worldId, "veg_tile_" .. vegId, h)
        end
    end

    -- ALL registered material tile textures, not just the curated
    -- set above. Debug terrain placement and dig yields (spoil
    -- piles → heavy_gravel, etc.) can put ANY material on screen in
    -- the arena; a material missing from the page's texture table
    -- renders with a garbage fallback slot (the cursor texture, in
    -- practice). The loam/granite direct-load fallbacks above still
    -- cover the YAML-not-yet-loaded edge case.
    local mats = world.listMaterials and world.listMaterials() or {}
    for _, m in ipairs(mats) do
        local h = engine.getTextureHandle("mat_tile_" .. m.id)
        if h and h >= 0 then
            world.setTexture(worldId, "mat_tile_" .. m.id, h)
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

    -- Note: tile-editor arena-mode is armed by ui_manager's
    -- test_arena_view branch (after this loading screen finishes).
    -- Arming here would be wiped by the mid-transition hide().
end

function testArena.hide()
    testArena.visible = false
    if testArena.page then UI.hidePage(testArena.page) end
    if testArena.created then
        world.hide(testArena.arenaWorldId)
    end
    require("scripts.tile_editor").setArenaActive(false)
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
    -- Same velocity-based zoom as world_view.onScroll. The old direct
    -- setZoom clamped to [0.1, 1.5], which predates the current zoom
    -- scale (world_view boots at zoom 64) — one scroll notch would
    -- have snapped the camera to 1.5.
    local current = camera.getZoomVelocity()
    local zoom = camera.getZoom()
    local impulse = 0.4 * zoom
    if dy > 0 then
        camera.setZoomVelocity(current - impulse)
    elseif dy < 0 then
        camera.setZoomVelocity(current + impulse)
    end
end

function testArena.onZSliceScroll(dx, dy)
    if not testArena.visible then return end
    -- Match world_view: manual z-slice control disables auto tracking
    -- (Home re-enables it there; arena has no rebind, F8 debug only).
    camera.setZTracking(false)
    local zSlice = camera.getZSlice()
    if dy > 0 then
        camera.setZSlice(zSlice + 1)
    elseif dy < 0 then
        camera.setZSlice(zSlice - 1)
    end
end

function testArena.onKeyDown(key)
    if not testArena.visible then return end
    -- Camera rotation (same as world_view)
    if key == "Q" then
        camera.rotateCCW()
    elseif key == "E" then
        camera.rotateCW()
    elseif key == "Home" then
        -- Parity with world_view: re-enable z-slice auto tracking
        -- after manual slice scrolling. Without this, anything
        -- placed ABOVE a pinned slice (debug terrain placement!)
        -- is silently cut away — "it renders as nothing".
        camera.setZTracking(true)
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
