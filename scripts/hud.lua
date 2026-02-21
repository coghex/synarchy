-- HUD Overlay Module
-- Displays a toggle group at the bottom-right of the screen
-- for switching between map display modes.
local scale = require("scripts.ui.scale")
local toggle = require("scripts.ui.toggle")
local hud = {}

hud.page = nil
hud.visible = false
hud.uiCreated = false
hud.fbW = 0
hud.fbH = 0

hud.mapToggleId = nil

-- Texture handles (loaded once)
hud.texMapDefault         = nil
hud.texMapDefaultSelected = nil
hud.texMapTemp            = nil
hud.texMapTempSelected    = nil

-- Base sizes (unscaled)
hud.baseSizes = {
    buttonSize = 64,
    padding    = 8,
    margin     = 16,   -- distance from screen edges
}

-----------------------------------------------------------
-- Init (called from ui_manager once assets are ready)
-----------------------------------------------------------

function hud.init(width, height)
    hud.fbW = width
    hud.fbH = height

    -- Load HUD textures
    hud.texMapDefault         = engine.loadTexture("assets/textures/hud/map_default.png")
    hud.texMapDefaultSelected = engine.loadTexture("assets/textures/hud/map_default_selected.png")
    hud.texMapTemp            = engine.loadTexture("assets/textures/hud/map_temp.png")
    hud.texMapTempSelected    = engine.loadTexture("assets/textures/hud/map_temp_selected.png")

    engine.logDebug("HUD initialized")
end

-----------------------------------------------------------
-- Create / Rebuild UI
-----------------------------------------------------------

function hud.createUI()
    -- Tear down previous page if resizing
    if hud.uiCreated and hud.page then
        UI.deletePage(hud.page)
        if hud.mapToggleId then
            toggle.destroy(hud.mapToggleId)
            hud.mapToggleId = nil
        end
    end

    local uiscale = scale.get()
    local s = scale.applyAll(hud.baseSizes)

    hud.page = UI.newPage("hud_overlay", "overlay")

    -- Position: bottom-right, anchored so the right edge of the last
    -- button is (fbW - margin) and the bottom edge is (fbH - margin).
    local anchorX = hud.fbW - s.margin
    local anchorY = hud.fbH - s.margin - s.buttonSize

    -- Items are laid out left-to-right; the *last* item sits at the
    -- rightmost position.  So mapTemp is first (left), mapDefault is
    -- second (right).
    hud.mapToggleId = toggle.new({
        name = "map_mode_toggle",
        page = hud.page,
        items = {
            {
                name        = "map_temp",
                texDefault  = hud.texMapTemp,
                texSelected = hud.texMapTempSelected,
            },
            {
                name        = "map_default",
                texDefault  = hud.texMapDefault,
                texSelected = hud.texMapDefaultSelected,
            },
        },
        selectedIndex = 2,   -- mapDefault selected by default
        size    = hud.baseSizes.buttonSize,
        padding = hud.baseSizes.padding,
        x       = anchorX,
        y       = anchorY,
        zIndex  = 100,
        uiscale = uiscale,
        onChange = function(index, itemName)
            world.setMapMode("main_world", itemName)
            engine.logDebug("Map mode changed to: " .. tostring(itemName))
        end,
    })

    hud.uiCreated = true
    engine.logDebug("HUD UI created")
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function hud.show()
    if not hud.uiCreated then
        hud.createUI()
    end

    hud.visible = true

    if hud.page then
        UI.showPage(hud.page)
    end

    engine.logDebug("HUD shown")
end

function hud.hide()
    hud.visible = false

    if hud.page then
        UI.hidePage(hud.page)
    end

    engine.logDebug("HUD hidden")
end

-----------------------------------------------------------
-- Resize
-----------------------------------------------------------

function hud.onFramebufferResize(width, height)
    hud.fbW = width
    hud.fbH = height

    if hud.uiCreated then
        hud.createUI()
        if hud.visible then
            UI.showPage(hud.page)
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function hud.shutdown()
    if hud.mapToggleId then
        toggle.destroy(hud.mapToggleId)
        hud.mapToggleId = nil
    end
    if hud.page then
        UI.hidePage(hud.page)
        UI.deletePage(hud.page)
        hud.page = nil
    end
end

return hud
