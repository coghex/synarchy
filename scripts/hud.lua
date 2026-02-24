-- HUD Overlay Module
-- Displays a toggle group at the bottom-right of the screen
-- for switching between map display modes.
-- Also hosts a tile/chunk info panel in the top-right corner.
local scale     = require("scripts.ui.scale")
local toggle    = require("scripts.ui.toggle")
local infoPanel = require("scripts.hud.info_panel")
local hud = {}

hud.world_page = nil
hud.zoom_page = nil
hud.visible = false
hud.uiCreated = false
hud.fbW = 0
hud.fbH = 0
hud.currentView = "none"  -- "zoomed_in", "zoomed_out", or "none"

hud.mapToggleId = nil

-- Assets passed in from ui_manager
hud.boxTexSet = nil
hud.menuFont  = nil

-- Texture handles (loaded once)
hud.texMapDefault          = nil
hud.texMapDefaultSelected  = nil
hud.texMapTemp             = nil
hud.texMapTempSelected     = nil
hud.texMapSeaTemp          = nil
hud.texMapSeaTempSelected  = nil
hud.texMapPressure         = nil
hud.texMapPressureSelected = nil

hud.texToolDefault         = nil
hud.texToolDefaultSelected = nil
hud.texToolInfo            = nil
hud.texToolInfoSelected    = nil
hud.texToolMine            = nil
hud.texToolMineSelected    = nil

hud.texZoomSelect    = nil
hud.texZoomHover     = nil
hud.texWorldSelect   = nil
hud.texWorldSelectBg = nil
hud.texWorldHover    = nil
hud.texWorldHoverBg  = nil

-- Base sizes (unscaled)
hud.baseSizes = {
    buttonSize = 64,
    padding    = 8,
    margin     = 16,   -- distance from screen edges
}

-----------------------------------------------------------
-- Init (called from ui_manager once assets are ready)
-----------------------------------------------------------

function hud.init(boxTexSet, menuFont, width, height)
    hud.boxTexSet = boxTexSet
    hud.menuFont  = menuFont
    hud.fbW = width
    hud.fbH = height

    -- Load HUD textures
    hud.texMapDefault         = engine.loadTexture("assets/textures/hud/map_default.png")
    hud.texMapDefaultSelected = engine.loadTexture("assets/textures/hud/map_default_selected.png")
    hud.texMapTemp            = engine.loadTexture("assets/textures/hud/map_temp.png")
    hud.texMapTempSelected    = engine.loadTexture("assets/textures/hud/map_temp_selected.png")
    hud.texMapSeaTemp         = engine.loadTexture("assets/textures/hud/map_seatemp.png")
    hud.texMapSeaTempSelected = engine.loadTexture("assets/textures/hud/map_seatemp_selected.png")
    hud.texMapPressure        = engine.loadTexture("assets/textures/hud/map_pressure.png")
    hud.texMapPressureSelected = engine.loadTexture("assets/textures/hud/map_pressure_selected.png")
    hud.texToolDefault         = engine.loadTexture("assets/textures/hud/tool_default.png")
    hud.texToolDefaultSelected = engine.loadTexture("assets/textures/hud/tool_default_selected.png")
    hud.texToolInfo            = engine.loadTexture("assets/textures/hud/tool_info.png")
    hud.texToolInfoSelected    = engine.loadTexture("assets/textures/hud/tool_info_selected.png")
    hud.texToolMine            = engine.loadTexture("assets/textures/hud/tool_mine.png")
    hud.texToolMineSelected    = engine.loadTexture("assets/textures/hud/tool_mine_selected.png")
    hud.texZoomSelect          = engine.loadTexture("assets/textures/hud/utility/zoom_select.png")
    hud.texZoomHover           = engine.loadTexture("assets/textures/hud/utility/zoom_hover.png")
    hud.texWorldSelect         = engine.loadTexture("assets/textures/hud/utility/world_select.png")
    hud.texWorldSelectBg       = engine.loadTexture("assets/textures/hud/utility/world_select_bg.png")
    hud.texWorldHover          = engine.loadTexture("assets/textures/hud/utility/world_hover.png")
    hud.texWorldHoverBg        = engine.loadTexture("assets/textures/hud/utility/world_hover_bg.png")
    engine.logDebug("HUD initialized")
end

-----------------------------------------------------------
-- Create / Rebuild UI
-----------------------------------------------------------

function hud.createUI()
    -- Tear down previous page if resizing
    if hud.uiCreated and hud.world_page and hud.zoom_page then
        UI.deletePage(hud.world_page)
        UI.deletePage(hud.zoom_page)
        if hud.mapToggleId then
            toggle.destroy(hud.mapToggleId)
            hud.mapToggleId = nil
        end
        -- Info panels are destroyed with their pages; clean up module state
        infoPanel.destroyOwned()
    end

    local uiscale = scale.get()
    local s = scale.applyAll(hud.baseSizes)

    hud.world_page = UI.newPage("world_hud_overlay", "overlay")
    hud.zoom_page = UI.newPage("zoom_hud_overlay", "overlay")
    if hud.texZoomSelect and hud.texZoomHover then
        world.setZoomCursorSelectTexture("main_world", hud.texZoomSelect)
        world.setZoomCursorHoverTexture("main_world", hud.texZoomHover)
    end
    if hud.texWorldSelect and hud.texWorldHover
            and hud.texWorldSelectBg and hud.texWorldHoverBg then
        world.setWorldCursorSelectTexture("main_world", hud.texWorldSelect)
        world.setWorldCursorHoverTexture("main_world", hud.texWorldHover)
        world.setWorldCursorSelectBgTexture("main_world", hud.texWorldSelectBg)
        world.setWorldCursorHoverBgTexture("main_world", hud.texWorldHoverBg)
    end

    -- Position: bottom-right, anchored so the right edge of the last
    -- button is (fbW - margin) and the bottom edge is (fbH - margin).
    local anchorX = hud.fbW - s.margin
    local anchorY = hud.fbH - s.margin - s.buttonSize

    -- Items are laid out left-to-right; the *last* item sits at the
    -- rightmost position.  So mapTemp is first (left), mapDefault is
    -- second (right).
    hud.mapToggleId = toggle.new({
        name = "map_mode_toggle",
        page = hud.zoom_page,
        items = {
            {
                name        = "map_temp",
                texDefault  = hud.texMapTemp,
                texSelected = hud.texMapTempSelected,
                options = {
                    { name = "map_pressure",
                      texDefault = hud.texMapPressure,
                      texSelected = hud.texMapPressureSelected },
                    { name = "map_seatemp",
                      texDefault = hud.texMapSeaTemp,
                      texSelected = hud.texMapSeaTempSelected },
                },
            },
            {
                name        = "map_default",
                texDefault  = hud.texMapDefault,
                texSelected = hud.texMapDefaultSelected,
            },
        },
        selectedIndex = 2,   -- mapDefault selected by default
        direction = "left",
        optionsDirection = "up",
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

    local toolAnchorX = s.margin
    local toolAnchorY = hud.fbH - s.margin - s.buttonSize

    hud.mapToggleId = toggle.new({
        name = "tool_mode_toggle",
        page = hud.world_page,
        items = {
            {
                name        = "tool_mine",
                texDefault  = hud.texToolMine,
                texSelected = hud.texToolMineSelected,
            },
            {
                name        = "tool_default",
                texDefault  = hud.texToolDefault,
                texSelected = hud.texToolDefaultSelected,
                options = {
                    { name = "tool_info",
                      texDefault = hud.texToolInfo,
                      texSelected = hud.texToolInfoSelected },
                },
            },
        },
        selectedIndex = 2,   -- toolDefault selected by default
        direction = "up",
        optionsDirection = "right",
        size    = hud.baseSizes.buttonSize,
        padding = hud.baseSizes.padding,
        x       = toolAnchorX,
        y       = toolAnchorY,
        zIndex  = 100,
        uiscale = uiscale,
        onChange = function(index, itemName)
            world.setToolMode("main_world", itemName)
            engine.logDebug("Tool mode changed to: " .. tostring(itemName))
        end,
    })

    ---------------------------------------------------------
    -- Info panels (one instance per page)
    -- We create the info panel on the world_page; on zoom
    -- transitions we hide/show it as needed.
    -- Both pages share the same infoPanel module state, but
    -- the panel elements live on world_page.  We recreate
    -- onto whichever page is active, but for simplicity we
    -- create it on the world_page and manually manage
    -- visibility during zoom transitions.
    ---------------------------------------------------------
    infoPanel.create({
        page      = hud.world_page,
        boxTexSet = hud.boxTexSet,
        menuFont  = hud.menuFont,
        fbW       = hud.fbW,
        fbH       = hud.fbH,
    })

    local zoom = camera.getZoom()
    local zoomFadeStart = camera.getZoomFadeStart()
    local zoomFadeEnd = camera.getZoomFadeEnd()
    if zoom > zoomFadeEnd then
        UI.showPage(hud.zoom_page)
        hud.currentView = "zoomed_out"
    elseif zoom < zoomFadeStart then
        UI.showPage(hud.world_page)
        hud.currentView = "zoomed_in"
    else
        hud.currentView = "none"
    end

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

    if hud.currentView == "zoomed_in" and hud.world_page then
        UI.showPage(hud.world_page)
    elseif hud.currentView == "zoomed_out" and hud.zoom_page then
        UI.showPage(hud.zoom_page)
    end

    engine.logDebug("HUD shown")
end

function hud.hide()
    hud.visible = false

    if hud.world_page then
        UI.hidePage(hud.world_page)
    end
    if hud.zoom_page then
        UI.hidePage(hud.zoom_page)
    end

    engine.logDebug("HUD hidden")
end

-- Add to the same onMouseDown, or a separate check:
function hud.onMouseDown(button_num, mx, my)
    if hud.currentView == "zoomed_out" then
        if button_num == 1 then
            world.setZoomCursorSelect("main_world")
        elseif button_num == 2 then
            world.clearZoomCursorSelect("main_world")
        end
    elseif hud.currentView == "zoomed_in" then
        if button_num == 1 then
            world.setWorldCursorSelect("main_world")
        elseif button_num == 2 then
            world.clearWorldCursorSelect("main_world")
        end
    end
end

-----------------------------------------------------------
-- manage hud (zoom transitions)
-----------------------------------------------------------

function hud.onScroll(dx, dy)
    local zoom = camera.getZoom()
    local zoomFadeStart = camera.getZoomFadeStart()
    local zoomFadeEnd = camera.getZoomFadeEnd()
    local oldView = hud.currentView
    if zoom > zoomFadeEnd then
        if oldView ~= "zoomed_out" and hud.zoom_page and hud.world_page then
            UI.showPage(hud.zoom_page)
            UI.hidePage(hud.world_page)
            hud.currentView = "zoomed_out"
            -- Hide the info panel during zoom transition;
            -- it will re-appear when new info is sent.
            infoPanel.clear()
        end
    elseif zoom < zoomFadeStart then
        if oldView ~= "zoomed_in" and hud.zoom_page and hud.world_page then
            UI.showPage(hud.world_page)
            UI.hidePage(hud.zoom_page)
            hud.currentView = "zoomed_in"
            -- Hide the info panel during zoom transition;
            -- it will re-appear when new info is sent.
            infoPanel.clear()
        end
    else
        if oldView == "zoomed_in" and hud.world_page then
            UI.hidePage(hud.world_page)
        elseif oldView == "zoomed_out" and hud.zoom_page then
            UI.hidePage(hud.zoom_page)
        end
        hud.currentView = "none"
        -- In the fade zone, hide the info panel
        infoPanel.clear()
    end
end

-----------------------------------------------------------
-- manage mouse hover
-----------------------------------------------------------

function hud.update(dt)
    local mx, my = engine.getMousePosition()
    if mx and my then
        if hud.currentView == "zoomed_out" then
            world.setZoomCursorHover("main_world", mx, my)
        elseif hud.currentView == "zoomed_in" then
            world.setWorldCursorHover("main_world", mx, my)
        end
    end
end

-----------------------------------------------------------
-- Info Panel Public API
-- Call these from Haskell (via ui_manager) to populate
-- the tile/chunk info panel.
-----------------------------------------------------------

-- Set text for the "basic" tab
function hud.setInfoBasic(text)
    infoPanel.setText("basic", text)
end

-- Set text for the "advanced" tab
function hud.setInfoAdvanced(text)
    infoPanel.setText("advanced", text)
end

-- Set both tabs at once
function hud.setInfoText(basicText, advancedText)
    infoPanel.setInfo(basicText, advancedText)
end

-- Clear all info (hides the panel)
function hud.clearInfo()
    infoPanel.clear()
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
            if hud.currentView == "zoomed_in" and hud.world_page then
                UI.showPage(hud.world_page)
            elseif hud.currentView == "zoomed_out" and hud.zoom_page then
                UI.showPage(hud.zoom_page)
            end
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function hud.shutdown()
    infoPanel.destroyOwned()
    if hud.mapToggleId then
        toggle.destroy(hud.mapToggleId)
        hud.mapToggleId = nil
    end
    if hud.zoom_page then
        UI.hidePage(hud.zoom_page)
        UI.deletePage(hud.zoom_page)
        hud.zoom_page = nil
    end
    if hud.world_page then
        UI.hidePage(hud.world_page)
        UI.deletePage(hud.world_page)
        hud.world_page = nil
    end
end

return hud
