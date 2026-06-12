-- HUD Overlay Module
-- Displays a toggle group at the bottom-right of the screen
-- for switching between map display modes.
-- Also hosts a tile/chunk info panel in the top-right corner.
local scale     = require("scripts.ui.scale")
local toggle    = require("scripts.ui.toggle")
local infoPanel = require("scripts.hud.info_panel")
local hud = {}

-- The log icon is a 1-item toggle group (same widget the map/tool
-- icons use). Right-click expands an option strip with the other
-- log; clicking the option swaps the log mode and opens that panel.
-- Left-click on the slot toggles the active log's panel open/closed.

hud.world_page  = nil
hud.zoom_page   = nil
hud.info_page   = nil   -- dedicated page for the info panel
hud.global_page = nil   -- always-on gameplay UI (e.g. event-log button)
hud.visible = false
hud.uiCreated = false
hud.fbW = 0
hud.fbH = 0
hud.currentView = "none"  -- "zoomed_in", "zoomed_out", or "none"
hud.worldId = "main_world" -- default overridden per-context

hud.mapToggleId  = nil   -- map-mode toggle (bottom-right, zoom_page)
hud.toolToggleId = nil   -- tool-mode toggle (bottom-left, world_page)

-- Log icon (top-left HUD button). The slot icon shows the current
-- log mode; expanded option strip shows the alternate mode.
hud.logToggleId       = nil    -- toggle group handle
hud.logMode           = "event" -- "event" or "combat" — kept in sync
                                -- with the toggle's slot item name.
hud.texEventLog          = nil  -- option-strip (unselected) texture
hud.texEventLogSelected  = nil  -- slot (selected) texture
hud.texCombatLog         = nil
hud.texCombatLogSelected = nil

-- Assets passed in from ui_manager
hud.boxTexSet = nil
hud.menuFont  = nil

-- Texture handles (loaded once)
hud.texMapDefault               = nil
hud.texMapDefaultSelected       = nil
hud.texMapTemp                  = nil
hud.texMapTempSelected          = nil
hud.texMapSeaTemp               = nil
hud.texMapSeaTempSelected       = nil
hud.texMapPressure              = nil
hud.texMapPressureSelected      = nil
hud.texMapHumidity              = nil
hud.texMapHumiditySelected      = nil
hud.texMapPrecipitation         = nil
hud.texMapPrecipitationSelected = nil
hud.texMapPrecipType            = nil
hud.texMapPrecipTypeSelected    = nil
hud.texMapEvaporation           = nil
hud.texMapEvaporationSelected   = nil

hud.texToolDefault         = nil
hud.texToolDefaultSelected = nil
hud.texToolInfo            = nil
hud.texToolInfoSelected    = nil
hud.texToolMine            = nil
hud.texToolMineSelected    = nil

hud.texZoomSelect    = nil
hud.texZoomHover     = nil
hud.texWorldSelect   = nil
hud.texMineDesignate = nil
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
    hud.texMapHumidity        = engine.loadTexture("assets/textures/hud/map_humidity.png")
    hud.texMapHumiditySelected = engine.loadTexture("assets/textures/hud/map_humidity_selected.png")
    hud.texMapPrecipitation        = engine.loadTexture("assets/textures/hud/map_precipitation.png")
    hud.texMapPrecipitationSelected = engine.loadTexture("assets/textures/hud/map_precipitation_selected.png")
    hud.texMapPrecipType       = engine.loadTexture("assets/textures/hud/map_preciptype.png")
    hud.texMapPrecipTypeSelected = engine.loadTexture("assets/textures/hud/map_preciptype_selected.png")
    hud.texMapEvaporation       = engine.loadTexture("assets/textures/hud/map_evaporation.png")
    hud.texMapEvaporationSelected = engine.loadTexture("assets/textures/hud/map_evaporation_selected.png")
    hud.texToolDefault         = engine.loadTexture("assets/textures/hud/tool_default.png")
    hud.texToolDefaultSelected = engine.loadTexture("assets/textures/hud/tool_default_selected.png")
    hud.texToolInfo            = engine.loadTexture("assets/textures/hud/tool_info.png")
    hud.texToolInfoSelected    = engine.loadTexture("assets/textures/hud/tool_info_selected.png")
    hud.texToolMine            = engine.loadTexture("assets/textures/hud/tool_mine.png")
    hud.texToolMineSelected    = engine.loadTexture("assets/textures/hud/tool_mine_selected.png")
    hud.texToolBuild           = engine.loadTexture("assets/textures/hud/tool_build.png")
    hud.texToolBuildSelected   = engine.loadTexture("assets/textures/hud/tool_build_selected.png")
    hud.texZoomSelect          = engine.loadTexture("assets/textures/hud/utility/zoom_select.png")
    hud.texZoomHover           = engine.loadTexture("assets/textures/hud/utility/zoom_hover.png")
    hud.texWorldSelect         = engine.loadTexture("assets/textures/hud/utility/world_select.png")
    hud.texWorldSelectBg       = engine.loadTexture("assets/textures/hud/utility/world_select_bg.png")
    hud.texWorldHover          = engine.loadTexture("assets/textures/hud/utility/world_hover.png")
    hud.texWorldHoverBg        = engine.loadTexture("assets/textures/hud/utility/world_hover_bg.png")
    -- Mine-designation marker: dedicated art so standing designations
    -- read differently from the cursor/selection at a glance.
    hud.texMineDesignate       = engine.loadTexture("assets/textures/hud/utility/mine_designate.png")

    -- Event-log toggle (top-left). Two states: default and selected
    -- (drawn while the event log panel is open). The combat-log
    -- variants are loaded here too so the step-5 icon swap (right-
    -- click picker) can flip between them without re-loading.
    hud.texEventLog         = engine.loadTexture("assets/textures/hud/event_log.png")
    hud.texEventLogSelected = engine.loadTexture("assets/textures/hud/event_log_selected.png")
    hud.texCombatLog         = engine.loadTexture("assets/textures/hud/combat_log.png")
    hud.texCombatLogSelected = engine.loadTexture("assets/textures/hud/combat_log_selected.png")
    engine.logDebug("HUD initialized")
end

-----------------------------------------------------------
-- Create / Rebuild UI
-----------------------------------------------------------

function hud.createUI()
    -- Tear down previous pages if resizing
    if hud.uiCreated and hud.world_page and hud.zoom_page then
        UI.deletePage(hud.world_page)
        UI.deletePage(hud.zoom_page)
        if hud.mapToggleId then
            toggle.destroy(hud.mapToggleId)
            hud.mapToggleId = nil
        end
        if hud.toolToggleId then
            toggle.destroy(hud.toolToggleId)
            hud.toolToggleId = nil
        end
    end
    if hud.uiCreated and hud.info_page then
        -- info panel elements live on this page; destroy module state
        infoPanel.destroyOwned()
        UI.deletePage(hud.info_page)
    end
    if hud.uiCreated and hud.global_page then
        if hud.logToggleId then
            toggle.destroy(hud.logToggleId)
            hud.logToggleId = nil
        end
        UI.deletePage(hud.global_page)
    end

    local uiscale = scale.get()
    local s = scale.applyAll(hud.baseSizes)

    hud.world_page  = UI.newPage("world_hud_overlay", "overlay")
    hud.zoom_page   = UI.newPage("zoom_hud_overlay", "overlay")
    hud.info_page   = UI.newPage("hud_info_overlay", "overlay")
    hud.global_page = UI.newPage("hud_global_overlay", "overlay")

    ---------------------------------------------------------
    -- Log-mode toggle (top-left). 1-item toggle group with the
    -- alternate log in `options`, mirroring the map/tool toggle
    -- pattern: right-click expands the option strip, click the
    -- option to swap which log is in the slot. Left-click on the
    -- slot toggles that log's panel open/closed.
    ---------------------------------------------------------
    local function logItem(mode)
        if mode == "combat" then
            return {
                name        = "combat",
                texDefault  = hud.texCombatLog,
                texSelected = hud.texCombatLogSelected,
                tooltip     = "Combat log",
            }
        end
        return {
            name        = "event",
            texDefault  = hud.texEventLog,
            texSelected = hud.texEventLogSelected,
            tooltip     = "Event log",
        }
    end
    local otherMode = (hud.logMode == "combat") and "event" or "combat"

    hud.logToggleId = toggle.new({
        name = "log_mode_toggle",
        page = hud.global_page,
        items = {
            (function()
                local it = logItem(hud.logMode)
                it.options = { logItem(otherMode) }
                return it
            end)(),
        },
        selectedIndex    = 1,
        direction        = "right",
        optionsDirection = "right",
        size    = hud.baseSizes.buttonSize,
        padding = hud.baseSizes.padding,
        x       = s.margin,
        y       = s.margin,
        zIndex  = 100,
        uiscale = uiscale,
        onChange = function(_idx, itemName)
            if itemName == hud.logMode then
                -- Same-mode click: toggle that log's panel.
                if itemName == "combat" then
                    require("scripts.combat_log").toggle()
                else
                    require("scripts.event_log").toggle()
                end
            else
                -- Option-swap: switch modes and open the new panel.
                hud.setLogMode(itemName)
            end
        end,
    })

    if hud.texZoomSelect and hud.texZoomHover then
        world.setZoomCursorSelectTexture(hud.worldId, hud.texZoomSelect)
        world.setZoomCursorHoverTexture(hud.worldId, hud.texZoomHover)
    end
    if hud.texWorldSelect and hud.texWorldHover
            and hud.texWorldSelectBg and hud.texWorldHoverBg then
        world.setWorldCursorSelectTexture(hud.worldId, hud.texWorldSelect)
        world.setWorldCursorHoverTexture(hud.worldId, hud.texWorldHover)
        world.setWorldCursorSelectBgTexture(hud.worldId, hud.texWorldSelectBg)
        world.setWorldCursorHoverBgTexture(hud.worldId, hud.texWorldHoverBg)
    end
    if hud.texMineDesignate then
        world.setMineDesignateTexture(hud.worldId, hud.texMineDesignate)
    end

    -- Position: bottom-right, anchored so the right edge of the last
    -- button is (fbW - margin) and the bottom edge is (fbH - margin).
    local anchorX = hud.fbW - s.margin
    local anchorY = hud.fbH - s.margin - s.buttonSize

    hud.mapToggleId = toggle.new({
        name = "map_mode_toggle",
        page = hud.zoom_page,
        items = {
            {
                name        = "map_temp",
                texDefault  = hud.texMapTemp,
                texSelected = hud.texMapTempSelected,
                tooltip     = "Temperature map",
                options = {
                    { name        = "map_pressure",
                      texDefault  = hud.texMapPressure,
                      texSelected = hud.texMapPressureSelected,
                      tooltip     = "Pressure map" },
                    { name        = "map_humidity",
                      texDefault  = hud.texMapHumidity,
                      texSelected = hud.texMapHumiditySelected,
                      tooltip     = "Humidity map" },
                    { name        = "map_precipitation",
                      texDefault  = hud.texMapPrecipitation,
                      texSelected = hud.texMapPrecipitationSelected,
                      tooltip     = "Precipitation map" },
                    { name        = "map_preciptype",
                      texDefault  = hud.texMapPrecipType,
                      texSelected = hud.texMapPrecipTypeSelected,
                      tooltip     = "Precipitation type map" },
                    { name        = "map_evaporation",
                      texDefault  = hud.texMapEvaporation,
                      texSelected = hud.texMapEvaporationSelected,
                      tooltip     = "Evaporation map" },
                    { name        = "map_seatemp",
                      texDefault  = hud.texMapSeaTemp,
                      texSelected = hud.texMapSeaTempSelected,
                      tooltip     = "Sea Temperature map" },
                },
            },
            {
                name        = "map_default",
                texDefault  = hud.texMapDefault,
                texSelected = hud.texMapDefaultSelected,
                tooltip     = "Default view",
            },
        },
        selectedIndex = 2,
        direction = "left",
        optionsDirection = "up",
        size    = hud.baseSizes.buttonSize,
        padding = hud.baseSizes.padding,
        x       = anchorX,
        y       = anchorY,
        zIndex  = 100,
        uiscale = uiscale,
        onChange = function(index, itemName)
            world.setMapMode(hud.worldId, itemName)
            engine.logDebug("Map mode changed to: " .. tostring(itemName))
        end,
    })

    local toolAnchorX = s.margin
    local toolAnchorY = hud.fbH - s.margin - s.buttonSize

    hud.toolToggleId = toggle.new({
        name = "tool_mode_toggle",
        page = hud.world_page,
        items = {
            -- Stacked bottom-up: mine above build above default/info
            -- (direction = "up" puts earlier items higher).
            {
                name        = "tool_mine",
                texDefault  = hud.texToolMine,
                texSelected = hud.texToolMineSelected,
                tooltip     = "Mine tool",
            },
            {
                name        = "tool_build",
                texDefault  = hud.texToolBuild,
                texSelected = hud.texToolBuildSelected,
                tooltip     = "Build tool",
            },
            {
                name        = "tool_default",
                texDefault  = hud.texToolDefault,
                texSelected = hud.texToolDefaultSelected,
                tooltip     = "Default tool",
                options = {
                    { name        = "tool_info",
                      texDefault  = hud.texToolInfo,
                      texSelected = hud.texToolInfoSelected,
                      tooltip     = "Info tool" },
                },
            },
        },
        selectedIndex = 3,
        direction = "up",
        optionsDirection = "right",
        size    = hud.baseSizes.buttonSize,
        padding = hud.baseSizes.padding,
        x       = toolAnchorX,
        y       = toolAnchorY,
        zIndex  = 100,
        uiscale = uiscale,
        onChange = function(index, itemName)
            world.setToolMode(hud.worldId, itemName)
            engine.logDebug("Tool mode changed to: " .. tostring(itemName))
            -- Route to the build tool so it can show/hide its picker.
            local buildTool = require("scripts.build_tool")
            buildTool.onToolMode(itemName)
            -- Route to the mine tool so a pending anchor cancels.
            local mineTool = require("scripts.mine_tool")
            mineTool.onToolMode(itemName)
            -- Route to the tile editor so its popup can hide on
            -- non-info tools.
            local tileEditor = require("scripts.tile_editor")
            tileEditor.onToolMode(itemName)
        end,
    })

    -- Pass HUD context to the build tool so its popup knows where to
    -- anchor itself and how to switch back to the default tool.
    local buildTool = require("scripts.build_tool")
    buildTool.setup({
        page       = hud.world_page,
        fbW        = hud.fbW,
        fbH        = hud.fbH,
        boxTexSet  = hud.boxTexSet,
        menuFont   = hud.menuFont,
        buttonSize = hud.baseSizes.buttonSize,
        selectDefaultTool = function()
            -- Default tool is at index 3 in the items list above
            -- (after tool_mine at 1 and tool_build at 2).
            toggle.select(hud.toolToggleId, 3)
        end,
    })

    -- Mine tool: needs the hud reference for worldId / current view.
    local mineToolMod = require("scripts.mine_tool")
    mineToolMod.setup({ hud = hud })

    -- Tile editor lives on the same world_page as the build tool.
    -- It uses the same box texture set + menu font; worldId is used
    -- as the WorldPageId target for the WorldDeleteTile command.
    local tileEditor = require("scripts.tile_editor")
    tileEditor.setup({
        page      = hud.world_page,
        fbW       = hud.fbW,
        fbH       = hud.fbH,
        boxTexSet = hud.boxTexSet,
        menuFont  = hud.menuFont,
        worldId   = hud.worldId,
    })

    -- Cargo inventory popup: shares the world_page so it z-orders
    -- above standard HUD overlays and follows the same lifecycle.
    local cargoInventoryPanel = require("scripts.cargo_inventory_panel")
    cargoInventoryPanel.setup({
        page      = hud.world_page,
        fbW       = hud.fbW,
        fbH       = hud.fbH,
        boxTexSet = hud.boxTexSet,
        menuFont  = hud.menuFont,
    })

    ---------------------------------------------------------
    -- Info panel on its own dedicated page.
    -- This page is shown/hidden independently so we can
    -- cleanly toggle ALL its elements (panel box, tab bar
    -- boxes, frame, labels) without fighting the tabbar
    -- module's internal structure.
    ---------------------------------------------------------
    -- The shared HUD info panel handles tile + building info display.
    -- unit_info_panel.lua auto-suppresses its unit push while
    -- unit_info_v2 is loaded, so this panel won't compete for unit
    -- selection — it just shows what the tile-info / building-info
    -- broadcasts push through hud.setInfoText.
    infoPanel.create({
        page      = hud.info_page,
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
    -- info_page starts hidden (infoPanel.create hides it);
    -- it will show when content is sent via setInfoText.

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

    -- Re-apply cursor textures every show, because the WorldState is
    -- recreated on world regeneration and loses its texture handles.
    if hud.texZoomSelect and hud.texZoomHover then
        world.setZoomCursorSelectTexture(hud.worldId, hud.texZoomSelect)
        world.setZoomCursorHoverTexture(hud.worldId, hud.texZoomHover)
    end
    if hud.texWorldSelect and hud.texWorldHover
            and hud.texWorldSelectBg and hud.texWorldHoverBg then
        world.setWorldCursorSelectTexture(hud.worldId, hud.texWorldSelect)
        world.setWorldCursorHoverTexture(hud.worldId, hud.texWorldHover)
        world.setWorldCursorSelectBgTexture(hud.worldId, hud.texWorldSelectBg)
        world.setWorldCursorHoverBgTexture(hud.worldId, hud.texWorldHoverBg)
    end
    if hud.texMineDesignate then
        world.setMineDesignateTexture(hud.worldId, hud.texMineDesignate)
    end

    hud.visible = true

    if hud.currentView == "zoomed_in" and hud.world_page then
        UI.showPage(hud.world_page)
    elseif hud.currentView == "zoomed_out" and hud.zoom_page then
        UI.showPage(hud.zoom_page)
    end
    -- info_page visibility is managed by infoPanel itself;
    -- if it has content it will already be shown.

    -- The global page (log toggle button) is always visible during
    -- gameplay regardless of zoom level.
    if hud.global_page then
        UI.showPage(hud.global_page)
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
    if hud.info_page then
        UI.hidePage(hud.info_page)
    end
    if hud.global_page then
        UI.hidePage(hud.global_page)
    end
    -- If the log panel happens to be open when the HUD hides
    -- (e.g. world-view exits to main menu), hide it too so it
    -- doesn't leak into the next screen.
    pcall(function() require("scripts.event_log").hide() end)

    engine.logDebug("HUD hidden")
end

function hud.onMouseDown(button_num, mx, my)
    if hud.currentView == "zoomed_out" then
        if button_num == 1 then
            world.setZoomCursorSelect(hud.worldId)
        elseif button_num == 2 then
            world.clearZoomCursorSelect(hud.worldId)
        end
    elseif hud.currentView == "zoomed_in" then
        if button_num == 1 then
            -- game.onMouseDown runs earlier in the broadcast and has
            -- already settled the unit selection (synchronous, via
            -- atomicModifyIORef). If a unit is selected now, the click
            -- was a unit click — skip the tile-select so we don't
            -- race-clobber the unit info panel.
            if unit and unit.getSelected and #unit.getSelected() > 0 then
                return
            end
            -- Same rule for buildings: a building click takes over the
            -- info panel; don't also push tile info on top.
            if building and building.getSelected
               and building.getSelected() then
                return
            end
            -- Tile-info popup only fires in info-tool mode. In other
            -- tools, a click that misses a unit just clears any active
            -- selection (game.onMouseDown already did that) and is
            -- otherwise inert.
            if world.getToolMode and world.getToolMode() ~= "info" then
                return
            end
            world.setWorldCursorSelect(hud.worldId)
            -- Refresh the tile-editor popup at the just-selected
            -- tile. Gated internally by arenaMode so non-arena worlds
            -- silently no-op.
            local gx, gy = world.getHoverTile()
            if gx and gy then
                local tileEditor = require("scripts.tile_editor")
                tileEditor.onTileSelected(gx, gy)
            end
        elseif button_num == 2 then
            world.clearWorldCursorSelect(hud.worldId)
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
            -- Clear info panel on zoom transition
            infoPanel.clear()
        end
    elseif zoom < zoomFadeStart then
        if oldView ~= "zoomed_in" and hud.zoom_page and hud.world_page then
            UI.showPage(hud.world_page)
            UI.hidePage(hud.zoom_page)
            hud.currentView = "zoomed_in"
            -- Clear info panel on zoom transition
            infoPanel.clear()
        end
    else
        if oldView == "zoomed_in" and hud.world_page then
            UI.hidePage(hud.world_page)
        elseif oldView == "zoomed_out" and hud.zoom_page then
            UI.hidePage(hud.zoom_page)
        end
        hud.currentView = "none"
        -- In the fade zone, clear the info panel
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
            world.setZoomCursorHover(hud.worldId, mx, my)
        elseif hud.currentView == "zoomed_in" then
            world.setWorldCursorHover(hud.worldId, mx, my)
        end
    end

end

-- Switch the HUD log mode. The toggle widget already updates the
-- in-slot icon when an option is picked (it's how the map/tool
-- toggles work); this helper only handles the side effects we
-- want on top of the visual swap: hide the outgoing log and show
-- the incoming one so the picker always lands the player on a
-- visible panel. Called from the toggle's onChange after a swap,
-- and from the `toggleEventLog` keybind path when needed.
function hud.setLogMode(mode)
    if mode ~= "event" and mode ~= "combat" then return end
    if mode ~= hud.logMode then
        if hud.logMode == "combat" then
            pcall(function() require("scripts.combat_log").hide() end)
        else
            pcall(function() require("scripts.event_log").hide() end)
        end
        hud.logMode = mode
    end
    if mode == "combat" then
        require("scripts.combat_log").show()
    else
        require("scripts.event_log").show()
    end
end

-----------------------------------------------------------
-- Info Panel Public API
-----------------------------------------------------------

function hud.setInfoBasic(text)
    infoPanel.setText("basic", text)
end

function hud.setInfoAdvanced(text)
    infoPanel.setText("advanced", text)
end

function hud.setInfoText(basicText, advancedText)
    infoPanel.setInfo(basicText, advancedText)
end

function hud.setWeatherInfo(text)
    infoPanel.setWeatherInfo(text)
end

function hud.setResourcesInfo(text)
    infoPanel.setResourcesInfo(text)
end

function hud.setGroundItemInfo(text)
    infoPanel.setGroundItemInfo(text)
end

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
    if hud.toolToggleId then
        toggle.destroy(hud.toolToggleId)
        hud.toolToggleId = nil
    end
    if hud.logToggleId then
        toggle.destroy(hud.logToggleId)
        hud.logToggleId = nil
    end
    if hud.info_page then
        UI.hidePage(hud.info_page)
        UI.deletePage(hud.info_page)
        hud.info_page = nil
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
    if hud.global_page then
        UI.hidePage(hud.global_page)
        UI.deletePage(hud.global_page)
        hud.global_page = nil
    end
end

return hud
