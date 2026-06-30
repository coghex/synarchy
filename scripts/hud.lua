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

-- Set when a save load has reset main_world's engine ToolMode to default
-- but the shared toolbar wasn't bound to main_world at the time (HUD on
-- the arena page, or hidden in a menu). Consumed the next time the HUD
-- binds to main_world and shows. See hud.markLoadedToolReset. (#103)
hud.mainWorldToolDirty = false

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
hud.texToolConstruct       = nil
hud.texToolConstructSelected = nil

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
    hud.texToolConstruct       = engine.loadTexture("assets/textures/hud/tool_construct.png")
    hud.texToolConstructSelected = engine.loadTexture("assets/textures/hud/tool_construct_selected.png")
    hud.texZoomSelect          = engine.loadTexture("assets/textures/hud/utility/zoom_select.png")
    hud.texZoomHover           = engine.loadTexture("assets/textures/hud/utility/zoom_hover.png")
    hud.texWorldSelect         = engine.loadTexture("assets/textures/hud/utility/world_select.png")
    hud.texWorldSelectBg       = engine.loadTexture("assets/textures/hud/utility/world_select_bg.png")
    hud.texWorldHover          = engine.loadTexture("assets/textures/hud/utility/world_hover.png")
    hud.texWorldHoverBg        = engine.loadTexture("assets/textures/hud/utility/world_hover_bg.png")
    -- Mine-designation marker: dedicated art so standing designations
    -- read differently from the cursor/selection at a glance.
    hud.texMineDesignate       = engine.loadTexture("assets/textures/hud/utility/mine_designate.png")
    -- Construction-designation ghosts (#95): one per target category so a
    -- planned structure reads differently from a planned building.
    hud.texConstructStructure  = engine.loadTexture("assets/textures/hud/utility/construct_designate_structure.png")
    hud.texConstructBuilding   = engine.loadTexture("assets/textures/hud/utility/construct_designate_building.png")

    -- Event-log toggle (top-left). Two states: default and selected
    -- (drawn while the event log panel is open). The combat-log
    -- variants are loaded here too so the step-5 icon swap (right-
    -- click picker) can flip between them without re-loading.
    hud.texEventLog         = engine.loadTexture("assets/textures/hud/event_log.png")
    hud.texEventLogSelected = engine.loadTexture("assets/textures/hud/event_log_selected.png")
    hud.texCombatLog         = engine.loadTexture("assets/textures/hud/combat_log.png")
    hud.texCombatLogSelected = engine.loadTexture("assets/textures/hud/combat_log_selected.png")
    hud.texInjuryLog         = engine.loadTexture("assets/textures/hud/injury_log.png")
    hud.texInjuryLogSelected = engine.loadTexture("assets/textures/hud/injury_log_selected.png")
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
        elseif mode == "injury" then
            return {
                name        = "injury",
                texDefault  = hud.texInjuryLog,
                texSelected = hud.texInjuryLogSelected,
                tooltip     = "Injury log",
            }
        end
        return {
            name        = "event",
            texDefault  = hud.texEventLog,
            texSelected = hud.texEventLogSelected,
            tooltip     = "Event log",
        }
    end
    -- The two modes that aren't current become the toggle's options
    -- (in a stable order so the fan doesn't reshuffle each open).
    local LOG_MODE_ORDER = { "event", "combat", "injury" }
    local otherModes = {}
    for _, m in ipairs(LOG_MODE_ORDER) do
        if m ~= hud.logMode then otherModes[#otherModes + 1] = logItem(m) end
    end

    hud.logToggleId = toggle.new({
        name = "log_mode_toggle",
        page = hud.global_page,
        items = {
            (function()
                local it = logItem(hud.logMode)
                it.options = otherModes
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
                require(hud.logModuleFor(itemName)).toggle()
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
    if hud.texConstructStructure then
        construction.setDesignateTexture(hud.worldId, "structure",
            hud.texConstructStructure)
    end
    if hud.texConstructBuilding then
        construction.setDesignateTexture(hud.worldId, "building",
            hud.texConstructBuilding)
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
                name        = "tool_construct",
                texDefault  = hud.texToolConstruct,
                texSelected = hud.texToolConstructSelected,
                tooltip     = "Construction designation tool",
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
        selectedIndex = 4,
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
            -- Route to the construction tool so it pops its picker on
            -- entry and cancels a pending anchor on exit.
            local constructTool = require("scripts.construct_tool")
            constructTool.onToolMode(itemName)
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
        selectDefaultTool = hud.selectDefaultTool,
    })

    -- Mine tool: needs the hud reference for worldId / current view.
    local mineToolMod = require("scripts.mine_tool")
    mineToolMod.setup({ hud = hud })

    -- Construction designation tool (#95): same hud reference.
    local constructToolMod = require("scripts.construct_tool")
    constructToolMod.setup({ hud = hud })

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

    -- Item contents popup (unit-carried container) shares the same
    -- page + assets as the cargo panel.
    local itemContentsPanel = require("scripts.item_contents_panel")
    itemContentsPanel.setup({
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
-- Tool selection
-----------------------------------------------------------

-- Switch the toolbar back to the default tool. Routes through
-- applyOptionByName, NOT a bare toggle.select: toggle.select is
-- visual-only and skips the toolbar onChange, so the engine-side
-- ToolMode would stay on the previous tool (e.g. BuildTool after a
-- placement/cancel/Escape — HUD icon flips to default, but
-- world.getToolMode() and the next save still report "build").
-- applyOptionByName mirrors a real user pick: it selects the
-- tool_default slot AND fires onChange → world.setToolMode
-- (..., "tool_default") → DefaultTool, and routes onToolMode so any
-- stale picker / mine anchor is cleared. Used by the build tool's
-- auto-exit and by the post-load reset in hud.show. (#103)
function hud.selectDefaultTool()
    if hud.toolToggleId then
        toggle.applyOptionByName(hud.toolToggleId, "tool_default")
    end
end

-- A save load ALWAYS targets the main_world page and resets its engine
-- ToolMode to default (World/Thread/Command/Save.hs). Mirror that into
-- the shared toolbar — but only while the toolbar is actually bound to
-- main_world, since its onChange writes world.setToolMode(hud.worldId,
-- ...) and must never reset the arena's tool state on a main_world load.
-- If the HUD is on another page or hidden when the load lands, the reset
-- is deferred (mainWorldToolDirty) and consumed the next time the HUD
-- binds to main_world in hud.show.
--
-- The flag is only CLEARED once the reset can actually be applied — the
-- toolbar must already exist (created in hud.createUI). A load that lands
-- before any gameplay UI has opened (e.g. a debug-console engine.loadSave
-- from the main menu) leaves toolToggleId nil; keep the flag set so the
-- reset still fires when the toolbar is first shown on main_world, rather
-- than silently consuming a no-op. (#103)
function hud.resetMainWorldToolIfDirty()
    if hud.mainWorldToolDirty and hud.worldId == "main_world"
            and hud.toolToggleId then
        hud.mainWorldToolDirty = false
        hud.selectDefaultTool()
    end
end

-- Called (via uiManager.onSaveLoaded) whenever any save finishes loading.
-- Flags the main_world toolbar for a default-tool reset and applies it
-- immediately if the HUD is already on main_world; otherwise it waits for
-- the next main_world bind. (#103)
function hud.markLoadedToolReset()
    hud.mainWorldToolDirty = true
    hud.resetMainWorldToolIfDirty()
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
    if hud.texConstructStructure then
        construction.setDesignateTexture(hud.worldId, "structure",
            hud.texConstructStructure)
    end
    if hud.texConstructBuilding then
        construction.setDesignateTexture(hud.worldId, "building",
            hud.texConstructBuilding)
    end

    hud.visible = true

    if hud.currentView == "zoomed_in" and hud.world_page then
        UI.showPage(hud.world_page)
    elseif hud.currentView == "zoomed_out" and hud.zoom_page then
        UI.showPage(hud.zoom_page)
    end
    -- Release the HUD-hidden suppression and restore the info panel from
    -- its content (show iff it has something, stay hidden otherwise). The
    -- stored tab text survives the hide; going through infoPanel keeps the
    -- page and infoPanel.visible in sync — don't show the page directly
    -- here, that desyncs the flag (#134).
    infoPanel.unsuppress("hud")

    -- The global page (log toggle button) is always visible during
    -- gameplay regardless of zoom level.
    if hud.global_page then
        UI.showPage(hud.global_page)
    end

    -- If a save was loaded while the HUD was bound elsewhere (arena) or
    -- hidden in a menu, the main_world toolbar reset was deferred until
    -- the HUD next binds to main_world — apply it now. (#103)
    hud.resetMainWorldToolIfDirty()

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
    -- Suppress the info panel while the HUD is hidden. This both hides
    -- the page and blocks background info watchers (which keep pushing
    -- text for a still-selected object) from re-opening it over the menu.
    -- hud.show() releases the suppression and restores from content (#134).
    infoPanel.suppress("hud")
    if hud.global_page then
        UI.hidePage(hud.global_page)
    end
    -- If any HUD-owned overlay happens to be open when the HUD hides
    -- (e.g. world-view exits to main menu), hide it too so it doesn't
    -- leak into the next screen. Each owns its own modal page / visible
    -- flag, so hiding the HUD pages above does not cover them:
    --   * event/combat/injury log panels (#84)
    --   * notification popups (#85)
    --   * right-click context menus (#86)
    --   * item-contents popup (#100)
    --   * per-unit log overlay (#104)
    --   * cargo inventory popup (#99)
    --   * main debug overlay (#147)
    -- Ground-item selection (#175) is NOT cleared here: it lives in the
    -- per-world cursor (wsCursorRef) and a Lua deselect resolves through
    -- activeWorld, which head-falls-back to a different registered world
    -- once this page leaves wmVisible — so it could clear the wrong
    -- world. The world thread clears it deterministically in
    -- handleWorldHideCommand, keyed on the exact page being hidden (which
    -- worldView.hide()/testArena.hide() issue just before this runs).
    --
    -- Building selection (#176), by contrast, IS cleared here: it is a
    -- single global id (bmSelected), not a per-world cursor, so deselect()
    -- has no wrong-world hazard and is a no-op when nothing is selected.
    -- Suppressing the panel alone is not enough — the building-info watcher
    -- keeps polling building.getSelected() and re-pushing the selected
    -- building into the panel's stored content, which hud.show() then
    -- restores stale. Clearing the selection stops the repopulation.
    building.deselect()
    pcall(function() require("scripts.event_log").hide() end)
    pcall(function() require("scripts.combat_log").hide() end)
    pcall(function() require("scripts.injury_log_panel").hide() end)
    pcall(function() require("scripts.popup").dismissAll() end)
    pcall(function() require("scripts.ui.context_menu").hide() end)
    pcall(function() require("scripts.item_contents_panel").closeIfOpen() end)
    pcall(function() require("scripts.unit_log").hide() end)
    pcall(function() require("scripts.cargo_inventory_panel").closeIfOpen() end)
    pcall(function() require("scripts.debug").hide() end)
    -- Active drag-select box (#146): its overlay page is independent of
    -- the HUD pages hidden above, so a box in progress when the HUD
    -- hides (e.g. world-view exits to a menu) would leak across the
    -- transition. cancel() abandons it without committing a selection.
    pcall(function() require("scripts.unit_drag_select").cancel() end)

    engine.logDebug("HUD hidden")
end

function hud.onMouseDown(button_num, mx, my)
    -- Same gate as hud.update: when the HUD is hidden (a menu is open
    -- over a hidden gameplay view), don't act on the world. currentView
    -- is preserved across hud.hide() for re-show, so without this a
    -- blank-area menu click forwarded by uiManager.onMouseDown would
    -- still call world.setZoomCursorSelect / setWorldCursorSelect (or
    -- the clear variants) against the hidden world (#153).
    --
    -- hud.visible alone isn't enough (#154): the pause menu and keep-world
    -- Settings open as overlays that bypass hud.hide(), so hud.visible
    -- stays true while the world sits behind a non-gameplay menu. A blank
    -- click forwarded here would still set/clear the zoom/world cursor
    -- behind that overlay. isGameplayInputActive() is false for those
    -- overlays (it treats pause as "menu on top"), so gate on it too —
    -- the same predicate game.onMouseDown uses for the matching left/
    -- right gameplay gates.
    if not hud.visible
       or not require("scripts.ui_manager").isGameplayInputActive() then
        return
    end
    -- Recover window-pixel click coords. uiManager.onMouseDown forwards
    -- mx,my already scaled into FRAMEBUFFER space, but the synchronous
    -- pickers (world.pickTile) and the zoom-cursor hit-test
    -- (pixelToChunkOrigin, fed by setZoomCursorHover) both expect WINDOW
    -- pixels — the same space engine.getMousePosition() reports. Convert
    -- back so a click resolves at the actual click position rather than
    -- the 0.1s-stale cached hover the select APIs would otherwise read
    -- (#123).
    local cx, cy = mx, my
    do
        local ww, wh   = engine.getWindowSize()
        local fbW, fbH = engine.getFramebufferSize()
        if fbW and fbH and fbW > 0 and fbH > 0 then
            cx = mx * (ww / fbW)
            cy = my * (wh / fbH)
        end
    end
    if hud.currentView == "zoomed_out" then
        if button_num == 1 then
            -- Refresh the zoom-cursor position to the actual click coords
            -- before arming the select. The chunk is committed from
            -- zoomCursorPos at render time (makeCursorQuad); without this
            -- it would commit the periodically-cached hover, so a fast
            -- move-then-click selected the previously hovered chunk (#123).
            world.setZoomCursorHover(hud.worldId, cx, cy)
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
            -- Same rule for ground items: game.onMouseDown already
            -- settled the item selection (item.select), and the item
            -- watcher owns the panel + expects tile-info pushes to
            -- deselect it. Consume the click here so tile-info doesn't
            -- race-clobber the item info on the same click (#173).
            if item and item.getSelected and item.getSelected() then
                return
            end
            -- Tile-info popup only fires in info-tool mode. In other
            -- tools, a click that misses a unit just clears any active
            -- selection (game.onMouseDown already did that) and is
            -- otherwise inert.
            if world.getToolMode and world.getToolMode() ~= "info" then
                return
            end
            -- Live pick at the click coords, then select that tile
            -- directly. setWorldCursorSelect would arm a select that
            -- commits from the 0.1s-cached worldHoverTile at render time,
            -- so a fast move-then-click selected the previously hovered
            -- tile; pickTile resolves the tile under the click NOW and
            -- world.selectTile sets it in one shot (also dropping any
            -- chunk selection, #135) (#123). Pass the picked z so a click
            -- below the surface selects the clicked tile, not the column
            -- top (#367) — pickTile resolves the tile at the active
            -- z-slice, world.selectTile would otherwise snap to surface z.
            local gx, gy, gz = world.pickTile(cx, cy)
            if gx and gy then
                world.selectTile(hud.worldId, gx, gy, gz)
                -- Refresh the tile-editor popup at the just-selected
                -- tile. Gated internally by arenaMode so non-arena worlds
                -- silently no-op.
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

-- Reconcile the HUD view (zoomed_in / zoomed_out / none) with the camera's
-- actual zoom band, doing the page swap + per-transition teardown exactly
-- when the band genuinely changes.
--
-- This is driven both from hud.update() (every tick) and hud.onScroll()
-- (the wheel hook). The wheel hook alone is NOT enough: worldView.onScroll
-- only imparts zoom *velocity*, and the camera crosses the band later in
-- Engine.Loop.Camera.updateCameraZoom. If the final wheel event carries
-- enough momentum to coast across the band with no further scroll event,
-- a wheel-timed teardown never fires. Running it per-tick off the real
-- zoom closes that gap (#175). It also keeps hud.currentView authoritative
-- so the HUD page never lags the camera.
--
-- Teardown on each real transition:
--   * infoPanel.clear() — the shared HUD info panel.
--   * item-contents popup (#142): mounted on hud.world_page, so hiding
--     the page only takes it off-view; its logical state stays open and
--     could reappear stale. closeIfOpen() tears it down.
--   * cargo-inventory popup (#141): same story as the item-contents
--     popup — mounted on hud.world_page, so a band change only hides the
--     page while state.open stays true and the popup reappears stale on
--     return. closeIfOpen() (idempotent) tears it down on every band change.
--   * right-click context menu (#139): own modal page, anchored to the
--     click target; hide() it so it can't survive over the wrong view.
--   * ground-item selection (#175): per-world cursor state the item
--     watcher (item_info_panel.update) keeps polling and would use to
--     repopulate the panel. item.deselect() clears the active world's
--     selection — and here the world is still the visible/active one, so
--     activeWorld resolves correctly (unlike the menu-hide path, which is
--     handled engine-side in handleWorldHideCommand).
--   * chunk/tile cursor selection (#132): zoomSelectedPos (zoom-map chunk)
--     and worldSelectedTile (zoomed-in tile) also live in the per-world
--     cursor. infoPanel.clear() blanks the panel, but the selection itself
--     survives the band change, and pollCursorInfo only republishes on a
--     selection *change* — so without clearing it the highlight stays set
--     while the panel stays empty. clearZoom/WorldCursorSelect tear both
--     down with the panel.
function hud.reconcileView()
    local zoom = camera.getZoom()
    local zoomFadeStart = camera.getZoomFadeStart()
    local zoomFadeEnd = camera.getZoomFadeEnd()

    local newView
    if zoom > zoomFadeEnd then
        newView = "zoomed_out"
    elseif zoom < zoomFadeStart then
        newView = "zoomed_in"
    else
        newView = "none"
    end

    local oldView = hud.currentView
    if newView == oldView then return end

    -- Page swaps are guarded on page existence (pages don't exist before
    -- createUI / in headless); the teardown below runs on every real
    -- transition regardless, so selection/info never survive a band change.
    if newView == "zoomed_out" then
        if hud.zoom_page then UI.showPage(hud.zoom_page) end
        if hud.world_page then UI.hidePage(hud.world_page) end
    elseif newView == "zoomed_in" then
        if hud.world_page then UI.showPage(hud.world_page) end
        if hud.zoom_page then UI.hidePage(hud.zoom_page) end
    else -- "none": in the fade band, hide whichever view page was up
        if oldView == "zoomed_in" and hud.world_page then
            UI.hidePage(hud.world_page)
        elseif oldView == "zoomed_out" and hud.zoom_page then
            UI.hidePage(hud.zoom_page)
        end
    end
    hud.currentView = newView

    infoPanel.clear()
    require("scripts.item_contents_panel").closeIfOpen()
    require("scripts.cargo_inventory_panel").closeIfOpen()
    -- Right-click context menu (#139): it lives on its own modal page and
    -- is anchored to the tile/unit/item under the click in the zoomed-in
    -- view. Hiding the world/zoom pages above only takes it off-view; its
    -- logical state stays open and it can reappear over the wrong view.
    -- hide() is idempotent (no-op when no menu is open), so dismiss it on
    -- every band change.
    require("scripts.ui.context_menu").hide()
    -- Active drag-select box (#146): the rect lives on its own
    -- "drag_select_overlay" page, so the world/zoom page swap above
    -- never touches it. An armed/dragging box would otherwise survive
    -- the band change and resume or commit against the wrong view.
    -- cancel() is idempotent (no-op when idle), so abandon any in-flight
    -- box on every band change.
    require("scripts.unit_drag_select").cancel()
    -- Mine-designation anchor (#144): a pending first-corner anchor lives in
    -- mine_tool's Lua state + the world cursor (mineAnchor) and renders a
    -- preview grid. The tool only acts in zoomed_in, but nothing cleared the
    -- anchor on a band change, so it survived off-view and could not be
    -- canceled there (Escape/right-click are gated on the zoomed_in view).
    -- cancel() is idempotent (clears Lua state + WorldClearMineAnchor is a
    -- no-op when nothing is pending), so tear it down on every transition.
    require("scripts.mine_tool").cancel()
    -- Construction designation anchor (#95): same idempotent teardown as
    -- the mine anchor above, so a pending rectangle can't survive off-view.
    require("scripts.construct_tool").cancel()
    -- Build picker (#143): the picker panel lives on hud.world_page and
    -- its "picker" mode persists across band changes. The world/zoom page
    -- swap above only takes it off-view, so a picker opened in zoomed_in
    -- stays logically alive and reappears stale when the world page is
    -- next shown. hidePicker() is idempotent (destroyPicker no-ops with
    -- no panel; mode only resets from "picker"), so tear it down on every
    -- transition. It deliberately does NOT touch "placement" mode.
    require("scripts.build_tool").hidePicker()
    -- Build placement (#140): once the build tool enters "placement" mode,
    -- its ghost preview and click handling keep running off the world
    -- cursor regardless of the HUD view. The page swap above only takes
    -- the world-page tool UI off-view, so placement stays live in
    -- zoomed-out / fade-zone and keeps consuming clicks. exitPlacement()
    -- is idempotent (clearGhost no-ops with no ghost; mode resets to
    -- "off"), so cancel any in-progress placement on every band change.
    require("scripts.build_tool").exitPlacement()
    -- Arena tile-editor popup (#138): the test-arena tile editor lives on
    -- hud.world_page and was only torn down on empty tile-info broadcasts,
    -- tool changes, or arena exit. The page swap above only takes it
    -- off-view, and zoom-map chunk selection produces non-empty HUD info
    -- text, so neither the band change nor the empty-text clear path
    -- closed it — it survived off-view and reappeared stale when the world
    -- page returned. clear() is idempotent (destroyPopup no-ops with no
    -- popup, and is a no-op outside arena mode), so tear it down on every
    -- transition.
    require("scripts.tile_editor").clear()
    if newView ~= "zoomed_in" then
        require("scripts.debug").hide()
    end
    item.deselect()
    -- Building selection (#176): unlike the per-world cursor selections
    -- below, building selection is a single global id (bmSelected), so the
    -- page swap above never touches it and the building-info watcher keeps
    -- polling building.getSelected() and re-pushing the same building into
    -- the shared info panel — repopulating it stale after the band change.
    -- deselect() clears the global selection and is a no-op when nothing is
    -- selected, matching item.deselect() above; no wrong-world hazard since
    -- there is no per-world building cursor to resolve through activeWorld.
    building.deselect()
    -- Chunk/tile cursor selection (#132): the zoom-map chunk selection
    -- (zoomSelectedPos) and zoomed-in tile selection (worldSelectedTile)
    -- both live in the per-world cursor (wsCursorRef), independent of the
    -- HUD pages swapped above. The info panel is cleared on this transition
    -- (infoPanel.clear() above), but pollCursorInfo only republishes when
    -- the selection *changes* — so a selection that survives the band change
    -- unchanged leaves the highlight logically set while the panel stays
    -- blank until the user re-selects. Clear both selections here, exactly
    -- like item.deselect() does for the ground-item selection, so the
    -- highlight and panel tear down together. The clears are keyed on
    -- hud.worldId (the visible/active world that owns this view, the same id
    -- onMouseDown uses), and are no-ops when nothing is selected.
    world.clearZoomCursorSelect(hud.worldId)
    world.clearWorldCursorSelect(hud.worldId)
end

-- Wheel hook (no UI element under cursor, no shift). Reconcile immediately
-- so a transition the wheel itself crosses doesn't wait for the next tick.
--
-- Gated on hud.visible, same as hud.update()/hud.onMouseDown(): uiManager
-- still routes game scrolls here in the plain "test_arena" loading/builder
-- state, where the HUD is hidden (hud.show() only runs for the
-- "test_arena_view"/"world_view" gameplay states). Without the gate, a
-- scroll there would run reconcileView()'s item.deselect() against
-- activeWorld behind the hidden HUD — the very wrong-world clear this
-- change moves engine-side for the hide path (#175). The per-tick driver
-- in hud.update() is gated the same way, so reconcile only ever runs while
-- the HUD owns the view.
function hud.onScroll(dx, dy)
    if not hud.visible then
        return
    end
    hud.reconcileView()
end

-----------------------------------------------------------
-- manage mouse hover
-----------------------------------------------------------

function hud.update(dt)
    -- Don't push hover state while the HUD is hidden (e.g. a menu is
    -- open over a hidden gameplay view). hud.hide() leaves currentView
    -- intact so it can be restored on re-show, so this loop would
    -- otherwise keep mutating the hidden world's cursor/hover behind
    -- the menu (#153). hud.visible is the authoritative gate.
    if not hud.visible then
        return
    end
    -- Reconcile the view band against the camera's actual zoom every tick.
    -- The camera crosses the band in the engine camera loop, which the
    -- wheel-timed onScroll can miss when momentum coasts past the band
    -- (#175). Idempotent: returns immediately when the band is unchanged.
    hud.reconcileView()
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
-- Script module backing each log mode.
local LOG_MODULES = {
    event  = "scripts.event_log",
    combat = "scripts.combat_log",
    injury = "scripts.injury_log_panel",
}
function hud.logModuleFor(mode)
    return LOG_MODULES[mode] or LOG_MODULES.event
end

function hud.setLogMode(mode)
    if not LOG_MODULES[mode] then return end
    if mode ~= hud.logMode then
        pcall(function() require(LOG_MODULES[hud.logMode]).hide() end)
        hud.logMode = mode
    end
    require(LOG_MODULES[mode]).show()
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
