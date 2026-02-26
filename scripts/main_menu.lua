-- Main Menu Module (pure module, no global callbacks)
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local mainMenu = {}

mainMenu.page = nil
mainMenu.panelId = nil
mainMenu.boxTexSet = nil
mainMenu.buttonTexSet = nil
mainMenu.menuFont = nil
mainMenu.titleFont = nil
mainMenu.fbW = 0
mainMenu.fbH = 0
mainMenu.uiCreated = false

mainMenu.titleLabelId = nil

-- Owned element IDs for scoped cleanup
mainMenu.ownedLabels  = {}
mainMenu.ownedBoxes   = {}   -- raw UI element handles (not button IDs)
mainMenu.ownedPanels  = {}

-- Save state
mainMenu.saves = {}
mainMenu.latestSave = nil

-- Callback to switch menus (set by ui_manager)
mainMenu.showMenuCallback = nil

-- Click dispatch table: elemHandle -> function
mainMenu.clickHandlers = {}

-- Base sizes (unscaled)
mainMenu.baseSizes = {
    titleFontSize = 96,
    fontSize = 32,
    buttonHeight = 80,
    buttonSpacing = 24,
    buttonTileSize = 64,
    buttonPaddingX = 160,
    buttonOverflow = 16,
    menuPaddingX = 100,
    menuPaddingY = 100,
    menuTileSize = 64,
    menuOverflow = 0,
    titleOffset = 80,
}

-- Build menu items dynamically based on available saves
function mainMenu.buildMenuItems()
    local items = {}

    -- Check for saves
    mainMenu.saves = engine.listSaves() or {}

    if #mainMenu.saves > 0 then
        table.sort(mainMenu.saves, function(a, b)
            return a.timestamp > b.timestamp
        end)
        mainMenu.latestSave = mainMenu.saves[1].name

        table.insert(items, {
            name = "continue",
            label = "Continue",
            onClick = function() mainMenu.onContinue() end,
        })

        if #mainMenu.saves > 1 then
            table.insert(items, {
                name = "load_game",
                label = "Load Game",
                onClick = function() mainMenu.onLoadGame() end,
            })
        end
    end

    table.insert(items, {
        name = "create_world",
        label = "Create World",
        onClick = function() mainMenu.onCreateWorld() end,
    })
    table.insert(items, {
        name = "settings",
        label = "Settings",
        onClick = function() mainMenu.onSettings() end,
    })
    table.insert(items, {
        name = "quit",
        label = "Quit",
        onClick = function() mainMenu.onQuit() end,
    })

    return items
end

-----------------------------------------------------------
-- Scoped cleanup
-----------------------------------------------------------

function mainMenu.destroyOwned()
    for _, id in ipairs(mainMenu.ownedLabels) do label.destroy(id) end
    for _, h  in ipairs(mainMenu.ownedBoxes)  do UI.deleteElement(h) end
    for _, id in ipairs(mainMenu.ownedPanels) do panel.destroy(id) end
    mainMenu.ownedLabels  = {}
    mainMenu.ownedBoxes   = {}
    mainMenu.ownedPanels  = {}
    mainMenu.clickHandlers = {}
end

function mainMenu.setShowMenuCallback(callback)
    mainMenu.showMenuCallback = callback
end

function mainMenu.init(boxTex, btnTex, font, tFont, width, height)
    mainMenu.boxTexSet = boxTex
    mainMenu.buttonTexSet = btnTex
    mainMenu.menuFont = font
    mainMenu.titleFont = tFont
    mainMenu.fbW = width
    mainMenu.fbH = height

    mainMenu.createUI()
end

function mainMenu.createUI()
    if mainMenu.uiCreated then
        if mainMenu.page then
            UI.deletePage(mainMenu.page)
            mainMenu.destroyOwned()
            mainMenu.titleLabelId = nil
        end
    end

    local menuItems = mainMenu.buildMenuItems()

    local uiscale = scale.get()
    local s = scale.applyAll(mainMenu.baseSizes)

    engine.logDebug("Creating main menu with framebuffer size: "
        .. mainMenu.fbW .. " x " .. mainMenu.fbH
        .. ", scale: " .. uiscale
        .. ", saves found: " .. #mainMenu.saves)

    mainMenu.page = UI.newPage("main_menu", "menu")

    -- Calculate max text width for sizing
    local maxLabelWidth = 0
    for _, item in ipairs(menuItems) do
        local w = engine.getTextWidth(mainMenu.menuFont, item.label, s.fontSize)
        if w > maxLabelWidth then maxLabelWidth = w end
    end

    local itemWidth  = maxLabelWidth + s.buttonPaddingX
    local menuHeight = #menuItems * (s.buttonHeight + s.buttonSpacing)
                     + s.buttonSpacing + s.menuPaddingY
    local menuWidth  = itemWidth + s.menuPaddingX

    local menuX = (mainMenu.fbW - menuWidth) / 2
    local menuY = (mainMenu.fbH - menuHeight) / 2

    -- Background panel
    mainMenu.panelId = panel.new({
        name       = "menu_panel",
        page       = mainMenu.page,
        x = menuX, y = menuY,
        width      = menuWidth,
        height     = menuHeight,
        textureSet = mainMenu.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = s.menuTileSize,
        overflow   = s.menuOverflow,
        zIndex     = 1,
        padding    = {
            top    = s.menuPaddingY / 2,
            bottom = s.menuPaddingY / 2,
            left   = s.menuPaddingX / 2,
            right  = s.menuPaddingX / 2,
        },
        uiscale = 1.0,
    })
    table.insert(mainMenu.ownedPanels, mainMenu.panelId)

    local baseZ = panel.getZIndex(mainMenu.panelId)

    -- Title
    mainMenu.titleLabelId = label.new({
        name     = "title",
        text     = "Ecce Homo",
        font     = mainMenu.titleFont,
        fontSize = mainMenu.baseSizes.titleFontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = mainMenu.page,
        uiscale  = uiscale,
    })
    table.insert(mainMenu.ownedLabels, mainMenu.titleLabelId)

    local titleW, _ = label.getSize(mainMenu.titleLabelId)
    local titleX = (mainMenu.fbW - titleW) / 2
    local titleY = menuY - s.titleOffset
    UI.addToPage(mainMenu.page,
        label.getElementHandle(mainMenu.titleLabelId), titleX, titleY)

    -- Menu items: box + centered label, no button widget
    local boxElements = {}
    local boxSizes    = {}

    for _, item in ipairs(menuItems) do
        local lblW = engine.getTextWidth(mainMenu.menuFont, item.label, s.fontSize)
        local thisW = lblW + s.buttonPaddingX
        local thisH = s.buttonHeight

        -- Create a 9-slice box (same texture as the panel)
        local boxH = UI.newBox(
            item.name .. "_box",
            thisW, thisH,
            mainMenu.boxTexSet,
            s.buttonTileSize,
            1.0, 1.0, 1.0, 1.0,   -- color
            s.buttonOverflow,
            mainMenu.page
        )
        table.insert(mainMenu.ownedBoxes, boxH)

        -- Make it clickable with the generic callback
        UI.setClickable(boxH, true)
        UI.setOnClick(boxH, "onMainMenuItem")

        -- Register the onClick handler
        mainMenu.clickHandlers[boxH] = item.onClick

        -- Create a text child, centered inside the box
        local textH = UI.newText(
            item.name .. "_label",
            item.label,
            mainMenu.menuFont,
            s.fontSize,
            1.0, 1.0, 1.0, 1.0,   -- text color
            mainMenu.page
        )

        local labelX = (thisW - lblW) / 2
        local labelY = (thisH / 2) + (s.fontSize / 2)
        UI.addChild(boxH, textH, labelX, labelY)
        UI.setZIndex(textH, 1)

        -- Set the box z above the panel
        UI.setZIndex(boxH, baseZ + 1)

        table.insert(boxElements, boxH)
        table.insert(boxSizes, { width = thisW, height = thisH })
    end

    -- Place items in a centered column inside the panel
    panel.placeColumn(
        mainMenu.panelId,
        boxElements,
        boxSizes,
        {
            x = "50%",
            y = "50%",
            origin = "center",
            spacing = s.buttonSpacing,
        }
    )

    mainMenu.uiCreated = true
    engine.logDebug("Main menu created with " .. #menuItems .. " items")
end

-----------------------------------------------------------
-- Click dispatch (called from ui_manager.onMainMenuItem)
-----------------------------------------------------------

function mainMenu.handleClick(elemHandle)
    local handler = mainMenu.clickHandlers[elemHandle]
    if handler then
        handler()
        return true
    end
    return false
end

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function mainMenu.onContinue()
    if mainMenu.latestSave then
        engine.logInfo("Continuing from save: " .. mainMenu.latestSave)
        mainMenu.loadAndShowSave(mainMenu.latestSave)
    end
end

function mainMenu.onLoadGame()
    engine.logInfo("Opening load game screen")
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("save_browser")
    end
end

function mainMenu.onCreateWorld()
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("create_world")
    end
end

function mainMenu.onSettings()
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("settings")
    end
end

function mainMenu.onQuit()
    engine.quit()
end

-----------------------------------------------------------
-- Load a save and transition to world view
-----------------------------------------------------------

-----------------------------------------------------------
-- Load a save and transition to world view via loading screen
-----------------------------------------------------------

function mainMenu.loadAndShowSave(saveName)
    local worldView = require("scripts.world_view")
    local worldManager = require("scripts.world_manager")

    local ok = engine.loadSave(saveName)
    if not ok then
        engine.logError("Failed to load save: " .. saveName)
        return
    end

    worldManager.currentWorld = "main_world"
    worldManager.active = true

    worldView.sendTexturesToWorld("main_world")
    world.show("main_world")

    -- Show loading screen instead of jumping straight to world_view.
    -- The loading screen polls world.getInitProgress() and transitions
    -- to world_view when all chunks are loaded.
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("loading", {
            statusText = "Loading " .. saveName .. "...",
        })
    end
end

-----------------------------------------------------------
-- Show / Hide / Resize / Shutdown
-----------------------------------------------------------

function mainMenu.show()
    mainMenu.createUI()
    if mainMenu.page then
        UI.showPage(mainMenu.page)
        engine.logDebug("Main menu shown")
    end
end

function mainMenu.hide()
    if mainMenu.page then
        UI.hidePage(mainMenu.page)
        engine.logDebug("Main menu hidden")
    end
end

function mainMenu.onFramebufferResize(width, height)
    mainMenu.fbW = width
    mainMenu.fbH = height
    if mainMenu.uiCreated then
        mainMenu.createUI()
    end
end

function mainMenu.shutdown()
    mainMenu.destroyOwned()
    if mainMenu.page then
        UI.deletePage(mainMenu.page)
    end
end

return mainMenu
