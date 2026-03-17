-- Pause Menu Module (in-game escape menu)
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local pauseMenu = {}

pauseMenu.page = nil
pauseMenu.panelId = nil
pauseMenu.boxTexSet = nil
pauseMenu.menuFont = nil
pauseMenu.titleFont = nil
pauseMenu.fbW = 0
pauseMenu.fbH = 0
pauseMenu.uiCreated = false
pauseMenu.visible = false

pauseMenu.titleLabelId = nil

-- Owned element IDs for scoped cleanup
pauseMenu.ownedLabels  = {}
pauseMenu.ownedBoxes   = {}
pauseMenu.ownedPanels  = {}

-- Click dispatch table: elemHandle -> function
pauseMenu.clickHandlers = {}

-- Callback to switch menus (set by ui_manager)
pauseMenu.showMenuCallback = nil

-- Base sizes (same as main menu)
pauseMenu.baseSizes = {
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

function pauseMenu.buildMenuItems()
    local items = {}

    if pauseMenu.showSave then
        table.insert(items, {
            name = "save",
            label = "Save",
            onClick = function() pauseMenu.onSave() end,
        })
    end

    table.insert(items, {
        name = "settings",
        label = "Settings",
        onClick = function() pauseMenu.onSettings() end,
    })
    table.insert(items, {
        name = "exit_to_menu",
        label = "Exit to Menu",
        onClick = function() pauseMenu.onExitToMenu() end,
    })
    table.insert(items, {
        name = "exit_to_desktop",
        label = "Exit to Desktop",
        onClick = function() pauseMenu.onExitToDesktop() end,
    })

    return items
end

-----------------------------------------------------------
-- Scoped cleanup
-----------------------------------------------------------

function pauseMenu.destroyOwned()
    for _, id in ipairs(pauseMenu.ownedLabels) do label.destroy(id) end
    for _, h  in ipairs(pauseMenu.ownedBoxes)  do UI.deleteElement(h) end
    for _, id in ipairs(pauseMenu.ownedPanels) do panel.destroy(id) end
    pauseMenu.ownedLabels  = {}
    pauseMenu.ownedBoxes   = {}
    pauseMenu.ownedPanels  = {}
    pauseMenu.clickHandlers = {}
end

function pauseMenu.setShowMenuCallback(callback)
    pauseMenu.showMenuCallback = callback
end

function pauseMenu.init(boxTex, btnTex, font, tFont, width, height)
    pauseMenu.boxTexSet = boxTex
    pauseMenu.menuFont = font
    pauseMenu.titleFont = tFont
    pauseMenu.fbW = width
    pauseMenu.fbH = height
end

function pauseMenu.createUI()
    if pauseMenu.uiCreated then
        if pauseMenu.page then
            UI.deletePage(pauseMenu.page)
            pauseMenu.destroyOwned()
            pauseMenu.titleLabelId = nil
        end
    end

    local menuItems = pauseMenu.buildMenuItems()

    local uiscale = scale.get()
    local s = scale.applyAll(pauseMenu.baseSizes)

    pauseMenu.page = UI.newPage("pause_menu", "modal")

    -- Calculate max text width for sizing
    local maxLabelWidth = 0
    for _, item in ipairs(menuItems) do
        local w = engine.getTextWidth(pauseMenu.menuFont, item.label, s.fontSize)
        if w > maxLabelWidth then maxLabelWidth = w end
    end

    local itemWidth  = maxLabelWidth + s.buttonPaddingX
    local menuHeight = #menuItems * (s.buttonHeight + s.buttonSpacing)
                     + s.buttonSpacing + s.menuPaddingY
    local menuWidth  = itemWidth + s.menuPaddingX

    local menuX = (pauseMenu.fbW - menuWidth) / 2
    local menuY = (pauseMenu.fbH - menuHeight) / 2

    -- Background panel
    pauseMenu.panelId = panel.new({
        name       = "pause_panel",
        page       = pauseMenu.page,
        x = menuX, y = menuY,
        width      = menuWidth,
        height     = menuHeight,
        textureSet = pauseMenu.boxTexSet,
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
    table.insert(pauseMenu.ownedPanels, pauseMenu.panelId)

    local baseZ = panel.getZIndex(pauseMenu.panelId)

    -- Title
    pauseMenu.titleLabelId = label.new({
        name     = "pause_title",
        text     = "Paused",
        font     = pauseMenu.titleFont,
        fontSize = pauseMenu.baseSizes.titleFontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = pauseMenu.page,
        uiscale  = uiscale,
    })
    table.insert(pauseMenu.ownedLabels, pauseMenu.titleLabelId)

    local titleW, _ = label.getSize(pauseMenu.titleLabelId)
    local titleX = (pauseMenu.fbW - titleW) / 2
    local titleY = menuY - s.titleOffset
    UI.addToPage(pauseMenu.page,
        label.getElementHandle(pauseMenu.titleLabelId), titleX, titleY)

    -- Menu items
    local boxElements = {}
    local boxSizes    = {}

    for _, item in ipairs(menuItems) do
        local lblW = engine.getTextWidth(pauseMenu.menuFont, item.label, s.fontSize)
        local thisW = lblW + s.buttonPaddingX
        local thisH = s.buttonHeight

        local boxH = UI.newBox(
            item.name .. "_pause_box",
            thisW, thisH,
            pauseMenu.boxTexSet,
            s.buttonTileSize,
            1.0, 1.0, 1.0, 1.0,
            s.buttonOverflow,
            pauseMenu.page
        )
        table.insert(pauseMenu.ownedBoxes, boxH)

        UI.setClickable(boxH, true)
        UI.setOnClick(boxH, "onPauseMenuItem")

        pauseMenu.clickHandlers[boxH] = item.onClick

        local textH = UI.newText(
            item.name .. "_pause_label",
            item.label,
            pauseMenu.menuFont,
            s.fontSize,
            1.0, 1.0, 1.0, 1.0,
            pauseMenu.page
        )

        local labelX = (thisW - lblW) / 2
        local labelY = (thisH / 2) + (s.fontSize / 2)
        UI.addChild(boxH, textH, labelX, labelY)
        UI.setZIndex(textH, 1)

        UI.setZIndex(boxH, baseZ + 1)

        table.insert(boxElements, boxH)
        table.insert(boxSizes, { width = thisW, height = thisH })
    end

    panel.placeColumn(
        pauseMenu.panelId,
        boxElements,
        boxSizes,
        {
            x = "50%",
            y = "50%",
            origin = "center",
            spacing = s.buttonSpacing,
        }
    )

    pauseMenu.uiCreated = true
end

-----------------------------------------------------------
-- Click dispatch (called from ui_manager.onPauseMenuItem)
-----------------------------------------------------------

function pauseMenu.handleClick(elemHandle)
    local handler = pauseMenu.clickHandlers[elemHandle]
    if handler then
        handler()
        return true
    end
    return false
end

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function pauseMenu.onSave()
    engine.logInfo("Pause menu: Save")
    local worldManager = require("scripts.world_manager")
    if worldManager.currentWorld then
        engine.save(worldManager.currentWorld)
    end
    pauseMenu.hide()
end

function pauseMenu.onSettings()
    engine.logInfo("Pause menu: Settings")
    if pauseMenu.showMenuCallback then
        pauseMenu.showMenuCallback("settings")
    end
end

function pauseMenu.onExitToMenu()
    engine.logInfo("Pause menu: Exit to Menu")
    local worldManager = require("scripts.world_manager")
    if worldManager.currentWorld then
        world.hide(worldManager.currentWorld)
        worldManager.active = false
        worldManager.currentWorld = nil
    end
    if pauseMenu.showMenuCallback then
        pauseMenu.showMenuCallback("main")
    end
end

function pauseMenu.onExitToDesktop()
    engine.quit()
end

-----------------------------------------------------------
-- Show / Hide / Toggle
-----------------------------------------------------------

function pauseMenu.show(opts)
    opts = opts or {}
    if opts.showSave ~= nil then
        pauseMenu.showSave = opts.showSave
    end
    pauseMenu.createUI()
    if pauseMenu.page then
        UI.showPage(pauseMenu.page)
        pauseMenu.visible = true
    end
end

function pauseMenu.hide()
    if pauseMenu.page then
        UI.hidePage(pauseMenu.page)
        pauseMenu.visible = false
    end
end

function pauseMenu.toggle(opts)
    if pauseMenu.visible then
        pauseMenu.hide()
    else
        pauseMenu.show(opts)
    end
end

function pauseMenu.onFramebufferResize(width, height)
    pauseMenu.fbW = width
    pauseMenu.fbH = height
    if pauseMenu.uiCreated and pauseMenu.visible then
        pauseMenu.createUI()
        UI.showPage(pauseMenu.page)
    end
end

function pauseMenu.shutdown()
    pauseMenu.destroyOwned()
    if pauseMenu.page then
        UI.deletePage(pauseMenu.page)
    end
end

return pauseMenu
