-- Main Menu Module (pure module, no global callbacks)
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local button = require("scripts.ui.button")
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
mainMenu.ownedButtons = {}
mainMenu.ownedPanels  = {}

-- Base sizes (unscaled)
mainMenu.baseSizes = {
    titleFontSize = 96,
    fontSize = 24,
    buttonHeight = 60,
    buttonSpacing = 20,
    buttonTileSize = 64,
    buttonPaddingX = 120,
    menuPaddingX = 80,
    menuPaddingY = 80,
    titleOffset = 60,
}

mainMenu.buttons = {}

mainMenu.menuItems = {
    { name = "create_world", label = "Create World", callback = "onSettings" },
    { name = "settings", label = "Settings", callback = "onSettings" },
    { name = "quit", label = "Quit", callback = "onQuit" }
}

-----------------------------------------------------------
-- Scoped cleanup
-----------------------------------------------------------

function mainMenu.destroyOwned()
    for _, id in ipairs(mainMenu.ownedLabels)  do label.destroy(id) end
    for _, id in ipairs(mainMenu.ownedButtons) do button.destroy(id) end
    for _, id in ipairs(mainMenu.ownedPanels)  do panel.destroy(id) end
    mainMenu.ownedLabels  = {}
    mainMenu.ownedButtons = {}
    mainMenu.ownedPanels  = {}
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
            mainMenu.buttons = {}
            mainMenu.titleLabelId = nil
        end
    end
    
    local uiscale = scale.get()
    local s = scale.applyAll(mainMenu.baseSizes)
    
    engine.logDebug("Creating main menu with framebuffer size: " .. mainMenu.fbW .. " x " .. mainMenu.fbH .. ", scale: " .. uiscale)
    
    mainMenu.page = UI.newPage("main_menu", "menu")

    -- Calculate max text width for button sizing
    local maxLabelWidth = 0
    for i, item in ipairs(mainMenu.menuItems) do
        local labelWidth = engine.getTextWidth(mainMenu.menuFont, item.label, s.fontSize)
        if labelWidth > maxLabelWidth then
            maxLabelWidth = labelWidth
        end
    end
    
    local buttonWidth = maxLabelWidth + s.buttonPaddingX
    
    -- Calculate menu dimensions
    local menuHeight = #mainMenu.menuItems * (s.buttonHeight + s.buttonSpacing) + s.buttonSpacing + s.menuPaddingY
    local menuWidth = buttonWidth + s.menuPaddingX
    
    -- Center the menu panel
    local menuX = (mainMenu.fbW - menuWidth) / 2
    local menuY = (mainMenu.fbH - menuHeight) / 2
    
    -- Create menu panel
    mainMenu.panelId = panel.new({
        name = "menu_panel",
        page = mainMenu.page,
        x = menuX,
        y = menuY,
        width = menuWidth,
        height = menuHeight,
        textureSet = mainMenu.boxTexSet,
        color = {1.0, 1.0, 1.0, 1.0},
        tileSize = s.buttonTileSize,
        zIndex = 1,
        padding = { top = s.menuPaddingY / 2, bottom = s.menuPaddingY / 2, left = s.menuPaddingX / 2, right = s.menuPaddingX / 2 },
        uiscale = 1.0,  -- Already scaled above
    })
    table.insert(mainMenu.ownedPanels, mainMenu.panelId)
    
    local baseZ = panel.getZIndex(mainMenu.panelId)
    
    -- Create title above menu panel using label component
    mainMenu.titleLabelId = label.new({
        name = "title",
        text = "Ecce Homo",
        font = mainMenu.titleFont,
        fontSize = mainMenu.baseSizes.titleFontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = mainMenu.page,
        uiscale = uiscale,
    })
    table.insert(mainMenu.ownedLabels, mainMenu.titleLabelId)
    
    local titleW, titleH = label.getSize(mainMenu.titleLabelId)
    local titleX = (mainMenu.fbW - titleW) / 2
    local titleY = menuY - s.titleOffset
    UI.addToPage(mainMenu.page, label.getElementHandle(mainMenu.titleLabelId), titleX, titleY)
    
    -- Create menu buttons and collect them for placeColumn
    local buttonElements = {}
    local buttonSizes = {}
    
    for i, item in ipairs(mainMenu.menuItems) do
        local labelWidth = engine.getTextWidth(mainMenu.menuFont, item.label, s.fontSize)
        local thisButtonWidth = labelWidth + s.buttonPaddingX
        
        local btnId = button.new({
            name = item.name,
            text = item.label,
            width = thisButtonWidth / uiscale,  -- Base size (button.new will scale it)
            height = mainMenu.baseSizes.buttonHeight,
            fontSize = mainMenu.baseSizes.fontSize,
            uiscale = uiscale,
            page = mainMenu.page,
            font = mainMenu.menuFont,
            textureSet = mainMenu.boxTexSet,
            tileSize = s.buttonTileSize,
            bgColor = {1.0, 1.0, 1.0, 1.0},
            textColor = {1.0, 1.0, 1.0, 1.0},
            callbackName = item.callback,
        })
        table.insert(mainMenu.ownedButtons, btnId)
        
        local btnW, btnH = button.getSize(btnId)
        
        table.insert(buttonElements, button.getElementHandle(btnId))
        table.insert(buttonSizes, { width = btnW, height = btnH })
        
        UI.setZIndex(button.getElementHandle(btnId), baseZ + 1)
        
        mainMenu.buttons[item.name] = btnId
    end
    
    -- Place buttons in a centered column
    panel.placeColumn(
        mainMenu.panelId,
        buttonElements,
        buttonSizes,
        {
            x = "50%",
            y = "50%",
            origin = "center",
            spacing = s.buttonSpacing,
        }
    )
    
    mainMenu.uiCreated = true
    engine.logDebug("Main menu created with " .. #mainMenu.menuItems .. " buttons")
end

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
    engine.logDebug("Main menu onFramebufferResize: " .. width .. " x " .. height)
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
