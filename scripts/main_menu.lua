-- Main Menu Module (pure module, no global callbacks)
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local button = require("scripts.ui.button")
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
            button.destroyAll()
            panel.destroyAll()
            mainMenu.buttons = {}
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
    
    local baseZ = panel.getZIndex(mainMenu.panelId)
    
    -- Create title above menu panel
    local titleStr = "Ecce Homo"
    local titleWidth = engine.getTextWidth(mainMenu.titleFont, titleStr, s.titleFontSize)
    local titleX = (mainMenu.fbW - titleWidth) / 2
    local titleY = menuY - s.titleOffset
    local titleText = UI.newText("title", titleStr, mainMenu.titleFont, s.titleFontSize, 1.0, 1.0, 1.0, 1.0, mainMenu.page)
    UI.addToPage(mainMenu.page, titleText, titleX, titleY)
    
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
    button.destroyAll()
    panel.destroyAll()
    if mainMenu.page then
        UI.deletePage(mainMenu.page)
    end
end

return mainMenu
