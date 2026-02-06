-- Main Menu Module (pure module, no global callbacks)
local scale = require("scripts.ui.scale")
local mainMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

-- Base sizes (unscaled)
local baseSizes = {
    titleFontSize = 96,
    fontSize = 24,
    buttonWidth = 300,
    buttonHeight = 60,
    buttonSpacing = 20,
    buttonTileSize = 64,
    buttonPaddingX = 120,
    menuPaddingX = 80,
    menuPaddingY = 80,
    titleOffset = 60,
    labelOffsetY = 8,
}

local buttons = {}

local menuItems = {
    { name = "create_world", label = "Create World", callback = "onSettings" },
    { name = "settings", label = "Settings", callback = "onSettings" },
    { name = "quit", label = "Quit", callback = "onQuit" }
}

function mainMenu.init(boxTex, btnTex, font, tFont, width, height)
    boxTexSet = boxTex
    menuFont = font
    titleFont = tFont
    fbW = width
    fbH = height
    
    mainMenu.createUI()
end

function mainMenu.createUI()
    if uiCreated then
        if page then
            UI.deletePage(page)
            buttons = {}
        end
    end
    
    -- Apply scaling to all base sizes
    local s = scale.applyAll(baseSizes)
    
    engine.logDebug("Creating main menu with framebuffer size: " .. fbW .. " x " .. fbH .. ", scale: " .. scale.get())
    
    page = UI.newPage("main_menu", "menu")

    -- Calculate max text width
    local maxLabelWidth = 0
    for i, item in ipairs(menuItems) do
        local labelWidth = engine.getTextWidth(menuFont, item.label, s.fontSize)
        if labelWidth > maxLabelWidth then
            maxLabelWidth = labelWidth
        end
    end
    
    local buttonWidth = maxLabelWidth + s.buttonPaddingX
    
    -- Calculate menu dimensions
    local menuHeight = #menuItems * (s.buttonHeight + s.buttonSpacing) + s.buttonSpacing + s.menuPaddingY
    local menuWidth = buttonWidth + s.menuPaddingX
    
    -- Center the menu box
    local menuX = (fbW - menuWidth) / 2
    local menuY = (fbH - menuHeight) / 2
    
    -- Create menu container box
    local menuBox = UI.newBox("menu_box", menuWidth, menuHeight, boxTexSet, s.buttonTileSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, menuBox, menuX, menuY)
    UI.setZIndex(menuBox, 1)
    
    -- Create title above menu box
    local titleStr = "Ecce Homo"
    local titleWidth = engine.getTextWidth(titleFont, titleStr, s.titleFontSize)
    local titleX = (fbW - titleWidth) / 2
    local titleY = menuY - s.titleOffset
    local titleText = UI.newText("title", titleStr, titleFont, s.titleFontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, titleText, titleX, titleY)
    
    -- Create menu buttons inside the menu box
    for i, item in ipairs(menuItems) do
        local buttonOffsetY = s.menuPaddingY / 2
        local buttonY = s.buttonSpacing + (i - 1) * (s.buttonHeight + s.buttonSpacing) + buttonOffsetY
        
        local labelWidth = engine.getTextWidth(menuFont, item.label, s.fontSize)
        local thisButtonWidth = labelWidth + s.buttonPaddingX
        local thisButtonX = (menuWidth - thisButtonWidth) / 2
        local btn = UI.newBox(item.name, thisButtonWidth, s.buttonHeight, boxTexSet, s.buttonTileSize, 1.0, 1.0, 1.0, 1.0, page)
        UI.addChild(menuBox, btn, thisButtonX, buttonY)
        UI.setZIndex(btn, 2)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, item.callback)
        
        local labelX = (thisButtonWidth - labelWidth) / 2
        local labelY = (s.buttonHeight / 2) + s.labelOffsetY
        local label = UI.newText(item.name .. "_label", item.label, menuFont, s.fontSize, 1.0, 1.0, 1.0, 1.0, page)
        UI.addChild(btn, label, labelX, labelY)
        
        buttons[item.name] = {
            box = btn,
            label = label
        }
    end
    
    uiCreated = true
    engine.logDebug("Main menu created with " .. #menuItems .. " buttons")
end

function mainMenu.show()
    mainMenu.createUI()
    if page then
        UI.showPage(page)
        engine.logDebug("Main menu shown")
    end
end

function mainMenu.hide()
    if page then
        UI.hidePage(page)
        engine.logDebug("Main menu hidden")
    end
end

function mainMenu.onFramebufferResize(width, height)
    engine.logDebug("Main menu onFramebufferResize: " .. width .. " x " .. height)
    fbW = width
    fbH = height
    
    if uiCreated then
        mainMenu.createUI()
    end
end

function mainMenu.shutdown()
    if page then
        UI.deletePage(page)
    end
end

return mainMenu
