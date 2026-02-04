-- Main Menu Module (pure module, no global callbacks)
local mainMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local titleFontSize = 96
local fontSize = 24
local uiCreated = false
local uiscale = 1.0

local buttons = {}

local menuItems = {
    { name = "create_world", label = "Create World", callback = "onSettings" },
    { name = "settings", label = "Settings", callback = "onSettings" },
    { name = "quit", label = "Quit", callback = "onQuit" }
}

local buttonWidth = 300
local buttonHeight = 60
local buttonSpacing = 20
local buttonTileSize = 64
local buttonPaddingX = 120
local menuPaddingX = 80
local menuPaddingY = 80

function mainMenu.init(boxTex, font, tFont, width, height)
    boxTexSet = boxTex
    menuFont = font
    titleFont = tFont
    fbW = width
    fbH = height
    uiscale = engine.getUIScale()
    fontSize = math.floor(fontSize * uiscale)
    titleFontSize = math.floor(titleFontSize * uiscale)
    buttonWidth = math.floor(buttonWidth * uiscale)
    buttonHeight = math.floor(buttonHeight * uiscale)
    buttonSpacing = math.floor(buttonSpacing * uiscale)
    buttonTileSize = math.floor(buttonTileSize * uiscale)
    buttonPaddingX = math.floor(buttonPaddingX * uiscale)
    menuPaddingX = math.floor(menuPaddingX * uiscale)
    menuPaddingY = math.floor(menuPaddingY * uiscale)
    
    mainMenu.createUI()
end

function mainMenu.createUI()
    if uiCreated then
        if page then
            UI.deletePage(page)
            buttons = {}
        end
    end
    
    engine.logDebug("Creating main menu with framebuffer size: " .. fbW .. " x " .. fbH)
    
    page = UI.newPage("main_menu", "menu")

    -- Calculate max text width
    local maxLabelWidth = 0
    for i, item in ipairs(menuItems) do
        local labelWidth = engine.getTextWidth(menuFont, item.label, fontSize)
        if labelWidth > maxLabelWidth then
            maxLabelWidth = labelWidth
        end
    end
    
    local buttonWidth = maxLabelWidth + buttonPaddingX
    
    -- Calculate menu dimensions
    local menuHeight = #menuItems * (buttonHeight + buttonSpacing) + buttonSpacing + menuPaddingY
    local menuWidth = buttonWidth + menuPaddingX
    
    -- Center the menu box
    local menuX = (fbW - menuWidth) / 2
    local menuY = (fbH - menuHeight) / 2
    
    -- Create menu container box
    local menuBox = UI.newBox("menu_box", menuWidth, menuHeight, boxTexSet, buttonTileSize, 0.2, 0.2, 0.3, 0.9, page)
    UI.addToPage(page, menuBox, menuX, menuY)
    UI.setZIndex(menuBox, 1)
    
    -- Create title above menu box
    local titleStr = "Ecce Homo"
    local titleWidth = engine.getTextWidth(titleFont, titleStr, titleFontSize)
    local titleX = (fbW - titleWidth) / 2
    local titleY = menuY - 60
    local titleText = UI.newText("title", titleStr, titleFont, titleFontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, titleText, titleX, titleY)
    
    -- Create menu buttons inside the menu box
    local buttonX = (menuWidth - buttonWidth) / 2
    
    for i, item in ipairs(menuItems) do
        local buttonOffsetX = menuPaddingX / 2
        local buttonOffsetY = menuPaddingY / 2
        local buttonY = buttonSpacing + (i - 1) * (buttonHeight + buttonSpacing) + buttonOffsetY
        
        local labelWidth = engine.getTextWidth(menuFont, item.label, fontSize)
        local thisButtonWidth = labelWidth + buttonPaddingX
        local thisButtonX = (menuWidth - thisButtonWidth) / 2
        local btn = UI.newBox(item.name, thisButtonWidth, buttonHeight, boxTexSet, buttonTileSize, 0.3, 0.4, 0.5, 1.0, page)
        UI.addChild(menuBox, btn, thisButtonX, buttonY)
        UI.setZIndex(btn, 2)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, item.callback)
        
        local labelX = (thisButtonWidth - labelWidth) / 2
        local labelY = (buttonHeight / 2) + 8
        local label = UI.newText(item.name .. "_label", item.label, menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
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
