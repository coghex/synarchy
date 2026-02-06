-- Main Menu Module (pure module, no global callbacks)
local mainMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

-- Base sizes (unscaled)
local baseTitleFontSize = 96
local baseFontSize = 24
local baseButtonWidth = 300
local baseButtonHeight = 60
local baseButtonSpacing = 20
local baseButtonTileSize = 64
local baseButtonPaddingX = 120
local baseMenuPaddingX = 80
local baseMenuPaddingY = 80

-- Scaled sizes (computed in createUI)
local titleFontSize = 96
local fontSize = 24
local buttonWidth = 300
local buttonHeight = 60
local buttonSpacing = 20
local buttonTileSize = 64
local buttonPaddingX = 120
local menuPaddingX = 80
local menuPaddingY = 80

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
    
    -- Get current UI scale and apply to all sizes
    local uiscale = engine.getUIScale()
    fontSize = math.floor(baseFontSize * uiscale)
    titleFontSize = math.floor(baseTitleFontSize * uiscale)
    buttonWidth = math.floor(baseButtonWidth * uiscale)
    buttonHeight = math.floor(baseButtonHeight * uiscale)
    buttonSpacing = math.floor(baseButtonSpacing * uiscale)
    buttonTileSize = math.floor(baseButtonTileSize * uiscale)
    buttonPaddingX = math.floor(baseButtonPaddingX * uiscale)
    menuPaddingX = math.floor(baseMenuPaddingX * uiscale)
    menuPaddingY = math.floor(baseMenuPaddingY * uiscale)
    
    engine.logDebug("Creating main menu with framebuffer size: " .. fbW .. " x " .. fbH .. ", scale: " .. uiscale)
    
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
    local menuBox = UI.newBox("menu_box", menuWidth, menuHeight, boxTexSet, buttonTileSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, menuBox, menuX, menuY)
    UI.setZIndex(menuBox, 1)
    
    -- Create title above menu box
    local titleStr = "Ecce Homo"
    local titleWidth = engine.getTextWidth(titleFont, titleStr, titleFontSize)
    local titleX = (fbW - titleWidth) / 2
    local titleY = menuY - math.floor(60 * uiscale)
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
        local btn = UI.newBox(item.name, thisButtonWidth, buttonHeight, boxTexSet, buttonTileSize, 1.0, 1.0, 1.0, 1.0, page)
        UI.addChild(menuBox, btn, thisButtonX, buttonY)
        UI.setZIndex(btn, 2)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, item.callback)
        
        local labelX = (thisButtonWidth - labelWidth) / 2
        local labelY = (buttonHeight / 2) + math.floor(8 * uiscale)
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
    -- Rebuild UI to pick up any scale changes
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
