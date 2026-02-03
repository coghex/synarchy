-- Main Menu module
local mainMenu = {}

-- State
local page = nil
local boxTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

-- Track loading state
local fontsLoaded = {
    menu = false,
    title = false
}
local fontsReady = false
local menuFontHandle = nil
local titleFontHandle = nil

-- UI element handles
local titleText = nil
local menuBox = nil
local buttons = {}

-- Menu configuration
local menuItems = {
    { name = "create_world", label = "Create World", callback = "onCreateWorld" },
    { name = "settings", label = "Settings", callback = "onSettings" },
    { name = "quit", label = "Quit", callback = "onQuit" }
}

-- Layout configuration
local buttonWidth = 300
local buttonHeight = 60
local buttonSpacing = 20
local buttonTileSize = 64
local buttonPaddingX = 120
local menuPaddingX = 80
local menuPaddingY = 80

function mainMenu.init(scriptId)
    -- Load fonts - store handles for comparison in onAssetLoaded
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    
    -- Also store in menuFont/titleFont for use in createUI
    menuFont = menuFontHandle
    titleFont = titleFontHandle
    
    -- Load box textures
    local texCenter = engine.loadTexture("assets/textures/box/box.png")
    local texN = engine.loadTexture("assets/textures/box/boxn.png")
    local texS = engine.loadTexture("assets/textures/box/boxs.png")
    local texE = engine.loadTexture("assets/textures/box/boxe.png")
    local texW = engine.loadTexture("assets/textures/box/boxw.png")
    local texNE = engine.loadTexture("assets/textures/box/boxne.png")
    local texNW = engine.loadTexture("assets/textures/box/boxnw.png")
    local texSE = engine.loadTexture("assets/textures/box/boxse.png")
    local texSW = engine.loadTexture("assets/textures/box/boxsw.png")
    
    boxTexSet = UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW)
    
    engine.logInfo("Main menu initialized, waiting for assets...")
end

function mainMenu.checkReady()
    engine.logInfo("checkReady: fontsReady=" .. tostring(fontsReady) .. " fbW=" .. fbW .. " fbH=" .. fbH .. " uiCreated=" .. tostring(uiCreated))
    if fontsReady and fbW > 0 and fbH > 0 and not uiCreated then
        mainMenu.createUI()
    end
end

function mainMenu.onAssetLoaded(assetType, handle, path)
    engine.logInfo("Asset loaded: " .. assetType .. " handle=" .. handle .. " path=" .. path)
    
    if assetType == "font" then
        if handle == menuFontHandle then
            fontsLoaded.menu = true
            engine.logInfo("Menu font ready")
        elseif handle == titleFontHandle then
            fontsLoaded.title = true
            engine.logInfo("Title font ready")
        end
        
        -- Check if all fonts are loaded
        if fontsLoaded.menu and fontsLoaded.title then
            fontsReady = true
            engine.logInfo("All fonts loaded!")
            mainMenu.checkReady()
        end
    end
end

function mainMenu.onFramebufferResize(width, height)
    engine.logInfo("Main menu onFramebufferResize: " .. width .. " x " .. height)
    fbW = width
    fbH = height
    
    if uiCreated then
        -- Recreate UI with new dimensions
        mainMenu.createUI()
    else
        -- Check if we can create UI now
        mainMenu.checkReady()
    end
end

function mainMenu.createUI()
    if uiCreated then
        -- Clean up old UI first
        if page then
            UI.deletePage(page)
            buttons = {}
        end
    end
    
    engine.logInfo("Creating main menu with framebuffer size: " .. fbW .. " x " .. fbH)
    
    -- Create the menu page
    page = UI.newPage("main_menu", "menu")

    -- Calculate max text width across all buttons
    local maxLabelWidth = 0
    for i, item in ipairs(menuItems) do
        local labelWidth = engine.getTextWidth(menuFont, item.label)
        if labelWidth > maxLabelWidth then
            maxLabelWidth = labelWidth
        end
    end
    
    local buttonWidth = maxLabelWidth + buttonPaddingX
    
    -- Calculate menu dimensions
    local menuHeight = #menuItems * (buttonHeight + buttonSpacing) + buttonSpacing + menuPaddingY
    local menuWidth = buttonWidth + menuPaddingX
    
    -- Center the menu box (Y=0 at top, Y increases downward)
    local menuX = (fbW - menuWidth) / 2
    local menuY = (fbH - menuHeight) / 2
    
    -- Create menu container box
    menuBox = UI.newBox("menu_box", menuWidth, menuHeight, boxTexSet, buttonTileSize, 0.2, 0.2, 0.3, 0.9, page)
    UI.addToPage(page, menuBox, menuX, menuY)
    UI.setZIndex(menuBox, 1)
    
    -- Create title ABOVE menu box (smaller Y value = higher on screen)
    local titleStr = "Ecce Homo"
    local titleWidth = engine.getTextWidth(titleFont, titleStr)
    local titleX = (fbW - titleWidth) / 2
    local titleY = menuY - 60
    titleText = UI.newText("title", titleStr, titleFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, titleText, titleX, titleY)
    
    -- Create menu buttons inside the menu box
    -- Buttons stack from top to bottom (Y increases downward)
    local buttonX = (menuWidth - buttonWidth) / 2
    
    for i, item in ipairs(menuItems) do
        local buttonOffsetX = menuPaddingX / 2
        local buttonOffsetY = menuPaddingY / 2
        local buttonY = buttonSpacing + (i - 1) * (buttonHeight + buttonSpacing) + buttonOffsetY
        -- Center text in button
        local labelWidth = engine.getTextWidth(menuFont, item.label)
        local thisButtonWidth = labelWidth + buttonPaddingX
        local thisButtonX = (menuWidth - thisButtonWidth) / 2
        local btn = UI.newBox(item.name, thisButtonWidth, buttonHeight, boxTexSet, buttonTileSize, 0.3, 0.4, 0.5, 1.0, page)
        UI.addChild(menuBox, btn, thisButtonX, buttonY)
        UI.setZIndex(btn, 2)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, item.callback)
        local labelX = (thisButtonWidth - labelWidth) / 2
        local labelY = (buttonHeight / 2) + 8
        local label = UI.newText(item.name .. "_label", item.label, menuFont, 1.0, 1.0, 1.0, 1.0, page)
        UI.addChild(btn, label, labelX, labelY)
        
        buttons[item.name] = {
            box = btn,
            label = label
        }
    end
    
    -- Show the menu
    UI.showPage(page)
    uiCreated = true
    
    engine.logInfo("Main menu created with " .. #menuItems .. " buttons")
end

function mainMenu.update(dt)
end

-- Button callbacks
function mainMenu.onCreateWorld()
    engine.logInfo("Create World selected")
    -- TODO: Start world creation
end

function mainMenu.onSettings()
    engine.logInfo("Settings selected")
    -- TODO: Show settings menu
end

function mainMenu.onQuit()
    engine.logInfo("Quit selected")
    engine.quit()
end

function mainMenu.show()
    if page then
        UI.showPage(page)
    end
end

function mainMenu.hide()
    if page then
        UI.hidePage(page)
    end
end

function mainMenu.shutdown()
    if page then
        UI.deletePage(page)
    end
end

return mainMenu
