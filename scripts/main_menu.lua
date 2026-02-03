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
local fontsReady = false  -- FIX: was "fontReady"
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

function mainMenu.init(scriptId)
    -- Load fonts - store handles for comparison in onAssetLoaded
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 48)
    
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
    
    -- Calculate menu dimensions
    local menuHeight = #menuItems * (buttonHeight + buttonSpacing) + buttonSpacing
    local menuWidth = buttonWidth + 60
    
    -- Center the menu box on screen
    local menuX = (fbW - menuWidth) / 2
    local menuY = (fbH - menuHeight) / 2
    
    -- Create menu container box
    menuBox = UI.newBox("menu_box", menuWidth, menuHeight, boxTexSet, buttonTileSize, 0.2, 0.2, 0.3, 0.9, page)
    UI.addToPage(page, menuBox, menuX, menuY)
    UI.setZIndex(menuBox, 1)
    
    -- Create title above the menu box
    local titleStr = "Ecce Homo"
    local titleWidth = engine.getTextWidth(titleFont, titleStr)
    local titleX = (fbW - titleWidth) / 2
    local titleY = menuY - 60  -- Above the menu box
    titleText = UI.newText("title", titleStr, titleFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, titleText, titleX, titleY)
    
    -- Create menu buttons inside the menu box
    local buttonX = (menuWidth - buttonWidth) / 2
    
    for i, item in ipairs(menuItems) do
        local buttonY = buttonSpacing + (i - 1) * (buttonHeight + buttonSpacing)
        
        local btn = UI.newBox(item.name, buttonWidth, buttonHeight, boxTexSet, buttonTileSize, 0.3, 0.4, 0.5, 1.0, page)
        UI.addChild(menuBox, btn, buttonX, buttonY)
        UI.setZIndex(btn, 2)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, item.callback)
        
        -- Center text in button
        local labelWidth = engine.getTextWidth(menuFont, item.label)
        local labelX = (buttonWidth - labelWidth) / 2
        local labelY = (buttonHeight / 2) + 48
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
end

function mainMenu.onSettings()
    engine.logInfo("Settings selected")
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
