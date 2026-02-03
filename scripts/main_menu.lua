-- Main Menu module
local mainMenu = {}

-- State
local page = nil
local boxTexSet = nil
local menuFont = nil
local titleFont = nil

-- UI element handles
local titleText = nil
local menuBox = nil
local buttons = {}

-- Menu configuration
local menuItems = {
    { name = "new_game", label = "New Game", callback = "onNewGame" },
    { name = "options", label = "Options", callback = "onOptions" },
    { name = "quit", label = "Quit", callback = "onQuit" }
}

-- Layout configuration
local buttonWidth = 300
local buttonHeight = 60
local buttonSpacing = 20
local buttonTileSize = 64

function mainMenu.init(scriptId)
    -- Load fonts
    menuFont = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFont = engine.loadFont("assets/fonts/gothic.ttf", 48)
    
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
    
    -- Create the menu page
    page = UI.newPage("main_menu", "menu")
    
    -- Get framebuffer dimensions for layout
    -- Coordinate system: origin at BOTTOM-LEFT, Y increases UPWARD
    local fbW, fbH = engine.getFramebufferSize()
    
    -- Calculate menu dimensions
    local menuHeight = #menuItems * (buttonHeight + buttonSpacing) + buttonSpacing
    local menuWidth = buttonWidth + 60
    
    -- Center the menu box on screen
    -- menuX is distance from left edge
    -- menuY is distance from BOTTOM edge (box extends upward from this point)
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
    -- Title goes above the menu box (menuY + menuHeight + some padding)
    local titleY = menuY + menuHeight + 40
    titleText = UI.newText("title", titleStr, titleFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, titleText, titleX, titleY)
    
    -- Create menu buttons inside the menu box
    -- Child positions are relative to parent's bottom-left corner
    -- Buttons stack from bottom to top
    local buttonX = (menuWidth - buttonWidth) / 2
    
    -- Start from top of box and work down, but remember Y=0 is at bottom
    -- So first button (New Game) should be at the TOP of the box
    for i, item in ipairs(menuItems) do
        -- Calculate Y from top of menu box, going downward
        -- Top of box is at menuHeight, first button starts at menuHeight - buttonSpacing - buttonHeight
        local buttonY = menuHeight - (i * (buttonHeight + buttonSpacing))
        
        local btn = UI.newBox(item.name, buttonWidth, buttonHeight, boxTexSet, buttonTileSize, 0.3, 0.4, 0.5, 1.0, page)
        UI.addChild(menuBox, btn, buttonX, buttonY)
        UI.setZIndex(btn, 2)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, item.callback)
        
        -- Center text in button
        -- Text baseline is at the Y position, so we need to offset for proper centering
        local labelWidth = engine.getTextWidth(menuFont, item.label)
        local labelX = (buttonWidth - labelWidth) / 2
        -- Text Y should be near the vertical center of the button
        -- Since text renders from baseline upward, place it at roughly 1/3 from bottom
        local labelY = buttonHeight / 3
        local label = UI.newText(item.name .. "_label", item.label, menuFont, 1.0, 1.0, 1.0, 1.0, page)
        UI.addChild(btn, label, labelX, labelY)
        
        buttons[item.name] = {
            box = btn,
            label = label
        }
    end
    
    -- Show the menu
    UI.showPage(page)
end

function mainMenu.update(dt)
end

-- Button callbacks
function mainMenu.onNewGame()
    engine.logInfo("New Game selected")
end

function mainMenu.onOptions()
    engine.logInfo("Options selected")
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
