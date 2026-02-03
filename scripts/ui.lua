local ui = {}

local mainMenuPage = nil
local testBox = nil
local boxTexSet = nil
local uiFont = nil

function ui.init(scriptId)
    engine.logInfo("=== UI System Test ===")
    
    -- Load font for text
    uiFont = engine.loadFont("assets/fonts/arcade.ttf", 24)
    
    -- Load all 9 box textures
    local texCenter = engine.loadTexture("assets/textures/box/box.png")
    local texN = engine.loadTexture("assets/textures/box/boxn.png")
    local texS = engine.loadTexture("assets/textures/box/boxs.png")
    local texE = engine.loadTexture("assets/textures/box/boxe.png")
    local texW = engine.loadTexture("assets/textures/box/boxw.png")
    local texNE = engine.loadTexture("assets/textures/box/boxne.png")
    local texNW = engine.loadTexture("assets/textures/box/boxnw.png")
    local texSE = engine.loadTexture("assets/textures/box/boxse.png")
    local texSW = engine.loadTexture("assets/textures/box/boxsw.png")
    
    -- Register the box texture set
    boxTexSet = UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW)
    engine.logInfo("Box texture set registered: " .. tostring(boxTexSet))
    
    -- Create a page
    mainMenuPage = UI.newPage("main_menu", "menu")
    engine.logInfo("Created page: " .. tostring(mainMenuPage))
    
    -- Create a 9-tile box
    -- UI.newBox(name, width, height, boxTexHandle, tileSize, r, g, b, a, pageHandle)
    testBox = UI.newBox("test_box", 600, 200, boxTexSet, 64, 1.0, 1.0, 1.0, 1.0, mainMenuPage)
    engine.logInfo("Created box: " .. tostring(testBox))
    
    -- Add box to page
    UI.addToPage(mainMenuPage, testBox, 100, 100)
    engine.logInfo("Added box to page at (100, 100)")
    
    -- Create a second smaller box
    local box2 = UI.newBox("box2", 150, 100, boxTexSet, 64, 0.8, 0.8, 1.0, 1.0, mainMenuPage)
    UI.addToPage(mainMenuPage, box2, 450, 150)
    engine.logInfo("Created second box at (450, 150)")
    
    -- Create text label inside the first box
    local label = UI.newText("label", "Hello UI!", uiFont, 1.0, 1.0, 1.0, 1.0, mainMenuPage)
    UI.addChild(testBox, label, 80, 80)
    engine.logInfo("Added text label as child of box")

    local testButton = UI.newBox("test_button", 300, 150, boxTexSet, 64, 0.3, 0.6, 0.3, 1.0, mainMenuPage)
    UI.addToPage(mainMenuPage, testButton, 250, 400)
    UI.setZIndex(testButton, 12)  -- Ensure button is on top
    local btnLabel = UI.newText("btn_label", "Click Me", uiFont, 1.0, 1.0, 1.0, 1.0, mainMenuPage)
    UI.addChild(testButton, btnLabel, 50, 86)
    UI.setClickable(testButton, true)
    UI.setOnClick(testButton, "onTestButtonClick")
    -- Add a second button to test z-ordering
    local testButton2 = UI.newBox("test_button2", 300, 150, boxTexSet, 64, 0.6, 0.2, 0.2, 1.0, mainMenuPage)
    UI.addToPage(mainMenuPage, testButton2, 300, 350)  -- Slightly overlapping
    UI.setZIndex(testButton2, 13)  -- Ensure button is on top
    
    local btnLabel2 = UI.newText("btn_label2", "Button 2", uiFont, 1.0, 1.0, 1.0, 1.0, mainMenuPage)
    UI.addChild(testButton2, btnLabel2, 50, 86)
    
    UI.setClickable(testButton2, true)
    UI.setOnClick(testButton2, "onTestButton2Click")

    -- Show the page
    UI.showPage(mainMenuPage)
    engine.logInfo("Page shown")
end

function ui.onTestButton2Click()
    engine.logInfo("BUTTON 2 CLICKED!")
end
 
function ui.onTestButtonClick()
    engine.logInfo("Test button clicked!")
end

function ui.update(dt)
    -- Nothing to update for now
end

function ui.shutdown()
    if mainMenuPage then
        UI.deletePage(mainMenuPage)
        engine.logInfo("UI page deleted")
    end
end

return ui
