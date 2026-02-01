local ui = {}

local mainMenuPage = nil
local testBox = nil
local boxTex = nil

function ui.init(scriptId)
    engine.logInfo("=== UI System Test ===")
    
    -- Load box texture (use existing tile or a solid color texture)
    boxTex = engine.loadTexture("assets/textures/box/box.png")
    
    -- Set default box texture for UI system
    UI.setDefaultBoxTexture(boxTex)
    
    -- Create a test page
    mainMenuPage = UI.newPage("main_menu", "menu")
    engine.logInfo("Created page: " .. tostring(mainMenuPage))
    
    -- Create a test box (red, semi-transparent)
    testBox = UI.newBox("test_box", 200, 100, 0.8, 0.2, 0.2, 0.9, mainMenuPage)
    engine.logInfo("Created box: " .. tostring(testBox))
    
    -- Add box to page at position (100, 100)
    UI.addToPage(mainMenuPage, testBox, 100, 100)
    engine.logInfo("Added box to page at (100, 100)")
    
    -- Create a second box (blue)
    local box2 = UI.newBox("box2", 150, 75, 0.2, 0.2, 0.8, 0.9, mainMenuPage)
    UI.addToPage(mainMenuPage, box2, 350, 150)
    engine.logInfo("Created second box at (350, 150)")
    
    -- Create a third box (green) as child of first box
    local box3 = UI.newBox("box3", 50, 50, 0.2, 0.8, 0.2, 0.9, mainMenuPage)
    UI.addChild(testBox, box3, 25, 25)
    engine.logInfo("Created child box inside first box")
    
    -- Show the page
    UI.showPage(mainMenuPage)
    engine.logInfo("Page shown")
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
