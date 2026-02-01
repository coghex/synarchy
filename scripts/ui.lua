local ui = {}

local mainMenuPage = nil
local testBox = nil
local boxTex = nil

function ui.init(scriptId)
    engine.logInfo("=== UI System Test ===")
    
    boxTex = engine.loadTexture("assets/textures/box/box.png")
    UI.setDefaultBoxTexture(boxTex)
    
    -- Load a font for UI text
    local uiFont = engine.loadFont("assets/fonts/Cabal.ttf", 24)
    
    mainMenuPage = UI.newPage("main_menu", "menu")
    
    -- Create boxes
    testBox = UI.newBox("test_box", 200, 100, 0.8, 0.2, 0.2, 0.9, mainMenuPage)
    UI.addToPage(mainMenuPage, testBox, 100, 100)
    
    local box2 = UI.newBox("box2", 150, 75, 0.2, 0.2, 0.8, 0.9, mainMenuPage)
    UI.addToPage(mainMenuPage, box2, 350, 150)
    
    local box3 = UI.newBox("box3", 50, 50, 0.2, 0.8, 0.2, 0.9, mainMenuPage)
    UI.addChild(testBox, box3, 25, 25)
    
    -- Create text element
    local label = UI.newText("label", "Hello UI!", uiFont, 1.0, 1.0, 1.0, 1.0, mainMenuPage)
    UI.addToPage(mainMenuPage, label, 120, 130)
    
    UI.showPage(mainMenuPage)
    engine.logInfo("Page shown with text")
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
