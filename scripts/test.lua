-- Coordinate system test v4 - using resize callback
local test = {}

local page = nil
local boxTexSet = nil
local testFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

function test.init(scriptId)
    engine.logInfo("Test script initializing, waiting for framebuffer resize callback...")
    
    -- Load font
    testFont = engine.loadFont("assets/fonts/arcade.ttf", 24)
    
    -- Load box textures for UI
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
end

function test.createUI()
    if uiCreated then
        -- Clean up old UI first
        if page then
            UI.deletePage(page)
        end
    end
    
    engine.logInfo("Creating UI with framebuffer size: " .. fbW .. " x " .. fbH)
    
    page = UI.newPage("coord_test", "menu")
    
    -- RED at origin (top-left)
    local originBox = UI.newBox("origin_box", 100, 100, boxTexSet, 32, 1.0, 0.0, 0.0, 1.0, page)
    UI.addToPage(page, originBox, 0, 0)
    
    -- GREEN at center
    local centerX = (fbW - 100) / 2
    local centerY = (fbH - 100) / 2
    local centerBox = UI.newBox("center_box", 100, 100, boxTexSet, 32, 0.0, 1.0, 0.0, 1.0, page)
    UI.addToPage(page, centerBox, centerX, centerY)
    
    -- BLUE at bottom-right
    local brBox = UI.newBox("br_box", 100, 100, boxTexSet, 32, 0.0, 0.0, 1.0, 1.0, page)
    UI.addToPage(page, brBox, fbW - 100, fbH - 100)
    
    -- Text labels
    local text1 = UI.newText("t1", "RED(0,0)", testFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, text1, 10, 110)
    
    local text2 = UI.newText("t2", "GREEN(center)", testFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, text2, centerX + 10, centerY + 110)
    
    local text3 = UI.newText("t3", "BLUE(bottom-right)", testFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, text3, fbW - 250, fbH - 130)
    
    engine.logInfo("=== COORDINATE TEST v4 ===")
    engine.logInfo("RED box at (0, 0)")
    engine.logInfo("GREEN box at center (" .. centerX .. ", " .. centerY .. ")")
    engine.logInfo("BLUE box at bottom-right (" .. (fbW - 100) .. ", " .. (fbH - 100) .. ")")
    engine.logInfo("==========================")
    
    UI.showPage(page)
    uiCreated = true
end

function test.onFramebufferResize(width, height)
    engine.logInfo("onFramebufferResize called: " .. width .. " x " .. height)
    fbW = width
    fbH = height
    
    -- Create or recreate UI with correct dimensions
    test.createUI()
end

function test.update(dt)
end

function test.shutdown()
    if page then
        UI.deletePage(page)
    end
end

return test
