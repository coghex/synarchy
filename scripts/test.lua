-- Coordinate system test v2
local test = {}

local page = nil
local boxTexSet = nil
local testFont = nil

function test.init(scriptId)
    local fbW, fbH = engine.getFramebufferSize()
    engine.logInfo("Framebuffer size: " .. fbW .. " x " .. fbH)
    
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
    
    page = UI.newPage("coord_test", "menu")
    
    -- Test 1: RED box at (0, 0)
    local originBox = UI.newBox("origin_box", 100, 100, boxTexSet, 32, 1.0, 0.0, 0.0, 1.0, page)
    UI.addToPage(page, originBox, 0, 0)
    
    -- Test 2: GREEN box at absolute position (400, 300)
    local absBox = UI.newBox("abs_box", 100, 100, boxTexSet, 32, 0.0, 1.0, 0.0, 1.0, page)
    UI.addToPage(page, absBox, 400, 300)
    
    -- Test 3: BLUE box at absolute position (800, 500)
    local farBox = UI.newBox("far_box", 100, 100, boxTexSet, 32, 0.0, 0.0, 1.0, 1.0, page)
    UI.addToPage(page, farBox, 800, 500)
    
    -- Test 4: YELLOW box using calculated center
    local centerX = (fbW - 100) / 2
    local centerY = (fbH - 100) / 2
    local centerBox = UI.newBox("center_box", 100, 100, boxTexSet, 32, 1.0, 1.0, 0.0, 1.0, page)
    UI.addToPage(page, centerBox, centerX, centerY)
    
    -- Test 5: MAGENTA box at bottom-right using fbW/fbH
    local brBox = UI.newBox("br_box", 100, 100, boxTexSet, 32, 1.0, 0.0, 1.0, 1.0, page)
    UI.addToPage(page, brBox, fbW - 100, fbH - 100)
    
    -- Text labels at fixed positions
    local text1 = UI.newText("t1", "RED(0,0)", testFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, text1, 10, 110)
    
    local text2 = UI.newText("t2", "GREEN(400,300)", testFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, text2, 410, 410)
    
    local text3 = UI.newText("t3", "BLUE(800,500)", testFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, text3, 810, 610)
    
    engine.logInfo("=== COORDINATE TEST v2 ===")
    engine.logInfo("RED box at (0, 0)")
    engine.logInfo("GREEN box at (400, 300)")
    engine.logInfo("BLUE box at (800, 500)")
    engine.logInfo("YELLOW box at calculated center (" .. centerX .. ", " .. centerY .. ")")
    engine.logInfo("MAGENTA box at bottom-right (" .. (fbW-100) .. ", " .. (fbH-100) .. ")")
    engine.logInfo("==========================")
    
    UI.showPage(page)
end

function test.update(dt)
end

function test.shutdown()
    if page then
        UI.deletePage(page)
    end
end

return test
