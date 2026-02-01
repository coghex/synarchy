local game = {}
local tex1 = nil
local tex2 = nil
local tex3 = nil
local tex4 = nil
local tex5 = nil
local tex6 = nil
local tex7 = nil
local tex8 = nil
local tex9 = nil
local tex10 = nil
local font = nil
local spawnedSprites = {}
local shellScriptId = nil
local uiScriptId = nil
local spriteCount = 0
local lastLogTime = 0
local logInterval = 1.0  -- Log mouse position every 1 second
local w, h = 800,600
local testText = nil

function game.init(scriptId)
    engine.logInfo("=== Input System Test ===")
    engine.logInfo("Controls:")
    engine.logInfo("  WASD - Move camera")
    engine.logInfo("  Left Click - Spawn sprite at mouse")
    engine.logInfo("  Grave (`) - Toggle shell")
    engine.logInfo("  Escape - Quit")
    
    -- quick window size test
    w,h = engine.getWindowSize()
    engine.logInfo("Window size: " .. w .. "x" .. h)
    -- Load texture for spawning
    tex1 = engine.loadTexture("assets/textures/tile01.png")
    tex2 = engine.loadTexture("assets/textures/tile02.png")
    tex3 = engine.loadTexture("assets/textures/tile03.png")
    tex4 = engine.loadTexture("assets/textures/tile04.png")
    tex5 = engine.loadTexture("assets/textures/tile05.png")
    tex6 = engine.loadTexture("assets/textures/tile06.png")
    tex7 = engine.loadTexture("assets/textures/tile07.png")
    tex8 = engine.loadTexture("assets/textures/tile08.png")
    tex9 = engine.loadTexture("assets/textures/tile09.png")
    tex10 = engine.loadTexture("assets/textures/tile10.png")
    engine.logInfo("Texture loaded, ready for input")
    -- load font
    font = engine.loadFont("assets/fonts/ChrustyRock-ORLA.ttf", 32)
    if font then
        engine.logInfo("Font loaded successfully")
        testText = engine.spawnText(100,100,font,"hello, scene graph", "#99FF00", 1)
    else
        engine.logError("Failed to load font")
    end
    
    -- Initialize shell
    shellScriptId = engine.loadScript("scripts/shell.lua",0.5)
    -- initialize ui
    uiScriptId = engine.loadScript("scripts/ui.lua",1.0)
end

function game.update(dt)
    -- Mouse position logging (throttled)
    lastLogTime = lastLogTime + dt
    if lastLogTime >= logInterval then
        local mx, my = engine.getMousePosition()
        lastLogTime = 0
    end
end

function game.onMouseDown(button, x, y)
    spriteCount = spriteCount + 1
    -- cycle through layers 0,1,2
    local layer = spriteCount % 3
    -- cycle through textures
    local texI = spriteCount % 11
    if texI == 1 then
        tex = tex1
    elseif texI == 2 then
        tex = tex3
    elseif texI == 3 then
        tex = tex2
    elseif texI == 4 then
        tex = tex4
    elseif texI == 5 then
        tex = tex5
    elseif texI == 6 then
        tex = tex6
    elseif texI == 7 then
        tex = tex7
    elseif texI == 8 then
        tex = tex8
    elseif texI == 9 then
        tex = tex9
    elseif texI == 10 then
        tex = tex10
    else 
        tex = 0
    end
    local w, h = engine.getWindowSize()
    local worldX, worldY = engine.getWorldCoord(x, y)
    local objId = engine.spawnSprite(worldX, worldY, 0.1, 0.1, tex, layer)
    engine.setColor(testText, "#FF00FF")
end

function game.onMouseUp(button, x, y)
end

function game.onKeyDown(key)
end

function game.onKeyUp(key)
end

function game.shutdown()
    if shellScriptId then
        engine.killScript(shellScriptId)
    end
end

return game
