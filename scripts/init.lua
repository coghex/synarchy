local tex = nil
local font = nil
local spawnedSprites = {}
local spriteCount = 0
local lastLogTime = 0
local logInterval = 1.0  -- Log mouse position every 1 second
local w, h = 800,600

function init()
    engine.logInfo("=== Input System Test ===")
    engine.logInfo("Controls:")
    engine.logInfo("  WASD - Move camera")
    engine.logInfo("  Left Click - Spawn sprite at mouse")
    engine.logInfo("  Escape - Quit")
    
    -- quick window size test
    w,h = engine.getWindowSize()
    engine.logInfo("Window size: " .. w .. "x" .. h)
    -- Load texture for spawning
    tex = engine.loadTexture("assets/textures/tile01.png")
    engine.logInfo("Texture loaded, ready for input")
    -- load font
    font = engine.loadFont("assets/fonts/ChrustyRock-ORLA.ttf", 32)
    if font then
        engine.logInfo("Font loaded successfully")
        engine.spawnText(100,100,font,"hello, scene graph", 1)
    else
        engine.logError("Failed to load font")
    end
end

function update(dt)
    -- Mouse position logging (throttled)
    lastLogTime = lastLogTime + dt
    if lastLogTime >= logInterval then
        local mx, my = engine.getMousePosition()
        lastLogTime = 0
    end
end

function onMouseDown(button, x, y)
    spriteCount = spriteCount + 1
    -- cycle through layers 0,1,2
    local layer = spriteCount % 3
    local w, h = engine.getWindowSize()
    local worldX, worldY = engine.getWorldCoord(x, y)
    local objId = engine.spawnSprite(worldX, worldY, 0.1, 0.1, tex, layer)
end

function onMouseUp(button, x, y)
end

function onKeyDown(key)
    engine.logInfo("Key " .. key .. " pressed")
end

function onKeyUp(key)
    engine.logInfo("Key " .. key .. " released")
end
