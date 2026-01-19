local tex = nil
local spawnedSprites = {}
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
end

function update(dt)
    -- Mouse position logging (throttled)
    lastLogTime = lastLogTime + dt
    if lastLogTime >= logInterval then
        local mx, my = engine.getMousePosition()
        engine.logInfo("Mouse position: " .. math.floor(mx) .. ", " .. math.floor(my))
        lastLogTime = 0
    end
end

function onMouseDown(button, x, y)
    local worldX, worldY = engine.getWorldCoord(x, y)
    engine.spawnSprite(worldX, worldY, 0.5, 0.5, tex)
    engine.logInfo("Mouse button " .. button .. " pressed at: " .. x .. ", " .. y)
end

function onMouseUp(button, x, y)
    engine.logInfo("Mouse button " .. button .. " released at: " .. x .. ", " .. y)
end

function onKeyDown(key)
    engine.logInfo("Key " .. key .. " pressed")
end

function onKeyUp(key)
    engine.logInfo("Key " .. key .. " released")
end
