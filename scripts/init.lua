local tickCount = 0
local testx, testy = 0, 0
local testspeed = 0.1
local colorR = 1.0

function init()
    engine.logInfo("=== Sprite Manipulation Test ===")
    tex = engine.loadTexture("assets/textures/tile01.png")
    
    sprite1 = engine.spawnSprite(testx, testy, 1, 1, tex)
    sprite2 = engine.spawnSprite(1, 0, 1, 1, tex)
    
    -- Make sprite2 red
    engine.setSpriteColor(sprite2, 1, 0, 0, 1)
    
    engine.logInfo("Spawned sprites: " .. sprite1 .. ", " .. sprite2)
end

function update(dt)
    tickCount = tickCount + 1
    
    -- Move sprite1
    testx = testx + testspeed * dt
    engine.moveSprite(sprite1, testx, testy)
    
    -- Pulse color on sprite1
    colorR = 0.5 + 0.5 * math.sin(tickCount * 0.1)
    engine.setSpriteColor(sprite1, colorR, 1, 1, 1)
    
    -- Toggle visibility
    if tickCount % 2 == 0 then
        engine.setSpriteVisible(sprite2, false)
    elseif tickCount % 2 == 1 then
        engine.setSpriteVisible(sprite2, true)
    end
    
    -- Destroy sprite1 after 10 ticks
    if tickCount == 10 then
        engine.destroySprite(sprite1)
        engine.logInfo("Sprite1 destroyed!")
    end
end
