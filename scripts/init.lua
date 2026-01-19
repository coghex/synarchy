local tickCount = 0
local testx, testy = 0, 0
local testspeed = 0.1

function init()
    engine.logInfo ("=== Sprite Test ===")
    tex = engine.loadTexture("assets/textures/tile01.png")
    engine.logInfo("Texture handle: " .. tostring(tex))
    sprite1 = engine.spawnSprite(testx,testy,1,1,tex)
    engine.logInfo("Sprite1 handle: " .. tostring(sprite1))
    sprite2 = engine.spawnSprite(1,0,1,1,tex)
    engine.logInfo("Sprite2 handle: " .. tostring(sprite2))
end

function update(dt)
    -- empty for now
    testx = testx + testspeed * dt
    engine.moveSprite(sprite1, testx, testy)
end
