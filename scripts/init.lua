local tickCount = 0

function init()
    engine.logInfo ("=== Sprite Test ===")
    tex = engine.loadTexture("assets/textures/tile01.png")
    engine.logInfo("Texture handle: " .. tostring(tex))
    sprite1 = engine.spawnSprite(0,0,1,1,tex)
    engine.logInfo("Sprite1 handle: " .. tostring(sprite1))
    sprite2 = engine.spawnSprite(1,0,1,1,tex)
    engine.logInfo("Sprite2 handle: " .. tostring(sprite2))
end

function update(dt)
    -- empty for now
end
