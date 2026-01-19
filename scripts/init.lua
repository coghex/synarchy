local tickCount = 0

function init()
    engine.logInfo("Lua init script started.")
    engine.logInfo("loading resources from lua...")
    local handle = engine.loadTexture("assets/textures/tile01.png")
    engine.logInfo("Texture loaded with handle: " .. tostring(handle))
    playerTexture = handle
end

function update(dt)
    -- empty for now
end
