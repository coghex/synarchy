-- Box texture loading utility for 9-tile UI elements
local boxTextures = {}

-- Cache for loaded texture sets (keyed by base path)
local textureCache = {}

-- Load a 9-tile box texture set from a directory
-- Expects files: box.png, boxn.png, boxs.png, boxe.png, boxw.png, boxne.png, boxnw.png, boxse.png, boxsw.png
-- Or with a prefix: {prefix}.png, {prefix}n.png, etc.
function boxTextures.load(basePath, prefix)
    prefix = prefix or "box"
    local cacheKey = basePath .. "/" .. prefix
    
    -- Return cached if available
    if textureCache[cacheKey] then
        return textureCache[cacheKey]
    end
    
    -- Load all 9 textures
    local texCenter = engine.loadTexture(basePath .. "/" .. prefix .. ".png")
    local texN = engine.loadTexture(basePath .. "/" .. prefix .. "n.png")
    local texS = engine.loadTexture(basePath .. "/" .. prefix .. "s.png")
    local texE = engine.loadTexture(basePath .. "/" .. prefix .. "e.png")
    local texW = engine.loadTexture(basePath .. "/" .. prefix .. "w.png")
    local texNE = engine.loadTexture(basePath .. "/" .. prefix .. "ne.png")
    local texNW = engine.loadTexture(basePath .. "/" .. prefix .. "nw.png")
    local texSE = engine.loadTexture(basePath .. "/" .. prefix .. "se.png")
    local texSW = engine.loadTexture(basePath .. "/" .. prefix .. "sw.png")
    
    -- Register with UI system
    local texSet = UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW)
    
    -- Cache it
    textureCache[cacheKey] = texSet
    
    engine.logDebug("Box textures loaded: " .. cacheKey)
    
    return texSet
end

-- Clear the cache (useful on shutdown or asset reload)
function boxTextures.clearCache()
    textureCache = {}
end

-- Check if a texture set is cached
function boxTextures.isCached(basePath, prefix)
    prefix = prefix or "box"
    local cacheKey = basePath .. "/" .. prefix
    return textureCache[cacheKey] ~= nil
end

return boxTextures
