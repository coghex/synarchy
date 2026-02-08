-- Sprite UI Widget
local sprite = {}

local nextId = 1
local sprites = {}

function sprite.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local s = {
        id      = id,
        name    = params.name or ("sprite_" .. id),
        page    = params.page,
        x       = params.x or 0,
        y       = params.y or 0,
        width   = params.width or 100,
        height  = params.height or 100,
        texture = params.texture or 0,
        color   = params.color or {1, 1, 1, 1},
        zIndex  = params.zIndex or 0,
        uiscale = params.uiscale or 1.0,
    }
    
    -- Create sprite element using UI.newSprite
    s.elementHandle = UI.newSprite(
        s.name,
        s.width,
        s.height,
        s.texture,
        s.color[1], s.color[2], s.color[3], s.color[4],
        s.page
    )
    
    if s.elementHandle then
        -- Add to page and set properties
        UI.addToPage(s.page, s.elementHandle, s.x, s.y)
        UI.setZIndex(s.elementHandle, s.zIndex)
    end
    
    sprites[id] = s
    return id
end

function sprite.destroy(id)
    local s = sprites[id]
    if not s then return end
    
    if s.elementHandle then
        UI.deleteElement(s.elementHandle)
    end
    
    sprites[id] = nil
end

function sprite.setPosition(id, x, y)
    local s = sprites[id]
    if not s then return end
    
    s.x = x
    s.y = y
    
    if s.elementHandle then
        UI.setPosition(s.elementHandle, x, y)
    end
end

function sprite.setVisible(id, visible)
    local s = sprites[id]
    if not s then return end
    
    if s.elementHandle then
        UI.setVisible(s.elementHandle, visible)
    end
end

function sprite.getElementHandle(id)
    local s = sprites[id]
    if s then
        return s.elementHandle
    end
    return nil
end

function sprite.get(id)
    return sprites[id]
end

return sprite
