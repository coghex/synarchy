-- Panel UI component with flexible positioning
local scale = require("scripts.ui.scale")
local panel = {}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local panels = {}
local nextId = 1

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

local ORIGIN_MAP = {
    ["top-left"]      = { x = 0,   y = 0 },
    ["top-center"]    = { x = 0.5, y = 0 },
    ["top-right"]     = { x = 1,   y = 0 },
    ["center-left"]   = { x = 0,   y = 0.5 },
    ["center"]        = { x = 0.5, y = 0.5 },
    ["center-right"]  = { x = 1,   y = 0.5 },
    ["bottom-left"]   = { x = 0,   y = 1 },
    ["bottom-center"] = { x = 0.5, y = 1 },
    ["bottom-right"]  = { x = 1,   y = 1 },
}

-----------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------

function panel.parsePosition(value)
    if type(value) == "string" then
        if value:match("%%$") then
            local num = tonumber(value:match("^([%d%.%-]+)%%$"))
            return { value = (num or 0) / 100, isPercent = true }
        elseif value:match("px$") then
            local num = tonumber(value:match("^([%d%.%-]+)px$"))
            return { value = num or 0, isPercent = false }
        else
            local num = tonumber(value)
            return { value = num or 0, isPercent = false }
        end
    elseif type(value) == "number" then
        return { value = value, isPercent = false }
    end
    return { value = 0, isPercent = false }
end

function panel.calcPosition(parsed, containerSize)
    if parsed.isPercent then
        return parsed.value * containerSize
    else
        return parsed.value
    end
end

function panel.parseOrigin(origin)
    origin = origin or "top-left"
    return ORIGIN_MAP[origin] or ORIGIN_MAP["top-left"]
end

function panel.parsePadding(padding)
    if type(padding) == "number" then
        return { top = padding, bottom = padding, left = padding, right = padding }
    elseif type(padding) == "table" then
        return {
            top = padding.top or padding[1] or 0,
            bottom = padding.bottom or padding[2] or padding[1] or 0,
            left = padding.left or padding[3] or padding[1] or 0,
            right = padding.right or padding[4] or padding[3] or padding[1] or 0,
        }
    end
    return { top = 0, bottom = 0, left = 0, right = 0 }
end

function panel.getElementSize(options)
    if options and options.width and options.height then
        return options.width, options.height
    end
    return 0, 0
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function panel.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or scale.get()
    local padding = panel.parsePadding(params.padding)
    
    padding.top = math.floor(padding.top * uiscale)
    padding.bottom = math.floor(padding.bottom * uiscale)
    padding.left = math.floor(padding.left * uiscale)
    padding.right = math.floor(padding.right * uiscale)
    
    local p = {
        id = id,
        name = params.name or ("panel_" .. id),
        page = params.page,
        parentPanel = params.parent,
        x = params.x or 0,
        y = params.y or 0,
        width = params.width or 400,
        height = params.height or 300,
        padding = padding,
        zIndex = params.zIndex or 10,
        boxId = nil,
        elements = {},
        uiscale = uiscale,
        nextChildZ = 1,  -- auto-incrementing z-index for placed children
    }
    
    p.contentX = padding.left
    p.contentY = padding.top
    p.contentWidth = p.width - padding.left - padding.right
    p.contentHeight = p.height - padding.top - padding.bottom
    
    local color = params.color or {1.0, 1.0, 1.0, 1.0}
    local tileSize = params.tileSize or 64
    
    p.boxId = UI.newBox(
        p.name .. "_box",
        p.width,
        p.height,
        params.textureSet,
        tileSize,
        color[1], color[2], color[3], color[4],
        params.overflow or 0,
        p.page
    )
    
    if p.parentPanel then
        local parentPanelData = panels[p.parentPanel]
        if parentPanelData then
            UI.addChild(parentPanelData.boxId, p.boxId, p.x, p.y)
        else
            engine.logWarn("Panel parent not found: " .. tostring(p.parentPanel))
            UI.addToPage(p.page, p.boxId, p.x, p.y)
        end
    else
        UI.addToPage(p.page, p.boxId, p.x, p.y)
    end
    
    UI.setZIndex(p.boxId, p.zIndex)
    
    panels[id] = p
    
    engine.logDebug("Panel created: " .. p.name .. " (" .. p.width .. "x" .. p.height .. ")")
    
    return id
end

function panel.destroy(id)
    local p = panels[id]
    if not p then return end
    
    panels[id] = nil
    engine.logDebug("Panel destroyed: " .. p.name)
end

function panel.destroyAll()
    panels = {}
    nextId = 1
    engine.logDebug("All panels destroyed")
end

-----------------------------------------------------------
-- Element Placement
-----------------------------------------------------------

function panel.place(id, elemHandle, options)
    local p = panels[id]
    if not p then
        engine.logError("Panel not found: " .. tostring(id))
        return
    end
    
    options = options or {}
    
    local xParsed = panel.parsePosition(options.x or 0)
    local yParsed = panel.parsePosition(options.y or 0)
    
    local posX = panel.calcPosition(xParsed, p.contentWidth)
    local posY = panel.calcPosition(yParsed, p.contentHeight)
    
    local elemWidth, elemHeight = panel.getElementSize(options)
    
    local origin = panel.parseOrigin(options.origin)
    local originOffsetX = origin.x * elemWidth
    local originOffsetY = origin.y * elemHeight
    
    local offsetX = options.offsetX or 0
    local offsetY = options.offsetY or 0
    
    local finalX = p.contentX + posX - originOffsetX + offsetX
    local finalY = p.contentY + posY - originOffsetY + offsetY
    
    UI.addChild(p.boxId, elemHandle, finalX, finalY)
    
    -- Auto-assign a unique z-index unless caller explicitly set one via options
    local z = options.zIndex or p.nextChildZ
    UI.setZIndex(elemHandle, z)
    p.nextChildZ = p.nextChildZ + 1
    
    table.insert(p.elements, elemHandle)
    
    return elemHandle
end

function panel.placeRow(id, elements, sizes, options)
    local p = panels[id]
    if not p then
        engine.logError("Panel not found: " .. tostring(id))
        return
    end
    
    options = options or {}
    local spacing = options.spacing or 0
    
    local totalWidth = 0
    local maxHeight = 0
    
    for i, elem in ipairs(elements) do
        local size = sizes[i] or { width = 0, height = 0 }
        totalWidth = totalWidth + size.width
        if i < #elements then
            totalWidth = totalWidth + spacing
        end
        if size.height > maxHeight then
            maxHeight = size.height
        end
    end
    
    local xParsed = panel.parsePosition(options.x or "50%")
    local yParsed = panel.parsePosition(options.y or "50%")
    
    local rowX = panel.calcPosition(xParsed, p.contentWidth)
    local rowY = panel.calcPosition(yParsed, p.contentHeight)
    
    local origin = panel.parseOrigin(options.origin or "center")
    local rowOriginOffsetX = origin.x * totalWidth
    local rowOriginOffsetY = origin.y * maxHeight
    
    local offsetX = options.offsetX or 0
    local offsetY = options.offsetY or 0
    
    local currentX = p.contentX + rowX - rowOriginOffsetX + offsetX
    local baseY = p.contentY + rowY - rowOriginOffsetY + offsetY
    
    for i, elem in ipairs(elements) do
        local size = sizes[i] or { width = 0, height = 0 }
        
        local elemY = baseY + (maxHeight - size.height) / 2
        
        UI.addChild(p.boxId, elem, currentX, elemY)
        
        -- Auto-assign unique z-index
        UI.setZIndex(elem, p.nextChildZ)
        p.nextChildZ = p.nextChildZ + 1
        
        table.insert(p.elements, elem)
        
        currentX = currentX + size.width + spacing
    end
end

function panel.placeColumn(id, elements, sizes, options)
    local p = panels[id]
    if not p then
        engine.logError("Panel not found: " .. tostring(id))
        return
    end
    
    options = options or {}
    local spacing = options.spacing or 0
    
    local totalHeight = 0
    local maxWidth = 0
    
    for i, elem in ipairs(elements) do
        local size = sizes[i] or { width = 0, height = 0 }
        totalHeight = totalHeight + size.height
        if i < #elements then
            totalHeight = totalHeight + spacing
        end
        if size.width > maxWidth then
            maxWidth = size.width
        end
    end
    
    local xParsed = panel.parsePosition(options.x or "50%")
    local yParsed = panel.parsePosition(options.y or "50%")
    
    local colX = panel.calcPosition(xParsed, p.contentWidth)
    local colY = panel.calcPosition(yParsed, p.contentHeight)
    
    local origin = panel.parseOrigin(options.origin or "center")
    local colOriginOffsetX = origin.x * maxWidth
    local colOriginOffsetY = origin.y * totalHeight
    
    local offsetX = options.offsetX or 0
    local offsetY = options.offsetY or 0
    
    local baseX = p.contentX + colX - colOriginOffsetX + offsetX
    local currentY = p.contentY + colY - colOriginOffsetY + offsetY
    
    for i, elem in ipairs(elements) do
        local size = sizes[i] or { width = 0, height = 0 }
        
        local elemX = baseX + (maxWidth - size.width) / 2
        
        UI.addChild(p.boxId, elem, elemX, currentY)
        
        -- Auto-assign unique z-index
        UI.setZIndex(elem, p.nextChildZ)
        p.nextChildZ = p.nextChildZ + 1
        
        table.insert(p.elements, elem)
        
        currentY = currentY + size.height + spacing
    end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function panel.getContentBounds(id)
    local p = panels[id]
    if not p then return nil end
    
    return {
        x = p.contentX,
        y = p.contentY,
        width = p.contentWidth,
        height = p.contentHeight,
    }
end

function panel.getBoxHandle(id)
    local p = panels[id]
    if not p then return nil end
    return p.boxId
end

function panel.getZIndex(id)
    local p = panels[id]
    if not p then return 0 end
    return p.zIndex
end

function panel.getSize(id)
    local p = panels[id]
    if not p then return 0, 0 end
    return p.width, p.height
end

function panel.getPosition(id)
    local p = panels[id]
    if not p then return 0, 0 end
    return p.x, p.y
end

function panel.getElements(id)
    local p = panels[id]
    if not p then return {} end
    return p.elements
end

function panel.getPage(id)
    local p = panels[id]
    if not p then return nil end
    return p.page
end

function panel.getUIScale(id)
    local p = panels[id]
    if not p then return 1.0 end
    return p.uiscale
end

return panel
