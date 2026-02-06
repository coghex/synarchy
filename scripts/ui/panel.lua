-- Panel UI component with flexible positioning
local scale = require("scripts.ui.scale")
local panel = {}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

panel.panels = {}
panel.nextId = 1

-----------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------

-- Parse position value: "50%" -> {value=0.5, isPercent=true}, "100px" or 100 -> {value=100, isPercent=false}
function panel.parsePosition(value)
    if type(value) == "string" then
        if value:match("%%$") then
            local num = tonumber(value:match("^([%d%.%-]+)%%$"))
            return { value = (num or 0) / 100, isPercent = true }
        elseif value:match("px$") then
            local num = tonumber(value:match("^([%d%.%-]+)px$"))
            return { value = num or 0, isPercent = false }
        else
            -- Try to parse as number
            local num = tonumber(value)
            return { value = num or 0, isPercent = false }
        end
    elseif type(value) == "number" then
        return { value = value, isPercent = false }
    end
    return { value = 0, isPercent = false }
end

-- Calculate actual pixel position from parsed position and container size
function panel.calcPosition(parsed, containerSize)
    if parsed.isPercent then
        return parsed.value * containerSize
    else
        return parsed.value
    end
end

-- Parse origin string into x and y multipliers (0, 0.5, or 1)
function panel.parseOrigin(origin)
    origin = origin or "top-left"
    
    panel.originMap = {
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
    
    return panel.originMap[origin] or panel.originMap["top-left"]
end

-- Parse padding into {top, bottom, left, right}
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

-- Get element size from options or return 0,0
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
    local id = panel.nextId
    panel.nextId = panel.nextId + 1
    
    local uiscale = params.uiscale or scale.get()
    local padding = panel.parsePadding(params.padding)
    
    -- Scale padding
    padding.top = math.floor(padding.top * uiscale)
    padding.bottom = math.floor(padding.bottom * uiscale)
    padding.left = math.floor(padding.left * uiscale)
    padding.right = math.floor(padding.right * uiscale)
    
    local p = {
        id = id,
        name = params.name or ("panel_" .. id),
        page = params.page,
        parentPanel = params.parent,    -- Parent panel id (for nesting)
        x = params.x or 0,
        y = params.y or 0,
        width = params.width or 400,
        height = params.height or 300,
        padding = padding,
        zIndex = params.zIndex or 10,
        boxId = nil,
        elements = {},                  -- Track placed elements
        uiscale = uiscale,
    }
    
    -- Calculate content area
    p.contentX = padding.left
    p.contentY = padding.top
    p.contentWidth = p.width - padding.left - padding.right
    p.contentHeight = p.height - padding.top - padding.bottom
    
    -- Create the panel box
    local color = params.color or {1.0, 1.0, 1.0, 1.0}
    local tileSize = params.tileSize or 64
    
    p.boxId = UI.newBox(
        p.name .. "_box",
        p.width,
        p.height,
        params.textureSet,
        tileSize,
        color[1], color[2], color[3], color[4],
        p.page
    )
    
    -- Position the panel
    if p.parentPanel then
        -- Nested panel: add as child to parent panel's box
        local parentPanelData = panel.panels[p.parentPanel]
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
    
    panel.panels[id] = p
    
    engine.logDebug("Panel created: " .. p.name .. " (" .. p.width .. "x" .. p.height .. ")")
    
    return id
end

function panel.destroy(id)
    local p = panel.panels[id]
    if not p then return end
    
    panel.panels[id] = nil
    engine.logDebug("Panel destroyed: " .. p.name)
end

function panel.destroyAll()
    panel.panels = {}
    panel.nextId = 1
    engine.logDebug("All panels destroyed")
end

-----------------------------------------------------------
-- Element Placement
-----------------------------------------------------------

function panel.place(id, elemHandle, options)
    local p = panel.panels[id]
    if not p then
        engine.logError("Panel not found: " .. tostring(id))
        return
    end
    
    options = options or {}
    
    -- Parse position
    local xParsed = panel.parsePosition(options.x or 0)
    local yParsed = panel.parsePosition(options.y or 0)
    
    -- Calculate position within content area
    local posX = panel.calcPosition(xParsed, p.contentWidth)
    local posY = panel.calcPosition(yParsed, p.contentHeight)
    
    -- Get element size for origin offset
    local elemWidth, elemHeight = panel.getElementSize(options)
    
    -- Parse origin and calculate offset
    local origin = panel.parseOrigin(options.origin)
    local originOffsetX = origin.x * elemWidth
    local originOffsetY = origin.y * elemHeight
    
    -- Apply user offset
    local offsetX = options.offsetX or 0
    local offsetY = options.offsetY or 0
    
    -- Final position relative to panel's content area
    local finalX = p.contentX + posX - originOffsetX + offsetX
    local finalY = p.contentY + posY - originOffsetY + offsetY
    
    -- Add element as child of panel box
    UI.addChild(p.boxId, elemHandle, finalX, finalY)
    
    -- Track element
    table.insert(p.elements, elemHandle)
    
    return elemHandle
end

function panel.placeRow(id, elements, sizes, options)
    local p = panel.panels[id]
    if not p then
        engine.logError("Panel not found: " .. tostring(id))
        return
    end
    
    options = options or {}
    local spacing = options.spacing or 0
    
    -- Calculate total width of all elements plus spacing
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
    
    -- Parse row position
    local xParsed = panel.parsePosition(options.x or "50%")
    local yParsed = panel.parsePosition(options.y or "50%")
    
    local rowX = panel.calcPosition(xParsed, p.contentWidth)
    local rowY = panel.calcPosition(yParsed, p.contentHeight)
    
    -- Parse origin for the row as a whole
    local origin = panel.parseOrigin(options.origin or "center")
    local rowOriginOffsetX = origin.x * totalWidth
    local rowOriginOffsetY = origin.y * maxHeight
    
    -- Apply user offset
    local offsetX = options.offsetX or 0
    local offsetY = options.offsetY or 0
    
    -- Starting position for first element
    local currentX = p.contentX + rowX - rowOriginOffsetX + offsetX
    local baseY = p.contentY + rowY - rowOriginOffsetY + offsetY
    
    -- Place each element
    for i, elem in ipairs(elements) do
        local size = sizes[i] or { width = 0, height = 0 }
        
        -- Center element vertically within row
        local elemY = baseY + (maxHeight - size.height) / 2
        
        UI.addChild(p.boxId, elem, currentX, elemY)
        table.insert(p.elements, elem)
        
        currentX = currentX + size.width + spacing
    end
end

function panel.placeColumn(id, elements, sizes, options)
    local p = panel.panels[id]
    if not p then
        engine.logError("Panel not found: " .. tostring(id))
        return
    end
    
    options = options or {}
    local spacing = options.spacing or 0
    
    -- Calculate total height of all elements plus spacing
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
    
    -- Parse column position
    local xParsed = panel.parsePosition(options.x or "50%")
    local yParsed = panel.parsePosition(options.y or "50%")
    
    local colX = panel.calcPosition(xParsed, p.contentWidth)
    local colY = panel.calcPosition(yParsed, p.contentHeight)
    
    -- Parse origin for the column as a whole
    local origin = panel.parseOrigin(options.origin or "center")
    local colOriginOffsetX = origin.x * maxWidth
    local colOriginOffsetY = origin.y * totalHeight
    
    -- Apply user offset
    local offsetX = options.offsetX or 0
    local offsetY = options.offsetY or 0
    
    -- Starting position for first element
    local baseX = p.contentX + colX - colOriginOffsetX + offsetX
    local currentY = p.contentY + colY - colOriginOffsetY + offsetY
    
    -- Place each element
    for i, elem in ipairs(elements) do
        local size = sizes[i] or { width = 0, height = 0 }
        
        -- Center element horizontally within column
        local elemX = baseX + (maxWidth - size.width) / 2
        
        UI.addChild(p.boxId, elem, elemX, currentY)
        table.insert(p.elements, elem)
        
        currentY = currentY + size.height + spacing
    end
end

-----------------------------------------------------------
-- Text Helper
-----------------------------------------------------------

function panel.placeText(id, text, font, fontSize, color, options)
    local p = panel.panels[id]
    if not p then return nil end
    
    color = color or {1.0, 1.0, 1.0, 1.0}
    options = options or {}
    
    local textElem = UI.newText(
        options.name or (p.name .. "_text_" .. #p.elements),
        text,
        font,
        fontSize,
        color[1], color[2], color[3], color[4],
        p.page
    )
    
    -- Calculate text size
    local width = engine.getTextWidth(font, text, fontSize)
    local height = fontSize
    
    options.width = width
    options.height = height
    
    panel.place(id, textElem, options)
    
    return textElem
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function panel.getContentBounds(id)
    local p = panel.panels[id]
    if not p then return nil end
    
    return {
        x = p.contentX,
        y = p.contentY,
        width = p.contentWidth,
        height = p.contentHeight,
    }
end

function panel.getBoxHandle(id)
    local p = panel.panels[id]
    if not p then return nil end
    return p.boxId
end

function panel.getZIndex(id)
    local p = panel.panels[id]
    if not p then return 0 end
    return p.zIndex
end

function panel.getSize(id)
    local p = panel.panels[id]
    if not p then return 0, 0 end
    return p.width, p.height
end

function panel.getPosition(id)
    local p = panel.panels[id]
    if not p then return 0, 0 end
    return p.x, p.y
end

function panel.getElements(id)
    local p = panel.panels[id]
    if not p then return {} end
    return p.elements
end

function panel.getPage(id)
    local p = panel.panels[id]
    if not p then return nil end
    return p.page
end

function panel.getUIScale(id)
    local p = panel.panels[id]
    if not p then return 1.0 end
    return p.uiscale
end

return panel
