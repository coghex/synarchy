-- Dropdown UI component
local scale = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")
local dropdown = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local DROPDOWN_CALLBACK = "onDropdownClick"
local OPTION_CALLBACK = "onDropdownOptionClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local dropdowns = {}
local nextId = 1

local texArrowNormal = nil
local texArrowClicked = nil
local optionTexSet = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function dropdown.init()
    if assetsLoaded then return end
    
    texArrowNormal = engine.loadTexture("assets/textures/ui/dropdown.png")
    texArrowClicked = engine.loadTexture("assets/textures/ui/dropdownclicked.png")
    optionTexSet = boxTextures.load("assets/textures/ui/textbox", "textbox")
    
    assetsLoaded = true
    engine.logDebug("Dropdown module initialized")
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

function dropdown.measureOptions(options, font, fontSize)
    local maxWidth = 0
    for _, opt in ipairs(options) do
        local w = engine.getTextWidth(font, opt.text, fontSize)
        if w > maxWidth then
            maxWidth = w
        end
    end
    return maxWidth + fontSize * 2
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function dropdown.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or scale.get()
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local height = math.floor((params.height or 40) * uiscale)
    local arrowSize = height
    local tileSize = math.floor((params.tileSize or 16) * uiscale)
    local optionHeight = math.floor((params.optionHeight or 36) * uiscale)
    local textPadding = math.floor(8 * uiscale)
    
    local textColor = params.textColor or {0.0, 0.0, 0.0, 1.0}
    local highlightColor = params.highlightColor or {0.3, 0.5, 0.8, 1.0}
    
    local options = params.options or {}
    
    local measuredWidth = dropdown.measureOptions(options, params.font, fontSize)
    local displayWidth = math.max(measuredWidth, math.floor((params.minWidth or 100) * uiscale))
    local totalWidth = displayWidth + arrowSize
    
    local dd = {
        id = id,
        name = params.name or ("dropdown_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = totalWidth,
        height = height,
        arrowSize = arrowSize,
        displayWidth = displayWidth,
        tileSize = tileSize,
        fontSize = fontSize,
        optionHeight = optionHeight,
        textPadding = textPadding,
        page = params.page,
        parent = params.parent,
        font = params.font,
        options = options,
        selectedIndex = nil,
        open = false,
        onChange = params.onChange,
        textColor = textColor,
        highlightColor = highlightColor,
        uiscale = uiscale,
        displayBoxId = nil,
        displayTextId = nil,
        arrowSpriteId = nil,
        listBoxId = nil,
        optionElements = {},
        hoveredOptionIndex = nil,
    }
    
    if params.default then
        for i, opt in ipairs(dd.options) do
            if opt.value == params.default then
                dd.selectedIndex = i
                break
            end
        end
    end
    if not dd.selectedIndex and #dd.options > 0 then
        dd.selectedIndex = 1
    end
    
    -- Create display box
    dd.displayBoxId = UI.newBox(
        dd.name .. "_display",
        dd.displayWidth,
        dd.height,
        optionTexSet,
        dd.tileSize,
        1.0, 1.0, 1.0, 1.0,
        dd.page
    )
    
    local displayText = ""
    if dd.selectedIndex and dd.options[dd.selectedIndex] then
        displayText = dd.options[dd.selectedIndex].text
    end
    
    local textY = (dd.height / 2) + (dd.fontSize / 3)
    dd.displayTextId = UI.newText(
        dd.name .. "_display_text",
        displayText,
        dd.font,
        dd.fontSize,
        textColor[1], textColor[2], textColor[3], textColor[4],
        dd.page
    )
    UI.addChild(dd.displayBoxId, dd.displayTextId, dd.textPadding, textY)
    UI.setZIndex(dd.displayTextId, 1)
    
    dd.arrowSpriteId = UI.newSprite(
        dd.name .. "_arrow",
        dd.arrowSize,
        dd.arrowSize,
        texArrowNormal,
        1.0, 1.0, 1.0, 1.0,
        dd.page
    )
    UI.setClickable(dd.arrowSpriteId, true)
    UI.setOnClick(dd.arrowSpriteId, DROPDOWN_CALLBACK)
    
    UI.setClickable(dd.displayBoxId, true)
    UI.setOnClick(dd.displayBoxId, DROPDOWN_CALLBACK)
    
    -- ALWAYS add to page directly, not as panel child.
    -- The caller is responsible for positioning via setPosition.
    -- This avoids additive z-index layer conflicts with sibling elements.
    UI.addToPage(dd.page, dd.displayBoxId, dd.x, dd.y)
    UI.addToPage(dd.page, dd.arrowSpriteId, dd.x + dd.displayWidth, dd.y)
    
    if params.zIndex then
        UI.setZIndex(dd.displayBoxId, params.zIndex)
        UI.setZIndex(dd.arrowSpriteId, params.zIndex)
    end
    
    dropdowns[id] = dd
    
    engine.logDebug("Dropdown created: " .. dd.name
        .. " (" .. dd.displayWidth .. "+" .. dd.arrowSize .. ")"
        .. " with " .. #dd.options .. " options")
    
    return id
end

function dropdown.destroy(id)
    local dd = dropdowns[id]
    if not dd then return end
    
    dropdown.closeList(id)
    dropdowns[id] = nil
    engine.logDebug("Dropdown destroyed: " .. dd.name)
end

function dropdown.destroyAll()
    for id, dd in pairs(dropdowns) do
        if dd.open then
            dropdown.closeList(id)
        end
    end
    dropdowns = {}
    nextId = 1
end

-----------------------------------------------------------
-- Option List Management
-----------------------------------------------------------

function dropdown.openList(id)
    local dd = dropdowns[id]
    if not dd then return end
    if dd.open then return end
    
    for otherId, otherDd in pairs(dropdowns) do
        if otherId ~= id and otherDd.open then
            dropdown.closeList(otherId)
        end
    end
    
    dd.open = true
    UI.setSpriteTexture(dd.arrowSpriteId, texArrowClicked)
    
    local listHeight = #dd.options * dd.optionHeight
    local listX = dd.x
    local listY = dd.y + dd.height
    
    dd.listBoxId = UI.newBox(
        dd.name .. "_list",
        dd.displayWidth,
        listHeight,
        optionTexSet,
        dd.tileSize,
        1.0, 1.0, 1.0, 1.0,
        dd.page
    )
    
    -- Add list to page directly, high z-index
    UI.addToPage(dd.page, dd.listBoxId, listX, listY)
    UI.setZIndex(dd.listBoxId, 500)
    
    dd.optionElements = {}
    for i, opt in ipairs(dd.options) do
        local optY = (i - 1) * dd.optionHeight
        local textY = optY + (dd.optionHeight / 2) + (dd.fontSize / 3)
        
        local optTextId = UI.newText(
            dd.name .. "_opt_" .. i,
            opt.text,
            dd.font,
            dd.fontSize,
            dd.textColor[1], dd.textColor[2], dd.textColor[3], dd.textColor[4],
            dd.page
        )
        UI.addChild(dd.listBoxId, optTextId, dd.textPadding, textY)
        UI.setZIndex(optTextId, 1)
        
        local optBoxId = UI.newSprite(
            dd.name .. "_opt_hit_" .. i,
            dd.displayWidth,
            dd.optionHeight,
            texArrowNormal,
            0.0, 0.0, 0.0, 0.0,
            dd.page
        )
        UI.addChild(dd.listBoxId, optBoxId, 0, optY)
        UI.setClickable(optBoxId, true)
        UI.setOnClick(optBoxId, OPTION_CALLBACK)
        UI.setZIndex(optBoxId, 2)
        
        table.insert(dd.optionElements, {
            boxId = optBoxId,
            textId = optTextId,
            index = i,
        })
    end
    
    engine.logDebug("Dropdown list opened: " .. dd.name)
end

function dropdown.closeList(id)
    local dd = dropdowns[id]
    if not dd then return end
    if not dd.open then return end
    
    dd.open = false
    dd.hoveredOptionIndex = nil
    UI.setSpriteTexture(dd.arrowSpriteId, texArrowNormal)
    
    if dd.listBoxId then
        UI.deleteElement(dd.listBoxId)
        dd.listBoxId = nil
    end
    dd.optionElements = {}
    
    engine.logDebug("Dropdown list closed: " .. dd.name)
end

function dropdown.toggleList(id)
    local dd = dropdowns[id]
    if not dd then return end
    
    if dd.open then
        dropdown.closeList(id)
    else
        dropdown.openList(id)
    end
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

function dropdown.selectOption(id, optionIndex)
    local dd = dropdowns[id]
    if not dd then return end
    if optionIndex < 1 or optionIndex > #dd.options then return end
    
    dd.selectedIndex = optionIndex
    local opt = dd.options[optionIndex]
    
    if dd.displayTextId then
        UI.setText(dd.displayTextId, opt.text)
    end
    
    dropdown.closeList(id)
    
    if dd.onChange then
        dd.onChange(opt.value, opt.text, id, dd.name)
    end
    
    engine.logDebug("Dropdown selected: " .. dd.name .. " = " .. opt.text)
end

function dropdown.setOptions(id, options, defaultValue)
    local dd = dropdowns[id]
    if not dd then return end
    
    dropdown.closeList(id)
    
    dd.options = options
    dd.selectedIndex = nil
    
    if defaultValue then
        for i, opt in ipairs(dd.options) do
            if opt.value == defaultValue then
                dd.selectedIndex = i
                break
            end
        end
    end
    if not dd.selectedIndex and #dd.options > 0 then
        dd.selectedIndex = 1
    end
    
    local displayText = ""
    if dd.selectedIndex and dd.options[dd.selectedIndex] then
        displayText = dd.options[dd.selectedIndex].text
    end
    if dd.displayTextId then
        UI.setText(dd.displayTextId, displayText)
    end
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function dropdown.findByElementHandle(elemHandle)
    for id, dd in pairs(dropdowns) do
        if dd.arrowSpriteId == elemHandle or dd.displayBoxId == elemHandle then
            return id, "toggle"
        end
        for _, opt in ipairs(dd.optionElements) do
            if opt.boxId == elemHandle then
                return id, "option", opt.index
            end
        end
    end
    return nil
end

function dropdown.handleCallback(callbackName, elemHandle)
    if callbackName == DROPDOWN_CALLBACK then
        local id, action = dropdown.findByElementHandle(elemHandle)
        if id then
            dropdown.toggleList(id)
            return true
        end
    elseif callbackName == OPTION_CALLBACK then
        local id, action, optIndex = dropdown.findByElementHandle(elemHandle)
        if id and optIndex then
            dropdown.selectOption(id, optIndex)
            return true
        end
    end
    return false
end

function dropdown.onClickOutside(mouseX, mouseY)
    for id, dd in pairs(dropdowns) do
        if dd.open then
            local totalHeight = dd.height + (#dd.options * dd.optionHeight)
            if mouseX < dd.x or mouseX > dd.x + dd.width
                or mouseY < dd.y or mouseY > dd.y + totalHeight then
                dropdown.closeList(id)
            end
        end
    end
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function dropdown.update(dt)
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function dropdown.getValue(id)
    local dd = dropdowns[id]
    if not dd then return nil end
    if not dd.selectedIndex then return nil end
    return dd.options[dd.selectedIndex].value
end

function dropdown.getText(id)
    local dd = dropdowns[id]
    if not dd then return "" end
    if not dd.selectedIndex then return "" end
    return dd.options[dd.selectedIndex].text
end

function dropdown.getSelectedIndex(id)
    local dd = dropdowns[id]
    if not dd then return nil end
    return dd.selectedIndex
end

function dropdown.isOpen(id)
    local dd = dropdowns[id]
    if not dd then return false end
    return dd.open
end

function dropdown.getSize(id)
    local dd = dropdowns[id]
    if not dd then return 0, 0 end
    return dd.width, dd.height
end

function dropdown.setPosition(id, x, y)
    local dd = dropdowns[id]
    if not dd then return end
    
    dd.x = x
    dd.y = y
    
    UI.setPosition(dd.displayBoxId, x, y)
    UI.setPosition(dd.arrowSpriteId, x + dd.displayWidth, y)
end

function dropdown.getElementHandle(id)
    local dd = dropdowns[id]
    if not dd then return nil end
    return dd.displayBoxId
end

function dropdown.isDropdownCallback(callbackName)
    return callbackName == DROPDOWN_CALLBACK or callbackName == OPTION_CALLBACK
end

return dropdown
