-- Dropdown UI component (combo box - editable + selectable + scrollable)
local scale = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")
local scrollbar = require("scripts.ui.scrollbar")
local dropdown = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local DROPDOWN_CALLBACK = "onDropdownClick"
local OPTION_CALLBACK = "onDropdownOptionClick"
local DISPLAY_CALLBACK = "onDropdownDisplayClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local dropdowns = {}
local nextId = 1

local texArrowNormal = nil
local texArrowClicked = nil
local optionTexSet = nil
local displayTexSetNormal = nil
local displayTexSetFocused = nil
local highlightTex = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function dropdown.init()
    if assetsLoaded then return end
    
    texArrowNormal = engine.loadTexture("assets/textures/ui/dropdown.png")
    texArrowClicked = engine.loadTexture("assets/textures/ui/dropdownclicked.png")
    optionTexSet = boxTextures.load("assets/textures/ui/textbox", "textbox")
    displayTexSetNormal = optionTexSet
    displayTexSetFocused = boxTextures.load("assets/textures/ui/textboxselected", "textbox")
    highlightTex = engine.loadTexture("assets/textures/ui/highlight.png")
    
    scrollbar.init()
    
    assetsLoaded = true
    engine.logDebug("Dropdown module initialized")
end

-----------------------------------------------------------
-- Input Validation
-----------------------------------------------------------

function dropdown.isValidChar(dd, char)
    if not dd.validateChar then
        return true
    end
    return dd.validateChar(char, dropdown.getInputText(dd))
end

function dropdown.getInputText(dd)
    if dd.displayBoxId then
        return UI.getTextInput(dd.displayBoxId) or ""
    end
    return ""
end

-- Built-in validator: resolution format (digits and x/X)
function dropdown.resolutionValidator(char, currentText)
    if char:match("^%d$") then
        return true
    elseif char == "x" or char == "X" then
        return not currentText:lower():find("x")
    end
    return false
end

-- Built-in validator: numeric only
function dropdown.numericValidator(char, currentText)
    return char:match("^%d$") ~= nil
end

-----------------------------------------------------------
-- Value Matching
-----------------------------------------------------------

function dropdown.findBestMatch(dd, inputText)
    if not inputText or inputText == "" then return nil end
    
    local lower = inputText:lower()
    for i, opt in ipairs(dd.options) do
        if opt.value:lower() == lower or opt.text:lower() == lower then
            return i
        end
    end
    
    if dd.matchFn then
        return dd.matchFn(dd.options, inputText)
    end
    
    for i, opt in ipairs(dd.options) do
        if opt.text:lower():sub(1, #lower) == lower then
            return i
        end
    end
    
    return nil
end

-- Built-in matcher: resolution (find nearest supported resolution)
function dropdown.resolutionMatcher(options, inputText)
    local w, h = inputText:lower():match("^(%d+)x(%d+)$")
    if not w or not h then return nil end
    
    w = tonumber(w)
    h = tonumber(h)
    if not w or not h or w <= 0 or h <= 0 then return nil end
    
    local candidates = {}
    for i, opt in ipairs(options) do
        if opt.width and opt.height then
            table.insert(candidates, { index = i, w = opt.width, h = opt.height })
        end
    end
    table.sort(candidates, function(a, b)
        if a.w == b.w then return a.h < b.h end
        return a.w < b.w
    end)
    
    local bestIndex = nil
    local bestDist = math.huge
    
    for _, c in ipairs(candidates) do
        if c.w <= w and c.h >= h then
            local dist = (w - c.w) + (c.h - h)
            if dist < bestDist then
                bestDist = dist
                bestIndex = c.index
            end
        end
    end
    
    if not bestIndex then
        for _, c in ipairs(candidates) do
            local dist = math.abs(c.w - w) + math.abs(c.h - h)
            if dist < bestDist then
                bestDist = dist
                bestIndex = c.index
            end
        end
    end
    
    return bestIndex
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
    local highlightColor = params.highlightColor or {0.3, 0.5, 0.8, 0.8}
    local highlightTextColor = params.highlightTextColor or {1.0, 1.0, 1.0, 1.0}
    
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
        focused = false,
        onChange = params.onChange,
        textColor = textColor,
        highlightColor = highlightColor,
        highlightTextColor = highlightTextColor,
        uiscale = uiscale,
        displayBoxId = nil,
        displayTextId = nil,
        cursorId = nil,
        arrowSpriteId = nil,
        listBoxId = nil,
        optionElements = {},
        hoveredOptionIndex = nil,
        -- Input validation and matching
        validateChar = params.validateChar or nil,
        matchFn = params.matchFn or nil,
        -- Scrollbar
        maxVisibleOptions = params.maxVisibleOptions or 8,
        scrollbarId = nil,
        scrollOffset = 0,
        needsScroll = false,
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
    
    -- Create display box (also serves as text input)
    dd.displayBoxId = UI.newBox(
        dd.name .. "_display",
        dd.displayWidth,
        dd.height,
        displayTexSetNormal,
        dd.tileSize,
        1.0, 1.0, 1.0, 1.0,
        dd.page
    )
    
    -- Enable text input on the display box
    UI.enableTextInput(dd.displayBoxId)
    
    local displayText = ""
    if dd.selectedIndex and dd.options[dd.selectedIndex] then
        displayText = dd.options[dd.selectedIndex].text
        UI.setTextInput(dd.displayBoxId, displayText)
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
    
    -- Cursor (hidden by default)
    dd.cursorId = UI.newText(
        dd.name .. "_cursor",
        "|",
        dd.font,
        dd.fontSize,
        textColor[1], textColor[2], textColor[3], textColor[4],
        dd.page
    )
    UI.addChild(dd.displayBoxId, dd.cursorId, dd.textPadding, textY)
    UI.setZIndex(dd.cursorId, 2)
    UI.setVisible(dd.cursorId, false)
    
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
    
    -- Display box click focuses for text input
    UI.setClickable(dd.displayBoxId, true)
    UI.setOnClick(dd.displayBoxId, DISPLAY_CALLBACK)
    
    UI.addToPage(dd.page, dd.displayBoxId, dd.x, dd.y)
    UI.addToPage(dd.page, dd.arrowSpriteId, dd.x + dd.displayWidth, dd.y)
    
    if params.zIndex then
        UI.setZIndex(dd.displayBoxId, params.zIndex)
        UI.setZIndex(dd.arrowSpriteId, params.zIndex)
    end
    
    dropdowns[id] = dd
    
    engine.logDebug("Dropdown created: " .. dd.name
        .. " (" .. dd.displayWidth .. "+" .. dd.arrowSize .. ")"
        .. " with " .. #dd.options .. " options"
        .. " maxVisible=" .. dd.maxVisibleOptions)
    
    return id
end

function dropdown.destroy(id)
    local dd = dropdowns[id]
    if not dd then return end
    
    dropdown.closeList(id)
    dropdown.unfocus(id)
    dropdowns[id] = nil
    engine.logDebug("Dropdown destroyed: " .. dd.name)
end

function dropdown.destroyAll()
    for id, dd in pairs(dropdowns) do
        if dd.open then
            dropdown.closeList(id)
        end
        if dd.focused then
            dropdown.unfocus(id)
        end
    end
    dropdowns = {}
    nextId = 1
end

-----------------------------------------------------------
-- Focus Management (Text Editing)
-----------------------------------------------------------

function dropdown.focus(id)
    local dd = dropdowns[id]
    if not dd then return end
    if dd.focused then return end
    
    for otherId, otherDd in pairs(dropdowns) do
        if otherId ~= id and otherDd.focused then
            dropdown.unfocus(otherId)
        end
    end
    
    dd.focused = true
    UI.setFocus(dd.displayBoxId)
    UI.setBoxTextures(dd.displayBoxId, displayTexSetFocused)
    
    local text = UI.getTextInput(dd.displayBoxId) or ""
    UI.setCursor(dd.displayBoxId, #text)
    
    if dd.cursorId then
        UI.setVisible(dd.cursorId, true)
    end
    
    dropdown.updateDisplay(id)
    engine.logDebug("Dropdown focused: " .. dd.name)
end

function dropdown.unfocus(id)
    local dd = dropdowns[id]
    if not dd then return end
    if not dd.focused then return end
    
    dd.focused = false
    
    if UI.hasFocus(dd.displayBoxId) then
        UI.clearFocus()
    end
    
    UI.setBoxTextures(dd.displayBoxId, displayTexSetNormal)
    
    if dd.cursorId then
        UI.setVisible(dd.cursorId, false)
    end
    
    local displayText = ""
    if dd.selectedIndex and dd.options[dd.selectedIndex] then
        displayText = dd.options[dd.selectedIndex].text
    end
    UI.setTextInput(dd.displayBoxId, displayText)
    UI.setText(dd.displayTextId, displayText)
    dropdown.updateDisplay(id)
    
    engine.logDebug("Dropdown unfocused: " .. dd.name)
end

function dropdown.unfocusAll()
    for id, dd in pairs(dropdowns) do
        if dd.focused then
            dropdown.unfocus(id)
        end
    end
end

function dropdown.isFocused(id)
    local dd = dropdowns[id]
    if not dd then return false end
    return dd.focused
end

function dropdown.getFocusedId()
    for id, dd in pairs(dropdowns) do
        if dd.focused then
            return id
        end
    end
    return nil
end

-----------------------------------------------------------
-- Display Update
-----------------------------------------------------------

function dropdown.updateDisplay(id)
    local dd = dropdowns[id]
    if not dd then return end
    if not dd.displayTextId then return end
    
    local text = UI.getTextInput(dd.displayBoxId) or ""
    local cursorPos = UI.getCursor(dd.displayBoxId) or 0
    
    local textWidth = engine.getTextWidth(dd.font, text, dd.fontSize)
    local textX = dd.textPadding
    local textY = (dd.height / 2) + (dd.fontSize / 3)
    
    UI.setText(dd.displayTextId, text)
    UI.setPosition(dd.displayTextId, textX, textY)
    
    if dd.cursorId and dd.focused then
        local textBeforeCursor = text:sub(1, cursorPos)
        local cursorTextWidth = engine.getTextWidth(dd.font, textBeforeCursor, dd.fontSize)
        local cursorX = textX + cursorTextWidth - (engine.getTextWidth(dd.font, "|", dd.fontSize) / 2)
        UI.setPosition(dd.cursorId, cursorX, textY)
    end
end

-----------------------------------------------------------
-- Text Input Submission
-----------------------------------------------------------

function dropdown.submitInput(id)
    local dd = dropdowns[id]
    if not dd then return end
    
    local inputText = UI.getTextInput(dd.displayBoxId) or ""
    engine.logDebug("Dropdown submit: " .. dd.name .. " input='" .. inputText .. "'")
    
    local matchIndex = dropdown.findBestMatch(dd, inputText)
    
    if matchIndex then
        dropdown.selectOption(id, matchIndex)
    else
        engine.logDebug("Dropdown no match for: " .. inputText)
    end
    
    dropdown.unfocus(id)
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
    dd.scrollOffset = 0
    dd.hoveredOptionIndex = nil
    UI.setSpriteTexture(dd.arrowSpriteId, texArrowClicked)
    
    local totalOptions = #dd.options
    local visibleCount = math.min(totalOptions, dd.maxVisibleOptions)
    dd.needsScroll = totalOptions > dd.maxVisibleOptions
    
    local listHeight = visibleCount * dd.optionHeight
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
    
    UI.addToPage(dd.page, dd.listBoxId, listX, listY)
    UI.setZIndex(dd.listBoxId, 500)
    
    -- Create only the visible option slots (virtual scrolling)
    dd.optionElements = {}
    for i = 1, visibleCount do
        local optY = (i - 1) * dd.optionHeight
        local textY = optY + (dd.optionHeight / 2) + (dd.fontSize / 3)
        
        local highlightId = UI.newSprite(
            dd.name .. "_opt_hl_" .. i,
            dd.displayWidth,
            dd.optionHeight,
            highlightTex,
            dd.highlightColor[1], dd.highlightColor[2],
            dd.highlightColor[3], dd.highlightColor[4],
            dd.page
        )
        UI.addChild(dd.listBoxId, highlightId, 0, optY)
        UI.setZIndex(highlightId, 1)
        UI.setVisible(highlightId, false)
        
        local dataIndex = dd.scrollOffset + i
        local optText = ""
        if dataIndex <= totalOptions then
            optText = dd.options[dataIndex].text
        end
        
        local optTextId = UI.newText(
            dd.name .. "_opt_" .. i,
            optText,
            dd.font,
            dd.fontSize,
            dd.textColor[1], dd.textColor[2], dd.textColor[3], dd.textColor[4],
            dd.page
        )
        UI.addChild(dd.listBoxId, optTextId, dd.textPadding, textY)
        UI.setZIndex(optTextId, 3)
        
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
        UI.setZIndex(optBoxId, 4)
        
        table.insert(dd.optionElements, {
            boxId = optBoxId,
            textId = optTextId,
            highlightId = highlightId,
            slot = i,
        })
    end
    
    -- Create scrollbar if needed
    if dd.needsScroll then
        local scrollTrackHeight = listHeight - (dd.arrowSize * 2) - (math.floor(4 * dd.uiscale) * 2)
        if scrollTrackHeight < math.floor(20 * dd.uiscale) then
            scrollTrackHeight = math.floor(20 * dd.uiscale)
        end
        
        dd.scrollbarId = scrollbar.new({
            name = dd.name .. "_scrollbar",
            page = dd.page,
            x = dd.x + dd.displayWidth,
            y = listY,
            buttonSize = dd.arrowSize,
            trackHeight = scrollTrackHeight,
            capHeight = math.floor(4 * dd.uiscale),
            tileSize = math.floor(8 * dd.uiscale),
            totalItems = totalOptions,
            visibleItems = visibleCount,
            uiscale = dd.uiscale,
            onScroll = function(offset, sbId, sbName)
                dropdown.onScrollChanged(id, offset)
            end,
        })
    end
    
    engine.logDebug("Dropdown list opened: " .. dd.name
        .. " visible=" .. visibleCount .. "/" .. totalOptions
        .. " scroll=" .. tostring(dd.needsScroll))
end

function dropdown.closeList(id)
    local dd = dropdowns[id]
    if not dd then return end
    if not dd.open then return end
    
    dd.open = false
    dd.hoveredOptionIndex = nil
    dd.scrollOffset = 0
    UI.setSpriteTexture(dd.arrowSpriteId, texArrowNormal)
    
    if dd.scrollbarId then
        scrollbar.destroy(dd.scrollbarId)
        dd.scrollbarId = nil
    end
    
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
-- Virtual Scrolling
-----------------------------------------------------------

function dropdown.onScrollChanged(id, newOffset)
    local dd = dropdowns[id]
    if not dd or not dd.open then return end
    
    dd.scrollOffset = newOffset
    dd.hoveredOptionIndex = nil
    dropdown.refreshVisibleOptions(id)
end

function dropdown.refreshVisibleOptions(id)
    local dd = dropdowns[id]
    if not dd or not dd.open then return end
    
    for _, opt in ipairs(dd.optionElements) do
        local dataIndex = dd.scrollOffset + opt.slot
        
        if dataIndex <= #dd.options then
            local optData = dd.options[dataIndex]
            UI.setText(opt.textId, optData.text)
            UI.setVisible(opt.highlightId, false)
            UI.setColor(opt.textId,
                dd.textColor[1], dd.textColor[2],
                dd.textColor[3], dd.textColor[4])
        else
            UI.setText(opt.textId, "")
            UI.setVisible(opt.highlightId, false)
        end
    end
end

-----------------------------------------------------------
-- Scroll Input Handling (mouse wheel)
-----------------------------------------------------------

function dropdown.onScroll(elemHandle, dx, dy)
    -- Find which dropdown this scroll belongs to
    for id, dd in pairs(dropdowns) do
        if dd.open and dd.needsScroll and dd.scrollbarId then
            -- Check if the element is part of this dropdown's list
            local isInList = false
            
            -- Check list box itself
            if elemHandle == dd.listBoxId then
                isInList = true
            end
            
            -- Check option elements
            if not isInList then
                for _, opt in ipairs(dd.optionElements) do
                    if opt.boxId == elemHandle or opt.textId == elemHandle
                        or opt.highlightId == elemHandle then
                        isInList = true
                        break
                    end
                end
            end
            
            -- Check scrollbar elements
            if not isInList then
                local sbId, _ = scrollbar.findByElementHandle(elemHandle)
                if sbId == dd.scrollbarId then
                    isInList = true
                end
            end
            
            if isInList then
                -- dy > 0 means scroll up, dy < 0 means scroll down
                if dy > 0 then
                    scrollbar.scrollUp(dd.scrollbarId)
                elseif dy < 0 then
                    scrollbar.scrollDown(dd.scrollbarId)
                end
                return
            end
        end
    end
end

-----------------------------------------------------------
-- Hover Handling
-----------------------------------------------------------

function dropdown.setHoveredOption(id, optionSlot)
    local dd = dropdowns[id]
    if not dd or not dd.open then return end
    if dd.hoveredOptionIndex == optionSlot then return end
    
    if dd.hoveredOptionIndex then
        local prevOpt = dd.optionElements[dd.hoveredOptionIndex]
        if prevOpt then
            UI.setVisible(prevOpt.highlightId, false)
            UI.setColor(prevOpt.textId,
                dd.textColor[1], dd.textColor[2],
                dd.textColor[3], dd.textColor[4])
        end
    end
    
    dd.hoveredOptionIndex = optionSlot
    
    if optionSlot then
        local opt = dd.optionElements[optionSlot]
        if opt then
            local dataIndex = dd.scrollOffset + optionSlot
            if dataIndex >= 1 and dataIndex <= #dd.options then
                UI.setVisible(opt.highlightId, true)
                UI.setColor(opt.textId,
                    dd.highlightTextColor[1], dd.highlightTextColor[2],
                    dd.highlightTextColor[3], dd.highlightTextColor[4])
            end
        end
    end
end

function dropdown.clearHover(id)
    dropdown.setHoveredOption(id, nil)
end

function dropdown.onHoverEnter(elemHandle)
    for id, dd in pairs(dropdowns) do
        if dd.open then
            for _, opt in ipairs(dd.optionElements) do
                if opt.boxId == elemHandle then
                    dropdown.setHoveredOption(id, opt.slot)
                    return
                end
            end
        end
    end
end

function dropdown.onHoverLeave(elemHandle)
    for id, dd in pairs(dropdowns) do
        if dd.open and dd.hoveredOptionIndex then
            local opt = dd.optionElements[dd.hoveredOptionIndex]
            if opt and opt.boxId == elemHandle then
                dropdown.clearHover(id)
                return
            end
        end
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
    
    UI.setTextInput(dd.displayBoxId, opt.text)
    if dd.displayTextId then
        UI.setText(dd.displayTextId, opt.text)
    end
    
    dropdown.closeList(id)
    dropdown.unfocus(id)
    
    if dd.onChange then
        dd.onChange(opt.value, opt.text, id, dd.name)
    end
    
    engine.logDebug("Dropdown selected: " .. dd.name .. " = " .. opt.text)
end

function dropdown.setOptions(id, options, defaultValue)
    local dd = dropdowns[id]
    if not dd then return end
    
    dropdown.closeList(id)
    dropdown.unfocus(id)
    
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
    UI.setTextInput(dd.displayBoxId, displayText)
    if dd.displayTextId then
        UI.setText(dd.displayTextId, displayText)
    end
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function dropdown.findByElementHandle(elemHandle)
    for id, dd in pairs(dropdowns) do
        if dd.displayBoxId == elemHandle then
            return id, "display"
        end
        if dd.arrowSpriteId == elemHandle then
            return id, "toggle"
        end
        for _, opt in ipairs(dd.optionElements) do
            if opt.boxId == elemHandle then
                return id, "option", opt.slot
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
    elseif callbackName == DISPLAY_CALLBACK then
        local id, action = dropdown.findByElementHandle(elemHandle)
        if id then
            local dd = dropdowns[id]
            if dd.open then
                dropdown.closeList(id)
            end
            dropdown.focus(id)
            return true
        end
    elseif callbackName == OPTION_CALLBACK then
        local id, action, slot = dropdown.findByElementHandle(elemHandle)
        if id and slot then
            local dd = dropdowns[id]
            local dataIndex = dd.scrollOffset + slot
            if dataIndex >= 1 and dataIndex <= #dd.options then
                dropdown.selectOption(id, dataIndex)
            end
            return true
        end
    elseif callbackName == "onScrollUp" then
        local sbId, action = scrollbar.findByElementHandle(elemHandle)
        if sbId then
            scrollbar.scrollUp(sbId)
            return true
        end
    elseif callbackName == "onScrollDown" then
        local sbId, action = scrollbar.findByElementHandle(elemHandle)
        if sbId then
            scrollbar.scrollDown(sbId)
            return true
        end
    end
    return false
end

function dropdown.onClickOutside(mouseX, mouseY)
    for id, dd in pairs(dropdowns) do
        if dd.open then
            local scrollWidth = 0
            if dd.scrollbarId then
                scrollWidth = scrollbar.getTrackWidth(dd.scrollbarId)
            end
            local visibleCount = math.min(#dd.options, dd.maxVisibleOptions)
            local totalHeight = dd.height + (visibleCount * dd.optionHeight)
            local totalWidth = dd.displayWidth + scrollWidth
            if mouseX < dd.x or mouseX > dd.x + totalWidth
                or mouseY < dd.y or mouseY > dd.y + totalHeight then
                dropdown.closeList(id)
            end
        end
        if dd.focused then
            local inDisplay = mouseX >= dd.x and mouseX <= dd.x + dd.displayWidth
                and mouseY >= dd.y and mouseY <= dd.y + dd.height
            if not inDisplay then
                dropdown.submitInput(id)
            end
        end
    end
end

-----------------------------------------------------------
-- Input Event Handlers (forwarded from uiManager)
-----------------------------------------------------------

function dropdown.onCharInput(char)
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    if not dropdown.isValidChar(dd, char) then
        return true
    end
    
    UI.insertChar(dd.displayBoxId, char)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onBackspace()
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    UI.deleteBackward(dd.displayBoxId)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onDelete()
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    UI.deleteForward(dd.displayBoxId)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onCursorLeft()
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    UI.cursorLeft(dd.displayBoxId)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onCursorRight()
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    UI.cursorRight(dd.displayBoxId)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onHome()
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    UI.cursorHome(dd.displayBoxId)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onEnd()
    local id = dropdown.getFocusedId()
    if not id then return false end
    local dd = dropdowns[id]
    
    UI.cursorEnd(dd.displayBoxId)
    dropdown.updateDisplay(id)
    return true
end

function dropdown.onSubmit()
    local id = dropdown.getFocusedId()
    if not id then return false end
    
    dropdown.submitInput(id)
    return true
end

function dropdown.onEscape()
    local id = dropdown.getFocusedId()
    if not id then return false end
    
    dropdown.unfocus(id)
    return true
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

local cursorBlinkTime = 0
local cursorBlinkRate = 0.5
local cursorVisible = true

function dropdown.update(dt)
    local id = dropdown.getFocusedId()
    if not id then
        cursorBlinkTime = 0
        cursorVisible = true
        return
    end
    
    local dd = dropdowns[id]
    cursorBlinkTime = cursorBlinkTime + dt
    if cursorBlinkTime >= cursorBlinkRate then
        cursorBlinkTime = cursorBlinkTime - cursorBlinkRate
        cursorVisible = not cursorVisible
        if dd.cursorId then
            UI.setVisible(dd.cursorId, cursorVisible)
        end
    end
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
    return callbackName == DROPDOWN_CALLBACK
        or callbackName == OPTION_CALLBACK
        or callbackName == DISPLAY_CALLBACK
        or callbackName == "onScrollUp"
        or callbackName == "onScrollDown"
end

return dropdown
