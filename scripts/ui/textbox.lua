-- TextBox UI component (using engine focus and text buffer)
local textbox = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local TEXTBOX_CALLBACK = "onTextBoxClick"

-- Textbox types
local TextBoxType = {
    TEXT = "text",           -- Any text
    NUMBER = "number",       -- Numbers only (integer)
    DECIMAL = "decimal",     -- Numbers with decimal point
    SCALE = "scale",         -- Scale value (e.g., "1.5x")
}
textbox.Type = TextBoxType

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

-- Storage for all textboxes
local textboxes = {}
local nextId = 1

-- Shared textures (loaded once) - 9-tile box texture sets
local texSetNormal = nil
local texSetSelected = nil
local assetsLoaded = false

-- Default tile size for 9-tile boxes
local defaultTileSize = 16

-- Cursor blink state (shared across all textboxes)
local cursorBlinkTime = 0
local cursorBlinkRate = 0.5
local cursorVisible = true

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function textbox.init()
    if assetsLoaded then return end
    
    -- Load normal textbox textures (9-tile)
    local texCenter = engine.loadTexture("assets/textures/ui/textbox/textbox.png")
    local texN = engine.loadTexture("assets/textures/ui/textbox/textboxn.png")
    local texS = engine.loadTexture("assets/textures/ui/textbox/textboxs.png")
    local texE = engine.loadTexture("assets/textures/ui/textbox/textboxe.png")
    local texW = engine.loadTexture("assets/textures/ui/textbox/textboxw.png")
    local texNE = engine.loadTexture("assets/textures/ui/textbox/textboxne.png")
    local texNW = engine.loadTexture("assets/textures/ui/textbox/textboxnw.png")
    local texSE = engine.loadTexture("assets/textures/ui/textbox/textboxse.png")
    local texSW = engine.loadTexture("assets/textures/ui/textbox/textboxsw.png")
    texSetNormal = UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW)
    
    -- Load selected textbox textures (9-tile)
    local texCenterSel = engine.loadTexture("assets/textures/ui/textboxselected/textbox.png")
    local texNSel = engine.loadTexture("assets/textures/ui/textboxselected/textboxn.png")
    local texSSel = engine.loadTexture("assets/textures/ui/textboxselected/textboxs.png")
    local texESel = engine.loadTexture("assets/textures/ui/textboxselected/textboxe.png")
    local texWSel = engine.loadTexture("assets/textures/ui/textboxselected/textboxw.png")
    local texNESel = engine.loadTexture("assets/textures/ui/textboxselected/textboxne.png")
    local texNWSel = engine.loadTexture("assets/textures/ui/textboxselected/textboxnw.png")
    local texSESel = engine.loadTexture("assets/textures/ui/textboxselected/textboxse.png")
    local texSWSel = engine.loadTexture("assets/textures/ui/textboxselected/textboxsw.png")
    texSetSelected = UI.loadBoxTextures(texCenterSel, texNSel, texSSel, texESel, texWSel, texNESel, texNWSel, texSESel, texSWSel)
    
    assetsLoaded = true
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function textbox.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or 1.0
    local tileSize = math.floor((params.tileSize or defaultTileSize) * uiscale)
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local textPadding = math.floor(8 * uiscale)
    
    local tb = {
        id = id,
        name = params.name or ("textbox_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = params.width or 200,
        height = params.height or 40,
        tileSize = tileSize,
        fontSize = fontSize,
        textPadding = textPadding,
        page = params.page,
        parent = params.parent,
        boxId = nil,
        textId = nil,
        cursorId = nil,
        defaultValue = params.default or "",
        font = params.font,
        textType = params.textType or TextBoxType.TEXT,
        suffix = "",  -- Suffix to display (e.g., "x" for scale)
    }
    
    -- Set suffix based on type
    if tb.textType == TextBoxType.SCALE then
        tb.suffix = "x"
    end
    
    -- Create the box using UI system (9-tile) with WHITE background
    tb.boxId = UI.newBox(
        tb.name .. "_box",
        tb.width,
        tb.height,
        texSetNormal,
        tb.tileSize,
        1.0, 1.0, 1.0, 1.0,
        tb.page
    )
    
    -- Enable text input on this element (creates the buffer in engine)
    UI.enableTextInput(tb.boxId)
    
    -- Set default value if provided (strip suffix for storage)
    local cleanDefault = tb.defaultValue:gsub("x$", "")
    if cleanDefault ~= "" then
        UI.setTextInput(tb.boxId, cleanDefault)
    end
    
    -- Add to parent or page
    if tb.parent then
        UI.addChild(tb.parent, tb.boxId, tb.x, tb.y)
    elseif tb.page then
        UI.addToPage(tb.page, tb.boxId, tb.x, tb.y)
    end
    
    -- Create text element for display (BLACK text)
    if tb.font then
        local textY = (tb.height / 2) + (tb.fontSize / 3)
        tb.textId = UI.newText(
            tb.name .. "_text",
            "",
            tb.font,
            tb.fontSize,
            0.0, 0.0, 0.0, 1.0,
            tb.page
        )
        UI.addChild(tb.boxId, tb.textId, tb.textPadding, textY)
        UI.setZIndex(tb.textId, 1)
        
        -- Create cursor element (BLACK, initially hidden)
        tb.cursorId = UI.newText(
            tb.name .. "_cursor",
            "|",
            tb.font,
            tb.fontSize,
            0.0, 0.0, 0.0, 1.0,
            tb.page
        )
        UI.addChild(tb.boxId, tb.cursorId, tb.textPadding, textY)
        UI.setZIndex(tb.cursorId, 2)
        UI.setVisible(tb.cursorId, false)
    end
    
    -- Make it clickable
    UI.setClickable(tb.boxId, true)
    UI.setOnClick(tb.boxId, TEXTBOX_CALLBACK)
    
    textboxes[id] = tb
    
    -- Initial display update
    textbox.updateDisplay(id)
    
    return id
end

function textbox.destroy(id)
    local tb = textboxes[id]
    if not tb then return end
    
    -- Clear focus if this was focused
    if tb.boxId and UI.hasFocus(tb.boxId) then
        UI.clearFocus()
    end
    
    -- Note: We don't destroy the UI elements here because 
    -- they will be destroyed when the page is deleted.
    -- We just remove our tracking of them.
    
    textboxes[id] = nil
    engine.logInfo("TextBox destroyed: " .. tb.name)
end

function textbox.destroyAll()
    -- Clear any focus first
    UI.clearFocus()
    
    -- Clear all textbox entries
    for id, tb in pairs(textboxes) do
        engine.logInfo("TextBox destroyed: " .. tb.name)
    end
    textboxes = {}
    nextId = 1
    
    -- Reset cursor state
    cursorVisible = true
    cursorBlinkTime = 0
end

-----------------------------------------------------------
-- Validation
-----------------------------------------------------------

function textbox.isValidChar(id, char)
    local tb = textboxes[id]
    if not tb then return false end
    
    local currentText = UI.getTextInput(tb.boxId) or ""
    
    if tb.textType == TextBoxType.TEXT then
        return true
    elseif tb.textType == TextBoxType.NUMBER then
        return char:match("^%d$") ~= nil
    elseif tb.textType == TextBoxType.DECIMAL or tb.textType == TextBoxType.SCALE then
        if char:match("^%d$") then
            return true
        elseif char == "." then
            return not currentText:find("%.")
        end
        return false
    end
    
    return true
end

-----------------------------------------------------------
-- Value Access
-----------------------------------------------------------

function textbox.formatDisplayText(id)
    local tb = textboxes[id]
    if not tb then return "" end
    
    local text = UI.getTextInput(tb.boxId) or ""
    
    if tb.suffix ~= "" and text ~= "" then
        return text .. tb.suffix
    end
    
    return text
end

function textbox.getValue(id)
    local tb = textboxes[id]
    if not tb then return "" end
    return UI.getTextInput(tb.boxId) or ""
end

function textbox.getNumericValue(id)
    local tb = textboxes[id]
    if not tb then return 0 end
    local text = UI.getTextInput(tb.boxId) or "0"
    return tonumber(text) or 0
end

function textbox.getText(id)
    local tb = textboxes[id]
    if not tb then return "" end
    return UI.getTextInput(tb.boxId) or ""
end

function textbox.setText(id, text)
    local tb = textboxes[id]
    if not tb then return end
    UI.setTextInput(tb.boxId, text)
    textbox.updateDisplay(id)
end

-----------------------------------------------------------
-- Display Updates
-----------------------------------------------------------

function textbox.updateDisplay(id)
    local tb = textboxes[id]
    if not tb then return end
    if not tb.textId then return end
    
    local displayText = textbox.formatDisplayText(id)
    local rawText = UI.getTextInput(tb.boxId) or ""
    local cursorPos = UI.getCursor(tb.boxId) or 0
    
    -- Calculate text width for right justification
    local textWidth = engine.getTextWidth(tb.font, displayText, tb.fontSize)
    local availableWidth = tb.width - (tb.textPadding * 2)
    
    -- Right justify: position text so it ends at the right edge
    local textX = tb.width - tb.textPadding - textWidth
    if textX < tb.textPadding then
        textX = tb.textPadding
    end
    
    -- Update text position and content
    UI.setText(tb.textId, displayText)
    local textY = (tb.height / 2) + (tb.fontSize / 3)
    UI.setPosition(tb.textId, textX, textY)
    
    -- Update cursor position
    if tb.cursorId then
        local textBeforeCursor = rawText:sub(1, cursorPos)
        local cursorTextWidth = engine.getTextWidth(tb.font, textBeforeCursor, tb.fontSize)
        local cursorX = textX + cursorTextWidth - (engine.getTextWidth(tb.font, "|", tb.fontSize) / 2)
        UI.setPosition(tb.cursorId, cursorX, textY)
    end
end

function textbox.update(dt)
    cursorBlinkTime = cursorBlinkTime + dt
    
    if cursorBlinkTime >= cursorBlinkRate then
        cursorBlinkTime = cursorBlinkTime - cursorBlinkRate
        cursorVisible = not cursorVisible
        
        local focusedId = textbox.getFocusedId()
        if focusedId then
            local tb = textboxes[focusedId]
            if tb and tb.cursorId then
                UI.setVisible(tb.cursorId, cursorVisible)
            end
        end
    end
end

-----------------------------------------------------------
-- Focus Management
-----------------------------------------------------------

function textbox.findByElementHandle(elemHandle)
    for id, tb in pairs(textboxes) do
        if tb.boxId == elemHandle then
            return id
        end
    end
    return nil
end

function textbox.handleClickByElement(elemHandle)
    local id = textbox.findByElementHandle(elemHandle)
    if id then
        textbox.focus(id)
        return true
    end
    return false
end

function textbox.isTextBoxElement(elemHandle)
    return textbox.findByElementHandle(elemHandle) ~= nil
end

function textbox.handleCallback(callbackName, elemHandle)
    if callbackName == TEXTBOX_CALLBACK and elemHandle then
        return textbox.handleClickByElement(elemHandle)
    end
    return false
end

function textbox.focus(id)
    local tb = textboxes[id]
    if not tb then return end
    
    -- Unfocus any previously focused textbox (update visuals)
    for otherId, otherTb in pairs(textboxes) do
        if otherId ~= id and otherTb.boxId and UI.hasFocus(otherTb.boxId) then
            UI.setBoxTextures(otherTb.boxId, texSetNormal)
            if otherTb.cursorId then
                UI.setVisible(otherTb.cursorId, false)
            end
        end
    end
    
    -- Set engine focus to this element
    UI.setFocus(tb.boxId)
    
    -- Update visual
    UI.setBoxTextures(tb.boxId, texSetSelected)
    
    -- Show cursor and reset blink
    if tb.cursorId then
        cursorVisible = true
        cursorBlinkTime = 0
        UI.setVisible(tb.cursorId, true)
    end
    
    -- Move cursor to end
    local text = UI.getTextInput(tb.boxId) or ""
    UI.setCursor(tb.boxId, #text)
    textbox.updateDisplay(id)
    
    engine.logInfo("TextBox focused: " .. tb.name)
end

function textbox.unfocus(id)
    local tb = textboxes[id]
    if not tb then return end
    
    if UI.hasFocus(tb.boxId) then
        UI.clearFocus()
    end
    
    UI.setBoxTextures(tb.boxId, texSetNormal)
    
    if tb.cursorId then
        UI.setVisible(tb.cursorId, false)
    end
    
    engine.logInfo("TextBox unfocused: " .. tb.name)
end

function textbox.unfocusAll()
    for id, tb in pairs(textboxes) do
        if tb.boxId and UI.hasFocus(tb.boxId) then
            textbox.unfocus(id)
            return
        end
    end
    UI.clearFocus()
end

function textbox.isFocused(id)
    local tb = textboxes[id]
    if not tb then return false end
    return UI.hasFocus(tb.boxId)
end

function textbox.getFocusedId()
    for id, tb in pairs(textboxes) do
        if tb.boxId and UI.hasFocus(tb.boxId) then
            return id
        end
    end
    return nil
end

function textbox.getElementHandle(id)
    local tb = textboxes[id]
    if not tb then return nil end
    return tb.boxId
end

function textbox.isTextBoxCallback(callbackName)
    if not callbackName then return false end
    return callbackName == TEXTBOX_CALLBACK
end

-----------------------------------------------------------
-- Cursor Operations (delegate to UI)
-----------------------------------------------------------

function textbox.getCursor(id)
    local tb = textboxes[id]
    if not tb then return 0 end
    return UI.getCursor(tb.boxId) or 0
end

function textbox.setCursor(id, pos)
    local tb = textboxes[id]
    if not tb then return end
    UI.setCursor(tb.boxId, pos)
    textbox.updateDisplay(id)
end

function textbox.insertChar(id, char)
    local tb = textboxes[id]
    if not tb then return end
    
    if not textbox.isValidChar(id, char) then
        return
    end
    
    UI.insertChar(tb.boxId, char)
end

function textbox.deleteBackward(id)
    local tb = textboxes[id]
    if not tb then return end
    UI.deleteBackward(tb.boxId)
end

function textbox.deleteForward(id)
    local tb = textboxes[id]
    if not tb then return end
    UI.deleteForward(tb.boxId)
end

function textbox.cursorLeft(id)
    local tb = textboxes[id]
    if not tb then return end
    UI.cursorLeft(tb.boxId)
end

function textbox.cursorRight(id)
    local tb = textboxes[id]
    if not tb then return end
    UI.cursorRight(tb.boxId)
end

function textbox.cursorHome(id)
    local tb = textboxes[id]
    if not tb then return end
    UI.cursorHome(tb.boxId)
end

function textbox.cursorEnd(id)
    local tb = textboxes[id]
    if not tb then return end
    UI.cursorEnd(tb.boxId)
end

-----------------------------------------------------------
-- Input Event Handlers (called from ui_manager)
-----------------------------------------------------------

function textbox.onCharInput(char)
    local id = textbox.getFocusedId()
    if id then
        textbox.insertChar(id, char)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onBackspace()
    local id = textbox.getFocusedId()
    if id then
        textbox.deleteBackward(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onDelete()
    local id = textbox.getFocusedId()
    if id then
        textbox.deleteForward(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onCursorLeft()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorLeft(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onCursorRight()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorRight(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onHome()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorHome(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onEnd()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorEnd(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

function textbox.onSubmit()
    local id = textbox.getFocusedId()
    if id then
        local tb = textboxes[id]
        local value = textbox.getValue(id)
        local displayText = textbox.formatDisplayText(id)
        engine.logInfo("TextBox submitted: " .. tb.name .. " = '" .. displayText .. "' (value=" .. value .. ")")
        textbox.unfocus(id)
        return true, value, id, tb.name
    end
    return false, nil, nil, nil
end

function textbox.onEscape()
    local id = textbox.getFocusedId()
    if id then
        textbox.unfocus(id)
        return true
    end
    return false
end

return textbox
