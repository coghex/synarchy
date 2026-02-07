-- TextBox UI component (using engine focus and text buffer)
local boxTextures = require("scripts.ui.box_textures")
local textbox = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
textbox.CALLBACK = "onTextBoxClick"

textbox.Type = {
    TEXT = "text",
    NUMBER = "number",
    DECIMAL = "decimal",
    SCALE = "scale",
}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local textboxes = {}
local nextId = 1

local texSetNormal = nil
local texSetSelected = nil
local assetsLoaded = false

local defaultTileSize = 16

local cursorBlinkTime = 0
local cursorBlinkRate = 0.5
local cursorVisible = true

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function textbox.init()
    if assetsLoaded then return end
    
    texSetNormal = boxTextures.load("assets/textures/ui/textbox", "textbox")
    texSetSelected = boxTextures.load("assets/textures/ui/textboxselected", "textbox")
    
    assetsLoaded = true
    engine.logDebug("TextBox module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function textbox.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or 1.0
    local width = math.floor((params.width or 200) * uiscale)
    local height = math.floor((params.height or 40) * uiscale)
    local tileSize = math.floor((params.tileSize or defaultTileSize) * uiscale)
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local textPadding = math.floor(8 * uiscale)
    
    local tb = {
        id = id,
        name = params.name or ("textbox_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = width,
        height = height,
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
        textType = params.textType or textbox.Type.TEXT,
        suffix = "",
        uiscale = uiscale,
    }
    
    if tb.textType == textbox.Type.SCALE then
        tb.suffix = "x"
    end
    
    tb.boxId = UI.newBox(
        tb.name .. "_box",
        tb.width,
        tb.height,
        texSetNormal,
        tb.tileSize,
        1.0, 1.0, 1.0, 1.0,
        tb.page
    )
    
    UI.enableTextInput(tb.boxId)
    
    local cleanDefault = tb.defaultValue:gsub("x$", "")
    if cleanDefault ~= "" then
        UI.setTextInput(tb.boxId, cleanDefault)
    end
    
    if tb.parent then
        UI.addChild(tb.parent, tb.boxId, tb.x, tb.y)
    elseif tb.page then
        UI.addToPage(tb.page, tb.boxId, tb.x, tb.y)
    end
    
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
    
    UI.setClickable(tb.boxId, true)
    UI.setOnClick(tb.boxId, textbox.CALLBACK)
    
    textboxes[id] = tb
    
    textbox.updateDisplay(id)
    
    return id
end

function textbox.destroy(id)
    local tb = textboxes[id]
    if not tb then return end
    
    if tb.boxId and UI.hasFocus(tb.boxId) then
        UI.clearFocus()
    end
    
    textboxes[id] = nil
    engine.logDebug("TextBox destroyed: " .. tb.name)
end

function textbox.destroyAll()
    UI.clearFocus()
    
    for id, tb in pairs(textboxes) do
        engine.logDebug("TextBox destroyed: " .. tb.name)
    end
    textboxes = {}
    nextId = 1
    
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
    
    if tb.textType == textbox.Type.TEXT then
        return true
    elseif tb.textType == textbox.Type.NUMBER then
        return char:match("^%d$") ~= nil
    elseif tb.textType == textbox.Type.DECIMAL or tb.textType == textbox.Type.SCALE then
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
    
    local textWidth = engine.getTextWidth(tb.font, displayText, tb.fontSize)
    local availableWidth = tb.width - (tb.textPadding * 2)
    
    local textX = tb.width - tb.textPadding - textWidth
    if textX < tb.textPadding then
        textX = tb.textPadding
    end
    
    UI.setText(tb.textId, displayText)
    local textY = (tb.height / 2) + (tb.fontSize / 3)
    engine.logInfo("TBDEBUG " .. tb.name .. " text='" .. displayText .. "' textW=" .. tostring(textWidth) .. " textX=" .. tostring(textX) .. " textY=" .. tostring(textY) .. " boxId=" .. tostring(tb.boxId) .. " textId=" .. tostring(tb.textId))
    UI.setPosition(tb.textId, textX, textY)
    
    if tb.cursorId then
        local textBeforeCursor = rawText:sub(1, cursorPos)
        local cursorTextWidth = engine.getTextWidth(tb.font, textBeforeCursor, tb.fontSize)
        local cursorX = textX + cursorTextWidth - (engine.getTextWidth(tb.font, "|", tb.fontSize) / 2)
        UI.setPosition(tb.cursorId, cursorX, textY)
    end
    engine.logInfo("TBDEBUG " .. tb.name .. " text='" .. displayText .. "' textW=" .. tostring(textWidth) .. " textX=" .. tostring(textX) .. " textY=" .. tostring(textY) .. " boxW=" .. tostring(tb.width) .. " boxH=" .. tostring(tb.height))
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
    if callbackName == textbox.CALLBACK and elemHandle then
        return textbox.handleClickByElement(elemHandle)
    end
    return false
end

function textbox.focus(id)
    local tb = textboxes[id]
    if not tb then return end
    
    for otherId, otherTb in pairs(textboxes) do
        if otherId ~= id and otherTb.boxId and UI.hasFocus(otherTb.boxId) then
            UI.setBoxTextures(otherTb.boxId, texSetNormal)
            if otherTb.cursorId then
                UI.setVisible(otherTb.cursorId, false)
            end
        end
    end
    
    UI.setFocus(tb.boxId)
    UI.setBoxTextures(tb.boxId, texSetSelected)
    
    if tb.cursorId then
        cursorVisible = true
        cursorBlinkTime = 0
        UI.setVisible(tb.cursorId, true)
    end
    
    local text = UI.getTextInput(tb.boxId) or ""
    UI.setCursor(tb.boxId, #text)
    textbox.updateDisplay(id)
    
    engine.logDebug("TextBox focused: " .. tb.name)
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
    
    engine.logDebug("TextBox unfocused: " .. tb.name)
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
    return callbackName == textbox.CALLBACK
end

-----------------------------------------------------------
-- Cursor Operations
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
-- Input Event Handlers
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
        engine.logDebug("TextBox submitted: " .. tb.name .. " = '" .. displayText .. "' (value=" .. value .. ")")
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

-----------------------------------------------------------
-- Size Query
-----------------------------------------------------------

function textbox.getSize(id)
    local tb = textboxes[id]
    if not tb then return 0, 0 end
    return tb.width, tb.height
end

return textbox
