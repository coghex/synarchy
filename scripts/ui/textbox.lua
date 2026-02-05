-- TextBox UI component (using engine focus and text buffer)
local textbox = {}

-- Storage for all textboxes
local textboxes = {}
local nextId = 1

-- Shared textures (loaded once) - 9-tile box texture sets
local texSetNormal = nil
local texSetSelected = nil
local assetsLoaded = false

-- Default tile size for 9-tile boxes
local defaultTileSize = 16

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
    engine.logInfo("TextBox module initialized")
end

function textbox.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or 1.0
    local tileSize = math.floor((params.tileSize or defaultTileSize) * uiscale)
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    
    local tb = {
        id = id,
        name = params.name or ("textbox_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = params.width or 200,
        height = params.height or 40,
        tileSize = tileSize,
        fontSize = fontSize,
        page = params.page,
        parent = params.parent,
        boxId = nil,
        textId = nil,
        defaultValue = params.default or "",
        callbackName = "onTextBoxClick",
        font = params.font,
    }
    
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
    
    -- Set default value if provided
    if tb.defaultValue ~= "" then
        UI.setTextInput(tb.boxId, tb.defaultValue)
    end
    
    -- Add to parent or page
    if tb.parent then
        UI.addChild(tb.parent, tb.boxId, tb.x, tb.y)
    elseif tb.page then
        UI.addToPage(tb.page, tb.boxId, tb.x, tb.y)
    end
    
    -- Create text element for display (BLACK text)
    if tb.font then
        local textPadding = 8
        local textY = (tb.height / 2) + (tb.fontSize / 3)
        tb.textId = UI.newText(
            tb.name .. "_text",
            tb.defaultValue,
            tb.font,
            tb.fontSize,
            0.0, 0.0, 0.0, 1.0,
            tb.page
        )
        UI.addChild(tb.boxId, tb.textId, textPadding, textY)
        UI.setZIndex(tb.textId, 1)
    end
    
    -- Make it clickable
    UI.setClickable(tb.boxId, true)
    UI.setOnClick(tb.boxId, tb.callbackName)
    
    textboxes[id] = tb
    engine.logInfo("TextBox created: " .. tb.name .. " (id=" .. id .. ", boxId=" .. tb.boxId .. ")")
    
    return id
end

function textbox.destroy(id)
    local tb = textboxes[id]
    if not tb then return end
    
    -- Clear focus if this was focused
    if tb.boxId and UI.hasFocus(tb.boxId) then
        UI.clearFocus()
    end
    
    textboxes[id] = nil
    engine.logInfo("TextBox destroyed: " .. tb.name)
end

-- Handle click callback - find which textbox was clicked and focus it
function textbox.handleCallback(callbackName)
    for id, tb in pairs(textboxes) do
        if tb.callbackName == callbackName then
            textbox.focus(id)
            return true
        end
    end
    return false
end

-- Focus a textbox by its local id
function textbox.focus(id)
    local tb = textboxes[id]
    if not tb then return end
    
    -- Unfocus any previously focused textbox (update visuals)
    for otherId, otherTb in pairs(textboxes) do
        if otherId ~= id and otherTb.boxId and UI.hasFocus(otherTb.boxId) then
            UI.setBoxTextures(otherTb.boxId, texSetNormal)
        end
    end
    
    -- Set engine focus to this element
    UI.setFocus(tb.boxId)
    
    -- Update visual
    UI.setBoxTextures(tb.boxId, texSetSelected)
    
    engine.logInfo("TextBox focused: " .. tb.name)
end

-- Unfocus a textbox by its local id
function textbox.unfocus(id)
    local tb = textboxes[id]
    if not tb then return end
    
    if UI.hasFocus(tb.boxId) then
        UI.clearFocus()
    end
    
    -- Update visual
    UI.setBoxTextures(tb.boxId, texSetNormal)
    
    engine.logInfo("TextBox unfocused: " .. tb.name)
end

-- Unfocus all textboxes
function textbox.unfocusAll()
    for id, tb in pairs(textboxes) do
        if tb.boxId and UI.hasFocus(tb.boxId) then
            textbox.unfocus(id)
            return
        end
    end
    -- Also clear focus if something else had it
    UI.clearFocus()
end

-- Check if a textbox is focused
function textbox.isFocused(id)
    local tb = textboxes[id]
    if not tb then return false end
    return UI.hasFocus(tb.boxId)
end

-- Get the currently focused textbox id (local id, not element handle)
function textbox.getFocusedId()
    for id, tb in pairs(textboxes) do
        if tb.boxId and UI.hasFocus(tb.boxId) then
            return id
        end
    end
    return nil
end

-- Get the element handle for a textbox
function textbox.getElementHandle(id)
    local tb = textboxes[id]
    if not tb then return nil end
    return tb.boxId
end

-- Check if a callback name belongs to a textbox
function textbox.isTextBoxCallback(callbackName)
    if not callbackName then return false end
    return callbackName == "onTextBoxClick"
end

-- Update the displayed text from the buffer
function textbox.updateDisplay(id)
    local tb = textboxes[id]
    if not tb then return end
    if not tb.textId then return end
    
    local text = UI.getTextInput(tb.boxId) or ""
    UI.setText(tb.textId, text)
end

-----------------------------------------------------------
-- Text Buffer Operations (delegate to engine)
-----------------------------------------------------------

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

function textbox.getCursor(id)
    local tb = textboxes[id]
    if not tb then return 0 end
    return UI.getCursor(tb.boxId) or 0
end

function textbox.setCursor(id, pos)
    local tb = textboxes[id]
    if not tb then return end
    UI.setCursor(tb.boxId, pos)
end

function textbox.insertChar(id, char)
    local tb = textboxes[id]
    if not tb then return end
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

-- Handle character input for the focused textbox
function textbox.onCharInput(char)
    local id = textbox.getFocusedId()
    if id then
        textbox.insertChar(id, char)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

-- Handle backspace for the focused textbox
function textbox.onBackspace()
    local id = textbox.getFocusedId()
    if id then
        textbox.deleteBackward(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

-- Handle delete key for the focused textbox
function textbox.onDelete()
    local id = textbox.getFocusedId()
    if id then
        textbox.deleteForward(id)
        textbox.updateDisplay(id)
        return true
    end
    return false
end

-- Handle cursor keys
function textbox.onCursorLeft()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorLeft(id)
        return true
    end
    return false
end

function textbox.onCursorRight()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorRight(id)
        return true
    end
    return false
end

function textbox.onHome()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorHome(id)
        return true
    end
    return false
end

function textbox.onEnd()
    local id = textbox.getFocusedId()
    if id then
        textbox.cursorEnd(id)
        return true
    end
    return false
end

-- Handle submit (Enter key)
function textbox.onSubmit()
    local id = textbox.getFocusedId()
    if id then
        local tb = textboxes[id]
        local text = textbox.getText(id)
        engine.logInfo("TextBox submitted: " .. tb.name .. " = '" .. text .. "'")
        -- Unfocus after submit
        textbox.unfocus(id)
        return true, text
    end
    return false, nil
end

-- Handle escape (unfocus)
function textbox.onEscape()
    local id = textbox.getFocusedId()
    if id then
        textbox.unfocus(id)
        return true
    end
    return false
end

return textbox
