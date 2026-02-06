-- Button UI component
local button = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local BUTTON_CALLBACK = "onButtonClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local buttons = {}
local nextId = 1

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function button.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or 1.0
    local width = math.floor((params.width or 160) * uiscale)
    local height = math.floor((params.height or 64) * uiscale)
    local fontSize = math.floor((params.fontSize or 32) * uiscale)
    local tileSize = params.tileSize or fontSize
    
    local bgColor = params.bgColor or {1.0, 1.0, 1.0, 1.0}
    local textColor = params.textColor or {1.0, 1.0, 1.0, 1.0}
    
    local btn = {
        id = id,
        name = params.name or ("button_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = width,
        height = height,
        fontSize = fontSize,
        tileSize = tileSize,
        page = params.page,
        parent = params.parent,
        boxId = nil,
        labelId = nil,
        text = params.text or "",
        font = params.font,
        onClick = params.onClick,
        callbackName = params.callbackName or BUTTON_CALLBACK,
        bgColor = bgColor,
        textColor = textColor,
    }
    
    if not params.textureSet then
        engine.logError("Button '" .. btn.name .. "' created without textureSet!")
    end
    
    btn.boxId = UI.newBox(
        btn.name .. "_box",
        btn.width,
        btn.height,
        params.textureSet,
        btn.tileSize,
        bgColor[1], bgColor[2], bgColor[3], bgColor[4],
        btn.page
    )
    
    if btn.parent then
        UI.addChild(btn.parent, btn.boxId, btn.x, btn.y)
    elseif btn.page then
        UI.addToPage(btn.page, btn.boxId, btn.x, btn.y)
    end
    
    if btn.font and btn.text ~= "" then
        btn.labelId = UI.newText(
            btn.name .. "_label",
            btn.text,
            btn.font,
            btn.fontSize,
            textColor[1], textColor[2], textColor[3], textColor[4],
            btn.page
        )
        
        local labelWidth = engine.getTextWidth(btn.font, btn.text, btn.fontSize)
        local labelX = (btn.width - labelWidth) / 2
        local labelY = (btn.height / 2) + (btn.fontSize / 2)
        
        UI.addChild(btn.boxId, btn.labelId, labelX, labelY)
    end
    
    UI.setClickable(btn.boxId, true)
    UI.setZIndex(btn.boxId, params.zIndex or 20)
    UI.setOnClick(btn.boxId, btn.callbackName)
    
    buttons[id] = btn
    
    return id
end

function button.destroy(id)
    local btn = buttons[id]
    if not btn then return end
    buttons[id] = nil
end

function button.destroyAll()
    buttons = {}
    nextId = 1
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function button.findByElementHandle(elemHandle)
    for id, btn in pairs(buttons) do
        if btn.boxId == elemHandle then
            return id
        end
    end
    return nil
end

function button.handleClickByElement(elemHandle)
    local id = button.findByElementHandle(elemHandle)
    if id then
        local btn = buttons[id]
        if btn.onClick then
            btn.onClick(id, btn.name)
        end
        return true
    end
    return false
end

function button.handleCallback(callbackName, elemHandle)
    if callbackName == BUTTON_CALLBACK and elemHandle then
        return button.handleClickByElement(elemHandle)
    end
    return false
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function button.getElementHandle(id)
    local btn = buttons[id]
    if not btn then return nil end
    return btn.boxId
end

return button
