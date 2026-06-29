-- Button UI component
local boxTextures = require("scripts.ui.box_textures")
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

local texSetNormal = nil
local texSetHovered = nil
local texSetClicked = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function button.init()
    if assetsLoaded then return end
    
    texSetHovered = boxTextures.load("assets/textures/ui/buttonhovered", "buttonhovered")
    texSetClicked = boxTextures.load("assets/textures/ui/buttonclicked", "buttonclicked")
    
    assetsLoaded = true
    engine.logDebug("Button module initialized")
end

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
        normalTexSet = params.textureSet,
        hoveredTexSet = texSetHovered,
        clickedTexSet = texSetClicked,
        state = "normal",  -- "normal", "hovered", "clicked"
    }
    
    if not params.textureSet then
        engine.logError("Button '" .. btn.name .. "' created without textureSet!")
    end
    local overflow = params.overflow or 0
    
    btn.boxId = UI.newBox(
        btn.name .. "_box",
        btn.width,
        btn.height,
        params.textureSet,
        btn.tileSize,
        bgColor[1], bgColor[2], bgColor[3], bgColor[4],
        overflow,
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
        UI.setZIndex(btn.labelId, 1)
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
    
    if btn.labelId then
        UI.deleteElement(btn.labelId)
    end
    if btn.boxId then
        UI.deleteElement(btn.boxId)
    end
    
    buttons[id] = nil
end

function button.destroyAll()
    buttons = {}
    nextId = 1
end

-----------------------------------------------------------
-- State Management
-----------------------------------------------------------

function button.setState(id, newState)
    local btn = buttons[id]
    if not btn then return end
    if btn.state == newState then return end
    
    btn.state = newState
    
    local texSet = btn.normalTexSet
    if newState == "hovered" and btn.hoveredTexSet then
        texSet = btn.hoveredTexSet
    elseif newState == "clicked" and btn.clickedTexSet then
        texSet = btn.clickedTexSet
    end
    
    if texSet then
        UI.setBoxTextures(btn.boxId, texSet)
    end
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
        -- Show clicked texture
        button.setState(id, "clicked")
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
-- Hover Handling
-----------------------------------------------------------

function button.onHoverEnter(elemHandle)
    local id = button.findByElementHandle(elemHandle)
    if id then
        local btn = buttons[id]
        if btn and btn.state ~= "clicked" then
            button.setState(id, "hovered")
        end
    end
end

function button.onHoverLeave(elemHandle)
    local id = button.findByElementHandle(elemHandle)
    if id then
        button.setState(id, "normal")
    end
end

-----------------------------------------------------------
-- Mouse Up Handling
-----------------------------------------------------------

function button.onMouseUp()
    for id, btn in pairs(buttons) do
        if btn.state == "clicked" then
            -- If mouse is still over the button, go to hovered; otherwise normal
            -- For simplicity, go to normal — the next hover poll will fix it
            button.setState(id, "normal")
        end
    end
end

-----------------------------------------------------------
-- In-place mutation (used by the keybind editor for live key
-- updates and the settings scroll/tab-hide model)
-----------------------------------------------------------

-- Replace the button's label text and re-center it. Creates the label
-- lazily if the button was made with empty text.
function button.setText(id, text)
    local btn = buttons[id]
    if not btn then return end
    btn.text = text or ""

    local labelWidth = 0
    if btn.font and btn.text ~= "" then
        labelWidth = engine.getTextWidth(btn.font, btn.text, btn.fontSize)
    end
    local labelX = (btn.width - labelWidth) / 2
    local labelY = (btn.height / 2) + (btn.fontSize / 2)

    if btn.labelId then
        UI.setText(btn.labelId, btn.text)
        UI.setPosition(btn.labelId, labelX, labelY)
    elseif btn.font and btn.text ~= "" then
        btn.labelId = UI.newText(
            btn.name .. "_label",
            btn.text,
            btn.font,
            btn.fontSize,
            btn.textColor[1], btn.textColor[2], btn.textColor[3], btn.textColor[4],
            btn.page
        )
        UI.addChild(btn.boxId, btn.labelId, labelX, labelY)
        UI.setZIndex(btn.labelId, 1)
    end
end

-- Toggle the whole button (box + child label). An invisible box prunes
-- its children in the render walk, so toggling the box suffices.
function button.setVisible(id, visible)
    local btn = buttons[id]
    if not btn then return end
    if btn.boxId then UI.setVisible(btn.boxId, visible) end
end

-- Move the button. The label is a child of the box, so it follows.
function button.setPosition(id, x, y)
    local btn = buttons[id]
    if not btn then return end
    btn.x = x
    btn.y = y
    if btn.boxId then UI.setPosition(btn.boxId, x, y) end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function button.getElementHandle(id)
    local btn = buttons[id]
    if not btn then return nil end
    return btn.boxId
end

function button.getSize(id)
    local btn = buttons[id]
    if not btn then return 0, 0 end
    return btn.width, btn.height
end

function button.isButtonCallback(callbackName)
    return callbackName == BUTTON_CALLBACK
end

return button
