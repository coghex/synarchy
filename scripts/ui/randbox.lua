-- RandBox UI component (textbox with randomize button)
-- Like a textbox but with a dice/shuffle icon that generates random values.
-- Supports different random modes via randbox.Type constants.
local scale = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")
local randbox = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local RANDBOX_CALLBACK = "onRandBoxClick"
local RANDOMIZE_CALLBACK = "onRandomizeClick"

-----------------------------------------------------------
-- Random Types
-----------------------------------------------------------
randbox.Type = {
    -- 8-digit hex seed (e.g. "A3F7C901")
    HEX_SEED = "hex_seed",
    -- Random name from a word list
    NAME = "name",
    -- Random integer in a range
    NUMBER = "number",
}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local randboxes = {}
local nextId = 1

local texSetNormal = nil
local texSetSelected = nil
local texRandomNormal = nil
local texRandomClicked = nil
local assetsLoaded = false

local cursorBlinkTime = 0
local cursorBlinkRate = 0.5
local cursorVisible = true

-----------------------------------------------------------
-- Name Generation Data
-----------------------------------------------------------

-- Syllable-based name generator for fantasy world names
local prefixes = {
    "Ar", "El", "Mor", "Val", "Kor", "Thal", "Zan", "Dor",
    "Fen", "Gal", "Har", "Ith", "Kel", "Lor", "Myr", "Nar",
    "Oth", "Pel", "Rav", "Sil", "Tor", "Vor", "Wyn", "Xan",
    "Bal", "Cyr", "Dra", "Eld", "Fyr", "Grim", "Hel", "Ire",
    "Jas", "Kael", "Lum", "Mal", "Nyx", "Orm", "Pyr", "Ryn",
    "Sol", "Tal", "Urd", "Vex", "Wyr", "Yth", "Zor", "Ash",
    "Bel", "Cra", "Dur", "Eol", "Far", "Gor", "Hav", "Ion",
}

local middles = {
    "an", "en", "in", "on", "un", "ar", "er", "ir", "or", "ur",
    "al", "el", "il", "ol", "ul", "ath", "eth", "ith", "oth",
    "and", "end", "ind", "ond", "aer", "ier", "oar", "ael",
    "am", "em", "im", "om", "ak", "ek", "ik", "ok",
}

local suffixes = {
    "ia", "or", "us", "is", "on", "an", "en", "ar",
    "ium", "ion", "oth", "ath", "iel", "ael", "orn",
    "und", "eld", "alt", "ard", "heim", "land", "gar",
    "dal", "mir", "rin", "wen", "dor", "thos", "ros",
    "las", "nar", "var", "mar", "ias", "eos", "yss",
}

-----------------------------------------------------------
-- Random Generation
-----------------------------------------------------------

local function randomHexSeed()
    local hex = ""
    local chars = "0123456789ABCDEF"
    for i = 1, 8 do
        local idx = math.random(1, #chars)
        hex = hex .. chars:sub(idx, idx)
    end
    return hex
end

local function randomName()
    local hasMiddle = math.random() > 0.4
    local name
    if hasMiddle then
        name = prefixes[math.random(#prefixes)]
             .. middles[math.random(#middles)]
             .. suffixes[math.random(#suffixes)]
    else
        name = prefixes[math.random(#prefixes)]
             .. suffixes[math.random(#suffixes)]
    end
    -- Capitalize first letter (already done by prefix), ensure rest is lower
    return name:sub(1,1):upper() .. name:sub(2):lower()
end

local function randomNumber(minVal, maxVal)
    return tostring(math.random(minVal, maxVal))
end

local function generateRandom(randType, params)
    if randType == randbox.Type.HEX_SEED then
        return randomHexSeed()
    elseif randType == randbox.Type.NAME then
        return randomName()
    elseif randType == randbox.Type.NUMBER then
        local minVal = (params and params.min) or 0
        local maxVal = (params and params.max) or 9999
        return randomNumber(minVal, maxVal)
    end
    return ""
end

-----------------------------------------------------------
-- Input Validation
-----------------------------------------------------------

local function isValidChar(rb, char)
    if rb.randType == randbox.Type.HEX_SEED then
        return char:match("^[0-9a-fA-F]$") ~= nil
    elseif rb.randType == randbox.Type.NAME then
        return char:match("^[a-zA-Z]$") ~= nil
    elseif rb.randType == randbox.Type.NUMBER then
        return char:match("^%d$") ~= nil
    end
    return true
end

local function getMaxLength(rb)
    if rb.randType == randbox.Type.HEX_SEED then
        return 8
    elseif rb.randType == randbox.Type.NAME then
        return 24
    elseif rb.randType == randbox.Type.NUMBER then
        return 10
    end
    return 64
end

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function randbox.init()
    if assetsLoaded then return end

    texSetNormal = boxTextures.load("assets/textures/ui/textbox", "textbox")
    texSetSelected = boxTextures.load("assets/textures/ui/textboxselected", "textbox")
    texRandomNormal = engine.loadTexture("assets/textures/ui/randomize.png")
    texRandomClicked = engine.loadTexture("assets/textures/ui/randomizeclicked.png")

    -- Seed the RNG
    math.randomseed(os.time())

    assetsLoaded = true
    engine.logDebug("RandBox module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function randbox.new(params)
    local id = nextId
    nextId = nextId + 1

    local uiscale = params.uiscale or scale.get()
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local height = math.floor((params.height or 40) * uiscale)
    local btnSize = height
    local tileSize = math.floor((params.tileSize or 16) * uiscale)
    local textPadding = math.floor(8 * uiscale)

    local textColor = params.textColor or {0.0, 0.0, 0.0, 1.0}

    local inputWidth = math.floor((params.width or 200) * uiscale)
    local totalWidth = inputWidth + btnSize

    local rb = {
        id = id,
        name = params.name or ("randbox_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = totalWidth,
        height = height,
        inputWidth = inputWidth,
        btnSize = btnSize,
        tileSize = tileSize,
        fontSize = fontSize,
        textPadding = textPadding,
        page = params.page,
        font = params.font,
        randType = params.randType or randbox.Type.HEX_SEED,
        randParams = params.randParams or nil,
        onChange = params.onChange,
        textColor = textColor,
        uiscale = uiscale,
        zIndex = params.zIndex or 0,
        -- Element IDs
        boxId = nil,
        textId = nil,
        cursorId = nil,
        btnSpriteId = nil,
        -- State
        focused = false,
    }

    -- Create the text input box
    rb.boxId = UI.newBox(
        rb.name .. "_box",
        rb.inputWidth,
        rb.height,
        texSetNormal,
        rb.tileSize,
        1.0, 1.0, 1.0, 1.0,
        0,
        rb.page
    )

    UI.enableTextInput(rb.boxId)

    -- Set initial value: generate random or use provided default
    local initValue = params.default
    if not initValue or initValue == "" then
        initValue = generateRandom(rb.randType, rb.randParams)
    end
    UI.setTextInput(rb.boxId, initValue)

    -- Display text
    local textY = (rb.height / 2) + (rb.fontSize / 3)
    rb.textId = UI.newText(
        rb.name .. "_text",
        initValue,
        rb.font,
        rb.fontSize,
        textColor[1], textColor[2], textColor[3], textColor[4],
        rb.page
    )
    UI.addChild(rb.boxId, rb.textId, rb.textPadding, textY)
    UI.setZIndex(rb.textId, 1)

    -- Cursor (hidden by default)
    rb.cursorId = UI.newText(
        rb.name .. "_cursor",
        "|",
        rb.font,
        rb.fontSize,
        textColor[1], textColor[2], textColor[3], textColor[4],
        rb.page
    )
    UI.addChild(rb.boxId, rb.cursorId, rb.textPadding, textY)
    UI.setZIndex(rb.cursorId, 2)
    UI.setVisible(rb.cursorId, false)

    -- Randomize button (same pattern as dropdown arrow)
    rb.btnSpriteId = UI.newSprite(
        rb.name .. "_btn",
        rb.btnSize,
        rb.btnSize,
        texRandomNormal,
        1.0, 1.0, 1.0, 1.0,
        rb.page
    )
    UI.setClickable(rb.btnSpriteId, true)
    UI.setOnClick(rb.btnSpriteId, RANDOMIZE_CALLBACK)

    -- Text box click focuses for editing
    UI.setClickable(rb.boxId, true)
    UI.setOnClick(rb.boxId, RANDBOX_CALLBACK)

    -- Position elements
    UI.addToPage(rb.page, rb.boxId, rb.x, rb.y)
    UI.addToPage(rb.page, rb.btnSpriteId, rb.x + rb.inputWidth, rb.y)

    if rb.zIndex > 0 then
        UI.setZIndex(rb.boxId, rb.zIndex)
        UI.setZIndex(rb.btnSpriteId, rb.zIndex)
    end

    -- Fire onChange with the initial random value
    if rb.onChange then
        rb.onChange(initValue, id, rb.name)
    end

    randboxes[id] = rb

    engine.logDebug("RandBox created: " .. rb.name
        .. " type=" .. rb.randType
        .. " initial=" .. initValue)

    return id
end

function randbox.destroy(id)
    local rb = randboxes[id]
    if not rb then return end

    if rb.focused then
        randbox.unfocus(id)
    end

    if rb.cursorId then
        UI.deleteElement(rb.cursorId)
    end
    if rb.textId then
        UI.deleteElement(rb.textId)
    end
    if rb.btnSpriteId then
        UI.deleteElement(rb.btnSpriteId)
    end
    if rb.boxId then
        UI.deleteElement(rb.boxId)
    end

    randboxes[id] = nil
    engine.logDebug("RandBox destroyed: " .. rb.name)
end

function randbox.destroyAll()
    for id, rb in pairs(randboxes) do
        if rb.focused then
            randbox.unfocus(id)
        end
    end
    randboxes = {}
    nextId = 1
    cursorVisible = true
    cursorBlinkTime = 0
end

-----------------------------------------------------------
-- Focus Management
-----------------------------------------------------------

function randbox.focus(id)
    local rb = randboxes[id]
    if not rb then return end
    if rb.focused then return end

    -- Unfocus any other focused randbox
    for otherId, otherRb in pairs(randboxes) do
        if otherId ~= id and otherRb.focused then
            randbox.unfocus(otherId)
        end
    end

    rb.focused = true
    UI.setFocus(rb.boxId)
    UI.setBoxTextures(rb.boxId, texSetSelected)

    local text = UI.getTextInput(rb.boxId) or ""
    UI.setCursor(rb.boxId, #text)

    if rb.cursorId then
        cursorVisible = true
        cursorBlinkTime = 0
        UI.setVisible(rb.cursorId, true)
    end

    randbox.updateDisplay(id)
    engine.logDebug("RandBox focused: " .. rb.name)
end

function randbox.unfocus(id)
    local rb = randboxes[id]
    if not rb then return end
    if not rb.focused then return end

    rb.focused = false

    if UI.hasFocus(rb.boxId) then
        UI.clearFocus()
    end

    UI.setBoxTextures(rb.boxId, texSetNormal)

    if rb.cursorId then
        UI.setVisible(rb.cursorId, false)
    end

    -- Normalize display (uppercase hex, etc.)
    local text = UI.getTextInput(rb.boxId) or ""
    if rb.randType == randbox.Type.HEX_SEED then
        text = text:upper()
        UI.setTextInput(rb.boxId, text)
    end

    randbox.updateDisplay(id)

    -- Fire onChange on unfocus with final value
    if rb.onChange then
        rb.onChange(text, id, rb.name)
    end

    engine.logDebug("RandBox unfocused: " .. rb.name)
end

function randbox.unfocusAll()
    for id, rb in pairs(randboxes) do
        if rb.focused then
            randbox.unfocus(id)
        end
    end
end

function randbox.isFocused(id)
    local rb = randboxes[id]
    if not rb then return false end
    return rb.focused
end

function randbox.getFocusedId()
    for id, rb in pairs(randboxes) do
        if rb.focused then
            return id
        end
    end
    return nil
end

-----------------------------------------------------------
-- Display Update
-----------------------------------------------------------

function randbox.updateDisplay(id)
    local rb = randboxes[id]
    if not rb then return end
    if not rb.textId then return end

    local text = UI.getTextInput(rb.boxId) or ""
    local cursorPos = UI.getCursor(rb.boxId) or 0

    local textWidth = engine.getTextWidth(rb.font, text, rb.fontSize)
    local textX = rb.textPadding
    local textY = (rb.height / 2) + (rb.fontSize / 3)

    UI.setText(rb.textId, text)
    UI.setPosition(rb.textId, textX, textY)

    if rb.cursorId and rb.focused then
        local textBeforeCursor = text:sub(1, cursorPos)
        local cursorTextWidth = engine.getTextWidth(rb.font, textBeforeCursor, rb.fontSize)
        local cursorX = textX + cursorTextWidth
                      - (engine.getTextWidth(rb.font, "|", rb.fontSize) / 2)
        UI.setPosition(rb.cursorId, cursorX, textY)
    end
end

-----------------------------------------------------------
-- Randomize
-----------------------------------------------------------

function randbox.randomize(id)
    local rb = randboxes[id]
    if not rb then return end

    local value = generateRandom(rb.randType, rb.randParams)
    UI.setTextInput(rb.boxId, value)
    randbox.updateDisplay(id)

    -- Brief visual feedback on the button
    UI.setSpriteTexture(rb.btnSpriteId, texRandomClicked)

    if rb.onChange then
        rb.onChange(value, id, rb.name)
    end

    engine.logDebug("RandBox randomized: " .. rb.name .. " = " .. value)

    -- Reset button texture after a short delay (next frame via update)
    rb._resetBtn = true
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function randbox.findByElementHandle(elemHandle)
    for id, rb in pairs(randboxes) do
        if rb.boxId == elemHandle then
            return id, "box"
        end
        if rb.btnSpriteId == elemHandle then
            return id, "btn"
        end
    end
    return nil
end

function randbox.handleCallback(callbackName, elemHandle)
    if callbackName == RANDBOX_CALLBACK then
        local id, action = randbox.findByElementHandle(elemHandle)
        if id then
            randbox.focus(id)
            return true
        end
    elseif callbackName == RANDOMIZE_CALLBACK then
        local id, action = randbox.findByElementHandle(elemHandle)
        if id then
            randbox.randomize(id)
            return true
        end
    end
    return false
end

function randbox.isRandBoxCallback(callbackName)
    return callbackName == RANDBOX_CALLBACK
        or callbackName == RANDOMIZE_CALLBACK
end

-----------------------------------------------------------
-- Input Event Handlers (forwarded from uiManager)
-----------------------------------------------------------

function randbox.onCharInput(char)
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    if not isValidChar(rb, char) then
        return true  -- consumed but rejected
    end

    -- Enforce max length
    local text = UI.getTextInput(rb.boxId) or ""
    if #text >= getMaxLength(rb) then
        return true
    end

    UI.insertChar(rb.boxId, char)
    randbox.updateDisplay(id)
    return true
end

function randbox.onBackspace()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    UI.deleteBackward(rb.boxId)
    randbox.updateDisplay(id)
    return true
end

function randbox.onDelete()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    UI.deleteForward(rb.boxId)
    randbox.updateDisplay(id)
    return true
end

function randbox.onCursorLeft()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    UI.cursorLeft(rb.boxId)
    randbox.updateDisplay(id)
    return true
end

function randbox.onCursorRight()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    UI.cursorRight(rb.boxId)
    randbox.updateDisplay(id)
    return true
end

function randbox.onHome()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    UI.cursorHome(rb.boxId)
    randbox.updateDisplay(id)
    return true
end

function randbox.onEnd()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    UI.cursorEnd(rb.boxId)
    randbox.updateDisplay(id)
    return true
end

function randbox.onSubmit()
    local id = randbox.getFocusedId()
    if not id then return false end

    randbox.unfocus(id)
    return true
end

function randbox.onEscape()
    local id = randbox.getFocusedId()
    if not id then return false end

    randbox.unfocus(id)
    return true
end

-----------------------------------------------------------
-- Update (cursor blink + button reset)
-----------------------------------------------------------

function randbox.update(dt)
    -- Cursor blink
    local id = randbox.getFocusedId()
    if id then
        local rb = randboxes[id]
        cursorBlinkTime = cursorBlinkTime + dt
        if cursorBlinkTime >= cursorBlinkRate then
            cursorBlinkTime = cursorBlinkTime - cursorBlinkRate
            cursorVisible = not cursorVisible
            if rb and rb.cursorId then
                UI.setVisible(rb.cursorId, cursorVisible)
            end
        end
    else
        cursorBlinkTime = 0
        cursorVisible = true
    end

    -- Reset randomize button texture after click feedback
    for _, rb in pairs(randboxes) do
        if rb._resetBtn then
            UI.setSpriteTexture(rb.btnSpriteId, texRandomNormal)
            rb._resetBtn = nil
        end
    end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function randbox.getValue(id)
    local rb = randboxes[id]
    if not rb then return "" end
    return UI.getTextInput(rb.boxId) or ""
end

function randbox.setValue(id, value)
    local rb = randboxes[id]
    if not rb then return end
    UI.setTextInput(rb.boxId, value)
    randbox.updateDisplay(id)
    if rb.onChange then
        rb.onChange(value, id, rb.name)
    end
end

function randbox.getSize(id)
    local rb = randboxes[id]
    if not rb then return 0, 0 end
    return rb.width, rb.height
end

function randbox.getElementHandle(id)
    local rb = randboxes[id]
    if not rb then return nil end
    return rb.boxId
end

function randbox.getBtnHandle(id)
    local rb = randboxes[id]
    if not rb then return nil end
    return rb.btnSpriteId
end

function randbox.setVisible(id, visible)
    local rb = randboxes[id]
    if not rb then return end

    if rb.boxId then UI.setVisible(rb.boxId, visible) end
    if rb.textId then UI.setVisible(rb.textId, visible) end
    if rb.cursorId and not visible then UI.setVisible(rb.cursorId, false) end
    if rb.btnSpriteId then UI.setVisible(rb.btnSpriteId, visible) end

    if not visible and rb.focused then
        randbox.unfocus(id)
    end
end

function randbox.setPosition(id, x, y)
    local rb = randboxes[id]
    if not rb then return end

    rb.x = x
    rb.y = y

    UI.setPosition(rb.boxId, x, y)
    UI.setPosition(rb.btnSpriteId, x + rb.inputWidth, y)
end

-----------------------------------------------------------
-- Click-Outside Handling
-----------------------------------------------------------

function randbox.onClickOutside(mouseX, mouseY)
    for id, rb in pairs(randboxes) do
        if rb.focused then
            local inBox = mouseX >= rb.x and mouseX <= rb.x + rb.inputWidth
                and mouseY >= rb.y and mouseY <= rb.y + rb.height
            local inBtn = mouseX >= rb.x + rb.inputWidth
                and mouseX <= rb.x + rb.width
                and mouseY >= rb.y and mouseY <= rb.y + rb.height
            if not inBox and not inBtn then
                randbox.unfocus(id)
            end
        end
    end
end

-----------------------------------------------------------
-- Hover Handling (placeholder for consistency)
-----------------------------------------------------------

function randbox.onHoverEnter(elemHandle)
end

function randbox.onHoverLeave(elemHandle)
end

return randbox
