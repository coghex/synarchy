-- RandBox UI component (textbox with randomize button)
-- Like a textbox but with a dice/shuffle icon that generates random values.
-- Supports different random modes via randbox.Type constants.
local scale = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")
local utf8Safe = require("scripts.ui.utf8_safe")
-- #1330: every value this widget rolls comes from the UI kit's own
-- stream. `math.random` belongs to gameplay — AI cadence, thoughts,
-- mental state, wildlife — and a suggested seed is not a simulation
-- decision, so pressing randomize must not move it.
local uiRandom = require("scripts.ui.random")
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
    -- A name. #1106 removed this type's built-in generator: a world
    -- name now comes from the generated-language system, which needs
    -- the world seed and lives outside this widget kit, so a NAME
    -- randbox supplies its own `generate` (see below). The type itself
    -- stays: it still classifies the field's input validation and
    -- length limit.
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
local texHighlight = nil
local assetsLoaded = false

-- Hover overlay colour, matched to the dropdown/list highlight so the
-- whole UI kit gives the same hover affordance.
local HIGHLIGHT_COLOR = {0.3, 0.5, 0.8, 0.8}

local cursorBlinkTime = 0
local cursorBlinkRate = 0.5
local cursorVisible = true

-----------------------------------------------------------
-- Random Generation
-----------------------------------------------------------

-- #1106: exported so a caller that must finalize a seed BEFORE some
-- other control derives from it (Create World's World Name, which is
-- generated in the seed's own language) can obtain one without first
-- building the seed widget.
function randbox.newHexSeed()
    local hex = ""
    local chars = "0123456789ABCDEF"
    for i = 1, 8 do
        local idx = uiRandom.integer(1, #chars)
        hex = hex .. chars:sub(idx, idx)
    end
    return hex
end

local function randomNumber(minVal, maxVal)
    return tostring(uiRandom.integer(minVal, maxVal))
end

-- Produce a fresh value for one randbox, or nil when it cannot.
--
-- An injected `generate` (#1106) takes precedence over every built-in
-- type: it is how a field whose values come from outside this widget
-- kit -- the generated-language world name, which needs the world seed
-- and the concept catalogue -- is filled without teaching randbox
-- about any of that. It may return nil to mean "no value this time",
-- which leaves the field untouched rather than substituting anything.
--
-- randbox.Type.NAME has no built-in generator at all: the fixed
-- word-list generator that used to serve it produced text with no
-- language, no meaning, and no gloss, and drew from Lua's global RNG,
-- all of which #708 principle 9 and #1106 requirement 9 rule out.
local function generateRandom(rb)
    if rb.generate then
        return rb.generate()
    end
    if rb.randType == randbox.Type.HEX_SEED then
        return randbox.newHexSeed()
    elseif rb.randType == randbox.Type.NUMBER then
        local params = rb.randParams
        local minVal = (params and params.min) or 0
        local maxVal = (params and params.max) or 9999
        return randomNumber(minVal, maxVal)
    end
    return nil
end

-----------------------------------------------------------
-- Input Validation
-----------------------------------------------------------

local function isValidChar(rb, char)
    -- An injected validator wins, the same opt-in dropdown.lua already
    -- offers. #1106 uses it for World Name, whose admissible characters
    -- are the generated language's business rather than this kit's.
    if rb.validateChar then
        return rb.validateChar(char) and true or false
    end
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
        -- #1106: a generated native name runs to 32 characters
        -- (Language.Generated.Render's own bound), so the player must
        -- be able to type one that long too -- a 24-character cap would
        -- make a name the game itself suggests unreachable by hand.
        return 32
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
    texRandomNormal = engine.loadTexture("assets/textures/ui/randomize.png", "ui")
    texRandomClicked = engine.loadTexture("assets/textures/ui/randomizeclicked.png", "ui")
    texHighlight = engine.loadTexture("assets/textures/ui/highlight.png", "ui")

    -- #1330: no RNG seeding here. This used to run
    -- `math.randomseed(os.time())`, which put a UI widget in charge of
    -- gameplay's stream and replaced the per-state entropy
    -- `luaopen_math` had already installed with a one-second-granularity
    -- value — so two engines launched in the same second simulated
    -- identically. Gameplay's stream is the engine's
    -- (`Engine.Scripting.Lua.Thread.createLuaBackendState`); this
    -- widget's own draws come from `scripts.ui.random`.

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
        parent = params.parent,
        font = params.font,
        randType = params.randType or randbox.Type.HEX_SEED,
        randParams = params.randParams or nil,
        onChange = params.onChange,
        -- #1106: the two hooks a field whose values come from outside
        -- this widget kit needs.
        --   generate()  -> the next value, or nil to leave the field
        --                  alone (a failure must not invent one).
        --   onUserEdit(value, id, name) -> fired on the mutation itself
        --                  (typing, backspace, delete), NOT on unfocus,
        --                  so a caller tracking whether the text is
        --                  still the generator's can drop that claim the
        --                  instant the player takes over. Deliberately
        --                  separate from onChange, which also fires for
        --                  programmatic sets (restoreAll after a resize
        --                  rebuild) and so cannot tell the two apart.
        generate = params.generate,
        onUserEdit = params.onUserEdit,
        validateChar = params.validateChar,
        textColor = textColor,
        uiscale = uiscale,
        zIndex = params.zIndex or 0,
        -- Element IDs
        boxId = nil,
        textId = nil,
        cursorId = nil,
        btnSpriteId = nil,
        boxHighlightId = nil,
        btnHighlightId = nil,
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

    -- Set initial value: generate one or use the provided default. A
    -- generator that declines (#1106: no catalogue, unbuildable
    -- language) leaves the field empty rather than substituting text.
    --
    -- autoGenerate = false opts out entirely, for a field whose owner
    -- decides when a value is due (#1106: a rebuild must not re-fill a
    -- World Name the player deliberately emptied).
    local initValue = params.default
    if (not initValue or initValue == "") and params.autoGenerate ~= false then
        initValue = generateRandom(rb)
    end
    -- An empty field is a real state, and everything below (the text
    -- element, onChange, the debug line) needs a string for it. Both
    -- ways of reaching it are ordinary: a rebuild of a World Name the
    -- player cleared supplies no default and generates nothing, and a
    -- generator may decline.
    initValue = initValue or ""
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

    -- Hover highlights: non-clickable overlays parented to the box and
    -- the randomize button. As children they resolve their hover target
    -- up to the clickable parent (no flicker). Hidden until hovered. The
    -- box overlay (zIndex 0) sits below the text (zIndex 1) so it tints
    -- the field without obscuring its value.
    rb.boxHighlightId = UI.newSprite(
        rb.name .. "_box_hl",
        rb.inputWidth, rb.height,
        texHighlight,
        HIGHLIGHT_COLOR[1], HIGHLIGHT_COLOR[2],
        HIGHLIGHT_COLOR[3], HIGHLIGHT_COLOR[4],
        rb.page
    )
    UI.addChild(rb.boxId, rb.boxHighlightId, 0, 0)
    UI.setZIndex(rb.boxHighlightId, 0)
    UI.setVisible(rb.boxHighlightId, false)

    rb.btnHighlightId = UI.newSprite(
        rb.name .. "_btn_hl",
        rb.btnSize, rb.btnSize,
        texHighlight,
        HIGHLIGHT_COLOR[1], HIGHLIGHT_COLOR[2],
        HIGHLIGHT_COLOR[3], HIGHLIGHT_COLOR[4],
        rb.page
    )
    UI.addChild(rb.btnSpriteId, rb.btnHighlightId, 0, 0)
    UI.setZIndex(rb.btnHighlightId, 1)
    UI.setVisible(rb.btnHighlightId, false)

    -- Position elements. #747: opt-in parenting (e.g. a clipping
    -- viewport) instead of always attaching directly to the page.
    if rb.parent then
        UI.addChild(rb.parent, rb.boxId, rb.x, rb.y)
        UI.addChild(rb.parent, rb.btnSpriteId, rb.x + rb.inputWidth, rb.y)
    else
        UI.addToPage(rb.page, rb.boxId, rb.x, rb.y)
        UI.addToPage(rb.page, rb.btnSpriteId, rb.x + rb.inputWidth, rb.y)
    end

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
    UI.setCursor(rb.boxId, utf8Safe.codepointLength(text))

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
            if rb.boxId and UI.hasFocus(rb.boxId) then
                return id
            else
                -- Stale mirror: engine-side focus moved on. Repair
                -- instead of routing keys here (unfocus only clears
                -- the engine focus if this box still holds it).
                randbox.unfocus(id)
            end
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
        local textBeforeCursor = utf8Safe.prefix(text, cursorPos)
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

    -- A generator that declines leaves the field EXACTLY as it was
    -- (#1106 requirement 7: a failed suggestion must not fall back to
    -- anything). The button's click feedback is skipped too, so a press
    -- that produced nothing doesn't look like one that succeeded.
    local value = generateRandom(rb)
    if value == nil then
        engine.logDebug("RandBox randomize produced no value: " .. rb.name)
        return
    end

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

-- #1106: report a mutation the PLAYER made, immediately. Called only
-- from the three handlers that change text in response to a keystroke —
-- never from setValue, restoreAll, or randomize, which are the widget
-- acting on someone else's behalf.
--
-- `before` is the buffer as it stood BEFORE the keystroke, and a
-- keystroke that left it untouched is not an edit: backspace at the
-- start and delete at the end are ordinary no-ops, and reporting them
-- would strip a suggested name's gloss and provenance while the name on
-- screen never changed.
local function notifyUserEdit(id, before)
    local rb = randboxes[id]
    if not rb or not rb.onUserEdit then return end
    local after = UI.getTextInput(rb.boxId) or ""
    if after == before then return end
    rb.onUserEdit(after, id, rb.name)
end

function randbox.onCharInput(char)
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    if not isValidChar(rb, char) then
        return true  -- consumed but rejected
    end

    -- Enforce max length
    local text = UI.getTextInput(rb.boxId) or ""
    if utf8Safe.codepointLength(text) >= getMaxLength(rb) then
        return true
    end

    UI.insertChar(rb.boxId, char)
    randbox.updateDisplay(id)
    notifyUserEdit(id, text)
    return true
end

function randbox.onBackspace()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    local before = UI.getTextInput(rb.boxId) or ""
    UI.deleteBackward(rb.boxId)
    randbox.updateDisplay(id)
    notifyUserEdit(id, before)
    return true
end

function randbox.onDelete()
    local id = randbox.getFocusedId()
    if not id then return false end
    local rb = randboxes[id]

    local before = UI.getTextInput(rb.boxId) or ""
    UI.deleteForward(rb.boxId)
    randbox.updateDisplay(id)
    notifyUserEdit(id, before)
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

function randbox.getCursor(id)
    local rb = randboxes[id]
    if not rb then return 0 end
    return UI.getCursor(rb.boxId) or 0
end

function randbox.setCursor(id, pos)
    local rb = randboxes[id]
    if not rb then return end
    UI.setCursor(rb.boxId, pos)
    randbox.updateDisplay(id)
end

-- #748 round 6: mirrors textbox.snapshotPage/restoreAll exactly — a
-- resize rebuild must preserve an in-progress (unsubmitted) edit,
-- cursor, and keyboard focus for World Name/Seed too, not just plain
-- textboxes. setValue's onChange fire is harmless here: it re-sets
-- `pending` to the same value it should already hold.
--
-- #1106: restore goes through setValue, NOT the user-edit path, so a
-- rebuild can never be mistaken for the player retyping the name. The
-- suggestion metadata a restored name carries (gloss, provenance,
-- reroll ordinal) is not snapshotted here at all — it lives on the
-- owning screen's own `pending` table, which the rebuild never
-- destroys.
function randbox.snapshotPage(page)
    local snap = {}
    for id, rb in pairs(randboxes) do
        if rb.page == page then
            snap[rb.name] = {
                text = randbox.getValue(id),
                cursor = randbox.getCursor(id),
                focused = rb.focused,
            }
        end
    end
    return snap
end

function randbox.restoreAll(snap)
    if not snap then return end

    local focusedId = nil
    for id, rb in pairs(randboxes) do
        local saved = snap[rb.name]
        if saved then
            randbox.setValue(id, saved.text)
            if saved.focused then focusedId = id end
        end
    end

    if focusedId then
        randbox.focus(focusedId)
        local savedCursor = snap[randboxes[focusedId].name].cursor
        if savedCursor then
            randbox.setCursor(focusedId, savedCursor)
        end
    end
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

    -- UI.setVisible doesn't cascade to children, so a randbox hovered
    -- when hidden would come back already highlighted on the next show.
    -- Clear both hover overlays explicitly when hiding.
    if not visible then
        if rb.boxHighlightId then UI.setVisible(rb.boxHighlightId, false) end
        if rb.btnHighlightId then UI.setVisible(rb.btnHighlightId, false) end
    end

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
        -- #742 review round 1: same page-scope guard as
        -- scripts/ui/dropdown.lua's onClickOutside — a randbox on a
        -- page the modal boundary has excluded must not react to a
        -- click the boundary already consumed.
        if rb.focused and UI.isPageInScope(rb.page) then
            -- #747: rb.x/rb.y are only correct in the same (absolute)
            -- space mouseX/mouseY arrive in when the randbox is
            -- unparented — query the box's live absolute position
            -- (parent-aware) so this stays correct once rb.parent is
            -- used too.
            local info = rb.boxId and UI.getElementInfo(rb.boxId)
            local absX = info and info.x or rb.x
            local absY = info and info.y or rb.y
            local inBox = mouseX >= absX and mouseX <= absX + rb.inputWidth
                and mouseY >= absY and mouseY <= absY + rb.height
            local inBtn = mouseX >= absX + rb.inputWidth
                and mouseX <= absX + rb.width
                and mouseY >= absY and mouseY <= absY + rb.height
            if not inBox and not inBtn then
                randbox.unfocus(id)
            end
        end
    end
end

-----------------------------------------------------------
-- Hover Handling
-----------------------------------------------------------

-- Resolve the hover-highlight overlay for the box or randomize-button
-- handle. Returns nil if the handle isn't one of ours.
local function highlightForHandle(elemHandle)
    for _, rb in pairs(randboxes) do
        if rb.boxId == elemHandle then
            return rb.boxHighlightId
        elseif rb.btnSpriteId == elemHandle then
            return rb.btnHighlightId
        end
    end
    return nil
end

function randbox.onHoverEnter(elemHandle)
    local hlId = highlightForHandle(elemHandle)
    if hlId then UI.setVisible(hlId, true) end
end

function randbox.onHoverLeave(elemHandle)
    local hlId = highlightForHandle(elemHandle)
    if hlId then UI.setVisible(hlId, false) end
end

-----------------------------------------------------------
-- Introspection (F3, #645)
-----------------------------------------------------------

-- A randbox is a textbox variant (text input + a randomize button), so
-- it's reported as type "textbox" rather than inventing a new taxonomy
-- entry. Bounds cover the full assembly (input + randomize button);
-- rb.width already includes both (see rb.new above).
function randbox.dump()
    local out = {}
    for id, rb in pairs(randboxes) do
        local info = rb.boxId and UI.getElementInfo(rb.boxId) or nil
        local value = rb.boxId and (UI.getTextInput(rb.boxId) or "") or ""
        if info and info.pageVisible and info.visible then
            table.insert(out, {
                id = "randbox:" .. id,
                name = rb.name,
                type = "textbox",
                bounds = {
                    x = info.x,
                    y = info.y,
                    w = rb.width,
                    h = info.height,
                },
                label = value,
                enabled = info.clickable,
                visible = info.visible,
                hovered = info.hovered,
                focused = info.focused,
                value = value,
                screen = info.page,
                handle = info.handle,
            })
        end
    end
    return out
end

return randbox
