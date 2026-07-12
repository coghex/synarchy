-- Test fixture for tools/input_check.py (#644) — NOT part of the game.
--
-- Loaded on demand (engine.loadScript) by the input-injection check
-- against a running graphical instance. Creates a small page of UI
-- elements at KNOWN framebuffer coordinates (UI space == framebuffer
-- pixels) and records every input broadcast it receives, so the tool
-- can inject input.* events and assert they arrived through the real
-- pipeline: click hit-testing, modifier state, key up/down routing,
-- char/text-input focus routing, scroll routing, and mouse up/down
-- route pairing.
--
-- Singleton via package.loaded so the console `require` and the
-- loadScript module instance share state.

local M = package.loaded["scripts.input_check_fixture"] or {}
package.loaded["scripts.input_check_fixture"] = M

-- Fixture geometry (framebuffer pixels). Kept far left so it can't
-- overlap the centered main-menu buttons at any sane resolution.
local BTN_X, BTN_Y, BTN_W, BTN_H = 10, 250, 200, 80
local TXT_X, TXT_Y, TXT_W, TXT_H = 10, 360, 200, 40

function M.resetState()
    M.state = {
        clicks = 0,          -- onInputCheckClick fires
        shiftAtClick = nil,  -- engine.isKeyDown("Shift") sampled in the click
        keysDown = {},       -- onKeyDown names, in order
        keysUp = {},         -- onKeyUp names, in order
        chars = "",          -- onUICharInput accumulation
        submits = 0,         -- onUISubmit fires
        uiScrolls = 0,       -- onUIScroll fires (any element)
        gameScrolls = 0,     -- onScroll fires
        mouseDowns = {},     -- {button, x, y} from onMouseDown
        mouseUps = {},       -- {button, x, y, route} from onMouseUp
    }
end
M.resetState()

-- Build the page + elements. Returns the click target's center and
-- the text element's center (framebuffer pixels) for the tool.
function M.setup()
    if not M.page then
        M.page = UI.newPage("input_check_fixture", "modal")
        -- #742: this fixture probes raw input ROUTING (incl. the miss
        -- paths — gameScrolls/mouseDowns below only fire on a miss),
        -- not modal-dialog semantics, so it must stay pass-through — a
        -- LayerModal page defaults input-exclusive, which would swallow
        -- the very misses tools/input_check.py asserts on.
        UI.setPageInputExclusive(M.page, false)
        M.btn = UI.newElement("input_check_btn", BTN_W, BTN_H, M.page)
        UI.addToPage(M.page, M.btn, BTN_X, BTN_Y)
        -- setOnClick alone doesn't make the element hit-testable; an
        -- element left unclickable routes clicks through to the game.
        UI.setClickable(M.btn, true)
        UI.setOnClick(M.btn, "onInputCheckClick")
        -- #743: scroll-capture is independent of click policy — tools/
        -- input_check.py's scroll-over-the-clickable-element assertion
        -- needs this explicit opt-in now that wheel routing no longer
        -- rides along with a registered click callback.
        UI.setScrollCapture(M.btn, true)
        M.txt = UI.newElement("input_check_txt", TXT_W, TXT_H, M.page)
        UI.addToPage(M.page, M.txt, TXT_X, TXT_Y)
        UI.enableTextInput(M.txt)
        UI.showPage(M.page)
    end
    M.resetState()
    return {
        btnX = BTN_X + BTN_W / 2, btnY = BTN_Y + BTN_H / 2,
        txtX = TXT_X + TXT_W / 2, txtY = TXT_Y + TXT_H / 2,
        btnHandle = M.btn,
    }
end

function M.getState()
    return M.state
end

-- Focus / unfocus the text element (text entry needs UI focus, same
-- as clicking into a real textbox).
function M.focusText()
    UI.setFocus(M.txt)
    return UI.hasFocus(M.txt)
end

function M.unfocusText()
    UI.clearFocus()
    return true
end

function M.getText()
    return UI.getTextInput(M.txt) or ""
end

function M.teardown()
    if M.page then
        UI.hidePage(M.page)
        UI.deletePage(M.page)
        M.page, M.btn, M.txt = nil, nil, nil
    end
    M.resetState()
    return true
end

-- Broadcast handlers — the recording surface. These fire through the
-- exact same dispatch real input uses.

function M.onInputCheckClick(elemHandle)
    M.state.clicks = M.state.clicks + 1
    M.state.shiftAtClick = engine.isKeyDown("Shift")
end

function M.onKeyDown(key)
    table.insert(M.state.keysDown, key)
end

function M.onKeyUp(key)
    table.insert(M.state.keysUp, key)
end

-- Acting as our own text widget (the same thing scripts/ui/textbox.lua
-- does for real textboxes): chars and edit keys arrive as broadcasts
-- while our element holds UI focus, and we apply them to the element's
-- engine-side text buffer.
function M.onUICharInput(char)
    if M.txt and UI.hasFocus(M.txt) then
        M.state.chars = M.state.chars .. char
        UI.insertChar(M.txt, char)
    end
end

function M.onUIBackspace()
    if M.txt and UI.hasFocus(M.txt) then
        UI.deleteBackward(M.txt)
    end
end

function M.onUISubmit()
    if M.txt and UI.hasFocus(M.txt) then
        M.state.submits = M.state.submits + 1
    end
end

function M.onUIScroll(elemHandle, dx, dy)
    M.state.uiScrolls = M.state.uiScrolls + 1
end

function M.onScroll(dx, dy)
    M.state.gameScrolls = M.state.gameScrolls + 1
end

function M.onMouseDown(button, x, y)
    table.insert(M.state.mouseDowns, { button = button, x = x, y = y })
end

function M.onMouseUp(button, x, y, downRoute)
    table.insert(M.state.mouseUps,
        { button = button, x = x, y = y, route = downRoute })
end

return M
