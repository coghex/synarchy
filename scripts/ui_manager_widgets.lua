-- UI Manager: generic widget click/hover forwarding (#544 split from
-- ui_manager.lua).
--
-- Owns the click-callback routes for the basic UI widget families
-- (textbox/checkbox/button/tab/dropdown/randbox/toggle/slider/
-- scrollbar-grab/list), raw mouse down/up dispatch, and hover
-- enter/leave routing. M.handleNonTextBoxClick is shared with
-- scripts/ui_manager_menu.lua (menu-level clicks also need to drop
-- textbox/randbox/dropdown focus).
local uiManager = package.loaded["scripts.ui_manager"]
local M = {}

local textbox  = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local button   = require("scripts.ui.button")
local dropdown = require("scripts.ui.dropdown")
local scrollbar = require("scripts.ui.scrollbar")
local tabbar   = require("scripts.ui.tabbar")
local slider   = require("scripts.ui.slider")
local randbox  = require("scripts.ui.randbox")
local toggle   = require("scripts.ui.toggle")
local uiList   = require("scripts.ui.list")
local contextMenu = require("scripts.ui.context_menu")
local hud      = require("scripts.hud")

function M.handleNonTextBoxClick()
    textbox.unfocusAll()
    randbox.unfocusAll()
    -- Dropdowns keep their own focused flag; missing them here left
    -- dd.focused == true after the engine-side focus moved on, and the
    -- input dispatch below routes by dd.focused first — so keystrokes
    -- meant for a later-focused textbox landed in the stale dropdown.
    dropdown.unfocusAll()
end

function uiManager.onMouseDown(button_num, x, y)
    -- Defer to the debug overlay's parallel hit-test first. It runs
    -- independently of the UI manager and is idempotent across all
    -- onMouseDown subscribers, so re-calling here is safe.
    local debugOverlay = require("scripts.debug")
    if debugOverlay.tryClaimClick(button_num, x, y) then
        return
    end

    local ww, wh = engine.getWindowSize()
    local sx, sy = x, y
    if ww and wh and ww > 0 and wh > 0 then
        sx = x * (uiManager.fbW / ww)
        sy = y * (uiManager.fbH / wh)
    end
    dropdown.onClickOutside(sx, sy)
    randbox.onClickOutside(sx, sy)
    hud.onMouseDown(button_num, sx, sy)
end

function uiManager.onMouseUp(button_num, x, y)
    slider.onMouseUp()
    scrollbar.onMouseUp()
    button.onMouseUp()
end

function uiManager.onHoverEnter(elemHandle, callbackName)
    if callbackName == "onButtonClick" then
        button.onHoverEnter(elemHandle)
    elseif callbackName == "onCheckboxClick" then
        checkbox.onHoverEnter(elemHandle)
    elseif textbox.isTextBoxCallback(callbackName) then
        textbox.onHoverEnter(elemHandle)
    elseif dropdown.isDropdownCallback(callbackName) then
        dropdown.onHoverEnter(elemHandle)
    elseif slider.isSliderCallback(callbackName) then
        slider.onHoverEnter(elemHandle)
    elseif toggle.isToggleCallback(callbackName) then
        toggle.onHoverEnter(elemHandle)
    elseif randbox.isRandBoxCallback(callbackName) then
        randbox.onHoverEnter(elemHandle)
    elseif uiList.isListCallback(callbackName) then
        uiList.onHoverEnter(elemHandle)
    elseif contextMenu.isContextMenuCallback(callbackName) then
        contextMenu.onHoverEnter(elemHandle)
    end
end

function uiManager.onHoverLeave(elemHandle, callbackName)
    if callbackName == "onButtonClick" then
        button.onHoverLeave(elemHandle)
    elseif callbackName == "onCheckboxClick" then
        checkbox.onHoverLeave(elemHandle)
    elseif textbox.isTextBoxCallback(callbackName) then
        textbox.onHoverLeave(elemHandle)
    elseif dropdown.isDropdownCallback(callbackName) then
        dropdown.onHoverLeave(elemHandle)
    elseif slider.isSliderCallback(callbackName) then
        slider.onHoverLeave(elemHandle)
    elseif toggle.isToggleCallback(callbackName) then
        toggle.onHoverLeave(elemHandle)
    elseif randbox.isRandBoxCallback(callbackName) then
        randbox.onHoverLeave(elemHandle)
    elseif uiList.isListCallback(callbackName) then
        uiList.onHoverLeave(elemHandle)
    elseif contextMenu.isContextMenuCallback(callbackName) then
        contextMenu.onHoverLeave(elemHandle)
    end
end

function uiManager.onListItemClick(elemHandle)
    M.handleNonTextBoxClick()
    return uiList.handleCallback("onListItemClick", elemHandle)
end

function uiManager.onTextBoxClick(elemHandle)
    -- Clear the other widget families (textbox.focus handles its own
    -- family) so their focused flags can't go stale and steal keys.
    dropdown.unfocusAll()
    randbox.unfocusAll()
    textbox.handleClickByElement(elemHandle)
end

function uiManager.onCheckboxClick(elemHandle)
    M.handleNonTextBoxClick()
    return checkbox.handleClickByElement(elemHandle)
end

function uiManager.onButtonClick(elemHandle)
    M.handleNonTextBoxClick()
    return button.handleClickByElement(elemHandle)
end

function uiManager.onTabClick(elemHandle)
    return tabbar.handleCallback("onTabClick", elemHandle)
end

function uiManager.onDropdownClick(elemHandle)
    M.handleNonTextBoxClick()
    return dropdown.handleCallback("onDropdownClick", elemHandle)
end

function uiManager.onDropdownOptionClick(elemHandle)
    M.handleNonTextBoxClick()
    return dropdown.handleCallback("onDropdownOptionClick", elemHandle)
end

function uiManager.onDropdownDisplayClick(elemHandle)
    -- Clear the OTHER widget families only — the dropdown module
    -- manages focus within its own family, and a blanket
    -- dropdown.unfocusAll() here would reset the typed filter text
    -- when clicking the display box of an already-focused dropdown.
    textbox.unfocusAll()
    randbox.unfocusAll()
    return dropdown.handleCallback("onDropdownDisplayClick", elemHandle)
end

function uiManager.onRandBoxClick(elemHandle)
    M.handleNonTextBoxClick()
    return randbox.handleCallback("onRandBoxClick", elemHandle)
end

function uiManager.onRandomizeClick(elemHandle)
    M.handleNonTextBoxClick()
    return randbox.handleCallback("onRandomizeClick", elemHandle)
end

function uiManager.onToggleClick(elemHandle)
    return toggle.handleClickByElement(elemHandle)
end

function uiManager.onToggleRightClick(elemHandle)
    return toggle.handleRightClickByElement(elemHandle)
end

function uiManager.onToggleOptionClick(elemHandle)
    return toggle.handleClickByElement(elemHandle)
end

-------------------------------------------------------------
--- Slider Clicks
-------------------------------------------------------------

function uiManager.onSliderTrackClick(elemHandle)
    M.handleNonTextBoxClick()
    return slider.handleCallback("onSliderTrackClick", elemHandle)
end

function uiManager.onSliderKnobClick(elemHandle)
    M.handleNonTextBoxClick()
    return slider.handleCallback("onSliderKnobClick", elemHandle)
end

-- Click on a scrollbar tab → start dragging. The per-frame poll in
-- uiManager.update takes it from here. onMouseUp clears the drag state.
function uiManager.onScrollTabGrab(elemHandle)
    M.handleNonTextBoxClick()
    return scrollbar.onTabGrab(elemHandle)
end

return M
