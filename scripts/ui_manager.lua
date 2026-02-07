-- UI Manager - coordinates all UI pages
local uiManager = {}

local boxTextures = require("scripts.ui.box_textures")
local boxTexSet = nil
local btnTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local uiscale = 1.0

local fontsLoaded = {
    menu = false,
    title = false
}
local fontsReady = false
local menuFontHandle = nil
local titleFontHandle = nil
local initialized = false

local mainMenu = nil
local settingsMenu = nil
local textbox = nil
local checkbox = nil
local button = nil
local dropdown = nil

local hoveredElement = nil
local hoveredCallback = nil

local currentMenu = "main"

local function handleNonTextBoxClick()
    if textbox then
        textbox.unfocusAll()
    end
end

function uiManager.init(scriptId)
    engine.logDebug("UI Manager initializing...")
    
    textbox = require("scripts.ui.textbox")
    checkbox = require("scripts.ui.checkbox")
    button = require("scripts.ui.button")
    dropdown = require("scripts.ui.dropdown")
    scrollbar = require("scripts.ui.scrollbar")

    button.init()
    scrollbar.init()
    uiscale = engine.getUIScale()
    
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    menuFont = menuFontHandle
    titleFont = titleFontHandle
    
    boxTexSet = boxTextures.load("assets/textures/box", "box")
    btnTexSet = boxTextures.load("assets/textures/ui/button", "button")
    
    mainMenu = require("scripts.main_menu")
    settingsMenu = require("scripts.settings_menu")
    
    settingsMenu.setShowMenuCallback(function(menuName)
        uiManager.showMenu(menuName)
    end)
    
    engine.logDebug("UI Manager waiting for assets...")
end

function uiManager.onAssetLoaded(assetType, handle, path)
    if assetType == "font" then
        if handle == menuFontHandle then
            fontsLoaded.menu = true
        elseif handle == titleFontHandle then
            fontsLoaded.title = true
        end
        
        if fontsLoaded.menu and fontsLoaded.title then
            fontsReady = true
            uiManager.checkReady()
        end
    end
end

function uiManager.checkReady()
    if fontsReady and fbW > 0 and fbH > 0 then
        if not initialized then
            mainMenu.init(boxTexSet, btnTexSet, menuFont, titleFont, fbW, fbH)
            settingsMenu.init(boxTexSet, btnTexSet, menuFont, fbW, fbH)
            uiManager.showMenu("main")
            initialized = true
        else
            uiManager.showMenu(currentMenu)
        end
    end
end

function uiManager.onFramebufferResize(width, height)
    fbW = width
    fbH = height
    
    if mainMenu then mainMenu.onFramebufferResize(width, height) end
    if settingsMenu then settingsMenu.onFramebufferResize(width, height) end
    
    uiManager.checkReady()
end

function uiManager.showMenu(menuName)
    currentMenu = menuName
    
    mainMenu.hide()
    settingsMenu.hide()
    
    if menuName == "main" then
        mainMenu.show()
    elseif menuName == "settings" then
        settingsMenu.show()
    end
end

function uiManager.onSettings()
    handleNonTextBoxClick()
    uiManager.showMenu("settings")
end

function uiManager.onQuit()
    engine.quit()
end

function uiManager.onSettingsBack()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onBack() end
    uiManager.showMenu("main")
end

function uiManager.onSettingsSave()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onSave() end
end

function uiManager.onSettingsApply()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onApply() end
end

function uiManager.onCancel()
    handleNonTextBoxClick()
    uiManager.showMenu("main")
end

function uiManager.update(dt)
    if textbox then
        textbox.update(dt)
    end
    if dropdown then
        dropdown.update(dt)
    end
    
    -- Hover detection with coordinate scaling
    local mx, my = engine.getMousePosition()
    if mx and my then
        local ww, wh = engine.getWindowSize()
        if ww and wh and ww > 0 and wh > 0 then
            mx = mx * (fbW / ww)
            my = my * (fbH / wh)
        end
        
        local elem, cb = UI.findHoverTarget(mx, my)
        
        if elem ~= hoveredElement then
            -- Leave old element
            if hoveredElement and hoveredCallback then
                uiManager.onHoverLeave(hoveredElement, hoveredCallback)
            end
            
            -- Enter new element
            hoveredElement = elem
            hoveredCallback = cb
            if elem and cb then
                uiManager.onHoverEnter(elem, cb)
            end
        end
    end
end

function uiManager.onMouseDown(button_num, x, y)
    -- Scale coordinates for dropdown click-outside detection
    local ww, wh = engine.getWindowSize()
    local sx, sy = x, y
    if ww and wh and ww > 0 and wh > 0 then
        sx = x * (fbW / ww)
        sy = y * (fbH / wh)
    end
    if dropdown then
        dropdown.onClickOutside(sx, sy)
    end
end

function uiManager.onHoverEnter(elemHandle, callbackName)
    if button and callbackName == "onButtonClick" then
        button.onHoverEnter(elemHandle)
    elseif checkbox and callbackName == "onCheckboxClick" then
        checkbox.onHoverEnter(elemHandle)
    elseif textbox and textbox.isTextBoxCallback(callbackName) then
        textbox.onHoverEnter(elemHandle)
    elseif dropdown and dropdown.isDropdownCallback(callbackName) then
        dropdown.onHoverEnter(elemHandle)
    end
end

function uiManager.onHoverLeave(elemHandle, callbackName)
    if button and callbackName == "onButtonClick" then
        button.onHoverLeave(elemHandle)
    elseif checkbox and callbackName == "onCheckboxClick" then
        checkbox.onHoverLeave(elemHandle)
    elseif textbox and textbox.isTextBoxCallback(callbackName) then
        textbox.onHoverLeave(elemHandle)
    elseif dropdown and dropdown.isDropdownCallback(callbackName) then
        dropdown.onHoverLeave(elemHandle)
    end
end

function uiManager.shutdown()
    if mainMenu then mainMenu.shutdown() end
    if settingsMenu then settingsMenu.shutdown() end
end

function uiManager.onTextBoxClick(elemHandle)
    if textbox then
        textbox.handleClickByElement(elemHandle)
    end
end

function uiManager.onCheckboxClick(elemHandle)
    handleNonTextBoxClick()
    if checkbox then
        return checkbox.handleClickByElement(elemHandle)
    end
    return false
end

function uiManager.onButtonClick(elemHandle)
    handleNonTextBoxClick()
    if button then
        return button.handleClickByElement(elemHandle)
    end
    return false
end

function uiManager.onDropdownClick(elemHandle)
    handleNonTextBoxClick()
    if dropdown then
        return dropdown.handleCallback("onDropdownClick", elemHandle)
    end
    return false
end

function uiManager.onDropdownOptionClick(elemHandle)
    handleNonTextBoxClick()
    if dropdown then
        return dropdown.handleCallback("onDropdownOptionClick", elemHandle)
    end
    return false
end

function uiManager.onMouseUp(button_num, x, y)
    if button then
        button.onMouseUp()
    end
end

function uiManager.onScrollUp(elemHandle)
    if dropdown then
        return dropdown.handleCallback("onScrollUp", elemHandle)
    end
    return false
end

function uiManager.onScrollDown(elemHandle)
    if dropdown then
        return dropdown.handleCallback("onScrollDown", elemHandle)
    end
    return false
end

-----------------------------------------------------------
-- Input Event Forwarding to Textboxes
-----------------------------------------------------------

function uiManager.onDropdownDisplayClick(elemHandle)
    handleNonTextBoxClick()
    if dropdown then
        return dropdown.handleCallback("onDropdownDisplayClick", elemHandle)
    end
    return false
end

function uiManager.onUICharInput(char)
    -- Dropdown gets priority if focused
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onCharInput(char)
    end
    if textbox then
        return textbox.onCharInput(char)
    end
    return false
end

function uiManager.onUIBackspace()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onBackspace()
    end
    if textbox then
        return textbox.onBackspace()
    end
    return false
end

function uiManager.onUIDelete()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onDelete()
    end
    if textbox then
        return textbox.onDelete()
    end
    return false
end

function uiManager.onUICursorLeft()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onCursorLeft()
    end
    if textbox then
        return textbox.onCursorLeft()
    end
    return false
end

function uiManager.onUICursorRight()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onCursorRight()
    end
    if textbox then
        return textbox.onCursorRight()
    end
    return false
end

function uiManager.onUIHome()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onHome()
    end
    if textbox then
        return textbox.onHome()
    end
    return false
end

function uiManager.onUIEnd()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onEnd()
    end
    if textbox then
        return textbox.onEnd()
    end
    return false
end

function uiManager.onUISubmit()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onSubmit()
    end
    if textbox then
        local handled, value, id, name = textbox.onSubmit()
        if handled and value then
            engine.logDebug("UI Submit received: " .. tostring(value) .. " from " .. tostring(name))
            if settingsMenu then settingsMenu.onTextBoxSubmit(name, value) end
        end
        return handled
    end
    return false
end

function uiManager.onUIEscape()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onEscape()
    end
    if textbox then
        local handled = textbox.onEscape()
        if handled then return true end
    end
    if dropdown then
        for id = 1, 100 do
            if dropdown.isOpen(id) then
                dropdown.closeList(id)
                return true
            end
        end
    end
    return false
end

function uiManager.onUIFocusLost()
    if dropdown then
        dropdown.unfocusAll()
    end
    if textbox then
        textbox.unfocusAll()
    end
end

function uiManager.onUIScroll(elemHandle, dx, dy)
    -- Forward scroll events to dropdown for scrollbar
    if dropdown then
        dropdown.onScroll(elemHandle, dx, dy)
    end
end

return uiManager
