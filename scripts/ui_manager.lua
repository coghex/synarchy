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
local createWorldMenu = nil
local worldView = nil
local worldManager = nil
local textbox = nil
local checkbox = nil
local button = nil
local dropdown = nil
local slider = nil

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
    tabbar = require("scripts.ui.tabbar")
    slider = require("scripts.ui.slider")

    button.init()
    scrollbar.init()
    tabbar.init()
    slider.init()
    uiscale = engine.getUIScale()
    
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    menuFont = menuFontHandle
    titleFont = titleFontHandle
    
    boxTexSet = boxTextures.load("assets/textures/box", "box")
    btnTexSet = boxTextures.load("assets/textures/ui/button", "button")
    
    mainMenu = require("scripts.main_menu")
    settingsMenu = require("scripts.settings_menu")
    createWorldMenu = require("scripts.create_world_menu")
    worldManager = require("scripts.world_manager")
    worldView = require("scripts.world_view")
    
    settingsMenu.setShowMenuCallback(function(menuName)
        uiManager.showMenu(menuName)
    end)

    createWorldMenu.setShowMenuCallback(function(menuName)
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
    if worldView and worldView.onAssetLoaded then
        worldView.onAssetLoaded(assetType, handle, path)
    end
end

function uiManager.checkReady()
    if fontsReady and fbW > 0 and fbH > 0 then
        if not initialized then
            mainMenu.init(boxTexSet, btnTexSet, menuFont, titleFont, fbW, fbH)
            settingsMenu.init(boxTexSet, btnTexSet, menuFont, fbW, fbH)
            createWorldMenu.init(boxTexSet, btnTexSet, menuFont, fbW, fbH)
            worldView.init(fbW, fbH)
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
    
    if not initialized then
        uiManager.checkReady()
        return
    end
    
    if mainMenu then mainMenu.onFramebufferResize(width, height) end
    if settingsMenu then settingsMenu.onFramebufferResize(width, height) end
    if createWorldMenu then createWorldMenu.onFramebufferResize(width, height) end
    if worldView then worldView.onFramebufferResize(width, height) end
    
    if currentMenu == "main" then
        if mainMenu and mainMenu.page then UI.showPage(mainMenu.page) end
    elseif currentMenu == "settings" then
        if settingsMenu and settingsMenu.page then UI.showPage(settingsMenu.page) end
    elseif currentMenu == "create_world" then
        if createWorldMenu and createWorldMenu.page then UI.showPage(createWorldMenu.page) end
    elseif currentMenu == "world_view" then
        if worldView and worldView.page then UI.showPage(worldView.page) end
    end
end

function uiManager.showMenu(menuName)
    currentMenu = menuName
    
    mainMenu.hide()
    settingsMenu.hide()
    createWorldMenu.hide()
    worldView.hide()
    
    if menuName == "main" then
        mainMenu.show()
    elseif menuName == "settings" then
        settingsMenu.show()
    elseif menuName == "create_world" then
        createWorldMenu.show()
    elseif menuName == "world_view" then
        worldView.show()
    end
end

function uiManager.onCreateWorld()
    handleNonTextBoxClick()
    uiManager.showMenu("create_world")
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
    
    if worldManager then
        worldManager.update(dt)
    end
    
    if worldView then
        worldView.update(dt)
    end

    -- slider drag detection
    if slider and slider.getDraggingId() then
        local mx, my = engine.getMousePosition()
        if mx and my then
            local ww, wh = engine.getWindowSize()
            if ww and wh and ww > 0 and wh > 0 then
                mx = mx * (fbW / ww)
                my = my * (fbH / wh)
            end
            slider.onDragMove(mx, my)
        end
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
            if hoveredElement and hoveredCallback then
                uiManager.onHoverLeave(hoveredElement, hoveredCallback)
            end
            
            hoveredElement = elem
            hoveredCallback = cb
            if elem and cb then
                uiManager.onHoverEnter(elem, cb)
            end
        end
    end
end

function uiManager.onMouseDown(button_num, x, y)
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
    elseif slider and slider.isSliderCallback(callbackName) then
        slider.onHoverEnter(elemHandle)
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
    elseif slider and slider.isSliderCallback(callbackName) then
        slider.onHoverLeave(elemHandle)
    end
end

function uiManager.shutdown()
    if mainMenu then mainMenu.shutdown() end
    if settingsMenu then settingsMenu.shutdown() end
    if createWorldMenu then createWorldMenu.shutdown() end
    if worldView then worldView.shutdown() end
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

function uiManager.onTabClick(elemHandle)
    if tabbar then
        return tabbar.handleCallback("onTabClick", elemHandle)
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
    if slider then
        slider.onMouseUp()
    end
    if button then
        button.onMouseUp()
    end
end

function uiManager.onTabFrameScroll(elemHandle)
    return false
end

-----------------------------------------------------------
-- Scroll Button Clicks (scrollbar up/down arrows)
-----------------------------------------------------------

function uiManager.onScrollUp(elemHandle)
    if dropdown then
        local handled = dropdown.handleCallback("onScrollUp", elemHandle)
        if handled then return true end
    end
    if settingsMenu and currentMenu == "settings" then
        if settingsMenu.handleScrollCallback then
            if settingsMenu.handleScrollCallback("onScrollUp", elemHandle) then
                return true
            end
        end
    end
    return false
end

function uiManager.onScrollDown(elemHandle)
    if dropdown then
        local handled = dropdown.handleCallback("onScrollDown", elemHandle)
        if handled then return true end
    end
    if settingsMenu and currentMenu == "settings" then
        if settingsMenu.handleScrollCallback then
            if settingsMenu.handleScrollCallback("onScrollDown", elemHandle) then
                return true
            end
        end
    end
    return false
end

-------------------------------------------------------------
--- Slider Clicks
-------------------------------------------------------------

function uiManager.onSliderTrackClick(elemHandle)
    handleNonTextBoxClick()
    if slider then
        return slider.handleCallback("onSliderTrackClick", elemHandle)
    end
    return false
end

function uiManager.onSliderKnobClick(elemHandle)
    handleNonTextBoxClick()
    if slider then
        return slider.handleCallback("onSliderKnobClick", elemHandle)
    end
    return false
end

-----------------------------------------------------------
-- Mouse Wheel Scroll (UI elements)
-----------------------------------------------------------

function uiManager.onUIScroll(elemHandle, dx, dy)
    if dropdown then
        local handled = dropdown.onScroll(elemHandle, dx, dy)
        if handled then return end
    end
    if settingsMenu and currentMenu == "settings" then
        if settingsMenu.onScroll(elemHandle, dx, dy) then
            return
        end
    end
end

-----------------------------------------------------------
-- Game Scroll (no UI element under cursor, no shift)
-----------------------------------------------------------

function uiManager.onScroll(dx, dy)
    if currentMenu == "world_view" and worldView then
        worldView.onScroll(dx, dy)
    end
end

-----------------------------------------------------------
-- Z-Slice Scroll (shift+scroll)
-----------------------------------------------------------

function uiManager.onZSliceScroll(dx, dy)
    if currentMenu == "world_view" and worldView then
        worldView.onZSliceScroll(dx, dy)
    end
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

return uiManager
