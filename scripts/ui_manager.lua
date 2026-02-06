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

local currentMenu = "main"

local function handleNonTextBoxClick()
    if textbox then
        textbox.unfocusAll()
    end
end

function uiManager.init(scriptId)
    engine.logInfo("UI Manager initializing...")
    
    textbox = require("scripts.ui.textbox")
    checkbox = require("scripts.ui.checkbox")
    button = require("scripts.ui.button")

    uiscale = engine.getUIScale()
    
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    menuFont = menuFontHandle
    titleFont = titleFontHandle
    
    boxTexSet = boxTextures.load("assets/textures/box", "box")
    btnTexSet = boxTextures.load("assets/textures/button", "button")
    
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

-----------------------------------------------------------
-- Input Event Forwarding to Textboxes
-----------------------------------------------------------

function uiManager.onUICharInput(char)
    if textbox then
        return textbox.onCharInput(char)
    end
    return false
end

function uiManager.onUIBackspace()
    if textbox then
        return textbox.onBackspace()
    end
    return false
end

function uiManager.onUIDelete()
    if textbox then
        return textbox.onDelete()
    end
    return false
end

function uiManager.onUICursorLeft()
    if textbox then
        return textbox.onCursorLeft()
    end
    return false
end

function uiManager.onUICursorRight()
    if textbox then
        return textbox.onCursorRight()
    end
    return false
end

function uiManager.onUIHome()
    if textbox then
        return textbox.onHome()
    end
    return false
end

function uiManager.onUIEnd()
    if textbox then
        return textbox.onEnd()
    end
    return false
end

function uiManager.onUISubmit()
    if textbox then
        local handled, value, id, name = textbox.onSubmit()
        if handled and value then
            engine.logInfo("UI Submit received: " .. tostring(value) .. " from " .. tostring(name))
            if settingsMenu then settingsMenu.onTextBoxSubmit(name, value) end
        end
        return handled
    end
    return false
end

function uiManager.onUIEscape()
    if textbox then
        return textbox.onEscape()
    end
    return false
end

function uiManager.onUIFocusLost()
    if textbox then
        textbox.unfocusAll()
    end
end

return uiManager
