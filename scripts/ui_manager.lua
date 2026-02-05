-- UI Manager - coordinates all UI pages
local uiManager = {}

-- Shared resources
local boxTexSet = nil
local btnTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local uiscale = 1.0

-- Font loading state
local fontsLoaded = {
    menu = false,
    title = false
}
local fontsReady = false
local menuFontHandle = nil
local titleFontHandle = nil
local initialized = false

-- Sub-modules (declared here so all functions can access them)
local mainMenu = nil
local settingsMenu = nil
local textbox = nil  -- MOVED: declare at module level

-- Current active menu
local currentMenu = "main"

-- Helper to handle unfocusing (MOVED: define before it's used)
local function handleNonTextBoxClick()
    if textbox then
        textbox.unfocusAll()
    end
end

function uiManager.init(scriptId)
    engine.logInfo("UI Manager initializing...")
    
    -- CHANGED: assign to module-level variable, not local
    textbox = require("scripts.ui.textbox")

    uiscale = engine.getUIScale()
    
    -- Load fonts
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    menuFont = menuFontHandle
    titleFont = titleFontHandle
    
    -- Load box textures
    local texCenter = engine.loadTexture("assets/textures/box/box.png")
    local texN = engine.loadTexture("assets/textures/box/boxn.png")
    local texS = engine.loadTexture("assets/textures/box/boxs.png")
    local texE = engine.loadTexture("assets/textures/box/boxe.png")
    local texW = engine.loadTexture("assets/textures/box/boxw.png")
    local texNE = engine.loadTexture("assets/textures/box/boxne.png")
    local texNW = engine.loadTexture("assets/textures/box/boxnw.png")
    local texSE = engine.loadTexture("assets/textures/box/boxse.png")
    local texSW = engine.loadTexture("assets/textures/box/boxsw.png")
    boxTexSet = UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW)
    
    -- Load button textures
    local btnTexCenter = engine.loadTexture("assets/textures/button/button.png")
    local btnTexN = engine.loadTexture("assets/textures/button/buttonn.png")
    local btnTexS = engine.loadTexture("assets/textures/button/buttons.png")
    local btnTexE = engine.loadTexture("assets/textures/button/buttone.png")
    local btnTexW = engine.loadTexture("assets/textures/button/buttonw.png")
    local btnTexNE = engine.loadTexture("assets/textures/button/buttonne.png")
    local btnTexNW = engine.loadTexture("assets/textures/button/buttonnw.png")
    local btnTexSE = engine.loadTexture("assets/textures/button/buttonse.png")
    local btnTexSW = engine.loadTexture("assets/textures/button/buttonsw.png")
    btnTexSet = UI.loadBoxTextures(btnTexCenter, btnTexN, btnTexS, btnTexE, btnTexW, btnTexNE, btnTexNW, btnTexSE, btnTexSW)
    
    -- Load sub-modules
    mainMenu = require("scripts.main_menu")
    settingsMenu = require("scripts.settings_menu")
    
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
            mainMenu.init(boxTexSet, menuFont, titleFont, fbW, fbH)
            settingsMenu.init(btnTexSet, menuFont, fbW, fbH)
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

-- Settings menu callbacks
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

function uiManager.onToggle_fullscreen()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onToggle_fullscreen() end
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

-----------------------------------------------------------
-- Input Event Forwarding to Textboxes
-----------------------------------------------------------

-- Character input for UI elements
function uiManager.onUICharInput(char)
    if textbox then
        return textbox.onCharInput(char)
    end
    return false
end

-- Backspace
function uiManager.onUIBackspace()
    if textbox then
        return textbox.onBackspace()
    end
    return false
end

-- Delete
function uiManager.onUIDelete()
    if textbox then
        return textbox.onDelete()
    end
    return false
end

-- Cursor keys
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

-- Submit (Enter)
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

-- Escape - unfocus
function uiManager.onUIEscape()
    if textbox then
        return textbox.onEscape()
    end
    return false
end

-- Focus lost (clicked outside)
function uiManager.onUIFocusLost()
    if textbox then
        textbox.unfocusAll()
    end
end

return uiManager
