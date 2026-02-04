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

-- Sub-modules
local mainMenu = nil
local settingsMenu = nil
local createWorldMenu = nil

-- Current active menu
local currentMenu = "main"

function uiManager.init(scriptId)
    engine.logInfo("UI Manager initializing...")

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
    createWorldMenu = require("scripts.create_world_menu")
    
    engine.logInfo("UI Manager waiting for assets...")
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
            createWorldMenu.init(btnTexSet, menuFont, fbW, fbH)
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
    if createWorldMenu then createWorldMenu.onFramebufferResize(width, height) end
    
    uiManager.checkReady()
end

function uiManager.showMenu(menuName)
    currentMenu = menuName
    
    mainMenu.hide()
    settingsMenu.hide()
    createWorldMenu.hide()
    
    if menuName == "main" then
        mainMenu.show()
    elseif menuName == "settings" then
        settingsMenu.show()
    elseif menuName == "createWorld" then
        createWorldMenu.show()
    end
end

-- Main menu button callbacks
function uiManager.onCreateWorld()
    uiManager.showMenu("createWorld")
end

function uiManager.onSettings()
    uiManager.showMenu("settings")
end

function uiManager.onQuit()
    engine.quit()
end

-- Settings menu callbacks
function uiManager.onSettingsBack()
    uiManager.showMenu("main")
end

function uiManager.onSettingsSave()
    if settingsMenu then settingsMenu.onSave() end
end

function uiManager.onToggle_fullscreen()
    if settingsMenu then settingsMenu.onToggle_fullscreen() end
end

-- Create world menu callbacks
function uiManager.onRandomSeed()
    if createWorldMenu then createWorldMenu.onRandomSeed() end
end

function uiManager.onSizePrev()
    if createWorldMenu then createWorldMenu.onSizePrev() end
end

function uiManager.onSizeNext()
    if createWorldMenu then createWorldMenu.onSizeNext() end
end

function uiManager.onCreate()
    if createWorldMenu then
        local worldSettings = createWorldMenu.getSettings()
        engine.logInfo("Creating world: " .. worldSettings.name .. " (seed: " .. worldSettings.seed .. ", size: " .. worldSettings.size .. ")")
        -- TODO: Actually create world
        uiManager.showMenu("main")
    end
end

function uiManager.onCancel()
    uiManager.showMenu("main")
end

function uiManager.update(dt)
end

function uiManager.shutdown()
    if mainMenu then mainMenu.shutdown() end
    if settingsMenu then settingsMenu.shutdown() end
    if createWorldMenu then createWorldMenu.shutdown() end
end

return uiManager
