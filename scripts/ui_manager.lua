-- UI Manager - coordinates all UI pages
local uiManager = {}

-- Shared resources
local boxTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0

-- Font loading state
local fontsLoaded = {
    menu = false,
    title = false
}
local fontsReady = false
local menuFontHandle = nil
local titleFontHandle = nil

-- Sub-modules
local mainMenu = nil
local settingsMenu = nil
local createWorldMenu = nil

-- Current active menu
local currentMenu = "main"  -- "main", "settings", "createWorld"

function uiManager.init(scriptId)
    engine.logInfo("UI Manager initializing...")
    
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
    
    -- Load sub-modules (just require, don't init yet)
    mainMenu = require("scripts.main_menu")
    settingsMenu = require("scripts.settings_menu")
    createWorldMenu = require("scripts.create_world_menu")
    
    engine.logInfo("UI Manager waiting for assets...")
end

function uiManager.onAssetLoaded(assetType, handle, path)
    engine.logInfo("UI Manager asset loaded: " .. assetType .. " handle=" .. handle)
    
    if assetType == "font" then
        if handle == menuFontHandle then
            fontsLoaded.menu = true
            engine.logInfo("Menu font ready")
        elseif handle == titleFontHandle then
            fontsLoaded.title = true
            engine.logInfo("Title font ready")
        end
        
        if fontsLoaded.menu and fontsLoaded.title then
            fontsReady = true
            engine.logInfo("All fonts loaded!")
            uiManager.checkReady()
        end
    end
end

function uiManager.checkReady()
    engine.logInfo("checkReady: fontsReady=" .. tostring(fontsReady) .. " fbW=" .. fbW .. " fbH=" .. fbH)
    if fontsReady and fbW > 0 and fbH > 0 then
        -- Initialize all menus
        mainMenu.init(boxTexSet, menuFont, titleFont, fbW, fbH)
        settingsMenu.init(boxTexSet, menuFont, fbW, fbH)
        createWorldMenu.init(boxTexSet, menuFont, fbW, fbH)
        
        -- Show main menu
        uiManager.showMenu("main")
    end
end

function uiManager.onFramebufferResize(width, height)
    engine.logInfo("UI Manager onFramebufferResize: " .. width .. " x " .. height)
    fbW = width
    fbH = height
    
    -- Update all menus if they exist
    if mainMenu then mainMenu.onFramebufferResize(width, height) end
    if settingsMenu then settingsMenu.onFramebufferResize(width, height) end
    if createWorldMenu then createWorldMenu.onFramebufferResize(width, height) end
    
    uiManager.checkReady()
end

function uiManager.showMenu(menuName)
    currentMenu = menuName
    
    -- Hide all menus
    mainMenu.hide()
    settingsMenu.hide()
    createWorldMenu.hide()
    
    -- Show requested menu
    if menuName == "main" then
        mainMenu.show()
    elseif menuName == "settings" then
        settingsMenu.show()
    elseif menuName == "createWorld" then
        createWorldMenu.show()
    end
end

-- Menu callbacks (these are called from the button onClick)
function uiManager.onCreateWorld()
    engine.logInfo("Create World selected")
    uiManager.showMenu("createWorld")
end

function uiManager.onSettings()
    engine.logInfo("Settings selected")
    uiManager.showMenu("settings")
end

function uiManager.onQuit()
    engine.logInfo("Quit selected")
    engine.quit()
end

function uiManager.onSettingsBack()
    engine.logInfo("Settings back")
    uiManager.showMenu("main")
end

function uiManager.onSettingsApply(settings)
    engine.logInfo("Applying settings...")
    engine.setVideoConfig(settings.width, settings.height, settings.fullscreen, settings.vsync, settings.msaa)
    engine.saveVideoConfig()
    engine.logInfo("Settings saved!")
    uiManager.showMenu("main")
end

function uiManager.onCreateWorldCancel()
    engine.logInfo("Create world cancelled")
    uiManager.showMenu("main")
end

function uiManager.onCreateWorldConfirm(worldSettings)
    engine.logInfo("Creating world: " .. worldSettings.name .. " (seed: " .. worldSettings.seed .. ", size: " .. worldSettings.size .. ")")
    -- TODO: Actually create world
    uiManager.showMenu("main")
end

-- Forward all the specific callbacks to the manager
function onCreateWorld()
    uiManager.onCreateWorld()
end

function onSettings()
    uiManager.onSettings()
end

function onQuit()
    uiManager.onQuit()
end

function onSettingsBack()
    uiManager.onSettingsBack()
end

function onSettingsApply()
    if settingsMenu then
        local settings = settingsMenu.getSettings()
        uiManager.onSettingsApply(settings)
    end
end

function onCreateWorldCancel()
    uiManager.onCreateWorldCancel()
end

function onCreateWorldConfirm()
    if createWorldMenu then
        local worldSettings = createWorldMenu.getSettings()
        uiManager.onCreateWorldConfirm(worldSettings)
    end
end

-- Forward resolution/toggle callbacks
function onResolutionPrev()
    if settingsMenu then settingsMenu.onResolutionPrev() end
end

function onResolutionNext()
    if settingsMenu then settingsMenu.onResolutionNext() end
end

function onToggle_fullscreen()
    if settingsMenu then settingsMenu.onToggle_fullscreen() end
end

function onToggle_vsync()
    if settingsMenu then settingsMenu.onToggle_vsync() end
end

function onMSAA_1()
    if settingsMenu then settingsMenu.onMSAA_1() end
end

function onMSAA_2()
    if settingsMenu then settingsMenu.onMSAA_2() end
end

function onMSAA_4()
    if settingsMenu then settingsMenu.onMSAA_4() end
end

function onRandomSeed()
    if createWorldMenu then createWorldMenu.onRandomSeed() end
end

function onSizePrev()
    if createWorldMenu then createWorldMenu.onSizePrev() end
end

function onSizeNext()
    if createWorldMenu then createWorldMenu.onSizeNext() end
end

function onCreate()
    if createWorldMenu then
        local worldSettings = createWorldMenu.getSettings()
        uiManager.onCreateWorldConfirm(worldSettings)
    end
end

function onCancel()
    uiManager.onCreateWorldCancel()
end

function uiManager.update(dt)
end

function uiManager.shutdown()
    if mainMenu then mainMenu.shutdown() end
    if settingsMenu then settingsMenu.shutdown() end
    if createWorldMenu then createWorldMenu.shutdown() end
end

return uiManager
