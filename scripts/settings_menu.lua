-- Settings Menu Module (simplified with debug logging)
local settingsMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

local currentSettings = {
    fullscreen = false
}

local elements = {}

function settingsMenu.init(boxTex, font, width, height)
    boxTexSet = boxTex
    menuFont = font
    fbW = width
    fbH = height
    
    -- Load current settings
    local w, h, fs, vs, msaa = engine.getVideoConfig()
    currentSettings.fullscreen = fs
    
    engine.logInfo("Settings menu initialized with fullscreen=" .. tostring(fs))
    
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    if uiCreated and page then
        UI.deletePage(page)
        elements = {}
    end
    
    page = UI.newPage("settings_menu", "modal")
    
    -- Much larger panel
    local panelWidth = math.floor(fbW * 0.6)
    local panelHeight = math.floor(fbH * 0.6)
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    engine.logInfo("Creating settings panel: " .. panelWidth .. "x" .. panelHeight .. " at (" .. panelX .. ", " .. panelY .. ")")
    engine.logInfo("Framebuffer: " .. fbW .. "x" .. fbH)
    
    local panel = UI.newBox("settings_panel", panelWidth, panelHeight, boxTexSet, 64, 0.15, 0.15, 0.2, 0.95, page)
    UI.addToPage(page, panel, panelX, panelY)
    UI.setZIndex(panel, 10)
    
    -- Title
    local titleText = UI.newText("settings_title", "Settings", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Settings")
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 80)
    
    -- Fullscreen toggle - directly on page, not panel child
    local yPos = math.floor(fbH / 2) - 40  -- Absolute center of screen
    local toggleWidth = 200
    local toggleHeight = 80
    local toggleX = math.floor((fbW - toggleWidth) / 2)  -- Absolute center horizontally
    
    engine.logInfo("Creating toggle button: " .. toggleWidth .. "x" .. toggleHeight .. " at ABSOLUTE (" .. toggleX .. ", " .. yPos .. ")")
    
    local toggleBtn = UI.newBox("fullscreen_toggle", toggleWidth, toggleHeight, boxTexSet, 32, 
        currentSettings.fullscreen and 0.2 or 0.4, 
        currentSettings.fullscreen and 0.6 or 0.3, 
        currentSettings.fullscreen and 0.3 or 0.3, 
        1.0, page)
    -- Add directly to page, not as child of panel
    UI.addToPage(page, toggleBtn, toggleX, yPos)
    UI.setClickable(toggleBtn, true)
    UI.setZIndex(toggleBtn, 20)
    UI.setOnClick(toggleBtn, "onToggle_fullscreen")
    
    -- Label also directly on page
    local labelText = UI.newText("fullscreen_label", "Fullscreen:", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local labelWidth = engine.getTextWidth(menuFont, "Fullscreen:")
    UI.addToPage(page, labelText, toggleX - labelWidth - 20, yPos + 28)
    
    -- Text centered in button
    local toggleText = currentSettings.fullscreen and "ON" or "OFF"
    local toggleTextWidth = engine.getTextWidth(menuFont, toggleText)
    elements.fullscreenText = UI.newText("fullscreen_text", toggleText, menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(toggleBtn, elements.fullscreenText, (toggleWidth - toggleTextWidth) / 2, (toggleHeight / 2) - 8)
    
    elements.fullscreenToggle = toggleBtn
    
    engine.logInfo("Toggle button clickable=true, zIndex=20, onClick=onToggle_fullscreen")
    
    -- Back button - directly on page
    local btnWidth = 200
    local btnHeight = 80
    local btnX = (fbW - btnWidth) / 2
    local btnY = math.floor(fbH * 0.75)
    
    engine.logInfo("Creating back button: " .. btnWidth .. "x" .. btnHeight .. " at ABSOLUTE (" .. btnX .. ", " .. btnY .. ")")
    
    local backBtn = UI.newBox("back_btn", btnWidth, btnHeight, boxTexSet, 32, 0.4, 0.3, 0.3, 1.0, page)
    UI.addToPage(page, backBtn, btnX, btnY)
    UI.setClickable(backBtn, true)
    UI.setZIndex(backBtn, 20)
    UI.setOnClick(backBtn, "onSettingsBack")
    
    local backLabel = UI.newText("back_label", "Back", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local backLabelWidth = engine.getTextWidth(menuFont, "Back")
    UI.addChild(backBtn, backLabel, (btnWidth - backLabelWidth) / 2, (btnHeight / 2) - 8)
    
    uiCreated = true
    engine.logInfo("Settings menu created (simple debug version)")
end

function settingsMenu.onToggle_fullscreen()
    engine.logInfo("Toggle fullscreen clicked! Current: " .. tostring(currentSettings.fullscreen))
    currentSettings.fullscreen = not currentSettings.fullscreen
    engine.logInfo("New value: " .. tostring(currentSettings.fullscreen))
    
    -- Update the toggle button appearance
    UI.setColor(elements.fullscreenToggle, 
        currentSettings.fullscreen and 0.2 or 0.4,
        currentSettings.fullscreen and 0.6 or 0.3,
        currentSettings.fullscreen and 0.3 or 0.3,
        1.0)
    
    -- Update the text
    local newText = currentSettings.fullscreen and "ON" or "OFF"
    UI.setText(elements.fullscreenText, newText)
    
    engine.logInfo("Toggle updated visually")
end

function settingsMenu.getSettings()
    return currentSettings
end

function settingsMenu.show()
    if page then
        UI.showPage(page)
        engine.logInfo("Settings menu shown")
    end
end

function settingsMenu.hide()
    if page then
        UI.hidePage(page)
        engine.logInfo("Settings menu hidden")
    end
end

function settingsMenu.onFramebufferResize(width, height)
    fbW = width
    fbH = height
    if uiCreated then
        settingsMenu.createUI()
    end
end

function settingsMenu.shutdown()
    if page then
        UI.deletePage(page)
    end
end

return settingsMenu
