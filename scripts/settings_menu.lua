-- Settings Menu Module
local settingsMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

-- Checkbox textures
local texCheckboxChecked = nil
local texCheckboxUnchecked = nil

local currentSettings = {
    width = 800,
    height = 600,
    fullscreen = false,
    vsync = true,
    msaa = 0
}

local elements = {}

function settingsMenu.init(boxTex, font, width, height)
    boxTexSet = boxTex
    menuFont = font
    fbW = width
    fbH = height
    
    -- Load checkbox textures
    texCheckboxChecked = engine.loadTexture("assets/textures/ui/checkboxchecked.png")
    texCheckboxUnchecked = engine.loadTexture("assets/textures/ui/checkboxunchecked.png")
    
    -- Load current settings
    local w, h, fs, vs, msaa = engine.getVideoConfig()
    currentSettings.width = w
    currentSettings.height = h
    currentSettings.fullscreen = fs
    currentSettings.vsync = vs
    currentSettings.msaa = msaa
    
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    if uiCreated and page then
        UI.deletePage(page)
        elements = {}
    end
    
    page = UI.newPage("settings_menu", "modal")
    
    -- Panel
    local panelWidth = math.floor(fbW * 0.6)
    local panelHeight = math.floor(fbH * 0.6)
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    local panel = UI.newBox("settings_panel", panelWidth, panelHeight, boxTexSet, 64, 0.15, 0.15, 0.2, 0.95, page)
    UI.addToPage(page, panel, panelX, panelY)
    UI.setZIndex(panel, 10)
    
    -- Title
    local titleText = UI.newText("settings_title", "Settings", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Settings")
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 80)
    
    -- Fullscreen toggle row
    local yPos = 200
    local checkboxSize = 48
    local rowX = 200
    
    -- Label
    local labelText = UI.newText("fullscreen_label", "Fullscreen", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, labelText, rowX, yPos + 8)
    
    -- Checkbox sprite
    local checkboxX = panelWidth - rowX - checkboxSize
    local currentTex = currentSettings.fullscreen and texCheckboxChecked or texCheckboxUnchecked
    elements.fullscreenCheckbox = UI.newSprite("fullscreen_checkbox", checkboxSize, checkboxSize, currentTex, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, elements.fullscreenCheckbox, checkboxX, yPos)
    UI.setClickable(elements.fullscreenCheckbox, true)
    UI.setZIndex(elements.fullscreenCheckbox, 20)
    UI.setOnClick(elements.fullscreenCheckbox, "onToggle_fullscreen")

    -- UI scaling row
    local yPos = 360
    local scalingLabel = UI.newText("scaling_label", "UI Scaling", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, scalingLabel, rowX, yPos + 8)
    
    -- Back button
    local btnWidth = 200
    local btnHeight = 60
    local btnX = (panelWidth - btnWidth) / 2
    local btnY = panelHeight - 120
    
    local backBtn = UI.newBox("back_btn", btnWidth, btnHeight, boxTexSet, 32, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, backBtn, btnX-128, btnY)
    UI.setClickable(backBtn, true)
    UI.setZIndex(backBtn, 20)
    UI.setOnClick(backBtn, "onSettingsBack")
    
    local backLabel = UI.newText("back_label", "Back", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local backLabelWidth = engine.getTextWidth(menuFont, "Back")
    UI.addChild(backBtn, backLabel, (btnWidth - backLabelWidth) / 2, (btnHeight / 2) + 8)
    -- Save button
    local saveBtn = UI.newBox("save_btn", btnWidth, btnHeight, boxTexSet, 32, 1.0, 1.0, 1.0, 1.0, page)
    local backlabelWidth = engine.getTextWidth(menuFont, "Save")
    UI.addChild(panel, saveBtn, btnX+128, btnY)
    UI.setClickable(saveBtn, true)
    UI.setZIndex(saveBtn, 20)
    UI.setOnClick(saveBtn, "onSettingsSave")
    local saveLabel = UI.newText("save_label", "Save", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(saveBtn, saveLabel, (btnWidth - backlabelWidth) / 2, (btnHeight / 2) + 8)
    
    uiCreated = true
end

function settingsMenu.onToggle_fullscreen()
    currentSettings.fullscreen = not currentSettings.fullscreen
    
    -- Swap the checkbox texture
    local newTex = currentSettings.fullscreen and texCheckboxChecked or texCheckboxUnchecked
    UI.setSpriteTexture(elements.fullscreenCheckbox, newTex)
    engine.setFullscreen(currentSettings.fullscreen)
end

function settingsMenu.getSettings()
    return currentSettings
end

function settingsMenu.onSave()
    -- Save settings to config file or apply them as needed
    engine.setVideoConfig(
        currentSettings.width,
        currentSettings.height,
        currentSettings.fullscreen,
        currentSettings.vsync,
        currentSettings.msaa
    )
    engine.saveVideoConfig()
end

function settingsMenu.show()
    if page then
        UI.showPage(page)
    end
end

function settingsMenu.hide()
    if page then
        UI.hidePage(page)
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
