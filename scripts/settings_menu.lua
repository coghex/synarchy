-- Settings Menu Module (pure module)
local settingsMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

local currentSettings = {
    width = 1920,
    height = 1080,
    fullscreen = false,
    vsync = true,
    msaa = 1
}

local resolutionOptions = {
    {width = 1280, height = 720, label = "1280x720"},
    {width = 1920, height = 1080, label = "1920x1080"},
    {width = 2560, height = 1440, label = "2560x1440"},
    {width = 3840, height = 2160, label = "3840x2160"}
}
local currentResolutionIndex = 2

local elements = {}

function settingsMenu.init(boxTex, font, width, height)
    boxTexSet = boxTex
    menuFont = font
    fbW = width
    fbH = height
    
    -- Load current settings
    local w, h, fs, vs, msaa = engine.getVideoConfig()
    currentSettings.width = w
    currentSettings.height = h
    currentSettings.fullscreen = fs
    currentSettings.vsync = vs
    currentSettings.msaa = msaa
    
    -- Find matching resolution
    for i, res in ipairs(resolutionOptions) do
        if res.width == w and res.height == h then
            currentResolutionIndex = i
            break
        end
    end
    
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    if uiCreated and page then
        UI.deletePage(page)
        elements = {}
    end
    
    page = UI.newPage("settings_menu", "modal")
    
    local panelWidth = 600
    local panelHeight = 500
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    local panel = UI.newBox("settings_panel", panelWidth, panelHeight, boxTexSet, 64, 0.15, 0.15, 0.2, 0.95, page)
    UI.addToPage(page, panel, panelX, panelY)
    
    local titleText = UI.newText("settings_title", "Settings", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Settings")
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 30)
    
    local yOffset = 100
    local lineHeight = 60
    
    settingsMenu.createResolutionControl(panel, panelWidth, yOffset)
    yOffset = yOffset + lineHeight
    
    settingsMenu.createToggle(panel, "Fullscreen", panelWidth, yOffset, "fullscreen", currentSettings.fullscreen)
    yOffset = yOffset + lineHeight
    
    settingsMenu.createToggle(panel, "VSync", panelWidth, yOffset, "vsync", currentSettings.vsync)
    yOffset = yOffset + lineHeight
    
    settingsMenu.createMSAAControl(panel, panelWidth, yOffset)
    yOffset = yOffset + lineHeight + 40
    
    local btnWidth = 150
    local btnHeight = 50
    local btnSpacing = 20
    local totalBtnWidth = btnWidth * 2 + btnSpacing
    local btnStartX = (panelWidth - totalBtnWidth) / 2
    
    local applyBtn = UI.newBox("apply_btn", btnWidth, btnHeight, boxTexSet, 32, 0.2, 0.6, 0.3, 1.0, page)
    UI.addChild(panel, applyBtn, btnStartX, yOffset)
    UI.setClickable(applyBtn, true)
    UI.setOnClick(applyBtn, "onSettingsApply")
    local applyLabel = UI.newText("apply_label", "Apply", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local applyLabelWidth = engine.getTextWidth(menuFont, "Apply")
    UI.addChild(applyBtn, applyLabel, (btnWidth - applyLabelWidth) / 2, 18)
    
    local backBtn = UI.newBox("back_btn", btnWidth, btnHeight, boxTexSet, 32, 0.4, 0.3, 0.3, 1.0, page)
    UI.addChild(panel, backBtn, btnStartX + btnWidth + btnSpacing, yOffset)
    UI.setClickable(backBtn, true)
    UI.setOnClick(backBtn, "onSettingsBack")
    local backLabel = UI.newText("back_label", "Back", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local backLabelWidth = engine.getTextWidth(menuFont, "Back")
    UI.addChild(backBtn, backLabel, (btnWidth - backLabelWidth) / 2, 18)
    
    uiCreated = true
    engine.logInfo("Settings menu created")
end

function settingsMenu.createResolutionControl(parent, parentWidth, yPos)
    local labelText = UI.newText("res_label", "Resolution:", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, labelText, 40, yPos)
    
    local arrowBtn = UI.newBox("res_left", 40, 40, boxTexSet, 32, 0.3, 0.3, 0.4, 1.0, page)
    UI.addChild(parent, arrowBtn, 200, yPos)
    UI.setClickable(arrowBtn, true)
    UI.setOnClick(arrowBtn, "onResolutionPrev")
    local leftArrow = UI.newText("res_left_text", "<", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(arrowBtn, leftArrow, 12, 8)
    
    local resText = resolutionOptions[currentResolutionIndex].label
    elements.resolutionText = UI.newText("res_display", resText, menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, elements.resolutionText, 260, yPos + 8)
    
    local arrowBtnR = UI.newBox("res_right", 40, 40, boxTexSet, 32, 0.3, 0.3, 0.4, 1.0, page)
    UI.addChild(parent, arrowBtnR, 450, yPos)
    UI.setClickable(arrowBtnR, true)
    UI.setOnClick(arrowBtnR, "onResolutionNext")
    local rightArrow = UI.newText("res_right_text", ">", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(arrowBtnR, rightArrow, 12, 8)
end

function settingsMenu.createToggle(parent, label, parentWidth, yPos, key, value)
    local labelText = UI.newText(key .. "_label", label .. ":", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, labelText, 40, yPos)
    
    local toggleBtn = UI.newBox(key .. "_toggle", 80, 40, boxTexSet, 32, 
        value and 0.2 or 0.4, 
        value and 0.6 or 0.3, 
        value and 0.3 or 0.3, 
        1.0, page)
    UI.addChild(parent, toggleBtn, 300, yPos)
    UI.setClickable(toggleBtn, true)
    UI.setOnClick(toggleBtn, "onToggle_" .. key)
    
    local toggleText = UI.newText(key .. "_text", value and "ON" or "OFF", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(toggleBtn, toggleText, 20, 8)
    
    elements[key .. "_toggle"] = toggleBtn
    elements[key .. "_text"] = toggleText
end

function settingsMenu.createMSAAControl(parent, parentWidth, yPos)
    local labelText = UI.newText("msaa_label", "MSAA:", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, labelText, 40, yPos)
    
    local msaaValues = {1, 2, 4}
    local xStart = 200
    local btnWidth = 60
    local btnSpacing = 10
    
    for i, val in ipairs(msaaValues) do
        local isActive = currentSettings.msaa == val
        local btn = UI.newBox("msaa_" .. val, btnWidth, 40, boxTexSet, 32,
            isActive and 0.2 or 0.35,
            isActive and 0.6 or 0.35,
            isActive and 0.3 or 0.35,
            1.0, page)
        UI.addChild(parent, btn, xStart + (i-1) * (btnWidth + btnSpacing), yPos)
        UI.setClickable(btn, true)
        UI.setOnClick(btn, "onMSAA_" .. val)
        
        local text = UI.newText("msaa_text_" .. val, tostring(val) .. "x", menuFont, 1.0, 1.0, 1.0, 1.0, page)
        UI.addChild(btn, text, 15, 8)
        
        elements["msaa_" .. val] = btn
    end
end

-- Internal update functions (called by callbacks)
function settingsMenu.onResolutionPrev()
    currentResolutionIndex = currentResolutionIndex - 1
    if currentResolutionIndex < 1 then currentResolutionIndex = #resolutionOptions end
    
    local res = resolutionOptions[currentResolutionIndex]
    currentSettings.width = res.width
    currentSettings.height = res.height
    
    UI.setText(elements.resolutionText, res.label)
end

function settingsMenu.onResolutionNext()
    currentResolutionIndex = currentResolutionIndex + 1
    if currentResolutionIndex > #resolutionOptions then currentResolutionIndex = 1 end
    
    local res = resolutionOptions[currentResolutionIndex]
    currentSettings.width = res.width
    currentSettings.height = res.height
    
    UI.setText(elements.resolutionText, res.label)
end

function settingsMenu.onToggle_fullscreen()
    currentSettings.fullscreen = not currentSettings.fullscreen
    settingsMenu.updateToggle("fullscreen", currentSettings.fullscreen)
end

function settingsMenu.onToggle_vsync()
    currentSettings.vsync = not currentSettings.vsync
    settingsMenu.updateToggle("vsync", currentSettings.vsync)
end

function settingsMenu.updateToggle(key, value)
    UI.setColor(elements[key .. "_toggle"], 
        value and 0.2 or 0.4,
        value and 0.6 or 0.3,
        value and 0.3 or 0.3,
        1.0)
    UI.setText(elements[key .. "_text"], value and "ON" or "OFF")
end

function settingsMenu.onMSAA_1()
    settingsMenu.setMSAA(1)
end

function settingsMenu.onMSAA_2()
    settingsMenu.setMSAA(2)
end

function settingsMenu.onMSAA_4()
    settingsMenu.setMSAA(4)
end

function settingsMenu.setMSAA(value)
    currentSettings.msaa = value
    
    for _, val in ipairs({1, 2, 4}) do
        local isActive = val == value
        UI.setColor(elements["msaa_" .. val],
            isActive and 0.2 or 0.35,
            isActive and 0.6 or 0.35,
            isActive and 0.3 or 0.35,
            1.0)
    end
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
