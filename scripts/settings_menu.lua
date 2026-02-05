-- Settings Menu Module
local textbox = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local settingsMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local fbW, fbH = 0, 0
local baseFontSize = 32
local baseCheckboxSize = 48
local baseButtonSize = 64
local baseBtnWidth = 160
local baseBtnHeight = 64
local baseSplit = 100

local fontSize = 32
local checkboxSize = 48
local buttonSize = 64
local btnWidth = 160
local btnHeight = 64
local split = 100

local uiCreated = false

local uiScaleTextBox = nil
local frameLimitTextBox = nil
local fullscreenCheckboxId = nil  -- Store checkbox ID

local uiScaleMin = 0.5
local uiScaleMax = 4.0
local frameLimitMin = 30
local frameLimitMax = 240

local currentSettings = {
    width = 800,
    height = 600,
    fullscreen = false,
    uiScale = 1.0,
    vsync = true,
    frameLimit = 60,
    msaa = 0
}

-- Pending changes (not yet applied)
local pendingSettings = {}

local elements = {}

function settingsMenu.init(boxTex, font, width, height)
    boxTexSet = boxTex
    menuFont = font
    fbW = width
    fbH = height
    
    currentSettings.uiScale = engine.getUIScale()
    local uiscale = currentSettings.uiScale
    
    fontSize = math.floor(baseFontSize * uiscale)
    checkboxSize = math.floor(baseCheckboxSize * uiscale)
    buttonSize = math.floor(baseButtonSize * uiscale)
    btnWidth = math.floor(baseBtnWidth * uiscale)
    btnHeight = math.floor(baseBtnHeight * uiscale)
    split = math.floor(baseSplit * uiscale)
    
    textbox.init()
    checkbox.init()
    
    settingsMenu.reloadSettings()
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    textbox.destroyAll()
    checkbox.destroyAll()
    uiScaleTextBox = nil
    frameLimitTextBox = nil
    fullscreenCheckboxId = nil
    
    if uiCreated and page then
        UI.deletePage(page)
        elements = {}
    end
    
    -- Reset pending to current
    pendingSettings = {
        fullscreen = currentSettings.fullscreen,
        uiScale = currentSettings.uiScale,
        frameLimit = currentSettings.frameLimit,
    }
    
    local uiscale = currentSettings.uiScale
    
    page = UI.newPage("settings_menu", "modal")
    
    local panelWidth = math.floor(fbW * 0.6)
    local panelHeight = math.floor(fbH * 0.6)
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    local panel = UI.newBox("settings_panel", panelWidth, panelHeight, boxTexSet, 64, 0.15, 0.15, 0.2, 0.95, page)
    UI.addToPage(page, panel, panelX, panelY)
    UI.setZIndex(panel, 10)
    
    local titleText = UI.newText("settings_title", "Settings", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Settings", fontSize)
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 80)
    
    -- Fullscreen toggle row
    local yPos = 200
    local rowX = 200
    
    local labelText = UI.newText("fullscreen_label", "Fullscreen", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, labelText, rowX, yPos + (fontSize / 2))
    
    local checkboxX = panelWidth - rowX - checkboxSize
    fullscreenCheckboxId = checkbox.new({
        name = "fullscreen",
        size = baseCheckboxSize,
        uiscale = uiscale,
        page = page,
        parent = panel,
        x = checkboxX,
        y = yPos - (checkboxSize / 2),
        default = currentSettings.fullscreen,
        onChange = function(checked, id, name)
            -- Just update pending, don't apply yet
            pendingSettings.fullscreen = checked
            engine.logInfo("Fullscreen pending: " .. tostring(checked))
        end,
    })
    UI.setZIndex(checkbox.getElementHandle(fullscreenCheckboxId), 20)

    -- UI scaling row
    yPos = 360
    local scalingLabel = UI.newText("scaling_label", "UI Scaling", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, scalingLabel, rowX, yPos + 8)
    
    local textboxWidth = math.floor(150 * uiscale)
    local textboxHeight = math.floor(40 * uiscale)
    local textboxX = panelWidth - rowX - textboxWidth
    uiScaleTextBox = textbox.new({
        name = "uiscale_input",
        x = textboxX,
        y = yPos - (textboxHeight / 2) + 8,
        width = textboxWidth,
        height = textboxHeight,
        page = page,
        parent = panel,
        uiscale = uiscale,
        font = menuFont,
        fontSize = 24,
        default = tostring(currentSettings.uiScale),
        textType = textbox.Type.SCALE
    })

    -- Frame limit row
    yPos = yPos + 160
    local frameLimitLabel = UI.newText("framelimit_label", "Frame Limit", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, frameLimitLabel, rowX, yPos + 8)
    
    frameLimitTextBox = textbox.new({
        name = "framelimit_input",
        x = textboxX,
        y = yPos - (textboxHeight / 2) + 8,
        width = textboxWidth,
        height = textboxHeight,
        page = page,
        parent = panel,
        uiscale = uiscale,
        font = menuFont,
        fontSize = 24,
        default = tostring(currentSettings.frameLimit or 60),
        textType = textbox.Type.NUMBER,
    })
    
    -- Button row
    local btnSpacing = math.floor(20 * uiscale)
    local totalBtnWidth = (btnWidth * 3) + (btnSpacing * 2)
    local btnStartX = (panelWidth - totalBtnWidth) / 2
    local btnY = panelHeight - 120
    
    -- Back button
    local backBtn = UI.newBox("back_btn", btnWidth, btnHeight, boxTexSet, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, backBtn, btnStartX, btnY)
    UI.setClickable(backBtn, true)
    UI.setZIndex(backBtn, 20)
    UI.setOnClick(backBtn, "onSettingsBack")
    
    local backLabel = UI.newText("back_label", "Back", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    local backLabelWidth = engine.getTextWidth(menuFont, "Back", fontSize)
    UI.addChild(backBtn, backLabel, (btnWidth - backLabelWidth) / 2, (btnHeight / 2) + (fontSize / 2))
    
    -- Apply button
    local applyBtn = UI.newBox("apply_btn", btnWidth, btnHeight, boxTexSet, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, applyBtn, btnStartX + btnWidth + btnSpacing, btnY)
    UI.setClickable(applyBtn, true)
    UI.setZIndex(applyBtn, 20)
    UI.setOnClick(applyBtn, "onSettingsApply")
    
    local applyLabel = UI.newText("apply_label", "Apply", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    local applyLabelWidth = engine.getTextWidth(menuFont, "Apply", fontSize)
    UI.addChild(applyBtn, applyLabel, (btnWidth - applyLabelWidth) / 2, (btnHeight / 2) + (fontSize / 2))
    
    -- Save button
    local saveBtn = UI.newBox("save_btn", btnWidth, btnHeight, boxTexSet, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, saveBtn, btnStartX + (btnWidth + btnSpacing) * 2, btnY)
    UI.setClickable(saveBtn, true)
    UI.setZIndex(saveBtn, 20)
    UI.setOnClick(saveBtn, "onSettingsSave")
    
    local saveLabel = UI.newText("save_label", "Save", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    local saveLabelWidth = engine.getTextWidth(menuFont, "Save", fontSize)
    UI.addChild(saveBtn, saveLabel, (btnWidth - saveLabelWidth) / 2, (btnHeight / 2) + (fontSize / 2))
    
    uiCreated = true
end

function settingsMenu.getSettings()
    return currentSettings
end

function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    
    local scaleChanged = false
    
    -- Apply fullscreen from pending
    if pendingSettings.fullscreen ~= currentSettings.fullscreen then
        currentSettings.fullscreen = pendingSettings.fullscreen
        engine.setFullscreen(currentSettings.fullscreen)
        engine.logInfo("Fullscreen applied: " .. tostring(currentSettings.fullscreen))
    end
    
    -- Get UI scale from textbox
    if uiScaleTextBox then
        local scale = textbox.getNumericValue(uiScaleTextBox)
        if scale >= uiScaleMin and scale <= uiScaleMax then
            if currentSettings.uiScale ~= scale then
                scaleChanged = true
                currentSettings.uiScale = scale
                pendingSettings.uiScale = scale
                engine.setUIScale(scale)
                
                fontSize = math.floor(baseFontSize * scale)
                checkboxSize = math.floor(baseCheckboxSize * scale)
                buttonSize = math.floor(baseButtonSize * scale)
                btnWidth = math.floor(baseBtnWidth * scale)
                btnHeight = math.floor(baseBtnHeight * scale)
                split = math.floor(baseSplit * scale)
                
                engine.logInfo("UI scale applied: " .. tostring(scale))
            end
        else
            engine.logWarn("UI scale out of range: " .. tostring(scale))
        end
    end
    
    -- Get frame limit from textbox
    if frameLimitTextBox then
        local frameLimit = textbox.getNumericValue(frameLimitTextBox)
        if frameLimit >= frameLimitMin and frameLimit <= frameLimitMax then
            currentSettings.frameLimit = math.floor(frameLimit)
            pendingSettings.frameLimit = currentSettings.frameLimit
            engine.setFrameLimit(currentSettings.frameLimit)
            engine.logInfo("Frame limit applied: " .. tostring(currentSettings.frameLimit))
        else
            engine.logWarn("Frame limit out of range: " .. tostring(frameLimit))
        end
    end
    
    if scaleChanged then
        settingsMenu.createUI()
        settingsMenu.show()
    end
end

function settingsMenu.onSave()
    engine.logInfo("Saving settings...")
    settingsMenu.onApply()
    engine.saveVideoConfig()
    engine.logInfo("Settings saved.")
end

function settingsMenu.show()
    settingsMenu.reloadSettings()
    settingsMenu.createUI()
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

function settingsMenu.onTextBoxSubmit(name, value)
    engine.logInfo("TextBox submit: " .. tostring(name) .. " = " .. tostring(value))
    
    if name == "uiscale_input" then
        local scale = tonumber(value)
        if not scale then
            engine.logWarn("Invalid UI scale value: " .. tostring(value))
            if uiScaleTextBox then
                textbox.setText(uiScaleTextBox, tostring(currentSettings.uiScale))
            end
            return
        end
        scale = math.max(uiScaleMin, math.min(uiScaleMax, scale))
        if uiScaleTextBox then
            textbox.setText(uiScaleTextBox, tostring(scale))
        end
        pendingSettings.uiScale = scale
        engine.logInfo("UI scale ready to apply: " .. tostring(scale))
        
    elseif name == "framelimit_input" then
        local frameLimit = tonumber(value)
        if not frameLimit then
            engine.logWarn("Invalid frame limit value: " .. tostring(value))
            if frameLimitTextBox then
                textbox.setText(frameLimitTextBox, tostring(currentSettings.frameLimit or 60))
            end
            return
        end
        frameLimit = math.max(frameLimitMin, math.min(frameLimitMax, math.floor(frameLimit)))
        if frameLimitTextBox then
            textbox.setText(frameLimitTextBox, tostring(frameLimit))
        end
        pendingSettings.frameLimit = frameLimit
        engine.logInfo("Frame limit ready to apply: " .. tostring(frameLimit))
    end
end

function settingsMenu.revertSettings()
    engine.logInfo("Reverting settings to saved config...")
    
    local w, h, fs, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()
    
    local scaleChanged = (currentSettings.uiScale ~= uiScale)
    local fullscreenChanged = (currentSettings.fullscreen ~= fs)
    local frameLimitChanged = (currentSettings.frameLimit ~= frameLimit)
    
    currentSettings.width = w
    currentSettings.height = h
    currentSettings.fullscreen = fs
    currentSettings.uiScale = uiScale
    currentSettings.vsync = vs
    currentSettings.frameLimit = frameLimit
    currentSettings.msaa = msaa
    
    if fullscreenChanged then
        engine.setFullscreen(fs)
        engine.logInfo("Fullscreen reverted to: " .. tostring(fs))
    end
    
    if scaleChanged then
        engine.setUIScale(uiScale)
        engine.logInfo("UI scale reverted to: " .. tostring(uiScale))
        
        fontSize = math.floor(baseFontSize * uiScale)
        checkboxSize = math.floor(baseCheckboxSize * uiScale)
        buttonSize = math.floor(baseButtonSize * uiScale)
        btnWidth = math.floor(baseBtnWidth * uiScale)
        btnHeight = math.floor(baseBtnHeight * uiScale)
        split = math.floor(baseSplit * uiScale)
    end
    
    if frameLimitChanged then
        engine.setFrameLimit(frameLimit)
        engine.logInfo("Frame limit reverted to: " .. tostring(frameLimit))
    end
    
    engine.logInfo("Settings reverted.")
end

function settingsMenu.onBack()
    settingsMenu.revertSettings()
end

function settingsMenu.reloadSettings()
    local w, h, fs, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()
    
    currentSettings.width = w
    currentSettings.height = h
    currentSettings.fullscreen = fs
    currentSettings.uiScale = uiScale
    currentSettings.vsync = vs
    currentSettings.frameLimit = frameLimit or 60
    currentSettings.msaa = msaa
    
    fontSize = math.floor(baseFontSize * uiScale)
    checkboxSize = math.floor(baseCheckboxSize * uiScale)
    buttonSize = math.floor(baseButtonSize * uiScale)
    btnWidth = math.floor(baseBtnWidth * uiScale)
    btnHeight = math.floor(baseBtnHeight * uiScale)
    split = math.floor(baseSplit * uiScale)
    
    engine.logDebug("Settings reloaded from config")
end

function settingsMenu.shutdown()
    textbox.destroyAll()
    checkbox.destroyAll()
    if page then
        UI.deletePage(page)
    end
end

return settingsMenu
