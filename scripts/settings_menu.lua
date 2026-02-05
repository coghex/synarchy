-- Settings Menu Module
local textbox = require("scripts.ui.textbox")
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

-- Scaled values (computed in init)
local fontSize = 32
local checkboxSize = 48
local buttonSize = 64
local btnWidth = 160
local btnHeight = 64
local split = 100

local uiCreated = false

-- Remove duplicate declaration
local uiScaleTextBox = nil
local frameLimitTextBox = nil
local uiScaleMin = 0.5
local uiScaleMax = 4.0
local frameLimitMin = 30
local frameLimitMax = 240

-- Checkbox textures
local texCheckboxChecked = nil
local texCheckboxUnchecked = nil

local currentSettings = {
    width = 800,
    height = 600,
    fullscreen = false,
    uiScale = 1.0,
    vsync = true,
    frameLimit = 60,
    msaa = 0
}

local elements = {}

function settingsMenu.init(boxTex, font, width, height)
    boxTexSet = boxTex
    menuFont = font
    fbW = width
    fbH = height
    
    -- Get UI scale from engine config
    currentSettings.uiScale = engine.getUIScale()
    local uiscale = currentSettings.uiScale
    
    -- Apply scaling to sizes
    fontSize = math.floor(baseFontSize * uiscale)
    checkboxSize = math.floor(baseCheckboxSize * uiscale)
    buttonSize = math.floor(baseButtonSize * uiscale)
    btnWidth = math.floor(baseBtnWidth * uiscale)
    btnHeight = math.floor(baseBtnHeight * uiscale)
    split = math.floor(baseSplit * uiscale)
    
    -- Load checkbox textures
    texCheckboxChecked = engine.loadTexture("assets/textures/ui/checkboxchecked.png")
    texCheckboxUnchecked = engine.loadTexture("assets/textures/ui/checkboxunchecked.png")
    textbox.init()
    
    -- Load current settings
    settingsMenu.reloadSettings()
    
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    -- Destroy old textbox if it exists
    textbox.destroyAll()
    uiScaleTextBox = nil
    frameLimitTextBox = nil
    
    if uiCreated and page then
        UI.deletePage(page)
        elements = {}
    end
    
    local uiscale = currentSettings.uiScale
    
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
    local titleText = UI.newText("settings_title", "Settings", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Settings", fontSize)
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 80)
    
    -- Fullscreen toggle row
    local yPos = 200
    local rowX = 200
    
    -- Label
    local labelText = UI.newText("fullscreen_label", "Fullscreen", menuFont, fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, labelText, rowX, yPos + (fontSize / 2))
    
    -- Checkbox sprite
    local checkboxX = panelWidth - rowX - checkboxSize
    local currentTex = currentSettings.fullscreen and texCheckboxChecked or texCheckboxUnchecked
    elements.fullscreenCheckbox = UI.newSprite("fullscreen_checkbox", checkboxSize, checkboxSize, currentTex, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, elements.fullscreenCheckbox, checkboxX, yPos - (checkboxSize / 2))
    UI.setClickable(elements.fullscreenCheckbox, true)
    UI.setZIndex(elements.fullscreenCheckbox, 20)
    UI.setOnClick(elements.fullscreenCheckbox, "onToggle_fullscreen")

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
    
    -- Button row - Back, Apply, Save
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

function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    
    local scaleChanged = false
    
    -- Get current UI scale from textbox and apply it
    if uiScaleTextBox then
        local scale = textbox.getNumericValue(uiScaleTextBox)
        if scale >= uiScaleMin and scale <= uiScaleMax then
            if currentSettings.uiScale ~= scale then
                scaleChanged = true
                currentSettings.uiScale = scale
                engine.setUIScale(scale)
                
                -- Recalculate scaled sizes
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
    
    -- Get frame limit from textbox and apply it
    if frameLimitTextBox then
        local frameLimit = textbox.getNumericValue(frameLimitTextBox)
        if frameLimit >= frameLimitMin and frameLimit <= frameLimitMax then
            currentSettings.frameLimit = math.floor(frameLimit)
            engine.setFrameLimit(currentSettings.frameLimit)
            engine.logInfo("Frame limit applied: " .. tostring(currentSettings.frameLimit))
        else
            engine.logWarn("Frame limit out of range: " .. tostring(frameLimit))
        end
    end
    
    if scaleChanged then
        -- Rebuild UI with new scale
        settingsMenu.createUI()
        settingsMenu.show()
    end
end

function settingsMenu.onSave()
    engine.logInfo("Saving settings...")
    
    -- Apply first to make sure current values are set
    settingsMenu.onApply()
    
    -- Save video config to file
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
        -- Destroy old textbox before recreating
        if uiScaleTextBox then
            textbox.destroy(uiScaleTextBox)
            uiScaleTextBox = nil
        end
        if frameLimitTextBox then
            textbox.destroy(frameLimitTextBox)
            frameLimitTextBox = nil
        end
        settingsMenu.createUI()
    end
end

function settingsMenu.handleTextBoxClick(callbackName)
    return textbox.handleCallback(callbackName)
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
        
        -- Clamp to valid range
        scale = math.max(uiScaleMin, math.min(uiScaleMax, scale))
        
        -- Update textbox to show clamped value
        if uiScaleTextBox then
            textbox.setText(uiScaleTextBox, tostring(scale))
        end
        
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
        
        -- Clamp to valid range
        frameLimit = math.max(frameLimitMin, math.min(frameLimitMax, math.floor(frameLimit)))
        
        -- Update textbox to show clamped value
        if frameLimitTextBox then
            textbox.setText(frameLimitTextBox, tostring(frameLimit))
        end
        
        engine.logInfo("Frame limit ready to apply: " .. tostring(frameLimit))
    end
end

-- Revert settings to saved config (discard unsaved changes)
function settingsMenu.revertSettings()
    engine.logInfo("Reverting settings to saved config...")
    
    -- Read saved config from engine
    local w, h, fs, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()
    
    -- Check if UI scale changed (need to revert visual changes)
    local scaleChanged = (currentSettings.uiScale ~= uiScale)
    local frameLimitChanged = (currentSettings.frameLimit ~= frameLimit)
    
    -- Reset currentSettings to saved values
    currentSettings.width = w
    currentSettings.height = h
    currentSettings.fullscreen = fs
    currentSettings.uiScale = uiScale
    currentSettings.vsync = vs
    currentSettings.frameLimit = frameLimit
    currentSettings.msaa = msaa
    
    -- If scale was changed but not saved, revert it in engine
    if scaleChanged then
        engine.setUIScale(uiScale)
        engine.logInfo("UI scale reverted to: " .. tostring(uiScale))
        
        -- Recalculate scaled sizes
        fontSize = math.floor(baseFontSize * uiScale)
        checkboxSize = math.floor(baseCheckboxSize * uiScale)
        buttonSize = math.floor(baseButtonSize * uiScale)
        btnWidth = math.floor(baseBtnWidth * uiScale)
        btnHeight = math.floor(baseBtnHeight * uiScale)
        split = math.floor(baseSplit * uiScale)
    end
    
    -- If frame limit was changed but not saved, revert it
    if frameLimitChanged then
        engine.setFrameLimit(frameLimit)
        engine.logInfo("Frame limit reverted to: " .. tostring(frameLimit))
    end
    
    engine.logInfo("Settings reverted.")
end

-- Call this when Back is pressed
function settingsMenu.onBack()
    settingsMenu.revertSettings()
end

-- Reload settings from engine config
function settingsMenu.reloadSettings()
    local w, h, fs, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()
    
    currentSettings.width = w
    currentSettings.height = h
    currentSettings.fullscreen = fs
    currentSettings.uiScale = uiScale
    currentSettings.vsync = vs
    currentSettings.frameLimit = frameLimit or 60
    currentSettings.msaa = msaa
    
    -- Recalculate scaled sizes
    fontSize = math.floor(baseFontSize * uiScale)
    checkboxSize = math.floor(baseCheckboxSize * uiScale)
    buttonSize = math.floor(baseButtonSize * uiScale)
    btnWidth = math.floor(baseBtnWidth * uiScale)
    btnHeight = math.floor(baseBtnHeight * uiScale)
    split = math.floor(baseSplit * uiScale)
    
    engine.logDebug("Settings reloaded from config")
end

function settingsMenu.shutdown()
    if uiScaleTextBox then
        textbox.destroy(uiScaleTextBox)
        uiScaleTextBox = nil
    end
    if frameLimitTextBox then
        textbox.destroy(frameLimitTextBox)
        frameLimitTextBox = nil
    end
    if page then
        UI.deletePage(page)
    end
end

return settingsMenu
