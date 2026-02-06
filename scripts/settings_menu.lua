-- Settings Menu Module
local scale = require("scripts.ui.scale")
local textbox = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local button = require("scripts.ui.button")
local settingsMenu = {}

local page = nil
local panelTexSet = nil
local buttonTexSet = nil
local menuFont = nil
local fbW, fbH = 0, 0

-- Base sizes (unscaled)
local baseSizes = {
    fontSize = 32,
    checkboxSize = 48,
    btnWidth = 200,
    btnHeight = 64,
    topMargin = 80,
    bottomMargin = 100,
    rowSpacing = 120,
    rowX = 100,
    titleOffset = 60,
    textboxWidth = 150,
    textboxHeight = 40,
    btnSpacing = 20,
}

local uiCreated = false

local uiScaleTextBox = nil
local frameLimitTextBox = nil
local fullscreenCheckboxId = nil
local backButtonId = nil
local applyButtonId = nil
local saveButtonId = nil

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

local pendingSettings = {}

local showMenuCallback = nil

function settingsMenu.setShowMenuCallback(callback)
    showMenuCallback = callback
end

function settingsMenu.init(panelTex, btnTex, font, width, height)
    panelTexSet = panelTex
    buttonTexSet = btnTex
    menuFont = font
    fbW = width
    fbH = height
    
    currentSettings.uiScale = scale.get()
    
    textbox.init()
    checkbox.init()
    
    settingsMenu.reloadSettings()
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    textbox.destroyAll()
    checkbox.destroyAll()
    button.destroyAll()
    uiScaleTextBox = nil
    frameLimitTextBox = nil
    fullscreenCheckboxId = nil
    backButtonId = nil
    applyButtonId = nil
    saveButtonId = nil
    
    if uiCreated and page then
        UI.deletePage(page)
    end
    
    pendingSettings = {
        fullscreen = currentSettings.fullscreen,
        uiScale = currentSettings.uiScale,
        frameLimit = currentSettings.frameLimit,
    }
    
    local uiscale = currentSettings.uiScale
    local s = scale.applyAllWith(baseSizes, uiscale)
    
    page = UI.newPage("settings_menu", "modal")
    
    -- Panel sizing
    local panelWidth = math.floor(fbW * 0.7)
    local panelHeight = math.floor(fbH * 0.7)
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    local panel = UI.newBox("settings_panel", panelWidth, panelHeight, panelTexSet, 64, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, panel, panelX, panelY)
    UI.setZIndex(panel, 10)
    
    -- Title
    local titleY = s.topMargin + s.fontSize
    local titleText = UI.newText("settings_title", "Settings", menuFont, s.fontSize, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Settings", s.fontSize)
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, titleY)
    
    -- Content area starts after title
    local contentStartY = titleY + s.titleOffset
    
    -- Fullscreen toggle row
    local yPos = contentStartY
    
    local labelText = UI.newText("fullscreen_label", "Fullscreen", menuFont, s.fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, labelText, s.rowX, yPos + (s.fontSize / 2))
    
    local checkboxX = panelWidth - s.rowX - s.checkboxSize
    fullscreenCheckboxId = checkbox.new({
        name = "fullscreen",
        size = baseSizes.checkboxSize,
        uiscale = uiscale,
        page = page,
        parent = panel,
        x = checkboxX,
        y = yPos - (s.checkboxSize / 2) + (s.fontSize / 2),
        default = currentSettings.fullscreen,
        onChange = function(checked, id, name)
            pendingSettings.fullscreen = checked
            engine.logInfo("Fullscreen pending: " .. tostring(checked))
        end,
    })
    UI.setZIndex(checkbox.getElementHandle(fullscreenCheckboxId), 20)

    -- UI scaling row
    yPos = yPos + s.rowSpacing
    local scalingLabel = UI.newText("scaling_label", "UI Scaling", menuFont, s.fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, scalingLabel, s.rowX, yPos + (s.fontSize / 2))
    
    local textboxX = panelWidth - s.rowX - s.textboxWidth
    uiScaleTextBox = textbox.new({
        name = "uiscale_input",
        x = textboxX,
        y = yPos,
        width = s.textboxWidth,
        height = s.textboxHeight,
        page = page,
        parent = panel,
        uiscale = uiscale,
        font = menuFont,
        fontSize = 24,
        default = tostring(currentSettings.uiScale),
        textType = textbox.Type.SCALE
    })

    -- Frame limit row
    yPos = yPos + s.rowSpacing
    local frameLimitLabel = UI.newText("framelimit_label", "Frame Limit", menuFont, s.fontSize, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(panel, frameLimitLabel, s.rowX, yPos + (s.fontSize / 2))
    
    frameLimitTextBox = textbox.new({
        name = "framelimit_input",
        x = textboxX,
        y = yPos,
        width = s.textboxWidth,
        height = s.textboxHeight,
        page = page,
        parent = panel,
        uiscale = uiscale,
        font = menuFont,
        fontSize = 24,
        default = tostring(currentSettings.frameLimit or 60),
        textType = textbox.Type.NUMBER,
    })
    
    -- Button row
    local totalBtnWidth = (s.btnWidth * 3) + (s.btnSpacing * 2)
    local btnStartX = (panelWidth - totalBtnWidth) / 2
    local btnY = panelHeight - s.bottomMargin - s.btnHeight
    
    -- Back button
    backButtonId = button.new({
        name = "back_btn",
        text = "Back",
        x = btnStartX,
        y = btnY,
        width = baseSizes.btnWidth,
        height = baseSizes.btnHeight,
        fontSize = baseSizes.fontSize,
        uiscale = uiscale,
        page = page,
        parent = panel,
        font = menuFont,
        textureSet = buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0},
        textColor = {0.0, 0.0, 0.0, 1.0},
        onClick = function(id, name)
            settingsMenu.onBack()
            if showMenuCallback then
                showMenuCallback("main")
            end
        end,
    })
    
    -- Apply button
    applyButtonId = button.new({
        name = "apply_btn",
        text = "Apply",
        x = btnStartX + s.btnWidth + s.btnSpacing,
        y = btnY,
        width = baseSizes.btnWidth,
        height = baseSizes.btnHeight,
        fontSize = baseSizes.fontSize,
        uiscale = uiscale,
        page = page,
        parent = panel,
        font = menuFont,
        textureSet = buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0},
        textColor = {0.0, 0.0, 0.0, 1.0},
        onClick = function(id, name)
            settingsMenu.onApply()
        end,
    })
    
    -- Save button
    saveButtonId = button.new({
        name = "save_btn",
        text = "Save",
        x = btnStartX + (s.btnWidth + s.btnSpacing) * 2,
        y = btnY,
        width = baseSizes.btnWidth,
        height = baseSizes.btnHeight,
        fontSize = baseSizes.fontSize,
        uiscale = uiscale,
        page = page,
        parent = panel,
        font = menuFont,
        textureSet = buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0},
        textColor = {0.0, 0.0, 0.0, 1.0},
        onClick = function(id, name)
            settingsMenu.onSave()
        end,
    })
    
    uiCreated = true
end

function settingsMenu.getSettings()
    return currentSettings
end

function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    
    local scaleChanged = false
    
    if pendingSettings.fullscreen ~= currentSettings.fullscreen then
        currentSettings.fullscreen = pendingSettings.fullscreen
        engine.setFullscreen(currentSettings.fullscreen)
        engine.logInfo("Fullscreen applied: " .. tostring(currentSettings.fullscreen))
    end
    
    if uiScaleTextBox then
        local newScale = textbox.getNumericValue(uiScaleTextBox)
        if newScale >= uiScaleMin and newScale <= uiScaleMax then
            if currentSettings.uiScale ~= newScale then
                scaleChanged = true
                currentSettings.uiScale = newScale
                pendingSettings.uiScale = newScale
                engine.setUIScale(newScale)
                engine.logInfo("UI scale applied: " .. tostring(newScale))
            end
        else
            engine.logWarn("UI scale out of range: " .. tostring(newScale))
        end
    end
    
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
        local newScale = tonumber(value)
        if not newScale then
            if uiScaleTextBox then
                textbox.setText(uiScaleTextBox, tostring(currentSettings.uiScale))
            end
            return
        end
        newScale = math.max(uiScaleMin, math.min(uiScaleMax, newScale))
        if uiScaleTextBox then
            textbox.setText(uiScaleTextBox, tostring(newScale))
        end
        pendingSettings.uiScale = newScale
        
    elseif name == "framelimit_input" then
        local frameLimit = tonumber(value)
        if not frameLimit then
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
    end
    
    if scaleChanged then
        engine.setUIScale(uiScale)
    end
    
    if frameLimitChanged then
        engine.setFrameLimit(frameLimit)
    end
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
end

function settingsMenu.shutdown()
    textbox.destroyAll()
    checkbox.destroyAll()
    button.destroyAll()
    if page then
        UI.deletePage(page)
    end
end

return settingsMenu
