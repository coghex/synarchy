-- Settings Menu Module
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local button = require("scripts.ui.button")
local settingsMenu = {}

settingsMenu.page = nil
settingsMenu.panelId = nil
settingsMenu.panelTexSet = nil
settingsMenu.buttonTexSet = nil
settingsMenu.menuFont = nil
settingsMenu.fbW = 0
settingsMenu.fbH = 0

-- Base sizes (unscaled)
settingsMenu.baseSizes = {
    fontSize = 32,
    checkboxSize = 48,
    btnWidth = 200,
    btnHeight = 64,
    textboxWidth = 150,
    textboxHeight = 40,
    rowSpacing = 100,
    btnSpacing = 20,
}

settingsMenu.uiCreated = false

settingsMenu.titleLabelId = nil
settingsMenu.fullscreenLabelId = nil
settingsMenu.scalingLabelId = nil
settingsMenu.frameLimitLabelId = nil

settingsMenu.uiScaleTextBox = nil
settingsMenu.frameLimitTextBox = nil
settingsMenu.fullscreenCheckboxId = nil
settingsMenu.backButtonId = nil
settingsMenu.applyButtonId = nil
settingsMenu.saveButtonId = nil

settingsMenu.uiScaleMin = 0.5
settingsMenu.uiScaleMax = 4.0
settingsMenu.frameLimitMin = 30
settingsMenu.frameLimitMax = 240

settingsMenu.currentSettings = {
    width = 800,
    height = 600,
    fullscreen = false,
    uiScale = 1.0,
    vsync = true,
    frameLimit = 60,
    msaa = 0
}

settingsMenu.pendingSettings = {}

settingsMenu.showMenuCallback = nil

function settingsMenu.setShowMenuCallback(callback)
    settingsMenu.showMenuCallback = callback
end

function settingsMenu.init(panelTex, btnTex, font, width, height)
    settingsMenu.panelTexSet = panelTex
    settingsMenu.buttonTexSet = btnTex
    settingsMenu.menuFont = font
    settingsMenu.fbW = width
    settingsMenu.fbH = height
    
    settingsMenu.currentSettings.uiScale = scale.get()
    
    textbox.init()
    checkbox.init()
    
    settingsMenu.reloadSettings()
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    label.destroyAll()
    textbox.destroyAll()
    checkbox.destroyAll()
    button.destroyAll()
    panel.destroyAll()
    
    settingsMenu.titleLabelId = nil
    settingsMenu.fullscreenLabelId = nil
    settingsMenu.scalingLabelId = nil
    settingsMenu.frameLimitLabelId = nil
    settingsMenu.uiScaleTextBox = nil
    settingsMenu.frameLimitTextBox = nil
    settingsMenu.fullscreenCheckboxId = nil
    settingsMenu.backButtonId = nil
    settingsMenu.applyButtonId = nil
    settingsMenu.saveButtonId = nil
    settingsMenu.panelId = nil
    
    if settingsMenu.uiCreated and settingsMenu.page then
        UI.deletePage(settingsMenu.page)
    end
    
    settingsMenu.pendingSettings = {
        fullscreen = settingsMenu.currentSettings.fullscreen,
        uiScale = settingsMenu.currentSettings.uiScale,
        frameLimit = settingsMenu.currentSettings.frameLimit,
    }
    
    local uiscale = settingsMenu.currentSettings.uiScale
    local s = scale.applyAllWith(settingsMenu.baseSizes, uiscale)
    
    settingsMenu.page = UI.newPage("settings_menu", "modal")
    
    -- Panel sizing
    local panelWidth = math.floor(settingsMenu.fbW * 0.7)
    local panelHeight = math.floor(settingsMenu.fbH * 0.7)
    local panelX = (settingsMenu.fbW - panelWidth) / 2
    local panelY = (settingsMenu.fbH - panelHeight) / 2
    
    -- Create panel
    settingsMenu.panelId = panel.new({
        name = "settings_panel",
        page = settingsMenu.page,
        x = panelX,
        y = panelY,
        width = panelWidth,
        height = panelHeight,
        textureSet = settingsMenu.panelTexSet,
        color = {1.0, 1.0, 1.0, 1.0},
        tileSize = 64,
        zIndex = 10,
        padding = { top = 80, bottom = 120, left = 60, right = 60 },
        uiscale = uiscale,
    })
    
    local baseZ = panel.getZIndex(settingsMenu.panelId)
    
    -- Title label (centered at top)
    settingsMenu.titleLabelId = label.new({
        name = "settings_title",
        text = "Settings",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 1,
    })
    
    local titleW, titleH = label.getSize(settingsMenu.titleLabelId)
    panel.place(settingsMenu.panelId, label.getElementHandle(settingsMenu.titleLabelId), {
        x = "50%",
        y = "0px",
        origin = "top-center",
        width = titleW,
        height = titleH,
    })
    
    -- Calculate row positions
    local rowY1 = s.fontSize + s.rowSpacing
    local rowY2 = rowY1 + s.rowSpacing
    local rowY3 = rowY2 + s.rowSpacing
    
    -- Row 1: Fullscreen label and checkbox
    settingsMenu.fullscreenLabelId = label.new({
        name = "fullscreen_label",
        text = "Fullscreen",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 1,
    })
    
    local flLabelW, flLabelH = label.getSize(settingsMenu.fullscreenLabelId)
    panel.place(settingsMenu.panelId, label.getElementHandle(settingsMenu.fullscreenLabelId), {
        x = "0px",
        y = rowY1 .. "px",
        origin = "top-left",
        width = flLabelW,
        height = flLabelH,
    })
    
    settingsMenu.fullscreenCheckboxId = checkbox.new({
        name = "fullscreen",
        size = settingsMenu.baseSizes.checkboxSize,
        uiscale = uiscale,
        page = settingsMenu.page,
        default = settingsMenu.currentSettings.fullscreen,
        onChange = function(checked, id, name)
            settingsMenu.pendingSettings.fullscreen = checked
            engine.logInfo("Fullscreen pending: " .. tostring(checked))
        end,
    })
    
    local cbW, cbH = checkbox.getSize(settingsMenu.fullscreenCheckboxId)
    panel.place(settingsMenu.panelId, checkbox.getElementHandle(settingsMenu.fullscreenCheckboxId), {
        x = "100%",
        y = rowY1 .. "px",
        origin = "top-right",
        width = cbW,
        height = cbH,
    })
    UI.setZIndex(checkbox.getElementHandle(settingsMenu.fullscreenCheckboxId), baseZ + 1)
    
    -- Row 2: UI Scaling label and textbox
    settingsMenu.scalingLabelId = label.new({
        name = "scaling_label",
        text = "UI Scaling",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 1,
    })
    
    local scLabelW, scLabelH = label.getSize(settingsMenu.scalingLabelId)
    panel.place(settingsMenu.panelId, label.getElementHandle(settingsMenu.scalingLabelId), {
        x = "0px",
        y = rowY2 .. "px",
        origin = "top-left",
        width = scLabelW,
        height = scLabelH,
    })
    
    settingsMenu.uiScaleTextBox = textbox.new({
        name = "uiscale_input",
        width = settingsMenu.baseSizes.textboxWidth,
        height = settingsMenu.baseSizes.textboxHeight,
        page = settingsMenu.page,
        uiscale = uiscale,
        font = settingsMenu.menuFont,
        fontSize = 24,
        default = tostring(settingsMenu.currentSettings.uiScale),
        textType = textbox.Type.SCALE
    })
    
    local tbW, tbH = textbox.getSize(settingsMenu.uiScaleTextBox)
    panel.place(settingsMenu.panelId, textbox.getElementHandle(settingsMenu.uiScaleTextBox), {
        x = "100%",
        y = rowY2 .. "px",
        origin = "top-right",
        width = tbW,
        height = tbH,
    })
    
    -- Row 3: Frame Limit label and textbox
    settingsMenu.frameLimitLabelId = label.new({
        name = "framelimit_label",
        text = "Frame Limit",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 1,
    })
    
    local frLabelW, frLabelH = label.getSize(settingsMenu.frameLimitLabelId)
    panel.place(settingsMenu.panelId, label.getElementHandle(settingsMenu.frameLimitLabelId), {
        x = "0px",
        y = rowY3 .. "px",
        origin = "top-left",
        width = frLabelW,
        height = frLabelH,
    })
    
    settingsMenu.frameLimitTextBox = textbox.new({
        name = "framelimit_input",
        width = settingsMenu.baseSizes.textboxWidth,
        height = settingsMenu.baseSizes.textboxHeight,
        page = settingsMenu.page,
        uiscale = uiscale,
        font = settingsMenu.menuFont,
        fontSize = 24,
        default = tostring(settingsMenu.currentSettings.frameLimit or 60),
        textType = textbox.Type.NUMBER,
    })
    
    local flW, flH = textbox.getSize(settingsMenu.frameLimitTextBox)
    panel.place(settingsMenu.panelId, textbox.getElementHandle(settingsMenu.frameLimitTextBox), {
        x = "100%",
        y = rowY3 .. "px",
        origin = "top-right",
        width = flW,
        height = flH,
    })
    
    -- Create buttons
    settingsMenu.backButtonId = button.new({
        name = "back_btn",
        text = "Back",
        width = settingsMenu.baseSizes.btnWidth,
        height = settingsMenu.baseSizes.btnHeight,
        fontSize = settingsMenu.baseSizes.fontSize,
        uiscale = uiscale,
        page = settingsMenu.page,
        font = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0},
        textColor = {0.0, 0.0, 0.0, 1.0},
        onClick = function(id, name)
            settingsMenu.onBack()
            if settingsMenu.showMenuCallback then
                settingsMenu.showMenuCallback("main")
            end
        end,
    })
    
    settingsMenu.applyButtonId = button.new({
        name = "apply_btn",
        text = "Apply",
        width = settingsMenu.baseSizes.btnWidth,
        height = settingsMenu.baseSizes.btnHeight,
        fontSize = settingsMenu.baseSizes.fontSize,
        uiscale = uiscale,
        page = settingsMenu.page,
        font = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0},
        textColor = {0.0, 0.0, 0.0, 1.0},
        onClick = function(id, name)
            settingsMenu.onApply()
        end,
    })
    
    settingsMenu.saveButtonId = button.new({
        name = "save_btn",
        text = "Save",
        width = settingsMenu.baseSizes.btnWidth,
        height = settingsMenu.baseSizes.btnHeight,
        fontSize = settingsMenu.baseSizes.fontSize,
        uiscale = uiscale,
        page = settingsMenu.page,
        font = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0},
        textColor = {0.0, 0.0, 0.0, 1.0},
        onClick = function(id, name)
            settingsMenu.onSave()
        end,
    })
    
    -- Get button sizes and place in row at bottom
    local backW, backH = button.getSize(settingsMenu.backButtonId)
    local applyW, applyH = button.getSize(settingsMenu.applyButtonId)
    local saveW, saveH = button.getSize(settingsMenu.saveButtonId)
    
    panel.placeRow(
        settingsMenu.panelId,
        {
            button.getElementHandle(settingsMenu.backButtonId),
            button.getElementHandle(settingsMenu.applyButtonId),
            button.getElementHandle(settingsMenu.saveButtonId),
        },
        {
            { width = backW, height = backH },
            { width = applyW, height = applyH },
            { width = saveW, height = saveH },
        },
        {
            x = "50%",
            y = "100%",
            origin = "bottom-center",
            spacing = s.btnSpacing,
        }
    )
    
    UI.setZIndex(button.getElementHandle(settingsMenu.backButtonId), baseZ + 1)
    UI.setZIndex(button.getElementHandle(settingsMenu.applyButtonId), baseZ + 1)
    UI.setZIndex(button.getElementHandle(settingsMenu.saveButtonId), baseZ + 1)
    
    settingsMenu.uiCreated = true
end

function settingsMenu.getSettings()
    return settingsMenu.currentSettings
end

function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    
    local scaleChanged = false
    
    if settingsMenu.pendingSettings.fullscreen ~= settingsMenu.currentSettings.fullscreen then
        settingsMenu.currentSettings.fullscreen = settingsMenu.pendingSettings.fullscreen
        engine.setFullscreen(settingsMenu.currentSettings.fullscreen)
        engine.logInfo("Fullscreen applied: " .. tostring(settingsMenu.currentSettings.fullscreen))
    end
    
    if settingsMenu.uiScaleTextBox then
        local newScale = textbox.getNumericValue(settingsMenu.uiScaleTextBox)
        if newScale >= settingsMenu.uiScaleMin and newScale <= settingsMenu.uiScaleMax then
            if settingsMenu.currentSettings.uiScale ~= newScale then
                scaleChanged = true
                settingsMenu.currentSettings.uiScale = newScale
                settingsMenu.pendingSettings.uiScale = newScale
                engine.setUIScale(newScale)
                engine.logInfo("UI scale applied: " .. tostring(newScale))
            end
        else
            engine.logWarn("UI scale out of range: " .. tostring(newScale))
        end
    end
    
    if settingsMenu.frameLimitTextBox then
        local frameLimit = textbox.getNumericValue(settingsMenu.frameLimitTextBox)
        if frameLimit >= settingsMenu.frameLimitMin and frameLimit <= settingsMenu.frameLimitMax then
            settingsMenu.currentSettings.frameLimit = math.floor(frameLimit)
            settingsMenu.pendingSettings.frameLimit = settingsMenu.currentSettings.frameLimit
            engine.setFrameLimit(settingsMenu.currentSettings.frameLimit)
            engine.logInfo("Frame limit applied: " .. tostring(settingsMenu.currentSettings.frameLimit))
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
    if settingsMenu.page then
        UI.showPage(settingsMenu.page)
    end
end

function settingsMenu.hide()
    if settingsMenu.page then
        UI.hidePage(settingsMenu.page)
    end
end

function settingsMenu.onFramebufferResize(width, height)
    settingsMenu.fbW = width
    settingsMenu.fbH = height
    if settingsMenu.uiCreated then
        settingsMenu.createUI()
    end
end

function settingsMenu.onTextBoxSubmit(name, value)
    engine.logInfo("TextBox submit: " .. tostring(name) .. " = " .. tostring(value))
    
    if name == "uiscale_input" then
        local newScale = tonumber(value)
        if not newScale then
            if settingsMenu.uiScaleTextBox then
                textbox.setText(settingsMenu.uiScaleTextBox, tostring(settingsMenu.currentSettings.uiScale))
            end
            return
        end
        newScale = math.max(settingsMenu.uiScaleMin, math.min(settingsMenu.uiScaleMax, newScale))
        if settingsMenu.uiScaleTextBox then
            textbox.setText(settingsMenu.uiScaleTextBox, tostring(newScale))
        end
        settingsMenu.pendingSettings.uiScale = newScale
        
    elseif name == "framelimit_input" then
        local frameLimit = tonumber(value)
        if not frameLimit then
            if settingsMenu.frameLimitTextBox then
                textbox.setText(settingsMenu.frameLimitTextBox, tostring(settingsMenu.currentSettings.frameLimit or 60))
            end
            return
        end
        frameLimit = math.max(settingsMenu.frameLimitMin, math.min(settingsMenu.frameLimitMax, math.floor(frameLimit)))
        if settingsMenu.frameLimitTextBox then
            textbox.setText(settingsMenu.frameLimitTextBox, tostring(frameLimit))
        end
        settingsMenu.pendingSettings.frameLimit = frameLimit
    end
end

function settingsMenu.revertSettings()
    engine.logInfo("Reverting settings to saved config...")
    
    local w, h, fs, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()
    
    local scaleChanged = (settingsMenu.currentSettings.uiScale ~= uiScale)
    local fullscreenChanged = (settingsMenu.currentSettings.fullscreen ~= fs)
    local frameLimitChanged = (settingsMenu.currentSettings.frameLimit ~= frameLimit)
    
    settingsMenu.currentSettings.width = w
    settingsMenu.currentSettings.height = h
    settingsMenu.currentSettings.fullscreen = fs
    settingsMenu.currentSettings.uiScale = uiScale
    settingsMenu.currentSettings.vsync = vs
    settingsMenu.currentSettings.frameLimit = frameLimit
    settingsMenu.currentSettings.msaa = msaa
    
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
    
    settingsMenu.currentSettings.width = w
    settingsMenu.currentSettings.height = h
    settingsMenu.currentSettings.fullscreen = fs
    settingsMenu.currentSettings.uiScale = uiScale
    settingsMenu.currentSettings.vsync = vs
    settingsMenu.currentSettings.frameLimit = frameLimit or 60
    settingsMenu.currentSettings.msaa = msaa
end

function settingsMenu.shutdown()
    label.destroyAll()
    textbox.destroyAll()
    checkbox.destroyAll()
    button.destroyAll()
    panel.destroyAll()
    if settingsMenu.page then
        UI.deletePage(settingsMenu.page)
    end
end

return settingsMenu
