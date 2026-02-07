-- Settings Menu Module
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local button = require("scripts.ui.button")
local dropdown = require("scripts.ui.dropdown")
local tabbar = require("scripts.ui.tabbar")
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
    dropdownHeight = 40,
    rowSpacing = 100,
    btnSpacing = 20,
    tabHeight = 40,
    tabFontSize = 24,
}

settingsMenu.uiCreated = false
settingsMenu.tabBarId = nil
settingsMenu.activeTab = "graphics"

-- Tab content element handles (for show/hide)
settingsMenu.tabContent = {
    system = {},
    graphics = {},
    input = {},
}

settingsMenu.titleLabelId = nil
settingsMenu.resolutionLabelId = nil
settingsMenu.resolutionDropdownId = nil
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

-- Standard resolutions grouped by aspect ratio
settingsMenu.resolutions = {
    { text = "1280x720",   value = "1280x720",   width = 1280,  height = 720 },
    { text = "1366x768",   value = "1366x768",   width = 1366,  height = 768 },
    { text = "1600x900",   value = "1600x900",   width = 1600,  height = 900 },
    { text = "1920x1080",  value = "1920x1080",  width = 1920,  height = 1080 },
    { text = "2560x1440",  value = "2560x1440",  width = 2560,  height = 1440 },
    { text = "3840x2160",  value = "3840x2160",  width = 3840,  height = 2160 },
    { text = "1280x800",   value = "1280x800",   width = 1280,  height = 800 },
    { text = "1440x900",   value = "1440x900",   width = 1440,  height = 900 },
    { text = "1680x1050",  value = "1680x1050",  width = 1680,  height = 1050 },
    { text = "1920x1200",  value = "1920x1200",  width = 1920,  height = 1200 },
    { text = "2560x1600",  value = "2560x1600",  width = 2560,  height = 1600 },
    { text = "800x600",    value = "800x600",     width = 800,   height = 600 },
    { text = "1024x768",   value = "1024x768",    width = 1024,  height = 768 },
    { text = "1600x1200",  value = "1600x1200",   width = 1600,  height = 1200 },
    { text = "2560x1080",  value = "2560x1080",  width = 2560,  height = 1080 },
    { text = "3440x1440",  value = "3440x1440",  width = 3440,  height = 1440 },
}

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

function settingsMenu.resolutionString(w, h)
    return tostring(w) .. "x" .. tostring(h)
end

function settingsMenu.findResolutionIndex(w, h)
    local target = settingsMenu.resolutionString(w, h)
    for i, res in ipairs(settingsMenu.resolutions) do
        if res.value == target then
            return i
        end
    end
    return nil
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
    dropdown.init()
    tabbar.init()
    
    settingsMenu.reloadSettings()
    settingsMenu.createUI()
end

function settingsMenu.createUI()
    label.destroyAll()
    textbox.destroyAll()
    checkbox.destroyAll()
    button.destroyAll()
    dropdown.destroyAll()
    tabbar.destroyAll()
    panel.destroyAll()
    
    settingsMenu.titleLabelId = nil
    settingsMenu.resolutionLabelId = nil
    settingsMenu.resolutionDropdownId = nil
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
    settingsMenu.tabBarId = nil
    settingsMenu.tabContent = { system = {}, graphics = {}, input = {} }
    
    if settingsMenu.uiCreated and settingsMenu.page then
        UI.deletePage(settingsMenu.page)
    end
    
    settingsMenu.pendingSettings = {
        width = settingsMenu.currentSettings.width,
        height = settingsMenu.currentSettings.height,
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
    local bounds = panel.getContentBounds(settingsMenu.panelId)
    
    -- Title
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
    
    -- Tab bar below title
    local tabY = panelY + bounds.y + s.fontSize + math.floor(20 * uiscale)
    local tabFrameHeight = panelHeight - bounds.y - s.fontSize
        - math.floor(20 * uiscale) - s.tabHeight - s.btnHeight
        - math.floor(40 * uiscale) - bounds.y
    
    settingsMenu.tabBarId = tabbar.new({
        name = "settings_tabs",
        page = settingsMenu.page,
        x = panelX + bounds.x,
        y = tabY,
        width = bounds.width,
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.tabFontSize,
        tabHeight = settingsMenu.baseSizes.tabHeight,
        frameHeight = tabFrameHeight,
        uiscale = uiscale,
        zIndex = baseZ + 1,
        textColor = {0.0, 0.0, 0.0, 1.0},
        selectedTextColor = {1.0, 1.0, 1.0, 1.0},
        tabs = {
            { name = "System",   key = "system" },
            { name = "Graphics", key = "graphics" },
            { name = "Input",    key = "input" },
        },
        onChange = function(key, index, tbId)
            settingsMenu.onTabChanged(key)
        end,
    })
    
    -- Select the active tab
    tabbar.selectByKey(settingsMenu.tabBarId, settingsMenu.activeTab)
    
    -- Get frame bounds for content placement
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(settingsMenu.tabBarId)
    local contentPadding = math.floor(20 * uiscale)
    local contentX = frameX + contentPadding
    local contentY = frameY + contentPadding
    local contentW = frameW - (contentPadding * 2)
    
    -- Build graphics tab content
    settingsMenu.createGraphicsTab(contentX, contentY, contentW, s, baseZ, uiscale)
    
    -- Build system tab content (placeholder)
    settingsMenu.createSystemTab(contentX, contentY, contentW, s, baseZ, uiscale)
    
    -- Build input tab content (placeholder)
    settingsMenu.createInputTab(contentX, contentY, contentW, s, baseZ, uiscale)
    
    -- Buttons at bottom
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
    
    -- Show only the active tab's content
    settingsMenu.showTab(settingsMenu.activeTab)
    
    settingsMenu.uiCreated = true
end

-----------------------------------------------------------
-- Tab Content Creation
-----------------------------------------------------------

function settingsMenu.createGraphicsTab(contentX, contentY, contentW, s, baseZ, uiscale)
    local rowY1 = contentY
    local rowY2 = rowY1 + s.rowSpacing
    local rowY3 = rowY2 + s.rowSpacing
    local rowY4 = rowY3 + s.rowSpacing
    
    -- Row 1: Resolution
    settingsMenu.resolutionLabelId = label.new({
        name = "resolution_label",
        text = "Resolution",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 2,
    })
    local resLabelHandle = label.getElementHandle(settingsMenu.resolutionLabelId)
    UI.addToPage(settingsMenu.page, resLabelHandle, contentX, rowY1 + s.fontSize)
    
    local currentRes = settingsMenu.resolutionString(
        settingsMenu.currentSettings.width,
        settingsMenu.currentSettings.height
    )
    
    local ddW_est = math.floor(200 * uiscale)
    
    settingsMenu.resolutionDropdownId = dropdown.new({
        name = "resolution",
        options = settingsMenu.resolutions,
        default = currentRes,
        font = settingsMenu.menuFont,
        fontSize = 24,
        height = settingsMenu.baseSizes.dropdownHeight,
        page = settingsMenu.page,
        x = 0,
        y = 0,
        uiscale = uiscale,
        zIndex = baseZ + 2,
        validateChar = dropdown.resolutionValidator,
        matchFn = dropdown.resolutionMatcher,
        maxVisibleOptions = 8,
        onChange = function(value, text, id, name)
            local w, h = value:match("^(%d+)x(%d+)$")
            if w and h then
                settingsMenu.pendingSettings.width = tonumber(w)
                settingsMenu.pendingSettings.height = tonumber(h)
                engine.logInfo("Resolution pending: " .. text)
            end
        end,
    })
    
    local ddW, ddH = dropdown.getSize(settingsMenu.resolutionDropdownId)
    dropdown.setPosition(settingsMenu.resolutionDropdownId,
        contentX + contentW - ddW, rowY1)
    
    -- Row 2: Fullscreen
    settingsMenu.fullscreenLabelId = label.new({
        name = "fullscreen_label",
        text = "Fullscreen",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 2,
    })
    local flLabelHandle = label.getElementHandle(settingsMenu.fullscreenLabelId)
    UI.addToPage(settingsMenu.page, flLabelHandle, contentX, rowY2 + s.fontSize)
    
    settingsMenu.fullscreenCheckboxId = checkbox.new({
        name = "fullscreen",
        size = settingsMenu.baseSizes.checkboxSize,
        uiscale = uiscale,
        page = settingsMenu.page,
        x = contentX + contentW - math.floor(settingsMenu.baseSizes.checkboxSize * uiscale),
        y = rowY2,
        default = settingsMenu.currentSettings.fullscreen,
        onChange = function(checked, id, name)
            settingsMenu.pendingSettings.fullscreen = checked
            engine.logInfo("Fullscreen pending: " .. tostring(checked))
        end,
    })
    local cbHandle = checkbox.getElementHandle(settingsMenu.fullscreenCheckboxId)
    
    -- Row 3: UI Scaling
    settingsMenu.scalingLabelId = label.new({
        name = "scaling_label",
        text = "UI Scaling",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 2,
    })
    local scLabelHandle = label.getElementHandle(settingsMenu.scalingLabelId)
    UI.addToPage(settingsMenu.page, scLabelHandle, contentX, rowY3 + s.fontSize)
    
    local tbW_est = math.floor(settingsMenu.baseSizes.textboxWidth * uiscale)
    settingsMenu.uiScaleTextBox = textbox.new({
        name = "uiscale_input",
        width = settingsMenu.baseSizes.textboxWidth,
        height = settingsMenu.baseSizes.textboxHeight,
        page = settingsMenu.page,
        x = contentX + contentW - tbW_est,
        y = rowY3,
        uiscale = uiscale,
        font = settingsMenu.menuFont,
        fontSize = 24,
        default = tostring(settingsMenu.currentSettings.uiScale),
        textType = textbox.Type.SCALE,
        zIndex = baseZ + 2
    })
    local tbHandle = textbox.getElementHandle(settingsMenu.uiScaleTextBox)
    
    -- Row 4: Frame Limit
    settingsMenu.frameLimitLabelId = label.new({
        name = "framelimit_label",
        text = "Frame Limit",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 2,
    })
    local frLabelHandle = label.getElementHandle(settingsMenu.frameLimitLabelId)
    UI.addToPage(settingsMenu.page, frLabelHandle, contentX, rowY4 + s.fontSize)
    
    local flW_est = math.floor(settingsMenu.baseSizes.textboxWidth * uiscale)
    settingsMenu.frameLimitTextBox = textbox.new({
        name = "framelimit_input",
        width = settingsMenu.baseSizes.textboxWidth,
        height = settingsMenu.baseSizes.textboxHeight,
        page = settingsMenu.page,
        x = contentX + contentW - flW_est,
        y = rowY4,
        uiscale = uiscale,
        font = settingsMenu.menuFont,
        fontSize = 24,
        default = tostring(settingsMenu.currentSettings.frameLimit or 60),
        textType = textbox.Type.NUMBER,
        zIndex = baseZ + 2,
    })
    local flHandle = textbox.getElementHandle(settingsMenu.frameLimitTextBox)
    
    -- Track all graphics tab elements for show/hide
    settingsMenu.tabContent.graphics = {
        resLabelHandle,
        dropdown.getElementHandle(settingsMenu.resolutionDropdownId),
        dropdown.getArrowHandle(settingsMenu.resolutionDropdownId),
        flLabelHandle,
        cbHandle,
        scLabelHandle,
        tbHandle,
        frLabelHandle,
        flHandle,
    }
end

function settingsMenu.createSystemTab(contentX, contentY, contentW, s, baseZ, uiscale)
    local placeholderLabel = label.new({
        name = "system_placeholder",
        text = "System settings coming soon...",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {0.7, 0.7, 0.7, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 2,
    })
    local handle = label.getElementHandle(placeholderLabel)
    UI.addToPage(settingsMenu.page, handle, contentX, contentY + s.fontSize)
    
    settingsMenu.tabContent.system = { handle }
end

function settingsMenu.createInputTab(contentX, contentY, contentW, s, baseZ, uiscale)
    local placeholderLabel = label.new({
        name = "input_placeholder",
        text = "Input settings coming soon...",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {0.7, 0.7, 0.7, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
        zIndex = baseZ + 2,
    })
    local handle = label.getElementHandle(placeholderLabel)
    UI.addToPage(settingsMenu.page, handle, contentX, contentY + s.fontSize)
    
    settingsMenu.tabContent.input = { handle }
end

-----------------------------------------------------------
-- Tab Switching
-----------------------------------------------------------

function settingsMenu.onTabChanged(key)
    settingsMenu.activeTab = key
    settingsMenu.showTab(key)
end

function settingsMenu.showTab(key)
    for tabKey, _ in pairs(settingsMenu.tabContent) do
        local visible = (tabKey == key)
        
        if tabKey == "graphics" then
            if settingsMenu.resolutionLabelId then
                label.setVisible(settingsMenu.resolutionLabelId, visible)
            end
            if settingsMenu.resolutionDropdownId then
                dropdown.setVisible(settingsMenu.resolutionDropdownId, visible)
            end
            if settingsMenu.fullscreenLabelId then
                label.setVisible(settingsMenu.fullscreenLabelId, visible)
            end
            if settingsMenu.fullscreenCheckboxId then
                checkbox.setVisible(settingsMenu.fullscreenCheckboxId, visible)
            end
            if settingsMenu.scalingLabelId then
                label.setVisible(settingsMenu.scalingLabelId, visible)
            end
            if settingsMenu.uiScaleTextBox then
                textbox.setVisible(settingsMenu.uiScaleTextBox, visible)
            end
            if settingsMenu.frameLimitLabelId then
                label.setVisible(settingsMenu.frameLimitLabelId, visible)
            end
            if settingsMenu.frameLimitTextBox then
                textbox.setVisible(settingsMenu.frameLimitTextBox, visible)
            end
        elseif tabKey == "system" then
            for _, handle in ipairs(settingsMenu.tabContent.system) do
                UI.setVisible(handle, visible)
            end
        elseif tabKey == "input" then
            for _, handle in ipairs(settingsMenu.tabContent.input) do
                UI.setVisible(handle, visible)
            end
        end
    end
end

-----------------------------------------------------------
-- Settings Logic (unchanged)
-----------------------------------------------------------

function settingsMenu.getSettings()
    return settingsMenu.currentSettings
end

function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    
    local scaleChanged = false
    local resolutionChanged = false
    
    if settingsMenu.pendingSettings.width ~= settingsMenu.currentSettings.width
        or settingsMenu.pendingSettings.height ~= settingsMenu.currentSettings.height then
        settingsMenu.currentSettings.width = settingsMenu.pendingSettings.width
        settingsMenu.currentSettings.height = settingsMenu.pendingSettings.height
        resolutionChanged = true
        engine.logInfo("Resolution applied: " .. settingsMenu.currentSettings.width
            .. "x" .. settingsMenu.currentSettings.height)
    end
    
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
    
    if resolutionChanged then
        engine.setResolution(
            settingsMenu.currentSettings.width,
            settingsMenu.currentSettings.height
        )
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
    local resChanged = (settingsMenu.currentSettings.width ~= w or settingsMenu.currentSettings.height ~= h)
    
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
    
    if resChanged then
        engine.setResolution(w, h)
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
    dropdown.destroyAll()
    tabbar.destroyAll()
    panel.destroyAll()
    if settingsMenu.page then
        UI.deletePage(settingsMenu.page)
    end
end

return settingsMenu
