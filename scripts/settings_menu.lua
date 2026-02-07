-- Settings Menu Module
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local button = require("scripts.ui.button")
local dropdown = require("scripts.ui.dropdown")
local tabbar = require("scripts.ui.tabbar")
local scrollbar = require("scripts.ui.scrollbar")
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

-----------------------------------------------------------
-- Z-Index Plan (explicit, all page-level root elements)
--
-- Rendering rule: within a LayerId, sprites draw first,
-- then text on top. To get text above a sprite, the text
-- must be in a HIGHER LayerId.
--
-- Effective render LayerId = pageBaseLayer + elementZIndex
--   pageBaseLayer for modal page = 20
--
-- Layer map (keep gaps for future use):
--   Z_PANEL       = 1    -- panel background box (sprites)
--   Z_TITLE       = 2    -- title text
--   Z_TAB_FRAME   = 3    -- tab content frame box (sprites)
--   Z_TAB_BUTTONS = 4    -- tab bar buttons (sprites)
--   Z_CONTENT     = 5    -- tab content labels (text)
--   Z_WIDGETS     = 6    -- tab content widgets (sprites+text)
--   Z_SB_TRACK    = 7    -- scrollbar track (sprites)
--   Z_SB_BUTTONS  = 8    -- scrollbar up/down (sprites)
--   Z_SB_TAB      = 9    -- scrollbar thumb (sprites)
--   Z_BUTTONS     = 10   -- bottom buttons (sprites)
--   (button text is child z=1, so effective = 11)
-----------------------------------------------------------
local Z_PANEL       = 1
local Z_TITLE       = 2
local Z_TAB_FRAME   = 3
local Z_TAB_BUTTONS = 4
local Z_CONTENT     = 5
local Z_WIDGETS     = 6
local Z_SB_TRACK    = 7
local Z_SB_BUTTONS  = 8
local Z_SB_TAB      = 9
local Z_BUTTONS     = 10

settingsMenu.uiCreated = false
settingsMenu.tabBarId = nil
settingsMenu.activeTab = "graphics"

-- Tab content element handles (for show/hide)
settingsMenu.tabContent = {
    system = {},
    graphics = {},
    input = {},
}

-- Per-tab scroll state
settingsMenu.tabScroll = {
    graphics = { scrollbarId = nil, scrollOffset = 0, totalRows = 0,
                 maxVisibleRows = 0, rowHandles = {}, contentX = 0,
                 contentY = 0, contentW = 0, rowSpacing = 0, fontSize = 0 },
    system   = { scrollbarId = nil, scrollOffset = 0, totalRows = 0,
                 maxVisibleRows = 0, rowHandles = {}, contentX = 0,
                 contentY = 0, contentW = 0, rowSpacing = 0, fontSize = 0 },
    input    = { scrollbarId = nil, scrollOffset = 0, totalRows = 0,
                 maxVisibleRows = 0, rowHandles = {}, contentX = 0,
                 contentY = 0, contentW = 0, rowSpacing = 0, fontSize = 0 },
}

-- Frame bounds cached for scroll hit-testing
settingsMenu.frameBounds = { x = 0, y = 0, w = 0, h = 0 }

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
    scrollbar.init()
    
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
    
    -- Destroy any tab scrollbars
    for _, ts in pairs(settingsMenu.tabScroll) do
        if ts.scrollbarId then
            scrollbar.destroy(ts.scrollbarId)
        end
    end
    
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
    settingsMenu.tabScroll = {
        graphics = { scrollbarId = nil, scrollOffset = 0, totalRows = 0,
                     maxVisibleRows = 0, rowHandles = {}, contentX = 0,
                     contentY = 0, contentW = 0, rowSpacing = 0, fontSize = 0 },
        system   = { scrollbarId = nil, scrollOffset = 0, totalRows = 0,
                     maxVisibleRows = 0, rowHandles = {}, contentX = 0,
                     contentY = 0, contentW = 0, rowSpacing = 0, fontSize = 0 },
        input    = { scrollbarId = nil, scrollOffset = 0, totalRows = 0,
                     maxVisibleRows = 0, rowHandles = {}, contentX = 0,
                     contentY = 0, contentW = 0, rowSpacing = 0, fontSize = 0 },
    }
    
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
        zIndex = Z_PANEL,
        padding = { top = 80, bottom = 120, left = 60, right = 60 },
        uiscale = uiscale,
    })
    
    local bounds = panel.getContentBounds(settingsMenu.panelId)
    
    ---------------------------------------------------------
    -- Title (page-level, not panel child)
    ---------------------------------------------------------
    settingsMenu.titleLabelId = label.new({
        name = "settings_title",
        text = "Settings",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    
    local titleW, titleH = label.getSize(settingsMenu.titleLabelId)
    local titleHandle = label.getElementHandle(settingsMenu.titleLabelId)
    local titleX = panelX + bounds.x + (bounds.width - titleW) / 2
    local titleY = panelY + bounds.y + s.fontSize
    UI.addToPage(settingsMenu.page, titleHandle, titleX, titleY)
    UI.setZIndex(titleHandle, Z_TITLE)
    
    ---------------------------------------------------------
    -- Tab bar
    ---------------------------------------------------------
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
        zIndex = Z_TAB_FRAME,
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
    
    -- Explicitly set z-index on frame and tab boxes
    -- tabbar.new sets frame to zIndex and tab boxes to zIndex+1
    -- That gives us frame=Z_TAB_FRAME=14, tab boxes=15
    -- The tab box text children are set to z=1 internally, 
    -- so they render at 20+15+1=36 which is fine
    
    -- Select the active tab
    tabbar.selectByKey(settingsMenu.tabBarId, settingsMenu.activeTab)
    
    -- Get frame bounds for content placement
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(settingsMenu.tabBarId)
    local contentPadding = math.floor(20 * uiscale)
    local contentX = frameX + contentPadding
    local contentY = frameY + contentPadding
    local contentW = frameW - (contentPadding * 2)
    local contentH = frameH - (contentPadding * 2)
    
    -- Make the tab frame clickable so it receives scroll events
    local frameHandle = tabbar.getFrameHandle(settingsMenu.tabBarId)
    UI.setClickable(frameHandle, true)
    UI.setOnClick(frameHandle, "onTabFrameScroll")
    
    -- Cache frame bounds for scroll hit-testing
    settingsMenu.frameBounds = { x = frameX, y = frameY, w = frameW, h = frameH }
    
    -- Compute maxVisibleRows from available content height
    local maxVisibleRows = math.floor(contentH / s.rowSpacing)
    if maxVisibleRows < 1 then maxVisibleRows = 1 end
    
    -- Build graphics tab content
    settingsMenu.createGraphicsTab(contentX, contentY, contentW, contentH,
        s, uiscale, maxVisibleRows)
    
    -- Build system tab content (placeholder)
    settingsMenu.createSystemTab(contentX, contentY, contentW, contentH,
        s, uiscale, maxVisibleRows)
    
    -- Build input tab content (placeholder)
    settingsMenu.createInputTab(contentX, contentY, contentW, contentH,
        s, uiscale, maxVisibleRows)
    
    ---------------------------------------------------------
    -- Bottom buttons (page-level, NOT panel children)
    ---------------------------------------------------------
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
        zIndex = Z_BUTTONS,
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
        zIndex = Z_BUTTONS,
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
        zIndex = Z_BUTTONS,
        onClick = function(id, name)
            settingsMenu.onSave()
        end,
    })
    
    -- Position buttons manually at bottom of panel (page-level coordinates)
    local backW, backH = button.getSize(settingsMenu.backButtonId)
    local applyW, applyH = button.getSize(settingsMenu.applyButtonId)
    local saveW, saveH = button.getSize(settingsMenu.saveButtonId)
    
    local totalBtnWidth = backW + s.btnSpacing + applyW + s.btnSpacing + saveW
    local btnStartX = panelX + bounds.x + (bounds.width - totalBtnWidth) / 2
    local btnY = panelY + panelHeight - bounds.y - backH
    -- bounds.y here is padding.top; for bottom we use the actual bottom padding
    -- The panel bottom padding is 120*uiscale. bounds.y = padding.top = 80*uiscale
    -- So the button row sits at:
    local bottomPadding = math.floor(120 * uiscale)
    btnY = panelY + panelHeight - bottomPadding + (bottomPadding - backH) / 2
    
    -- buttons are already addToPage'd by button.new (since no parent is given)
    -- We just need to set their positions
    local backHandle = button.getElementHandle(settingsMenu.backButtonId)
    local applyHandle = button.getElementHandle(settingsMenu.applyButtonId)
    local saveHandle = button.getElementHandle(settingsMenu.saveButtonId)
    
    UI.setPosition(backHandle, btnStartX, btnY)
    UI.setPosition(applyHandle, btnStartX + backW + s.btnSpacing, btnY)
    UI.setPosition(saveHandle, btnStartX + backW + s.btnSpacing + applyW + s.btnSpacing, btnY)
    
    -- Explicitly ensure button z-index (button.new already sets it via params.zIndex)
    UI.setZIndex(backHandle, Z_BUTTONS)
    UI.setZIndex(applyHandle, Z_BUTTONS)
    UI.setZIndex(saveHandle, Z_BUTTONS)
    
    engine.logDebug("Button z-indices set to " .. Z_BUTTONS
        .. " | backHandle=" .. tostring(backHandle)
        .. " applyHandle=" .. tostring(applyHandle)
        .. " saveHandle=" .. tostring(saveHandle))
    
    -- Show only the active tab's content
    settingsMenu.showTab(settingsMenu.activeTab)
    
    settingsMenu.uiCreated = true
end

-----------------------------------------------------------
-- Tab Scrolling Helpers
-----------------------------------------------------------

-- Reposition visible rows and hide off-screen rows for a tab
function settingsMenu.refreshTabScroll(tabKey)
    local ts = settingsMenu.tabScroll[tabKey]
    if not ts then return end
    
    for i, row in ipairs(ts.rowHandles) do
        local visibleIndex = i - ts.scrollOffset  -- 1-based slot in visible area
        if visibleIndex >= 1 and visibleIndex <= ts.maxVisibleRows then
            -- Position this row at the visible slot
            local rowY = ts.contentY + (visibleIndex - 1) * ts.rowSpacing
            -- Reposition the label
            if row.labelHandle then
                UI.setPosition(row.labelHandle, ts.contentX, rowY + ts.fontSize)
                UI.setVisible(row.labelHandle, true)
            end
            -- Reposition the widget
            if row.widgetSetPosition then
                row.widgetSetPosition(rowY)
            end
            if row.widgetSetVisible then
                row.widgetSetVisible(true)
            end
        else
            -- Hide this row
            if row.labelHandle then
                UI.setVisible(row.labelHandle, false)
            end
            if row.widgetSetVisible then
                row.widgetSetVisible(false)
            end
        end
    end
end

-- Callback from scrollbar when tab content is scrolled
function settingsMenu.onTabScroll(tabKey, offset)
    local ts = settingsMenu.tabScroll[tabKey]
    if not ts then return end
    ts.scrollOffset = offset
    settingsMenu.refreshTabScroll(tabKey)
end

-- Create a scrollbar for a tab if it needs one
function settingsMenu.createTabScrollbar(tabKey, frameX, frameY, frameW, frameH,
                                          totalRows, maxVisibleRows, uiscale, s)
    local ts = settingsMenu.tabScroll[tabKey]
    ts.totalRows = totalRows
    ts.maxVisibleRows = maxVisibleRows
    ts.fontSize = s.fontSize
    
    if totalRows <= maxVisibleRows then
        -- No scrollbar needed
        return
    end
    
    -- Scrollbar along the right edge of the tab frame
    local sbButtonSize = math.floor(24 * uiscale)
    local sbCapHeight = math.floor(4 * uiscale)
    local sbTrackHeight = frameH - (sbButtonSize * 2) - (sbCapHeight * 2)
    if sbTrackHeight < math.floor(20 * uiscale) then
        sbTrackHeight = math.floor(20 * uiscale)
    end
    
    local sbX = frameX + frameW  -- just outside the right edge of the frame
    local sbY = frameY
    
    ts.scrollbarId = scrollbar.new({
        name = "tab_" .. tabKey .. "_scrollbar",
        page = settingsMenu.page,
        x = sbX,
        y = sbY,
        buttonSize = sbButtonSize,
        trackHeight = sbTrackHeight,
        capHeight = sbCapHeight,
        tileSize = math.floor(8 * uiscale),
        totalItems = totalRows,
        visibleItems = maxVisibleRows,
        uiscale = uiscale,
        onScroll = function(offset, sbId, sbName)
            settingsMenu.onTabScroll(tabKey, offset)
        end,
    })
    
    -- Override scrollbar z-indices to use our explicit scheme
    -- (scrollbar.new sets 500/501/502 by default which is excessive)
    scrollbar.setZIndices(ts.scrollbarId, Z_SB_TRACK, Z_SB_BUTTONS, Z_SB_TAB)
    
    engine.logDebug("Tab scrollbar created for '" .. tabKey
        .. "' totalRows=" .. totalRows
        .. " maxVisible=" .. maxVisibleRows)
end

-----------------------------------------------------------
-- Tab Content Creation
-----------------------------------------------------------

function settingsMenu.createGraphicsTab(contentX, contentY, contentW, contentH,
                                         s, uiscale, maxVisibleRows)
    local ts = settingsMenu.tabScroll.graphics
    ts.contentX = contentX
    ts.contentY = contentY
    ts.contentW = contentW
    ts.rowSpacing = s.rowSpacing
    ts.scrollOffset = 0
    ts.rowHandles = {}
    
    -- Row 1: Resolution
    settingsMenu.resolutionLabelId = label.new({
        name = "resolution_label",
        text = "Resolution",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    local resLabelHandle = label.getElementHandle(settingsMenu.resolutionLabelId)
    UI.addToPage(settingsMenu.page, resLabelHandle, contentX, contentY + s.fontSize)
    UI.setZIndex(resLabelHandle, Z_CONTENT)
    
    local currentRes = settingsMenu.resolutionString(
        settingsMenu.currentSettings.width,
        settingsMenu.currentSettings.height
    )
    
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
        zIndex = Z_WIDGETS,
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
        contentX + contentW - ddW, contentY)
    
    local ddId = settingsMenu.resolutionDropdownId
    table.insert(ts.rowHandles, {
        labelHandle = resLabelHandle,
        widgetSetPosition = function(rowY)
            dropdown.setPosition(ddId, contentX + contentW - ddW, rowY)
        end,
        widgetSetVisible = function(vis)
            dropdown.setVisible(ddId, vis)
        end,
    })
    
    -- Row 2: Fullscreen
    settingsMenu.fullscreenLabelId = label.new({
        name = "fullscreen_label",
        text = "Fullscreen",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    local flLabelHandle = label.getElementHandle(settingsMenu.fullscreenLabelId)
    UI.addToPage(settingsMenu.page, flLabelHandle, contentX, contentY + s.rowSpacing + s.fontSize)
    UI.setZIndex(flLabelHandle, Z_CONTENT)
    
    settingsMenu.fullscreenCheckboxId = checkbox.new({
        name = "fullscreen",
        size = settingsMenu.baseSizes.checkboxSize,
        uiscale = uiscale,
        page = settingsMenu.page,
        x = contentX + contentW - math.floor(settingsMenu.baseSizes.checkboxSize * uiscale),
        y = contentY + s.rowSpacing,
        default = settingsMenu.currentSettings.fullscreen,
        zIndex = Z_WIDGETS,
        onChange = function(checked, id, name)
            settingsMenu.pendingSettings.fullscreen = checked
            engine.logInfo("Fullscreen pending: " .. tostring(checked))
        end,
    })
    local cbSize = math.floor(settingsMenu.baseSizes.checkboxSize * uiscale)
    local cbId = settingsMenu.fullscreenCheckboxId
    
    table.insert(ts.rowHandles, {
        labelHandle = flLabelHandle,
        widgetSetPosition = function(rowY)
            checkbox.setPosition(cbId,
                contentX + contentW - cbSize, rowY)
        end,
        widgetSetVisible = function(vis)
            checkbox.setVisible(cbId, vis)
        end,
    })
    
    -- Row 3: UI Scaling
    settingsMenu.scalingLabelId = label.new({
        name = "scaling_label",
        text = "UI Scaling",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    local scLabelHandle = label.getElementHandle(settingsMenu.scalingLabelId)
    UI.addToPage(settingsMenu.page, scLabelHandle, contentX, contentY + s.rowSpacing * 2 + s.fontSize)
    UI.setZIndex(scLabelHandle, Z_CONTENT)
    
    local tbW_est = math.floor(settingsMenu.baseSizes.textboxWidth * uiscale)
    settingsMenu.uiScaleTextBox = textbox.new({
        name = "uiscale_input",
        width = settingsMenu.baseSizes.textboxWidth,
        height = settingsMenu.baseSizes.textboxHeight,
        page = settingsMenu.page,
        x = contentX + contentW - tbW_est,
        y = contentY + s.rowSpacing * 2,
        uiscale = uiscale,
        font = settingsMenu.menuFont,
        fontSize = 24,
        default = tostring(settingsMenu.currentSettings.uiScale),
        textType = textbox.Type.SCALE,
        zIndex = Z_WIDGETS,
    })
    local uiScaleId = settingsMenu.uiScaleTextBox
    
    table.insert(ts.rowHandles, {
        labelHandle = scLabelHandle,
        widgetSetPosition = function(rowY)
            textbox.setPosition(uiScaleId,
                contentX + contentW - tbW_est, rowY)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(uiScaleId, vis)
        end,
    })
    
    -- Row 4: Frame Limit
    settingsMenu.frameLimitLabelId = label.new({
        name = "framelimit_label",
        text = "Frame Limit",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    local frLabelHandle = label.getElementHandle(settingsMenu.frameLimitLabelId)
    UI.addToPage(settingsMenu.page, frLabelHandle, contentX, contentY + s.rowSpacing * 3 + s.fontSize)
    UI.setZIndex(frLabelHandle, Z_CONTENT)
    
    local flW_est = math.floor(settingsMenu.baseSizes.textboxWidth * uiscale)
    settingsMenu.frameLimitTextBox = textbox.new({
        name = "framelimit_input",
        width = settingsMenu.baseSizes.textboxWidth,
        height = settingsMenu.baseSizes.textboxHeight,
        page = settingsMenu.page,
        x = contentX + contentW - flW_est,
        y = contentY + s.rowSpacing * 3,
        uiscale = uiscale,
        font = settingsMenu.menuFont,
        fontSize = 24,
        default = tostring(settingsMenu.currentSettings.frameLimit or 60),
        textType = textbox.Type.NUMBER,
        zIndex = Z_WIDGETS,
    })
    local flId = settingsMenu.frameLimitTextBox
    
    table.insert(ts.rowHandles, {
        labelHandle = frLabelHandle,
        widgetSetPosition = function(rowY)
            textbox.setPosition(flId,
                contentX + contentW - flW_est, rowY)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(flId, vis)
        end,
    })
    
    -- Create scrollbar if rows exceed visible area
    local totalRows = #ts.rowHandles
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(settingsMenu.tabBarId)
    settingsMenu.createTabScrollbar("graphics", frameX, frameY, frameW, frameH,
        totalRows, maxVisibleRows, uiscale, s)
    
    -- Initial scroll positioning
    settingsMenu.refreshTabScroll("graphics")
end

function settingsMenu.createSystemTab(contentX, contentY, contentW, contentH,
                                       s, uiscale, maxVisibleRows)
    local ts = settingsMenu.tabScroll.system
    ts.contentX = contentX
    ts.contentY = contentY
    ts.contentW = contentW
    ts.rowSpacing = s.rowSpacing
    ts.scrollOffset = 0
    ts.rowHandles = {}
    
    local placeholderLabel = label.new({
        name = "system_placeholder",
        text = "System settings coming soon...",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {0.7, 0.7, 0.7, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    local handle = label.getElementHandle(placeholderLabel)
    UI.addToPage(settingsMenu.page, handle, contentX, contentY + s.fontSize)
    UI.setZIndex(handle, Z_CONTENT)
    
    table.insert(ts.rowHandles, {
        labelHandle = handle,
        widgetSetPosition = nil,
        widgetSetVisible = nil,
    })
    
    settingsMenu.tabContent.system = { handle }
    
    local totalRows = #ts.rowHandles
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(settingsMenu.tabBarId)
    settingsMenu.createTabScrollbar("system", frameX, frameY, frameW, frameH,
        totalRows, maxVisibleRows, uiscale, s)
    settingsMenu.refreshTabScroll("system")
end

function settingsMenu.createInputTab(contentX, contentY, contentW, contentH,
                                      s, uiscale, maxVisibleRows)
    local ts = settingsMenu.tabScroll.input
    ts.contentX = contentX
    ts.contentY = contentY
    ts.contentW = contentW
    ts.rowSpacing = s.rowSpacing
    ts.scrollOffset = 0
    ts.rowHandles = {}
    
    local placeholderLabel = label.new({
        name = "input_placeholder",
        text = "Input settings coming soon...",
        font = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color = {0.7, 0.7, 0.7, 1.0},
        page = settingsMenu.page,
        uiscale = uiscale,
    })
    local handle = label.getElementHandle(placeholderLabel)
    UI.addToPage(settingsMenu.page, handle, contentX, contentY + s.fontSize)
    UI.setZIndex(handle, Z_CONTENT)
    
    table.insert(ts.rowHandles, {
        labelHandle = handle,
        widgetSetPosition = nil,
        widgetSetVisible = nil,
    })
    
    settingsMenu.tabContent.input = { handle }
    
    local totalRows = #ts.rowHandles
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(settingsMenu.tabBarId)
    settingsMenu.createTabScrollbar("input", frameX, frameY, frameW, frameH,
        totalRows, maxVisibleRows, uiscale, s)
    settingsMenu.refreshTabScroll("input")
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
        local ts = settingsMenu.tabScroll[tabKey]
        
        if visible and ts then
            settingsMenu.refreshTabScroll(tabKey)
        else
            if ts then
                for _, row in ipairs(ts.rowHandles) do
                    if row.labelHandle then
                        UI.setVisible(row.labelHandle, false)
                    end
                    if row.widgetSetVisible then
                        row.widgetSetVisible(false)
                    end
                end
            end
        end
        
        -- Show/hide the tab's scrollbar
        if ts and ts.scrollbarId then
            scrollbar.setVisible(ts.scrollbarId, visible)
        end
    end
end

-----------------------------------------------------------
-- Scroll event handling (called from uiManager)
-----------------------------------------------------------

function settingsMenu.onScroll(elemHandle, dx, dy)
    local activeTs = settingsMenu.tabScroll[settingsMenu.activeTab]
    if not activeTs or not activeTs.scrollbarId then return false end
    
    local function doScroll()
        if dy > 0 then
            scrollbar.scrollUp(activeTs.scrollbarId)
        elseif dy < 0 then
            scrollbar.scrollDown(activeTs.scrollbarId)
        end
        return true
    end
    
    -- Check if the element is the tab frame itself
    local frameHandle = tabbar.getFrameHandle(settingsMenu.tabBarId)
    if elemHandle == frameHandle then
        return doScroll()
    end
    
    -- Check if the element belongs to any of the visible tab content rows
    for _, row in ipairs(activeTs.rowHandles) do
        if row.labelHandle == elemHandle then
            return doScroll()
        end
    end
    
    -- Check if the element is part of the tab scrollbar itself
    local sbId, _ = scrollbar.findByElementHandle(elemHandle)
    if sbId and sbId == activeTs.scrollbarId then
        return doScroll()
    end
    
    -- NOTE: removed the mouse-position fallback that was here before.
    -- It was too broad — it caught scroll events over dropdown option
    -- lists that physically overlap the tab frame bounds.
    
    return false
end

-- Handle scrollbar button clicks dispatched by uiManager
function settingsMenu.handleScrollCallback(callbackName, elemHandle)
    for tabKey, ts in pairs(settingsMenu.tabScroll) do
        if ts.scrollbarId then
            local sbId, _ = scrollbar.findByElementHandle(elemHandle)
            if sbId and sbId == ts.scrollbarId then
                if callbackName == "onScrollUp" then
                    scrollbar.scrollUp(sbId)
                    return true
                elseif callbackName == "onScrollDown" then
                    scrollbar.scrollDown(sbId)
                    return true
                end
            end
        end
    end
    return false
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
    -- Destroy tab scrollbars
    for _, ts in pairs(settingsMenu.tabScroll) do
        if ts.scrollbarId then
            scrollbar.destroy(ts.scrollbarId)
        end
    end
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
