-- Settings Menu Module
-- Orchestrates the settings page: panel, title, tab bar, buttons,
-- scroll infrastructure. Delegates tab content to tab modules and
-- settings state to data module.
local scale          = require("scripts.ui.scale")
local panel          = require("scripts.ui.panel")
local label          = require("scripts.ui.label")
local textbox        = require("scripts.ui.textbox")
local checkbox       = require("scripts.ui.checkbox")
local button         = require("scripts.ui.button")
local dropdown       = require("scripts.ui.dropdown")
local tabbar         = require("scripts.ui.tabbar")
local scrollbar      = require("scripts.ui.scrollbar")
local slider         = require("scripts.ui.slider")
local data           = require("scripts.settings.data")
local graphicsTab    = require("scripts.settings.graphics_tab")
local placeholderTab = require("scripts.settings.placeholder_tab")

local settingsMenu = {}

-----------------------------------------------------------
-- Z-Index Plan
--
-- Effective render LayerId = pageBaseLayer + elementZIndex
--   pageBaseLayer for modal page = 20
--
--   Z_PANEL       = 1    panel background box (sprites)
--   Z_TITLE       = 2    title text
--   Z_TAB_FRAME   = 3    tab content frame box (sprites)
--   Z_TAB_BUTTONS = 4    tab bar buttons (sprites)
--   Z_CONTENT     = 5    tab content labels (text)
--   Z_WIDGETS     = 6    tab content widgets (sprites+text)
--   Z_SB_TRACK    = 7    scrollbar track (sprites)
--   Z_SB_BUTTONS  = 8    scrollbar up/down (sprites)
--   Z_SB_TAB      = 9    scrollbar thumb (sprites)
--   Z_BUTTONS     = 10   bottom buttons (sprites)
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

-----------------------------------------------------------
-- Base sizes (unscaled)
-----------------------------------------------------------
settingsMenu.baseSizes = {
    fontSize       = 24,
    checkboxSize   = 36,
    btnWidth       = 200,
    btnHeight      = 52,
    textboxWidth   = 150,
    textboxHeight  = 36,
    dropdownHeight = 36,
    sliderWidth    = 200,
    sliderHeight   = 20,
    rowSpacing     = 56,
    btnSpacing     = 16,
    tabHeight      = 32,
    tabFontSize    = 20,
}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
settingsMenu.page         = nil
settingsMenu.panelId      = nil
settingsMenu.panelTexSet  = nil
settingsMenu.buttonTexSet = nil
settingsMenu.menuFont     = nil
settingsMenu.fbW          = 0
settingsMenu.fbH          = 0
settingsMenu.uiCreated    = false
settingsMenu.tabBarId     = nil
settingsMenu.activeTab    = "graphics"
settingsMenu.showMenuCallback = nil

-- Per-tab scroll state
settingsMenu.tabScroll = {}

-- Button widget IDs
settingsMenu.backButtonId  = nil
settingsMenu.applyButtonId = nil
settingsMenu.saveButtonId  = nil
settingsMenu.defaultsButtonId = nil

-- Owned element IDs for scoped cleanup
settingsMenu.ownedLabels     = {}
settingsMenu.ownedTextboxes  = {}
settingsMenu.ownedCheckboxes = {}
settingsMenu.ownedButtons    = {}
settingsMenu.ownedDropdowns  = {}
settingsMenu.ownedPanels     = {}
settingsMenu.ownedTabbars    = {}
settingsMenu.ownedSliders    = {}

-----------------------------------------------------------
-- Tab registry
-- Each entry: { key, name, createFn(params) → rowHandles[] }
-----------------------------------------------------------
local tabDefs = {
    { key = "system",   name = "System",   create = function(p)
        return placeholderTab.create({
            name = "system_placeholder",
            text = "System settings coming soon...",
            page = p.page, font = p.font, baseSizes = p.baseSizes,
            uiscale = p.uiscale, s = p.s,
            contentX = p.contentX, contentY = p.contentY,
            zContent = Z_CONTENT,
        })
    end },
    { key = "graphics", name = "Graphics", create = function(p)
        return graphicsTab.create(p)
    end },
    { key = "input",    name = "Input",    create = function(p)
        return placeholderTab.create({
            name = "input_placeholder",
            text = "Input settings coming soon...",
            page = p.page, font = p.font, baseSizes = p.baseSizes,
            uiscale = p.uiscale, s = p.s,
            contentX = p.contentX, contentY = p.contentY,
            zContent = Z_CONTENT,
        })
    end },
}

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function emptyScrollState()
    return {
        scrollbarId    = nil,
        scrollOffset   = 0,
        totalRows      = 0,
        maxVisibleRows = 0,
        rowHandles     = {},
        contentX       = 0,
        contentY       = 0,
        contentW       = 0,
        rowSpacing     = 0,
        fontSize       = 0,
    }
end

local function resetScrollStates()
    settingsMenu.tabScroll = {}
    for _, def in ipairs(tabDefs) do
        settingsMenu.tabScroll[def.key] = emptyScrollState()
    end
end

function settingsMenu.onDefaults()
    engine.logInfo("Loading defaults...")
    data.loadDefaults()
    settingsMenu.createUI()
    if settingsMenu.page then UI.showPage(settingsMenu.page) end
end

-----------------------------------------------------------
-- Scoped cleanup: destroy only elements we created
-----------------------------------------------------------

function settingsMenu.destroyOwned()
    for _, id in ipairs(settingsMenu.ownedLabels)     do label.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedTextboxes)   do textbox.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedCheckboxes)  do checkbox.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedButtons)     do button.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedDropdowns)   do dropdown.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedPanels)      do panel.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedTabbars)     do tabbar.destroy(id) end
    for _, id in ipairs(settingsMenu.ownedSliders)     do slider.destroy(id) end

    settingsMenu.ownedLabels     = {}
    settingsMenu.ownedTextboxes  = {}
    settingsMenu.ownedCheckboxes = {}
    settingsMenu.ownedButtons    = {}
    settingsMenu.ownedDropdowns  = {}
    settingsMenu.ownedPanels     = {}
    settingsMenu.ownedTabbars    = {}
    settingsMenu.ownedSliders    = {}
end

-- Tracking helpers — call after every widget .new()
function settingsMenu.trackLabel(id)
    table.insert(settingsMenu.ownedLabels, id)
    return id
end
function settingsMenu.trackTextbox(id)
    table.insert(settingsMenu.ownedTextboxes, id)
    return id
end
function settingsMenu.trackCheckbox(id)
    table.insert(settingsMenu.ownedCheckboxes, id)
    return id
end
function settingsMenu.trackButton(id)
    table.insert(settingsMenu.ownedButtons, id)
    return id
end
function settingsMenu.trackDropdown(id)
    table.insert(settingsMenu.ownedDropdowns, id)
    return id
end
function settingsMenu.trackPanel(id)
    table.insert(settingsMenu.ownedPanels, id)
    return id
end
function settingsMenu.trackTabbar(id)
    table.insert(settingsMenu.ownedTabbars, id)
    return id
end
function settingsMenu.trackSlider(id)
    table.insert(settingsMenu.ownedSliders, id)
    return id
end

-----------------------------------------------------------
-- Public: callbacks from ui_manager
-----------------------------------------------------------

function settingsMenu.setShowMenuCallback(callback)
    settingsMenu.showMenuCallback = callback
end

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function settingsMenu.init(panelTex, btnTex, font, width, height)
    settingsMenu.panelTexSet  = panelTex
    settingsMenu.buttonTexSet = btnTex
    settingsMenu.menuFont     = font
    settingsMenu.fbW          = width
    settingsMenu.fbH          = height

    data.current.uiScale = scale.get()

    textbox.init()
    checkbox.init()
    dropdown.init()
    tabbar.init()
    scrollbar.init()
    slider.init()

    data.reload()
    settingsMenu.createUI()
end

-----------------------------------------------------------
-- Full UI rebuild
-----------------------------------------------------------

function settingsMenu.createUI()
    -- Tear down owned elements only (not global destroyAll)
    settingsMenu.destroyOwned()

    -- Destroy tab scrollbars
    for _, ts in pairs(settingsMenu.tabScroll) do
        if ts.scrollbarId then scrollbar.destroy(ts.scrollbarId) end
    end

    settingsMenu.backButtonId  = nil
    settingsMenu.applyButtonId = nil
    settingsMenu.saveButtonId  = nil
    settingsMenu.panelId       = nil
    settingsMenu.tabBarId      = nil
    resetScrollStates()

    if settingsMenu.uiCreated and settingsMenu.page then
        UI.deletePage(settingsMenu.page)
    end

    data.resetPending()

    local uiscale = data.current.uiScale
    local s = scale.applyAllWith(settingsMenu.baseSizes, uiscale)

    settingsMenu.page = UI.newPage("settings_menu", "modal")

    -- Panel
    local panelWidth  = math.floor(settingsMenu.fbW * 0.8)
    local panelHeight = math.floor(settingsMenu.fbH * 0.8)
    local panelX = (settingsMenu.fbW - panelWidth) / 2
    local panelY = (settingsMenu.fbH - panelHeight) / 2

    settingsMenu.panelId = settingsMenu.trackPanel(panel.new({
        name       = "settings_panel",
        page       = settingsMenu.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = settingsMenu.panelTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = 64,
        zIndex     = Z_PANEL,
        padding    = { top = 60, bottom = 100, left = 50, right = 50 },
        uiscale    = uiscale,
    }))
    local bounds = panel.getContentBounds(settingsMenu.panelId)

    -- Title
    settingsMenu.createTitle(panelX, panelY, bounds, s, uiscale)

    -- Tab bar
    settingsMenu.createTabBar(panelX, panelY, panelWidth, panelHeight,
        bounds, s, uiscale)

    -- Tab content
    settingsMenu.createAllTabs(s, uiscale)

    -- Bottom buttons
    settingsMenu.createButtons(panelX, panelY, panelWidth, panelHeight,
        bounds, s, uiscale)

    settingsMenu.showTab(settingsMenu.activeTab)
    settingsMenu.uiCreated = true
end

-----------------------------------------------------------
-- Title
-----------------------------------------------------------

function settingsMenu.createTitle(panelX, panelY, bounds, s, uiscale)
    local titleLabelId = settingsMenu.trackLabel(label.new({
        name     = "settings_title",
        text     = "Settings",
        font     = settingsMenu.menuFont,
        fontSize = settingsMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = settingsMenu.page,
        uiscale  = uiscale,
    }))
    local titleW, _ = label.getSize(titleLabelId)
    local titleHandle = label.getElementHandle(titleLabelId)
    local titleX = panelX + bounds.x + (bounds.width - titleW) / 2
    local titleY = panelY + bounds.y + s.fontSize
    UI.addToPage(settingsMenu.page, titleHandle, titleX, titleY)
    UI.setZIndex(titleHandle, Z_TITLE)
end

-----------------------------------------------------------
-- Tab bar
-----------------------------------------------------------

function settingsMenu.createTabBar(panelX, panelY, panelWidth, panelHeight,
                                    bounds, s, uiscale)
    local tabY = panelY + bounds.y + s.fontSize + math.floor(20 * uiscale)
    local tabFrameHeight = panelHeight - bounds.y - s.fontSize
        - math.floor(20 * uiscale) - s.tabHeight - s.btnHeight
        - math.floor(40 * uiscale) - bounds.y

    -- Build tabs array from tabDefs
    local tabList = {}
    for _, def in ipairs(tabDefs) do
        table.insert(tabList, { name = def.name, key = def.key })
    end

    settingsMenu.tabBarId = settingsMenu.trackTabbar(tabbar.new({
        name              = "settings_tabs",
        page              = settingsMenu.page,
        x                 = panelX + bounds.x,
        y                 = tabY,
        width             = bounds.width,
        font              = settingsMenu.menuFont,
        fontSize          = settingsMenu.baseSizes.tabFontSize,
        tabHeight         = settingsMenu.baseSizes.tabHeight,
        frameHeight       = tabFrameHeight,
        uiscale           = uiscale,
        zIndex            = Z_TAB_FRAME,
        textColor         = {0.0, 0.0, 0.0, 1.0},
        selectedTextColor = {1.0, 1.0, 1.0, 1.0},
        tabs              = tabList,
        onChange = function(key, index, tbId)
            settingsMenu.onTabChanged(key)
        end,
    }))

    tabbar.selectByKey(settingsMenu.tabBarId, settingsMenu.activeTab)

    -- Make frame clickable for scroll events
    local frameHandle = tabbar.getFrameHandle(settingsMenu.tabBarId)
    UI.setClickable(frameHandle, true)
    UI.setOnClick(frameHandle, "onTabFrameScroll")
end

-----------------------------------------------------------
-- Create all tab contents
-----------------------------------------------------------

function settingsMenu.createAllTabs(s, uiscale)
    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(settingsMenu.tabBarId)
    local pad = math.floor(20 * uiscale)
    local contentX = frameX + pad
    local contentY = frameY + pad
    local contentW = frameW - pad * 2
    local contentH = frameH - pad * 2

    local maxVisibleRows = math.max(1, math.floor(contentH / s.rowSpacing))

    for _, def in ipairs(tabDefs) do
        local ts      = settingsMenu.tabScroll[def.key]
        ts.contentX   = contentX
        ts.contentY   = contentY
        ts.contentW   = contentW
        ts.rowSpacing = s.rowSpacing
        ts.scrollOffset = 0

        -- Call the tab's create function
        ts.rowHandles = def.create({
            page            = settingsMenu.page,
            font            = settingsMenu.menuFont,
            baseSizes       = settingsMenu.baseSizes,
            uiscale         = uiscale,
            s               = s,
            contentX        = contentX,
            contentY        = contentY,
            contentW        = contentW,
            zContent        = Z_CONTENT,
            zWidgets        = Z_WIDGETS,
            currentSettings = data.current,
            pendingSettings = data.pending,
            -- Pass tracking functions so tabs can register their widgets
            trackLabel      = settingsMenu.trackLabel,
            trackTextbox    = settingsMenu.trackTextbox,
            trackCheckbox   = settingsMenu.trackCheckbox,
            trackDropdown   = settingsMenu.trackDropdown,
        })

        -- Scrollbar if needed
        local totalRows = #ts.rowHandles
        settingsMenu.createTabScrollbar(
            def.key, frameX, frameY, frameW, frameH,
            totalRows, maxVisibleRows, uiscale, s)

        settingsMenu.refreshTabScroll(def.key)
    end
end

-----------------------------------------------------------
-- Bottom buttons
-----------------------------------------------------------

function settingsMenu.createButtons(panelX, panelY, panelWidth, panelHeight,
                                     bounds, s, uiscale)
    settingsMenu.backButtonId = settingsMenu.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = settingsMenu.baseSizes.btnWidth,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = settingsMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = settingsMenu.page,
        font       = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            data.revert()
            if settingsMenu.showMenuCallback then
                settingsMenu.showMenuCallback("main")
            end
        end,
    }))

    settingsMenu.defaultsButtonId = settingsMenu.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = settingsMenu.baseSizes.btnWidth,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = settingsMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = settingsMenu.page,
        font       = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            settingsMenu.onDefaults()
        end,
    }))

    settingsMenu.applyButtonId = settingsMenu.trackButton(button.new({
        name       = "apply_btn",
        text       = "Apply",
        width      = settingsMenu.baseSizes.btnWidth,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = settingsMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = settingsMenu.page,
        font       = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            settingsMenu.onApply()
        end,
    }))

    settingsMenu.saveButtonId = settingsMenu.trackButton(button.new({
        name       = "save_btn",
        text       = "Save",
        width      = settingsMenu.baseSizes.btnWidth,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = settingsMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = settingsMenu.page,
        font       = settingsMenu.menuFont,
        textureSet = settingsMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            settingsMenu.onSave()
        end,
    }))

    local backW, backH   = button.getSize(settingsMenu.backButtonId)
    local defaultsW, _   = button.getSize(settingsMenu.defaultsButtonId)
    local applyW, _      = button.getSize(settingsMenu.applyButtonId)
    local saveW, _       = button.getSize(settingsMenu.saveButtonId)
    local totalBtnW      = backW + s.btnSpacing + defaultsW + s.btnSpacing 
                           + applyW + s.btnSpacing + saveW
    local btnStartX      = panelX + bounds.x + (bounds.width - totalBtnW) / 2
    local bottomPad      = math.floor(100 * uiscale)
    local btnY           = panelY + panelHeight - bottomPad
                           + (bottomPad - backH) / 2

    local backH_   = button.getElementHandle(settingsMenu.backButtonId)
    local defaultsH_   = button.getElementHandle(settingsMenu.defaultsButtonId)
    local applyH_  = button.getElementHandle(settingsMenu.applyButtonId)
    local saveH_   = button.getElementHandle(settingsMenu.saveButtonId)

    UI.setPosition(backH_,     btnStartX, btnY)
    UI.setPosition(defaultsH_, btnStartX + backW + s.btnSpacing, btnY)
    UI.setPosition(applyH_,    btnStartX + backW + s.btnSpacing 
                               + defaultsW + s.btnSpacing, btnY)
    UI.setPosition(saveH_,     btnStartX + backW + s.btnSpacing 
                               + defaultsW + s.btnSpacing 
                               + applyW + s.btnSpacing, btnY)
    
    UI.setZIndex(backH_,     Z_BUTTONS)
    UI.setZIndex(defaultsH_, Z_BUTTONS)
    UI.setZIndex(applyH_,    Z_BUTTONS)
    UI.setZIndex(saveH_,     Z_BUTTONS)
end

-----------------------------------------------------------
-- Tab scrolling
-----------------------------------------------------------

function settingsMenu.refreshTabScroll(tabKey)
    local ts = settingsMenu.tabScroll[tabKey]
    if not ts then return end

    for i, row in ipairs(ts.rowHandles) do
        local slot = i - ts.scrollOffset
        if slot >= 1 and slot <= ts.maxVisibleRows then
            local rowY = ts.contentY + (slot - 1) * ts.rowSpacing
            if row.labelHandle then
                UI.setPosition(row.labelHandle, ts.contentX, rowY + ts.fontSize)
                UI.setVisible(row.labelHandle, true)
            end
            if row.widgetSetPosition then row.widgetSetPosition(rowY) end
            if row.widgetSetVisible  then row.widgetSetVisible(true) end
        else
            if row.labelHandle then UI.setVisible(row.labelHandle, false) end
            if row.widgetSetVisible then row.widgetSetVisible(false) end
        end
    end
end

function settingsMenu.onTabScroll(tabKey, offset)
    local ts = settingsMenu.tabScroll[tabKey]
    if not ts then return end
    ts.scrollOffset = offset
    settingsMenu.refreshTabScroll(tabKey)
end

function settingsMenu.createTabScrollbar(tabKey, frameX, frameY, frameW,
                                          frameH, totalRows, maxVisibleRows,
                                          uiscale, s)
    local ts = settingsMenu.tabScroll[tabKey]
    ts.totalRows      = totalRows
    ts.maxVisibleRows = maxVisibleRows
    ts.fontSize       = s.fontSize

    if totalRows <= maxVisibleRows then return end

    local btnSize    = math.floor(24 * uiscale)
    local capH       = math.floor(4 * uiscale)
    local trackH     = math.max(math.floor(20 * uiscale),
                                frameH - btnSize * 2 - capH * 2)

    ts.scrollbarId = scrollbar.new({
        name         = "tab_" .. tabKey .. "_scrollbar",
        page         = settingsMenu.page,
        x            = frameX + frameW,
        y            = frameY,
        buttonSize   = btnSize,
        trackHeight  = trackH,
        capHeight    = capH,
        tileSize     = math.floor(8 * uiscale),
        totalItems   = totalRows,
        visibleItems = maxVisibleRows,
        uiscale      = uiscale,
        zIndex       = { track = Z_SB_TRACK, button = Z_SB_BUTTONS,
                         tab = Z_SB_TAB },
        onScroll = function(offset, sbId, sbName)
            settingsMenu.onTabScroll(tabKey, offset)
        end,
    })

    engine.logDebug("Tab scrollbar created for '" .. tabKey
        .. "' totalRows=" .. totalRows
        .. " maxVisible=" .. maxVisibleRows)
end

-----------------------------------------------------------
-- Tab switching
-----------------------------------------------------------

function settingsMenu.onTabChanged(key)
    settingsMenu.activeTab = key
    settingsMenu.showTab(key)
end

function settingsMenu.showTab(key)
    for tabKey, ts in pairs(settingsMenu.tabScroll) do
        local visible = (tabKey == key)

        if visible then
            settingsMenu.refreshTabScroll(tabKey)
        else
            for _, row in ipairs(ts.rowHandles) do
                if row.labelHandle     then UI.setVisible(row.labelHandle, false) end
                if row.widgetSetVisible then row.widgetSetVisible(false) end
            end
        end

        if ts.scrollbarId then
            scrollbar.setVisible(ts.scrollbarId, visible)
        end
    end
end

-----------------------------------------------------------
-- Scroll events (called from ui_manager)
-----------------------------------------------------------

function settingsMenu.onScroll(elemHandle, dx, dy)
    local ts = settingsMenu.tabScroll[settingsMenu.activeTab]
    if not ts or not ts.scrollbarId then return false end

    local function doScroll()
        if     dy > 0 then scrollbar.scrollUp(ts.scrollbarId)
        elseif dy < 0 then scrollbar.scrollDown(ts.scrollbarId)
        end
        return true
    end

    -- Tab frame itself
    local frameHandle = tabbar.getFrameHandle(settingsMenu.tabBarId)
    if elemHandle == frameHandle then return doScroll() end

    -- Any label or widget in the active tab's rows
    for _, row in ipairs(ts.rowHandles) do
        if row.labelHandle == elemHandle then return doScroll() end
        if row.widgetHandles then
            for _, wh in ipairs(row.widgetHandles) do
                if wh == elemHandle then return doScroll() end
            end
        end
    end

    -- Scrollbar elements themselves
    local sbId, _ = scrollbar.findByElementHandle(elemHandle)
    if sbId and sbId == ts.scrollbarId then return doScroll() end

    return false
end

function settingsMenu.handleScrollCallback(callbackName, elemHandle)
    for _, ts in pairs(settingsMenu.tabScroll) do
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
-- Apply / Save / Back
-----------------------------------------------------------

function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    local vals = graphicsTab.getWidgetValues()
    local result = data.apply(vals)
    if result.scaleChanged then
        settingsMenu.createUI()
        settingsMenu.show()
    end
end

function settingsMenu.onSave()
    engine.logInfo("Saving settings...")
    local vals = graphicsTab.getWidgetValues()
    local result = data.save(vals)
    if result.scaleChanged then
        settingsMenu.createUI()
        settingsMenu.show()
    end
end

function settingsMenu.onBack()
    data.revert()
end

-----------------------------------------------------------
-- TextBox submit (forwarded from ui_manager)
-----------------------------------------------------------

function settingsMenu.onTextBoxSubmit(name, value)
    graphicsTab.onTextBoxSubmit(name, value)
end

-----------------------------------------------------------
-- Show / Hide / Resize
-----------------------------------------------------------

function settingsMenu.show()
    data.reload()
    settingsMenu.createUI()
    if settingsMenu.page then UI.showPage(settingsMenu.page) end
end

function settingsMenu.hide()
    if settingsMenu.page then UI.hidePage(settingsMenu.page) end
end

function settingsMenu.onFramebufferResize(width, height)
    settingsMenu.fbW = width
    settingsMenu.fbH = height
    if settingsMenu.uiCreated then settingsMenu.createUI() end
end

-----------------------------------------------------------
-- Shutdown (full teardown — only called at app exit)
-----------------------------------------------------------

function settingsMenu.shutdown()
    for _, ts in pairs(settingsMenu.tabScroll) do
        if ts.scrollbarId then scrollbar.destroy(ts.scrollbarId) end
    end
    settingsMenu.destroyOwned()
    if settingsMenu.page then UI.deletePage(settingsMenu.page) end
end

return settingsMenu
