-- Create World Menu Module
-- Orchestrates the create world page: panel, title, tab bar, buttons,
-- and preview. Delegates tab content to tab modules.
local scale          = require("scripts.ui.scale")
local panel          = require("scripts.ui.panel")
local label          = require("scripts.ui.label")
local button         = require("scripts.ui.button")
local tabbar         = require("scripts.ui.tabbar")
local sprite         = require("scripts.ui.sprite")
local randbox        = require("scripts.ui.randbox")
local dropdown       = require("scripts.ui.dropdown")
local textbox        = require("scripts.ui.textbox")
local scrollbar      = require("scripts.ui.scrollbar")
local settingsTab    = require("scripts.create_world.settings_tab")
local advancedTab    = require("scripts.create_world.advanced_tab")
local worldManager   = require("scripts.world_manager")

local createWorldMenu = {}

-----------------------------------------------------------
-- Z-Index Plan
-----------------------------------------------------------
local Z_PANEL       = 1
local Z_TITLE       = 2
local Z_LEFT_PANEL  = 3
local Z_RIGHT_PANEL = 3
local Z_TAB_FRAME   = 4
local Z_TAB_BUTTONS = 5
local Z_CONTENT     = 6
local Z_WIDGETS     = 7
local Z_PREVIEW     = 7
local Z_LOG_TEXT    = 8
local Z_LOG_SB_TRACK  = 9
local Z_LOG_SB_BUTTON = 10
local Z_LOG_SB_TAB    = 11
local Z_BUTTONS     = 12

-----------------------------------------------------------
-- Generation states
-----------------------------------------------------------
local GEN_IDLE       = "idle"        -- not started
local GEN_RUNNING    = "running"     -- world.init queued, waiting
local GEN_DONE       = "done"        -- world is active

-----------------------------------------------------------
-- Base sizes (unscaled)
-----------------------------------------------------------
createWorldMenu.baseSizes = {
    fontSize       = 24,
    btnWidth       = 270,
    btnHeight      = 52,
    generateBtnWidth = 360,
    btnSpacing     = 16,
    tabHeight      = 32,
    tabFontSize    = 20,
    rowSpacing     = 50,
    nameBoxWidth   = 400,
    randboxWidth   = 200,
    randboxHeight  = 40,
    dropdownHeight = 40,
    textboxWidth   = 100,
    textboxHeight  = 40,
    logFontSize    = 16,
}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
createWorldMenu.page         = nil
createWorldMenu.panelId      = nil
createWorldMenu.leftPanelId  = nil
createWorldMenu.rightPanelId = nil
createWorldMenu.panelTexSet  = nil
createWorldMenu.buttonTexSet = nil
createWorldMenu.menuFont     = nil
createWorldMenu.fbW          = 0
createWorldMenu.fbH          = 0
createWorldMenu.uiCreated    = false
createWorldMenu.tabBarId     = nil
createWorldMenu.activeTab    = "settings"
createWorldMenu.showMenuCallback = nil
createWorldMenu.worldPreviewTexture = nil

-- Pending world parameters
createWorldMenu.pending = {
    worldName  = "",
    seed       = "",
    worldSize  = "128",
    plateCount = "7",
}

-- Generation state
createWorldMenu.genState     = GEN_IDLE
createWorldMenu.genElapsed   = 0

-- Button IDs
createWorldMenu.backButtonId      = nil
createWorldMenu.defaultsButtonId  = nil
createWorldMenu.generateButtonId  = nil
createWorldMenu.regenerateButtonId = nil
createWorldMenu.continueButtonId   = nil

-- Log output
createWorldMenu.logLines      = {}    -- ALL lines (unbounded during generation)
createWorldMenu.logLabelIds   = {}    -- fixed-size label slots for visible window
createWorldMenu.statusLabelId = nil

-- Log scroll state
createWorldMenu.logScrollbarId  = nil
createWorldMenu.logScrollOffset = 0
createWorldMenu.logMaxVisible   = 0
createWorldMenu.logLineHeight   = 0
createWorldMenu.logX            = 0
createWorldMenu.logStartY       = 0

-- Per-tab element handles for show/hide
createWorldMenu.tabElements = {}

-- Owned element IDs for cleanup
createWorldMenu.ownedLabels     = {}
createWorldMenu.ownedButtons    = {}
createWorldMenu.ownedPanels     = {}
createWorldMenu.ownedTabbars    = {}
createWorldMenu.ownedSprites    = {}
createWorldMenu.ownedRandBoxes  = {}
createWorldMenu.ownedDropdowns  = {}
createWorldMenu.ownedTextBoxes  = {}

-- Layout cache for button rebuilds
createWorldMenu.btnLayout = nil

-----------------------------------------------------------
-- Tab registry
-----------------------------------------------------------
local tabDefs = {
    { key = "settings", name = "Settings" },
    { key = "advanced", name = "Advanced" },
}

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

function createWorldMenu.destroyOwned()
    -- Destroy log scrollbar first (not tracked in ownedLabels etc.)
    if createWorldMenu.logScrollbarId then
        scrollbar.destroy(createWorldMenu.logScrollbarId)
        createWorldMenu.logScrollbarId = nil
    end

    for _, id in ipairs(createWorldMenu.ownedLabels)    do label.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedButtons)   do button.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedPanels)    do panel.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedTabbars)   do tabbar.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedSprites)   do sprite.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedRandBoxes) do randbox.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedDropdowns) do dropdown.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedTextBoxes) do textbox.destroy(id) end

    createWorldMenu.ownedLabels     = {}
    createWorldMenu.ownedButtons    = {}
    createWorldMenu.ownedPanels     = {}
    createWorldMenu.ownedTabbars    = {}
    createWorldMenu.ownedSprites    = {}
    createWorldMenu.ownedRandBoxes  = {}
    createWorldMenu.ownedDropdowns  = {}
    createWorldMenu.ownedTextBoxes  = {}
end

-- Tracking helpers
function createWorldMenu.trackLabel(id)
    table.insert(createWorldMenu.ownedLabels, id)
    return id
end
function createWorldMenu.trackButton(id)
    table.insert(createWorldMenu.ownedButtons, id)
    return id
end
function createWorldMenu.trackPanel(id)
    table.insert(createWorldMenu.ownedPanels, id)
    return id
end
function createWorldMenu.trackTabbar(id)
    table.insert(createWorldMenu.ownedTabbars, id)
    return id
end
function createWorldMenu.trackSprite(id)
    table.insert(createWorldMenu.ownedSprites, id)
    return id
end
function createWorldMenu.trackRandBox(id)
    table.insert(createWorldMenu.ownedRandBoxes, id)
    return id
end
function createWorldMenu.trackDropdown(id)
    table.insert(createWorldMenu.ownedDropdowns, id)
    return id
end
function createWorldMenu.trackTextBox(id)
    table.insert(createWorldMenu.ownedTextBoxes, id)
    return id
end

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function createWorldMenu.setShowMenuCallback(callback)
    createWorldMenu.showMenuCallback = callback
end

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function createWorldMenu.init(panelTex, btnTex, font, width, height)
    createWorldMenu.panelTexSet  = panelTex
    createWorldMenu.buttonTexSet = btnTex
    createWorldMenu.menuFont     = font
    createWorldMenu.fbW          = width
    createWorldMenu.fbH          = height

    createWorldMenu.worldPreviewTexture = engine.loadTexture("assets/textures/world/notexture.png")

    createWorldMenu.createUI()
end

-----------------------------------------------------------
-- Full UI rebuild
-----------------------------------------------------------

function createWorldMenu.createUI()
    createWorldMenu.destroyOwned()

    createWorldMenu.backButtonId       = nil
    createWorldMenu.defaultsButtonId   = nil
    createWorldMenu.generateButtonId   = nil
    createWorldMenu.regenerateButtonId = nil
    createWorldMenu.continueButtonId   = nil
    createWorldMenu.panelId            = nil
    createWorldMenu.leftPanelId        = nil
    createWorldMenu.rightPanelId       = nil
    createWorldMenu.tabBarId           = nil
    createWorldMenu.tabElements        = {}
    createWorldMenu.logLabelIds        = {}
    createWorldMenu.statusLabelId      = nil
    createWorldMenu.btnLayout          = nil
    createWorldMenu.logScrollbarId     = nil
    createWorldMenu.logScrollOffset    = 0

    if createWorldMenu.uiCreated and createWorldMenu.page then
        UI.deletePage(createWorldMenu.page)
    end

    local uiscale = scale.get()
    local s = scale.applyAllWith(createWorldMenu.baseSizes, uiscale)

    createWorldMenu.page = UI.newPage("create_world_menu", "modal")

    -- Main panel
    local panelWidth  = math.floor(createWorldMenu.fbW * 0.85)
    local panelHeight = math.floor(createWorldMenu.fbH * 0.85)
    local panelX = (createWorldMenu.fbW - panelWidth) / 2
    local panelY = (createWorldMenu.fbH - panelHeight) / 2

    createWorldMenu.panelId = createWorldMenu.trackPanel(panel.new({
        name       = "create_world_panel",
        page       = createWorldMenu.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = createWorldMenu.panelTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = 64,
        zIndex     = Z_PANEL,
        padding    = { top = 60, bottom = 100, left = 50, right = 50 },
        uiscale    = uiscale,
    }))
    local bounds = panel.getContentBounds(createWorldMenu.panelId)

    -- Title
    createWorldMenu.createTitle(panelX, panelY, bounds, s, uiscale)

    -- Calculate split dimensions
    local contentStartY = panelY + bounds.y + s.fontSize + math.floor(30 * uiscale)
    local contentHeight = panelHeight - (contentStartY - panelY) - bounds.y - s.btnHeight - math.floor(60 * uiscale)
    local leftWidth = math.floor(bounds.width * 0.4)
    local rightWidth = bounds.width - leftWidth - math.floor(20 * uiscale)

    -- Left panel (tabbed)
    createWorldMenu.createLeftPanel(panelX, panelY, bounds, contentStartY, 
                                     leftWidth, contentHeight, s, uiscale)

    -- Right panel (world preview + log output)
    createWorldMenu.createRightPanel(panelX, panelY, bounds, contentStartY,
                                      leftWidth, rightWidth, contentHeight, s, uiscale)

    -- Bottom buttons (saves layout for later swaps)
    createWorldMenu.createButtons(panelX, panelY, panelWidth, panelHeight,
                                   bounds, s, uiscale)

    createWorldMenu.uiCreated = true
end

-----------------------------------------------------------
-- Title
-----------------------------------------------------------

function createWorldMenu.createTitle(panelX, panelY, bounds, s, uiscale)
    local titleLabelId = createWorldMenu.trackLabel(label.new({
        name     = "create_world_title",
        text     = "Create World",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local titleW, _ = label.getSize(titleLabelId)
    local titleHandle = label.getElementHandle(titleLabelId)
    local titleX = panelX + bounds.x + (bounds.width - titleW) / 2
    local titleY = panelY + bounds.y + s.fontSize
    UI.addToPage(createWorldMenu.page, titleHandle, titleX, titleY)
    UI.setZIndex(titleHandle, Z_TITLE)
end

-----------------------------------------------------------
-- Left Panel (Tabbed Settings)
-----------------------------------------------------------

function createWorldMenu.createLeftPanel(panelX, panelY, bounds, contentStartY,
                                          leftWidth, contentHeight, s, uiscale)
    local leftX = panelX + bounds.x
    
    createWorldMenu.leftPanelId = createWorldMenu.trackPanel(panel.new({
        name       = "left_panel",
        page       = createWorldMenu.page,
        x          = leftX,
        y          = contentStartY,
        width      = leftWidth,
        height     = contentHeight,
        textureSet = createWorldMenu.panelTexSet,
        color      = {0.9, 0.9, 0.9, 1.0},
        tileSize   = 64,
        zIndex     = Z_LEFT_PANEL,
        padding    = { top = 10, bottom = 10, left = 10, right = 10 },
        uiscale    = uiscale,
    }))
    
    local leftBounds = panel.getContentBounds(createWorldMenu.leftPanelId)
    
    -- Tab bar
    local tabList = {}
    for _, def in ipairs(tabDefs) do
        table.insert(tabList, { name = def.name, key = def.key })
    end
    
    createWorldMenu.tabBarId = createWorldMenu.trackTabbar(tabbar.new({
        name              = "create_world_tabs",
        page              = createWorldMenu.page,
        x                 = leftX + leftBounds.x,
        y                 = contentStartY + leftBounds.y,
        width             = leftBounds.width,
        font              = createWorldMenu.menuFont,
        fontSize          = createWorldMenu.baseSizes.tabFontSize,
        tabHeight         = createWorldMenu.baseSizes.tabHeight,
        frameHeight       = leftBounds.height - s.tabHeight - math.floor(20 * uiscale),
        uiscale           = uiscale,
        zIndex            = Z_TAB_FRAME,
        textColor         = {0.0, 0.0, 0.0, 1.0},
        selectedTextColor = {1.0, 1.0, 1.0, 1.0},
        tabs              = tabList,
        onChange = function(key, index, tbId)
            createWorldMenu.activeTab = key
            createWorldMenu.showTab(key)
        end,
    }))
    
    tabbar.selectByKey(createWorldMenu.tabBarId, createWorldMenu.activeTab)
    
    -- Create ALL tab contents up front
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(createWorldMenu.tabBarId)
    local pad = math.floor(20 * uiscale)

    local tabParams = {
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        baseSizes  = createWorldMenu.baseSizes,
        uiscale    = uiscale,
        s          = s,
        contentX   = frameX + pad,
        contentY   = frameY + pad,
        contentW   = frameW - pad * 2,
        zContent   = Z_CONTENT,
        zWidgets   = Z_WIDGETS,
        pending    = createWorldMenu.pending,
        trackLabel    = createWorldMenu.trackLabel,
        trackRandBox  = createWorldMenu.trackRandBox,
        trackDropdown = createWorldMenu.trackDropdown,
        trackTextBox  = createWorldMenu.trackTextBox,
    }

    createWorldMenu.tabElements["settings"] = settingsTab.create(tabParams)
    createWorldMenu.tabElements["advanced"]  = advancedTab.create(tabParams)

    -- Show only the active tab
    createWorldMenu.showTab(createWorldMenu.activeTab)
end

-----------------------------------------------------------
-- Tab Switching (show/hide, no rebuild)
-----------------------------------------------------------

function createWorldMenu.showTab(key)
    for tabKey, elements in pairs(createWorldMenu.tabElements) do
        local visible = (tabKey == key)
        for _, elem in ipairs(elements) do
            if elem.type == "label" then
                UI.setVisible(elem.handle, visible)
            elseif elem.type == "randbox" then
                randbox.setVisible(elem.id, visible)
            elseif elem.type == "dropdown" then
                dropdown.setVisible(elem.id, visible)
            elseif elem.type == "textbox" then
                textbox.setVisible(elem.id, visible)
            end
        end
    end
end

-----------------------------------------------------------
-- Right Panel (World Preview + Generation Log)
-----------------------------------------------------------

function createWorldMenu.createRightPanel(panelX, panelY, bounds, contentStartY,
                                           leftWidth, rightWidth, contentHeight, s, uiscale)
    local rightX = panelX + bounds.x + leftWidth + math.floor(20 * uiscale)
    
    createWorldMenu.rightPanelId = createWorldMenu.trackPanel(panel.new({
        name       = "right_panel",
        page       = createWorldMenu.page,
        x          = rightX,
        y          = contentStartY,
        width      = rightWidth,
        height     = contentHeight,
        textureSet = createWorldMenu.panelTexSet,
        color      = {0.2, 0.2, 0.2, 1.0},
        tileSize   = 64,
        zIndex     = Z_RIGHT_PANEL,
        padding    = { top = 10, bottom = 10, left = 10, right = 10 },
        uiscale    = uiscale,
    }))
    
    -- Make the right panel clickable so it receives scroll events
    local rightPanelHandle = panel.getBoxHandle(createWorldMenu.rightPanelId)
    UI.setClickable(rightPanelHandle, true)
    UI.setOnClick(rightPanelHandle, "onLogPanelScroll")
    
    local rightBounds = panel.getContentBounds(createWorldMenu.rightPanelId)

    -- World preview image (upper portion)
    local previewSize = math.min(rightBounds.width, rightBounds.height * 0.5) * 0.7
    local previewX = rightX + rightBounds.x + (rightBounds.width - previewSize) / 2
    local previewY = contentStartY + rightBounds.y + math.floor(20 * uiscale)
    
    if createWorldMenu.worldPreviewTexture then
        createWorldMenu.trackSprite(sprite.new({
            name    = "world_preview",
            page    = createWorldMenu.page,
            x       = previewX,
            y       = previewY,
            width   = previewSize,
            height  = previewSize,
            texture = createWorldMenu.worldPreviewTexture,
            color   = {1.0, 1.0, 1.0, 1.0},
            zIndex  = Z_PREVIEW,
            uiscale = uiscale,
        }))
    end

    -- Status label (below preview)
    local logTopY = previewY + previewSize + math.floor(20 * uiscale)
    local logX = rightX + rightBounds.x + math.floor(10 * uiscale)

    createWorldMenu.statusLabelId = createWorldMenu.trackLabel(label.new({
        name     = "gen_status",
        text     = "",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.logFontSize,
        color    = {0.7, 0.9, 0.7, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local statusHandle = label.getElementHandle(createWorldMenu.statusLabelId)
    UI.addToPage(createWorldMenu.page, statusHandle, logX, logTopY + s.logFontSize)
    UI.setZIndex(statusHandle, Z_LOG_TEXT)

    -- Log lines (below status) — fixed label slots, virtual scrolling
    local logLineStartY = logTopY + s.logFontSize + math.floor(10 * uiscale)
    local logLineHeight = math.floor(createWorldMenu.baseSizes.logFontSize * 1.4 * uiscale)
    local availableHeight = contentStartY + contentHeight - logLineStartY - math.floor(10 * uiscale)
    local maxLogLines = math.max(1, math.floor(availableHeight / logLineHeight))

    -- Save layout for scroll calculations
    createWorldMenu.logX           = logX
    createWorldMenu.logStartY      = logLineStartY
    createWorldMenu.logLineHeight  = logLineHeight
    createWorldMenu.logMaxVisible  = maxLogLines
    createWorldMenu.logScrollOffset = 0

    -- Create the fixed label slots
    createWorldMenu.logLabelIds = {}
    for i = 1, maxLogLines do
        local lid = createWorldMenu.trackLabel(label.new({
            name     = "gen_log_" .. i,
            text     = "",
            font     = createWorldMenu.menuFont,
            fontSize = createWorldMenu.baseSizes.logFontSize,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = createWorldMenu.page,
            uiscale  = uiscale,
        }))
        local lh = label.getElementHandle(lid)
        UI.addToPage(createWorldMenu.page, lh,
                     logX, logLineStartY + (i - 1) * logLineHeight + s.logFontSize)
        UI.setZIndex(lh, Z_LOG_TEXT)
        table.insert(createWorldMenu.logLabelIds, lid)
    end

    -- Create scrollbar for the log (hidden until needed)
    local sbBtnSize = math.floor(24 * uiscale)
    local sbCapH    = math.floor(4 * uiscale)
    local sbTrackH  = math.max(math.floor(20 * uiscale),
                               availableHeight - sbBtnSize * 2 - sbCapH * 2)
    local sbX       = rightX + rightBounds.x + rightBounds.width - sbBtnSize

    createWorldMenu.logScrollbarId = scrollbar.new({
        name         = "log_scrollbar",
        page         = createWorldMenu.page,
        x            = sbX,
        y            = logLineStartY,
        buttonSize   = sbBtnSize,
        trackHeight  = sbTrackH,
        capHeight    = sbCapH,
        tileSize     = math.floor(8 * uiscale),
        totalItems   = 0,
        visibleItems = maxLogLines,
        uiscale      = uiscale,
        zIndex       = { track  = Z_LOG_SB_TRACK,
                         button = Z_LOG_SB_BUTTON,
                         tab    = Z_LOG_SB_TAB },
        onScroll = function(offset, sbId, sbName)
            createWorldMenu.onLogScroll(offset)
        end,
    })

    -- Hide until we actually have content that overflows
    scrollbar.setVisible(createWorldMenu.logScrollbarId, false)

    engine.logDebug("Log panel created: maxVisible=" .. maxLogLines
        .. " lineHeight=" .. logLineHeight)
end

-----------------------------------------------------------
-- Log Scroll
-----------------------------------------------------------

function createWorldMenu.onLogScroll(offset)
    createWorldMenu.logScrollOffset = offset
    createWorldMenu.refreshLogDisplay()
end

-----------------------------------------------------------
-- Log Output Helpers
-----------------------------------------------------------

function createWorldMenu.addLogLine(text)
    table.insert(createWorldMenu.logLines, text)

    local totalLines  = #createWorldMenu.logLines
    local maxVisible  = createWorldMenu.logMaxVisible
    local needsScroll = totalLines > maxVisible

    -- Update scrollbar content size
    if createWorldMenu.logScrollbarId then
        scrollbar.setContentSize(createWorldMenu.logScrollbarId,
                                 totalLines, maxVisible)
        scrollbar.setVisible(createWorldMenu.logScrollbarId, needsScroll)
    end

    -- Auto-scroll to bottom
    if needsScroll then
        local maxOffset = totalLines - maxVisible
        createWorldMenu.logScrollOffset = maxOffset
        if createWorldMenu.logScrollbarId then
            scrollbar.setScrollOffset(createWorldMenu.logScrollbarId, maxOffset)
        end
    else
        createWorldMenu.logScrollOffset = 0
    end

    createWorldMenu.refreshLogDisplay()
end

function createWorldMenu.clearLog()
    createWorldMenu.logLines = {}
    createWorldMenu.logScrollOffset = 0

    if createWorldMenu.logScrollbarId then
        scrollbar.setContentSize(createWorldMenu.logScrollbarId, 0,
                                 createWorldMenu.logMaxVisible)
        scrollbar.setVisible(createWorldMenu.logScrollbarId, false)
    end

    createWorldMenu.refreshLogDisplay()
end

function createWorldMenu.refreshLogDisplay()
    local maxVisible = #createWorldMenu.logLabelIds
    local offset     = createWorldMenu.logScrollOffset

    for i = 1, maxVisible do
        local lid       = createWorldMenu.logLabelIds[i]
        local dataIndex = offset + i
        local text      = createWorldMenu.logLines[dataIndex] or ""
        local lh        = label.getElementHandle(lid)
        UI.setText(lh, text)
    end
end

function createWorldMenu.setStatus(text)
    if createWorldMenu.statusLabelId then
        local sh = label.getElementHandle(createWorldMenu.statusLabelId)
        UI.setText(sh, text)
    end
end

-----------------------------------------------------------
-- World Generation Log Receiver (from Haskell world thread)
-----------------------------------------------------------

function createWorldMenu.onWorldGenLog(text)
    if createWorldMenu.genState == GEN_RUNNING or createWorldMenu.genState == GEN_DONE then
        createWorldMenu.addLogLine(text)
    end
end

-----------------------------------------------------------
-- Scroll events (called from ui_manager)
-----------------------------------------------------------

function createWorldMenu.onScroll(elemHandle, dx, dy)
    if not createWorldMenu.logScrollbarId then return false end

    local totalLines = #createWorldMenu.logLines
    if totalLines <= createWorldMenu.logMaxVisible then return false end

    local function doScroll()
        if     dy > 0 then scrollbar.scrollUp(createWorldMenu.logScrollbarId)
        elseif dy < 0 then scrollbar.scrollDown(createWorldMenu.logScrollbarId)
        end
        return true
    end

    -- Check the right panel box itself (this is the clickable surface)
    if createWorldMenu.rightPanelId then
        local rh = panel.getBoxHandle(createWorldMenu.rightPanelId)
        if rh == elemHandle then return doScroll() end
    end

    -- Check scrollbar elements
    local sbId, _ = scrollbar.findByElementHandle(elemHandle)
    if sbId and sbId == createWorldMenu.logScrollbarId then
        return doScroll()
    end

    return false
end

function createWorldMenu.handleScrollCallback(callbackName, elemHandle)
    if not createWorldMenu.logScrollbarId then return false end

    local sbId, _ = scrollbar.findByElementHandle(elemHandle)
    if sbId and sbId == createWorldMenu.logScrollbarId then
        if callbackName == "onScrollUp" then
            scrollbar.scrollUp(sbId)
            return true
        elseif callbackName == "onScrollDown" then
            scrollbar.scrollDown(sbId)
            return true
        end
    end

    return false
end

-----------------------------------------------------------
-- Bottom buttons
-----------------------------------------------------------

function createWorldMenu.createButtons(panelX, panelY, panelWidth, panelHeight,
                                        bounds, s, uiscale)
    -- Save layout params for rebuilding buttons later
    createWorldMenu.btnLayout = {
        panelX = panelX, panelY = panelY,
        panelWidth = panelWidth, panelHeight = panelHeight,
        bounds = bounds, s = s, uiscale = uiscale,
    }

    createWorldMenu.buildButtons_idle()
end

-- Build the "idle" button set: [Back] [Defaults]     [Generate World]
function createWorldMenu.buildButtons_idle()
    createWorldMenu.destroyDynamicButtons()

    local L = createWorldMenu.btnLayout
    local uiscale = L.uiscale
    local s = L.s

    createWorldMenu.backButtonId = createWorldMenu.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onBack()
        end,
    }))

    createWorldMenu.defaultsButtonId = createWorldMenu.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onDefaults()
        end,
    }))

    createWorldMenu.generateButtonId = createWorldMenu.trackButton(button.new({
        name       = "generate_btn",
        text       = "Generate World",
        width      = createWorldMenu.baseSizes.generateBtnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {0.2, 0.8, 0.2, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onGenerateWorld()
        end,
    }))

    -- Layout: [Back] [Defaults]     [Generate World]
    local backW, backH = button.getSize(createWorldMenu.backButtonId)
    local defaultsW, _ = button.getSize(createWorldMenu.defaultsButtonId)
    local generateW, _ = button.getSize(createWorldMenu.generateButtonId)
    
    local bottomPad = math.floor(100 * uiscale)
    local btnY = L.panelY + L.panelHeight - bottomPad + (bottomPad - backH) / 2
    
    local backX = L.panelX + L.bounds.x
    local defaultsX = backX + backW + s.btnSpacing
    local generateX = L.panelX + L.bounds.x + L.bounds.width - generateW

    UI.setPosition(button.getElementHandle(createWorldMenu.backButtonId), backX, btnY)
    UI.setPosition(button.getElementHandle(createWorldMenu.defaultsButtonId), defaultsX, btnY)
    UI.setPosition(button.getElementHandle(createWorldMenu.generateButtonId), generateX, btnY)
    
    UI.setZIndex(button.getElementHandle(createWorldMenu.backButtonId), Z_BUTTONS)
    UI.setZIndex(button.getElementHandle(createWorldMenu.defaultsButtonId), Z_BUTTONS)
    UI.setZIndex(button.getElementHandle(createWorldMenu.generateButtonId), Z_BUTTONS)
end

-- Build the "done" button set: [Back] [Defaults] [Regenerate]     [Continue]
function createWorldMenu.buildButtons_done()
    createWorldMenu.destroyDynamicButtons()

    local L = createWorldMenu.btnLayout
    local uiscale = L.uiscale
    local s = L.s

    createWorldMenu.backButtonId = createWorldMenu.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onBack()
        end,
    }))

    createWorldMenu.defaultsButtonId = createWorldMenu.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onDefaults()
        end,
    }))

    createWorldMenu.regenerateButtonId = createWorldMenu.trackButton(button.new({
        name       = "regenerate_btn",
        text       = "Regenerate",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {0.8, 0.6, 0.1, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onRegenerate()
        end,
    }))

    createWorldMenu.continueButtonId = createWorldMenu.trackButton(button.new({
        name       = "continue_btn",
        text       = "Continue",
        width      = createWorldMenu.baseSizes.generateBtnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {0.2, 0.8, 0.2, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onContinue()
        end,
    }))

    -- Layout: [Back] [Defaults] [Regenerate]     [Continue]
    local backW, backH = button.getSize(createWorldMenu.backButtonId)
    local defaultsW, _ = button.getSize(createWorldMenu.defaultsButtonId)
    local regenW, _ = button.getSize(createWorldMenu.regenerateButtonId)
    local contW, _ = button.getSize(createWorldMenu.continueButtonId)
    
    local bottomPad = math.floor(100 * uiscale)
    local btnY = L.panelY + L.panelHeight - bottomPad + (bottomPad - backH) / 2
    
    local backX = L.panelX + L.bounds.x
    local defaultsX = backX + backW + s.btnSpacing
    local regenX = defaultsX + defaultsW + s.btnSpacing
    local contX = L.panelX + L.bounds.x + L.bounds.width - contW

    UI.setPosition(button.getElementHandle(createWorldMenu.backButtonId), backX, btnY)
    UI.setPosition(button.getElementHandle(createWorldMenu.defaultsButtonId), defaultsX, btnY)
    UI.setPosition(button.getElementHandle(createWorldMenu.regenerateButtonId), regenX, btnY)
    UI.setPosition(button.getElementHandle(createWorldMenu.continueButtonId), contX, btnY)
    
    UI.setZIndex(button.getElementHandle(createWorldMenu.backButtonId), Z_BUTTONS)
    UI.setZIndex(button.getElementHandle(createWorldMenu.defaultsButtonId), Z_BUTTONS)
    UI.setZIndex(button.getElementHandle(createWorldMenu.regenerateButtonId), Z_BUTTONS)
    UI.setZIndex(button.getElementHandle(createWorldMenu.continueButtonId), Z_BUTTONS)
end

-- Destroy only the dynamic bottom buttons (not the whole UI)
function createWorldMenu.destroyDynamicButtons()
    local function destroyAndUntrack(id)
        if not id then return end
        local handle = button.getElementHandle(id)
        if handle then
            UI.deleteElement(handle)
        end
        button.destroy(id)
        for i = #createWorldMenu.ownedButtons, 1, -1 do
            if createWorldMenu.ownedButtons[i] == id then
                table.remove(createWorldMenu.ownedButtons, i)
                break
            end
        end
    end

    destroyAndUntrack(createWorldMenu.backButtonId)
    destroyAndUntrack(createWorldMenu.defaultsButtonId)
    destroyAndUntrack(createWorldMenu.generateButtonId)
    destroyAndUntrack(createWorldMenu.regenerateButtonId)
    destroyAndUntrack(createWorldMenu.continueButtonId)

    createWorldMenu.backButtonId       = nil
    createWorldMenu.defaultsButtonId   = nil
    createWorldMenu.generateButtonId   = nil
    createWorldMenu.regenerateButtonId = nil
    createWorldMenu.continueButtonId   = nil
end

-----------------------------------------------------------
-- Button handlers
-----------------------------------------------------------

function createWorldMenu.onBack()
    if worldManager.isActive() then
        worldManager.destroyWorld()
    end
    createWorldMenu.genState = GEN_IDLE
    createWorldMenu.clearLog()
    if createWorldMenu.showMenuCallback then
        createWorldMenu.showMenuCallback("main")
    end
end

function createWorldMenu.onDefaults()
    engine.logInfo("Loading create world defaults...")
    createWorldMenu.pending = {
        worldName  = "",
        seed       = "",
        worldSize  = "128",
        plateCount = "7",
    }
    createWorldMenu.genState = GEN_IDLE
    createWorldMenu.clearLog()
    createWorldMenu.createUI()
    if createWorldMenu.page then UI.showPage(createWorldMenu.page) end
end

function createWorldMenu.onGenerateWorld()
    -- Read values from tab modules
    local advVals = advancedTab.getWidgetValues()
    if advVals.plateCount then
        createWorldMenu.pending.plateCount = advVals.plateCount
    end

    local p = createWorldMenu.pending

    local seedNum = tonumber(p.seed, 16) or 0
    local sizeNum = tonumber(p.worldSize) or 128
    local plateNum = tonumber(p.plateCount) or 7

    if plateNum < 2 then plateNum = 2 end
    if plateNum > 20 then plateNum = 20 end

    engine.logInfo("Generating world: name=" .. p.worldName
        .. " seed=0x" .. p.seed
        .. " size=" .. tostring(sizeNum)
        .. " plates=" .. tostring(plateNum))

    -- Destroy any previous world
    if worldManager.isActive() then
        worldManager.destroyWorld()
    end

    -- Store params on worldView so textures get wired up
    local worldView = require("scripts.world_view")
    worldView.worldParams = {
        seed = seedNum,
        worldSize = sizeNum,
        plateCount = plateNum,
        worldName = p.worldName,
    }

    -- Kick off generation (async on world thread)
    createWorldMenu.genState = GEN_RUNNING
    createWorldMenu.genElapsed = 0
    createWorldMenu.clearLog()
    createWorldMenu.setStatus("Generating world...")
    createWorldMenu.addLogLine("Seed: 0x" .. (p.seed ~= "" and p.seed or "0"))
    createWorldMenu.addLogLine("Size: " .. tostring(sizeNum))
    createWorldMenu.addLogLine("Plates: " .. tostring(plateNum))
    createWorldMenu.addLogLine("")

    worldView.startGeneration()
end

function createWorldMenu.onRegenerate()
    createWorldMenu.onGenerateWorld()
end

function createWorldMenu.onContinue()
    if createWorldMenu.showMenuCallback then
        createWorldMenu.showMenuCallback("world_view")
    end
end

-----------------------------------------------------------
-- Update (called every frame from ui_manager)
-----------------------------------------------------------

function createWorldMenu.update(dt)
    if createWorldMenu.genState == GEN_RUNNING then
        createWorldMenu.genElapsed = createWorldMenu.genElapsed + dt

        if worldManager.isActive() then
            createWorldMenu.genState = GEN_DONE
            local elapsed = string.format("%.1f", createWorldMenu.genElapsed)
            createWorldMenu.setStatus("World generated! (" .. elapsed .. "s)")
            createWorldMenu.addLogLine("Generation complete.")

            createWorldMenu.buildButtons_done()

            engine.logInfo("World generation complete in " .. elapsed .. "s")
        else
            local dots = string.rep(".", (math.floor(createWorldMenu.genElapsed * 3) % 4))
            createWorldMenu.setStatus("Generating world" .. dots)
        end
    end
end

-----------------------------------------------------------
-- Show / Hide / Resize
-----------------------------------------------------------

function createWorldMenu.show()
    createWorldMenu.createUI()
    if createWorldMenu.page then UI.showPage(createWorldMenu.page) end
end

function createWorldMenu.hide()
    if createWorldMenu.page then UI.hidePage(createWorldMenu.page) end
end

function createWorldMenu.onFramebufferResize(width, height)
    createWorldMenu.fbW = width
    createWorldMenu.fbH = height
    if createWorldMenu.uiCreated then createWorldMenu.createUI() end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function createWorldMenu.shutdown()
    createWorldMenu.destroyOwned()
    if createWorldMenu.page then UI.deletePage(createWorldMenu.page) end
end

return createWorldMenu
