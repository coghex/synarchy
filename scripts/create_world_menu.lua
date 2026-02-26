-- Create World Menu Module
-- Orchestrates the create world page: panel, title, tab bar, buttons,
-- and preview.  Delegates to sub-modules for tab content, log panel,
-- bottom buttons, and generation logic.
local scale          = require("scripts.ui.scale")
local panel          = require("scripts.ui.panel")
local label          = require("scripts.ui.label")
local button         = require("scripts.ui.button")
local tabbar         = require("scripts.ui.tabbar")
local randbox        = require("scripts.ui.randbox")
local dropdown       = require("scripts.ui.dropdown")
local textbox        = require("scripts.ui.textbox")
local scrollbar      = require("scripts.ui.scrollbar")
local sprite         = require("scripts.ui.sprite")
local settingsTab    = require("scripts.create_world.settings_tab")
local advancedTab    = require("scripts.create_world.advanced_tab")
local logPanelMod    = require("scripts.create_world.log_panel")
local bottomButtons  = require("scripts.create_world.bottom_buttons")
local generation     = require("scripts.create_world.generation")
local worldManager   = require("scripts.world_manager")

local createWorldMenu = {}

-----------------------------------------------------------
-- Z-Index Plan
-----------------------------------------------------------
local Z_PANEL         = 1
local Z_TITLE         = 2
local Z_LEFT_PANEL    = 3
local Z_RIGHT_PANEL   = 3
local Z_TAB_FRAME     = 4
local Z_TAB_BUTTONS   = 5
local Z_CONTENT       = 6
local Z_WIDGETS       = 7
local Z_PREVIEW       = 7
local Z_LOG_TEXT      = 8
local Z_LOG_SB_TRACK  = 9
local Z_LOG_SB_BUTTON = 10
local Z_LOG_SB_TAB    = 11
local Z_BUTTONS       = 12

-----------------------------------------------------------
-- Base sizes (unscaled)
-----------------------------------------------------------
createWorldMenu.baseSizes = {
    fontSize         = 24,
    btnWidth         = 270,
    btnHeight        = 52,
    generateBtnWidth = 360,
    btnSpacing       = 16,
    tabHeight        = 32,
    tabFontSize      = 20,
    rowSpacing       = 50,
    nameBoxWidth     = 400,
    randboxWidth     = 200,
    randboxHeight    = 40,
    dropdownHeight   = 40,
    textboxWidth     = 100,
    textboxHeight    = 40,
    logFontSize      = 16,
}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
createWorldMenu.page         = nil
createWorldMenu.panelId      = nil
createWorldMenu.leftPanelId  = nil
createWorldMenu.rightPanelId = nil
createWorldMenu.genBarId     = nil
createWorldMenu.panelTexSet  = nil
createWorldMenu.buttonTexSet = nil
createWorldMenu.barTextures  = nil
createWorldMenu.menuFont     = nil
createWorldMenu.fbW          = 0
createWorldMenu.fbH          = 0
createWorldMenu.uiCreated    = false
createWorldMenu.tabBarId     = nil
createWorldMenu.activeTab    = "settings"
createWorldMenu.showMenuCallback     = nil
createWorldMenu.worldPreviewTexture  = nil

-- Pending world parameters
createWorldMenu.pending = {
    worldName  = "",
    seed       = "",
    worldSize  = "128",
    plateCount = "7",
}

-- Generation state
createWorldMenu.genState   = generation.IDLE
createWorldMenu.genElapsed = 0

-- Button IDs (written by bottomButtons module)
createWorldMenu.backButtonId       = nil
createWorldMenu.defaultsButtonId   = nil
createWorldMenu.generateButtonId   = nil
createWorldMenu.regenerateButtonId = nil
createWorldMenu.continueButtonId   = nil

-- Log state (written by logPanelMod.create)
createWorldMenu.logLines       = {}
createWorldMenu.logLabelIds    = {}
createWorldMenu.statusLabelId  = nil
createWorldMenu.logScrollbarId = nil
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
    table.insert(createWorldMenu.ownedLabels, id)  return id end
function createWorldMenu.trackButton(id)
    table.insert(createWorldMenu.ownedButtons, id) return id end
function createWorldMenu.trackPanel(id)
    table.insert(createWorldMenu.ownedPanels, id)  return id end
function createWorldMenu.trackTabbar(id)
    table.insert(createWorldMenu.ownedTabbars, id) return id end
function createWorldMenu.trackSprite(id)
    table.insert(createWorldMenu.ownedSprites, id) return id end
function createWorldMenu.trackRandBox(id)
    table.insert(createWorldMenu.ownedRandBoxes, id) return id end
function createWorldMenu.trackDropdown(id)
    table.insert(createWorldMenu.ownedDropdowns, id) return id end
function createWorldMenu.trackTextBox(id)
    table.insert(createWorldMenu.ownedTextBoxes, id) return id end

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

    createWorldMenu.worldPreviewTexture =
        engine.loadTexture("assets/textures/world/notexture.png")
    createWorldMenu.barTextures = {
        trackLeft   = engine.loadTexture("assets/textures/ui/bar/bar_left.png"),
        trackCenter = engine.loadTexture("assets/textures/ui/bar/bar_center.png"),
        trackRight  = engine.loadTexture("assets/textures/ui/bar/bar_right.png"),
        fillLeft    = engine.loadTexture("assets/textures/ui/bar/bar_fill_left.png"),
        fillCenter  = engine.loadTexture("assets/textures/ui/bar/bar_fill_center.png"),
    }

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
    createWorldMenu.genBarId           = nil
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

    -- Split dimensions
    local contentStartY = panelY + bounds.y + s.fontSize
                        + math.floor(30 * uiscale)
    local contentHeight = panelHeight - (contentStartY - panelY)
                        - bounds.y - s.btnHeight
                        - math.floor(60 * uiscale)
    local leftWidth  = math.floor(bounds.width * 0.4)
    local rightWidth = bounds.width - leftWidth - math.floor(20 * uiscale)

    -- Left panel (tabs)
    createWorldMenu.createLeftPanel(panelX, panelY, bounds,
        contentStartY, leftWidth, contentHeight, s, uiscale)

    -- Right panel (preview + log)
    local logResult = logPanelMod.create({
        page       = createWorldMenu.page,
        panelTexSet = createWorldMenu.panelTexSet,
        menuFont   = createWorldMenu.menuFont,
        baseSizes  = createWorldMenu.baseSizes,
        uiscale    = uiscale,
        s          = s,
        panelX     = panelX,
        panelY     = panelY,
        bounds     = bounds,
        contentStartY = contentStartY,
        contentHeight = contentHeight,
        leftWidth  = leftWidth,
        rightWidth = rightWidth,
        worldPreviewTexture = createWorldMenu.worldPreviewTexture,
        trackPanel  = createWorldMenu.trackPanel,
        trackSprite = createWorldMenu.trackSprite,
        trackLabel  = createWorldMenu.trackLabel,
        zRightPanel  = Z_RIGHT_PANEL,
        zPreview     = Z_PREVIEW,
        zLogText     = Z_LOG_TEXT,
        zLogSbTrack  = Z_LOG_SB_TRACK,
        zLogSbButton = Z_LOG_SB_BUTTON,
        zLogSbTab    = Z_LOG_SB_TAB,
        onLogScroll  = function(offset)
            createWorldMenu.onLogScroll(offset)
        end,
    })

    createWorldMenu.rightPanelId   = logResult.rightPanelId
    createWorldMenu.statusLabelId  = logResult.statusLabelId
    createWorldMenu.logLabelIds    = logResult.logLabelIds
    createWorldMenu.logScrollbarId = logResult.logScrollbarId
    createWorldMenu.logMaxVisible  = logResult.logMaxVisible
    createWorldMenu.logLineHeight  = logResult.logLineHeight
    createWorldMenu.logX           = logResult.logX
    createWorldMenu.logStartY      = logResult.logStartY

    -- Bottom buttons
    createWorldMenu.btnLayout = {
        panelX = panelX, panelY = panelY,
        panelWidth = panelWidth, panelHeight = panelHeight,
        bounds = bounds, s = s, uiscale = uiscale,
    }
    -- Build buttons matching current generation state
    if createWorldMenu.genState == generation.RUNNING then
        createWorldMenu.buildButtonsGenerating()
    elseif createWorldMenu.genState == generation.DONE then
        createWorldMenu.buildButtonsDone()
    else
        createWorldMenu.buildButtonsIdle()
    end

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

function createWorldMenu.createLeftPanel(panelX, panelY, bounds,
                                          contentStartY, leftWidth,
                                          contentHeight, s, uiscale)
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
        frameHeight       = leftBounds.height - s.tabHeight
                          - math.floor(20 * uiscale),
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

    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(createWorldMenu.tabBarId)
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

    createWorldMenu.showTab(createWorldMenu.activeTab)
end

-----------------------------------------------------------
-- Tab Switching
-----------------------------------------------------------

function createWorldMenu.showTab(key)
    for tabKey, elements in pairs(createWorldMenu.tabElements) do
        local visible = (tabKey == key)
        for _, elem in ipairs(elements) do
            if     elem.type == "label"    then UI.setVisible(elem.handle, visible)
            elseif elem.type == "randbox"  then randbox.setVisible(elem.id, visible)
            elseif elem.type == "dropdown" then dropdown.setVisible(elem.id, visible)
            elseif elem.type == "textbox"  then textbox.setVisible(elem.id, visible)
            end
        end
    end
end

-----------------------------------------------------------
-- Button helpers (delegate to bottomButtons module)
-----------------------------------------------------------

function createWorldMenu.buttonParams()
    return {
        menu       = createWorldMenu,
        page       = createWorldMenu.page,
        menuFont   = createWorldMenu.menuFont,
        buttonTexSet = createWorldMenu.buttonTexSet,
        baseSizes  = createWorldMenu.baseSizes,
        btnLayout  = createWorldMenu.btnLayout,
        zButtons   = Z_BUTTONS,
        trackButton = createWorldMenu.trackButton,
        barTextures = createWorldMenu.barTextures,
        onBack          = function() createWorldMenu.onBack() end,
        onDefaults      = function() createWorldMenu.onDefaults() end,
        onGenerateWorld = function() createWorldMenu.onGenerateWorld() end,
        onRegenerate    = function() createWorldMenu.onGenerateWorld() end,
        onContinue      = function() createWorldMenu.onContinue() end,
    }
end

function createWorldMenu.buildButtonsIdle()
    bottomButtons.buildIdle(createWorldMenu.buttonParams())
end

function createWorldMenu.buildButtonsDone()
    bottomButtons.buildDone(createWorldMenu.buttonParams())
end

function createWorldMenu.buildButtonsGenerating()
    bottomButtons.buildGenerating(createWorldMenu.buttonParams())
end

-----------------------------------------------------------
-- Log scroll
-----------------------------------------------------------

function createWorldMenu.onLogScroll(newOffset)
    logPanelMod.onScrollChanged(createWorldMenu, newOffset)
end

function createWorldMenu.onWorldGenLog(text)
    logPanelMod.addLine(createWorldMenu, text)
end

function createWorldMenu.onWorldPreviewReady(textureHandle)
    engine.logInfo("Updating world preview with texture handle: "
        .. tostring(textureHandle))
    createWorldMenu.worldPreviewTexture = textureHandle
    if createWorldMenu.page and UI.isPageVisible(createWorldMenu.page) then
        local savedGenState = createWorldMenu.genState
        createWorldMenu.createUI()
        createWorldMenu.genState = savedGenState
        -- createUI already builds the right buttons now, no extra call needed
        UI.showPage(createWorldMenu.page)
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

    if createWorldMenu.rightPanelId then
        local rh = panel.getBoxHandle(createWorldMenu.rightPanelId)
        if rh == elemHandle then return doScroll() end
    end

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
            scrollbar.scrollUp(createWorldMenu.logScrollbarId)
            return true
        elseif callbackName == "onScrollDown" then
            scrollbar.scrollDown(createWorldMenu.logScrollbarId)
            return true
        end
    end
    return false
end

-----------------------------------------------------------
-- Button handlers
-----------------------------------------------------------

function createWorldMenu.onBack()
    if worldManager.isActive() then
        worldManager.destroyWorld()
    end
    createWorldMenu.genState = generation.IDLE
    logPanelMod.clear(createWorldMenu)
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
    createWorldMenu.genState = generation.IDLE
    logPanelMod.clear(createWorldMenu)
    createWorldMenu.createUI()
    if createWorldMenu.page then UI.showPage(createWorldMenu.page) end
end

function createWorldMenu.onGenerateWorld()
    engine.logInfo("onGenerateWorld called, current genState=" .. tostring(createWorldMenu.genState))
    generation.start(createWorldMenu, logPanelMod)
    engine.logInfo("After generation.start, genState=" .. tostring(createWorldMenu.genState))
    engine.logInfo("btnLayout=" .. tostring(createWorldMenu.btnLayout))
    engine.logInfo("barTextures=" .. tostring(createWorldMenu.barTextures))
    createWorldMenu.buildButtonsGenerating()
    engine.logInfo("After buildButtonsGenerating, genBarId=" .. tostring(createWorldMenu.genBarId))
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
    local bar = require("scripts.ui.bar")

    local prevState = createWorldMenu.genState

    generation.poll(createWorldMenu, dt, logPanelMod, function()
        -- generation.poll sets genState = DONE and calls this callback
    end)

    -- Update the progress bar while generating
    if createWorldMenu.genState == generation.RUNNING and createWorldMenu.genBarId then
        local remaining, total = world.getInitProgress()
        if total and total > 0 then
            local generated = total - (remaining or 0)
            local progress = generated / total
            bar.setProgress(createWorldMenu.genBarId, progress)
            bar.setText(createWorldMenu.genBarId,
                tostring(generated) .. " / " .. tostring(total))
        end
    end

    -- Transition: just became DONE → swap bar for Continue button
    if createWorldMenu.genState == generation.DONE
        and prevState == generation.RUNNING then
        createWorldMenu.buildButtonsDone()
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
