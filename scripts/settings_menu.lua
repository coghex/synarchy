-- Settings Menu Module
-- Orchestrates the settings page: panel, title, tab bar, buttons,
-- scroll infrastructure. Delegates tab content to tab modules and
-- settings state to data module.
local scale          = require("scripts.ui.scale")
local responsive     = require("scripts.ui.responsive")
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
local graphicsTab      = require("scripts.settings.graphics_tab")
local notificationsTab = require("scripts.settings.notifications_tab")
local inputTab         = require("scripts.settings.input_tab")

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
    { key = "graphics", name = "Graphics", create = function(p)
        return graphicsTab.create(p)
    end },
    { key = "input",    name = "Input",    create = function(p)
        return inputTab.create(p)
    end },
    { key = "notifications", name = "Notifications", create = function(p)
        return notificationsTab.create(p)
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
    -- Keybinds are write-through (no pending state), so the global
    -- Defaults reset restores factory bindings immediately and persists
    -- them — giving the Input tab its path back to defaults. The createUI
    -- rebuild below repopulates the Input rows from the reset bindings.
    if engine.loadDefaultKeybinds then
        engine.loadDefaultKeybinds()
        engine.saveKeybinds()
    end
    settingsMenu.createUI()
    if settingsMenu.page then UI.showPage(settingsMenu.page) end
end

-----------------------------------------------------------
-- Keybind editor (Input tab) glue
-----------------------------------------------------------

-- Refresh ONLY the Input tab after a keybind add/remove. A full
-- createUI() would also recreate the Graphics widgets from data.current
-- and reset pending state, silently discarding any un-applied Graphics
-- edits — so keybind changes must not touch the rest of the page.
function settingsMenu.refreshInputTab()
    local ts = settingsMenu.tabScroll["input"]
    if not ts or not settingsMenu.page then return end

    inputTab.destroyWidgets()
    if ts.scrollbarId then
        scrollbar.destroy(ts.scrollbarId)
        ts.scrollbarId = nil
    end

    local uiscale = data.current.uiScale
    local s = scale.applyAllWith(settingsMenu.baseSizes, uiscale)

    ts.scrollOffset = 0
    ts.rowHandles = inputTab.create(settingsMenu.tabCreateParams(uiscale, s, ts))

    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(settingsMenu.tabBarId)
    settingsMenu.createTabScrollbar("input", frameX, frameY, frameW, frameH,
        #ts.rowHandles, ts.maxVisibleRows, uiscale, s)
    settingsMenu.refreshTabScroll("input")
    if ts.scrollbarId then
        scrollbar.setVisible(ts.scrollbarId, settingsMenu.activeTab == "input")
    end
end

-- True while the Input tab is capturing a key (waiting for a press or
-- showing the conflict modal). ui_manager gates key routing + escape on
-- this.
function settingsMenu.isCapturingKey()
    return inputTab.captureActive and inputTab.captureActive()
end

-- The next key press while "+" capture is waiting (routed from
-- ui_manager.onKeyDown).
function settingsMenu.onKeyCapture(key)
    if inputTab.onKeyCapture then inputTab.onKeyCapture(key) end
end

-- Escape during capture (routed from ui_manager.onUIEscape).
function settingsMenu.cancelKeyCapture()
    if inputTab.cancelCapture then inputTab.cancelCapture() end
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

    -- The Input tab tracks its own key/plus buttons (so it can refresh
    -- in place without a full page rebuild); tear them down here too.
    inputTab.destroyWidgets()

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
--
-- opts.preserveState (#748): true for a mere geometry/scale-notification
-- rebuild (window resize, or another screen picking up a UI-scale change
-- someone else applied) as opposed to a semantic re-entry (init/show/
-- Defaults) — preserves unapplied pending settings and each tab's
-- clamped scroll offset instead of discarding them. Active tab is
-- always preserved (createUI never touches settingsMenu.activeTab)
-- regardless of opts.
-----------------------------------------------------------

function settingsMenu.createUI(opts)
    opts = opts or {}
    local preserveState = opts.preserveState or false

    local savedScroll = {}
    local textboxSnap = nil
    if preserveState then
        for key, ts in pairs(settingsMenu.tabScroll) do
            savedScroll[key] = ts.scrollOffset
        end
        -- Snapshot every textbox's raw (possibly unsubmitted) text,
        -- cursor, and focus BEFORE destroyOwned() tears them down —
        -- scoped to this page so a resize never captures another
        -- screen's still-live textboxes. (Keyboard CONTROL focus,
        -- #745, is captured/restored by the caller instead — see
        -- onFramebufferResize — since restoring it needs the page
        -- already visible, which createUI() itself never makes it.)
        textboxSnap = textbox.snapshotPage(settingsMenu.page)
    end

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

    if not preserveState then
        data.resetPending()
    end

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

    if preserveState then
        for key, ts in pairs(settingsMenu.tabScroll) do
            local maxOffset = math.max(0, ts.totalRows - ts.maxVisibleRows)
            local restored = math.max(0, math.min(savedScroll[key] or 0, maxOffset))
            settingsMenu.onTabScroll(key, restored)
        end
        -- Restores raw text + cursor + focus onto the freshly rebuilt
        -- textboxes (matched by name); a no-op for any name that no
        -- longer has a live textbox (e.g. a hidden tab's control).
        textbox.restoreAll(textboxSnap)
    end

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

    -- #748 round 5: the tab bar's own FRAME is correctly sized to
    -- bounds.width (the panel's content bounds after scaled padding —
    -- as narrow as ~240px at the supported 800x2160@4x combination),
    -- but tabbar.lua lays each tab out at a width driven purely by its
    -- OWN label text + scaled textPadding, left-to-right with no fit/
    -- clip/scroll of its own — unrelated to bounds.width, and able to
    -- overflow the framebuffer before any tab content is considered.
    -- Shrink one effective, LOCAL uiscale for the tab bar only (never
    -- the stored/configured scale, and never `s`/the rest of this
    -- screen's layout), fit against bounds.width, mirroring
    -- create_world_menu's identical tab-bar treatment for its own tabs.
    local tabFontSize = math.floor(settingsMenu.baseSizes.tabFontSize * uiscale)
    local textPadding = math.floor(10 * uiscale)
    local naturalTabWidth = 0
    for _, def in ipairs(tabDefs) do
        naturalTabWidth = naturalTabWidth
            + engine.getTextWidth(settingsMenu.menuFont, def.name, tabFontSize)
            + textPadding * 2
    end
    local tabBarUiscale = responsive.fitScale(naturalTabWidth, bounds.width, uiscale)

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
        uiscale           = tabBarUiscale,
        zIndex            = Z_TAB_FRAME,
        textColor         = {0.0, 0.0, 0.0, 1.0},
        selectedTextColor = {1.0, 1.0, 1.0, 1.0},
        tabs              = tabList,
        onChange = function(key, index, tbId)
            settingsMenu.onTabChanged(key)
        end,
    }))

    tabbar.selectByKey(settingsMenu.tabBarId, settingsMenu.activeTab)

    -- #743: explicit scroll-capture + pointer-block on the tab frame,
    -- replacing the old no-op-onClick-just-for-wheel-routing workaround
    -- (uiManager.onTabFrameScroll used to exist solely to satisfy the
    -- pre-#743 "clickable + callback" requirement for wheel routing).
    local frameHandle = tabbar.getFrameHandle(settingsMenu.tabBarId)
    UI.setScrollCapture(frameHandle, true)
    UI.setPointerBlocking(frameHandle, true)
end

-----------------------------------------------------------
-- Create all tab contents
-----------------------------------------------------------

-- Build the param table a tab's create() receives. Shared by the full
-- build (createAllTabs) and the Input-tab-only refresh so both stay in
-- sync. `ts` must already have contentX/contentY/contentW set.
--
-- #748: `contentBase` (falls back to settingsMenu.baseSizes when no
-- shrink is needed) shrinks the tab-content-relevant width fields
-- (textboxWidth/sliderWidth/checkboxSize) uniformly by one factor when
-- the tab content area is too narrow for them — e.g. the supported
-- 800x2160@4x combination, where the panel's own side padding alone
-- leaves only ~80px of tab content width, far less than a slider's
-- unshrunk 200px base width at 4x. Mirrors create_world_menu's
-- identical technique for its own left-panel controls: a shallow-
-- copied baseSizes table, so the tab modules themselves (which just
-- read whatever `params.baseSizes` they're handed) need no changes.
function settingsMenu.tabCreateParams(uiscale, s, ts, contentBase)
    return {
        page            = settingsMenu.page,
        font            = settingsMenu.menuFont,
        baseSizes       = contentBase or settingsMenu.baseSizes,
        uiscale         = uiscale,
        s               = s,
        contentX        = ts.contentX,
        contentY        = ts.contentY,
        contentW        = ts.contentW,
        zContent        = Z_CONTENT,
        zWidgets        = Z_WIDGETS,
        currentSettings = data.current,
        pendingSettings = data.pending,
        -- Tracking functions so tabs can register their widgets for the
        -- scoped cleanup (destroyOwned).
        trackLabel      = settingsMenu.trackLabel,
        trackTextbox    = settingsMenu.trackTextbox,
        trackCheckbox   = settingsMenu.trackCheckbox,
        trackDropdown   = settingsMenu.trackDropdown,
        trackButton     = settingsMenu.trackButton,
        -- Extras the input (keybind) tab needs for its key buttons and
        -- capture/conflict popups.
        panelTexSet     = settingsMenu.panelTexSet,
        buttonTexSet    = settingsMenu.buttonTexSet,
        fbW             = settingsMenu.fbW,
        fbH             = settingsMenu.fbH,
        rebuild         = settingsMenu.refreshInputTab,
    }
end

function settingsMenu.createAllTabs(s, uiscale)
    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(settingsMenu.tabBarId)
    local pad = math.floor(20 * uiscale)
    local contentX = frameX + pad
    local contentY = frameY + pad
    local contentW = frameW - pad * 2
    local contentH = frameH - pad * 2

    local maxVisibleRows = math.max(1, math.floor(contentH / s.rowSpacing))

    -- #748: see tabCreateParams' comment above.
    local base = settingsMenu.baseSizes
    local widestControlBase = math.max(base.sliderWidth or 200, base.textboxWidth)
    local naturalWidestControl = widestControlBase * uiscale
    local contentBase = base
    if naturalWidestControl > 0 and naturalWidestControl > contentW * 0.9 then
        local factor = (contentW * 0.9) / naturalWidestControl
        contentBase = {}
        for k, v in pairs(base) do contentBase[k] = v end
        contentBase.textboxWidth = base.textboxWidth * factor
        contentBase.sliderWidth = (base.sliderWidth or 200) * factor
        contentBase.checkboxSize = base.checkboxSize * factor
    end

    for _, def in ipairs(tabDefs) do
        local ts      = settingsMenu.tabScroll[def.key]
        ts.contentX   = contentX
        ts.contentY   = contentY
        ts.contentW   = contentW
        ts.rowSpacing = s.rowSpacing
        ts.scrollOffset = 0

        -- Call the tab's create function
        ts.rowHandles = def.create(settingsMenu.tabCreateParams(uiscale, s, ts, contentBase))

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
    -- #748: shrink all four bottom-action buttons uniformly if their
    -- natural width would overflow the panel's content area (e.g. the
    -- supported envelope's 800px-wide minimum, where the unshrunk row
    -- used to push Back/Save off the panel/framebuffer) — mirrors
    -- create_world/bottom_buttons.lua's identical technique.
    local base = settingsMenu.baseSizes
    local naturalWidth = base.btnWidth * 4 * uiscale + base.btnSpacing * 3 * uiscale
    local factor = 1.0
    if naturalWidth > 0 and naturalWidth > bounds.width then
        factor = bounds.width / naturalWidth
    end
    local btnW = math.floor(base.btnWidth * factor)
    local spacing = math.floor(s.btnSpacing * factor)
    -- #748 round 6: button.new scales `fontSize` by `uiscale` internally
    -- exactly like it scales `width` — shrinking only btnW while leaving
    -- fontSize at the unshrunk base.fontSize left the label rendering at
    -- full size inside a shrunk box (overflow/overlap at the supported
    -- 800x2160@4x combination). Apply the SAME `factor` to the font size
    -- base passed in, so button.new's own `* uiscale` multiplication
    -- ends up shrinking both by the identical ratio.
    local btnFontSize = base.fontSize * factor

    settingsMenu.backButtonId = settingsMenu.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = btnW,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = btnFontSize,
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
                settingsMenu.showMenuCallback("back")
            end
        end,
    }))

    settingsMenu.defaultsButtonId = settingsMenu.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = btnW,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = btnFontSize,
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
        width      = btnW,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = btnFontSize,
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
        width      = btnW,
        height     = settingsMenu.baseSizes.btnHeight,
        fontSize   = btnFontSize,
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
    local totalBtnW      = backW + spacing + defaultsW + spacing
                           + applyW + spacing + saveW
    local btnStartX      = panelX + bounds.x + (bounds.width - totalBtnW) / 2
    local bottomPad      = math.floor(100 * uiscale)
    local btnY           = panelY + panelHeight - bottomPad
                           + (bottomPad - backH) / 2

    local backH_   = button.getElementHandle(settingsMenu.backButtonId)
    local defaultsH_   = button.getElementHandle(settingsMenu.defaultsButtonId)
    local applyH_  = button.getElementHandle(settingsMenu.applyButtonId)
    local saveH_   = button.getElementHandle(settingsMenu.saveButtonId)

    UI.setPosition(backH_,     btnStartX, btnY)
    UI.setPosition(defaultsH_, btnStartX + backW + spacing, btnY)
    UI.setPosition(applyH_,    btnStartX + backW + spacing
                               + defaultsW + spacing, btnY)
    UI.setPosition(saveH_,     btnStartX + backW + spacing
                               + defaultsW + spacing
                               + applyW + spacing, btnY)
    
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

-- #748: a scale change fans out through the shared notification
-- contract instead of settings_menu rebuilding only itself — every
-- other already-initialized screen (main menu, create-world, ...)
-- picks up the new scale immediately too, not just on its next own
-- show(). Reuses the current fbW/fbH (only the scale changed).
function settingsMenu.onApply()
    engine.logInfo("Applying settings...")
    local vals = graphicsTab.getWidgetValues()
    local result = data.apply(vals)
    if result.scaleChanged then
        -- notifyResize -> settingsMenu.onFramebufferResize already
        -- re-shows itself (and restores control focus) when it was
        -- visible, which it always is here (this IS the visible
        -- settings screen the user just clicked Apply on).
        responsive.notifyResize(settingsMenu.fbW, settingsMenu.fbH)
    end
end

function settingsMenu.onSave()
    engine.logInfo("Saving settings...")
    local vals = graphicsTab.getWidgetValues()
    local result = data.save(vals)
    if result.scaleChanged then
        responsive.notifyResize(settingsMenu.fbW, settingsMenu.fbH)
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
    if settingsMenu.uiCreated then
        -- #748: keyboard CONTROL focus (#745) can only be restored
        -- once the rebuilt page is genuinely visible again — the
        -- engine's UI.getVisibleElements() (which restoreControlFocusName
        -- searches) only ever considers visible pages, and createUI()
        -- itself never shows the fresh page it builds (some callers,
        -- e.g. init(), deliberately want it built-but-hidden). Guarding
        -- the re-show on wasVisible (queried BEFORE teardown) mirrors
        -- pause_menu's own visible-only rebuild — a currently-hidden
        -- settings_menu (kept alive lazily in the background) must not
        -- suddenly pop up over whichever menu the resize actually hit.
        local wasVisible = settingsMenu.page and UI.isPageVisible(settingsMenu.page)
        local controlFocusName = wasVisible and responsive.snapshotControlFocusName()
        settingsMenu.createUI({ preserveState = true })
        if wasVisible and settingsMenu.page then
            UI.showPage(settingsMenu.page)
            responsive.restoreControlFocusName(controlFocusName)
        end
    end
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
