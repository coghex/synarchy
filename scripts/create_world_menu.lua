-- Create World Menu Module
-- Orchestrates the create world page: panel, title, tab bar, buttons,
-- and preview.  Delegates to sub-modules for tab content, log panel,
-- bottom buttons, and generation logic.
local scale          = require("scripts.ui.scale")
local responsive     = require("scripts.ui.responsive")
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
local generalTab     = require("scripts.create_world.general_tab")
local timelineTab    = require("scripts.create_world.timeline_tab")
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
local Z_TAB_SB_TRACK  = 12
local Z_TAB_SB_BUTTON = 13
local Z_TAB_SB_TAB    = 14
local Z_BUTTONS       = 15

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
    textboxWidth     = 140,
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

-- Pending world parameters (populated from YAML defaults at init time)
createWorldMenu.pending = {
    worldName  = "",
    seed       = "",
    worldSize  = "128",
    plateCount = "10",
    erosionIntensity = "0.7",
    volcanicActivity = "1.0",
    -- Calendar
    daysPerMonth   = "30",
    monthsPerYear  = "12",
    hoursPerDay    = "24",
    minutesPerHour = "60",
    -- Astronomy
    tiltAngle  = "0.4",
    dayLength  = "0.5",
    cycleDays  = "28",
    phaseOffset = "0.0",
    -- Climate
    climateIterations = "50",
    coriolisScale     = "1.0",
    windDrag          = "0.3",
    thermalInertia    = "0.7",
    orographicScale   = "1.5",
    evapScale         = "1.0",
    albedoFeedback    = "0.5",
    thcThreshold      = "1.025",
    -- Timeline depth
    timelineEon = "1",
    timelineEra = "2",
    periodMin   = "1",
    periodMax   = "3",
    epochMin    = "1",
    epochMax    = "3",
    ageMin      = "1",
    ageMax      = "3",
}

-- Format a float for display, avoiding long floating point representations.
-- Uses up to 4 decimal places, strips trailing zeros.
local function fmtFloat(val, fallback)
    local n = val or fallback
    local s = string.format("%.4f", n)
    -- Strip trailing zeros after decimal point
    s = s:gsub("(%..-)0+$", "%1")
    -- Strip trailing decimal point if no fractional part
    s = s:gsub("%.$", ".0")
    return s
end

local function fmtInt(val, fallback)
    return tostring(math.floor(val or fallback))
end

-- Load defaults from config/world_gen_default.yaml via Haskell
function createWorldMenu.loadDefaults()
    local defaults = world.getGenDefaults()
    if defaults then
        createWorldMenu.pending.worldSize  = fmtInt(defaults.world_size, 128)
        createWorldMenu.pending.plateCount = fmtInt(defaults.plate_count, 10)
        createWorldMenu.pending.erosionIntensity = fmtFloat(defaults.erosion_intensity, 0.7)
        createWorldMenu.pending.volcanicActivity = fmtFloat(defaults.volcanic_activity, 1.0)
        createWorldMenu.pending.waterfallQuantum = fmtInt(defaults.waterfall_quantum, 12)
        -- Calendar
        if defaults.calendar then
            local c = defaults.calendar
            createWorldMenu.pending.daysPerMonth   = fmtInt(c.days_per_month, 30)
            createWorldMenu.pending.monthsPerYear  = fmtInt(c.months_per_year, 12)
            createWorldMenu.pending.hoursPerDay    = fmtInt(c.hours_per_day, 24)
            createWorldMenu.pending.minutesPerHour = fmtInt(c.minutes_per_hour, 60)
        end
        -- Astronomy
        if defaults.sun then
            createWorldMenu.pending.tiltAngle = fmtFloat(defaults.sun.tilt_angle, 0.4)
            createWorldMenu.pending.dayLength = fmtFloat(defaults.sun.day_length, 0.5)
        end
        if defaults.moon then
            createWorldMenu.pending.cycleDays   = fmtInt(defaults.moon.cycle_days, 28)
            createWorldMenu.pending.phaseOffset = fmtFloat(defaults.moon.phase_offset, 0.0)
        end
        -- Climate
        if defaults.climate then
            local cl = defaults.climate
            createWorldMenu.pending.climateIterations = fmtInt(cl.iterations, 50)
            createWorldMenu.pending.coriolisScale     = fmtFloat(cl.coriolis_scale, 1.0)
            createWorldMenu.pending.windDrag          = fmtFloat(cl.wind_drag, 0.3)
            createWorldMenu.pending.thermalInertia    = fmtFloat(cl.thermal_inertia, 0.7)
            createWorldMenu.pending.orographicScale   = fmtFloat(cl.orographic_scale, 1.5)
            createWorldMenu.pending.evapScale         = fmtFloat(cl.evap_scale, 1.0)
            createWorldMenu.pending.albedoFeedback    = fmtFloat(cl.albedo_feedback, 0.5)
            createWorldMenu.pending.thcThreshold      = fmtFloat(cl.thc_threshold, 1.025)
        end
        -- Timeline
        if defaults.timeline then
            local t = defaults.timeline
            createWorldMenu.pending.timelineEon = fmtInt(t.eon_count, 1)
            createWorldMenu.pending.timelineEra = fmtInt(t.era_count, 2)
            createWorldMenu.pending.periodMin   = fmtInt(t.period_min, 1)
            createWorldMenu.pending.periodMax   = fmtInt(t.period_max, 3)
            createWorldMenu.pending.epochMin    = fmtInt(t.epoch_min, 1)
            createWorldMenu.pending.epochMax    = fmtInt(t.epoch_max, 3)
            createWorldMenu.pending.ageMin      = fmtInt(t.age_min, 1)
            createWorldMenu.pending.ageMax      = fmtInt(t.age_max, 3)
        end
    end
    return defaults
end

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

-- #748 round 5: per-tab scrollable-content state (key -> {scrollOffset,
-- totalRows, maxVisibleRows, viewportId, scrollContentId, scrollbarId}).
-- See createWorldMenu.createUI's tab-content build for how these are
-- populated — a clipping viewport + a movable scroll-content anchor per
-- tab, mirroring settings_menu's tab scrolling but via real clipping
-- (#747) instead of manual per-row show/hide.
createWorldMenu.tabScroll = {}

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
    { key = "settings", name = "General" },
    { key = "advanced", name = "Geology" },
    { key = "timeline", name = "Timeline" },
}

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

function createWorldMenu.destroyOwned()
    if createWorldMenu.logScrollbarId then
        scrollbar.destroy(createWorldMenu.logScrollbarId)
        createWorldMenu.logScrollbarId = nil
    end

    -- #748 round 5: each tab's scrollbar (scrollable tab content, below)
    -- isn't tracked via the generic owned-lists — the viewport/scroll-
    -- content elements themselves need no explicit cleanup (UI.deletePage
    -- below recursively deletes every element it owns), but the
    -- scrollbar widget module's own bookkeeping does.
    if createWorldMenu.tabScroll then
        for _, ts in pairs(createWorldMenu.tabScroll) do
            if ts.scrollbarId then scrollbar.destroy(ts.scrollbarId) end
        end
    end
    createWorldMenu.tabScroll = {}

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

    -- Load defaults from world_gen_default.yaml
    createWorldMenu.loadDefaults()

    createWorldMenu.worldPreviewTexture =
        engine.loadTexture("assets/textures/ui/world_preview_pending.png")
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

-- opts.preserveState (#748): defaults to true, since createWorldMenu's
-- own `pending` table is already a persistent module field that only
-- onDefaults ever resets — every OTHER rebuild (resize, a fresh
-- preview image arriving mid-generation, re-entering the screen) wants
-- to keep showing whatever the user has in progress, including RAW
-- unsubmitted textbox edits that never made it into `pending` (Plate
-- Count/timeline fields only sync to `pending` at Generate time).
-- onDefaults explicitly passes preserveState=false, the one rebuild
-- that must discard in-progress edits rather than restore them.
function createWorldMenu.createUI(opts)
    opts = opts or {}
    local preserveState = (opts.preserveState == nil) and true or opts.preserveState

    -- #748: preserve the log's clamped scroll position and repaint its
    -- existing lines across a mere rebuild (resize, or a fresh preview
    -- image arriving mid-generation) — createWorldMenu.logLines itself
    -- is never touched here (only logPanelMod.clear does that), but the
    -- fresh label slots logPanelMod.create builds always start blank.
    local prevLogScrollOffset = createWorldMenu.logScrollOffset

    -- #748 round 5: preserve each tab's clamped scroll position across
    -- a mere rebuild too (destroyOwned() below wipes tabScroll).
    local prevTabScrollOffsets = {}
    for key, ts in pairs(createWorldMenu.tabScroll) do
        prevTabScrollOffsets[key] = ts.scrollOffset
    end

    -- Snapshot every textbox's raw (possibly unsubmitted) text, cursor,
    -- and focus BEFORE destroyOwned() tears them down. (Keyboard
    -- CONTROL focus, #745, is captured/restored by the caller instead —
    -- see onFramebufferResize — since restoring it needs the page
    -- already visible, which createUI() itself never makes it.)
    local textboxSnap = nil
    -- #748 round 6: World Name/Seed are randbox (not textbox) controls
    -- — snapshot/restore those too, or an in-progress edit there loses
    -- its text/cursor/focus on a mere rebuild exactly like an unfixed
    -- textbox would.
    local randboxSnap = nil
    if preserveState then
        textboxSnap = textbox.snapshotPage(createWorldMenu.page)
        randboxSnap = randbox.snapshotPage(createWorldMenu.page)
    end

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

    -- #748: compact fallback — the left panel's content width is
    -- derived by peeling off SEVERAL fixed-base paddings that all
    -- scale with uiscale (main panel padding 2*50, left-panel padding
    -- 2*10, tab content padding 2*20 — 100 units of uiscale total),
    -- from a panel width that's a FIXED FRACTION of the framebuffer
    -- (0.85 panel * 0.4 left-split = 0.34) that does NOT scale with
    -- uiscale at all. At a narrow, high-scale supported combination
    -- (e.g. 800x2160@4x) those paddings alone can exceed the whole
    -- left column, driving contentW negative — the per-control shrink
    -- below can't fix that on its own since it only ever narrows the
    -- controls, not the paddings consuming the space around them.
    -- Shrinks this menu's own effective scale (never the stored UI
    -- scale) so the fixed overhead never eats the full budget, leaving
    -- CONTENT_MIN px of real content width regardless of framebuffer
    -- shape.
    local CONTENT_MIN = 150
    local naturalOverhead = 100 * uiscale
    local maxOverhead = 0.34 * createWorldMenu.fbW - CONTENT_MIN
    uiscale = responsive.fitScale(naturalOverhead, maxOverhead, uiscale)

    -- #748: second refinement pass — the tab BAR itself (its three
    -- "General"/"Geology"/"Timeline" buttons, each textWidth + 2*10px
    -- padding per scripts/ui/tabbar.lua) must also fit within
    -- leftBounds.width = 0.34*fbW - 60*uiscale (the same uiscale-
    -- dependent padding chain, one step short of the extra tab-content
    -- padding contentW itself subtracts). The per-control shrink above
    -- only ever narrows CONTROLS inside the tab frame, never the tab
    -- BAR's own buttons, which the previous fallback didn't account
    -- for. Measures real tab-label width (0 headless, like every other
    -- text-width-dependent check in this file — only meaningful with a
    -- real font in a graphical boot) so the fallback only tightens
    -- further when actual label metrics need it to.
    do
        local tabFontSize = math.floor(createWorldMenu.baseSizes.tabFontSize * uiscale)
        local totalTabTextWidth = 0
        for _, def in ipairs(tabDefs) do
            totalTabTextWidth = totalTabTextWidth
                + engine.getTextWidth(createWorldMenu.menuFont, def.name, tabFontSize)
        end
        local naturalTabOverhead = totalTabTextWidth + 120 * uiscale
        local maxTabOverhead = 0.34 * createWorldMenu.fbW
        uiscale = responsive.fitScale(naturalTabOverhead, maxTabOverhead, uiscale)
    end

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

    -- #748 round 5: restore each tab's scroll position (clamped against
    -- the just-rebuilt content size) — mirrors the log scroll restore
    -- below. setScrollOffset triggers the scrollbar's own onScroll ->
    -- createWorldMenu.onTabScroll chain, so this both restores and
    -- repositions the scroll-content anchor in one call.
    if preserveState then
        for key, ts in pairs(createWorldMenu.tabScroll) do
            if ts.scrollbarId and prevTabScrollOffsets[key] then
                scrollbar.setScrollOffset(ts.scrollbarId, prevTabScrollOffsets[key])
            end
        end
    end

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

    if createWorldMenu.logScrollbarId then
        logPanelMod.updateScrollbar(createWorldMenu)
        -- setScrollOffset clamps against the just-updated content size
        -- and repaints via its onScroll -> onLogScroll -> refreshDisplay
        -- chain, so this both restores and redraws in one call.
        scrollbar.setScrollOffset(createWorldMenu.logScrollbarId, prevLogScrollOffset)
    else
        logPanelMod.refreshDisplay(createWorldMenu)
    end

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

    if preserveState then
        textbox.restoreAll(textboxSnap)
        randbox.restoreAll(randboxSnap)
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
-- #748: content-width shrink for the left (tabbed settings) panel
--
-- The left panel's share of the main panel is a fixed 40% split; at
-- the supported envelope's 800px-wide minimum, its content area (~172px)
-- is far narrower than the widest fixed-width control across every
-- tab — World Name's randbox (nameBoxWidth=400 + its own dice-button
-- width), which used to render partly off-screen to the left. Mirrors
-- bottom_buttons.lua's button-bar shrink: one uniform factor, computed
-- from the single widest control, applied to every tab's width-bearing
-- base-size fields via a shallow-copied baseSizes table — the tab
-- modules themselves need no changes, since they already just read
-- whatever `params.baseSizes` they're handed. The dice-button width
-- (randboxHeight) is treated as a fixed, unshrinkable part of that
-- widest control's own total width when solving for the factor.
-----------------------------------------------------------

local function computeContentScaleFactor(base, availableWidth, uiscale)
    local fixedPart = base.randboxHeight * uiscale
    local shrinkablePart = base.nameBoxWidth * uiscale
    if shrinkablePart <= 0 then return 1.0 end
    local budget = availableWidth - fixedPart
    if budget >= shrinkablePart then return 1.0 end
    if budget <= 0 then return 0.1 end
    return budget / shrinkablePart
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

    local contentW = frameW - pad * 2
    local contentH = frameH - pad * 2
    local contentFactor = computeContentScaleFactor(
        createWorldMenu.baseSizes, contentW, uiscale)
    local contentBase = {}
    for k, v in pairs(createWorldMenu.baseSizes) do contentBase[k] = v end
    contentBase.nameBoxWidth = createWorldMenu.baseSizes.nameBoxWidth * contentFactor
    contentBase.randboxWidth = createWorldMenu.baseSizes.randboxWidth * contentFactor
    contentBase.textboxWidth = createWorldMenu.baseSizes.textboxWidth * contentFactor

    -- #748 round 5: scrollable tab content. showTab previously only
    -- toggled per-element visibility — with all rows root-mounted at
    -- absolute positions and no clip/scroll of its own, a tab whose
    -- rows exceed the frame's height (e.g. General's 5 rows at the
    -- formal 800x600@1x minimum, well over its ~152px frame) rendered
    -- rows outside the frame with no way to reach them. Each tab now
    -- gets its own clipping viewport (fixed at the tab frame's bounds)
    -- plus a movable scroll-content anchor as its child; every row
    -- element is parented to that anchor (via each widget's #747
    -- `parent` support) instead of the page root, so a row scrolled
    -- outside the viewport is both visually clipped AND un-hittable
    -- (UI.Clipping backs hit-testing too) with no manual per-row
    -- show/hide needed — scrolling is just moving the anchor's Y.
    local function buildTabViewport(key)
        local viewportId = UI.newElement(
            "cw_tab_" .. key .. "_viewport", contentW, contentH, createWorldMenu.page)
        UI.addToPage(createWorldMenu.page, viewportId, frameX + pad, frameY + pad)
        UI.setClipChildren(viewportId, true)
        UI.setScrollCapture(viewportId, true)
        UI.setZIndex(viewportId, Z_CONTENT)

        local scrollContentId = UI.newElement(
            "cw_tab_" .. key .. "_scroll", contentW, contentH, createWorldMenu.page)
        UI.addChild(viewportId, scrollContentId, 0, 0)

        createWorldMenu.tabScroll[key] = {
            scrollOffset = 0, totalRows = 0, maxVisibleRows = 0,
            viewportId = viewportId, scrollContentId = scrollContentId,
            scrollbarId = nil, rowSpacing = s.rowSpacing,
        }
        return scrollContentId
    end

    local function tabParamsFor(key)
        return {
            page       = createWorldMenu.page,
            font       = createWorldMenu.menuFont,
            baseSizes  = contentBase,
            uiscale    = uiscale,
            s          = s,
            contentX   = 0,
            contentY   = 0,
            contentW   = contentW,
            zContent   = Z_CONTENT,
            zWidgets   = Z_WIDGETS,
            pending    = createWorldMenu.pending,
            container  = buildTabViewport(key),
            trackLabel    = createWorldMenu.trackLabel,
            trackRandBox  = createWorldMenu.trackRandBox,
            trackDropdown = createWorldMenu.trackDropdown,
            trackTextBox  = createWorldMenu.trackTextBox,
        }
    end

    -- General tab: name/seed/size plus the active calendar settings,
    -- sharing ONE scrollable viewport (they render as a single combined
    -- list under the "General" tab).
    local settingsParams = tabParamsFor("settings")
    local settingsElems, settingsRows = settingsTab.create(settingsParams)
    -- Offset the general tab content below the settings rows
    local generalParams = {}
    for k, v in pairs(settingsParams) do generalParams[k] = v end
    generalParams.contentY = settingsParams.contentY + s.rowSpacing * 3
    local generalElems, generalRows = generalTab.create(generalParams)
    -- Merge both element lists
    local combinedSettings = {}
    for _, e in ipairs(settingsElems) do table.insert(combinedSettings, e) end
    for _, e in ipairs(generalElems) do table.insert(combinedSettings, e) end
    createWorldMenu.tabElements["settings"] = combinedSettings
    createWorldMenu.tabScroll["settings"].totalRows = settingsRows + generalRows

    local advancedElems, advancedRows = advancedTab.create(tabParamsFor("advanced"))
    createWorldMenu.tabElements["advanced"] = advancedElems
    createWorldMenu.tabScroll["advanced"].totalRows = advancedRows

    local timelineElems, timelineRows = timelineTab.create(tabParamsFor("timeline"))
    createWorldMenu.tabElements["timeline"] = timelineElems
    createWorldMenu.tabScroll["timeline"].totalRows = timelineRows

    local maxVisibleRows = math.max(1, math.floor(contentH / s.rowSpacing))
    for key, ts in pairs(createWorldMenu.tabScroll) do
        ts.maxVisibleRows = maxVisibleRows
        createWorldMenu.createTabScrollbar(key, frameX, frameY, frameW, frameH, uiscale)
    end

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

    -- #748 round 5: toggle each tab's scrollable-content viewport +
    -- scrollbar too (the per-element toggle above already handles
    -- closing an open dropdown / clearing randbox state on hide — kept
    -- unchanged rather than relying on ancestor-hiding alone for that).
    -- Never resets scrollOffset on switch, so returning to a tab later
    -- keeps its prior scroll position.
    for tabKey, ts in pairs(createWorldMenu.tabScroll) do
        local visible = (tabKey == key)
        UI.setVisible(ts.viewportId, visible)
        if ts.scrollbarId then scrollbar.setVisible(ts.scrollbarId, visible) end
    end
end

-----------------------------------------------------------
-- Tab content scrolling (#748 round 5)
-----------------------------------------------------------

function createWorldMenu.onTabScroll(key, offset)
    local ts = createWorldMenu.tabScroll[key]
    if not ts then return end
    ts.scrollOffset = offset
    UI.setPosition(ts.scrollContentId, 0, -offset * ts.rowSpacing)
end

function createWorldMenu.createTabScrollbar(key, frameX, frameY, frameW, frameH, uiscale)
    local ts = createWorldMenu.tabScroll[key]
    if not ts or ts.totalRows <= ts.maxVisibleRows then return end

    local btnSize = math.floor(24 * uiscale)
    local capH    = math.floor(4 * uiscale)
    local trackH  = math.max(math.floor(20 * uiscale),
                              frameH - btnSize * 2 - capH * 2)

    ts.scrollbarId = scrollbar.new({
        name         = "cw_tab_" .. key .. "_scrollbar",
        page         = createWorldMenu.page,
        x            = frameX + frameW,
        y            = frameY,
        buttonSize   = btnSize,
        trackHeight  = trackH,
        capHeight    = capH,
        tileSize     = math.floor(8 * uiscale),
        totalItems   = ts.totalRows,
        visibleItems = ts.maxVisibleRows,
        uiscale      = uiscale,
        zIndex       = { track = Z_TAB_SB_TRACK, button = Z_TAB_SB_BUTTON,
                         tab = Z_TAB_SB_TAB },
        onScroll = function(offset, sbId, sbName)
            createWorldMenu.onTabScroll(key, offset)
        end,
    })
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

-- #748 round 5: scroll routing for the active tab's scrollable content.
-- The tab's own viewport is the element that actually captures the
-- wheel event (#747, UI.setScrollCapture — routeScroll selects the
-- topmost in-scope capturing surface regardless of which non-capturing
-- row happens to be visually on top of it), so matching just the
-- viewport handle (plus the scrollbar's own elements) is sufficient —
-- mirrors settings_menu.onScroll's equivalent tab-frame-handle check.
function createWorldMenu.tabScrollFor(elemHandle)
    local ts = createWorldMenu.tabScroll[createWorldMenu.activeTab]
    if not ts or not ts.scrollbarId then return nil end

    if elemHandle == ts.viewportId then return ts end

    local sbId, _ = scrollbar.findByElementHandle(elemHandle)
    if sbId and sbId == ts.scrollbarId then return ts end

    return nil
end

function createWorldMenu.onScroll(elemHandle, dx, dy)
    local ts = createWorldMenu.tabScrollFor(elemHandle)
    if ts then
        if     dy > 0 then scrollbar.scrollUp(ts.scrollbarId)
        elseif dy < 0 then scrollbar.scrollDown(ts.scrollbarId)
        end
        return true
    end

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
    local ts = createWorldMenu.tabScrollFor(elemHandle)
    if ts then
        if callbackName == "onScrollUp" then
            scrollbar.scrollUp(ts.scrollbarId)
            return true
        elseif callbackName == "onScrollDown" then
            scrollbar.scrollDown(ts.scrollbarId)
            return true
        end
    end

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
    engine.logInfo("Loading create world defaults from config...")
    createWorldMenu.pending = {
        worldName  = "",
        seed       = "",
        worldSize  = "128",
        plateCount = "10",
        erosionIntensity = "0.7",
        volcanicActivity = "1.0",
        daysPerMonth   = "30",
        monthsPerYear  = "12",
        hoursPerDay    = "24",
        minutesPerHour = "60",
        tiltAngle  = "0.4",
        dayLength  = "0.5",
        cycleDays  = "28",
        phaseOffset = "0.0",
        climateIterations = "50",
        coriolisScale     = "1.0",
        windDrag          = "0.3",
        thermalInertia    = "0.7",
        orographicScale   = "1.5",
        evapScale         = "1.0",
        albedoFeedback    = "0.5",
        thcThreshold      = "1.025",
    }
    createWorldMenu.loadDefaults()
    createWorldMenu.genState = generation.IDLE
    logPanelMod.clear(createWorldMenu)
    -- #748: the one rebuild that must discard in-progress edits
    -- (the pending table was just reset above) rather than restore
    -- them from the about-to-be-destroyed widgets.
    createWorldMenu.createUI({ preserveState = false })
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
        local phase, current, total = world.getInitProgress()

        if phase == 1 and total > 0 then
            -- Phase 1: setup steps (timeline, ocean, climate, etc.)
            -- Map to 0..0.5 of the bar
            local progress = (current / total) * 0.5
            bar.setProgress(createWorldMenu.genBarId, progress)
            bar.setText(createWorldMenu.genBarId,
                "Setup " .. tostring(current) .. "/" .. tostring(total))
        elseif phase == 2 and total > 0 then
            -- Phase 2: chunk generation
            -- Map to 0.5..1.0 of the bar
            local progress = 0.5 + (current / total) * 0.5
            bar.setProgress(createWorldMenu.genBarId, progress)
            bar.setText(createWorldMenu.genBarId,
                "Chunks " .. tostring(current) .. "/" .. tostring(total))
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
    if createWorldMenu.uiCreated then
        -- #748: keyboard CONTROL focus (#745) can only be restored once
        -- the rebuilt page is genuinely visible again (see
        -- settings_menu.onFramebufferResize's identical comment) — a
        -- currently-hidden create-world menu must not suddenly pop up
        -- over whichever menu the resize actually hit.
        local wasVisible = createWorldMenu.page and UI.isPageVisible(createWorldMenu.page)
        local controlFocusName = wasVisible and responsive.snapshotControlFocusName()
        createWorldMenu.createUI()
        if wasVisible and createWorldMenu.page then
            UI.showPage(createWorldMenu.page)
            responsive.restoreControlFocusName(controlFocusName)
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function createWorldMenu.shutdown()
    createWorldMenu.destroyOwned()
    if createWorldMenu.page then UI.deletePage(createWorldMenu.page) end
end

return createWorldMenu
