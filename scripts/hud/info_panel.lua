-- Info Panel for HUD
-- Creates a tabbed panel (Basic / Advanced / Weather) in the top-right
-- corner of the screen.  Each tab holds multiple lines of pre-formatted
-- text.  The panel auto-hides when all tabs are empty and auto-shows
-- when any tab receives content.
local scale   = require("scripts.ui.scale")
local panel   = require("scripts.ui.panel")
local label   = require("scripts.ui.label")
local tabbar  = require("scripts.ui.tabbar")

local infoPanel = {}

-----------------------------------------------------------
-- Z-Index Plan (relative to HUD overlay)
-----------------------------------------------------------
local Z_PANEL       = 50
local Z_TAB_FRAME   = 51
local Z_TAB_BUTTONS = 52
local Z_CONTENT     = 53

-----------------------------------------------------------
-- Base sizes (unscaled)
-----------------------------------------------------------
infoPanel.baseSizes = {
    fontSize    = 16,
    tabHeight   = 28,
    tabFontSize = 16,
    padding     = 10,
    margin      = 16,
    lineSpacing = 1.4,   -- multiplier on fontSize for line height
    -- Fraction of framebuffer
    widthFrac   = 0.20,
    heightFrac  = 0.33,
}

-----------------------------------------------------------
-- Tab definitions
-----------------------------------------------------------
local tabDefs = {
    { key = "basic",    name = "Basic" },
    { key = "advanced", name = "Advanced" },
}
local weatherTabDef = { key = "weather", name = "Weather" }

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
infoPanel.panelId    = nil
infoPanel.tabBarId   = nil
infoPanel.activeTab  = "basic"
infoPanel.visible    = false
infoPanel.page       = nil
infoPanel.weatherTabActive = false  -- whether the weather tab exists

-- Per-tab text content (raw multi-line string)
infoPanel.tabText = {
    basic    = "",
    weather  = "",
    advanced = "",
}

-- Per-tab line label IDs (arrays of label ids, one per visible line slot)
infoPanel.tabLineIds = {
    basic    = {},
    weather  = {},
    advanced = {},
}

-- Max visible lines (computed at create time from panel height)
infoPanel.maxLines = 0

-- Owned IDs for cleanup
infoPanel.ownedPanels  = {}
infoPanel.ownedTabbars = {}
infoPanel.ownedLabels  = {}

-----------------------------------------------------------
-- Tracking helpers
-----------------------------------------------------------
local function trackPanel(id)
    table.insert(infoPanel.ownedPanels, id);  return id end
local function trackTabbar(id)
    table.insert(infoPanel.ownedTabbars, id); return id end
local function trackLabel(id)
    table.insert(infoPanel.ownedLabels, id);  return id end

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------
function infoPanel.destroyOwned()
    for _, id in ipairs(infoPanel.ownedLabels)  do label.destroy(id)  end
    for _, id in ipairs(infoPanel.ownedTabbars) do tabbar.destroy(id) end
    for _, id in ipairs(infoPanel.ownedPanels)  do panel.destroy(id)  end
    infoPanel.ownedLabels  = {}
    infoPanel.ownedTabbars = {}
    infoPanel.ownedPanels  = {}
    infoPanel.panelId      = nil
    infoPanel.tabBarId     = nil
    infoPanel.tabLineIds   = { basic = {}, weather = {}, advanced = {} }
end

-----------------------------------------------------------
-- Visibility helpers
-----------------------------------------------------------
local function hasContent()
    return (infoPanel.tabText.basic    ~= "")
        or (infoPanel.tabText.weather  ~= "")
        or (infoPanel.tabText.advanced ~= "")
end

-----------------------------------------------------------
-- Split a string on newlines
-----------------------------------------------------------
local function splitLines(text)
    if not text or text == "" then return {} end
    local lines = {}
    for line in (text .. "\n"):gmatch("([^\n]*)\n") do
        table.insert(lines, line)
    end
    return lines
end

-----------------------------------------------------------
-- Build the list of tab definitions to use right now
-----------------------------------------------------------
local function currentTabDefs()
    local defs = {}
    for _, def in ipairs(tabDefs) do
        table.insert(defs, def)
    end
    if infoPanel.weatherTabActive then
        table.insert(defs, weatherTabDef)
    end
    return defs
end

-----------------------------------------------------------
-- Create / Rebuild
-----------------------------------------------------------

-- params = {
--   page, boxTexSet, menuFont, fbW, fbH,
-- }
function infoPanel.create(params)
    infoPanel.destroyOwned()

    local page      = params.page
    local boxTexSet = params.boxTexSet
    local menuFont  = params.menuFont
    local fbW       = params.fbW
    local fbH       = params.fbH
    local uiscale   = scale.get()
    local base      = infoPanel.baseSizes
    local s         = scale.applyAllWith(base, uiscale)

    -- Store page reference for later visibility toggling
    infoPanel.page = page
    infoPanel.createParams = params

    -- Decide whether to include weather tab
    infoPanel.weatherTabActive = (infoPanel.tabText.weather ~= "")

    -- Panel dimensions
    local panelWidth  = math.floor(fbW * base.widthFrac)
    local panelHeight = math.floor(fbH * base.heightFrac)
    local panelX = fbW - panelWidth - s.margin
    local panelY = s.margin

    infoPanel.panelId = trackPanel(panel.new({
        name       = "hud_info_panel",
        page       = page,
        x          = panelX,
        y          = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = boxTexSet,
        color      = {0.1, 0.1, 0.1, 0.85},
        tileSize   = 64,
        zIndex     = Z_PANEL,
        padding    = { top = base.padding, bottom = base.padding,
                       left = base.padding, right = base.padding },
        uiscale    = uiscale,
    }))

    local bounds = panel.getContentBounds(infoPanel.panelId)

    ---------------------------------------------------------
    -- Tab bar
    ---------------------------------------------------------
    local defs = currentTabDefs()
    local tabList = {}
    for _, def in ipairs(defs) do
        table.insert(tabList, { name = def.name, key = def.key })
    end

    local tabX = panelX + bounds.x
    local tabY = panelY + bounds.y

    local frameHeight = bounds.height - s.tabHeight
                      - math.floor(10 * uiscale)

    infoPanel.tabBarId = trackTabbar(tabbar.new({
        name              = "hud_info_tabs",
        page              = page,
        x                 = tabX,
        y                 = tabY,
        width             = bounds.width,
        font              = menuFont,
        fontSize          = base.tabFontSize,
        tabHeight         = base.tabHeight,
        frameHeight       = frameHeight,
        uiscale           = uiscale,
        zIndex            = Z_TAB_FRAME,
        textColor         = {0.6, 0.6, 0.6, 1.0},
        selectedTextColor = {1.0, 1.0, 1.0, 1.0},
        tabs              = tabList,
        onChange = function(key, index, tbId)
            infoPanel.activeTab = key
            infoPanel.showTab(key)
        end,
    }))

    -- If the active tab was "weather" but weather tab is now gone,
    -- fall back to "basic"
    if infoPanel.activeTab == "weather" and not infoPanel.weatherTabActive then
        infoPanel.activeTab = "basic"
    end
    tabbar.selectByKey(infoPanel.tabBarId, infoPanel.activeTab)

    ---------------------------------------------------------
    -- Content line labels (one set per tab, inside the frame)
    ---------------------------------------------------------
    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(infoPanel.tabBarId)
    local contentPad = math.floor(8 * uiscale)
    local lineHeight = math.floor(base.fontSize * base.lineSpacing * uiscale)
    local availableH = frameH - contentPad * 2
    local maxLines   = math.max(1, math.floor(availableH / lineHeight))
    infoPanel.maxLines = maxLines

    for _, def in ipairs(defs) do
        local lineIds = {}
        for i = 1, maxLines do
            local lid = trackLabel(label.new({
                name     = "hud_info_" .. def.key .. "_line_" .. i,
                text     = "",
                font     = menuFont,
                fontSize = base.fontSize,
                color    = {0.9, 0.9, 0.9, 1.0},
                page     = page,
                uiscale  = uiscale,
            }))
            local lh = label.getElementHandle(lid)
            UI.addToPage(page, lh,
                frameX + contentPad,
                frameY + contentPad + (i - 1) * lineHeight + s.fontSize)
            UI.setZIndex(lh, Z_CONTENT)
            table.insert(lineIds, lid)
        end
        infoPanel.tabLineIds[def.key] = lineIds
    end

    -- Populate labels from current text
    for _, def in ipairs(defs) do
        infoPanel.refreshTabLines(def.key)
    end

    -- Show only the active tab's labels
    infoPanel.showTab(infoPanel.activeTab)

    -- Start hidden via page visibility
    infoPanel.visible = false
    UI.hidePage(page)

    -- If we already have content (e.g. rebuild after resize), show it
    if hasContent() then
        infoPanel.visible = true
        UI.showPage(page)
        infoPanel.showTab(infoPanel.activeTab)
    end

    engine.logDebug("HUD info panel created: maxLines=" .. maxLines)
end

-----------------------------------------------------------
-- Refresh a single tab's line labels from its stored text
-----------------------------------------------------------
function infoPanel.refreshTabLines(tabKey)
    local lineIds = infoPanel.tabLineIds[tabKey]
    if not lineIds then return end

    local lines = splitLines(infoPanel.tabText[tabKey])
    for i, lid in ipairs(lineIds) do
        local text = lines[i] or ""
        label.setText(lid, text)
    end
end

-----------------------------------------------------------
-- Show / hide individual tab labels
-----------------------------------------------------------
function infoPanel.showTab(key)
    local defs = currentTabDefs()
    for _, def in ipairs(defs) do
        local lineIds = infoPanel.tabLineIds[def.key]
        if lineIds then
            local vis = (def.key == key)
            for _, lid in ipairs(lineIds) do
                label.setVisible(lid, vis)
            end
        end
    end
end

-----------------------------------------------------------
-- Show / hide the entire panel via its dedicated page
-----------------------------------------------------------
function infoPanel.setAllVisible(vis)
    if not infoPanel.page then return end
    if vis then
        UI.showPage(infoPanel.page)
        infoPanel.showTab(infoPanel.activeTab)
    else
        UI.hidePage(infoPanel.page)
    end
end

-----------------------------------------------------------
-- Public API: set text for a tab
-----------------------------------------------------------

-- Sets the text for the given tab ("basic", "advanced", or "weather").
-- Text can contain newline characters which will be split across
-- pre-allocated line labels.
-- If all tabs become empty, the panel hides itself.
-- If any tab has content, the panel shows itself.
function infoPanel.setText(tabKey, text)
    text = text or ""
    local oldText = infoPanel.tabText[tabKey]
    infoPanel.tabText[tabKey] = text

    -- If the weather tab appeared or disappeared, we need a full rebuild
    if tabKey == "weather" then
        local wasActive = infoPanel.weatherTabActive
        local shouldBeActive = (text ~= "")
        if wasActive ~= shouldBeActive then
            -- Need to rebuild the tabbar to add/remove the weather tab.
            -- But we can only do that if we have the create params.
            -- Store a flag so the next createUI call picks it up.
            infoPanel.weatherTabActive = shouldBeActive
            -- For now, if we have a page, rebuild via the stored params
            if infoPanel.createParams then
                infoPanel.create(infoPanel.createParams)
                if hasContent() then
                    infoPanel.visible = true
                    infoPanel.setAllVisible(true)
                end
                return
            end
        end
    end

    -- Fast path: just update the line labels in place
    infoPanel.refreshTabLines(tabKey)

    -- Auto-show / auto-hide
    if hasContent() then
        if not infoPanel.visible then
            infoPanel.visible = true
            infoPanel.setAllVisible(true)
        end
    else
        if infoPanel.visible then
            infoPanel.visible = false
            infoPanel.setAllVisible(false)
        end
    end
end

-- Convenience: set both tabs at once
function infoPanel.setInfo(basicText, advancedText)
    infoPanel.setText("basic",    basicText    or "")
    infoPanel.setText("advanced", advancedText or "")
end

function infoPanel.setWeatherInfo(weatherText)
    infoPanel.setText("weather", weatherText or "")
end

-- Clear all text (hides the panel)
function infoPanel.clear()
    infoPanel.setText("basic",    "")
    infoPanel.setText("advanced", "")
    infoPanel.setText("weather",  "")
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function infoPanel.isVisible()
    return infoPanel.visible
end

return infoPanel
