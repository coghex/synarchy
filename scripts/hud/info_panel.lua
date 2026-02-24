-- Info Panel for HUD
-- Creates a tabbed panel (Basic / Advanced) in the top-right corner
-- of the screen.  Each tab holds a single block of pre-formatted text.
-- The panel auto-hides when both tabs have empty text and auto-shows
-- when either tab receives content.
--
-- An optional third tab ("Weather") appears only when weather text
-- is set, and disappears when it is cleared.
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
    -- Fraction of framebuffer
    widthFrac   = 0.20,
    heightFrac  = 0.33,
}

-----------------------------------------------------------
-- Tab definitions
-----------------------------------------------------------
local coreTabDefs = {
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

-- Per-tab text content (stored, not displayed directly)
infoPanel.tabText = {
    basic    = "",
    weather  = "",
    advanced = "",
}

-- Single content label shared across all tabs
infoPanel.contentLabelId = nil

-- Whether the weather tab is currently built into the UI
infoPanel.weatherTabBuilt = false

-- Cached create params so we can rebuild on the fly
infoPanel.createParams = nil

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
    infoPanel.contentLabelId = nil
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function hasContent()
    return (infoPanel.tabText.basic    ~= "")
        or (infoPanel.tabText.weather  ~= "")
        or (infoPanel.tabText.advanced ~= "")
end

local function weatherHasContent()
    return infoPanel.tabText.weather ~= ""
end

-- Build the list of tab defs that should currently exist
local function buildTabList()
    local tabs = {}
    for _, def in ipairs(coreTabDefs) do
        table.insert(tabs, { name = def.name, key = def.key })
    end
    if weatherHasContent() then
        table.insert(tabs, { name = weatherTabDef.name,
                             key  = weatherTabDef.key })
    end
    return tabs
end

-----------------------------------------------------------
-- Create / Rebuild
-----------------------------------------------------------

-- params = {
--   page, boxTexSet, menuFont, fbW, fbH,
-- }
function infoPanel.create(params)
    infoPanel.destroyOwned()

    -- Cache params for later rebuilds
    infoPanel.createParams = params

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
    -- Tab bar (tabs chosen dynamically)
    ---------------------------------------------------------
    local tabList = buildTabList()
    infoPanel.weatherTabBuilt = weatherHasContent()

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
            infoPanel.refreshContent()
        end,
    }))

    -- If the saved activeTab no longer exists (e.g. weather was removed),
    -- fall back to "basic"
    local activeValid = false
    for _, t in ipairs(tabList) do
        if t.key == infoPanel.activeTab then activeValid = true; break end
    end
    if not activeValid then
        infoPanel.activeTab = "basic"
    end

    tabbar.selectByKey(infoPanel.tabBarId, infoPanel.activeTab)

    ---------------------------------------------------------
    -- Single content label (shared across all tabs)
    ---------------------------------------------------------
    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(infoPanel.tabBarId)
    local contentPad = math.floor(8 * uiscale)

    infoPanel.contentLabelId = trackLabel(label.new({
        name     = "hud_info_content",
        text     = infoPanel.tabText[infoPanel.activeTab] or "",
        font     = menuFont,
        fontSize = base.fontSize,
        color    = {0.9, 0.9, 0.9, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local lh = label.getElementHandle(infoPanel.contentLabelId)
    UI.addToPage(page, lh,
        frameX + contentPad,
        frameY + contentPad + s.fontSize)
    UI.setZIndex(lh, Z_CONTENT)

    -- Start hidden via page visibility
    infoPanel.visible = false
    UI.hidePage(page)

    -- If we already have content (e.g. rebuild after resize), show it
    if hasContent() then
        infoPanel.visible = true
        UI.showPage(page)
    end

    engine.logDebug("HUD info panel created (weather tab: "
        .. tostring(infoPanel.weatherTabBuilt) .. ")")
end

-----------------------------------------------------------
-- Rebuild (preserving text and active tab)
-----------------------------------------------------------

local function rebuild()
    if not infoPanel.createParams then return end
    infoPanel.create(infoPanel.createParams)
end

-----------------------------------------------------------
-- Refresh the single content label with the active tab's text
-----------------------------------------------------------

function infoPanel.refreshContent()
    if not infoPanel.contentLabelId then return end
    local text = infoPanel.tabText[infoPanel.activeTab] or ""
    local lh = label.getElementHandle(infoPanel.contentLabelId)
    UI.setText(lh, text)
end

-----------------------------------------------------------
-- Show / hide the entire panel via its dedicated page
-----------------------------------------------------------
function infoPanel.setAllVisible(vis)
    if not infoPanel.page then return end
    if vis then
        UI.showPage(infoPanel.page)
    else
        UI.hidePage(infoPanel.page)
    end
end

-----------------------------------------------------------
-- Public API: set text for a tab
-----------------------------------------------------------

function infoPanel.setText(tabKey, text)
    text = text or ""
    infoPanel.tabText[tabKey] = text

    -- Check whether the weather tab needs to appear or disappear
    if tabKey == "weather" then
        local needsWeatherTab = (text ~= "")
        if needsWeatherTab ~= infoPanel.weatherTabBuilt then
            if not needsWeatherTab
               and infoPanel.activeTab == "weather" then
                infoPanel.activeTab = "basic"
            end
            rebuild()
            return
        end
    end

    -- If this is the active tab, update the displayed text immediately
    if tabKey == infoPanel.activeTab then
        infoPanel.refreshContent()
    end

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

-- Convenience: set both core tabs at once
function infoPanel.setInfo(basicText, advancedText)
    infoPanel.setText("basic",    basicText    or "")
    infoPanel.setText("advanced", advancedText or "")
end

-- Convenience: set the optional weather tab
function infoPanel.setWeatherInfo(weatherText)
    infoPanel.setText("weather", weatherText or "")
end

-- Clear all text (hides the panel, removes weather tab)
function infoPanel.clear()
    infoPanel.tabText.weather  = ""
    infoPanel.tabText.basic    = ""
    infoPanel.tabText.advanced = ""
    infoPanel.weatherTabBuilt  = false
    infoPanel.activeTab = "basic"
    rebuild()
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function infoPanel.isVisible()
    return infoPanel.visible
end

return infoPanel
