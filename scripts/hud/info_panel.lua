-- Info Panel for HUD
-- Creates a tabbed panel (Basic / Advanced) in the top-right corner
-- of the screen.  Each tab holds a single block of pre-formatted text.
-- The panel auto-hides when both tabs have empty text and auto-shows
-- when either tab receives content.
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
local tabDefs = {
    { key = "basic",    name = "Basic" },
    { key = "advanced", name = "Advanced" },
}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
infoPanel.panelId    = nil
infoPanel.tabBarId   = nil
infoPanel.activeTab  = "basic"
infoPanel.visible    = false

-- Per-tab text content and label IDs
infoPanel.tabText = {
    basic    = "",
    advanced = "",
}
infoPanel.tabLabelIds = {
    basic    = nil,
    advanced = nil,
}

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
    infoPanel.tabLabelIds  = { basic = nil, advanced = nil }
end

-----------------------------------------------------------
-- Visibility helpers
-----------------------------------------------------------
local function hasContent()
    return (infoPanel.tabText.basic    ~= "")
        or (infoPanel.tabText.advanced ~= "")
end

-- Hide/show all tab button boxes in the tabbar.
-- The tabbar module does not expose a setVisible, so we
-- reach into it via getFrameHandle and the internal tab
-- data.  We hide the frame plus each tab box individually.
local function setTabBarVisible(vis)
    if not infoPanel.tabBarId then return end
    -- Hide/show the content frame
    local fh = tabbar.getFrameHandle(infoPanel.tabBarId)
    if fh then UI.setVisible(fh, vis) end
    -- Hide/show each individual tab button box.
    -- tabbar stores tabs internally; we can access them
    -- through the tabbar's selected-index query to confirm
    -- the tabbar exists, then iterate using findByElementHandle
    -- approach.  But the simplest fix is to query the tab count
    -- and toggle each one.
    --
    -- Since tabbar doesn't expose tab handles directly, we need
    -- to track them ourselves at creation time.
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
    local tabList = {}
    for _, def in ipairs(tabDefs) do
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

    -- Collect the tab button box handles so we can hide/show them.
    -- The tabbar module creates tab boxes as root page elements.
    -- We discover them by probing findByElementHandle, but that
    -- requires an element handle we don't have.  Instead, we use
    -- the tabbar's internal structure: after creation the tab
    -- boxes are the last N elements added to the page.  The most
    -- robust approach is to record them via a small helper:
    infoPanel.tabBoxHandles = {}
    for i = 1, #tabList do
        -- The tabbar creates boxes named "hud_info_tabs_tab_N"
        -- We can find them by trying selectByKey and then
        -- using findByElementHandle... but the simplest is to
        -- note that tabbar stores them in its internal state.
        -- Since we can't access that directly, let's use a
        -- different approach: probe every element index.
        -- Actually, let's just query them via the module.
        -- tabbar.findByElementHandle won't help without a handle.
        --
        -- The cleanest solution: iterate a range of plausible
        -- element handles.  But that's fragile.
        --
        -- Best approach: we know each tab is an addToPage root
        -- element. The tabbar.getFrameHandle gives us the frame.
        -- The tab boxes are siblings.  Let's just keep them
        -- always visible/hidden via the page visibility itself,
        -- by using a SEPARATE page for the info panel.
    end

    tabbar.selectByKey(infoPanel.tabBarId, infoPanel.activeTab)

    ---------------------------------------------------------
    -- Content labels (one per tab, inside the tab frame)
    ---------------------------------------------------------
    local frameX, frameY, frameW, frameH =
        tabbar.getFrameBounds(infoPanel.tabBarId)
    local contentPad = math.floor(8 * uiscale)

    for _, def in ipairs(tabDefs) do
        local lid = trackLabel(label.new({
            name     = "hud_info_" .. def.key,
            text     = infoPanel.tabText[def.key],
            font     = menuFont,
            fontSize = base.fontSize,
            color    = {0.9, 0.9, 0.9, 1.0},
            page     = page,
            uiscale  = uiscale,
        }))
        local lh = label.getElementHandle(lid)
        UI.addToPage(page, lh,
            frameX + contentPad,
            frameY + contentPad + s.fontSize)
        UI.setZIndex(lh, Z_CONTENT)
        infoPanel.tabLabelIds[def.key] = lid
    end

    -- Show only the active tab's label
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

    engine.logDebug("HUD info panel created")
end

-----------------------------------------------------------
-- Show / hide individual tab labels
-----------------------------------------------------------
function infoPanel.showTab(key)
    for _, def in ipairs(tabDefs) do
        local lid = infoPanel.tabLabelIds[def.key]
        if lid then
            label.setVisible(lid, def.key == key)
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

-- Sets the text for the given tab ("basic" or "advanced").
-- If both tabs become empty, the panel hides itself.
-- If either tab has content, the panel shows itself.
function infoPanel.setText(tabKey, text)
    text = text or ""
    infoPanel.tabText[tabKey] = text

    local lid = infoPanel.tabLabelIds[tabKey]
    if lid then
        label.setText(lid, text)
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

-- Convenience: set both tabs at once
function infoPanel.setInfo(basicText, advancedText)
    infoPanel.setText("basic",    basicText    or "")
    infoPanel.setText("advanced", advancedText or "")
end

-- Clear all text (hides the panel)
function infoPanel.clear()
    infoPanel.setText("basic",    "")
    infoPanel.setText("advanced", "")
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function infoPanel.isVisible()
    return infoPanel.visible
end

return infoPanel
