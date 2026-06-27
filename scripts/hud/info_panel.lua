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
-- Tab schemas
-----------------------------------------------------------
-- The panel is shared between the tile-info system and the unit-info
-- watcher; they need different tab layouts. The active schema is
-- chosen via infoPanel.useSchema("tile" | "unit"), which destroys the
-- existing tabbar and rebuilds with the new tab set. Tab keys must
-- be globally unique across all schemas (we share one tabText map).
local schemas = {
    tile = {
        { key = "basic",    name = "Basic" },
        { key = "advanced", name = "Advanced" },
        -- "resources" and "weather" appear dynamically when they
        -- have text (chunk selection on the zoom map pushes both).
    },
    unit = {
        { key = "status",    name = "Status" },
        { key = "physical",  name = "Physical" },
        { key = "mental",    name = "Mental" },
        { key = "skills",    name = "Skills" },
        { key = "inventory", name = "Inventory" },
    },
    building = {
        { key = "binfo", name = "Info" },
    },
    grounditem = {
        { key = "iteminfo", name = "Item" },
    },
}
local weatherTabDef   = { key = "weather",   name = "Weather" }
local resourcesTabDef = { key = "resources", name = "Resources" }

-- All tab keys we know about. Listed once so per-tab maps can be
-- initialized with empty values for every key.
local allKeys = {
    "basic", "advanced", "weather", "resources",
    "status", "physical", "mental", "skills", "inventory",
    "binfo", "iteminfo",
}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
infoPanel.panelId    = nil
infoPanel.tabBarId   = nil
infoPanel.activeSchema = "tile"
infoPanel.activeTab  = "basic"
infoPanel.visible    = false
infoPanel.page       = nil
infoPanel.weatherTabActive   = false  -- whether the weather tab exists
infoPanel.resourcesTabActive = false  -- whether the resources tab exists

-- Per-tab text content (raw multi-line string). Initialized lazily
-- so any tab key from `allKeys` is safe to write to.
infoPanel.tabText    = {}
infoPanel.tabLineIds = {}
for _, k in ipairs(allKeys) do
    infoPanel.tabText[k]    = ""
    infoPanel.tabLineIds[k] = {}
end

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
    for _, k in ipairs(allKeys) do
        infoPanel.tabLineIds[k] = {}
    end
end

-----------------------------------------------------------
-- Visibility helpers
-----------------------------------------------------------
-- True if any tab in the ACTIVE schema has content. Tabs from the
-- inactive schema (residual unit text while showing tile, etc.) do
-- not count.
local function hasContent()
    local defs = schemas[infoPanel.activeSchema]
    if defs then
        for _, def in ipairs(defs) do
            if infoPanel.tabText[def.key] and infoPanel.tabText[def.key] ~= "" then
                return true
            end
        end
    end
    -- Tile schema's weather/resources tabs are dynamic; count if active.
    if infoPanel.activeSchema == "tile" and infoPanel.weatherTabActive
       and infoPanel.tabText.weather ~= "" then
        return true
    end
    if infoPanel.activeSchema == "tile" and infoPanel.resourcesTabActive
       and infoPanel.tabText.resources ~= "" then
        return true
    end
    return false
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
    local schemaDefs = schemas[infoPanel.activeSchema] or schemas.tile
    for _, def in ipairs(schemaDefs) do
        table.insert(defs, def)
    end
    -- Resources and weather are special tile-schema add-ons.
    if infoPanel.activeSchema == "tile" and infoPanel.resourcesTabActive then
        table.insert(defs, resourcesTabDef)
    end
    if infoPanel.activeSchema == "tile" and infoPanel.weatherTabActive then
        table.insert(defs, weatherTabDef)
    end
    return defs
end

-- Picks a sensible default active tab for the current schema (first
-- tab). Used after a schema swap when the previous activeTab key may
-- not exist in the new schema.
local function defaultTabKey()
    local defs = currentTabDefs()
    return (defs[1] and defs[1].key) or "basic"
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

    -- Decide whether to include the dynamic tabs
    infoPanel.weatherTabActive   = (infoPanel.tabText.weather ~= "")
    infoPanel.resourcesTabActive = (infoPanel.tabText.resources ~= "")

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

    -- If the active tab key isn't in the current schema's defs, fall
    -- back to the first tab (covers schema-swap + weather-disappeared
    -- cases in one rule).
    local activeFound = false
    for _, def in ipairs(defs) do
        if def.key == infoPanel.activeTab then activeFound = true; break end
    end
    if not activeFound then
        infoPanel.activeTab = defaultTabKey()
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
-- Canonical mutator for the panel's page visibility. ALWAYS go through
-- this (never call UI.showPage/UI.hidePage on infoPanel.page directly)
-- so the page state and the infoPanel.visible flag can never diverge.
-- Direct page calls leave infoPanel.visible stale, after which setText's
-- auto-show is gated on a lie and the panel sticks hidden/shown (#134).
function infoPanel.setAllVisible(vis)
    if not infoPanel.page then return end
    infoPanel.visible = vis
    if vis then
        UI.showPage(infoPanel.page)
        infoPanel.showTab(infoPanel.activeTab)
    else
        UI.hidePage(infoPanel.page)
    end
end

-- Re-derive visibility from current content. Use this to restore the
-- panel after an external hide (HUD hide/show, unit_info_v2 takeover):
-- the stored tab text survives a hide, so we can show iff there is
-- content and stay hidden otherwise — instead of unconditionally
-- showing (which resurfaces an empty/stale panel) or never showing
-- (which leaves real content hidden forever). See #134.
function infoPanel.refresh()
    if not infoPanel.page then return end
    infoPanel.setAllVisible(hasContent())
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

    -- If a dynamic tab (weather/resources) appeared or disappeared,
    -- we need a full rebuild of the tabbar.
    local dynFlag = nil
    if tabKey == "weather"   then dynFlag = "weatherTabActive"   end
    if tabKey == "resources" then dynFlag = "resourcesTabActive" end
    if dynFlag then
        local wasActive = infoPanel[dynFlag]
        local shouldBeActive = (text ~= "")
        if wasActive ~= shouldBeActive then
            -- Need to rebuild the tabbar to add/remove the tab. But we
            -- can only do that if we have the create params (stored on
            -- the first createUI; the rebuild re-reads tabText).
            infoPanel[dynFlag] = shouldBeActive
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

-- Convenience: set tile-info both tabs at once. Switches to the tile
-- schema first if needed so subsequent tile pushes land on visible
-- basic/advanced tabs rather than living on (now-hidden) labels.
function infoPanel.setInfo(basicText, advancedText)
    infoPanel.useSchema("tile")
    infoPanel.setText("basic",    basicText    or "")
    infoPanel.setText("advanced", advancedText or "")
end

function infoPanel.setWeatherInfo(weatherText)
    infoPanel.setText("weather", weatherText or "")
end

-- Resources push (zoom-map chunk selection): per-ore surviving tile
-- counts. Empty string removes the tab.
function infoPanel.setResourcesInfo(resourcesText)
    infoPanel.setText("resources", resourcesText or "")
end

-- Unit-info push: switches to the unit schema and writes the five
-- per-tab strings at once. Any of them may be "" (the tab still
-- exists but renders empty).
function infoPanel.setUnitInfo(statusText, physicalText, mentalText,
                                skillsText, inventoryText)
    infoPanel.useSchema("unit")
    infoPanel.setText("status",    statusText    or "")
    infoPanel.setText("physical",  physicalText  or "")
    infoPanel.setText("mental",    mentalText    or "")
    infoPanel.setText("skills",    skillsText    or "")
    infoPanel.setText("inventory", inventoryText or "")
end

-- Building-info push: switches to the building schema (one tab) and
-- writes the info text. Empty string hides the panel.
function infoPanel.setBuildingInfo(infoText)
    infoPanel.useSchema("building")
    infoPanel.setText("binfo", infoText or "")
end

-- Ground-item push: same single-tab pattern as buildings.
function infoPanel.setGroundItemInfo(infoText)
    infoPanel.useSchema("grounditem")
    infoPanel.setText("iteminfo", infoText or "")
end

-- Swap the tab schema in-place. Rebuilds the tabbar and labels.
-- Clears any text from the previous schema's tabs so they don't
-- linger if the panel is later switched back.
function infoPanel.useSchema(name)
    if name == infoPanel.activeSchema then return end
    if not schemas[name] then return end

    -- Wipe out the OUTGOING schema's text. The user-facing semantic is
    -- "selecting a unit replaces the tile readout, and vice versa".
    local prev = schemas[infoPanel.activeSchema]
    if prev then
        for _, def in ipairs(prev) do
            infoPanel.tabText[def.key] = ""
        end
    end
    if infoPanel.activeSchema == "tile" then
        infoPanel.tabText.weather = ""
        infoPanel.weatherTabActive = false
        infoPanel.tabText.resources = ""
        infoPanel.resourcesTabActive = false
    end

    infoPanel.activeSchema = name
    infoPanel.activeTab    = defaultTabKey()

    if infoPanel.createParams then
        infoPanel.create(infoPanel.createParams)
    end
end

-- Clear all text in the active schema (hides the panel)
function infoPanel.clear()
    local defs = currentTabDefs()
    for _, def in ipairs(defs) do
        infoPanel.setText(def.key, "")
    end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function infoPanel.isVisible()
    return infoPanel.visible
end

return infoPanel
