-- Cargo Inventory Panel
--
-- Floating popup showing a cargo building's stored items as a
-- tabbed icon list. Triggered by right-click on the cargo →
-- context menu → "Contents". Same look as the unit-info inventory
-- section (tabs, icon rows, stacking by defName+quality+condition).
--
-- Singleton: opening for a new cargo destroys the old popup.
-- Pinned at the mouse position when opened; doesn't follow the
-- building if the camera moves. Esc closes; clicking the cargo
-- again re-opens fresh.
--
-- Public API:
--   openFor(bid, mx, my)  — open on this cargo at framebuffer pixel (mx, my)
--   closeIfOpen()         — destroy the popup if shown
--   isOpen()              — bool
--
-- Engine script hooks: setup / init / update / shutdown.
--
-- Module is registered in package.loaded so init.lua's right-click
-- handler and ui_manager's click dispatchers all see the same
-- instance even though engine.loadScript uses dofile.

local cargoInventoryPanel =
    package.loaded["scripts.cargo_inventory_panel"] or {}
package.loaded["scripts.cargo_inventory_panel"] = cargoInventoryPanel

local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")

-----------------------------------------------------------
-- Layout constants. Mirrors unit_info_v2's inventory section so
-- the two read the same visually. Base units; uiscale applied at
-- draw time. Padding clears the 9-patch border art (~16–20 px per
-- side at scale 1) AND leaves visible breathing room — same lesson
-- as the build menu's padding fix.
-----------------------------------------------------------
local PANEL_W_BASE   = 460
local PANEL_PAD_X    = 32
local PANEL_PAD_TOP  = 28
local PANEL_PAD_BOT  = 20
local TITLE_FONT     = 16
local TITLE_H        = 22
local SUBTITLE_FONT  = 13
local SUBTITLE_H     = 18
local TAB_H          = 28
local TAB_TILE       = 16
local TAB_FONT       = 13
local TAB_TEXT_PAD   = 22    -- horizontal pad inside each tab
local TAB_GAP        = 6     -- gap between tabs
local TAB_TEXT_COL   = { 0.0, 0.0, 0.0, 1.0 }
local TAB_SEL_TEXT_COL = { 1.0, 1.0, 1.0, 1.0 }
local ROW_H          = 32
local ROW_PAD        = 2
local ICON_SZ        = 28
local TEXT_PAD       = 12    -- horizontal pad inside each row
local NAME_RIGHT_GAP = 24    -- gap between name and weight columns
local MAX_ROWS       = 10
local TITLE_COL      = { 1.0, 1.0, 1.0, 1.0 }
local SUBTITLE_COL   = { 0.85, 0.85, 0.85, 1.0 }
local ROW_NAME_COL   = { 1.0, 1.0, 1.0, 1.0 }
local ROW_WEIGHT_COL = { 0.85, 0.85, 0.85, 1.0 }

-----------------------------------------------------------
-- State
-----------------------------------------------------------
cargoInventoryPanel.state = cargoInventoryPanel.state or {
    open          = false,
    bid           = nil,
    panelId       = nil,
    activeTab     = "All",
    titleId       = nil,
    subtitleId    = nil,
    tabs          = {},   -- list of {category, boxId, labelId}
    tabsByHandle  = {},   -- handle → category name
    rowElements   = {},   -- list of {kind, id} for cleanup
    rowItems      = {},   -- list of {hitId, item} for right-click
    lastHash      = "",
}

cargoInventoryPanel.hud           = nil   -- assets set by setup()
cargoInventoryPanel.tabSelTexSet  = nil
cargoInventoryPanel.tabUnselTexSet = nil
cargoInventoryPanel.whitePixelTex = nil

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function cargoInventoryPanel.setup(opts)
    cargoInventoryPanel.hud = opts
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- Stack identical entries only when they are truly interchangeable.
-- Same rule as the unit-info inventory: defName + quality + condition +
-- fill, PLUS weight (raw gems roll a per-instance weight, and the row
-- shows weight×count) and — for weapons only — sharpness (two daggers
-- with different edge wear must stay distinct so withdraw can target the
-- exact one). Non-weapon gear also carries a mutated iiSharpness it never
-- shows, so it is NOT split on sharpness (an invisible, confusing split).
-- Anything that still merges is interchangeable, so withdrawing the
-- representative instanceId is always correct (#67).
local function stackKey(it)
    return table.concat({
        it.defName,
        tostring(it.quality     or "_"),
        tostring(it.condition   or "_"),
        tostring(it.currentFill or "_"),
        tostring(it.weight      or "_"),
        it.weapon and tostring(it.sharpness or "_") or "_",
    }, "|")
end

local function groupItems(items)
    local groups = {}
    local seen   = {}
    for _, it in ipairs(items) do
        local k = stackKey(it)
        if seen[k] then
            groups[seen[k]].count = groups[seen[k]].count + 1
        else
            local copy = {}
            for j, v in pairs(it) do copy[j] = v end
            copy.count = 1
            groups[#groups + 1] = copy
            seen[k] = #groups
        end
    end
    return groups
end

-- Snapshot of the cargo's storage that we hash to decide if a
-- rebuild is needed (cheap dedup; avoids reconstructing the whole
-- list every frame).
local function contentHash(bid)
    local stored = building.getStorage(bid) or {}
    local parts = { tostring(#stored) }
    for i, it in ipairs(stored) do
        parts[#parts + 1] = it.defName or "?"
        parts[#parts + 1] = tostring(it.quality     or "_")
        parts[#parts + 1] = tostring(it.condition   or "_")
        -- Fill + weight + (weapon) sharpness mirror stackKey so a change
        -- that splits/merges rows — a deposited canteen at a new level, a
        -- swapped-in gem of a different weight, a re-edged dagger — forces
        -- a panel rebuild (#67).
        parts[#parts + 1] = tostring(it.currentFill or "_")
        parts[#parts + 1] = tostring(it.weight      or "_")
        parts[#parts + 1] = it.weapon and tostring(it.sharpness or "_") or "_"
        if i > 200 then break end
    end
    return table.concat(parts, ",")
end

-- Chebyshev tile distance from (utx, uty) to the cargo footprint.
local function chebToFootprint(utx, uty, bx, by, tileW, tileH)
    local dx = 0
    if utx < bx then dx = bx - utx
    elseif utx >= bx + tileW then dx = utx - (bx + tileW - 1) end
    local dy = 0
    if uty < by then dy = by - uty
    elseif uty >= by + tileH then dy = uty - (by + tileH - 1) end
    return math.max(dx, dy)
end

-- One selected acolyte adjacent to the cargo, or nil.
local function adjacentSelectedUnit(bid)
    local sel = unit.getSelected() or {}
    if #sel == 0 then return nil end
    local binfo = building.getInfo(bid)
    if not binfo then return nil end
    local tw = binfo.tileW or 1
    local th = binfo.tileH or 1
    for _, uid in ipairs(sel) do
        local info = unit.getInfo(uid)
        if info then
            local cheb = chebToFootprint(math.floor(info.gridX),
                                         math.floor(info.gridY),
                                         binfo.gridX, binfo.gridY, tw, th)
            if cheb <= 1 then return uid end
        end
    end
    return nil
end

-----------------------------------------------------------
-- Element teardown
-----------------------------------------------------------
local function destroyTabs()
    local s = cargoInventoryPanel.state
    for _, t in ipairs(s.tabs) do
        if t.labelId then label.destroy(t.labelId) end
        if t.boxId   then UI.deleteElement(t.boxId)  end
    end
    s.tabs         = {}
    s.tabsByHandle = {}
end

local function destroyRows()
    local s = cargoInventoryPanel.state
    for _, e in ipairs(s.rowElements) do
        if e.kind == "label" then label.destroy(e.id)
        else UI.deleteElement(e.id) end
    end
    s.rowElements = {}
    s.rowItems    = {}
end

local function destroyTitle()
    local s = cargoInventoryPanel.state
    if s.titleId    then label.destroy(s.titleId);    s.titleId    = nil end
    if s.subtitleId then label.destroy(s.subtitleId); s.subtitleId = nil end
end

local function destroyAll()
    destroyRows()
    destroyTabs()
    destroyTitle()
    local s = cargoInventoryPanel.state
    if s.panelId then
        panel.destroy(s.panelId)
        s.panelId = nil
    end
end

-----------------------------------------------------------
-- Render: title row
-----------------------------------------------------------
local function buildTitle(originX, originY, bid)
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h then return end
    local uiscale = scale.get()
    local binfo = building.getInfo(bid)
    local name = (binfo and (binfo.displayName or binfo.defName)) or "Cargo"

    s.titleId = label.new({
        name     = "cargo_inv_title",
        text     = name,
        font     = h.menuFont,
        fontSize = TITLE_FONT,
        color    = TITLE_COL,
        page     = h.page,
        uiscale  = uiscale,
    })
    local th = label.getElementHandle(s.titleId)
    UI.addToPage(h.page, th, originX,
                 originY + math.floor(TITLE_FONT * 0.85))
    UI.setZIndex(th, 132)

    local cap  = building.getStorageCapacity(bid) or 0
    local used = building.getStorageWeight(bid)   or 0
    s.subtitleId = label.new({
        name     = "cargo_inv_subtitle",
        text     = string.format("Storage: %.2f / %.2f kg", used, cap),
        font     = h.menuFont,
        fontSize = SUBTITLE_FONT,
        color    = SUBTITLE_COL,
        page     = h.page,
        uiscale  = uiscale,
    })
    local sh = label.getElementHandle(s.subtitleId)
    UI.addToPage(h.page, sh, originX,
                 originY + TITLE_H + math.floor(SUBTITLE_FONT * 0.85))
    UI.setZIndex(sh, 132)
end

-----------------------------------------------------------
-- Render: tab strip ("All" + per-category from stored items)
-----------------------------------------------------------
local function computeTabs(grouped)
    local tabs = { { name = "All", count = #grouped } }
    local seen = { All = true }
    for _, g in ipairs(grouped) do
        local c = g.category or "Misc"
        if not seen[c] then
            seen[c] = true
            tabs[#tabs + 1] = { name = c, count = 0 }
        end
    end
    for _, g in ipairs(grouped) do
        for _, t in ipairs(tabs) do
            if t.name == g.category then t.count = t.count + 1 end
        end
    end
    return tabs
end

local function buildTabStrip(originX, originY, contentW, tabDefs)
    destroyTabs()
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h then return end
    local uiscale = scale.get()
    local tabH    = math.floor(TAB_H * uiscale)
    local fontPx  = math.floor(TAB_FONT * uiscale)
    local padX    = math.floor(TAB_TEXT_PAD * uiscale)

    -- Pre-measure at the SCALED font size — the label renders at
    -- TAB_FONT × uiscale, so measuring at unscaled TAB_FONT
    -- under-sizes the tab box on uiscale > 1 and the text spills
    -- out of its background.
    local widths = {}
    for i, td in ipairs(tabDefs) do
        local text = td.name .. " (" .. td.count .. ")"
        local w = engine.getTextWidth(h.menuFont, text, fontPx) or 0
        widths[i] = math.floor(w) + 2 * padX
    end

    local cursorX = originX
    for i, td in ipairs(tabDefs) do
        local text = td.name .. " (" .. td.count .. ")"
        local active = (td.name == s.activeTab)
        local texSet = active and cargoInventoryPanel.tabSelTexSet
                              or  cargoInventoryPanel.tabUnselTexSet
        local txtCol = active and TAB_SEL_TEXT_COL or TAB_TEXT_COL
        local tabW   = widths[i]

        local bgId = UI.newBox("cargo_inv_tab_bg_" .. i,
            tabW, tabH, texSet, TAB_TILE,
            1.0, 1.0, 1.0, 1.0, 0, h.page)
        UI.addToPage(h.page, bgId, cursorX, originY)
        UI.setZIndex(bgId, 132)
        UI.setClickable(bgId, true)
        UI.setOnClick(bgId, "onCargoInventoryTabClick")

        local lblId = label.new({
            name     = "cargo_inv_tab_lbl_" .. i,
            text     = text,
            font     = h.menuFont,
            fontSize = TAB_FONT,
            color    = txtCol,
            page     = h.page,
            uiscale  = uiscale,
        })
        local lh = label.getElementHandle(lblId)
        local lw = select(1, label.getSize(lblId))
        UI.addToPage(h.page, lh,
            cursorX + math.floor((tabW - lw) / 2),
            originY + math.floor(tabH / 2)
                    + math.floor(TAB_FONT * 0.3) + 2)
        UI.setZIndex(lh, 133)

        s.tabs[#s.tabs + 1] = { name = td.name, boxId = bgId, labelId = lblId }
        s.tabsByHandle[bgId] = td.name
        cursorX = cursorX + tabW + math.floor(TAB_GAP * uiscale)
    end
end

-- Binary-search truncation with ".." suffix when the text would
-- exceed maxPx in width. Lets long stored-item names sit politely in
-- the available column instead of running into the weight readout.
local function truncateToWidth(text, font, fontPx, maxPx)
    if not text or text == "" then return text end
    local fullW = engine.getTextWidth(font, text, fontPx) or 0
    if fullW <= maxPx then return text end
    -- Drop one character at a time from the end until it fits with "..".
    local lo, hi = 1, #text
    while lo < hi do
        local mid = math.floor((lo + hi) / 2)
        local candidate = string.sub(text, 1, mid) .. ".."
        local w = engine.getTextWidth(font, candidate, fontPx) or 0
        if w <= maxPx then lo = mid + 1 else hi = mid end
    end
    return string.sub(text, 1, math.max(1, lo - 1)) .. ".."
end

-----------------------------------------------------------
-- Render: item rows for the active tab
-----------------------------------------------------------
local function buildRows(originX, originY, contentW, grouped)
    destroyRows()
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h then return 0 end
    local uiscale = scale.get()
    local rowH   = math.floor(ROW_H   * uiscale)
    local rowPad = math.floor(ROW_PAD * uiscale)
    local iconSz = math.floor(ICON_SZ * uiscale)
    local txtPad = math.floor(TEXT_PAD * uiscale)

    -- Filter to active tab.
    local active = s.activeTab
    local visible = {}
    for _, g in ipairs(grouped) do
        if active == "All" or (g.category or "Misc") == active then
            visible[#visible + 1] = g
        end
    end

    local shownCount = math.min(#visible, MAX_ROWS)
    for i = 1, shownCount do
        local g = visible[i]
        local rowY = originY + (i - 1) * (rowH + rowPad)

        if g.iconTex then
            local iconY = rowY + math.floor((rowH - iconSz) / 2)
            local iconId = UI.newSprite("cargo_inv_icon_" .. i,
                iconSz, iconSz, g.iconTex,
                1.0, 1.0, 1.0, 1.0, h.page)
            UI.addToPage(h.page, iconId, originX + txtPad, iconY)
            UI.setZIndex(iconId, 133)
            s.rowElements[#s.rowElements + 1] = { kind = "sprite", id = iconId }
            -- Broken (condition 0): overlay broken_equipment.png.
            if g.condition and g.condition <= 0 then
                local bt = cargoInventoryPanel.brokenTex
                if not bt then
                    local hh = engine.getTextureHandle("broken_equipment")
                    if hh and hh >= 0 then
                        cargoInventoryPanel.brokenTex = hh
                        bt = hh
                    end
                end
                if bt then
                    local oid = UI.newSprite("cargo_inv_broken_" .. i,
                        iconSz, iconSz, bt, 1.0, 1.0, 1.0, 1.0, h.page)
                    UI.addToPage(h.page, oid, originX + txtPad, iconY)
                    UI.setZIndex(oid, 134)
                    s.rowElements[#s.rowElements + 1] =
                        { kind = "sprite", id = oid }
                end
            end
        end

        -- Right-aligned weight (rowWeight = base × count).
        local wText = string.format("%.2f kg",
            (g.weight or 0) * (g.count or 1))
        local wLbl = label.new({
            name     = "cargo_inv_w_" .. i,
            text     = wText,
            font     = h.menuFont,
            fontSize = 13,
            color    = ROW_WEIGHT_COL,
            page     = h.page,
            uiscale  = uiscale,
        })
        local wH = label.getElementHandle(wLbl)
        local wW = select(1, label.getSize(wLbl))
        UI.addToPage(h.page, wH,
            originX + contentW - txtPad - wW,
            rowY + math.floor(rowH / 2) + math.floor(13 * 0.3))
        UI.setZIndex(wH, 133)
        s.rowElements[#s.rowElements + 1] = { kind = "label", id = wLbl }

        -- Display name (with stack suffix), truncated with ".." if it
        -- would otherwise overrun into the weight column.
        local rawName = g.displayName or g.defName or "?"
        if (g.count or 1) > 1 then
            rawName = string.format("%s ×%d", rawName, g.count)
        end
        local nameX     = originX + txtPad + iconSz + txtPad
        local nameRight = originX + contentW - txtPad - wW
                                  - math.floor(NAME_RIGHT_GAP * uiscale)
        local nameMaxPx = math.max(0, nameRight - nameX)
        -- Measure truncation against the SCALED font size so the
        -- estimate matches what the label actually renders at
        -- (label.new scales fontSize by uiscale internally).
        local nameFontPx = math.floor(13 * uiscale)
        local nameText   = truncateToWidth(rawName, h.menuFont,
                                           nameFontPx, nameMaxPx)
        local nameLbl = label.new({
            name     = "cargo_inv_name_" .. i,
            text     = nameText,
            font     = h.menuFont,
            fontSize = 13,
            color    = ROW_NAME_COL,
            page     = h.page,
            uiscale  = uiscale,
        })
        local nh = label.getElementHandle(nameLbl)
        UI.addToPage(h.page, nh, nameX,
            rowY + math.floor(rowH / 2) + math.floor(13 * 0.3))
        UI.setZIndex(nh, 133)
        s.rowElements[#s.rowElements + 1] = { kind = "label", id = nameLbl }

        -- Full-row transparent hit-zone for right-click → withdraw.
        local hitId = UI.newSprite("cargo_inv_hit_" .. i,
            contentW, rowH, cargoInventoryPanel.whitePixelTex,
            1.0, 1.0, 1.0, 0.0, h.page)
        UI.addToPage(h.page, hitId, originX, rowY)
        UI.setZIndex(hitId, 134)
        UI.setClickable(hitId, true)
        UI.setOnRightClick(hitId, "onCargoInventoryItemRightClick")
        s.rowElements[#s.rowElements + 1] = { kind = "sprite", id = hitId }
        s.rowItems[#s.rowItems + 1] = { hitId = hitId, item = g }
    end
    return shownCount
end

-----------------------------------------------------------
-- Open / refresh
-----------------------------------------------------------
local function buildLayout(bid, mx, my)
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h or not h.page then return end

    if not cargoInventoryPanel.tabSelTexSet then
        cargoInventoryPanel.tabSelTexSet =
            boxTextures.load("assets/textures/ui/tabselected", "tabselected")
        cargoInventoryPanel.tabUnselTexSet =
            boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")
    end
    if not cargoInventoryPanel.whitePixelTex then
        cargoInventoryPanel.whitePixelTex = engine.loadTexture(
            "assets/textures/hud/utility/white.png")
    end

    local stored  = building.getStorage(bid) or {}
    local grouped = groupItems(stored)
    local tabDefs = computeTabs(grouped)

    -- Snap active tab to a still-present one.
    local stillValid = false
    for _, t in ipairs(tabDefs) do
        if t.name == s.activeTab then stillValid = true; break end
    end
    if not stillValid then s.activeTab = "All" end

    -- Size the panel.
    local uiscale = scale.get()
    local panelW  = math.floor(PANEL_W_BASE * uiscale)
    local padX    = math.floor(PANEL_PAD_X  * uiscale)
    local padTop  = math.floor(PANEL_PAD_TOP * uiscale)
    local padBot  = math.floor(PANEL_PAD_BOT * uiscale)
    local titleH  = math.floor(TITLE_H    * uiscale)
    local subH    = math.floor(SUBTITLE_H * uiscale)
    local tabH    = math.floor(TAB_H      * uiscale)
    local rowH    = math.floor(ROW_H      * uiscale)
    local rowPad  = math.floor(ROW_PAD    * uiscale)

    local visibleCount = 0
    do
        local active = s.activeTab
        for _, g in ipairs(grouped) do
            if active == "All" or (g.category or "Misc") == active then
                visibleCount = visibleCount + 1
            end
        end
    end
    visibleCount = math.min(visibleCount, MAX_ROWS)
    -- Always reserve one row's height so an empty cargo isn't a flat
    -- strip — easier to read "(empty)" / nothing than a single line.
    if visibleCount < 1 then visibleCount = 1 end

    local rowsH    = visibleCount * rowH + (visibleCount - 1) * rowPad
    local panelH   = padTop + titleH + subH + 6 + tabH + 8 + rowsH + padBot

    -- Clamp the panel position to the framebuffer so it doesn't open
    -- partly off-screen if the player right-clicks near an edge.
    local px = mx
    local py = my
    if h.fbW and px + panelW > h.fbW then px = math.max(0, h.fbW - panelW) end
    if h.fbH and py + panelH > h.fbH then py = math.max(0, h.fbH - panelH) end

    -- Recreate or reuse the panel box.
    if s.panelId then panel.destroy(s.panelId); s.panelId = nil end
    s.panelId = panel.new({
        name       = "cargo_inv_panel",
        page       = h.page,
        x          = px,
        y          = py,
        width      = panelW,
        height     = panelH,
        textureSet = h.boxTexSet,
        color      = { 0.1, 0.1, 0.1, 0.95 },
        tileSize   = 64,
        zIndex     = 130,
        padding    = { top = PANEL_PAD_TOP, bottom = PANEL_PAD_BOT,
                       left = PANEL_PAD_X,  right  = PANEL_PAD_X },
        uiscale    = uiscale,
    })
    local pbounds = panel.getContentBounds(s.panelId)
    local cx = px + pbounds.x
    local cy = py + pbounds.y
    local cw = pbounds.width

    destroyTitle()
    buildTitle(cx, cy, bid)
    buildTabStrip(cx, cy + titleH + subH + 6, cw, tabDefs)
    buildRows(cx, cy + titleH + subH + 6 + tabH + 8, cw, grouped)

    s.lastHash = contentHash(bid)
end

function cargoInventoryPanel.openFor(bid, mx, my)
    if not bid then return end
    local cap = building.getStorageCapacity(bid)
    if not cap or cap <= 0 then return end
    cargoInventoryPanel.closeIfOpen()
    cargoInventoryPanel.state.open = true
    cargoInventoryPanel.state.bid  = bid
    cargoInventoryPanel.state.mx   = mx
    cargoInventoryPanel.state.my   = my
    buildLayout(bid, mx, my)
end

function cargoInventoryPanel.closeIfOpen()
    if not cargoInventoryPanel.state.open then return end
    destroyAll()
    cargoInventoryPanel.state.open      = false
    cargoInventoryPanel.state.bid       = nil
    cargoInventoryPanel.state.activeTab = "All"
    cargoInventoryPanel.state.lastHash  = ""
end

function cargoInventoryPanel.isOpen()
    return cargoInventoryPanel.state.open == true
end

-----------------------------------------------------------
-- Click handlers (dispatched via ui_manager)
-----------------------------------------------------------
function cargoInventoryPanel.handleTabClick(elemHandle)
    local s = cargoInventoryPanel.state
    if not s.open then return false end
    local cat = s.tabsByHandle[elemHandle]
    if not cat then return false end
    if cat == s.activeTab then return true end
    s.activeTab = cat
    s.lastHash  = ""    -- force rebuild
    buildLayout(s.bid, s.mx, s.my)
    return true
end

function cargoInventoryPanel.handleItemRightClick(elemHandle)
    local s = cargoInventoryPanel.state
    if not s.open then return false end
    local row
    for _, r in ipairs(s.rowItems) do
        if r.hitId == elemHandle then row = r; break end
    end
    if not row then return false end

    local bid     = s.bid
    local defName = row.item.defName
    local instId  = row.item.instanceId
    local target  = adjacentSelectedUnit(bid)

    local items = {}
    if target then
        local info = unit.getInfo(target)
        local who  = (info and info.defName or "unit")
        items[1] = {
            label    = "Withdraw with " .. who,
            callback = function()
                unit.withdrawFromCargo(target, bid, defName, instId)
                cargoInventoryPanel.state.lastHash = ""
            end,
        }
    else
        items[1] = {
            label   = "Withdraw (select an adjacent unit first)",
            enabled = false,
        }
    end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(items, mx, my)
    return true
end

-----------------------------------------------------------
-- Per-tick refresh: cheap content-hash compare; rebuild only on
-- actual change (deposits/withdrawals).
-----------------------------------------------------------
function cargoInventoryPanel.update(dt)
    local s = cargoInventoryPanel.state
    if not s.open or not s.bid then return end
    if not building.getInfo(s.bid) then
        -- Cargo destroyed while popup was open — close.
        cargoInventoryPanel.closeIfOpen()
        return
    end
    local h = contentHash(s.bid)
    if h ~= s.lastHash then
        buildLayout(s.bid, s.mx, s.my)
    end
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function cargoInventoryPanel.init(scriptId)
    engine.logInfo("Cargo inventory panel initializing...")
end

function cargoInventoryPanel.shutdown()
    cargoInventoryPanel.closeIfOpen()
    engine.logInfo("Cargo inventory panel shut down")
end

-- Esc closes the popup. Returns true if consumed.
-- Named handle* (not on*) deliberately: this module is engine-loaded,
-- so an on*-named function would also fire directly on every engine
-- broadcast — double-firing on top of init.lua's ordered forward.
function cargoInventoryPanel.handleKeyDown(key)
    if key == "Escape" and cargoInventoryPanel.state.open then
        cargoInventoryPanel.closeIfOpen()
        return true
    end
    return false
end

return cargoInventoryPanel
