-- Item Contents Panel
--
-- Floating popup showing the contents of an ITEM-CONTAINER carried in
-- a unit's inventory (a first-aid kit, a toolbox). Triggered by
-- right-click on the container row in the unit-info inventory list →
-- context menu → "Contents".
--
-- This is the unit-carried analogue of cargo_inventory_panel (which
-- shows a storage building's contents). Same 9-patch box + icon-row
-- look, but a single flat list — a kit is one category, so no tab
-- strip. (If toolboxes later mix categories, lift the cargo panel's
-- computeTabs/buildTabStrip in here.)
--
-- Data source: unit.getItemContents(uid, defName) → grouped rows of
-- { defName, displayName, category, count, weight, iconTex, fill,
--   condition }. The Haskell side groups identical entries by defName.
--
-- Singleton: opening for a new container destroys the old popup.
-- Pinned at the mouse position when opened. Esc closes.
--
-- Public API:
--   openFor(uid, defName, mx, my) — open on this container
--   closeIfOpen()                 — destroy the popup if shown
--   isOpen()                      — bool
--
-- Engine script hooks: setup / init / update / shutdown.
--
-- Registered in package.loaded so init.lua's key handler, hud setup,
-- and unit_info_v2's right-click all see the same instance even though
-- engine.loadScript uses dofile.

local itemContentsPanel =
    package.loaded["scripts.item_contents_panel"] or {}
package.loaded["scripts.item_contents_panel"] = itemContentsPanel

local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")

-----------------------------------------------------------
-- Layout constants. Mirrors cargo_inventory_panel so the two read the
-- same visually. Base units; uiscale applied at draw time.
-----------------------------------------------------------
local PANEL_W_BASE   = 420
local PANEL_PAD_X    = 32
local PANEL_PAD_TOP  = 28
local PANEL_PAD_BOT  = 20
local TITLE_FONT     = 16
local TITLE_H        = 22
local SUBTITLE_FONT  = 13
local SUBTITLE_H     = 18
local ROW_H          = 32
local ROW_PAD        = 2
local ICON_SZ        = 28
local TEXT_PAD       = 12    -- horizontal pad inside each row
local NAME_RIGHT_GAP = 24    -- gap between name and weight columns
local MAX_ROWS       = 12
local TITLE_COL      = { 1.0, 1.0, 1.0, 1.0 }
local SUBTITLE_COL   = { 0.85, 0.85, 0.85, 1.0 }
local ROW_NAME_COL   = { 1.0, 1.0, 1.0, 1.0 }
local ROW_WEIGHT_COL = { 0.85, 0.85, 0.85, 1.0 }
local EMPTY_COL      = { 0.7, 0.7, 0.7, 1.0 }

-----------------------------------------------------------
-- State
-----------------------------------------------------------
itemContentsPanel.state = itemContentsPanel.state or {
    open        = false,
    uid         = nil,
    defName     = nil,
    mx          = 0,
    my          = 0,
    panelId     = nil,
    titleId     = nil,
    subtitleId  = nil,
    rowElements = {},   -- list of {kind, id} for cleanup
    lastHash    = "",
}

itemContentsPanel.hud           = nil   -- assets set by setup()
itemContentsPanel.whitePixelTex = nil
itemContentsPanel.brokenTex     = nil

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function itemContentsPanel.setup(opts)
    itemContentsPanel.hud = opts
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- Cheap snapshot we hash to decide whether a rebuild is needed
-- (contents change when a medic draws a bandage / returns a tool).
local function contentHash(uid, defName)
    local rows = unit.getItemContents(uid, defName)
    if not rows then return "__gone__" end
    local parts = { tostring(#rows) }
    for _, r in ipairs(rows) do
        parts[#parts + 1] = (r.defName or "?")
                          .. ":" .. tostring(r.count or 0)
                          .. ":" .. tostring(r.fill or 0)
                          .. ":" .. tostring(r.condition or 0)
    end
    return table.concat(parts, ",")
end

-- Binary-search truncation with ".." suffix (same rule as the cargo
-- panel) so long item names don't run into the weight column.
local function truncateToWidth(text, font, fontPx, maxPx)
    if not text or text == "" then return text end
    local fullW = engine.getTextWidth(font, text, fontPx) or 0
    if fullW <= maxPx then return text end
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
-- Element teardown
-----------------------------------------------------------
local function destroyRows()
    local s = itemContentsPanel.state
    for _, e in ipairs(s.rowElements) do
        if e.kind == "label" then label.destroy(e.id)
        else UI.deleteElement(e.id) end
    end
    s.rowElements = {}
end

local function destroyTitle()
    local s = itemContentsPanel.state
    if s.titleId    then label.destroy(s.titleId);    s.titleId    = nil end
    if s.subtitleId then label.destroy(s.subtitleId); s.subtitleId = nil end
end

local function destroyAll()
    destroyRows()
    destroyTitle()
    local s = itemContentsPanel.state
    if s.panelId then
        panel.destroy(s.panelId)
        s.panelId = nil
    end
end

-----------------------------------------------------------
-- Render: title + subtitle
-----------------------------------------------------------
local function buildTitle(originX, originY, defName, rows)
    local s = itemContentsPanel.state
    local h = itemContentsPanel.hud
    if not h then return end
    local uiscale = scale.get()

    -- Title = the container's display name (from its own inventory row,
    -- falling back to the def name).
    local name = defName
    local inv = unit.getInventory(s.uid) or {}
    for _, it in ipairs(inv) do
        if it.defName == defName then
            name = it.displayName or defName
            break
        end
    end

    s.titleId = label.new({
        name     = "item_contents_title",
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

    -- Subtitle = total piece count across all groups.
    local pieces = 0
    for _, r in ipairs(rows) do pieces = pieces + (r.count or 1) end
    local subText = (pieces == 1) and "1 item" or (pieces .. " items")
    s.subtitleId = label.new({
        name     = "item_contents_subtitle",
        text     = subText,
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
-- Render: contents rows
-----------------------------------------------------------
local function buildRows(originX, originY, contentW, rows)
    destroyRows()
    local s = itemContentsPanel.state
    local h = itemContentsPanel.hud
    if not h then return 0 end
    local uiscale = scale.get()
    local rowH   = math.floor(ROW_H   * uiscale)
    local rowPad = math.floor(ROW_PAD * uiscale)
    local iconSz = math.floor(ICON_SZ * uiscale)
    local txtPad = math.floor(TEXT_PAD * uiscale)

    if #rows == 0 then
        local emptyLbl = label.new({
            name     = "item_contents_empty",
            text     = "(empty)",
            font     = h.menuFont,
            fontSize = 13,
            color    = EMPTY_COL,
            page     = h.page,
            uiscale  = uiscale,
        })
        local eh = label.getElementHandle(emptyLbl)
        UI.addToPage(h.page, eh, originX + txtPad,
            originY + math.floor(rowH / 2) + math.floor(13 * 0.3))
        UI.setZIndex(eh, 133)
        s.rowElements[#s.rowElements + 1] = { kind = "label", id = emptyLbl }
        return 1
    end

    local shownCount = math.min(#rows, MAX_ROWS)
    for i = 1, shownCount do
        local g = rows[i]
        local rowY = originY + (i - 1) * (rowH + rowPad)

        -- Icon (with broken overlay when a reusable tool is worn out).
        if g.iconTex and g.iconTex >= 0 then
            local iconY = rowY + math.floor((rowH - iconSz) / 2)
            local iconId = UI.newSprite("item_contents_icon_" .. i,
                iconSz, iconSz, g.iconTex,
                1.0, 1.0, 1.0, 1.0, h.page)
            UI.addToPage(h.page, iconId, originX + txtPad, iconY)
            UI.setZIndex(iconId, 133)
            s.rowElements[#s.rowElements + 1] = { kind = "sprite", id = iconId }
            if g.condition and g.condition <= 0 then
                local bt = itemContentsPanel.brokenTex
                if not bt then
                    local hh = engine.getTextureHandle("broken_equipment")
                    if hh and hh >= 0 then
                        itemContentsPanel.brokenTex = hh
                        bt = hh
                    end
                end
                if bt then
                    local oid = UI.newSprite("item_contents_broken_" .. i,
                        iconSz, iconSz, bt, 1.0, 1.0, 1.0, 1.0, h.page)
                    UI.addToPage(h.page, oid, originX + txtPad, iconY)
                    UI.setZIndex(oid, 134)
                    s.rowElements[#s.rowElements + 1] =
                        { kind = "sprite", id = oid }
                end
            end
        end

        -- Right-aligned weight (base × count).
        local wText = string.format("%.2f kg",
            (g.weight or 0) * (g.count or 1))
        local wLbl = label.new({
            name     = "item_contents_w_" .. i,
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

        -- Display name with stack suffix, truncated with ".." to avoid
        -- overrunning the weight column.
        local rawName = g.displayName or g.defName or "?"
        if (g.count or 1) > 1 then
            rawName = string.format("%s ×%d", rawName, g.count)
        end
        local nameX     = originX + txtPad + iconSz + txtPad
        local nameRight = originX + contentW - txtPad - wW
                                  - math.floor(NAME_RIGHT_GAP * uiscale)
        local nameMaxPx = math.max(0, nameRight - nameX)
        local nameFontPx = math.floor(13 * uiscale)
        local nameText   = truncateToWidth(rawName, h.menuFont,
                                           nameFontPx, nameMaxPx)
        local nameLbl = label.new({
            name     = "item_contents_name_" .. i,
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

        -- Per-row tooltip: fill (litres / count in a bottle) and
        -- condition for the reusable tools. Hosted on a transparent
        -- full-row hit-zone above the icon + labels.
        local hintLines = {}
        if g.fill and g.fill > 0 then
            hintLines[#hintLines + 1] =
                string.format("Holds: %.2f", g.fill)
        end
        if g.condition and g.condition > 0 and g.condition < 100 then
            hintLines[#hintLines + 1] =
                string.format("Condition: %.0f%%", g.condition)
        elseif g.condition and g.condition <= 0 then
            hintLines[#hintLines + 1] = "Broken"
        end
        local hitId = UI.newSprite("item_contents_hit_" .. i,
            contentW, rowH, itemContentsPanel.whitePixelTex,
            1.0, 1.0, 1.0, 0.0, h.page)
        UI.addToPage(h.page, hitId, originX, rowY)
        UI.setZIndex(hitId, 135)
        if #hintLines > 0 then
            UI.setClickable(hitId, true)
            UI.setTooltipRich(hitId, {
                text = g.displayName or g.defName or "?",
                hint = table.concat(hintLines, "\n"),
            })
        end
        s.rowElements[#s.rowElements + 1] = { kind = "sprite", id = hitId }
    end
    return shownCount
end

-----------------------------------------------------------
-- Open / refresh
-----------------------------------------------------------
local function buildLayout(uid, defName, mx, my)
    local s = itemContentsPanel.state
    local h = itemContentsPanel.hud
    if not h or not h.page then return end

    if not itemContentsPanel.whitePixelTex then
        itemContentsPanel.whitePixelTex = engine.loadTexture(
            "assets/textures/hud/utility/white.png")
    end

    local rows = unit.getItemContents(uid, defName) or {}

    local uiscale = scale.get()
    local panelW  = math.floor(PANEL_W_BASE * uiscale)
    local padTop  = math.floor(PANEL_PAD_TOP * uiscale)
    local padBot  = math.floor(PANEL_PAD_BOT * uiscale)
    local titleH  = math.floor(TITLE_H    * uiscale)
    local subH    = math.floor(SUBTITLE_H * uiscale)
    local rowH    = math.floor(ROW_H      * uiscale)
    local rowPad  = math.floor(ROW_PAD    * uiscale)

    local visibleCount = math.min(math.max(#rows, 1), MAX_ROWS)
    local rowsH  = visibleCount * rowH + (visibleCount - 1) * rowPad
    local panelH = padTop + titleH + subH + 8 + rowsH + padBot

    -- Clamp the panel to the framebuffer so it never opens partly
    -- off-screen near an edge.
    local px, py = mx, my
    if h.fbW and px + panelW > h.fbW then px = math.max(0, h.fbW - panelW) end
    if h.fbH and py + panelH > h.fbH then py = math.max(0, h.fbH - panelH) end

    if s.panelId then panel.destroy(s.panelId); s.panelId = nil end
    s.panelId = panel.new({
        name       = "item_contents_panel",
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
    buildTitle(cx, cy, defName, rows)
    buildRows(cx, cy + titleH + subH + 8, cw, rows)

    s.lastHash = contentHash(uid, defName)
end

function itemContentsPanel.openFor(uid, defName, mx, my)
    if not uid or not defName then return end
    itemContentsPanel.closeIfOpen()
    local s = itemContentsPanel.state
    s.open    = true
    s.uid     = uid
    s.defName = defName
    s.mx      = mx
    s.my      = my
    buildLayout(uid, defName, mx, my)
end

function itemContentsPanel.closeIfOpen()
    local s = itemContentsPanel.state
    if not s.open then return end
    destroyAll()
    s.open     = false
    s.uid      = nil
    s.defName  = nil
    s.lastHash = ""
end

function itemContentsPanel.isOpen()
    return itemContentsPanel.state.open == true
end

-----------------------------------------------------------
-- Per-tick refresh: cheap content-hash compare. Closes the popup if
-- the unit or its container went away (consumed / unit died).
-----------------------------------------------------------
function itemContentsPanel.update(dt)
    local s = itemContentsPanel.state
    if not s.open or not s.uid or not s.defName then return end
    local h = contentHash(s.uid, s.defName)
    if h == "__gone__" then
        itemContentsPanel.closeIfOpen()
        return
    end
    if h ~= s.lastHash then
        buildLayout(s.uid, s.defName, s.mx, s.my)
    end
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function itemContentsPanel.init(scriptId)
    engine.logInfo("Item contents panel initializing...")
end

function itemContentsPanel.shutdown()
    itemContentsPanel.closeIfOpen()
    engine.logInfo("Item contents panel shut down")
end

-- Esc closes the popup. Returns true if consumed. Named handle* (not
-- on*) deliberately: this module is engine-loaded, so an on*-named
-- function would also fire on every engine broadcast.
function itemContentsPanel.handleKeyDown(key)
    if key == "Escape" and itemContentsPanel.state.open then
        itemContentsPanel.closeIfOpen()
        return true
    end
    return false
end

return itemContentsPanel
