-- Item Contents Panel
--
-- Floating popup showing the contents of an ITEM-CONTAINER carried in
-- a unit's inventory (a first-aid kit, a toolbox). Triggered by
-- right-click on the container row in the unit-info inventory list →
-- context menu → "Contents".
--
-- This is the unit-carried analogue of cargo_inventory_panel (which
-- shows a storage building's contents). Since #1088 both render their
-- rows through the SAME shared item-list widget
-- (scripts/ui/item_list.lua); this popup simply leaves the widget's
-- optional category tab strip off, because a kit is one category. (If
-- toolboxes later mix categories, enable the widget's tabs here.)
--
-- The rows are ALREADY GROUPED by defName on the Haskell side, so this
-- host hands them to the widget pre-grouped: the finer stack key the
-- other two hosts use must not re-split them, and their order (a
-- hashmap enumeration) must not be re-sorted.
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

local panel    = require("scripts.ui.panel")
local label    = require("scripts.ui.label")
local scale    = require("scripts.ui.scale")
local itemList = require("scripts.ui.item_list")

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
    instanceId  = nil,
    mx          = 0,
    my          = 0,
    panelId     = nil,
    titleId     = nil,
    subtitleId  = nil,
    listId      = nil,   -- shared item-list widget instance (#1088)
}

itemContentsPanel.hud = nil   -- assets set by setup()

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function itemContentsPanel.setup(opts)
    itemContentsPanel.hud = opts
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- The item-list parameters describing THIS panel's data and
-- presentation policy. Rows arrive ALREADY GROUPED by defName from the
-- Haskell side (src/Engine/Scripting/Lua/API/Units/Inventory.hs), so
-- the widget must neither re-split them by the finer stack key nor
-- reorder them, and this panel shows no category tabs.
local function listDataParams(rows)
    return {
        items      = rows,
        preGrouped = true,
        tabs       = false,
        uiscale    = scale.get(),
        maxRows    = MAX_ROWS,
        emptyText  = "(empty)",
        emptyColor = EMPTY_COL,
        rowIcon    = function(g)
            -- Unlike the other hosts, this API can report a missing
            -- icon as a negative handle.
            if g.iconTex and g.iconTex >= 0 then return g.iconTex end
            return nil
        end,
        rowName = function(g)
            local n = g.displayName or g.defName or "?"
            if (g.count or 1) > 1 then
                n = string.format("%s ×%d", n, g.count)
            end
            return n
        end,
        rowWeightText = function(g)
            -- Per-item TRUE mass (empty + fill + nested contents, from
            -- itemTotalWeight) × count.
            return string.format("%.2f kg", (g.weight or 0) * (g.count or 1))
        end,
        rowColor = function() return ROW_NAME_COL end,
        rowTooltip = function(g)
            local hintLines = {}
            if g.fill and g.fill > 0 then
                hintLines[#hintLines + 1] = string.format("Holds: %.2f", g.fill)
            end
            if g.condition and g.condition > 0 and g.condition < 100 then
                hintLines[#hintLines + 1] =
                    string.format("Condition: %.0f%%", g.condition)
            elseif g.condition and g.condition <= 0 then
                hintLines[#hintLines + 1] = "Broken"
            end
            if #hintLines == 0 then return nil end
            return {
                text = g.displayName or g.defName or "?",
                hint = table.concat(hintLines, "\n"),
            }
        end,
        -- No right-click action: this panel is read-only, so the widget
        -- registers no click callback for its rows at all.
    }
end

-----------------------------------------------------------
-- Element teardown
-----------------------------------------------------------
local function destroyList()
    local s = itemContentsPanel.state
    if s.listId then
        itemList.destroy(s.listId)
        s.listId = nil
    end
end

local function destroyTitle()
    local s = itemContentsPanel.state
    if s.titleId    then label.destroy(s.titleId);    s.titleId    = nil end
    if s.subtitleId then label.destroy(s.subtitleId); s.subtitleId = nil end
end

local function destroyAll()
    destroyList()
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
-- Open / refresh
-----------------------------------------------------------
local function buildLayout(uid, defName, mx, my, instanceId)
    local s = itemContentsPanel.state
    local h = itemContentsPanel.hud
    if not h or not h.page then return end

    local rows = unit.getItemContents(uid, defName, instanceId) or {}

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

    -- #750 round-7 review: cap against the actual framebuffer — the
    -- position clamp below only ever repositions the panel, never
    -- shrinks it, so PANEL_W_BASE*uiscale could exceed the framebuffer
    -- at a narrow, high-scale, still-C2-supported combination regardless
    -- of position. Best-effort degrade, same pattern as cargo_inventory_
    -- panel.lua/popup.lua/unit_info_v2.lua's fixes for the identical gap.
    if h.fbW then panelW = math.min(panelW, h.fbW) end
    if h.fbH then panelH = math.min(panelH, h.fbH) end

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
    destroyList()
    buildTitle(cx, cy, defName, rows)

    local dataParams = listDataParams(rows)
    dataParams.name         = "item_contents"
    dataParams.page         = h.page
    dataParams.font         = h.menuFont
    dataParams.x            = cx
    dataParams.y            = cy + titleH + subH + 8
    dataParams.width        = cw
    dataParams.height       = rowsH
    dataParams.rowHeight    = ROW_H
    dataParams.rowPad       = ROW_PAD
    dataParams.iconSize     = ICON_SZ
    dataParams.textPad      = TEXT_PAD
    dataParams.nameRightGap = NAME_RIGHT_GAP
    dataParams.rowFontSize  = 13
    dataParams.weightColor  = ROW_WEIGHT_COL
    dataParams.zBase        = 132
    s.listId = itemList.new(dataParams)
end

-- instanceId (optional) targets the EXACT container the player clicked,
-- so two same-def kits don't show each other's contents (#67). Falls
-- back to first-by-defName when nil.
function itemContentsPanel.openFor(uid, defName, mx, my, instanceId)
    if not uid or not defName then return end
    -- A new request always tears down the prior popup first (singleton),
    -- so a stale/invalid request never leaves an earlier container's
    -- popup on screen. Close BEFORE the existence guard below.
    itemContentsPanel.closeIfOpen()
    -- Don't open for a container that doesn't exist: the unit holds no
    -- inventory item matching defName/instanceId. unit.getItemContents
    -- returns nil in that case; an existing-but-empty container returns a
    -- table, so this only rejects genuinely missing containers (the popup
    -- still opens and shows "(empty)" for a real, empty kit).
    if not unit.getItemContents(uid, defName, instanceId) then return end
    local s = itemContentsPanel.state
    s.open       = true
    s.uid        = uid
    s.defName    = defName
    s.instanceId = instanceId
    s.mx         = mx
    s.my         = my
    buildLayout(uid, defName, mx, my, instanceId)
end

function itemContentsPanel.closeIfOpen()
    local s = itemContentsPanel.state
    if not s.open then return end
    destroyAll()
    s.open       = false
    s.uid        = nil
    s.defName    = nil
    s.instanceId = nil
end

function itemContentsPanel.isOpen()
    return itemContentsPanel.state.open == true
end

-----------------------------------------------------------
-- Per-tick refresh. The rebuild comparison belongs to the shared
-- item-list widget; the "container went away" check below is
-- host-owned lifecycle, not invalidation — the popup must not outlive
-- the kit it describes (consumed / unit died).
-----------------------------------------------------------
function itemContentsPanel.update(dt)
    local s = itemContentsPanel.state
    if not s.open or not s.uid or not s.defName then return end
    local rows = unit.getItemContents(s.uid, s.defName, s.instanceId)
    if not rows then
        itemContentsPanel.closeIfOpen()
        return
    end
    if itemList.isStale(s.listId, listDataParams(rows)) then
        buildLayout(s.uid, s.defName, s.mx, s.my, s.instanceId)
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
